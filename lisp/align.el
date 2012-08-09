;;; align.el --- align text to a specific column, by regexp

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: FSF
;; Keywords: convenience languages lisp

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

;; This mode allows you to align regions in a context-sensitive fashion.
;; The classic use is to align assignments:
;;
;;    int a = 1;
;;    short foo = 2;
;;    double blah = 4;
;;
;; becomes
;;
;;    int    a    = 1;
;;    short  foo  = 2;
;;    double blah = 4;

;;; Usage:

;; There are several variables which define how certain "categories"
;; of syntax are to be treated.  These variables go by the name
;; `align-CATEGORY-modes'.  For example, "c++" is such a category.
;; There are several rules which apply to c++, but since several other
;; languages have a syntax similar to c++ (e.g., c, java, etc), these
;; modes are treated as belonging to the same category.
;;
;; If you want to add a new mode under a certain category, just
;; customize that list, or add the new mode manually.  For example, to
;; make jde-mode a c++ category mode, use this code in your .emacs
;; file:
;;
;;    (setq align-c++-modes (cons 'jde-mode align-c++-modes))

;; In some programming modes, it's useful to have the aligner run only
;; after indentation is performed.  To achieve this, customize or set
;; the variable `align-indent-before-aligning' to t.

;;; Module Authors:

;; In order to incorporate align's functionality into your own
;; modules, there are only a few steps you have to follow.

;;  1. Require or load in the align.el library.
;;
;;  2. Define your alignment and exclusion rules lists, either
;;     customizable or not.
;;
;;  3. In your mode function, set the variables
;;     `align-mode-rules-list' and `align-mode-exclude-rules-list'
;;     to your own rules lists.

;; If there is any need to add your mode name to one of the
;; align-?-modes variables (for example, `align-dq-string-modes'), use
;; `add-to-list', or some similar function which checks first to see
;; if the value is already there.  Since the user may customize that
;; mode list, and then write your mode name into their .emacs file,
;; causing the symbol already to be present the next time they load
;; your package.

;; Example:
;;
;;   (require 'align)
;;
;;   (defcustom my-align-rules-list
;;     '((my-rule
;;        (regexp . "Sample")))
;;     :type align-rules-list-type
;;     :group 'my-package)
;;
;;   (put 'my-align-rules-list 'risky-local-variable t)
;;
;;   (add-to-list 'align-dq-string-modes 'my-package-mode)
;;   (add-to-list 'align-open-comment-modes 'my-package-mode)
;;
;;   (defun my-mode ()
;;      ...
;;      (setq align-mode-rules-list my-align-rules-list))
;;
;; Note that if you need to install your own exclusion rules, then you
;; will also need to reproduce any double-quoted string, or open
;; comment exclusion rules that are defined in the standard
;; `align-exclude-rules-list'.  At the moment there is no convenient
;; way to mix both mode-local and global rules lists.

;;; History:

;; Version 1.0 was created in the earlier part of 1996, using a very
;; simple algorithm that understand only basic regular expressions.
;; Parts of the code were broken up and included in vhdl-mode.el
;; around this time.  After several comments from users, and a need to
;; find a more robust, higher performing algorithm, 2.0 was born in late
;; 1998.  Many different approaches were taken (mostly due to the
;; complexity of TeX tables), but finally a scheme was discovered
;; which worked fairly well for most common usage cases.  Development
;; beyond version 2.8 is not planned, except for problems that users
;; might encounter.

;;; Code:

(defgroup align nil
  "Align text to a specific column, by regexp."
  :version "21.1"
  :group 'fill)

;;; User Variables:

(defcustom align-load-hook nil
  "Hook that gets run after the aligner has been loaded."
  :type 'hook
  :group 'align)

(defcustom align-indent-before-aligning nil
  "If non-nil, indent the marked region before aligning it."
  :type 'boolean
  :group 'align)

(defcustom align-default-spacing 1
  "An integer that represents the default amount of padding to use.
If `align-to-tab-stop' is non-nil, this will represent the number of
tab stops to use for alignment, rather than the number of spaces.
Each alignment rule can optionally override both this variable and
`align-to-tab-stop'.  See `align-rules-list'."
  :type 'integer
  :group 'align)

(defcustom align-to-tab-stop 'indent-tabs-mode
  "If non-nil, alignments will always fall on a tab boundary.
It may also be a symbol, whose value will be taken."
  :type '(choice (const nil) symbol)
  :group 'align)

(defcustom align-region-heuristic 500
  "If non-nil, used as a heuristic by `align-current'.
Since each alignment rule can possibly have its own set of alignment
sections (whenever `align-region-separate' is non-nil, and not a
string), this heuristic is used to determine how far before and after
point we should search in looking for a region separator.  Larger
values can mean slower performance in large files, although smaller
values may cause unexpected behavior at times."
  :type 'integer
  :group 'align)

(defcustom align-highlight-change-face 'highlight
  "The face to highlight with if changes are necessary."
  :type 'face
  :group 'align)

(defcustom align-highlight-nochange-face 'secondary-selection
  "The face to highlight with if no changes are necessary."
  :type 'face
  :group 'align)

(defcustom align-large-region 10000
  "If an integer, defines what constitutes a \"large\" region.
If nil, then no messages will ever be printed to the minibuffer."
  :type 'integer
  :group 'align)

(defcustom align-c++-modes '(c++-mode c-mode java-mode)
  "A list of modes whose syntax resembles C/C++."
  :type '(repeat symbol)
  :group 'align)

(defcustom align-perl-modes '(perl-mode cperl-mode)
  "A list of modes where Perl syntax is to be seen."
  :type '(repeat symbol)
  :group 'align)

(defcustom align-lisp-modes
  '(emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode)
  "A list of modes whose syntax resembles Lisp."
  :type '(repeat symbol)
  :group 'align)

(defcustom align-tex-modes
  '(tex-mode plain-tex-mode latex-mode slitex-mode)
  "A list of modes whose syntax resembles TeX (and family)."
  :type '(repeat symbol)
  :group 'align)

(defcustom align-text-modes '(text-mode outline-mode)
  "A list of modes whose content is plain text."
  :type '(repeat symbol)
  :group 'align)

(defcustom align-dq-string-modes
  (append align-lisp-modes align-c++-modes align-perl-modes
	  '(python-mode))
  "A list of modes where double quoted strings should be excluded."
  :type '(repeat symbol)
  :group 'align)

(defcustom align-sq-string-modes
  (append align-perl-modes '(python-mode))
  "A list of modes where single quoted strings should be excluded."
  :type '(repeat symbol)
  :group 'align)

(defcustom align-open-comment-modes
  (append align-lisp-modes align-c++-modes align-perl-modes
	  '(python-mode makefile-mode))
  "A list of modes with a single-line comment syntax.
These are comments as in Lisp, which have a beginning, but end with
the line (i.e., `comment-end' is an empty string)."
  :type '(repeat symbol)
  :group 'align)

(defcustom align-region-separate "^\\s-*[{}]?\\s-*$"
  "Select the method by which alignment sections will be separated.
If this is a symbol, that symbol's value will be used.

For the sake of clarification, consider the following example, which
will be referred to in the descriptions below.

    int alpha = 1; /* one */
    double beta = 2.0;
    long gamma; /* ten */

    unsigned int delta = 1; /* one */
    long double epsilon = 3.0;
    long long omega; /* ten */

The possible settings for `align-region-separate' are:

 `entire'  The entire region being aligned will be considered as a
	   single alignment section.  Assuming that comments were not
	   being aligned to a particular column, the example would
	   become:

	     int          alpha    = 1;   /* one */
	     double       beta     = 2.0;
	     long         gamma;          /* ten */

	     unsigned int delta    = 1;   /* one */
	     long double  epsilon;
	     long long    chi      = 10;  /* ten */

 `group'   Each contiguous set of lines where a specific alignment
	   occurs is considered a section for that alignment rule.
	   Note that each rule may have any entirely different set
           of section divisions than another.

	     int    alpha = 1; /* one */
	     double beta  = 2.0;
	     long   gamma; /* ten */

	     unsigned int delta = 1; /* one */
	     long double  epsilon;
	     long long    chi = 10; /* ten */

 `largest' When contiguous rule sets overlap, the largest section
	   described will be taken as the alignment section for each
	   rule touched by that section.

	     int    alpha = 1;   /* one */
	     double beta  = 2.0;
	     long   gamma;       /* ten */

	     unsigned int delta    = 1;  /* one */
	     long double  epsilon;
	     long long    chi      = 10; /* ten */

	   NOTE: This option is not supported yet, due to algorithmic
	   issues which haven't been satisfactorily resolved.  There
	   are ways to do it, but they're both ugly and resource
	   consumptive.

 regexp    A regular expression string which defines the section
	   divider.  If the mode you're in has a consistent divider
	   between sections, the behavior will be very similar to
	   `largest', and faster.  But if the mode does not use clear
	   separators (for example, if you collapse your braces onto
	   the preceding statement in C or Perl), `largest' is
	   probably the better alternative.

 function  A function that will be passed the beginning and ending
	   locations of the region in which to look for the section
	   separator.  At the very beginning of the attempt to align,
	   both of these parameters will be nil, in which case the
	   function should return non-nil if it wants each rule to
	   define its own section, or nil if it wants the largest
	   section found to be used as the common section for all
	   rules that occur there.

 list      A list of markers within the buffer that represent where
	   the section dividers lie.  Be certain to use markers!  For
	   when the aligning begins, the ensuing contract/expanding of
	   whitespace will throw off any non-marker positions.

	   This method is intended for use in Lisp programs, and not
	   by the user."
  :type '(choice
	  (const :tag "Entire region is one section" entire)
	  (const :tag "Align by contiguous groups" group)
;         (const largest)
	  (regexp :tag "Regexp defines section boundaries")
	  (function :tag "Function defines section boundaries"))
  :group 'align)

(put 'align-region-separate 'risky-local-variable t)

(defvar align-rules-list-type
  '(repeat
    (cons
     :tag "Alignment rule"
     (symbol :tag "Title")
     (cons :tag "Required attributes"
	   (cons :tag "Regexp"
		 (const :tag "(Regular expression to match)" regexp)
		 (choice :value "\\(\\s-+\\)" regexp function))
	   (repeat
	    :tag "Optional attributes"
	    (choice
	     (cons :tag "Repeat"
		   (const :tag "(Repeat this rule throughout line)"
			  repeat)
		   (boolean :value t))
	     (cons :tag "Paren group"
		   (const :tag "(Parenthesis group to use)" group)
		   (choice :value 2
			   integer (repeat integer)))
	     (cons :tag "Modes"
		   (const :tag "(Modes where this rule applies)" modes)
		   (sexp :value (text-mode)))
	     (cons :tag "Case-fold"
		   (const :tag "(Should case be ignored for this rule)"
			  case-fold)
		   (boolean :value t))
	     (cons :tag "To Tab Stop"
		   (const :tag "(Should rule align to tab stops)"
			  tab-stop)
		   (boolean :value nil))
	     (cons :tag "Valid"
		   (const :tag "(Return non-nil if rule is valid)"
			  valid)
		   (function :value t))
	     (cons :tag "Run If"
		   (const :tag "(Return non-nil if rule should run)"
			  run-if)
		   (function :value t))
	     (cons :tag "Column"
		   (const :tag "(Column to fix alignment at)" column)
		   (choice :value comment-column
			   integer symbol))
	     (cons :tag "Spacing"
		   (const :tag "(Amount of spacing to use)" spacing)
		   (integer :value 1))
	     (cons :tag "Justify"
		   (const :tag "(Should text be right justified)"
			  justify)
		   (boolean :value t))
	     ;; make sure this stays up-to-date with any changes
	     ;; in `align-region-separate'
	     (cons :tag "Separate"
		   (const :tag "(Separation to use for this rule)"
			  separate)
		   (choice :value "^\\s-*$"
			   (const entire)
			   (const group)
;                          (const largest)
			   regexp function)))))))
  "The `type' form for any `align-rules-list' variable.")

(defcustom align-rules-list
  `((lisp-second-arg
     (regexp   . "\\(^\\s-+[^( \t\n]\\|(\\(\\S-+\\)\\s-+\\)\\S-+\\(\\s-+\\)")
     (group    . 3)
     (modes    . align-lisp-modes)
     (run-if   . ,(function (lambda () current-prefix-arg))))

    (lisp-alist-dot
     (regexp   . "\\(\\s-*\\)\\.\\(\\s-*\\)")
     (group    . (1 2))
     (modes    . align-lisp-modes))

    (open-comment
     (regexp   . ,(function
		   (lambda (end reverse)
		     (funcall (if reverse 're-search-backward
				're-search-forward)
			      (concat "[^ \t\n\\\\]"
				      (regexp-quote comment-start)
				      "\\(.+\\)$") end t))))
     (modes    . align-open-comment-modes))

    (c-macro-definition
     (regexp   . "^\\s-*#\\s-*define\\s-+\\S-+\\(\\s-+\\)")
     (modes    . align-c++-modes))

    (c-variable-declaration
     (regexp   . ,(concat "[*&0-9A-Za-z_]>?[&*]*\\(\\s-+[*&]*\\)"
			  "[A-Za-z_][0-9A-Za-z:_]*\\s-*\\(\\()\\|"
			  "=[^=\n].*\\|(.*)\\|\\(\\[.*\\]\\)*\\)?"
			  "\\s-*[;,]\\|)\\s-*$\\)"))
     (group    . 1)
     (modes    . align-c++-modes)
     (justify  . t)
     (valid
      . ,(function
	  (lambda ()
	    (not (or (save-excursion
		       (goto-char (match-beginning 1))
		       (backward-word 1)
		       (looking-at
			"\\(goto\\|return\\|new\\|delete\\|throw\\)"))
		     (if (and (boundp 'font-lock-mode) font-lock-mode)
			 (eq (get-text-property (point) 'face)
			     'font-lock-comment-face)
		       (eq (caar (c-guess-basic-syntax)) 'c))))))))

    (c-assignment
     (regexp   . ,(concat "[^-=!^&*+<>/| \t\n]\\(\\s-*[-=!^&*+<>/|]*\\)"
			  "=\\(\\s-*\\)\\([^= \t\n]\\|$\\)"))
     (group    . (1 2))
     (modes    . align-c++-modes)
     (justify  . t)
     (tab-stop . nil))

    (perl-assignment
     (regexp   . ,(concat "[^=!^&*-+<>/| \t\n]\\(\\s-*\\)=[~>]?"
			  "\\(\\s-*\\)\\([^>= \t\n]\\|$\\)"))
     (group    . (1 2))
     (modes    . align-perl-modes)
     (tab-stop . nil))

    (python-assignment
     (regexp   . ,(concat "[^=!<> \t\n]\\(\\s-*\\)="
			  "\\(\\s-*\\)\\([^>= \t\n]\\|$\\)"))
     (group    . (1 2))
     (modes    . '(python-mode))
     (tab-stop . nil))

    (make-assignment
     (regexp   . "^\\s-*\\w+\\(\\s-*\\):?=\\(\\s-*\\)\\([^\t\n \\\\]\\|$\\)")
     (group    . (1 2))
     (modes    . '(makefile-mode))
     (tab-stop . nil))

    (c-comma-delimiter
     (regexp   . ",\\(\\s-*\\)[^/ \t\n]")
     (repeat   . t)
     (modes    . align-c++-modes)
     (run-if   . ,(function (lambda () current-prefix-arg))))
					;      (valid
					;       . ,(function
					;	  (lambda ()
					;	    (memq (caar (c-guess-basic-syntax))
					;		  '(brace-list-intro
					;		    brace-list-entry
					;		    brace-entry-open))))))

    ;; With a prefix argument, comma delimiter will be aligned.  Since
    ;; perl-mode doesn't give us enough syntactic information (and we
    ;; don't do our own parsing yet), this rule is too destructive to
    ;; run normally.
    (basic-comma-delimiter
     (regexp   . ",\\(\\s-*\\)[^# \t\n]")
     (repeat   . t)
     (modes    . (append align-perl-modes '(python-mode)))
     (run-if   . ,(function (lambda () current-prefix-arg))))

    (c++-comment
     (regexp   . "\\(\\s-*\\)\\(//.*\\|/\\*.*\\*/\\s-*\\)$")
     (modes    . align-c++-modes)
     (column   . comment-column)
     (valid    . ,(function
		   (lambda ()
		     (save-excursion
		       (goto-char (match-beginning 1))
		       (not (bolp)))))))

    (c-chain-logic
     (regexp   . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
     (modes    . align-c++-modes)
     (valid    . ,(function
		   (lambda ()
		     (save-excursion
		       (goto-char (match-end 2))
		       (looking-at "\\s-*\\(/[*/]\\|$\\)"))))))

    (perl-chain-logic
     (regexp   . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
     (modes    . align-perl-modes)
     (valid    . ,(function
		   (lambda ()
		     (save-excursion
		       (goto-char (match-end 2))
		       (looking-at "\\s-*\\(#\\|$\\)"))))))

    (python-chain-logic
     (regexp   . "\\(\\s-*\\)\\(\\<and\\>\\|\\<or\\>\\)")
     (modes    . '(python-mode))
     (valid    . ,(function
		   (lambda ()
		     (save-excursion
		       (goto-char (match-end 2))
		       (looking-at "\\s-*\\(#\\|$\\|\\\\\\)"))))))

    (c-macro-line-continuation
     (regexp   . "\\(\\s-*\\)\\\\$")
     (modes    . align-c++-modes)
     (column   . c-backslash-column))
					;      (valid
					;       . ,(function
					;	  (lambda ()
					;	    (memq (caar (c-guess-basic-syntax))
					;		  '(cpp-macro cpp-macro-cont))))))

    (basic-line-continuation
     (regexp   . "\\(\\s-*\\)\\\\$")
     (modes    . '(python-mode makefile-mode)))

    (tex-record-separator
     (regexp . ,(function
		 (lambda (end reverse)
		   (align-match-tex-pattern "&" end reverse))))
     (group    . (1 2))
     (modes    . align-tex-modes)
     (repeat   . t))

    (tex-tabbing-separator
     (regexp   . ,(function
		   (lambda (end reverse)
		     (align-match-tex-pattern "\\\\[=>]" end reverse))))
     (group    . (1 2))
     (modes    . align-tex-modes)
     (repeat   . t)
     (run-if   . ,(function
		   (lambda ()
		     (eq major-mode 'latex-mode)))))

    (tex-record-break
     (regexp   . "\\(\\s-*\\)\\\\\\\\")
     (modes    . align-tex-modes))

    ;; With a numeric prefix argument, or C-u, space delimited text
    ;; tables will be aligned.
    (text-column
     (regexp   . "\\(^\\|\\S-\\)\\([ \t]+\\)\\(\\S-\\|$\\)")
     (group    . 2)
     (modes    . align-text-modes)
     (repeat   . t)
     (run-if   . ,(function
		   (lambda ()
		     (and current-prefix-arg
			  (not (eq '- current-prefix-arg)))))))

    ;; With a negative prefix argument, lists of dollar figures will
    ;; be aligned.
    (text-dollar-figure
     (regexp   . "\\$?\\(\\s-+[0-9]+\\)\\.")
     (modes    . align-text-modes)
     (justify  . t)
     (run-if   . ,(function
		   (lambda ()
		     (eq '- current-prefix-arg)))))

    (css-declaration
     (regexp . "^\\s-*\\w+:\\(\\s-*\\).*;")
     (group . (1))
     (modes . '(css-mode html-mode))))
  "A list describing all of the available alignment rules.
The format is:

   ((TITLE
     (ATTRIBUTE . VALUE) ...)
    ...)

The following attributes are meaningful:

`regexp'    This required attribute must be either a string describing
	    a regular expression, or a function (described below).
	    For every line within the section that this regular
	    expression matches, the given rule will be applied to that
	    line.  The exclusion rules denote which part(s) of the
	    line should not be modified; the alignment rules cause the
	    identified whitespace group to be contracted/expanded such
	    that the \"alignment character\" (the character
	    immediately following the identified parenthesis group),
	    occurs in the same column for every line within the
	    alignment section (see `align-region-separate' for a
	    description of how the region is broken up into alignment
	    sections).

	    The `regexp' attribute describes how the text should be
	    treated.  Within this regexp, there must be at least one
	    group of characters (typically whitespace) identified by
	    the special opening and closing parens used in regexp
	    expressions (`\\\\(' and `\\\\)') (see the Emacs manual on
	    the syntax of regular expressions for more info).

	    If `regexp' is a function, it will be called as a
	    replacement for `re-search-forward'.  This means that it
	    should return nil if nothing is found to match the rule,
	    or it should set the match data appropriately, move point
	    to the end of the match, and return the value of point.

`group'     For exclusion rules, the group identifies the range of
	    characters that should be ignored.  For alignment rules,
	    these are the characters that will be deleted/expanded for
	    the purposes of alignment.  The \"alignment character\" is
	    always the first character immediately following this
	    parenthesis group.  This attribute may also be a list of
	    integers, in which case multiple alignment characters will
	    be aligned, with the list of integers identifying the
	    whitespace groups which precede them.  The default for
	    this attribute is 1.

`modes'     The `modes' attribute, if set, should name a list of
	    major modes -- or evaluate to such a value -- in which the
	    rule is valid.  If not set, the rule will apply to all
	    modes.

`case-fold' If `regexp' is an ordinary regular expression string
	    containing alphabetic character, sometimes you may want
	    the search to proceed case-insensitively (for languages
	    that ignore case, such as Pascal for example).  In that
	    case, set `case-fold' to a non-nil value, and the regular
	    expression search will ignore case.  If `regexp' is set to
	    a function, that function must handle the job of ignoring
	    case by itself.

`tab-stop'  If the `tab-stop' attribute is set, and non-nil, the
	    alignment character will always fall on a tab stop
	    (whether it uses tabs to get there or not depends on the
	    value of `indent-tabs-mode').  If the `tab-stop' attribute
	    is set to nil, tab stops will never be used.  Otherwise,
	    the value of `align-to-tab-stop' determines whether or not
	    to align to a tab stop.  The `tab-stop' attribute may also
	    be a list of t or nil values, corresponding to the number
	    of parenthesis groups specified by the `group' attribute.

`repeat'    If the `repeat' attribute is present, and non-nil, the
	    rule will be applied to the line continuously until no
	    further matches are found.

`valid'     If the `valid' attribute is set, it will be used to
	    determine whether the rule should be invoked.  This form
	    is evaluated after the regular expression match has been
	    performed, so that it is possible to use the results of
	    that match to determine whether the alignment should be
	    performed.  The buffer should not be modified during the
	    evaluation of this form.

`run-if'    Like `valid', the `run-if' attribute tests whether the
	    rule should be run at all -- even before any searches are
	    done to determine if the rule applies to the alignment
	    region.  This can save time, since `run-if' will only be
	    run once for each rule.  If it returns nil, the rule will
	    not be attempted.

`column'    For alignment rules, if the `column' attribute is set --
	    which must be an integer, or a symbol whose value is an
	    integer -- it will be used as the column in which to align
	    the alignment character.  If the text on a particular line
	    happens to overrun that column, a single space character,
	    or tab stop (see `align-to-tab-stop') will be added
	    between the last text character and the alignment
	    character.

`spacing'   Alignment rules may also override the amount of spacing
	    that would normally be used by providing a `spacing'
	    attribute.  This must be an integer, or a list of integers
	    corresponding to the number of parenthesis groups matched
	    by the `group' attribute.  If a list of value is used, and
	    any of those values is nil, `align-default-spacing' will
	    be used for that subgroup.  See `align-default-spacing'
	    for more details on spacing, tab stops, and how to
	    indicate how much spacing should be used.  If TAB-STOP is
	    present, it will override the value of `align-to-tab-stop'
	    for that rule.

`justify'   It is possible with `regexp' and `group' to identify a
	    character group that contains more than just whitespace
	    characters.  By default, any non-whitespace characters in
	    that group will also be deleted while aligning the
	    alignment character.  However, if the `justify' attribute
	    is set to a non-nil value, only the initial whitespace
	    characters within that group will be deleted.  This has
	    the effect of right-justifying the characters that remain,
	    and can be used for outdenting or just plain old right-
	    justification.

`separate'  Each rule can define its own section separator, which
	    describes how to identify the separation of \"sections\"
	    within the region to be aligned.  Setting the `separate'
	    attribute overrides the value of `align-region-separate'
	    (see the documentation of that variable for possible
	    values), and any separation argument passed to `align'."
  :type align-rules-list-type
  :group 'align)

(put 'align-rules-list 'risky-local-variable t)

(defvar align-exclude-rules-list-type
  '(repeat
    (cons
     :tag "Exclusion rule"
     (symbol :tag "Title")
     (cons :tag "Required attributes"
	   (cons :tag "Regexp"
		 (const :tag "(Regular expression to match)" regexp)
		 (choice :value "\\(\\s-+\\)" regexp function))
	   (repeat
	    :tag "Optional attributes"
	    (choice
	     (cons :tag "Repeat"
		   (const :tag "(Repeat this rule throughout line)"
			  repeat)
		   (boolean :value t))
	     (cons :tag "Paren group"
		   (const :tag "(Parenthesis group to use)" group)
		   (choice :value 2
			   integer (repeat integer)))
	     (cons :tag "Modes"
		   (const :tag "(Modes where this rule applies)" modes)
		   (sexp :value (text-mode)))
	     (cons :tag "Case-fold"
		   (const :tag "(Should case be ignored for this rule)"
			  case-fold)
		   (boolean :value t)))))))
  "The `type' form for any `align-exclude-rules-list' variable.")

(defcustom align-exclude-rules-list
  `((exc-dq-string
     (regexp . "\"\\([^\"\n]+\\)\"")
     (repeat . t)
     (modes  . align-dq-string-modes))

    (exc-sq-string
     (regexp . "'\\([^'\n]+\\)'")
     (repeat . t)
     (modes  . align-sq-string-modes))

    (exc-open-comment
     (regexp
      . ,(function
	  (lambda (end reverse)
	    (funcall (if reverse 're-search-backward
		       're-search-forward)
		     (concat "[^ \t\n\\\\]"
			     (regexp-quote comment-start)
			     "\\(.+\\)$") end t))))
     (modes  . align-open-comment-modes))

    (exc-c-comment
     (regexp . "/\\*\\(.+\\)\\*/")
     (repeat . t)
     (modes  . align-c++-modes))

    (exc-c-func-params
     (regexp . "(\\([^)\n]+\\))")
     (repeat . t)
     (modes  . align-c++-modes))

    (exc-c-macro
     (regexp . "^\\s-*#\\s-*\\(if\\w*\\|endif\\)\\(.*\\)$")
     (group  . 2)
     (modes  . align-c++-modes)))
  "A list describing text that should be excluded from alignment.
See the documentation for `align-rules-list' for more info."
  :type align-exclude-rules-list-type
  :group 'align)

(put 'align-exclude-rules-list 'risky-local-variable t)

;;; Internal Variables:

(defvar align-mode-rules-list nil
  "Alignment rules specific to the current major mode.
See the variable `align-rules-list' for more details.")

(make-variable-buffer-local 'align-mode-rules-list)

(defvar align-mode-exclude-rules-list nil
  "Alignment exclusion rules specific to the current major mode.
See the variable `align-exclude-rules-list' for more details.")

(make-variable-buffer-local 'align-mode-exclude-rules-list)

(defvar align-highlight-overlays nil
  "The current overlays highlighting the text matched by a rule.")

;; Sample extension rule set, for vhdl-mode.  This should properly be
;; in vhdl-mode.el itself.

(defcustom align-vhdl-rules-list
  `((vhdl-declaration
     (regexp   . "\\(signal\\|variable\\|constant\\)\\(\\s-+\\)\\S-")
     (group    . 2))

    (vhdl-case
     (regexp   . "\\(others\\|[^ \t\n=<]\\)\\(\\s-*\\)=>\\(\\s-*\\)\\S-")
     (group    . (2 3))
     (valid
      . ,(function
	  (lambda ()
	    (not (string= (downcase (match-string 1))
			  "others"))))))

    (vhdl-colon
     (regexp   . "[^ \t\n:]\\(\\s-*\\):\\(\\s-*\\)[^=\n]")
     (group    . (1 2)))

    (direction
     (regexp   . ":\\s-*\\(in\\|out\\|inout\\|buffer\\)\\(\\s-*\\)")
     (group    . 2))

    (sig-assign
     (regexp   . "[^ \t\n=<]\\(\\s-*\\)<=\\(\\s-*\\)\\S-")
     (group    . (1 2)))

    (var-assign
     (regexp   . "[^ \t\n:]\\(\\s-*\\):="))

    (use-entity
     (regexp   . "\\(\\s-+\\)use\\s-+entity")))
  "Alignment rules for `vhdl-mode'.  See `align-rules-list' for more info."
  :type align-rules-list-type
  :group 'align)

(put 'align-vhdl-rules-list 'risky-local-variable t)

(defun align-set-vhdl-rules ()
  "Setup the `align-mode-rules-list' variable for `vhdl-mode'."
  (setq align-mode-rules-list align-vhdl-rules-list))

(add-hook 'vhdl-mode-hook 'align-set-vhdl-rules)

(add-to-list 'align-dq-string-modes 'vhdl-mode)
(add-to-list 'align-open-comment-modes 'vhdl-mode)

;;; User Functions:

;;;###autoload
(defun align (beg end &optional separate rules exclude-rules)
  "Attempt to align a region based on a set of alignment rules.
BEG and END mark the region.  If BEG and END are specifically set to
nil (this can only be done programmatically), the beginning and end of
the current alignment section will be calculated based on the location
of point, and the value of `align-region-separate' (or possibly each
rule's `separate' attribute).

If SEPARATE is non-nil, it overrides the value of
`align-region-separate' for all rules, except those that have their
`separate' attribute set.

RULES and EXCLUDE-RULES, if either is non-nil, will replace the
default rule lists defined in `align-rules-list' and
`align-exclude-rules-list'.  See `align-rules-list' for more details
on the format of these lists."
  (interactive "r")
  (let ((separator
	 (or separate
	     (if (and (symbolp align-region-separate)
		      (boundp align-region-separate))
		 (symbol-value align-region-separate)
	       align-region-separate)
	     'entire)))
    (if (not (or ;(eq separator 'largest)
		 (and (functionp separator)
		      (not (funcall separator nil nil)))))
	(align-region beg end separator
		      (or rules align-mode-rules-list align-rules-list)
		      (or exclude-rules align-mode-exclude-rules-list
			  align-exclude-rules-list))
      (let ((sec-first end)
	    (sec-last beg))
	(align-region beg end
		      (or exclude-rules
			  align-mode-exclude-rules-list
			  align-exclude-rules-list) nil
		      separator
		      (function
		       (lambda (b e mode)
			 (when (and mode (listp mode))
			   (setq sec-first (min sec-first b)
				 sec-last  (max sec-last e))))))
	(if (< sec-first sec-last)
	    (align-region sec-first sec-last 'entire
			  (or rules align-mode-rules-list align-rules-list)
			  (or exclude-rules align-mode-exclude-rules-list
			      align-exclude-rules-list)))))))

;;;###autoload
(defun align-regexp (beg end regexp &optional group spacing repeat)
  "Align the current region using an ad-hoc rule read from the minibuffer.
BEG and END mark the limits of the region.  This function will prompt
for the REGEXP to align with.  If no prefix arg was specified, you
only need to supply the characters to be lined up and any preceding
whitespace is replaced.  If a prefix arg was specified, the full
regexp with parenthesized whitespace should be supplied; it will also
prompt for which parenthesis GROUP within REGEXP to modify, the amount
of SPACING to use, and whether or not to REPEAT the rule throughout
the line.  See `align-rules-list' for more information about these
options.

For example, let's say you had a list of phone numbers, and wanted to
align them so that the opening parentheses would line up:

    Fred (123) 456-7890
    Alice (123) 456-7890
    Mary-Anne (123) 456-7890
    Joe (123) 456-7890

There is no predefined rule to handle this, but you could easily do it
using a REGEXP like \"(\".  All you would have to do is to mark the
region, call `align-regexp' and type in that regular expression."
  (interactive
   (append
    (list (region-beginning) (region-end))
    (if current-prefix-arg
	(list (read-string "Complex align using regexp: "
			   "\\(\\s-*\\)")
	      (string-to-number
	       (read-string
		"Parenthesis group to modify (justify if negative): " "1"))
	      (string-to-number
	       (read-string "Amount of spacing (or column if negative): "
			    (number-to-string align-default-spacing)))
	      (y-or-n-p "Repeat throughout line? "))
      (list (concat "\\(\\s-*\\)"
		    (read-string "Align regexp: "))
	    1 align-default-spacing nil))))
  (or group (setq group 1))
  (or spacing (setq spacing align-default-spacing))
  (let ((rule
	 (list (list nil (cons 'regexp regexp)
		     (cons 'group (abs group))
		     (if (< group 0)
			 (cons 'justify t)
		       (cons 'bogus nil))
		     (if (>= spacing 0)
			 (cons 'spacing spacing)
		       (cons 'column (abs spacing)))
		     (cons 'repeat repeat)))))
    (align-region beg end 'entire rule nil nil)))

;;;###autoload
(defun align-entire (beg end &optional rules exclude-rules)
  "Align the selected region as if it were one alignment section.
BEG and END mark the extent of the region.  If RULES or EXCLUDE-RULES
is set to a list of rules (see `align-rules-list'), it can be used to
override the default alignment rules that would have been used to
align that section."
  (interactive "r")
  (align beg end 'entire rules exclude-rules))

;;;###autoload
(defun align-current (&optional rules exclude-rules)
  "Call `align' on the current alignment section.
This function assumes you want to align only the current section, and
so saves you from having to specify the region.  If RULES or
EXCLUDE-RULES is set to a list of rules (see `align-rules-list'), it
can be used to override the default alignment rules that would have
been used to align that section."
  (interactive)
  (align nil nil nil rules exclude-rules))

;;;###autoload
(defun align-highlight-rule (beg end title &optional rules exclude-rules)
  "Highlight the whitespace which a given rule would have modified.
BEG and END mark the extent of the region.  TITLE identifies the rule
that should be highlighted.  If RULES or EXCLUDE-RULES is set to a
list of rules (see `align-rules-list'), it can be used to override the
default alignment rules that would have been used to identify the text
to be colored."
  (interactive
   (list (region-beginning) (region-end)
	 (completing-read
	  "Title of rule to highlight: "
	  (mapcar
	   (function
	    (lambda (rule)
	      (list (symbol-name (car rule)))))
	   (append (or align-mode-rules-list align-rules-list)
		   (or align-mode-exclude-rules-list
		       align-exclude-rules-list))) nil t)))
  (let ((ex-rule (assq (intern title)
		       (or align-mode-exclude-rules-list
			   align-exclude-rules-list)))
	face)
    (align-unhighlight-rule)
    (align-region
     beg end 'entire
     (or rules (if ex-rule
		   (or exclude-rules align-mode-exclude-rules-list
		       align-exclude-rules-list)
		 (or align-mode-rules-list align-rules-list)))
     (unless ex-rule (or exclude-rules align-mode-exclude-rules-list
			 align-exclude-rules-list))
     (function
      (lambda (b e mode)
	(if (and mode (listp mode))
	    (if (equal (symbol-name (car mode)) title)
		(setq face (cons align-highlight-change-face
				 align-highlight-nochange-face))
	      (setq face nil))
	  (when face
	    (let ((overlay (make-overlay b e)))
	      (setq align-highlight-overlays
		    (cons overlay align-highlight-overlays))
	      (overlay-put overlay 'face
			   (if mode
			       (car face)
			     (cdr face)))))))))))

;;;###autoload
(defun align-unhighlight-rule ()
  "Remove any highlighting that was added by `align-highlight-rule'."
  (interactive)
  (while align-highlight-overlays
    (delete-overlay (car align-highlight-overlays))
    (setq align-highlight-overlays
	  (cdr align-highlight-overlays))))

;;;###autoload
(defun align-newline-and-indent ()
  "A replacement function for `newline-and-indent', aligning as it goes."
  (interactive)
  (let ((separate (or (if (and (symbolp align-region-separate)
			       (boundp align-region-separate))
			  (symbol-value align-region-separate)
			align-region-separate)
		      'entire))
	(end (point)))
    (call-interactively 'newline-and-indent)
    (save-excursion
      (forward-line -1)
      (while (not (or (bobp)
		      (align-new-section-p (point) end separate)))
	(forward-line -1))
      (align (point) end))))

;;; Internal Functions:

(defun align-match-tex-pattern (regexp end &optional reverse)
  "Match REGEXP in TeX mode, counting backslashes appropriately.
END denotes the end of the region to be searched, while REVERSE, if
non-nil, indicates that the search should proceed backward from the
current position."
  (let (result)
    (while
	(and (setq result
		   (funcall
		    (if reverse 're-search-backward
		      're-search-forward)
		    (concat "\\(\\s-*\\)" regexp
			    "\\(\\s-*\\)") end t))
	     (let ((pos (match-end 1))
		   (count 0))
	       (while (and (> pos (point-min))
			   (eq (char-before pos) ?\\))
		 (setq count (1+ count) pos (1- pos)))
	       (eq (mod count 2) 1))
	     (goto-char (match-beginning (if reverse 1 2)))))
    result))

(defun align-new-section-p (beg end separator)
  "Is there a section divider between BEG and END?
SEPARATOR specifies how to look for the section divider.  See the
documentation for `align-region-separate' for more details."
  (cond ((or (not separator)
	     (eq separator 'entire))
	 nil)
	((eq separator 'group)
	 (let ((amount 2))
	   (save-excursion
	     (goto-char end)
	     (if (bolp)
		 (setq amount 1)))
	   (> (count-lines beg end) amount)))
	((stringp separator)
	 (save-excursion
	   (goto-char beg)
	   (re-search-forward separator end t)))
	((functionp separator)
	 (funcall separator beg end))
	((listp separator)
	 (let ((seps separator) yes)
	   (while seps
	     (if (and (>= (car seps) beg)
		      (<= (car seps) end))
		 (setq yes t seps nil)
	     (setq seps (cdr seps))))
	   yes))))

(defun align-adjust-col-for-rule (column _rule spacing tab-stop)
  "Adjust COLUMN according to the given RULE.
SPACING specifies how much spacing to use.
TAB-STOP specifies whether SPACING refers to tab-stop boundaries."
  (unless spacing
    (setq spacing align-default-spacing))
  (if (<= spacing 0)
      column
    (if (not tab-stop)
	(+ column spacing)
      (let ((stops tab-stop-list))
	(while stops
	  (if (and (> (car stops) column)
		   (= (setq spacing (1- spacing)) 0))
	      (setq column (car stops)
		    stops nil)
	    (setq stops (cdr stops)))))
      column)))

(defsubst align-column (pos)
  "Given a position in the buffer, state what column it's in.
POS is the position whose column will be taken.  Note that this
function will change the location of point."
  (goto-char pos)
  (current-column))

(defsubst align-regions (regions props rule func)
  "Align the regions specified in REGIONS, a list of cons cells.
PROPS describes formatting features specific to the given regions.
RULE specifies exactly how to perform the alignments.
If FUNC is specified, it will be called with each region that would
have been aligned, rather than modifying the text."
  (while regions
    (save-excursion
      (align-areas (car regions) (car props) rule func))
    (setq regions (cdr regions)
	  props (cdr props))))

(defun align-areas (areas props rule func)
  "Given a list of AREAS and formatting PROPS, align according to RULE.
AREAS should be a list of cons cells containing beginning and ending
markers.  This function sweeps through all of the beginning markers,
finds out which one starts in the furthermost column, and then deletes
and inserts text such that all of the ending markers occur in the same
column.

If FUNC is non-nil, it will be called for each text region that would
have been aligned.  No changes will be made to the buffer."
  (let* ((column (cdr (assq 'column rule)))
	 (fixed (if (symbolp column)
		    (symbol-value column)
		  column))
	 (justify (cdr (assq 'justify rule)))
	 (col (or fixed 0))
	 (width 0)
	 ecol change)

    ;; Determine the alignment column.
    (let ((a areas))
      (while a
	(unless fixed
	  (setq col (max col (align-column (caar a)))))
	(unless change
	  (goto-char (cdar a))
	  (if ecol
	      (if (/= ecol (current-column))
		  (setq change t))
	    (setq ecol (current-column))))
	(when justify
	  (goto-char (caar a))
	  (if (and (re-search-forward "\\s-*" (cdar a) t)
		   (/= (point) (cdar a)))
	      (let ((bcol (current-column)))
		(setcdr (car a) (cons (point-marker) (cdar a)))
		(goto-char (cdr (cdar a)))
		(setq width (max width (- (current-column) bcol))))))
	(setq a (cdr a))))

    (unless fixed
      (setq col (+ (align-adjust-col-for-rule
		    col rule (car props) (cdr props)) width)))

    ;; Make all ending positions to occur in the goal column.  Since
    ;; the whitespace to be modified was already deleted by
    ;; `align-region', all we have to do here is indent.

    (unless change
      (setq change (and ecol (/= col ecol))))

    (when (or func change)
      (while areas
	(let ((area (car areas))
	      (gocol col) cur)
	  (when area
	    (if func
		(funcall func (car area) (cdr area) change)
	      (if (not (and justify
			    (consp (cdr area))))
		  (goto-char (cdr area))
		(goto-char (cddr area))
		(let ((ecol (current-column)))
		  (goto-char (cadr area))
		  (setq gocol (- col (- ecol (current-column))))))
	      (setq cur (current-column))
	      (cond ((< gocol 0) t)     ; don't do anything
		    ((= cur gocol) t)   ; don't need to
		    ((< cur gocol)      ; just add space
		     ;; FIXME: It is stated above that "...the
		     ;;	       whitespace to be modified was already
		     ;;	       deleted by `align-region', all we have
		     ;;	       to do here is indent."  However, this
		     ;;	       doesn't seem to be true, so we first
		     ;;	       delete the whitespace to avoid tabs
		     ;;	       after spaces.
		     (delete-horizontal-space t)
		     (indent-to gocol))
		    (t
		     ;; This code works around an oddity in the
		     ;; FORCE argument of `move-to-column', which
		     ;; tends to screw up markers if there is any
		     ;; tabbing.
		     (let ((endcol (align-column
				    (if (and justify
					     (consp (cdr area)))
					(cadr area)
				      (cdr area))))
			   (abuts (<= gocol
				      (align-column (car area)))))
		       (if abuts
			   (goto-char (car area))
			 (move-to-column gocol t))
		       (let ((here (point)))
			 (move-to-column endcol t)
			 (delete-region here (point))
			 (if abuts
			     (indent-to (align-adjust-col-for-rule
					 (current-column) rule
					 (car props) (cdr props)))))))))))
	(setq areas (cdr areas))))))

(defmacro align--set-marker (marker-var pos &optional type)
  "If MARKER-VAR is a marker, move it to position POS.
Otherwise, create a new marker at position POS, with type TYPE."
  `(if (markerp ,marker-var)
       (move-marker ,marker-var ,pos)
     (setq ,marker-var (copy-marker ,pos ,type))))

(defun align-region (beg end separate rules exclude-rules
			 &optional func)
  "Align a region based on a given set of alignment rules.
BEG and END specify the region to be aligned.  Either may be nil, in
which case the range will stop at the nearest section division (see
`align-region-separate', and `align-region-heuristic' for more
information').

The region will be divided into separate alignment sections based on
the value of SEPARATE.

RULES and EXCLUDE-RULES are a pair of lists describing how to align
the region, and which text areas within it should be excluded from
alignment.  See the `align-rules-list' for more information on the
required format of these two lists.

If FUNC is specified, no text will be modified.  What `align-region'
will do with the rules is to search for the alignment areas, as it
regularly would, taking account for exclusions, and then call FUNC,
first with the beginning and ending of the region to be aligned
according to that rule (this can be different for each rule, if BEG
and END were nil), and then with the beginning and ending of each
text region that the rule would have applied to.

The signature of FUNC should thus be:

 (defun my-align-function (beg end mode)
   \"If MODE is a rule (a list), return t if BEG to END are to be searched.
Otherwise BEG to END will be a region of text that matches the rule's
definition, and MODE will be non-nil if any changes are necessary.\"
   (unless (and mode (listp mode))
     (message \"Would have aligned from %d to %d...\" beg end)))

This feature (of passing a FUNC) is used internally to locate the
position of exclusion areas, but could also be used for any other
purpose where you might want to know where the regions that the
aligner would have dealt with are."
  (let ((end-mark (and end (copy-marker end t)))
	(real-beg beg)
	(report (and (not func) align-large-region beg end
		     (>= (- end beg) align-large-region)))
	(rule-index 1)
	(rule-count (length rules)))
    (if (and align-indent-before-aligning real-beg end-mark)
	(indent-region real-beg end-mark nil))
    (while rules
      (let* ((rule (car rules))
	     (run-if (assq 'run-if rule))
	     (modes (assq 'modes rule)))
	;; unless the `run-if' form tells us not to, look for the
	;; rule..
	(unless (or (and modes (not (memq major-mode
					  (eval (cdr modes)))))
		    (and run-if (not (funcall (cdr run-if)))))
	  (let* ((current-case-fold case-fold-search)
		 (case-fold (assq 'case-fold rule))
		 (regexp  (cdr (assq 'regexp rule)))
		 (regfunc (and (functionp regexp) regexp))
		 (rulesep (assq 'separate rule))
		 (thissep (if rulesep (cdr rulesep) separate))
		 same (eol 0)
		 search-start
		 group group-c
		 spacing spacing-c
		 tab-stop tab-stop-c
		 repeat repeat-c
		 valid valid-c
		 first
		 regions index
		 last-point b e
		 save-match-data
		 exclude-p
		 align-props)
	    (save-excursion
	      ;; if beg and end were not given, figure out what the
	      ;; current alignment region should be.  Depending on the
	      ;; value of `align-region-separate' it's possible for
	      ;; each rule to have its own definition of what that
	      ;; current alignment section is.
	      (if real-beg
		  (goto-char beg)
		(if (or (not thissep) (eq thissep 'entire))
		    (error "Cannot determine alignment region for '%s'"
			   (symbol-name (cdr (assq 'title rule)))))
		(beginning-of-line)
		(while (and (not (eobp))
			    (looking-at "^\\s-*$"))
		  (forward-line))
		(let* ((here (point))
		       (start here))
		  (while (and here
			      (let ((terminus
				     (and align-region-heuristic
					  (- (point)
					     align-region-heuristic))))
				(if regfunc
				    (funcall regfunc terminus t)
				  (re-search-backward regexp
						      terminus t))))
		    (if (align-new-section-p (point) here thissep)
			(setq beg here
			      here nil)
		      (setq here (point))))
		  (if (not here)
		      (goto-char beg))
		  (beginning-of-line)
		  (setq beg (point))
		  (goto-char start)
		  (setq here (point))
		  (while (and here
			      (let ((terminus
				     (and align-region-heuristic
					  (+ (point)
					     align-region-heuristic))))
				(if regfunc
				    (funcall regfunc terminus nil)
				  (re-search-forward regexp terminus t))))
		    (if (align-new-section-p here (point) thissep)
			(setq end here
			      here nil)
		      (setq here (point))))
		  (if (not here)
		      (goto-char end))
		  (forward-line)
		  (setq end (point))
                  (align--set-marker end-mark end t)
		  (goto-char beg)))

	      ;; If we have a region to align, and `func' is set and
	      ;; reports back that the region is ok, then align it.
	      (when (or (not func)
			(funcall func beg end rule))
		(unwind-protect
		    (let (exclude-areas)
		      ;; determine first of all where the exclusions
		      ;; lie in this region
		      (when exclude-rules
			;; guard against a problem with recursion and
			;; dynamic binding vs. lexical binding, since
			;; the call to `align-region' below will
			;; re-enter this function, and rebind
			;; `exclude-areas'
			(set (setq exclude-areas
				   (make-symbol "align-exclude-areas"))
			     nil)
			(align-region
			 beg end 'entire
			 exclude-rules nil
			 `(lambda (b e mode)
			    (or (and mode (listp mode))
				(set (quote ,exclude-areas)
				     (cons (cons b e)
					   ,exclude-areas)))))
			(setq exclude-areas
			      (sort (symbol-value exclude-areas)
				    (function
				     (lambda (l r)
				       (>= (car l) (car r)))))))

		      ;; set `case-fold-search' according to the
		      ;; (optional) `case-fold' property
		      (and case-fold
			   (setq case-fold-search (cdr case-fold)))

		      ;; while we can find the rule in the alignment
		      ;; region..
		      (while (and (< (point) end-mark)
				  (setq search-start (point))
				  (if regfunc
				      (funcall regfunc end-mark nil)
				    (re-search-forward regexp
						       end-mark t)))

			;; give the user some indication of where we
			;; are, if it's a very large region being
			;; aligned
			(if report
			    (let ((symbol (car rule)))
			      (if (and symbol (symbolp symbol))
				  (message
				   "Aligning `%s' (rule %d of %d) %d%%..."
				   (symbol-name symbol) rule-index rule-count
				   (/ (* (- (point) real-beg) 100)
				      (- end-mark real-beg)))
				(message
				 "Aligning %d%%..."
				 (/ (* (- (point) real-beg) 100)
				    (- end-mark real-beg))))))

			;; if the search ended us on the beginning of
			;; the next line, move back to the end of the
			;; previous line.
			(if (and (bolp) (> (point) search-start))
			    (forward-char -1))

			;; lookup the `group' attribute the first time
			;; that we need it
			(unless group-c
			  (setq group (or (cdr (assq 'group rule)) 1))
			  (if (listp group)
			      (setq first (car group))
			    (setq first group group (list group)))
			  (setq group-c t))

			(unless spacing-c
			  (setq spacing (cdr (assq 'spacing rule))
				spacing-c t))

			(unless tab-stop-c
			  (setq tab-stop
				(let ((rule-ts (assq 'tab-stop rule)))
				  (if rule-ts
				      (cdr rule-ts)
				    (if (symbolp align-to-tab-stop)
					(symbol-value align-to-tab-stop)
				      align-to-tab-stop)))
				tab-stop-c t))

			;; test whether we have found a match on the same
			;; line as a previous match
			(if (> (point) eol)
			    (progn
                              (setq same nil)
                              (align--set-marker eol (line-end-position))))

			;; lookup the `repeat' attribute the first time
			(or repeat-c
			    (setq repeat (cdr (assq 'repeat rule))
				  repeat-c t))

			;; lookup the `valid' attribute the first time
			(or valid-c
			    (setq valid (assq 'valid rule)
				  valid-c t))

			;; remember the beginning position of this rule
			;; match, and save the match-data, since either
			;; the `valid' form, or the code that searches for
			;; section separation, might alter it
			(setq b (match-beginning first)
			      save-match-data (match-data))

			;; unless the `valid' attribute is set, and tells
			;; us that the rule is not valid at this point in
			;; the code..
			(unless (and valid (not (funcall (cdr valid))))

			  ;; look to see if this match begins a new
			  ;; section.  If so, we should align what we've
			  ;; collected so far, and then begin collecting
			  ;; anew for the next alignment section
			  (if (and last-point
				   (align-new-section-p last-point b
							thissep))
			      (progn
				(align-regions regions align-props
					       rule func)
				(setq regions nil)
                                (setq align-props nil)))
                          (align--set-marker last-point b t)

			  ;; restore the match data
			  (set-match-data save-match-data)

			  ;; check whether the region to be aligned
			  ;; straddles an exclusion area
			  (let ((excls exclude-areas))
			    (setq exclude-p nil)
			    (while excls
			      (if (and (< (match-beginning (car group))
					  (cdar excls))
				       (> (match-end (car (last group)))
					  (caar excls)))
				  (setq exclude-p t
					excls nil)
				(setq excls (cdr excls)))))

			  ;; go through the list of parenthesis groups
			  ;; matching whitespace text to be
			  ;; contracted/expanded (or possibly
			  ;; justified, if the `justify' attribute was
			  ;; set)
			  (unless exclude-p
			    (let ((g group))
			      (while g

				;; we have to use markers, since
				;; `align-areas' may modify the buffer
				(setq b (copy-marker
					 (match-beginning (car g)) t)
				      e (copy-marker (match-end (car g)) t))

				;; record this text region for alignment
				(setq index (if same (1+ index) 0))
				(let ((region (cons b e))
				      (props (cons
					      (if (listp spacing)
						  (car spacing)
						spacing)
					      (if (listp tab-stop)
						  (car tab-stop)
						tab-stop))))
				  (if (nth index regions)
				      (setcar (nthcdr index regions)
					      (cons region
						    (nth index regions)))
				    (if regions
					(progn
					  (nconc regions
						 (list (list region)))
					  (nconc align-props (list props)))
				      (setq regions
					    (list (list region)))
				      (setq align-props (list props)))))

				;; if any further rule matches are
				;; found before `eol', then they are
				;; on the same line as this one; this
				;; can only happen if the `repeat'
				;; attribute is non-nil
				(if (listp spacing)
				    (setq spacing (cdr spacing)))
				(if (listp tab-stop)
				    (setq tab-stop (cdr tab-stop)))
				(setq same t g (cdr g))))

			    ;; if `repeat' has not been set, move to
			    ;; the next line; don't bother searching
			    ;; anymore on this one
			    (if (and (not repeat) (not (bolp)))
				(forward-line))

			    ;; if the search did not change point,
			    ;; move forward to avoid an infinite loop
			    (if (= (point) search-start)
				(forward-char)))))

		      ;; when they are no more matches for this rule,
		      ;; align whatever was left over
		      (if regions
			  (align-regions regions align-props rule func)))

		  (setq case-fold-search current-case-fold)))))))
      (setq rules (cdr rules)
	    rule-index (1+ rule-index)))

    (if report
	(message "Aligning...done"))))

;; Provide:

(provide 'align)

(run-hooks 'align-load-hook)

;;; align.el ends here
