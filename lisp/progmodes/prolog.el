;;; prolog.el --- major mode for editing and running Prolog (and Mercury) code

;; Copyright (C) 1986-1987, 1997-1999, 2002-2003, 2011-2012
;;   Free Software Foundation, Inc.

;; Authors: Emil Åström <emil_astrom(at)hotmail(dot)com>
;;          Milan Zamazal <pdm(at)freesoft(dot)cz>
;;          Stefan Bruda <stefan(at)bruda(dot)ca>
;;          * See below for more details
;; Maintainer: Stefan Bruda <stefan(at)bruda(dot)ca>
;; Keywords: prolog major mode sicstus swi mercury

(defvar prolog-mode-version "1.22"
  "Prolog mode version number.")

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

;; Original author: Masanobu UMEDA <umerin(at)mse(dot)kyutech(dot)ac(dot)jp>
;; Parts of this file was taken from a modified version of the original
;; by Johan Andersson, Peter Olin, Mats Carlsson, Johan Bevemyr, Stefan
;; Andersson, and Per Danielsson (all SICS people), and Henrik Båkman
;; at Uppsala University, Sweden.
;;
;; Some ideas and also a few lines of code have been borrowed (not stolen ;-)
;; from Oz.el, the Emacs major mode for the Oz programming language,
;; Copyright (C) 1993 DFKI GmbH, Germany, with permission.
;; Authored by Ralf Scheidhauer and Michael Mehl
;;   ([scheidhr|mehl](at)dfki(dot)uni-sb(dot)de)
;;
;; More ideas and code have been taken from the SICStus debugger mode
;; (http://www.csd.uu.se/~perm/source_debug/index.shtml -- broken link
;; as of Mon May 5 08:23:48 EDT 2003) by Per Mildner.
;;
;; Additions for ECLiPSe and other helpful suggestions: Stephan Heuel
;; <heuel(at)ipb(dot)uni-bonn(dot)de>

;;; Commentary:
;;
;; This package provides a major mode for editing Prolog code, with
;; all the bells and whistles one would expect, including syntax
;; highlighting and auto indentation.  It can also send regions to an
;; inferior Prolog process.
;;
;; The code requires the comint, easymenu, info, imenu, and font-lock
;; libraries.  These are normally distributed with GNU Emacs and
;; XEmacs.

;;; Installation:
;;
;; Insert the following lines in your init file--typically ~/.emacs
;; (GNU Emacs and XEmacs <21.4), or ~/.xemacs/init.el (XEmacs
;; 21.4)--to use this mode when editing Prolog files under Emacs:
;;
;; (setq load-path (cons "/usr/lib/xemacs/site-lisp" load-path))
;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
;; (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
;; (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
;; (setq prolog-system 'swi)  ; optional, the system you are using;
;;                            ; see `prolog-system' below for possible values
;; (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
;;                                 ("\\.m$" . mercury-mode))
;;                                auto-mode-alist))
;;
;; where the path in the first line is the file system path to this file.
;; MSDOS paths can be written like "d:/programs/emacs-19.34/site-lisp".
;; Note: In XEmacs, either `/usr/lib/xemacs/site-lisp' (RPM default in
;; Red Hat-based distributions) or `/usr/local/lib/xemacs/site-lisp'
;; (default when compiling from sources) are automatically added to
;; `load-path', so the first line is not necessary provided that you
;; put this file in the appropriate place.
;;
;; The last s-expression above makes sure that files ending with .pl
;; are assumed to be Prolog files and not Perl, which is the default
;; Emacs setting.  If this is not wanted, remove this line.  It is then
;; necessary to either
;;
;;  o  insert in your Prolog files the following comment as the first line:
;;
;;       % -*- Mode: Prolog -*-
;;
;;     and then the file will be open in Prolog mode no matter its
;;     extension, or
;;
;;  o  manually switch to prolog mode after opening a Prolog file, by typing
;;     M-x prolog-mode.
;;
;; If the command to start the prolog process ('sicstus', 'pl' or
;; 'swipl' for SWI prolog, etc.) is not available in the default path,
;; then it is necessary to set the value of the environment variable
;; EPROLOG to a shell command to invoke the prolog process.  In XEmacs
;; and Emacs 20+ you can also customize the variable
;; `prolog-program-name' (in the group `prolog-inferior') and provide
;; a full path for your Prolog system (swi, scitus, etc.).
;;
;; Note: I (Stefan, the current maintainer) work under XEmacs.  Future
;;   developments will thus be biased towards XEmacs (OK, I admit it,
;;   I am biased towards XEmacs in general), though I will do my best
;;   to keep the GNU Emacs compatibility.  So if you work under Emacs
;;   and see something that does not work do drop me a line, as I have
;;   a smaller chance to notice this kind of bugs otherwise.

;; Changelog:

;; Version 1.22:
;;  o  Allowed both 'swipl' and 'pl' as names for the SWI Prolog
;;     interpreter.
;;  o  Atoms that start a line are not blindly colored as
;;     predicates.  Instead we check that they are followed by ( or
;;     :- first.  Patch suggested by Guy Wiener.
;; Version 1.21:
;;  o  Cleaned up the code that defines faces.  The missing face
;;     warnings on some Emacsen should disappear.
;; Version 1.20:
;;  o  Improved the handling of clause start detection and multi-line
;;     comments: `prolog-clause-start' no longer finds non-predicate
;;     (e.g., capitalized strings) beginning of clauses.
;;     `prolog-tokenize' recognizes when the end point is within a
;;     multi-line comment.
;; Version 1.19:
;;  o  Minimal changes for Aquamacs inclusion and in general for
;;     better coping with finding the Prolog executable.  Patch
;;     provided by David Reitter
;; Version 1.18:
;;  o  Fixed syntax highlighting for clause heads that do not begin at
;;     the beginning of the line.
;;  o  Fixed compilation warnings under Emacs.
;;  o  Updated the email address of the current maintainer.
;; Version 1.17:
;;  o  Minor indentation fix (patch by Markus Triska)
;;  o  `prolog-underscore-wordchar-flag' defaults now to nil (more
;;     consistent to other Emacs modes)
;; Version 1.16:
;;  o  Eliminated a possible compilation warning.
;; Version 1.15:
;;  o  Introduced three new customizable variables: electric colon
;;     (`prolog-electric-colon-flag', default nil), electric dash
;;     (`prolog-electric-dash-flag', default nil), and a possibility
;;     to prevent the predicate template insertion from adding commas
;;     (`prolog-electric-dot-full-predicate-template', defaults to t
;;     since it seems quicker to me to just type those commas).  A
;;     trivial adaptation of a patch by Markus Triska.
;;  o  Improved the behavior of electric if-then-else to only skip
;;     forward if the parenthesis/semicolon is preceded by
;;     whitespace.  Once more a trivial adaptation of a patch by
;;     Markus Triska.
;; Version 1.14:
;;  o  Cleaned up align code.  `prolog-align-flag' is eliminated (since
;;     on a second thought it does not do anything useful).  Added key
;;     binding (C-c C-a) and menu entry for alignment.
;;  o  Condensed regular expressions for lower and upper case
;;     characters (GNU Emacs seems to go over the regexp length limit
;;     with the original form).  My code on the matter was improved
;;     considerably by Markus Triska.
;;  o  Fixed `prolog-insert-spaces-after-paren' (which used an
;;     uninitialized variable).
;;  o  Minor changes to clean up the code and avoid some implicit
;;     package requirements.
;; Version 1.13:
;;  o  Removed the use of `map-char-table' in `prolog-build-case-strings'
;;     which appears to cause problems in (at least) Emacs 23.0.0.1.
;;  o  Added if-then-else indentation + corresponding electric
;;     characters.  New customization: `prolog-electric-if-then-else-flag'
;;  o  Align support (requires `align').  New customization:
;;     `prolog-align-flag'.
;;  o  Temporary consult files have now the same name throughout the
;;     session.  This prevents issues with reconsulting a buffer
;;     (this event is no longer passed to Prolog as a request to
;;     consult a new file).
;;  o  Adaptive fill mode is now turned on.  Comment indentation is
;;     still worse than it could be though, I am working on it.
;;  o  Improved filling and auto-filling capabilities.  Now block
;;     comments should be [auto-]filled correctly most of the time;
;;     the following pattern in particular is worth noting as being
;;     filled correctly:
;;         <some code here> % some comment here that goes beyond the
;;                          % rightmost column, possibly combined with
;;                          % subsequent comment lines
;;  o  `prolog-char-quote-workaround' now defaults to nil.
;;  o  Note: Many of the above improvements have been suggested by
;;     Markus Triska, who also provided useful patches on the matter
;;     when he realized that I was slow in responding.  Many thanks.
;; Version 1.11 / 1.12
;;  o  GNU Emacs compatibility fix for paragraph filling (fixed
;;     incorrectly in 1.11, fix fixed in 1.12).
;; Version 1.10
;;  o  Added paragraph filling in comment blocks and also correct auto
;;     filling for comments.
;;  o  Fixed the possible "Regular expression too big" error in
;;     `prolog-electric-dot'.
;; Version 1.9
;;  o  Parenthesis expressions are now indented by default so that
;;     components go one underneath the other, just as for compound
;;     terms.  You can use the old style (the second and subsequent
;;     lines being indented to the right in a parenthesis expression)
;;     by setting the customizable variable `prolog-paren-indent-p'
;;     (group "Prolog Indentation") to t.
;;  o  (Somehow awkward) handling of the 0' character escape
;;     sequence.  I am looking into a better way of doing it but
;;     prospects look bleak.  If this breaks things for you please let
;;     me know and also set the `prolog-char-quote-workaround' (group
;;     "Prolog Other") to nil.
;; Version 1.8
;;  o  Key binding fix.
;; Version 1.7
;;  o  Fixed a number of issues with the syntax of single quotes,
;;     including Debian bug #324520.
;; Version 1.6
;;  o  Fixed mercury mode menu initialization (Debian bug #226121).
;;  o  Fixed (i.e., eliminated) Delete remapping (Debian bug #229636).
;;  o  Corrected indentation for clauses defining quoted atoms.
;; Version 1.5:
;;  o  Keywords fontifying should work in console mode so this is
;;     enabled everywhere.
;; Version 1.4:
;;  o  Now supports GNU Prolog--minor adaptation of a patch by Stefan
;;     Moeding.
;; Version 1.3:
;;  o  Info-follow-nearest-node now called correctly under Emacs too
;;     (thanks to Nicolas Pelletier).  Should be implemented more
;;     elegantly (i.e., without compilation warnings) in the future.
;; Version 1.2:
;;  o  Another prompt fix, still in SWI mode (people seem to have
;;     changed the prompt of SWI Prolog).
;; Version 1.1:
;;  o  Fixed dots in the end of line comments causing indentation
;;     problems.  The following code is now correctly indented (note
;;     the dot terminating the comment):
;;        a(X) :- b(X),
;;            c(X).                  % comment here.
;;        a(X).
;;     and so is this (and variants):
;;        a(X) :- b(X),
;;            c(X).                  /* comment here.  */
;;        a(X).
;; Version 1.0:
;;  o  Revamped the menu system.
;;  o  Yet another prompt recognition fix (SWI mode).
;;  o  This is more of a renumbering than a new edition.  I promoted
;;     the mode to version 1.0 to emphasize the fact that it is now
;;     mature and stable enough to be considered production (in my
;;     opinion anyway).
;; Version 0.1.41:
;;  o  GNU Emacs compatibility fixes.
;; Version 0.1.40:
;;  o  prolog-get-predspec is now suitable to be called as
;;     imenu-extract-index-name-function.  The predicate index works.
;;  o  Since imenu works now as advertised, prolog-imenu-flag is t
;;     by default.
;;  o  Eliminated prolog-create-predicate-index since the imenu
;;     utilities now work well.  Actually, this function is also
;;     buggy, and I see no reason to fix it since we do not need it
;;     anyway.
;;  o  Fixed prolog-pred-start, prolog-clause-start, prolog-clause-info.
;;  o  Fix for prolog-build-case-strings; now prolog-upper-case-string
;;     and prolog-lower-case-string are correctly initialized,
;;  o  Various font-lock changes; most importantly, block comments (/*
;;     ... */) are now correctly fontified in XEmacs even when they
;;     extend on multiple lines.
;; Version 0.1.36:
;;  o  The debug prompt of SWI Prolog is now correctly recognized.
;; Version 0.1.35:
;;  o  Minor font-lock bug fixes.

;;; TODO:

;; Replace ":type 'sexp" with more precise Custom types.

;;; Code:

(eval-when-compile
  (require 'font-lock)
  ;; We need imenu everywhere because of the predicate index!
  (require 'imenu)
  ;)
  (require 'info)
  (require 'shell)
  )

(require 'comint)
(require 'easymenu)
(require 'align)


(defgroup prolog nil
  "Major modes for editing and running Prolog and Mercury files."
  :group 'languages)

(defgroup prolog-faces nil
  "Prolog mode specific faces."
  :group 'font-lock)

(defgroup prolog-indentation nil
  "Prolog mode indentation configuration."
  :group 'prolog)

(defgroup prolog-font-lock nil
  "Prolog mode font locking patterns."
  :group 'prolog)

(defgroup prolog-keyboard nil
  "Prolog mode keyboard flags."
  :group 'prolog)

(defgroup prolog-inferior nil
  "Inferior Prolog mode options."
  :group 'prolog)

(defgroup prolog-other nil
  "Other Prolog mode options."
  :group 'prolog)


;;-------------------------------------------------------------------
;; User configurable variables
;;-------------------------------------------------------------------

;; General configuration

(defcustom prolog-system nil
  "Prolog interpreter/compiler used.
The value of this variable is nil or a symbol.
If it is a symbol, it determines default values of other configuration
variables with respect to properties of the specified Prolog
interpreter/compiler.

Currently recognized symbol values are:
eclipse - Eclipse Prolog
mercury - Mercury
sicstus - SICStus Prolog
swi     - SWI Prolog
gnu     - GNU Prolog"
  :version "24.1"
  :group 'prolog
  :type '(choice (const :tag "SICStus" :value sicstus)
                 (const :tag "SWI Prolog" :value swi)
                 (const :tag "GNU Prolog" :value gnu)
                 (const :tag "ECLiPSe Prolog" :value eclipse)
                 ;; Mercury shouldn't be needed since we have a separate
                 ;; major mode for it.
                 (const :tag "Default" :value nil)))
(make-variable-buffer-local 'prolog-system)

;; NB: This alist can not be processed in prolog-mode-variables to
;; create a prolog-system-version-i variable since it is needed
;; prior to the call to prolog-mode-variables.
(defcustom prolog-system-version
  '((sicstus  (3 . 6))
    (swi      (0 . 0))
    (mercury  (0 . 0))
    (eclipse  (3 . 7))
    (gnu      (0 . 0)))
  ;; FIXME: This should be auto-detected instead of user-provided.
  "Alist of Prolog system versions.
The version numbers are of the format (Major . Minor)."
  :version "24.1"
  :type '(repeat (list (symbol :tag "System")
                       (cons :tag "Version numbers" (integer :tag "Major")
                             (integer :tag "Minor"))))
  :group 'prolog)

;; Indentation

(defcustom prolog-indent-width 4
  "The indentation width used by the editing buffer."
  :group 'prolog-indentation
  :type 'integer)

(defcustom prolog-align-comments-flag t
  "Non-nil means automatically align comments when indenting."
  :version "24.1"
  :group 'prolog-indentation
  :type 'boolean)

(defcustom prolog-indent-mline-comments-flag t
  "Non-nil means indent contents of /* */ comments.
Otherwise leave such lines as they are."
  :version "24.1"
  :group 'prolog-indentation
  :type 'boolean)

(defcustom prolog-object-end-to-0-flag t
  "Non-nil means indent closing '}' in SICStus object definitions to level 0.
Otherwise indent to `prolog-indent-width'."
  :version "24.1"
  :group 'prolog-indentation
  :type 'boolean)

(defcustom prolog-left-indent-regexp "\\(;\\|\\*?->\\)"
  "Regexp for character sequences after which next line is indented.
Next line after such a regexp is indented to the opening parenthesis level."
  :version "24.1"
  :group 'prolog-indentation
  :type 'regexp)

(defcustom prolog-paren-indent-p nil
  "If non-nil, increase indentation for parenthesis expressions.
The second and subsequent line in a parenthesis expression other than
a compound term can either be indented `prolog-paren-indent' to the
right (if this variable is non-nil) or in the same way as for compound
terms (if this variable is nil, default)."
  :version "24.1"
  :group 'prolog-indentation
  :type 'boolean)

(defcustom prolog-paren-indent 4
  "The indentation increase for parenthesis expressions.
Only used in ( If -> Then ; Else) and ( Disj1 ; Disj2 ) style expressions."
  :version "24.1"
  :group 'prolog-indentation
  :type 'integer)

(defcustom prolog-parse-mode 'beg-of-clause
  "The parse mode used (decides from which point parsing is done).
Legal values:
'beg-of-line   - starts parsing at the beginning of a line, unless the
                 previous line ends with a backslash.  Fast, but has
                 problems detecting multiline /* */ comments.
'beg-of-clause - starts parsing at the beginning of the current clause.
                 Slow, but copes better with /* */ comments."
  :version "24.1"
  :group 'prolog-indentation
  :type '(choice (const :value beg-of-line)
                 (const :value beg-of-clause)))

;; Font locking

(defcustom prolog-keywords
  '((eclipse
     ("use_module" "begin_module" "module_interface" "dynamic"
      "external" "export" "dbgcomp" "nodbgcomp" "compile"))
    (mercury
     ("all" "else" "end_module" "equality" "external" "fail" "func" "if"
      "implementation" "import_module" "include_module" "inst" "instance"
      "interface" "mode" "module" "not" "pragma" "pred" "some" "then" "true"
      "type" "typeclass" "use_module" "where"))
    (sicstus
     ("block" "dynamic" "mode" "module" "multifile" "meta_predicate"
      "parallel" "public" "sequential" "volatile"))
    (swi
     ("discontiguous" "dynamic" "ensure_loaded" "export" "export_list" "import"
      "meta_predicate" "module" "module_transparent" "multifile" "require"
      "use_module" "volatile"))
    (gnu
     ("built_in" "char_conversion" "discontiguous" "dynamic" "ensure_linked"
      "ensure_loaded" "foreign" "include" "initialization" "multifile" "op"
      "public" "set_prolog_flag"))
    (t
     ;; FIXME: Shouldn't we just use the union of all the above here?
     ("dynamic" "module")))
  "Alist of Prolog keywords which is used for font locking of directives."
  :version "24.1"
  :group 'prolog-font-lock
  :type 'sexp)

(defcustom prolog-types
  '((mercury
     ("char" "float" "int" "io__state" "string" "univ"))
    (t nil))
  "Alist of Prolog types used by font locking."
  :version "24.1"
  :group 'prolog-font-lock
  :type 'sexp)

(defcustom prolog-mode-specificators
  '((mercury
     ("bound" "di" "free" "ground" "in" "mdi" "mui" "muo" "out" "ui" "uo"))
    (t nil))
  "Alist of Prolog mode specificators used by font locking."
  :version "24.1"
  :group 'prolog-font-lock
  :type 'sexp)

(defcustom prolog-determinism-specificators
  '((mercury
     ("cc_multi" "cc_nondet" "det" "erroneous" "failure" "multi" "nondet"
      "semidet"))
    (t nil))
  "Alist of Prolog determinism specificators used by font locking."
  :version "24.1"
  :group 'prolog-font-lock
  :type 'sexp)

(defcustom prolog-directives
  '((mercury
     ("^#[0-9]+"))
    (t nil))
  "Alist of Prolog source code directives used by font locking."
  :version "24.1"
  :group 'prolog-font-lock
  :type 'sexp)


;; Keyboard

(defcustom prolog-electric-newline-flag (not (fboundp 'electric-indent-mode))
  "Non-nil means automatically indent the next line when the user types RET."
  :version "24.1"
  :group 'prolog-keyboard
  :type 'boolean)

(defcustom prolog-hungry-delete-key-flag nil
  "Non-nil means delete key consumes all preceding spaces."
  :version "24.1"
  :group 'prolog-keyboard
  :type 'boolean)

(defcustom prolog-electric-dot-flag nil
  "Non-nil means make dot key electric.
Electric dot appends newline or inserts head of a new clause.
If dot is pressed at the end of a line where at least one white space
precedes the point, it inserts a recursive call to the current predicate.
If dot is pressed at the beginning of an empty line, it inserts the head
of a new clause for the current predicate.  It does not apply in strings
and comments.
It does not apply in strings and comments."
  :version "24.1"
  :group 'prolog-keyboard
  :type 'boolean)

(defcustom prolog-electric-dot-full-predicate-template nil
  "If nil, electric dot inserts only the current predicate's name and `('
for recursive calls or new clause heads.  Non-nil means to also
insert enough commas to cover the predicate's arity and `)',
and dot and newline for recursive calls."
  :version "24.1"
  :group 'prolog-keyboard
  :type 'boolean)

(defcustom prolog-electric-underscore-flag nil
  "Non-nil means make underscore key electric.
Electric underscore replaces the current variable with underscore.
If underscore is pressed not on a variable then it behaves as usual."
  :version "24.1"
  :group 'prolog-keyboard
  :type 'boolean)

(defcustom prolog-electric-tab-flag nil
  "Non-nil means make TAB key electric.
Electric TAB inserts spaces after parentheses, ->, and ;
in ( If -> Then ; Else) and ( Disj1 ; Disj2 ) style expressions."
  :version "24.1"
  :group 'prolog-keyboard
  :type 'boolean)

(defcustom prolog-electric-if-then-else-flag nil
  "Non-nil makes `(', `>' and `;' electric
to automatically indent if-then-else constructs."
  :version "24.1"
  :group 'prolog-keyboard
  :type 'boolean)

(defcustom prolog-electric-colon-flag nil
  "Makes `:' electric (inserts `:-' on a new line).
If non-nil, pressing `:' at the end of a line that starts in
the first column (i.e., clause heads) inserts ` :-' and newline."
  :version "24.1"
  :group 'prolog-keyboard
  :type 'boolean)

(defcustom prolog-electric-dash-flag nil
  "Makes `-' electric (inserts a `-->' on a new line).
If non-nil, pressing `-' at the end of a line that starts in
the first column (i.e., DCG heads) inserts ` -->' and newline."
  :version "24.1"
  :group 'prolog-keyboard
  :type 'boolean)

(defcustom prolog-old-sicstus-keys-flag nil
  "Non-nil means old SICStus Prolog mode keybindings are used."
  :version "24.1"
  :group 'prolog-keyboard
  :type 'boolean)

;; Inferior mode

(defcustom prolog-program-name
  `(((getenv "EPROLOG") (eval (getenv "EPROLOG")))
    (eclipse "eclipse")
    (mercury nil)
    (sicstus "sicstus")
    (swi ,(if (not (executable-find "swipl")) "pl" "swipl"))
    (gnu "gprolog")
    (t ,(let ((names '("prolog" "gprolog" "swipl" "pl")))
 	  (while (and names
 		      (not (executable-find (car names))))
 	    (setq names (cdr names)))
 	  (or (car names) "prolog"))))
  "Alist of program names for invoking an inferior Prolog with `run-prolog'."
  :group 'prolog-inferior
  :type 'sexp)
(defun prolog-program-name ()
  (prolog-find-value-by-system prolog-program-name))

(defcustom prolog-program-switches
  '((sicstus ("-i"))
    (t nil))
  "Alist of switches given to inferior Prolog run with `run-prolog'."
  :version "24.1"
  :group 'prolog-inferior
  :type 'sexp)
(defun prolog-program-switches ()
  (prolog-find-value-by-system prolog-program-switches))

(defcustom prolog-consult-string
  '((eclipse "[%f].")
    (mercury nil)
    (sicstus (eval (if (prolog-atleast-version '(3 . 7))
                       "prolog:zap_file(%m,%b,consult,%l)."
                     "prolog:zap_file(%m,%b,consult).")))
    (swi "[%f].")
    (gnu     "[%f].")
    (t "reconsult(%f)."))
  "Alist of strings defining predicate for reconsulting.

Some parts of the string are replaced:
`%f' by the name of the consulted file (can be a temporary file)
`%b' by the file name of the buffer to consult
`%m' by the module name and name of the consulted file separated by colon
`%l' by the line offset into the file.  This is 0 unless consulting a
     region of a buffer, in which case it is the number of lines before
     the region."
  :group 'prolog-inferior
  :type 'sexp)
(defun prolog-consult-string ()
  (prolog-find-value-by-system prolog-consult-string))

(defcustom prolog-compile-string
  '((eclipse "[%f].")
    (mercury "mmake ")
    (sicstus (eval (if (prolog-atleast-version '(3 . 7))
                       "prolog:zap_file(%m,%b,compile,%l)."
                     "prolog:zap_file(%m,%b,compile).")))
    (swi "[%f].")
    (t "compile(%f)."))
  "Alist of strings and lists defining predicate for recompilation.

Some parts of the string are replaced:
`%f' by the name of the compiled file (can be a temporary file)
`%b' by the file name of the buffer to compile
`%m' by the module name and name of the compiled file separated by colon
`%l' by the line offset into the file.  This is 0 unless compiling a
     region of a buffer, in which case it is the number of lines before
     the region.

If `prolog-program-name' is non-nil, it is a string sent to a Prolog process.
If `prolog-program-name' is nil, it is an argument to the `compile' function."
  :group 'prolog-inferior
  :type 'sexp)
(defun prolog-compile-string ()
  (prolog-find-value-by-system prolog-compile-string))

(defcustom prolog-eof-string "end_of_file.\n"
  "Alist of strings that represent end of file for prolog.
nil means send actual operating system end of file."
  :group 'prolog-inferior
  :type 'sexp)

(defcustom prolog-prompt-regexp
  '((eclipse "^[a-zA-Z0-9()]* *\\?- \\|^\\[[a-zA-Z]* [0-9]*\\]:")
    (sicstus "| [ ?][- ] *")
    (swi "^\\(\\[[a-zA-Z]*\\] \\)?[1-9]?[0-9]*[ ]?\\?- \\|^| +")
    (gnu "^| \\?-")
    (t "^|? *\\?-"))
  "Alist of prompts of the prolog system command line."
  :version "24.1"
  :group 'prolog-inferior
  :type 'sexp)
(defun prolog-prompt-regexp ()
  (prolog-find-value-by-system prolog-prompt-regexp))

;; (defcustom prolog-continued-prompt-regexp
;;   '((sicstus "^\\(| +\\|     +\\)")
;;     (t "^|: +"))
;;   "Alist of regexps matching the prompt when consulting `user'."
;;   :group 'prolog-inferior
;;   :type 'sexp)

(defcustom prolog-debug-on-string "debug.\n"
  "Predicate for enabling debug mode."
  :version "24.1"
  :group 'prolog-inferior
  :type 'string)

(defcustom prolog-debug-off-string "nodebug.\n"
  "Predicate for disabling debug mode."
  :version "24.1"
  :group 'prolog-inferior
  :type 'string)

(defcustom prolog-trace-on-string "trace.\n"
  "Predicate for enabling tracing."
  :version "24.1"
  :group 'prolog-inferior
  :type 'string)

(defcustom prolog-trace-off-string "notrace.\n"
  "Predicate for disabling tracing."
  :version "24.1"
  :group 'prolog-inferior
  :type 'string)

(defcustom prolog-zip-on-string "zip.\n"
  "Predicate for enabling zip mode for SICStus."
  :version "24.1"
  :group 'prolog-inferior
  :type 'string)

(defcustom prolog-zip-off-string "nozip.\n"
  "Predicate for disabling zip mode for SICStus."
  :version "24.1"
  :group 'prolog-inferior
  :type 'string)

(defcustom prolog-use-standard-consult-compile-method-flag t
  "Non-nil means use the standard compilation method.
Otherwise the new compilation method will be used.  This
utilizes a special compilation buffer with the associated
features such as parsing of error messages and automatically
jumping to the source code responsible for the error.

Warning: the new method is so far only experimental and
does contain bugs.  The recommended setting for the novice user
is non-nil for this variable."
  :version "24.1"
  :group 'prolog-inferior
  :type 'boolean)


;; Miscellaneous

(defcustom prolog-use-prolog-tokenizer-flag
  (not (fboundp 'syntax-propertize-rules))
  "Non-nil means use the internal prolog tokenizer for indentation etc.
Otherwise use `parse-partial-sexp' which is faster but sometimes incorrect."
  :version "24.1"
  :group 'prolog-other
  :type 'boolean)

(defcustom prolog-imenu-flag t
  "Non-nil means add a clause index menu for all prolog files."
  :version "24.1"
  :group 'prolog-other
  :type 'boolean)

(defcustom prolog-imenu-max-lines 3000
  "The maximum number of lines of the file for imenu to be enabled.
Relevant only when `prolog-imenu-flag' is non-nil."
  :version "24.1"
  :group 'prolog-other
  :type 'integer)

(defcustom prolog-info-predicate-index
  "(sicstus)Predicate Index"
  "The info node for the SICStus predicate index."
  :version "24.1"
  :group 'prolog-other
  :type 'string)

(defcustom prolog-underscore-wordchar-flag nil
  "Non-nil means underscore (_) is a word-constituent character."
  :version "24.1"
  :group 'prolog-other
  :type 'boolean)

(defcustom prolog-use-sicstus-sd nil
  "If non-nil, use the source level debugger of SICStus 3#7 and later."
  :version "24.1"
  :group 'prolog-other
  :type 'boolean)

(defcustom prolog-char-quote-workaround nil
  "If non-nil, declare 0 as a quote character to handle 0'<char>.
This is really kludgy, and unneeded (i.e. obsolete) in Emacs>=24."
  :version "24.1"
  :group 'prolog-other
  :type 'boolean)


;;-------------------------------------------------------------------
;; Internal variables
;;-------------------------------------------------------------------

;;(defvar prolog-temp-filename "")   ; Later set by `prolog-temporary-file'

(defvar prolog-mode-syntax-table
  ;; The syntax accepted varies depending on the implementation used.
  ;; Here are some of the differences:
  ;; - SWI-Prolog accepts nested /*..*/ comments.
  ;; - Edinburgh-style Prologs take <radix>'<number> for non-decimal number,
  ;;   whereas ISO-style Prologs use 0[obx]<number> instead.
  ;; - In atoms \x<hex> sometimes needs a terminating \ (ISO-style)
  ;;   and sometimes not.
  (let ((table (make-syntax-table)))
    (if prolog-underscore-wordchar-flag
        (modify-syntax-entry ?_ "w" table)
      (modify-syntax-entry ?_ "_" table))

    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\' "\"" table)

    ;; Any better way to handle the 0'<char> construct?!?
    (when prolog-char-quote-workaround
      (modify-syntax-entry ?0 "\\" table))

    (modify-syntax-entry ?% "<" table)
    (modify-syntax-entry ?\n ">" table)
    (if (featurep 'xemacs)
        (progn
          (modify-syntax-entry ?* ". 67" table)
          (modify-syntax-entry ?/ ". 58" table)
          )
      ;; Emacs wants to see this it seems:
      (modify-syntax-entry ?* ". 23b" table)
      (modify-syntax-entry ?/ ". 14" table)
      )
    table))
(defvar prolog-mode-abbrev-table nil)
(defvar prolog-upper-case-string ""
  "A string containing all upper case characters.
Set by prolog-build-case-strings.")
(defvar prolog-lower-case-string ""
  "A string containing all lower case characters.
Set by prolog-build-case-strings.")

(defvar prolog-atom-char-regexp ""
  "Set by prolog-set-atom-regexps.")
;; "Regexp specifying characters which constitute atoms without quoting.")
(defvar prolog-atom-regexp ""
  "Set by prolog-set-atom-regexps.")

(defconst prolog-left-paren "[[({]"     ;FIXME: Why not \\s(?
  "The characters used as left parentheses for the indentation code.")
(defconst prolog-right-paren "[])}]"    ;FIXME: Why not \\s)?
  "The characters used as right parentheses for the indentation code.")

(defconst prolog-quoted-atom-regexp
  "\\(^\\|[^0-9]\\)\\('\\([^\n']\\|\\\\'\\)*'\\)"
  "Regexp matching a quoted atom.")
(defconst prolog-string-regexp
  "\\(\"\\([^\n\"]\\|\\\\\"\\)*\"\\)"
  "Regexp matching a string.")
(defconst prolog-head-delimiter "\\(:-\\|\\+:\\|-:\\|\\+\\?\\|-\\?\\|-->\\)"
  "A regexp for matching on the end delimiter of a head (e.g. \":-\").")

(defvar prolog-compilation-buffer "*prolog-compilation*"
  "Name of the output buffer for Prolog compilation/consulting.")

(defvar prolog-temporary-file-name nil)
(defvar prolog-keywords-i nil)
(defvar prolog-types-i nil)
(defvar prolog-mode-specificators-i nil)
(defvar prolog-determinism-specificators-i nil)
(defvar prolog-directives-i nil)
(defvar prolog-eof-string-i nil)
;; (defvar prolog-continued-prompt-regexp-i nil)
(defvar prolog-help-function-i nil)

(defvar prolog-align-rules
  (eval-when-compile
    (mapcar
     (lambda (x)
       (let ((name (car x))
             (sym  (cdr x)))
         `(,(intern (format "prolog-%s" name))
           (regexp . ,(format "\\(\\s-*\\)%s\\(\\s-*\\)" sym))
           (tab-stop . nil)
           (modes . '(prolog-mode))
           (group . (1 2)))))
     '(("dcg" . "-->") ("rule" . ":-") ("simplification" . "<=>")
       ("propagation" . "==>")))))



;;-------------------------------------------------------------------
;; Prolog mode
;;-------------------------------------------------------------------

;; Example: (prolog-atleast-version '(3 . 6))
(defun prolog-atleast-version (version)
  "Return t if the version of the current prolog system is VERSION or later.
VERSION is of the format (Major . Minor)"
  ;; Version.major < major or
  ;; Version.major = major and Version.minor <= minor
  (let* ((thisversion (prolog-find-value-by-system prolog-system-version))
         (thismajor (car thisversion))
         (thisminor (cdr thisversion)))
    (or (< (car version) thismajor)
        (and (= (car version) thismajor)
             (<= (cdr version) thisminor)))
    ))

(define-abbrev-table 'prolog-mode-abbrev-table ())

(defun prolog-find-value-by-system (alist)
  "Get value from ALIST according to `prolog-system'."
  (let ((system (or prolog-system
                    (let ((infbuf (prolog-inferior-buffer 'dont-run)))
                      (when infbuf
                        (buffer-local-value 'prolog-system infbuf))))))
    (if (listp alist)
        (let (result
              id)
          (while alist
            (setq id (car (car alist)))
            (if (or (eq id system)
                    (eq id t)
                    (and (listp id)
                         (eval id)))
                (progn
                  (setq result (car (cdr (car alist))))
                  (if (and (listp result)
                           (eq (car result) 'eval))
                      (setq result (eval (car (cdr result)))))
                  (setq alist nil))
              (setq alist (cdr alist))))
          result)
      alist)))

(defconst prolog-syntax-propertize-function
  (when (fboundp 'syntax-propertize-rules)
    (syntax-propertize-rules
     ;; GNU Prolog only accepts 0'\' rather than 0'', but the only
     ;; possible meaning of 0'' is rather clear.
     ("\\<0\\(''?\\)"
      (1 (unless (save-excursion (nth 8 (syntax-ppss (match-beginning 0))))
           (string-to-syntax "_"))))
     ;; We could check that we're not inside an atom, but I don't think
     ;; that 'foo 8'z could be a valid syntax anyway, so why bother?
     ("\\<[1-9][0-9]*\\('\\)[0-9a-zA-Z]" (1 "_"))
     ;; Supposedly, ISO-Prolog wants \NNN\ for octal and \xNNN\ for hexadecimal
     ;; escape sequences in atoms, so be careful not to let the terminating \
     ;; escape a subsequent quote.
     ("\\\\[x0-7][0-9a-fA-F]*\\(\\\\\\)" (1 "_"))
     )))

(defun prolog-mode-variables ()
  "Set some common variables to Prolog code specific values."
  (setq local-abbrev-table prolog-mode-abbrev-table)
  (set (make-local-variable 'paragraph-start)
       (concat "[ \t]*$\\|" page-delimiter)) ;'%%..'
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'normal-auto-fill-function) 'prolog-do-auto-fill)
  (set (make-local-variable 'indent-line-function) 'prolog-indent-line)
  (set (make-local-variable 'comment-start) "%")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-add) 1)
  (set (make-local-variable 'comment-start-skip)
       ;; This complex regexp makes sure that comments cannot start
       ;; inside quoted atoms or strings
       (format "^\\(\\(%s\\|%s\\|[^\n\'\"%%]\\)*\\)\\(/\\*+ *\\|%%+ *\\)"
               prolog-quoted-atom-regexp prolog-string-regexp))
  (set (make-local-variable 'comment-indent-function) 'prolog-comment-indent)
  (set (make-local-variable 'parens-require-spaces) nil)
  ;; Initialize Prolog system specific variables
  (dolist (var '(prolog-keywords prolog-types prolog-mode-specificators
                 prolog-determinism-specificators prolog-directives
                 prolog-eof-string
                 ;; prolog-continued-prompt-regexp
                 prolog-help-function))
    (set (intern (concat (symbol-name var) "-i"))
         (prolog-find-value-by-system (symbol-value var))))
  (when (null (prolog-program-name))
    (set (make-local-variable 'compile-command) (prolog-compile-string)))
  (set (make-local-variable 'font-lock-defaults)
       '(prolog-font-lock-keywords nil nil ((?_ . "w"))))
  (set (make-local-variable 'syntax-propertize-function)
      prolog-syntax-propertize-function)
  )

(defun prolog-mode-keybindings-common (map)
  "Define keybindings common to both Prolog modes in MAP."
  (define-key map "\C-c?" 'prolog-help-on-predicate)
  (define-key map "\C-c/" 'prolog-help-apropos)
  (define-key map "\C-c\C-d" 'prolog-debug-on)
  (define-key map "\C-c\C-t" 'prolog-trace-on)
  (define-key map "\C-c\C-z" 'prolog-zip-on)
  (define-key map "\C-c\r" 'run-prolog))

(defun prolog-mode-keybindings-edit (map)
  "Define keybindings for Prolog mode in MAP."
  (define-key map "\M-a" 'prolog-beginning-of-clause)
  (define-key map "\M-e" 'prolog-end-of-clause)
  (define-key map "\M-q" 'prolog-fill-paragraph)
  (define-key map "\C-c\C-a" 'align)
  (define-key map "\C-\M-a" 'prolog-beginning-of-predicate)
  (define-key map "\C-\M-e" 'prolog-end-of-predicate)
  (define-key map "\M-\C-c" 'prolog-mark-clause)
  (define-key map "\M-\C-h" 'prolog-mark-predicate)
  (define-key map "\M-\C-n" 'prolog-forward-list)
  (define-key map "\M-\C-p" 'prolog-backward-list)
  (define-key map "\C-c\C-n" 'prolog-insert-predicate-template)
  (define-key map "\C-c\C-s" 'prolog-insert-predspec)
  (define-key map "\M-\r" 'prolog-insert-next-clause)
  (define-key map "\C-c\C-va" 'prolog-variables-to-anonymous)
  (define-key map "\C-c\C-v\C-s" 'prolog-view-predspec)

  (define-key map [Backspace] 'prolog-electric-delete)
  (define-key map "." 'prolog-electric-dot)
  (define-key map "_" 'prolog-electric-underscore)
  (define-key map "(" 'prolog-electric-if-then-else)
  (define-key map ";" 'prolog-electric-if-then-else)
  (define-key map ">" 'prolog-electric-if-then-else)
  (define-key map ":" 'prolog-electric-colon)
  (define-key map "-" 'prolog-electric-dash)
  (if prolog-electric-newline-flag
      (define-key map "\r" 'newline-and-indent))

  ;; If we're running SICStus, then map C-c C-c e/d to enabling
  ;; and disabling of the source-level debugging facilities.
  ;(if (and (eq prolog-system 'sicstus)
  ;         (prolog-atleast-version '(3 . 7)))
  ;    (progn
  ;      (define-key map "\C-c\C-ce" 'prolog-enable-sicstus-sd)
  ;      (define-key map "\C-c\C-cd" 'prolog-disable-sicstus-sd)
  ;      ))

  (if prolog-old-sicstus-keys-flag
      (progn
        (define-key map "\C-c\C-c" 'prolog-consult-predicate)
        (define-key map "\C-cc" 'prolog-consult-region)
        (define-key map "\C-cC" 'prolog-consult-buffer)
        (define-key map "\C-c\C-k" 'prolog-compile-predicate)
        (define-key map "\C-ck" 'prolog-compile-region)
        (define-key map "\C-cK" 'prolog-compile-buffer))
    (define-key map "\C-c\C-p" 'prolog-consult-predicate)
    (define-key map "\C-c\C-r" 'prolog-consult-region)
    (define-key map "\C-c\C-b" 'prolog-consult-buffer)
    (define-key map "\C-c\C-f" 'prolog-consult-file)
    (define-key map "\C-c\C-cp" 'prolog-compile-predicate)
    (define-key map "\C-c\C-cr" 'prolog-compile-region)
    (define-key map "\C-c\C-cb" 'prolog-compile-buffer)
    (define-key map "\C-c\C-cf" 'prolog-compile-file))

  ;; Inherited from the old prolog.el.
  (define-key map "\e\C-x" 'prolog-consult-region)
  (define-key map "\C-c\C-l" 'prolog-consult-file)
  (define-key map "\C-c\C-z" 'switch-to-prolog))

(defun prolog-mode-keybindings-inferior (_map)
  "Define keybindings for inferior Prolog mode in MAP."
  ;; No inferior mode specific keybindings now.
  )

(defvar prolog-mode-map
  (let ((map (make-sparse-keymap)))
    (prolog-mode-keybindings-common map)
    (prolog-mode-keybindings-edit map)
    map))


(defvar prolog-mode-hook nil
  "List of functions to call after the prolog mode has initialized.")

(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))
;;;###autoload
(define-derived-mode prolog-mode prog-mode "Prolog"
  "Major mode for editing Prolog code.

Blank lines and `%%...' separate paragraphs.  `%'s starts a comment
line and comments can also be enclosed in /* ... */.

If an optional argument SYSTEM is non-nil, set up mode for the given system.

To find out what version of Prolog mode you are running, enter
`\\[prolog-mode-version]'.

Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of `prolog-mode-hook'
if that value is non-nil."
  (setq mode-name (concat "Prolog"
                          (cond
                           ((eq prolog-system 'eclipse) "[ECLiPSe]")
                           ((eq prolog-system 'sicstus) "[SICStus]")
                           ((eq prolog-system 'swi) "[SWI]")
                           ((eq prolog-system 'gnu) "[GNU]")
                           (t ""))))
  (prolog-mode-variables)
  (prolog-build-case-strings)
  (prolog-set-atom-regexps)
  (dolist (ar prolog-align-rules) (add-to-list 'align-rules-list ar))

  ;; imenu entry moved to the appropriate hook for consistency

  ;; Load SICStus debugger if suitable
  (if (and (eq prolog-system 'sicstus)
           (prolog-atleast-version '(3 . 7))
           prolog-use-sicstus-sd)
      (prolog-enable-sicstus-sd))

  (prolog-menu))

(defvar mercury-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prolog-mode-map)
    map))

;;;###autoload
(define-derived-mode mercury-mode prolog-mode "Prolog[Mercury]"
  "Major mode for editing Mercury programs.
Actually this is just customized `prolog-mode'."
  (set (make-local-variable 'prolog-system) 'mercury))


;;-------------------------------------------------------------------
;; Inferior prolog mode
;;-------------------------------------------------------------------

(defvar prolog-inferior-mode-map
  (let ((map (make-sparse-keymap)))
    (prolog-mode-keybindings-common map)
    (prolog-mode-keybindings-inferior map)
    (define-key map [remap self-insert-command]
      'prolog-inferior-self-insert-command)
    map))

(defvar prolog-inferior-mode-hook nil
  "List of functions to call after the inferior prolog mode has initialized.")

(defvar prolog-inferior-error-regexp-alist
  '(;; GNU Prolog used to not follow the GNU standard format.
    ;; ("^\\(.*?\\):\\([0-9]+\\) error: .*(char:\\([0-9]+\\)" 1 2 3)
    ;; SWI-Prolog.
    ("^\\(?:\\?- *\\)?\\(\\(?:ERROR\\|\\(W\\)arning\\): *\\(.*?\\):\\([1-9][0-9]*\\):\\(?:\\([0-9]*\\):\\)?\\)\\(?:$\\| \\)"
     3 4 5 (2 . nil) 1)
    ;; GNU-Prolog now uses the GNU standard format.
    gnu))

(defun prolog-inferior-self-insert-command ()
  "Insert the char in the buffer or pass it directly to the process."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (and proc (marker-position (process-mark proc)))))
    ;; FIXME: the same treatment would be needed for SWI-Prolog, but I can't
    ;; seem to find any way for Emacs to figure out when to use it because
    ;; SWI doesn't include a " ? " or some such recognizable marker.
    (if (and (eq prolog-system 'gnu)
             pmark
             (null current-prefix-arg)
             (eobp)
             (eq (point) pmark)
             (save-excursion
               (goto-char (- pmark 3))
               ;; FIXME: check this comes from the process's output, maybe?
               (looking-at " \\? ")))
        ;; This is GNU prolog waiting to know whether you want more answers
        ;; or not (or abort, etc...).  The answer is a single char, not
        ;; a line, so pass this char directly rather than wait for RET to
        ;; send a whole line.
        (comint-send-string proc (string last-command-event))
      (call-interactively 'self-insert-command))))

(declare-function 'compilation-shell-minor-mode "compile" (&optional arg))
(defvar compilation-error-regexp-alist)

(define-derived-mode prolog-inferior-mode comint-mode "Inferior Prolog"
  "Major mode for interacting with an inferior Prolog process.

The following commands are available:
\\{prolog-inferior-mode-map}

Entry to this mode calls the value of `prolog-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`prolog-mode-hook' is called after `comint-mode-hook'.

You can send text to the inferior Prolog from other buffers
using the commands `send-region', `send-string' and \\[prolog-consult-region].

Commands:
Tab indents for Prolog; with argument, shifts rest
 of expression rigidly with the current line.
Paragraphs are separated only by blank lines and '%%'. '%'s start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-delchar-or-maybe-eof] sends end-of-file as input.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands,
imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-stop-subjob] stops, likewise.
\\[comint-quit-subjob] sends quit signal, likewise.

To find out what version of Prolog mode you are running, enter
`\\[prolog-mode-version]'."
  (require 'compile)
  (setq comint-input-filter 'prolog-input-filter)
  (setq mode-line-process '(": %s"))
  (prolog-mode-variables)
  (setq comint-prompt-regexp (prolog-prompt-regexp))
  (set (make-local-variable 'shell-dirstack-query) "pwd.")
  (set (make-local-variable 'compilation-error-regexp-alist)
       prolog-inferior-error-regexp-alist)
  (compilation-shell-minor-mode)
  (prolog-inferior-menu))

(defun prolog-input-filter (str)
  (cond ((string-match "\\`\\s *\\'" str) nil) ;whitespace
        ((not (derived-mode-p 'prolog-inferior-mode)) t)
        ((= (length str) 1) nil)        ;one character
        ((string-match "\\`[rf] *[0-9]*\\'" str) nil) ;r(edo) or f(ail)
        (t t)))

;;;###autoload
(defun run-prolog (arg)
  "Run an inferior Prolog process, input and output via buffer *prolog*.
With prefix argument ARG, restart the Prolog process if running before."
  (interactive "P")
  ;; FIXME: It should be possible to interactively specify the command to use
  ;; to run prolog.
  (if (and arg (get-process "prolog"))
      (progn
        (process-send-string "prolog" "halt.\n")
        (while (get-process "prolog") (sit-for 0.1))))
  (let ((buff (buffer-name)))
    (if (not (string= buff "*prolog*"))
        (prolog-goto-prolog-process-buffer))
    ;; Load SICStus debugger if suitable
    (if (and (eq prolog-system 'sicstus)
             (prolog-atleast-version '(3 . 7))
             prolog-use-sicstus-sd)
        (prolog-enable-sicstus-sd))
    (prolog-mode-variables)
    (prolog-ensure-process)
    ))

(defun prolog-inferior-guess-flavor (&optional ignored)
  (setq prolog-system
        (when (or (numberp prolog-system) (markerp prolog-system))
          (save-excursion
            (goto-char (1+ prolog-system))
            (cond
             ((looking-at "GNU Prolog") 'gnu)
             ((looking-at "Welcome to SWI-Prolog\\|%.*\\<swi_") 'swi)
             ((looking-at ".*\n") nil) ;There's at least one line.
             (t prolog-system)))))
  (when (symbolp prolog-system)
    (remove-hook 'comint-output-filter-functions
                 'prolog-inferior-guess-flavor t)
    (when prolog-system
      (setq comint-prompt-regexp (prolog-prompt-regexp))
      (if (eq prolog-system 'gnu)
          (set (make-local-variable 'comint-process-echoes) t)))))

(defun prolog-ensure-process (&optional wait)
  "If Prolog process is not running, run it.
If the optional argument WAIT is non-nil, wait for Prolog prompt specified by
the variable `prolog-prompt-regexp'."
  (if (null (prolog-program-name))
      (error "This Prolog system has defined no interpreter."))
  (if (comint-check-proc "*prolog*")
      ()
    (with-current-buffer (get-buffer-create "*prolog*")
      (prolog-inferior-mode)
      (apply 'make-comint-in-buffer "prolog" (current-buffer)
             (prolog-program-name) nil (prolog-program-switches))
      (unless prolog-system
        ;; Setup auto-detection.
        (set (make-local-variable 'prolog-system)
             ;; Force re-detection.
             (let* ((proc (get-buffer-process (current-buffer)))
                    (pmark (and proc (marker-position (process-mark proc)))))
               (cond
                ((null pmark) (1- (point-min)))
                ;; The use of insert-before-markers in comint.el together with
                ;; the potential use of comint-truncate-buffer in the output
                ;; filter, means that it's difficult to reliably keep track of
                ;; the buffer position where the process's output started.
                ;; If possible we use a marker at "start - 1", so that
                ;; insert-before-marker at `start' won't shift it.  And if not,
                ;; we fall back on using a plain integer.
                ((> pmark (point-min)) (copy-marker (1- pmark)))
                (t (1- pmark)))))
        (add-hook 'comint-output-filter-functions
                  'prolog-inferior-guess-flavor nil t))
      (if wait
          (progn
            (goto-char (point-max))
            (while
                (save-excursion
                  (not
                   (re-search-backward
                    (concat "\\(" (prolog-prompt-regexp) "\\)" "\\=")
                    nil t)))
              (sit-for 0.1)))))))

(defun prolog-inferior-buffer (&optional dont-run)
  (or (get-buffer "*prolog*")
      (unless dont-run
        (prolog-ensure-process)
        (get-buffer "*prolog*"))))

(defun prolog-process-insert-string (process string)
  "Insert STRING into inferior Prolog buffer running PROCESS."
  ;; Copied from elisp manual, greek to me
  (with-current-buffer (process-buffer process)
    ;; FIXME: Use window-point-insertion-type instead.
    (let ((moving (= (point) (process-mark process))))
      (save-excursion
        ;; Insert the text, moving the process-marker.
        (goto-char (process-mark process))
        (insert string)
        (set-marker (process-mark process) (point)))
      (if moving (goto-char (process-mark process))))))

;;------------------------------------------------------------
;; Old consulting and compiling functions
;;------------------------------------------------------------

(declare-function compilation-forget-errors "compile" ())
(declare-function compilation-fake-loc "compile"
                  (marker file &optional line col))

(defun prolog-old-process-region (compilep start end)
  "Process the region limited by START and END positions.
If COMPILEP is non-nil then use compilation, otherwise consulting."
   (prolog-ensure-process)
   ;(let ((tmpfile prolog-temp-filename)
   (let ((tmpfile (prolog-temporary-file))
         ;(process (get-process "prolog"))
         (first-line (1+ (count-lines
                          (point-min)
                          (save-excursion
                            (goto-char start)
                            (point))))))
     (write-region start end tmpfile)
     (setq start (copy-marker start))
     (with-current-buffer (prolog-inferior-buffer)
       (compilation-forget-errors)
       (compilation-fake-loc start tmpfile))
     (process-send-string
      "prolog" (prolog-build-prolog-command
                compilep tmpfile (prolog-bsts buffer-file-name)
                first-line))
     (prolog-goto-prolog-process-buffer)))

(defun prolog-old-process-predicate (compilep)
  "Process the predicate around point.
If COMPILEP is non-nil then use compilation, otherwise consulting."
  (prolog-old-process-region
   compilep (prolog-pred-start) (prolog-pred-end)))

(defun prolog-old-process-buffer (compilep)
  "Process the entire buffer.
If COMPILEP is non-nil then use compilation, otherwise consulting."
  (prolog-old-process-region compilep (point-min) (point-max)))

(defun prolog-old-process-file (compilep)
  "Process the file of the current buffer.
If COMPILEP is non-nil then use compilation, otherwise consulting."
  (save-some-buffers)
  (prolog-ensure-process)
  (with-current-buffer (prolog-inferior-buffer)
    (compilation-forget-errors))
    (process-send-string
     "prolog" (prolog-build-prolog-command
             compilep buffer-file-name
             (prolog-bsts buffer-file-name)))
  (prolog-goto-prolog-process-buffer))


;;------------------------------------------------------------
;; Consulting and compiling
;;------------------------------------------------------------

;; Interactive interface functions, used by both the standard
;; and the experimental consultation and compilation functions
(defun prolog-consult-file ()
  "Consult file of current buffer."
  (interactive)
  (if prolog-use-standard-consult-compile-method-flag
      (prolog-old-process-file nil)
    (prolog-consult-compile-file nil)))

(defun prolog-consult-buffer ()
  "Consult buffer."
  (interactive)
  (if prolog-use-standard-consult-compile-method-flag
      (prolog-old-process-buffer nil)
    (prolog-consult-compile-buffer nil)))

(defun prolog-consult-region (beg end)
  "Consult region between BEG and END."
  (interactive "r")
  (if prolog-use-standard-consult-compile-method-flag
      (prolog-old-process-region nil beg end)
    (prolog-consult-compile-region nil beg end)))

(defun prolog-consult-predicate ()
  "Consult the predicate around current point."
  (interactive)
  (if prolog-use-standard-consult-compile-method-flag
      (prolog-old-process-predicate nil)
    (prolog-consult-compile-predicate nil)))

(defun prolog-compile-file ()
  "Compile file of current buffer."
  (interactive)
  (if prolog-use-standard-consult-compile-method-flag
      (prolog-old-process-file t)
    (prolog-consult-compile-file t)))

(defun prolog-compile-buffer ()
  "Compile buffer."
  (interactive)
  (if prolog-use-standard-consult-compile-method-flag
      (prolog-old-process-buffer t)
    (prolog-consult-compile-buffer t)))

(defun prolog-compile-region (beg end)
  "Compile region between BEG and END."
  (interactive "r")
  (if prolog-use-standard-consult-compile-method-flag
      (prolog-old-process-region t beg end)
    (prolog-consult-compile-region t beg end)))

(defun prolog-compile-predicate ()
  "Compile the predicate around current point."
  (interactive)
  (if prolog-use-standard-consult-compile-method-flag
      (prolog-old-process-predicate t)
    (prolog-consult-compile-predicate t)))

(defun prolog-buffer-module ()
  "Select Prolog module name appropriate for current buffer.
Bases decision on buffer contents (-*- line)."
  ;; Look for -*- ... module: MODULENAME; ... -*-
  (let (beg end)
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t")
      (and (search-forward "-*-" (line-end-position) t)
           (progn
             (skip-chars-forward " \t")
             (setq beg (point))
             (search-forward "-*-" (line-end-position) t))
           (progn
             (forward-char -3)
             (skip-chars-backward " \t")
             (setq end (point))
             (goto-char beg)
             (and (let ((case-fold-search t))
                    (search-forward "module:" end t))
                  (progn
                    (skip-chars-forward " \t")
                    (setq beg (point))
                    (if (search-forward ";" end t)
                        (forward-char -1)
                      (goto-char end))
                    (skip-chars-backward " \t")
                    (buffer-substring beg (point)))))))))

(defun prolog-build-prolog-command (compilep file buffername
                                    &optional first-line)
  "Make Prolog command for FILE compilation/consulting.
If COMPILEP is non-nil, consider compilation, otherwise consulting."
  (let* ((compile-string
          ;; FIXME: If the process is not running yet, the auto-detection of
          ;; prolog-system won't help here, so we should make sure
          ;; we first run Prolog and then build the command.
          (if compilep (prolog-compile-string) (prolog-consult-string)))
         (module (prolog-buffer-module))
         (file-name (concat "'" (prolog-bsts file) "'"))
         (module-name (if module (concat "'" module "'")))
         (module-file (if module
                          (concat module-name ":" file-name)
                        file-name))
         strbeg strend
         (lineoffset (if first-line
                         (- first-line 1)
                       0)))

    ;; Assure that there is a buffer name
    (if (not buffername)
        (error "The buffer is not saved"))

    (if (not (string-match "\\`'.*'\\'" buffername)) ; Add quotes
        (setq buffername (concat "'" buffername "'")))
    (while (string-match "%m" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg module-file strend)))
    ;; FIXME: The code below will %-expand any %[fbl] that appears in
    ;; module-file.
    (while (string-match "%f" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg file-name strend)))
    (while (string-match "%b" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg buffername strend)))
    (while (string-match "%l" compile-string)
      (setq strbeg (substring compile-string 0 (match-beginning 0)))
      (setq strend (substring compile-string (match-end 0)))
      (setq compile-string (concat strbeg (format "%d" lineoffset) strend)))
    (concat compile-string "\n")))

;; The rest of this page is experimental code!

;; Global variables for process filter function
(defvar prolog-process-flag nil
  "Non-nil means that a prolog task (i.e. a consultation or compilation job)
is running.")
(defvar prolog-consult-compile-output ""
  "Hold the unprocessed output from the current prolog task.")
(defvar prolog-consult-compile-first-line 1
  "The number of the first line of the file to consult/compile.
Used for temporary files.")
(defvar prolog-consult-compile-file nil
  "The file to compile/consult (can be a temporary file).")
(defvar prolog-consult-compile-real-file nil
  "The file name of the buffer to compile/consult.")

(defvar compilation-parse-errors-function)

(defun prolog-consult-compile (compilep file &optional first-line)
  "Consult/compile FILE.
If COMPILEP is non-nil, perform compilation, otherwise perform CONSULTING.
COMMAND is a string described by the variables `prolog-consult-string'
and `prolog-compile-string'.
Optional argument FIRST-LINE is the number of the first line in the compiled
region.

This function must be called from the source code buffer."
  (if prolog-process-flag
      (error "Another Prolog task is running."))
  (prolog-ensure-process t)
  (let* ((buffer (get-buffer-create prolog-compilation-buffer))
         (real-file buffer-file-name)
         (command-string (prolog-build-prolog-command compilep file
                                                      real-file first-line))
         (process (get-process "prolog"))
         (old-filter (process-filter process)))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      ;; FIXME: Wasn't this supposed to use prolog-inferior-mode?
      (compilation-mode)
      ;; FIXME: This doesn't seem to cooperate well with new(ish) compile.el.
      ;; Setting up font-locking for this buffer
      (set (make-local-variable 'font-lock-defaults)
           '(prolog-font-lock-keywords nil nil ((?_ . "w"))))
      (if (eq prolog-system 'sicstus)
          ;; FIXME: This looks really problematic: not only is this using
          ;; the old compilation-parse-errors-function, but
          ;; prolog-parse-sicstus-compilation-errors only accepts one argument
          ;; whereas compile.el calls it with 2 (and did so at least since
          ;; Emacs-20).
            (set (make-local-variable 'compilation-parse-errors-function)
               'prolog-parse-sicstus-compilation-errors))
      (setq buffer-read-only nil)
      (insert command-string "\n"))
    (save-selected-window
      (pop-to-buffer buffer))
    (setq prolog-process-flag t
          prolog-consult-compile-output ""
          prolog-consult-compile-first-line (if first-line (1- first-line) 0)
          prolog-consult-compile-file file
          prolog-consult-compile-real-file (if (string=
                                                file buffer-file-name)
                                               nil
                                             real-file))
    (with-current-buffer buffer
      (goto-char (point-max))
      (set-process-filter process 'prolog-consult-compile-filter)
      (process-send-string "prolog" command-string)
      ;; (prolog-build-prolog-command compilep file real-file first-line))
      (while (and prolog-process-flag
                  (accept-process-output process 10)) ; 10 secs is ok?
        (sit-for 0.1)
        (unless (get-process "prolog")
          (setq prolog-process-flag nil)))
      (insert (if compilep
                  "\nCompilation finished.\n"
                "\nConsulted.\n"))
      (set-process-filter process old-filter))))

(defvar compilation-error-list)

(defun prolog-parse-sicstus-compilation-errors (limit)
  "Parse the prolog compilation buffer for errors.
Argument LIMIT is a buffer position limiting searching.
For use with the `compilation-parse-errors-function' variable."
  (setq compilation-error-list nil)
  (message "Parsing SICStus error messages...")
  (let (filepath dir file errorline)
    (while
        (re-search-backward
         "{\\([a-zA-Z ]* ERROR\\|Warning\\):.* in line[s ]*\\([0-9]+\\)"
         limit t)
      (setq errorline (string-to-number (match-string 2)))
      (save-excursion
        (re-search-backward
         "{\\(consulting\\|compiling\\|processing\\) \\(.*\\)\\.\\.\\.}"
         limit t)
        (setq filepath (match-string 2)))

      ;; ###### Does this work with SICStus under Windows (i.e. backslashes and stuff?)
      (if (string-match "\\(.*/\\)\\([^/]*\\)$" filepath)
          (progn
            (setq dir (match-string 1 filepath))
            (setq file (match-string 2 filepath))))

      (setq compilation-error-list
            (cons
             (cons (save-excursion
                     (beginning-of-line)
                     (point-marker))
                   (list (list file dir) errorline))
             compilation-error-list)
            ))
    ))

(defun prolog-consult-compile-filter (process output)
  "Filter function for Prolog compilation PROCESS.
Argument OUTPUT is a name of the output file."
  ;;(message "start")
  (setq prolog-consult-compile-output
        (concat prolog-consult-compile-output output))
  ;;(message "pccf1: %s" prolog-consult-compile-output)
  ;; Iterate through the lines of prolog-consult-compile-output
  (let (outputtype)
    (while (and prolog-process-flag
                (or
                 ;; Trace question
                 (progn
                   (setq outputtype 'trace)
                   (and (eq prolog-system 'sicstus)
                        (string-match
                         "^[ \t]*[0-9]+[ \t]*[0-9]+[ \t]*Call:.*? "
                         prolog-consult-compile-output)))

                 ;; Match anything
                 (progn
                   (setq outputtype 'normal)
                   (string-match "^.*\n" prolog-consult-compile-output))
                   ))
      ;;(message "outputtype: %s" outputtype)

      (setq output (match-string 0 prolog-consult-compile-output))
      ;; remove the text in output from prolog-consult-compile-output
      (setq prolog-consult-compile-output
            (substring prolog-consult-compile-output (length output)))
      ;;(message "pccf2: %s" prolog-consult-compile-output)

      ;; If temporary files were used, then we change the error
      ;; messages to point to the original source file.
      ;; FIXME: Use compilation-fake-loc instead.
      (cond

       ;; If the prolog process was in trace mode then it requires
       ;; user input
       ((and (eq prolog-system 'sicstus)
             (eq outputtype 'trace))
        (let ((input (concat (read-string output) "\n")))
          (process-send-string process input)
          (setq output (concat output input))))

       ((eq prolog-system 'sicstus)
        (if (and prolog-consult-compile-real-file
                 (string-match
                  "\\({.*:.* in line[s ]*\\)\\([0-9]+\\)-\\([0-9]+\\)" output))
            (setq output (replace-match
                          ;; Adds a {processing ...} line so that
                          ;; `prolog-parse-sicstus-compilation-errors'
                          ;; finds the real file instead of the temporary one.
                          ;; Also fixes the line numbers.
                          (format "Added by Emacs: {processing %s...}\n%s%d-%d"
                                  prolog-consult-compile-real-file
                                  (match-string 1 output)
                                  (+ prolog-consult-compile-first-line
                                     (string-to-number
                                      (match-string 2 output)))
                                  (+ prolog-consult-compile-first-line
                                     (string-to-number
                                      (match-string 3 output))))
                          t t output)))
        )

       ((eq prolog-system 'swi)
        (if (and prolog-consult-compile-real-file
                 (string-match (format
                                "%s\\([ \t]*:[ \t]*\\)\\([0-9]+\\)"
                                prolog-consult-compile-file)
                               output))
            (setq output (replace-match
                          ;; Real filename + text + fixed linenum
                          (format "%s%s%d"
                                  prolog-consult-compile-real-file
                                  (match-string 1 output)
                                  (+ prolog-consult-compile-first-line
                                     (string-to-number
                                      (match-string 2 output))))
                          t t output)))
        )

       (t ())
       )
      ;; Write the output in the *prolog-compilation* buffer
      (insert output)))

  ;; If the prompt is visible, then the task is finished
  (if (string-match (prolog-prompt-regexp) prolog-consult-compile-output)
      (setq prolog-process-flag nil)))

(defun prolog-consult-compile-file (compilep)
  "Consult/compile file of current buffer.
If COMPILEP is non-nil, compile, otherwise consult."
  (let ((file buffer-file-name))
    (if file
        (progn
          (save-some-buffers)
          (prolog-consult-compile compilep file))
      (prolog-consult-compile-region compilep (point-min) (point-max)))))

(defun prolog-consult-compile-buffer (compilep)
  "Consult/compile current buffer.
If COMPILEP is non-nil, compile, otherwise consult."
  (prolog-consult-compile-region compilep (point-min) (point-max)))

(defun prolog-consult-compile-region (compilep beg end)
  "Consult/compile region between BEG and END.
If COMPILEP is non-nil, compile, otherwise consult."
  ;(let ((file prolog-temp-filename)
  (let ((file (prolog-bsts (prolog-temporary-file)))
        (lines (count-lines 1 beg)))
    (write-region beg end file nil 'no-message)
    (write-region "\n" nil file t 'no-message)
    (prolog-consult-compile compilep file
                            (if (bolp) (1+ lines) lines))
    (delete-file file)))

(defun prolog-consult-compile-predicate (compilep)
  "Consult/compile the predicate around current point.
If COMPILEP is non-nil, compile, otherwise consult."
  (prolog-consult-compile-region
   compilep (prolog-pred-start) (prolog-pred-end)))


;;-------------------------------------------------------------------
;; Font-lock stuff
;;-------------------------------------------------------------------

;; Auxiliary functions
(defun prolog-make-keywords-regexp (keywords &optional protect)
  "Create regexp from the list of strings KEYWORDS.
If PROTECT is non-nil, surround the result regexp by word breaks."
  (let ((regexp
         (if (fboundp 'regexp-opt)
             ;; Emacs 20
             ;; Avoid compile warnings under earlier versions by using eval
             (eval '(regexp-opt keywords))
           ;; Older Emacsen
           (concat (mapconcat 'regexp-quote keywords "\\|")))
         ))
    (if protect
        (concat "\\<\\(" regexp "\\)\\>")
      regexp)))

(defun prolog-font-lock-object-matcher (bound)
  "Find SICStus objects method name for font lock.
Argument BOUND is a buffer position limiting searching."
  (let (point
        (case-fold-search nil))
    (while (and (not point)
                (re-search-forward "\\(::[ \t\n]*{\\|&\\)[ \t]*"
                                   bound t))
      (while (or (re-search-forward "\\=\n[ \t]*" bound t)
                 (re-search-forward "\\=%.*" bound t)
                 (and (re-search-forward "\\=/\\*" bound t)
                      (re-search-forward "\\*/[ \t]*" bound t))))
      (setq point (re-search-forward
                   (format "\\=\\(%s\\)" prolog-atom-regexp)
                   bound t)))
    point))

(defsubst prolog-face-name-p (facename)
  ;; Return t if FACENAME is the name of a face.  This method is
  ;; necessary since facep in XEmacs only returns t for the actual
  ;; face objects (while it's only their names that are used just
  ;; about anywhere else) without providing a predicate that tests
  ;; face names.  This function (including the above commentary) is
  ;; borrowed from cc-mode.
  (memq facename (face-list)))

;; Set everything up
(defun prolog-font-lock-keywords ()
  "Set up font lock keywords for the current Prolog system."
  ;(when window-system
    (require 'font-lock)

    ;; Define Prolog faces
    (defface prolog-redo-face
      '((((class grayscale)) (:italic t))
        (((class color)) (:foreground "darkorchid"))
        (t (:italic t)))
      "Prolog mode face for highlighting redo trace lines."
      :group 'prolog-faces)
    (defface prolog-exit-face
      '((((class grayscale)) (:underline t))
        (((class color) (background dark)) (:foreground "green"))
        (((class color) (background light)) (:foreground "ForestGreen"))
        (t (:underline t)))
      "Prolog mode face for highlighting exit trace lines."
      :group 'prolog-faces)
    (defface prolog-exception-face
      '((((class grayscale)) (:bold t :italic t :underline t))
        (((class color)) (:bold t :foreground "black" :background "Khaki"))
        (t (:bold t :italic t :underline t)))
      "Prolog mode face for highlighting exception trace lines."
      :group 'prolog-faces)
    (defface prolog-warning-face
      '((((class grayscale)) (:underline t))
        (((class color) (background dark)) (:foreground "blue"))
        (((class color) (background light)) (:foreground "MidnightBlue"))
        (t (:underline t)))
      "Face name to use for compiler warnings."
      :group 'prolog-faces)
    (defface prolog-builtin-face
      '((((class color) (background light)) (:foreground "Purple"))
        (((class color) (background dark)) (:foreground "Cyan"))
        (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
        (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
        (t (:bold t)))
      "Face name to use for compiler warnings."
      :group 'prolog-faces)
    (defvar prolog-warning-face
      (if (prolog-face-name-p 'font-lock-warning-face)
          'font-lock-warning-face
        'prolog-warning-face)
      "Face name to use for built in predicates.")
    (defvar prolog-builtin-face
      (if (prolog-face-name-p 'font-lock-builtin-face)
          'font-lock-builtin-face
        'prolog-builtin-face)
      "Face name to use for built in predicates.")
    (defvar prolog-redo-face 'prolog-redo-face
      "Face name to use for redo trace lines.")
    (defvar prolog-exit-face 'prolog-exit-face
      "Face name to use for exit trace lines.")
    (defvar prolog-exception-face 'prolog-exception-face
      "Face name to use for exception trace lines.")

    ;; Font Lock Patterns
    (let (
          ;; "Native" Prolog patterns
          (head-predicates
           (list (format "^\\(%s\\)\\((\\|[ \t]*:-\\)" prolog-atom-regexp)
                 1 font-lock-function-name-face))
           ;(list (format "^%s" prolog-atom-regexp)
           ;      0 font-lock-function-name-face))
          (head-predicates-1
           (list (format "\\.[ \t]*\\(%s\\)" prolog-atom-regexp)
                 1 font-lock-function-name-face) )
          (variables
           '("\\<\\([_A-Z][a-zA-Z0-9_]*\\)"
             1 font-lock-variable-name-face))
          (important-elements
           (list (if (eq prolog-system 'mercury)
                     "[][}{;|]\\|\\\\[+=]\\|<?=>?"
                   "[][}{!;|]\\|\\*->")
                 0 'font-lock-keyword-face))
          (important-elements-1
           '("[^-*]\\(->\\)" 1 font-lock-keyword-face))
          (predspecs                        ; module:predicate/cardinality
           (list (format "\\<\\(%s:\\|\\)%s/[0-9]+"
                         prolog-atom-regexp prolog-atom-regexp)
                 0 font-lock-function-name-face 'prepend))
          (keywords                        ; directives (queries)
           (list
            (if (eq prolog-system 'mercury)
                (concat
                 "\\<\\("
                 (prolog-make-keywords-regexp prolog-keywords-i)
                 "\\|"
                 (prolog-make-keywords-regexp
                  prolog-determinism-specificators-i)
                 "\\)\\>")
              (concat
               "^[?:]- *\\("
               (prolog-make-keywords-regexp prolog-keywords-i)
               "\\)\\>"))
              1 prolog-builtin-face))
          (quoted_atom (list prolog-quoted-atom-regexp
                             2 'font-lock-string-face 'append))
          (string (list prolog-string-regexp
                        1 'font-lock-string-face 'append))
          ;; SICStus specific patterns
          (sicstus-object-methods
           (if (eq prolog-system 'sicstus)
               '(prolog-font-lock-object-matcher
                 1 font-lock-function-name-face)))
          ;; Mercury specific patterns
          (types
           (if (eq prolog-system 'mercury)
               (list
                (prolog-make-keywords-regexp prolog-types-i t)
                0 'font-lock-type-face)))
          (modes
           (if (eq prolog-system 'mercury)
               (list
                (prolog-make-keywords-regexp prolog-mode-specificators-i t)
                0 'font-lock-reference-face)))
          (directives
           (if (eq prolog-system 'mercury)
               (list
                (prolog-make-keywords-regexp prolog-directives-i t)
                0 'prolog-warning-face)))
          ;; Inferior mode specific patterns
          (prompt
           ;; FIXME: Should be handled by comint already.
           (list (prolog-prompt-regexp) 0 'font-lock-keyword-face))
          (trace-exit
           ;; FIXME: Add to compilation-error-regexp-alist instead.
           (cond
            ((eq prolog-system 'sicstus)
             '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Exit\\):"
               1 prolog-exit-face))
            ((eq prolog-system 'swi)
             '("[ \t]*\\(Exit\\):[ \t]*([ \t0-9]*)" 1 prolog-exit-face))
            (t nil)))
          (trace-fail
           ;; FIXME: Add to compilation-error-regexp-alist instead.
           (cond
            ((eq prolog-system 'sicstus)
             '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Fail\\):"
               1 prolog-warning-face))
            ((eq prolog-system 'swi)
             '("[ \t]*\\(Fail\\):[ \t]*([ \t0-9]*)" 1 prolog-warning-face))
            (t nil)))
          (trace-redo
           ;; FIXME: Add to compilation-error-regexp-alist instead.
           (cond
            ((eq prolog-system 'sicstus)
             '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Redo\\):"
               1 prolog-redo-face))
            ((eq prolog-system 'swi)
             '("[ \t]*\\(Redo\\):[ \t]*([ \t0-9]*)" 1 prolog-redo-face))
            (t nil)))
          (trace-call
           ;; FIXME: Add to compilation-error-regexp-alist instead.
           (cond
            ((eq prolog-system 'sicstus)
             '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Call\\):"
               1 font-lock-function-name-face))
            ((eq prolog-system 'swi)
             '("[ \t]*\\(Call\\):[ \t]*([ \t0-9]*)"
               1 font-lock-function-name-face))
            (t nil)))
          (trace-exception
           ;; FIXME: Add to compilation-error-regexp-alist instead.
           (cond
            ((eq prolog-system 'sicstus)
             '("[ \t]*[0-9]+[ \t]+[0-9]+[ \t]*\\(Exception\\):"
               1 prolog-exception-face))
            ((eq prolog-system 'swi)
             '("[ \t]*\\(Exception\\):[ \t]*([ \t0-9]*)"
               1 prolog-exception-face))
            (t nil)))
          (error-message-identifier
           ;; FIXME: Add to compilation-error-regexp-alist instead.
           (cond
            ((eq prolog-system 'sicstus)
             '("{\\([A-Z]* ?ERROR:\\)" 1 prolog-exception-face prepend))
            ((eq prolog-system 'swi)
             '("^[[]\\(WARNING:\\)" 1 prolog-builtin-face prepend))
            (t nil)))
          (error-whole-messages
           ;; FIXME: Add to compilation-error-regexp-alist instead.
           (cond
            ((eq prolog-system 'sicstus)
             '("{\\([A-Z]* ?ERROR:.*\\)}[ \t]*$"
               1 font-lock-comment-face append))
            ((eq prolog-system 'swi)
             '("^[[]WARNING:[^]]*[]]$" 0 font-lock-comment-face append))
            (t nil)))
          (error-warning-messages
           ;; FIXME: Add to compilation-error-regexp-alist instead.
           ;; Mostly errors that SICStus asks the user about how to solve,
           ;; such as "NAME CLASH:" for example.
           (cond
            ((eq prolog-system 'sicstus)
             '("^[A-Z ]*[A-Z]+:" 0 prolog-warning-face))
            (t nil)))
          (warning-messages
           ;; FIXME: Add to compilation-error-regexp-alist instead.
           (cond
            ((eq prolog-system 'sicstus)
             '("\\({ ?\\(Warning\\|WARNING\\) ?:.*}\\)[ \t]*$"
               2 prolog-warning-face prepend))
            (t nil))))

      ;; Make font lock list
      (delq
       nil
       (cond
        ((eq major-mode 'prolog-mode)
         (list
          head-predicates
          head-predicates-1
          quoted_atom
          string
          variables
          important-elements
          important-elements-1
          predspecs
          keywords
          sicstus-object-methods
          types
          modes
          directives))
        ((eq major-mode 'prolog-inferior-mode)
         (list
         prompt
         error-message-identifier
         error-whole-messages
         error-warning-messages
         warning-messages
         predspecs
         trace-exit
         trace-fail
         trace-redo
         trace-call
         trace-exception))
        ((eq major-mode 'compilation-mode)
         (list
         error-message-identifier
         error-whole-messages
         error-warning-messages
         warning-messages
         predspecs))))
      ))


;;-------------------------------------------------------------------
;; Indentation stuff
;;-------------------------------------------------------------------

;; NB: This function *MUST* have this optional argument since XEmacs
;; assumes it. This does not mean we have to use it...
(defun prolog-indent-line (&optional _whole-exp)
  "Indent current line as Prolog code.
With argument, indent any additional lines of the same clause
rigidly along with this one (not yet)."
  (interactive "p")
  (let ((indent (prolog-indent-level))
        (pos (- (point-max) (point))))
    (beginning-of-line)
    (skip-chars-forward " \t")
    (indent-line-to indent)
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))

    ;; Align comments
    (if (and prolog-align-comments-flag
             (save-excursion
               (line-beginning-position)
               ;; (let ((start (comment-search-forward (line-end-position) t)))
               ;;   (and start             ;There's a comment to indent.
               ;;       ;; If it's first on the line, we've indented it already
               ;;       ;; and prolog-goto-comment-column would inf-loop.
               ;;       (progn (goto-char start) (skip-chars-backward " \t")
               ;;              (not (bolp)))))))
               (and (looking-at comment-start-skip)
                    ;; The definition of comment-start-skip used in this
                    ;; mode is unusual in that it only matches at BOL.
                    (progn (skip-chars-forward " \t")
                           (not (eq (point) (match-end 1)))))))
        (save-excursion
          (prolog-goto-comment-column t)))

    ;; Insert spaces if needed
    (if (or prolog-electric-tab-flag prolog-electric-if-then-else-flag)
        (prolog-insert-spaces-after-paren))
    ))

(defun prolog-comment-indent ()
  "Compute prolog comment indentation."
  ;; FIXME: Only difference with default behavior is that %%% is not
  ;; flushed to column 0 but just left where the user put it.
  (cond ((looking-at "%%%") (prolog-indentation-level-of-line))
        ((looking-at "%%") (prolog-indent-level))
        (t
         (save-excursion
           (skip-chars-backward " \t")
           ;; Insert one space at least, except at left margin.
           (max (+ (current-column) (if (bolp) 0 1))
                comment-column)))
        ))

(defun prolog-indent-level ()
  "Compute prolog indentation level."
  (save-excursion
    (beginning-of-line)
    (let ((totbal (prolog-region-paren-balance
                   (prolog-clause-start t) (point)))
          (oldpoint (point)))
      (skip-chars-forward " \t")
      (cond
       ((looking-at "%%%") (prolog-indentation-level-of-line))
                                             ;Large comment starts
       ((looking-at "%[^%]") comment-column) ;Small comment starts
       ((bobp) 0)                            ;Beginning of buffer

       ;; If we found '}' then we must check if it's the
       ;; end of an object declaration or something else.
       ((and (looking-at "}")
             (save-excursion
               (forward-char 1)
               ;; Goto to matching {
               (if prolog-use-prolog-tokenizer-flag
                   (prolog-backward-list)
                 (backward-list))
               (skip-chars-backward " \t")
               (backward-char 2)
               (looking-at "::")))
        ;; It was an object
        (if prolog-object-end-to-0-flag
            0
          prolog-indent-width))

       ;;End of /* */ comment
       ((looking-at "\\*/")
        (save-excursion
          (prolog-find-start-of-mline-comment)
          (skip-chars-backward " \t")
          (- (current-column) 2)))

       ;; Here we check if the current line is within a /* */ pair
       ((and (looking-at "[^%/]")
             (eq (prolog-in-string-or-comment) 'cmt))
        (if prolog-indent-mline-comments-flag
            (prolog-find-start-of-mline-comment)
          ;; Same as before
          (prolog-indentation-level-of-line)))

       (t
        (let ((empty t) ind linebal)
          ;; See previous indentation
          (while empty
            (forward-line -1)
            (beginning-of-line)
            (if (bobp)
                (setq empty nil)
              (skip-chars-forward " \t")
              (if (not (or (not (member (prolog-in-string-or-comment)
                                        '(nil txt)))
                           (looking-at "%")
                           (looking-at "\n")))
                  (setq empty nil))))

          ;; Store this line's indentation
          (setq ind (if (bobp)
                        0                ;Beginning of buffer.
                      (current-column))) ;Beginning of clause.

          ;; Compute the balance of the line
          (setq linebal (prolog-paren-balance))
          ;;(message "bal of previous line %d totbal %d" linebal totbal)
          (if (< linebal 0)
              (progn
                ;; Add 'indent-level' mode to find-unmatched-paren instead?
                (end-of-line)
                (setq ind (prolog-find-indent-of-matching-paren))))

          ;;(message "ind %d" ind)
          (beginning-of-line)

          ;; Check if the line ends with ":-", ".", ":: {", "}" (might be
          ;; unnecessary), "&" or ")" (The last four concerns SICStus objects)
          (cond
           ;; If the last char of the line is a '&' then set the indent level
           ;; to prolog-indent-width (used in SICStus objects)
           ((and (eq prolog-system 'sicstus)
                 (looking-at ".+&[ \t]*\\(%.*\\|\\)$"))
            (setq ind prolog-indent-width))

           ;; Increase indentation if the previous line was the head of a rule
           ;; and does not contain a '.'
           ((and (looking-at (format ".*%s[^\\.]*[ \t]*\\(%%.*\\|\\)$"
                                     prolog-head-delimiter))
                 ;; We must check that the match is at a paren balance of 0.
                 (save-excursion
                   (let ((p (point)))
                     (re-search-forward prolog-head-delimiter)
                     (>= 0 (prolog-region-paren-balance p (point))))))
            (let ((headindent
                   (if (< (prolog-paren-balance) 0)
                       (save-excursion
                         (end-of-line)
                         (prolog-find-indent-of-matching-paren))
                     (prolog-indentation-level-of-line))))
              (setq ind (+ headindent prolog-indent-width))))

           ;; The previous line was the head of an object
           ((looking-at ".+ *::.*{[ \t]*$")
            (setq ind prolog-indent-width))

           ;; If a '.' is found at the end of the previous line, then
           ;; decrease the indentation. (The \\(%.*\\|\\) part of the
           ;; regexp is for comments at the end of the line)
           ((and (looking-at "^.+\\.[ \t]*\\(%.*\\|\\)$")
                 ;; Make sure that the '.' found is not in a comment or string
                 (save-excursion
                   (end-of-line)
                   (re-search-backward "\\.[ \t]*\\(%.*\\|\\)$" (point-min))
                   ;; Guard against the real '.' being followed by a
                   ;; commented '.'.
                   (if (eq (prolog-in-string-or-comment) 'cmt)
                       ;; commented out '.'
                       (let ((here (line-beginning-position)))
                         (end-of-line)
                         (re-search-backward "\\.[ \t]*%.*$" here t))
                     (not (prolog-in-string-or-comment))
                     )
                   ))
            (setq ind 0))

           ;; If a '.' is found at the end of the previous line, then
           ;; decrease the indentation. (The /\\*.*\\*/ part of the
           ;; regexp is for C-like comments at the end of the
           ;; line--can we merge with the case above?).
           ((and (looking-at "^.+\\.[ \t]*\\(/\\*.*\\|\\)$")
                 ;; Make sure that the '.' found is not in a comment or string
                 (save-excursion
                   (end-of-line)
                   (re-search-backward "\\.[ \t]*\\(/\\*.*\\|\\)$" (point-min))
                   ;; Guard against the real '.' being followed by a
                   ;; commented '.'.
                   (if (eq (prolog-in-string-or-comment) 'cmt)
                       ;; commented out '.'
                       (let ((here (line-beginning-position)))
                         (end-of-line)
                         (re-search-backward "\\.[ \t]*/\\*.*$" here t))
                     (not (prolog-in-string-or-comment))
                     )
                   ))
            (setq ind 0))

           )

          ;; If the last non comment char is a ',' or left paren or a left-
          ;; indent-regexp then indent to open parenthesis level
          (if (and
               (> totbal 0)
               ;; SICStus objects have special syntax rules if point is
               ;; not inside additional parens (objects are defined
               ;; within {...})
               (not (and (eq prolog-system 'sicstus)
                         (= totbal 1)
                         (prolog-in-object))))
              (if (looking-at
                   (format "\\(%s\\|%s\\|0'.\\|[0-9]+'[0-9a-zA-Z]+\\|[^\n\'\"%%]\\)*\\(,\\|%s\\|%s\\)\[ \t]*\\(%%.*\\|\\)$"
                           prolog-quoted-atom-regexp prolog-string-regexp
                           prolog-left-paren prolog-left-indent-regexp))
                  (progn
                    (goto-char oldpoint)
                    (setq ind (prolog-find-unmatched-paren
                               (if prolog-paren-indent-p
                                   'termdependent
                                 'skipwhite)))
                    ;;(setq ind (prolog-find-unmatched-paren 'termdependent))
                    )
                (goto-char oldpoint)
                (setq ind (prolog-find-unmatched-paren nil))
                ))


          ;; Return the indentation level
          ind
          ))))))

(defun prolog-find-indent-of-matching-paren ()
  "Find the indentation level based on the matching parenthesis.
Indentation level is set to the one the point is after when the function is
called."
  (save-excursion
    ;; Go to the matching paren
    (if prolog-use-prolog-tokenizer-flag
        (prolog-backward-list)
      (backward-list))

    ;; If this was the first paren on the line then return this line's
    ;; indentation level
    (if (prolog-paren-is-the-first-on-line-p)
        (prolog-indentation-level-of-line)
      ;; It was not the first one
      (progn
         ;; Find the next paren
         (prolog-goto-next-paren 0)

         ;; If this paren is a left one then use its column as indent level,
         ;; if not then recurse this function
         (if (looking-at prolog-left-paren)
             (+ (current-column) 1)
           (progn
              (forward-char 1)
              (prolog-find-indent-of-matching-paren)))
         ))
    ))

(defun prolog-indentation-level-of-line ()
  "Return the indentation level of the current line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))

(defun prolog-paren-is-the-first-on-line-p ()
  "Return t if the parenthesis under the point is the first one on the line.
Return nil otherwise.
Note: does not check if the point is actually at a parenthesis!"
  (save-excursion
    (let ((begofline (line-beginning-position)))
      (if (= begofline (point))
          t
        (if (prolog-goto-next-paren begofline)
            nil
          t)))))

(defun prolog-find-unmatched-paren (&optional mode)
  "Return the column of the last unmatched left parenthesis.
If MODE is `skipwhite' then any white space after the parenthesis is added to
the answer.
If MODE is `plusone' then the parenthesis' column +1 is returned.
If MODE is `termdependent' then if the unmatched parenthesis is part of
a compound term the function will work as `skipwhite', otherwise
it will return the column paren plus the value of `prolog-paren-indent'.
If MODE is nil or not set then the parenthesis' exact column is returned."
  (save-excursion
    ;; If the next paren we find is a left one we're finished, if it's
    ;; a right one then we go back one step and recurse
    (prolog-goto-next-paren 0)

    (let ((roundparen (looking-at "(")))
      (if (looking-at prolog-left-paren)
          (let ((not-part-of-term
                 (save-excursion
                   (backward-char 1)
                   (looking-at "[ \t]"))))
            (if (eq mode nil)
                (current-column)
              (if (and roundparen
                       (eq mode 'termdependent)
                       not-part-of-term)
                  (+ (current-column)
                     (if prolog-electric-tab-flag
                         ;; Electric TAB
                         prolog-paren-indent
                       ;; Not electric TAB
                       (if (looking-at ".[ \t]*$")
                           2
                         prolog-paren-indent))
                     )

                (forward-char 1)
                (if (or (eq mode 'skipwhite) (eq mode 'termdependent) )
                    (skip-chars-forward " \t"))
                (current-column))))
        ;; Not looking at left paren
        (progn
          (forward-char 1)
          ;; Go to the matching paren. When we get there we have a total
          ;; balance of 0.
          (if prolog-use-prolog-tokenizer-flag
              (prolog-backward-list)
            (backward-list))
          (prolog-find-unmatched-paren mode)))
      )))


(defun prolog-paren-balance ()
  "Return the parenthesis balance of the current line.
A return value of n means n more left parentheses than right ones."
  (save-excursion
    (end-of-line)
    (prolog-region-paren-balance (line-beginning-position) (point))))

(defun prolog-region-paren-balance (beg end)
  "Return the summed parenthesis balance in the region.
The region is limited by BEG and END positions."
  (save-excursion
    (let ((state (if prolog-use-prolog-tokenizer-flag
                     (prolog-tokenize beg end)
                   (parse-partial-sexp beg end))))
      (nth 0 state))))

(defun prolog-goto-next-paren (limit-pos)
  "Move the point to the next parenthesis earlier in the buffer.
Return t if a match was found before LIMIT-POS.  Return nil otherwise."
  (let ((retval (re-search-backward
                 (concat prolog-left-paren "\\|" prolog-right-paren)
                 limit-pos t)))

    ;; If a match was found but it was in a string or comment, then recurse
    (if (and retval (prolog-in-string-or-comment))
        (prolog-goto-next-paren limit-pos)
      retval)
    ))

(defun prolog-in-string-or-comment ()
  "Check whether string, atom, or comment is under current point.
Return:
 `txt' if the point is in a string, atom, or character code expression
 `cmt' if the point is in a comment
 nil otherwise."
  (save-excursion
    (let* ((start
            (if (eq prolog-parse-mode 'beg-of-line)
                ;; 'beg-of-line
                (save-excursion
                  (let (safepoint)
                    (beginning-of-line)
                    (setq safepoint (point))
                    (while (and (> (point) (point-min))
                                (progn
                                  (forward-line -1)
                                  (end-of-line)
                                  (if (not (bobp))
                                      (backward-char 1))
                                  (looking-at "\\\\"))
                                )
                      (beginning-of-line)
                      (setq safepoint (point)))
                    safepoint))
              ;; 'beg-of-clause
              (prolog-clause-start)))
           (end (point))
           (state (if prolog-use-prolog-tokenizer-flag
                      (prolog-tokenize start end)
                    (if (fboundp 'syntax-ppss)
                        (syntax-ppss)
                      (parse-partial-sexp start end)))))
      (cond
       ((nth 3 state) 'txt) ; String
       ((nth 4 state) 'cmt) ; Comment
       (t
        (cond
         ((looking-at "%") 'cmt) ; Start of a comment
         ((looking-at "/\\*") 'cmt) ; Start of a comment
         ((looking-at "\'") 'txt) ; Start of an atom
         ((looking-at "\"") 'txt) ; Start of a string
         (t nil)
         ))))
    ))

(defun prolog-find-start-of-mline-comment ()
  "Return the start column of a /* */ comment.
This assumes that the point is inside a comment."
  (re-search-backward "/\\*" (point-min) t)
  (forward-char 2)
  (skip-chars-forward " \t")
  (current-column))

(defun prolog-insert-spaces-after-paren ()
  "Insert spaces after the opening parenthesis, \"then\" (->) and \"else\" (;) branches.
Spaces are inserted if all preceding objects on the line are
whitespace characters, parentheses, or then/else branches."
  (save-excursion
    (let ((regexp (concat "(\\|" prolog-left-indent-regexp))
          level)
      (beginning-of-line)
      (skip-chars-forward " \t")
      (when (looking-at regexp)
        ;; Treat "( If -> " lines specially.
        ;;(setq incr (if (looking-at "(.*->")
        ;;               2
        ;;             prolog-paren-indent))

        ;; work on all subsequent "->", "(", ";"
        (while (looking-at regexp)
          (goto-char (match-end 0))
          (setq level (+ (prolog-find-unmatched-paren) prolog-paren-indent))

          ;; Remove old white space
          (let ((start (point)))
            (skip-chars-forward " \t")
            (delete-region start (point)))
          (indent-to level)
          (skip-chars-forward " \t"))
        )))
  (when (save-excursion
          (backward-char 2)
          (looking-at "\\s ;\\|\\s (\\|->")) ; (looking-at "\\s \\((\\|;\\)"))
    (skip-chars-forward " \t"))
  )

;;;; Comment filling

(defun prolog-comment-limits ()
  "Return the current comment limits plus the comment type (block or line).
The comment limits are the range of a block comment or the range that
contains all adjacent line comments (i.e. all comments that starts in
the same column with no empty lines or non-whitespace characters
between them)."
  (let ((here (point))
        lit-limits-b lit-limits-e lit-type beg end
        )
    (save-restriction
      ;; Widen to catch comment limits correctly.
      (widen)
      (setq end (line-end-position)
            beg (line-beginning-position))
      (save-excursion
        (beginning-of-line)
        (setq lit-type (if (search-forward-regexp "%" end t) 'line 'block))
                        ;    (setq lit-type 'line)
                        ;(if (search-forward-regexp "^[ \t]*%" end t)
                        ;    (setq lit-type 'line)
                        ;  (if (not (search-forward-regexp "%" end t))
                        ;      (setq lit-type 'block)
                        ;    (if (not (= (forward-line 1) 0))
                        ;        (setq lit-type 'block)
                        ;      (setq done t
                        ;            ret (prolog-comment-limits)))
                        ;    ))
        (if (eq lit-type 'block)
            (progn
              (goto-char here)
              (when (looking-at "/\\*") (forward-char 2))
              (when (and (looking-at "\\*") (> (point) (point-min))
                         (forward-char -1) (looking-at "/"))
                (forward-char 1))
              (when (save-excursion (search-backward "/*" nil t))
                (list (save-excursion (search-backward "/*") (point))
                      (or (search-forward "*/" nil t) (point-max)) lit-type)))
          ;; line comment
          (setq lit-limits-b (- (point) 1)
                lit-limits-e end)
          (condition-case nil
              (if (progn (goto-char lit-limits-b)
                         (looking-at "%"))
                  (let ((col (current-column)) done)
                    (setq beg (point)
                          end lit-limits-e)
                    ;; Always at the beginning of the comment
                    ;; Go backward now
                    (beginning-of-line)
                    (while (and (zerop (setq done (forward-line -1)))
                                (search-forward-regexp "^[ \t]*%"
                                                       (line-end-position) t)
                                (= (+ 1 col) (current-column)))
                      (setq beg (- (point) 1)))
                    (when (= done 0)
                      (forward-line 1))
                    ;; We may have a line with code above...
                    (when (and (zerop (setq done (forward-line -1)))
                               (search-forward "%" (line-end-position) t)
                               (= (+ 1 col) (current-column)))
                      (setq beg (- (point) 1)))
                    (when (= done 0)
                      (forward-line 1))
                    ;; Go forward
                    (goto-char lit-limits-b)
                    (beginning-of-line)
                    (while (and (zerop (forward-line 1))
                                (search-forward-regexp "^[ \t]*%"
                                                       (line-end-position) t)
                                (= (+ 1 col) (current-column)))
                      (setq end (line-end-position)))
                    (list beg end lit-type))
                (list lit-limits-b lit-limits-e lit-type)
                )
            (error (list lit-limits-b lit-limits-e lit-type))))
        ))))

(defun prolog-guess-fill-prefix ()
  ;; fill 'txt entities?
  (when (save-excursion
          (end-of-line)
          (equal (prolog-in-string-or-comment) 'cmt))
    (let* ((bounds (prolog-comment-limits))
           (cbeg (car bounds))
           (type (nth 2 bounds))
           beg end)
      (save-excursion
        (end-of-line)
        (setq end (point))
        (beginning-of-line)
        (setq beg (point))
        (if (and (eq type 'line)
                 (> cbeg beg)
                 (save-excursion (not (search-forward-regexp "^[ \t]*%"
                                                             cbeg t))))
            (progn
              (goto-char cbeg)
              (search-forward-regexp "%+[ \t]*" end t)
              (prolog-replace-in-string (buffer-substring beg (point))
                                        "[^ \t%]" " "))
          ;(goto-char beg)
          (if (search-forward-regexp "^[ \t]*\\(%+\\|\\*+\\|/\\*+\\)[ \t]*"
                                     end t)
              (prolog-replace-in-string (buffer-substring beg (point)) "/" " ")
            (beginning-of-line)
            (when (search-forward-regexp "^[ \t]+" end t)
              (buffer-substring beg (point)))))))))

(defun prolog-fill-paragraph ()
  "Fill paragraph comment at or after point."
  (interactive)
  (let* ((bounds (prolog-comment-limits))
         (type (nth 2 bounds)))
    (if (eq type 'line)
        (let ((fill-prefix (prolog-guess-fill-prefix)))
          (fill-paragraph nil))
      (save-excursion
        (save-restriction
          ;; exclude surrounding lines that delimit a multiline comment
          ;; and don't contain alphabetic characters, like "/*******",
          ;; "- - - */" etc.
          (save-excursion
            (backward-paragraph)
            (unless (bobp) (forward-line))
            (if (string-match "^/\\*[^a-zA-Z]*$" (thing-at-point 'line))
                (narrow-to-region (point-at-eol) (point-max))))
          (save-excursion
            (forward-paragraph)
            (forward-line -1)
            (if (string-match "^[^a-zA-Z]*\\*/$" (thing-at-point 'line))
                (narrow-to-region (point-min) (point-at-bol))))
          (let ((fill-prefix (prolog-guess-fill-prefix)))
            (fill-paragraph nil))))
      )))

(defun prolog-do-auto-fill ()
  "Carry out Auto Fill for Prolog mode.
In effect it sets the `fill-prefix' when inside comments and then calls
`do-auto-fill'."
  (let ((fill-prefix (prolog-guess-fill-prefix)))
    (do-auto-fill)
    ))

(defalias 'prolog-replace-in-string
  (if (fboundp 'replace-in-string)
      #'replace-in-string
    (lambda (str regexp newtext &optional literal)
      (replace-regexp-in-string regexp newtext str nil literal))))

;;-------------------------------------------------------------------
;; The tokenizer
;;-------------------------------------------------------------------

(defconst prolog-tokenize-searchkey
  (concat "[0-9]+'"
          "\\|"
          "['\"]"
          "\\|"
          prolog-left-paren
          "\\|"
          prolog-right-paren
          "\\|"
          "%"
          "\\|"
          "/\\*"
          ))

(defun prolog-tokenize (beg end &optional stopcond)
  "Tokenize a region of prolog code between BEG and END.
STOPCOND decides the stop condition of the parsing.  Valid values
are 'zerodepth which stops the parsing at the first right parenthesis
where the parenthesis depth is zero, 'skipover which skips over
the current entity (e.g. a list, a string, etc.) and nil.

The function returns a list with the following information:
 0. parenthesis depth
 3. 'atm if END is inside an atom
    'str if END is inside a string
    'chr if END is in a character code expression (0'x)
    nil otherwise
 4. non-nil if END is inside a comment
 5. end position (always equal to END if STOPCOND is nil)
The rest of the elements are undefined."
  (save-excursion
    (let* ((end2 (1+ end))
           oldp
           (depth 0)
           (quoted nil)
           inside_cmt
           (endpos end2)
           skiptype ; The type of entity we'll skip over
           )
      (goto-char beg)

      (if (and (eq stopcond 'skipover)
               (looking-at "[^[({'\"]"))
          (setq endpos (point))                ; Stay where we are
        (while (and
                (re-search-forward prolog-tokenize-searchkey end2 t)
                (< (point) end2))
          (progn
            (setq oldp (point))
            (goto-char (match-beginning 0))
            (cond
             ;; Atoms and strings
             ((looking-at "'")
              ;; Find end of atom
              (if (re-search-forward "[^\\]'" end2 'limit)
                  ;; Found end of atom
                  (progn
                    (setq oldp end2)
                    (if (and (eq stopcond 'skipover)
                             (not skiptype))
                        (setq endpos (point))
                      (setq oldp (point)))) ; Continue tokenizing
                (setq quoted 'atm)))

             ((looking-at "\"")
              ;; Find end of string
              (if (re-search-forward "[^\\]\"" end2 'limit)
                  ;; Found end of string
                  (progn
                    (setq oldp end2)
                    (if (and (eq stopcond 'skipover)
                             (not skiptype))
                        (setq endpos (point))
                      (setq oldp (point)))) ; Continue tokenizing
                (setq quoted 'str)))

             ;; Paren stuff
             ((looking-at prolog-left-paren)
              (setq depth (1+ depth))
              (setq skiptype 'paren))

             ((looking-at prolog-right-paren)
              (setq depth (1- depth))
              (if (and
                   (or (eq stopcond 'zerodepth)
                       (and (eq stopcond 'skipover)
                            (eq skiptype 'paren)))
                   (= depth 0))
                  (progn
                    (setq endpos (1+ (point)))
                    (setq oldp end2))))

             ;; Comment stuff
             ((looking-at comment-start)
              (end-of-line)
              ;; (if (>= (point) end2)
              (if (>= (point) end)
                  (progn
                    (setq inside_cmt t)
                    (setq oldp end2))
                (setq oldp (point))))

             ((looking-at "/\\*")
              (if (re-search-forward "\\*/" end2 'limit)
                  (setq oldp (point))
                (setq inside_cmt t)
                (setq oldp end2)))

             ;; 0'char
             ((looking-at "0'")
              (setq oldp (1+ (match-end 0)))
              (if (> oldp end)
                  (setq quoted 'chr)))

             ;; base'number
             ((looking-at "[0-9]+'")
              (goto-char (match-end 0))
              (skip-chars-forward "0-9a-zA-Z")
              (setq oldp (point)))


             )
            (goto-char oldp)
            ))                                ; End of while
        )

      ;; Deal with multi-line comments
      (and (prolog-inside-mline-comment end)
           (setq inside_cmt t))

      ;; Create return list
      (list depth nil nil quoted inside_cmt endpos)
      )))

(defun prolog-inside-mline-comment (here)
  (save-excursion
    (goto-char here)
    (let* ((next-close (save-excursion (search-forward "*/" nil t)))
           (next-open  (save-excursion (search-forward "/*" nil t)))
           (prev-open  (save-excursion (search-backward "/*" nil t)))
           (prev-close (save-excursion (search-backward "*/" nil t)))
           (unmatched-next-close (and next-close
                                      (or (not next-open)
                                          (> next-open next-close))))
           (unmatched-prev-open  (and prev-open
                                      (or (not prev-close)
                                          (> prev-open prev-close))))
           )
      (or unmatched-next-close unmatched-prev-open)
      )))


;;-------------------------------------------------------------------
;; Online help
;;-------------------------------------------------------------------

(defvar prolog-help-function
  '((mercury nil)
    (eclipse prolog-help-online)
    ;; (sicstus prolog-help-info)
    (sicstus prolog-find-documentation)
    (swi prolog-help-online)
    (t prolog-help-online))
  "Alist for the name of the function for finding help on a predicate.")

(defun prolog-help-on-predicate ()
  "Invoke online help on the atom under cursor."
  (interactive)

  (cond
   ;; Redirect help for SICStus to `prolog-find-documentation'.
   ((eq prolog-help-function-i 'prolog-find-documentation)
    (prolog-find-documentation))

   ;; Otherwise, ask for the predicate name and then call the function
   ;; in prolog-help-function-i
   (t
    (let* ((word (prolog-atom-under-point))
           (predicate (read-string
                       (format "Help on predicate%s: "
                               (if word
                                   (concat " (default " word ")")
                                 ""))
                       nil nil word))
           ;;point
           )
      (if prolog-help-function-i
          (funcall prolog-help-function-i predicate)
        (error "Sorry, no help method defined for this Prolog system."))))
   ))

(defun prolog-help-info (predicate)
  (let ((buffer (current-buffer))
        oldp
        (str (concat "^\\* " (regexp-quote predicate) " */")))
    (require 'info)
    (pop-to-buffer nil)
    (Info-goto-node prolog-info-predicate-index)
    (if (not (re-search-forward str nil t))
        (error (format "Help on predicate `%s' not found." predicate)))

    (setq oldp (point))
    (if (re-search-forward str nil t)
        ;; Multiple matches, ask user
        (let ((max 2)
              n)
          ;; Count matches
          (while (re-search-forward str nil t)
            (setq max (1+ max)))

          (goto-char oldp)
          (re-search-backward "[^ /]" nil t)
          (recenter 0)
          (setq n (read-string  ;; was read-input, which is obsolete
                   (format "Several matches, choose (1-%d): " max) "1"))
          (forward-line (- (string-to-number n) 1)))
      ;; Single match
      (re-search-backward "[^ /]" nil t))

    ;; (Info-follow-nearest-node (point))
    (prolog-Info-follow-nearest-node)
    (re-search-forward (concat "^`" (regexp-quote predicate)) nil t)
    (beginning-of-line)
    (recenter 0)
    (pop-to-buffer buffer)))

(defun prolog-Info-follow-nearest-node ()
  (if (featurep 'xemacs)
      (Info-follow-nearest-node (point))
    (Info-follow-nearest-node)))

(defun prolog-help-online (predicate)
  (prolog-ensure-process)
  (process-send-string "prolog" (concat "help(" predicate ").\n"))
  (display-buffer "*prolog*"))

(defun prolog-help-apropos (string)
  "Find Prolog apropos on given STRING.
This function is only available when `prolog-system' is set to `swi'."
  (interactive "sApropos: ")
  (cond
   ((eq prolog-system 'swi)
    (prolog-ensure-process)
    (process-send-string "prolog" (concat "apropos(" string ").\n"))
    (display-buffer "*prolog*"))
   (t
    (error "Sorry, no Prolog apropos available for this Prolog system."))))

(defun prolog-atom-under-point ()
  "Return the atom under or left to the point."
  (save-excursion
    (let ((nonatom_chars "[](){},\. \t\n")
          start)
      (skip-chars-forward (concat "^" nonatom_chars))
      (skip-chars-backward nonatom_chars)
      (skip-chars-backward (concat "^" nonatom_chars))
      (setq start (point))
      (skip-chars-forward (concat "^" nonatom_chars))
      (buffer-substring-no-properties start (point))
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help function with completion
;; Stolen from Per Mildner's SICStus debugger mode and modified

(defun prolog-find-documentation ()
  "Go to the Info node for a predicate in the SICStus Info manual."
  (interactive)
  (let ((pred (prolog-read-predicate)))
    (prolog-goto-predicate-info pred)))

(defvar prolog-info-alist nil
  "Alist with all builtin predicates.
Only for internal use by `prolog-find-documentation'")

;; Very similar to prolog-help-info except that that function cannot
;; cope with arity and that it asks the user if there are several
;; functors with different arity. This function also uses
;; prolog-info-alist for finding the info node, rather than parsing
;; the predicate index.
(defun prolog-goto-predicate-info (predicate)
  "Go to the info page for PREDICATE, which is a PredSpec."
  (interactive)
  (require 'info)
  (string-match "\\(.*\\)/\\([0-9]+\\).*$" predicate)
  (let ((buffer (current-buffer))
        (name (match-string 1 predicate))
        (arity (string-to-number (match-string 2 predicate)))
        ;oldp
        ;(str (regexp-quote predicate))
        )
    (pop-to-buffer nil)

    (Info-goto-node
     prolog-info-predicate-index) ;; We must be in the SICStus pages
    (Info-goto-node (car (cdr (assoc predicate prolog-info-alist))))

    (prolog-find-term (regexp-quote name) arity "^`")

    (recenter 0)
    (pop-to-buffer buffer))
)

(defun prolog-read-predicate ()
  "Read a PredSpec from the user.
Returned value is a string \"FUNCTOR/ARITY\".
Interaction supports completion."
  (let ((default (prolog-atom-under-point)))
    ;; If the predicate index is not yet built, do it now
    (if (not prolog-info-alist)
        (prolog-build-info-alist))
    ;; Test if the default string could be the base for completion.
    ;; Discard it if not.
    (if (eq (try-completion default prolog-info-alist) nil)
        (setq default nil))
    ;; Read the PredSpec from the user
    (completing-read
     (if (zerop (length default))
         "Help on predicate: "
       (concat "Help on predicate (default " default "): "))
     prolog-info-alist nil t nil nil default)))

(defun prolog-build-info-alist (&optional verbose)
  "Build an alist of all builtins and library predicates.
Each element is of the form (\"NAME/ARITY\" . (INFO-NODE1 INFO-NODE2 ...)).
Typically there is just one Info node associated with each name
If an optional argument VERBOSE is non-nil, print messages at the beginning
and end of list building."
  (if verbose
      (message "Building info alist..."))
  (setq prolog-info-alist
        (let ((l ())
              (last-entry (cons "" ())))
          (save-excursion
            (save-window-excursion
              ;; select any window but the minibuffer (as we cannot switch
              ;; buffers in minibuffer window.
              ;; I am not sure this is the right/best way
              (if (active-minibuffer-window)  ; nil if none active
                  (select-window (next-window)))
              ;; Do this after going away from minibuffer window
              (save-window-excursion
                (info))
              (Info-goto-node prolog-info-predicate-index)
              (goto-char (point-min))
              (while (re-search-forward
                      "^\\* \\(.+\\)/\\([0-9]+\\)\\([^\n:*]*\\):" nil t)
                (let* ((name (match-string 1))
                       (arity (string-to-number (match-string 2)))
                       (comment (match-string 3))
                       (fa (format "%s/%d%s" name arity comment))
                       info-node)
                  (beginning-of-line)
                  ;; Extract the info node name
                  (setq info-node (progn
                                    (re-search-forward ":[ \t]*\\([^:]+\\).$")
                                    (match-string 1)
                                   ))
                  ;; ###### Easier? (from Milan version 0.1.28)
                  ;; (setq info-node (Info-extract-menu-node-name))
                  (if (equal fa (car last-entry))
                      (setcdr last-entry (cons info-node (cdr last-entry)))
                    (setq last-entry (cons fa (list info-node))
                          l (cons last-entry l)))))
              (nreverse l)
              ))))
  (if verbose
      (message "Building info alist... done.")))


;;-------------------------------------------------------------------
;; Miscellaneous functions
;;-------------------------------------------------------------------

;; For Windows. Change backslash to slash. SICStus handles either
;; path separator but backslash must be doubled, therefore use slash.
(defun prolog-bsts (string)
  "Change backslashes to slashes in STRING."
  (let ((str1 (copy-sequence string))
        (len (length string))
        (i 0))
    (while (< i len)
      (if (char-equal (aref str1 i) ?\\)
          (aset str1 i ?/))
      (setq i (1+ i)))
    str1))

;;(defun prolog-temporary-file ()
;;  "Make temporary file name for compilation."
;;  (make-temp-name
;;   (concat
;;    (or
;;     (getenv "TMPDIR")
;;     (getenv "TEMP")
;;     (getenv "TMP")
;;     (getenv "SYSTEMP")
;;     "/tmp")
;;    "/prolcomp")))
;;(setq prolog-temp-filename (prolog-bsts (prolog-temporary-file)))

(defun prolog-temporary-file ()
  "Make temporary file name for compilation."
  (if prolog-temporary-file-name
      ;; We already have a file, erase content and continue
      (progn
        (write-region "" nil prolog-temporary-file-name nil 'silent)
        prolog-temporary-file-name)
    ;; Actually create the file and set `prolog-temporary-file-name'
    ;; accordingly.
    (setq prolog-temporary-file-name
          (make-temp-file "prolcomp" nil ".pl"))))

(defun prolog-goto-prolog-process-buffer ()
  "Switch to the prolog process buffer and go to its end."
  (switch-to-buffer-other-window "*prolog*")
  (goto-char (point-max))
)

(defun prolog-enable-sicstus-sd ()
  "Enable the source level debugging facilities of SICStus 3.7 and later."
  (interactive)
  (require 'pltrace)  ; Load the SICStus debugger code
  ;; Turn on the source level debugging by default
  (add-hook 'prolog-inferior-mode-hook 'pltrace-on)
  (if (not prolog-use-sicstus-sd)
      (progn
        ;; If there is a *prolog* buffer, then call pltrace-on
        (if (get-buffer "*prolog*")
            ;; Avoid compilation warnings by using eval
            (eval '(pltrace-on)))
        (setq prolog-use-sicstus-sd t)
        )))

(defun prolog-disable-sicstus-sd ()
  "Disable the source level debugging facilities of SICStus 3.7 and later."
  (interactive)
  (setq prolog-use-sicstus-sd nil)
  ;; Remove the hook
  (remove-hook 'prolog-inferior-mode-hook 'pltrace-on)
  ;; If there is a *prolog* buffer, then call pltrace-off
  (if (get-buffer "*prolog*")
      ;; Avoid compile warnings by using eval
      (eval '(pltrace-off))))

(defun prolog-toggle-sicstus-sd ()
  ;; FIXME: Use define-minor-mode.
  "Toggle the source level debugging facilities of SICStus 3.7 and later."
  (interactive)
  (if prolog-use-sicstus-sd
      (prolog-disable-sicstus-sd)
    (prolog-enable-sicstus-sd)))

(defun prolog-debug-on (&optional arg)
  "Enable debugging.
When called with prefix argument ARG, disable debugging instead."
  (interactive "P")
  (if arg
      (prolog-debug-off)
    (prolog-process-insert-string (get-process "prolog")
                                  prolog-debug-on-string)
    (process-send-string "prolog" prolog-debug-on-string)))

(defun prolog-debug-off ()
  "Disable debugging."
  (interactive)
  (prolog-process-insert-string (get-process "prolog")
                                prolog-debug-off-string)
  (process-send-string "prolog" prolog-debug-off-string))

(defun prolog-trace-on (&optional arg)
  "Enable tracing.
When called with prefix argument ARG, disable tracing instead."
  (interactive "P")
  (if arg
      (prolog-trace-off)
    (prolog-process-insert-string (get-process "prolog")
                                  prolog-trace-on-string)
    (process-send-string "prolog" prolog-trace-on-string)))

(defun prolog-trace-off ()
  "Disable tracing."
  (interactive)
  (prolog-process-insert-string (get-process "prolog")
                                prolog-trace-off-string)
  (process-send-string "prolog" prolog-trace-off-string))

(defun prolog-zip-on (&optional arg)
  "Enable zipping (for SICStus 3.7 and later).
When called with prefix argument ARG, disable zipping instead."
  (interactive "P")
  (if (not (and (eq prolog-system 'sicstus)
                (prolog-atleast-version '(3 . 7))))
      (error "Only works for SICStus 3.7 and later"))
  (if arg
      (prolog-zip-off)
    (prolog-process-insert-string (get-process "prolog")
                                  prolog-zip-on-string)
    (process-send-string "prolog" prolog-zip-on-string)))

(defun prolog-zip-off ()
  "Disable zipping (for SICStus 3.7 and later)."
  (interactive)
  (prolog-process-insert-string (get-process "prolog")
                                prolog-zip-off-string)
  (process-send-string "prolog" prolog-zip-off-string))

;; (defun prolog-create-predicate-index ()
;;   "Create an index for all predicates in the buffer."
;;   (let ((predlist '())
;;         clauseinfo
;;         object
;;         pos
;;         )
;;     (goto-char (point-min))
;;     ;; Replace with prolog-clause-start!
;;     (while (re-search-forward "^.+:-" nil t)
;;       (setq pos (match-beginning 0))
;;       (setq clauseinfo (prolog-clause-info))
;;       (setq object (prolog-in-object))
;;       (setq predlist (append
;;                       predlist
;;                       (list (cons
;;                              (if (and (eq prolog-system 'sicstus)
;;                                       (prolog-in-object))
;;                                  (format "%s::%s/%d"
;;                                          object
;;                                          (nth 0 clauseinfo)
;;                                          (nth 1 clauseinfo))
;;                                (format "%s/%d"
;;                                        (nth 0 clauseinfo)
;;                                        (nth 1 clauseinfo)))
;;                              pos
;;                              ))))
;;       (prolog-end-of-predicate))
;;     predlist))

(defun prolog-get-predspec ()
  (save-excursion
    (let ((state (prolog-clause-info))
          (object (prolog-in-object)))
      (if (or (equal (nth 0 state) "") (equal (prolog-in-string-or-comment) 'cmt))
          nil
        (if (and (eq prolog-system 'sicstus)
                 object)
            (format "%s::%s/%d"
                    object
                    (nth 0 state)
                    (nth 1 state))
          (format "%s/%d"
                  (nth 0 state)
                  (nth 1 state)))
        ))))

;; For backward compatibility. Stolen from custom.el.
(or (fboundp 'match-string)
    ;; Introduced in Emacs 19.29.
    (defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
          (substring string (match-beginning num) (match-end num))
        (buffer-substring (match-beginning num) (match-end num))))))

(defun prolog-pred-start ()
  "Return the starting point of the first clause of the current predicate."
  (save-excursion
    (goto-char (prolog-clause-start))
    ;; Find first clause, unless it was a directive
    (if (and (not (looking-at "[:?]-"))
             (not (looking-at "[ \t]*[%/]"))  ; Comment

             )
        (let* ((pinfo (prolog-clause-info))
               (predname (nth 0 pinfo))
               (arity (nth 1 pinfo))
               (op (point)))
          (while (and (re-search-backward
                       (format "^%s\\([(\\.]\\| *%s\\)"
                               predname prolog-head-delimiter) nil t)
                      (= arity (nth 1 (prolog-clause-info)))
                      )
            (setq op (point)))
          (if (eq prolog-system 'mercury)
              ;; Skip to the beginning of declarations of the predicate
              (progn
                (goto-char (prolog-beginning-of-clause))
                (while (and (not (eq (point) op))
                            (looking-at
                             (format ":-[ \t]*\\(pred\\|mode\\)[ \t]+%s"
                                     predname)))
                  (setq op (point))
                  (goto-char (prolog-beginning-of-clause)))))
          op)
      (point))))

(defun prolog-pred-end ()
  "Return the position at the end of the last clause of the current predicate."
  (save-excursion
    (goto-char (prolog-clause-end))        ; if we are before the first predicate
    (goto-char (prolog-clause-start))
    (let* ((pinfo (prolog-clause-info))
          (predname (nth 0 pinfo))
          (arity (nth 1 pinfo))
          oldp
          (notdone t)
          (op (point)))
      (if (looking-at "[:?]-")
          ;; This was a directive
          (progn
            (if (and (eq prolog-system 'mercury)
                     (looking-at
                      (format ":-[ \t]*\\(pred\\|mode\\)[ \t]+\\(%s+\\)"
                              prolog-atom-regexp)))
                ;; Skip predicate declarations
                (progn
                  (setq predname (buffer-substring-no-properties
                                  (match-beginning 2) (match-end 2)))
                  (while (re-search-forward
                          (format
                           "\n*\\(:-[ \t]*\\(pred\\|mode\\)[ \t]+\\)?%s[( \t]"
                           predname)
                          nil t))))
            (goto-char (prolog-clause-end))
            (setq op (point)))
        ;; It was not a directive, find the last clause
        (while (and notdone
                    (re-search-forward
                     (format "^%s\\([(\\.]\\| *%s\\)"
                             predname prolog-head-delimiter) nil t)
                    (= arity (nth 1 (prolog-clause-info))))
          (setq oldp (point))
          (setq op (prolog-clause-end))
          (if (>= oldp op)
              ;; End of clause not found.
              (setq notdone nil)
            ;; Continue while loop
            (goto-char op))))
      op)))

(defun prolog-clause-start (&optional not-allow-methods)
  "Return the position at the start of the head of the current clause.
If NOTALLOWMETHODS is non-nil then do not match on methods in
objects (relevant only if 'prolog-system' is set to 'sicstus)."
  (save-excursion
    (let ((notdone t)
          (retval (point-min)))
      (end-of-line)

      ;; SICStus object?
      (if (and (not not-allow-methods)
               (eq prolog-system 'sicstus)
               (prolog-in-object))
          (while (and
                  notdone
                  ;; Search for a head or a fact
                  (re-search-backward
                   ;; If in object, then find method start.
                   ;; "^[ \t]+[a-z$].*\\(:-\\|&\\|:: {\\|,\\)"
                   "^[ \t]+[a-z$].*\\(:-\\|&\\|:: {\\)" ; The comma causes
                                        ; problems since we cannot assume
                                        ; that the line starts at column 0,
                                        ; thus we don't know if the line
                                        ; is a head or a subgoal
                   (point-min) t))
            (if (>= (prolog-paren-balance) 0) ; To no match on "   a) :-"
                ;; Start of method found
                (progn
                  (setq retval (point))
                  (setq notdone nil)))
            )                                ; End of while

        ;; Not in object
        (while (and
                notdone
                ;; Search for a text at beginning of a line
                ;; ######
                ;; (re-search-backward "^[a-z$']" nil t))
                (let ((case-fold-search nil))
                  (re-search-backward
                   ;; (format "^[%s$']" prolog-lower-case-string)
                   ;; FIXME: Use [:lower:]
                   (format "^\\([%s$']\\|[:?]-\\)" prolog-lower-case-string)
                   nil t)))
          (let ((bal (prolog-paren-balance)))
            (cond
             ((> bal 0)
              ;; Start of clause found
              (progn
                (setq retval (point))
                (setq notdone nil)))
             ((and (= bal 0)
                   (looking-at
                    (format ".*\\(\\.\\|%s\\|!,\\)[ \t]*\\(%%.*\\|\\)$"
                            prolog-head-delimiter)))
              ;; Start of clause found if the line ends with a '.' or
              ;; a prolog-head-delimiter
              (progn
                (setq retval (point))
                (setq notdone nil))
              )
             (t nil) ; Do nothing
             ))))

        retval)))

(defun prolog-clause-end (&optional not-allow-methods)
  "Return the position at the end of the current clause.
If NOTALLOWMETHODS is non-nil then do not match on methods in
objects (relevant only if 'prolog-system' is set to 'sicstus)."
  (save-excursion
    (beginning-of-line) ; Necessary since we use "^...." for the search.
    (if (re-search-forward
         (if (and (not not-allow-methods)
                  (eq prolog-system 'sicstus)
                  (prolog-in-object))
             (format
              "^\\(%s\\|%s\\|[^\n\'\"%%]\\)*&[ \t]*\\(\\|%%.*\\)$\\|[ \t]*}"
              prolog-quoted-atom-regexp prolog-string-regexp)
           (format
            "^\\(%s\\|%s\\|[^\n\'\"%%]\\)*\\.[ \t]*\\(\\|%%.*\\)$"
            prolog-quoted-atom-regexp prolog-string-regexp))
         nil t)
        (if (and (prolog-in-string-or-comment)
                 (not (eobp)))
            (progn
              (forward-char)
              (prolog-clause-end))
          (point))
      (point))))

(defun prolog-clause-info ()
  "Return a (name arity) list for the current clause."
  (save-excursion
    (goto-char (prolog-clause-start))
    (let* ((op (point))
           (predname
            (if (looking-at prolog-atom-char-regexp)
                (progn
                  (skip-chars-forward "^ (\\.")
                  (buffer-substring op (point)))
              ""))
           (arity 0))
      ;; Retrieve the arity.
      (if (looking-at prolog-left-paren)
          (let ((endp (save-excursion
                        (prolog-forward-list) (point))))
            (setq arity 1)
            (forward-char 1)            ; Skip the opening paren.
            (while (progn
                     (skip-chars-forward "^[({,'\"")
                     (< (point) endp))
              (if (looking-at ",")
                  (progn
                    (setq arity (1+ arity))
                    (forward-char 1)    ; Skip the comma.
                    )
                ;; We found a string, list or something else we want
                ;; to skip over. Always use prolog-tokenize,
                ;; parse-partial-sexp does not have a 'skipover mode.
                (goto-char (nth 5 (prolog-tokenize (point) endp 'skipover))))
              )))
      (list predname arity))))

(defun prolog-in-object ()
  "Return object name if the point is inside a SICStus object definition."
  ;; Return object name if the last line that starts with a character
  ;; that is neither white space nor a comment start
  (save-excursion
    (if (save-excursion
          (beginning-of-line)
          (looking-at "\\([^\n ]+\\)[ \t]*::[ \t]*{"))
        ;; We were in the head of the object
        (match-string 1)
      ;; We were not in the head
      (if (and (re-search-backward "^[a-z$'}]" nil t)
               (looking-at "\\([^\n ]+\\)[ \t]*::[ \t]*{"))
          (match-string 1)
        nil))))

(defun prolog-forward-list ()
  "Move the point to the matching right parenthesis."
  (interactive)
  (if prolog-use-prolog-tokenizer-flag
      (let ((state (prolog-tokenize (point) (point-max) 'zerodepth)))
        (goto-char (nth 5 state)))
    (forward-list)))

;; NB: This could be done more efficiently!
(defun prolog-backward-list ()
  "Move the point to the matching left parenthesis."
  (interactive)
  (if prolog-use-prolog-tokenizer-flag
      (let ((bal 0)
            (paren-regexp (concat prolog-left-paren "\\|" prolog-right-paren))
            (notdone t))
        ;; FIXME: Doesn't this incorrectly count 0'( and 0') ?
        (while (and notdone (re-search-backward paren-regexp nil t))
          (cond
           ((looking-at prolog-left-paren)
            (if (not (prolog-in-string-or-comment))
                (setq bal (1+ bal)))
            (if (= bal 0)
                (setq notdone nil)))
           ((looking-at prolog-right-paren)
            (if (not (prolog-in-string-or-comment))
                (setq bal (1- bal))))
           )))
    (backward-list)))

(defun prolog-beginning-of-clause ()
  "Move to the beginning of current clause.
If already at the beginning of clause, move to previous clause."
  (interactive)
  (let ((point (point))
        (new-point (prolog-clause-start)))
    (if (and (>= new-point point)
             (> point 1))
        (progn
          (goto-char (1- point))
          (goto-char (prolog-clause-start)))
      (goto-char new-point)
      (skip-chars-forward " \t"))))

;; (defun prolog-previous-clause ()
;;   "Move to the beginning of the previous clause."
;;   (interactive)
;;   (forward-char -1)
;;   (prolog-beginning-of-clause))

(defun prolog-end-of-clause ()
  "Move to the end of clause.
If already at the end of clause, move to next clause."
  (interactive)
  (let ((point (point))
        (new-point (prolog-clause-end)))
    (if (and (<= new-point point)
             (not (eq new-point (point-max))))
        (progn
          (goto-char (1+ point))
          (goto-char (prolog-clause-end)))
      (goto-char new-point))))

;; (defun prolog-next-clause ()
;;   "Move to the beginning of the next clause."
;;   (interactive)
;;   (prolog-end-of-clause)
;;   (forward-char)
;;   (prolog-end-of-clause)
;;   (prolog-beginning-of-clause))

(defun prolog-beginning-of-predicate ()
  "Go to the nearest beginning of predicate before current point.
Return the final point or nil if no such a beginning was found."
  (interactive)
  (let ((op (point))
        (pos (prolog-pred-start)))
    (if pos
        (if (= op pos)
            (if (not (bobp))
                (progn
                  (goto-char pos)
                  (backward-char 1)
                  (setq pos (prolog-pred-start))
                  (if pos
                      (progn
                        (goto-char pos)
                        (point)))))
          (goto-char pos)
          (point)))))

(defun prolog-end-of-predicate ()
  "Go to the end of the current predicate."
  (interactive)
  (let ((op (point)))
    (goto-char (prolog-pred-end))
    (if (= op (point))
        (progn
          (forward-line 1)
          (prolog-end-of-predicate)))))

(defun prolog-insert-predspec ()
  "Insert the predspec for the current predicate."
  (interactive)
  (let* ((pinfo (prolog-clause-info))
         (predname (nth 0 pinfo))
         (arity (nth 1 pinfo)))
    (insert (format "%s/%d" predname arity))))

(defun prolog-view-predspec ()
  "Insert the predspec for the current predicate."
  (interactive)
  (let* ((pinfo (prolog-clause-info))
         (predname (nth 0 pinfo))
         (arity (nth 1 pinfo)))
    (message (format "%s/%d" predname arity))))

(defun prolog-insert-predicate-template ()
  "Insert the template for the current clause."
  (interactive)
  (let* ((n 1)
         oldp
         (pinfo (prolog-clause-info))
         (predname (nth 0 pinfo))
         (arity (nth 1 pinfo)))
    (insert predname)
    (if (> arity 0)
        (progn
          (insert "(")
 	  (when prolog-electric-dot-full-predicate-template
 	    (setq oldp (point))
 	    (while (< n arity)
 	      (insert ",")
 	      (setq n (1+ n)))
 	    (insert ")")
 	    (goto-char oldp))
          ))
  ))

(defun prolog-insert-next-clause ()
  "Insert newline and the name of the current clause."
  (interactive)
  (insert "\n")
  (prolog-insert-predicate-template))

(defun prolog-insert-module-modeline ()
  "Insert a modeline for module specification.
This line should be first in the buffer.
The module name should be written manually just before the semi-colon."
  (interactive)
  (insert "%%% -*- Module: ; -*-\n")
  (backward-char 6))

(defalias 'prolog-uncomment-region
  (if (fboundp 'uncomment-region) #'uncomment-region
    (lambda (beg end)
      "Uncomment the region between BEG and END."
      (interactive "r")
      (comment-region beg end -1))))

(defun prolog-goto-comment-column (&optional nocreate)
  "Move comments on the current line to the correct position.
If NOCREATE is nil (or omitted) and there is no comment on the line, then
a new comment is created."
  (interactive)
  (beginning-of-line)
  (if (or (not nocreate)
          (and
           (re-search-forward
            (format "^\\(\\(%s\\|%s\\|[^\n\'\"%%]\\)*\\)%% *"
                    prolog-quoted-atom-regexp prolog-string-regexp)
            (line-end-position) 'limit)
           (progn
             (goto-char (match-beginning 0))
             (not (eq (prolog-in-string-or-comment) 'txt)))))
      (indent-for-comment)))

(defun prolog-indent-predicate ()
  "*Indent the current predicate."
  (interactive)
  (indent-region (prolog-pred-start) (prolog-pred-end) nil))

(defun prolog-indent-buffer ()
  "*Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun prolog-mark-clause ()
  "Put mark at the end of this clause and move point to the beginning."
  (interactive)
  (let ((pos (point)))
    (goto-char (prolog-clause-end))
    (forward-line 1)
    (beginning-of-line)
    (set-mark (point))
    (goto-char pos)
    (goto-char (prolog-clause-start))))

(defun prolog-mark-predicate ()
  "Put mark at the end of this predicate and move point to the beginning."
  (interactive)
  (goto-char (prolog-pred-end))
  (let ((pos (point)))
    (forward-line 1)
    (beginning-of-line)
    (set-mark (point))
    (goto-char pos)
    (goto-char (prolog-pred-start))))

;; Stolen from `cc-mode.el':
(defun prolog-electric-delete (arg)
  "Delete preceding character or whitespace.
If `prolog-hungry-delete-key-flag' is non-nil, then all preceding whitespace is
consumed.  If however an ARG is supplied, or `prolog-hungry-delete-key-flag' is
nil, or point is inside a literal then the function in the variable
`backward-delete-char' is called."
  (interactive "P")
  (if (or (not prolog-hungry-delete-key-flag)
          arg
          (prolog-in-string-or-comment))
      (funcall 'backward-delete-char (prefix-numeric-value arg))
    (let ((here (point)))
      (skip-chars-backward " \t\n")
      (if (/= (point) here)
          (delete-region (point) here)
        (funcall 'backward-delete-char 1)
        ))))

;; For XEmacs compatibility (suggested by Per Mildner)
(put 'prolog-electric-delete 'pending-delete 'supersede)

(defun prolog-electric-if-then-else (arg)
  "If `prolog-electric-if-then-else-flag' is non-nil, indent if-then-else constructs.
Bound to the >, ; and ( keys."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if prolog-electric-if-then-else-flag (prolog-insert-spaces-after-paren)))

(defun prolog-electric-colon (arg)
  "If `prolog-electric-colon-flag' is non-nil, insert the electric `:' construct.
That is, insert space (if appropriate), `:-' and newline if colon is pressed
at the end of a line that starts in the first column (i.e., clause
heads)."
  (interactive "P")
  (if (and prolog-electric-colon-flag
	   (null arg)
	   (eolp)
	   ;(not (string-match "^\\s " (thing-at-point 'line))))
           (not (string-match "^\\(\\s \\|%\\)" (thing-at-point 'line))))
      (progn
        (unless (save-excursion (backward-char 1) (looking-at "\\s "))
          (insert " "))
	(insert ":-\n")
	(prolog-indent-line))
    (self-insert-command (prefix-numeric-value arg))))

(defun prolog-electric-dash (arg)
  "If `prolog-electric-dash-flag' is non-nil, insert the electric `-' construct.
that is, insert space (if appropriate), `-->' and newline if dash is pressed
at the end of a line that starts in the first column (i.e., DCG
heads)."
  (interactive "P")
  (if (and prolog-electric-dash-flag
	   (null arg)
	   (eolp)
	   ;(not (string-match "^\\s " (thing-at-point 'line))))
           (not (string-match "^\\(\\s \\|%\\)" (thing-at-point 'line))))
      (progn
        (unless (save-excursion (backward-char 1) (looking-at "\\s "))
          (insert " "))
	(insert "-->\n")
	(prolog-indent-line))
    (self-insert-command (prefix-numeric-value arg))))

(defun prolog-electric-dot (arg)
  "Insert dot and newline or a head of a new clause.

If `prolog-electric-dot-flag' is nil, then simply insert dot.
Otherwise::
When invoked at the end of nonempty line, insert dot and newline.
When invoked at the end of an empty line, insert a recursive call to
the current predicate.
When invoked at the beginning of line, insert a head of a new clause
of the current predicate.

When called with prefix argument ARG, insert just dot."
  (interactive "P")
  ;; Check for situations when the electricity should not be active
  (if (or (not prolog-electric-dot-flag)
          arg
          (prolog-in-string-or-comment)
          ;; Do not be electric in a floating point number or an operator
          (not
           (or
            ;; (re-search-backward
            ;; ######
            ;; "\\(^\\|[])}a-zA-Z_!'0-9]+\\)[ \t]*\\=" nil t)))
            (save-excursion
              (re-search-backward
               ;; "\\(^\\|[])}_!'0-9]+\\)[ \t]*\\=" nil t)))
               "\\(^\\|[])}_!'0-9]+\\)[ \t]*\\="
               nil t))
            (save-excursion
              (re-search-backward
               ;; "\\(^\\|[])}a-zA-Z]+\\)[ \t]*\\=" nil t)))
               (format "\\(^\\|[])}%s]+\\)[ \t]*\\="
                       prolog-lower-case-string) ;FIXME: [:lower:]
               nil t))
              (save-excursion
              (re-search-backward
               ;; "\\(^\\|[])}a-zA-Z]+\\)[ \t]*\\=" nil t)))
               (format "\\(^\\|[])}%s]+\\)[ \t]*\\="
                       prolog-upper-case-string) ;FIXME: [:upper:]
               nil t))
             )
            )
          ;; Do not be electric if inside a parenthesis pair.
          (not (= (prolog-region-paren-balance (prolog-clause-start) (point))
                  0))
          )
      (funcall 'self-insert-command (prefix-numeric-value arg))
    (cond
     ;; Beginning of line
     ((bolp)
      (prolog-insert-predicate-template))
     ;; At an empty line with at least one whitespace
     ((save-excursion
        (beginning-of-line)
        (looking-at "[ \t]+$"))
      (prolog-insert-predicate-template)
      (when prolog-electric-dot-full-predicate-template
 	(save-excursion
 	  (end-of-line)
 	  (insert ".\n"))))
     ;; Default
     (t
      (insert ".\n"))
     )))

(defun prolog-electric-underscore ()
  "Replace variable with an underscore.
If `prolog-electric-underscore-flag' is non-nil and the point is
on a variable then replace the variable with underscore and skip
the following comma and whitespace, if any.
If the point is not on a variable then insert underscore."
  (interactive)
  (if prolog-electric-underscore-flag
      (let (;start
            (case-fold-search nil)
            (oldp (point)))
        ;; ######
        ;;(skip-chars-backward "a-zA-Z_")
        (skip-chars-backward
         (format "%s%s_"
                 ;; FIXME: Why not "a-zA-Z"?
                 prolog-lower-case-string
                 prolog-upper-case-string))

        ;(setq start (point))
        (if (and (not (prolog-in-string-or-comment))
                 ;; ######
                 ;; (looking-at "\\<[_A-Z][a-zA-Z_0-9]*\\>"))
                 (looking-at (format "\\<[_%s][%s%s_0-9]*\\>"
                                     ;; FIXME: Use [:upper:] and friends.
                                     prolog-upper-case-string
                                     prolog-lower-case-string
                                     prolog-upper-case-string)))
            (progn
              (replace-match "_")
              (skip-chars-forward ", \t\n"))
          (goto-char oldp)
          (self-insert-command 1))
        )
    (self-insert-command 1))
  )


(defun prolog-find-term (functor arity &optional prefix)
  "Go to the position at the start of the next occurrence of a term.
The term is specified with FUNCTOR and ARITY.  The optional argument
PREFIX is the prefix of the search regexp."
  (let* (;; If prefix is not set then use the default "\\<"
         (prefix (if (not prefix)
                     "\\<"
                   prefix))
         (regexp (concat prefix functor))
         (i 1))

    ;; Build regexp for the search if the arity is > 0
    (if (= arity 0)
        ;; Add that the functor must be at the end of a word. This
        ;; does not work if the arity is > 0 since the closing )
        ;; is not a word constituent.
        (setq regexp (concat regexp "\\>"))
      ;; Arity is > 0, add parens and commas
      (setq regexp (concat regexp "("))
      (while (< i arity)
        (setq regexp (concat regexp ".+,"))
        (setq i (1+ i)))
      (setq regexp (concat regexp ".+)")))

    ;; Search, and return position
    (if (re-search-forward regexp nil t)
        (goto-char (match-beginning 0))
      (error "Term not found"))
    ))

(defun prolog-variables-to-anonymous (beg end)
  "Replace all variables within a region BEG to END by anonymous variables."
  (interactive "r")
  (save-excursion
    (let ((case-fold-search nil))
      (goto-char end)
      (while (re-search-backward "\\<[A-Z_][a-zA-Z_0-9]*\\>" beg t)
        (progn
          (replace-match "_")
          (backward-char)))
      )))


(defun prolog-set-atom-regexps ()
  "Set the `prolog-atom-char-regexp' and `prolog-atom-regexp' variables.
Must be called after `prolog-build-case-strings'."
  (setq prolog-atom-char-regexp
        (format "[%s%s0-9_$]"
                ;; FIXME: why not a-zA-Z?
                prolog-lower-case-string
                prolog-upper-case-string))
  (setq prolog-atom-regexp
        (format "[%s$]%s*"
                prolog-lower-case-string
                prolog-atom-char-regexp))
  )

(defun prolog-build-case-strings ()
  "Set `prolog-upper-case-string' and `prolog-lower-case-string'.
Uses the current case-table for extracting the relevant information."
  (let ((up_string "")
        (low_string ""))
    ;; Use `map-char-table' if it is defined. Otherwise enumerate all
    ;; numbers between 0 and 255. `map-char-table' is probably safer.
    ;;
    ;; `map-char-table' causes problems under Emacs 23.0.0.1, the
    ;; while loop seems to do its job well (Ryszard Szopa)
    ;;
    ;;(if (and (not (featurep 'xemacs))
    ;;          (fboundp 'map-char-table))
    ;;    (map-char-table
    ;;     (lambda (key value)
    ;;       (cond
    ;;        ((and
    ;;          (eq (prolog-int-to-char key) (downcase key))
    ;;          (eq (prolog-int-to-char key) (upcase key)))
    ;;         ;; Do nothing if upper and lower case are the same
    ;;         )
    ;;        ((eq (prolog-int-to-char key) (downcase key))
    ;;         ;; The char is lower case
    ;;         (setq low_string (format "%s%c" low_string key)))
    ;;        ((eq (prolog-int-to-char key) (upcase key))
    ;;         ;; The char is upper case
    ;;         (setq up_string (format "%s%c" up_string key)))
    ;;        ))
    ;;     (current-case-table))
      ;; `map-char-table' was undefined.
      (let ((key 0))
        (while (< key 256)
          (cond
           ((and
             (eq (prolog-int-to-char key) (downcase key))
             (eq (prolog-int-to-char key) (upcase key)))
            ;; Do nothing if upper and lower case are the same
            )
           ((eq (prolog-int-to-char key) (downcase key))
            ;; The char is lower case
            (setq low_string (format "%s%c" low_string key)))
           ((eq (prolog-int-to-char key) (upcase key))
            ;; The char is upper case
            (setq up_string (format "%s%c" up_string key)))
           )
          (setq key (1+ key))))
      ;; )
      ;; The strings are single-byte strings
      (setq prolog-upper-case-string (prolog-dash-letters up_string))
      (setq prolog-lower-case-string (prolog-dash-letters low_string))
      ))

;(defun prolog-regexp-dash-continuous-chars (chars)
;  (let ((ints (mapcar #'prolog-char-to-int (string-to-list chars)))
;        (beg 0)
;        (end 0))
;    (if (null ints)
;        chars
;      (while (and (< (+ beg 1) (length chars))
;                  (not (or (= (+ (nth beg ints) 1) (nth (+ beg 1) ints))
;                           (= (nth beg ints) (nth (+ beg 1) ints)))))
;        (setq beg (+ beg 1)))
;      (setq beg (+ beg 1)
;            end beg)
;      (while (and (< (+ end 1) (length chars))
;                  (or (= (+ (nth end ints) 1) (nth (+ end 1) ints))
;                      (= (nth end ints) (nth (+ end 1) ints))))
;        (setq end (+ end 1)))
;      (if (equal (substring chars end) "")
;          (substring chars 0 beg)
;        (concat (substring chars 0 beg) "-"
;                (prolog-regexp-dash-continuous-chars (substring chars end))))
;    )))

(defun prolog-ints-intervals (ints)
  "Return a list of intervals (from . to) covering INTS."
  (when ints
    (setq ints (sort ints '<))
    (let ((prev (car ints))
	  (interval-start (car ints))
	  intervals)
      (while ints
	(let ((next (car ints)))
	  (when (> next (1+ prev))	; start of new interval
	      (setq intervals (cons (cons interval-start prev) intervals))
	      (setq interval-start next))
	  (setq prev next)
	  (setq ints (cdr ints))))
      (setq intervals (cons (cons interval-start prev) intervals))
      (reverse intervals))))

(defun prolog-dash-letters (string)
  "Return a condensed regexp covering all letters in STRING."
  (let ((intervals (prolog-ints-intervals (mapcar #'prolog-char-to-int
						 (string-to-list string))))
	codes)
    (while intervals
      (let* ((i (car intervals))
	     (from (car i))
	     (to (cdr i))
	     (c (cond ((= from to) `(,from))
		      ((= (1+ from) to) `(,from ,to))
		      (t `(,from ?- ,to)))))
	(setq codes (cons c codes)))
      (setq intervals (cdr intervals)))
    (apply 'concat (reverse codes))))

;(defun prolog-condense-character-sets (regexp)
;  "Condense adjacent characters in character sets of REGEXP."
;  (let ((next -1))
;    (while (setq next (string-match "\\[\\(.*?\\)\\]" regexp (1+ next)))
;      (setq regexp (replace-match (prolog-dash-letters (match-string 1 regexp))
;				  t t regexp 1))))
;  regexp)

;; GNU Emacs compatibility: GNU Emacs does not differentiate between
;; ints and chars, or at least these two are interchangeable.
(defalias 'prolog-int-to-char
  (if (fboundp 'int-to-char) #'int-to-char #'identity))

(defalias 'prolog-char-to-int
  (if (fboundp 'char-to-int) #'char-to-int #'identity))

;;-------------------------------------------------------------------
;; Menu stuff (both for the editing buffer and for the inferior
;; prolog buffer)
;;-------------------------------------------------------------------

(unless (fboundp 'region-exists-p)
  (defun region-exists-p ()
    "Non-nil iff the mark is set.  Lobotomized version for Emacsen that do not provide their own."
    (mark)))


;; GNU Emacs ignores `easy-menu-add' so the order in which the menus
;; are defined _is_ important!

(easy-menu-define
  prolog-menu-help (list prolog-mode-map prolog-inferior-mode-map)
  "Help menu for the Prolog mode."
  ;; FIXME: Does it really deserve a whole menu to itself?
  `(,(if (featurep 'xemacs) "Help"
       ;; Not sure it's worth the trouble.  --Stef
       ;; (add-to-list 'menu-bar-final-items
       ;;         (easy-menu-intern "Prolog-Help"))
       "Prolog-help")
    ["On predicate" prolog-help-on-predicate prolog-help-function-i]
    ["Apropos" prolog-help-apropos (eq prolog-system 'swi)]
    "---"
    ["Describe mode" describe-mode t]))

(easy-menu-define
  prolog-edit-menu-runtime prolog-mode-map
  "Runtime Prolog commands available from the editing buffer"
  ;; FIXME: Don't use a whole menu for just "Run Mercury".  --Stef
  `("System"
    ;; Runtime menu name.
    ,@(unless (featurep 'xemacs)
        '(:label (cond ((eq prolog-system 'eclipse) "ECLiPSe")
                       ((eq prolog-system 'mercury) "Mercury")
                       (t "System"))))

    ;; Consult items, NIL for mercury.
    ["Consult file" prolog-consult-file
     :included (not (eq prolog-system 'mercury))]
    ["Consult buffer" prolog-consult-buffer
     :included (not (eq prolog-system 'mercury))]
    ["Consult region" prolog-consult-region :active (region-exists-p)
     :included (not (eq prolog-system 'mercury))]
    ["Consult predicate" prolog-consult-predicate
     :included (not (eq prolog-system 'mercury))]

    ;; Compile items, NIL for everything but SICSTUS.
    ,(if (featurep 'xemacs) "---"
       ["---" nil :included (eq prolog-system 'sicstus)])
    ["Compile file" prolog-compile-file
     :included (eq prolog-system 'sicstus)]
    ["Compile buffer" prolog-compile-buffer
     :included (eq prolog-system 'sicstus)]
    ["Compile region" prolog-compile-region :active (region-exists-p)
     :included (eq prolog-system 'sicstus)]
    ["Compile predicate" prolog-compile-predicate
     :included (eq prolog-system 'sicstus)]

    ;; Debug items, NIL for Mercury.
    ,(if (featurep 'xemacs) "---"
       ["---" nil :included (not (eq prolog-system 'mercury))])
    ;; FIXME: Could we use toggle or radio buttons?  --Stef
    ["Debug" prolog-debug-on :included (not (eq prolog-system 'mercury))]
    ["Debug off" prolog-debug-off
     ;; In SICStus, these are pairwise disjunctive,
     ;; so it's enough with a single "off"-command
     :included (not (memq prolog-system '(mercury sicstus)))]
    ["Trace" prolog-trace-on :included (not (eq prolog-system 'mercury))]
    ["Trace off" prolog-trace-off
     :included (not (memq prolog-system '(mercury sicstus)))]
    ["Zip" prolog-zip-on :included (and (eq prolog-system 'sicstus)
                                        (prolog-atleast-version '(3 . 7)))]
    ["All debug off" prolog-debug-off
     :included (eq prolog-system 'sicstus)]
    ["Source level debugging"
     prolog-toggle-sicstus-sd
     :included (and (eq prolog-system 'sicstus)
                    (prolog-atleast-version '(3 . 7)))
     :style toggle
     :selected prolog-use-sicstus-sd]

    "---"
    ["Run" run-prolog
     :suffix (cond ((eq prolog-system 'eclipse) "ECLiPSe")
                   ((eq prolog-system 'mercury) "Mercury")
                   (t "Prolog"))]))

(easy-menu-define
  prolog-edit-menu-insert-move prolog-mode-map
  "Commands for Prolog code manipulation."
  '("Prolog"
    ["Comment region" comment-region (region-exists-p)]
    ["Uncomment region" prolog-uncomment-region (region-exists-p)]
    ["Add comment/move to comment" indent-for-comment t]
    ["Convert variables in region to '_'" prolog-variables-to-anonymous
     :active (region-exists-p) :included (not (eq prolog-system 'mercury))]
    "---"
    ["Insert predicate template" prolog-insert-predicate-template t]
    ["Insert next clause head" prolog-insert-next-clause t]
    ["Insert predicate spec" prolog-insert-predspec t]
    ["Insert module modeline" prolog-insert-module-modeline t]
    "---"
    ["Beginning of clause" prolog-beginning-of-clause t]
    ["End of clause" prolog-end-of-clause t]
    ["Beginning of predicate" prolog-beginning-of-predicate t]
    ["End of predicate" prolog-end-of-predicate t]
    "---"
    ["Indent line" prolog-indent-line t]
    ["Indent region" indent-region (region-exists-p)]
    ["Indent predicate" prolog-indent-predicate t]
    ["Indent buffer" prolog-indent-buffer t]
    ["Align region" align (region-exists-p)]
    "---"
    ["Mark clause" prolog-mark-clause t]
    ["Mark predicate" prolog-mark-predicate t]
    ["Mark paragraph" mark-paragraph t]
    ;;"---"
    ;;["Fontify buffer" font-lock-fontify-buffer t]
    ))

(defun prolog-menu ()
  "Add the menus for the Prolog editing buffers."

  (easy-menu-add prolog-edit-menu-insert-move)
  (easy-menu-add prolog-edit-menu-runtime)

  ;; Add predicate index menu
  (set (make-local-variable 'imenu-create-index-function)
       'imenu-default-create-index-function)
  ;;Milan (this has problems with object methods...)  ###### Does it? (Stefan)
  (setq imenu-prev-index-position-function 'prolog-beginning-of-predicate)
  (setq imenu-extract-index-name-function 'prolog-get-predspec)

  (if (and prolog-imenu-flag
           (< (count-lines (point-min) (point-max)) prolog-imenu-max-lines))
      (imenu-add-to-menubar "Predicates"))

  (easy-menu-add prolog-menu-help))

(easy-menu-define
  prolog-inferior-menu-all prolog-inferior-mode-map
  "Menu for the inferior Prolog buffer."
  `("Prolog"
    ;; Runtime menu name.
    ,@(unless (featurep 'xemacs)
        '(:label (cond ((eq prolog-system 'eclipse) "ECLiPSe")
                       ((eq prolog-system 'mercury) "Mercury")
                       (t "Prolog"))))

    ;; Debug items, NIL for Mercury.
    ,(if (featurep 'xemacs) "---"
       ["---" nil :included (not (eq prolog-system 'mercury))])
    ;; FIXME: Could we use toggle or radio buttons?  --Stef
    ["Debug" prolog-debug-on :included (not (eq prolog-system 'mercury))]
    ["Debug off" prolog-debug-off
     ;; In SICStus, these are pairwise disjunctive,
     ;; so it's enough with a single "off"-command
     :included (not (memq prolog-system '(mercury sicstus)))]
    ["Trace" prolog-trace-on :included (not (eq prolog-system 'mercury))]
    ["Trace off" prolog-trace-off
     :included (not (memq prolog-system '(mercury sicstus)))]
    ["Zip" prolog-zip-on :included (and (eq prolog-system 'sicstus)
                                        (prolog-atleast-version '(3 . 7)))]
    ["All debug off" prolog-debug-off
     :included (eq prolog-system 'sicstus)]
    ["Source level debugging"
     prolog-toggle-sicstus-sd
     :included (and (eq prolog-system 'sicstus)
                    (prolog-atleast-version '(3 . 7)))
     :style toggle
     :selected prolog-use-sicstus-sd]

    ;; Runtime.
    "---"
    ["Interrupt Prolog" comint-interrupt-subjob t]
    ["Quit Prolog" comint-quit-subjob t]
    ["Kill Prolog" comint-kill-subjob t]))


(defun prolog-inferior-menu ()
  "Create the menus for the Prolog inferior buffer.
This menu is dynamically created because one may change systems during
the life of an Emacs session."
  (easy-menu-add prolog-inferior-menu-all)
  (easy-menu-add prolog-menu-help))

(defun prolog-mode-version ()
  "Echo the current version of Prolog mode in the minibuffer."
  (interactive)
  (message "Using Prolog mode version %s" prolog-mode-version))

(provide 'prolog)

;;; prolog.el ends here
