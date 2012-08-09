;;; reftex-vars.el --- configuration variables for RefTeX

;; Copyright (C) 1997-1999, 2001-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@science.uva.nl>
;; Maintainer: auctex-devel@gnu.org
;; Version: 4.31
;; Package: reftex

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
(defvar reftex-tables-dirty)
(eval-when-compile (require 'cl))
(eval-and-compile
  (defun reftex-set-dirty (symbol value)
    (setq reftex-tables-dirty t)
    (set symbol value)))

;; Define the two constants which are needed during compilation

(eval-and-compile
(defconst reftex-label-alist-builtin
  '(
    ;; Some aliases, mostly for backward compatibility
    (Sideways    "Alias for -->rotating" (rotating))
    (AMSTeX      "amsmath with eqref macro"
                 ((nil ?e nil "~\\eqref{%s}")
                  amsmath))

    ;; Individual package defaults
    (amsmath "AMS-LaTeX math environments"
     (("align"        ?e nil nil eqnarray-like)
      ("gather"       ?e nil nil eqnarray-like)
      ("multline"     ?e nil nil t)
      ("flalign"      ?e nil nil eqnarray-like)
      ("alignat"      ?e nil nil alignat-like)
      ("xalignat"     ?e nil nil alignat-like)
      ("xxalignat"    ?e nil nil alignat-like)
      ("subequations" ?e nil nil t)))

    (endnotes "The \\endnote macro"
     (("\\endnote[]{}" ?N "en:" "~\\ref{%s}" 2
       (regexp "endnotes?" "notes?" "Anmerkung\\(en\\)?" "Anm\\."))))

    (fancybox "The Beqnarray environment"
     (("Beqnarray" ?e nil nil eqnarray-like)))

    (floatfig "The floatingfigure environment"
     (("floatingfigure" ?f nil nil caption)))

    (longtable   "The longtable environment"
     (("longtable"  ?t nil nil caption)))

    (picinpar    "The figwindow and tabwindow environments"
     (("figwindow" ?f nil nil 1)
      ("tabwindow" ?f nil nil 1)))

    (rotating    "Sidewaysfigure and table"
     (("sidewaysfigure" ?f nil nil caption)
      ("sidewaystable"  ?t nil nil caption)))

    (sidecap      "CSfigure and SCtable"
     (("SCfigure"       ?f nil nil caption)
      ("SCtable"        ?t nil nil caption)))

    (subfigure   "Subfigure environments/macro"
     (("subfigure"   ?f nil nil caption)
      ("subfigure*"  ?f nil nil caption)
      ("\\subfigure[]{}" ?f nil nil 1)))

    (supertab    "Supertabular environment"
     (("supertabular" ?t nil nil "\\tablecaption{")))

    (wrapfig     "The wrapfigure environment"
     (("wrapfigure" ?f nil nil caption)))

    ;; The LaTeX core stuff
    (LaTeX       "LaTeX default environments"
     (("section"   ?s "%S" "~\\ref{%s}" (nil . t)
       (regexp "parts?" "chapters?" "chap\\." "sections?" "sect?\\."
               "paragraphs?" "par\\."
               "\\\\S" "\247" "Teile?" "Kapitel" "Kap\\." "Abschnitte?"
               "appendi\\(x\\|ces\\)" "App\\."  "Anh\"?ange?" "Anh\\."))

      ("enumerate" ?i "item:" "~\\ref{%s}" item
       (regexp "items?" "Punkte?"))

      ("equation"  ?e "eq:" "~(\\ref{%s})" t
       (regexp "equations?" "eqs?\\." "eqn\\." "Gleichung\\(en\\)?"  "Gl\\."))
      ("eqnarray"  ?e "eq:" nil eqnarray-like)

      ("figure"    ?f "fig:" "~\\ref{%s}" caption
       (regexp "figure?[sn]?" "figs?\\." "Abbildung\\(en\\)?" "Abb\\."))
      ("figure*"   ?f nil nil caption)

      ("table"     ?t "tab:" "~\\ref{%s}" caption
       (regexp "tables?" "tab\\." "Tabellen?"))
      ("table*"    ?t nil nil caption)

      ("\\footnote[]{}" ?n "fn:" "~\\ref{%s}" 2
       (regexp "footnotes?" "Fussnoten?"))

      ("any"       ?\  " "   "~\\ref{%s}" nil)

      ;; The label macro is hard coded, but it *could* be defined like this:
      ;;("\\label{*}" nil nil nil nil)
      ))

    )
  "The default label environment descriptions.
Lower-case symbols correspond to a style file of the same name in the LaTeX
distribution.  Mixed-case symbols are convenience aliases.")

(defconst reftex-cite-format-builtin
  '((default "Default macro \\cite{%l}"
      "\\cite[]{%l}")
    (natbib "The Natbib package"
     ((?\C-m . "\\cite[][]{%l}")
      (?t    . "\\citet[][]{%l}")
      (?T    . "\\citet*[][]{%l}")
      (?p    . "\\citep[][]{%l}")
      (?P    . "\\citep*[][]{%l}")
      (?e    . "\\citep[e.g.][]{%l}")
      (?s    . "\\citep[see][]{%l}")
      (?a    . "\\citeauthor{%l}")
      (?A    . "\\citeauthor*{%l}")
      (?y    . "\\citeyear{%l}")
      (?n    . "\\nocite{%l}")))
    (amsrefs "The AMSRefs package"
     ((?\C-m . "\\cite{%l}")
      (?p    . "\\cite{%l}")
      (?P    . "\\cites{%l}")
      (?t    . "\\ocite{%l}")
      (?T    . "\\ocites{%l}")
      (?y    . "\\ycite{%l}")
      (?Y    . "\\ycites{%l}")
      (?a    . "\\citeauthor{%l}")
      (?A    . "\\citeauthory{%l}")
      (?f    . "\\fullcite{%l}")
      (?F    . "\\fullocite{%l}")
      (?n    . "\\nocite{%l}")))
    (jurabib "The Jurabib package"
     ((?\C-m . "\\cite{%l}")
      (?c    . "\\cite[][]{%l}")
      (?t    . "\\citet{%l}")
      (?p    . "\\citep{%l}")
      (?e    . "\\citep[e.g.][]{%l}")
      (?s    . "\\citep[see][]{%l}")
      (?u    . "\\fullcite{%l}")
      (?i    . "\\citetitle{%l}")
      (?a    . "\\citeauthor{%l}")
      (?e    . "\\citefield{}{%l}")
      (?y    . "\\citeyear{%l}")
      (?f    . "\\footcite{%l}")
      (?F    . "\\footcite[][]{%l}")
      (?l    . "\\footfullcite{%l}")))
    (bibentry "The Bibentry package"
      "\\bibentry{%l}")
    (harvard "The Harvard package"
     ((?\C-m . "\\cite[]{%l}")
      (?p    . "\\cite[]{%l}")
      (?t    . "\\citeasnoun{%l}")
      (?n    . "\\citeasnoun{%l}")
      (?s    . "\\possessivecite{%l}")
      (?e    . "\\citeaffixed{%l}{?}")
      (?y    . "\\citeyear{%l}")
      (?a    . "\\citename{%l}")))
    (chicago "The Chicago package"
     ((?\C-m . "\\cite[]{%l}")
      (?t    . "\\citeN[]{%l}")
      (?T    . "\\shortciteN{%l}")
      (?p    . "\\cite[]{%l}")
      (?P    . "\\shortcite{%l}")
      (?a    . "\\citeA{%l}")
      (?A    . "\\shortciteA{%l}")
      (?y    . "\\citeyear{%l}")))
    (astron "The Astron package"
     ((?\C-m . "\\cite[]{%l}")
      (?p    . "\\cite[]{%l}" )
      (?t    . "%2a (\\cite{%l})")))
    (author-year "Do-it-yourself Author-year"
     ((?\C-m . "\\cite{%l}")
      (?t    . "%2a (%y)\\nocite{%l}")
      (?p    . "(%2a %y\\nocite{%l})")))
    (locally     "Full info in parenthesis"
     "(%2a %y, %j %v, %P, %e: %b, %u, %s %<)")
    )
  "Builtin versions of the citation format.
The following conventions are valid for all alist entries:
`?\C-m' should always point to a straight \\cite{%l} macro.
`?t'    should point to a textual citation (citation as a noun).
`?p'    should point to a parenthetical citation.")

(defconst reftex-index-macros-builtin
  '((default "Default \\index and \\glossary macros"
      (("\\index{*}" "idx" ?i "" nil t)
       ("\\glossary{*}" "glo" ?g "" nil t)))
    (multind "The multind.sty package"
       (("\\index{}{*}" 1 ?i "" nil t)))
    (index "The index.sty package"
           (("\\index[]{*}" 1 ?i "" nil t)
            ("\\index*[]{*}" 1 ?I "" nil nil)))
    (Index-Shortcut "index.sty with \\shortindexingon"
       (("\\index[]{*}" 1 ?i "" nil t)
        ("\\index*[]{*}" 1 ?I "" nil nil)
        ("^[]{*}" 1 ?^ "" texmathp t)
        ("_[]{*}" 1 ?_ "" texmathp nil))))
  "Builtin stuff for `reftex-index-macros'.
Lower-case symbols correspond to a style file of the same name in the LaTeX
distribution.  Mixed-case symbols are convenience aliases.")
)

;; Configuration Variables and User Options for RefTeX ------------------

(defgroup reftex nil
  "LaTeX label and citation support."
  :tag "RefTeX"
  :link '(url-link :tag "Home Page"
                   "http://staff.science.uva.nl/~dominik/Tools/reftex/")
  :link '(emacs-commentary-link :tag "Commentary in reftex.el" "reftex.el")
  :link '(custom-manual "(reftex)Top")
  :prefix "reftex-"
  :group 'tex)


;; Table of contents configuration --------------------------------------

(defgroup reftex-table-of-contents-browser nil
  "A multifile table of contents browser."
  :group 'reftex)

(defcustom reftex-include-file-commands '("include" "input")
  "LaTeX commands which input another file.
The file name is expected after the command, either in braces or separated
by whitespace."
  :group 'reftex-table-of-contents-browser
  :type '(repeat string))

(defcustom reftex-max-section-depth 12
  "Maximum depth of section levels in document structure.
Standard LaTeX needs default is 7, but there are packages for which this
needs to be larger."
  :group 'reftex-table-of-contents-browser
  :type 'integer)

;; LaTeX section commands and level numbers
(defcustom reftex-section-levels
  '(
    ("part"            .  0)
    ("chapter"         .  1)
    ("section"         .  2)
    ("subsection"      .  3)
    ("subsubsection"   .  4)
    ("paragraph"       .  5)
    ("subparagraph"    .  6)
    ("addchap"         . -1) ; KOMA-Script
    ("addsec"          . -2) ; KOMA-Script
;;; ("minisec"         . -7) ; KOMA-Script
    )
  "Commands and levels used for defining sections in the document.
This is an alist with each element like (COMMAND-NAME . LEVEL).
The car of each cons cell is the name of the section macro (without
the backslash).  The cdr is a number indicating its level.  A negative
level means the same level as the positive value, but the section will
never get a number.  The cdr may also be a function which will be called
to after the section-re matched to determine the level.
This list is also used for promotion and demotion of sectioning commands.
If you are using a document class which has several sets of sectioning
commands, promotion only works correctly if this list is sorted first
by set, then within each set by level.  The promotion commands always
select the nearest entry with the correct new level."
  :group 'reftex-table-of-contents-browser
  :set 'reftex-set-dirty
  :type '(repeat
          (cons (string :tag "sectioning macro" "")
                (choice
                 (number :tag "level           " 0)
                 (symbol :tag "function        " my-level-func)))))

(defcustom reftex-toc-max-level 100
  "*The maximum level of toc entries which will be included in the TOC.
Section headings with a bigger level will be ignored.  In RefTeX, chapters
are level 1, sections are level 2 etc.
This variable can be changed from within the *toc* buffer with the `t' key."
  :group 'reftex-table-of-contents-browser
  :type 'integer)

(defcustom reftex-part-resets-chapter nil
  "*Non-nil means, \\part is like any other sectioning command.
This means, part numbers will be included in the numbering of chapters, and
chapter counters will be reset for each part.
When nil (the default), parts are special, do not reset the chapter counter
and also do not show up in chapter numbers."
  :group 'reftex-table-of-contents-browser
  :type 'boolean)


(defcustom reftex-auto-recenter-toc 'frame
  "*Non-nil means, turn automatic recentering of *TOC* window on.
When active, the *TOC* window will always show the section you
are currently working in.  Recentering happens whenever Emacs is idle for
more than `reftex-idle-time' seconds.

Value t means, turn on immediately when RefTeX gets started.  Then,
recentering will work for any TOC window created during the session.

Value 'frame (the default) means, turn automatic recentering on only while the
dedicated TOC frame does exist, and do the recentering only in that frame.  So
when creating that frame (with \"d\" key in an ordinary TOC window), the
automatic recentering is turned on.  When the frame gets destroyed, automatic
recentering is turned off again.

This feature can be turned on and off from the menu
\(Ref->Options)."
  :group 'reftex-table-of-contents-browser
  :type '(choice
          (const :tag "never" nil)
          (const :tag "always" t)
          (const :tag "in dedicated frame only" frame)))

(defcustom reftex-toc-split-windows-horizontally nil
  "*Non-nil means, create TOC window by splitting window horizontally."
  :group 'reftex-table-of-contents-browser
  :type 'boolean)

(defcustom reftex-toc-split-windows-fraction .3
  "*Fraction of the width or height of the frame to be used for TOC window.
See also `reftex-toc-split-windows-horizontally'."
  :group 'reftex-table-of-contents-browser
  :type 'number)

(defvar reftex-toc-split-windows-horizontally-fraction 0.5
  "This variable is obsolete, use `reftex-toc-split-windows-fraction' instead.")

(defcustom reftex-toc-keep-other-windows t
  "*Non-nil means, split the selected window to display the *toc* buffer.
This helps to keep the window configuration, but makes the *toc* small.
When nil, all other windows except the selected one will be deleted, so
that the *toc* window fills half the frame."
  :group 'reftex-table-of-contents-browser
  :type 'boolean)

(defcustom reftex-toc-include-file-boundaries nil
  "*Non-nil means, include file boundaries in *toc* buffer.
This flag can be toggled from within the *toc* buffer with the `F' key."
  :group 'reftex-table-of-contents-browser
  :type 'boolean)

(defcustom reftex-toc-include-labels nil
  "*Non-nil means, include labels in *toc* buffer.
This flag can be toggled from within the *toc* buffer with the `l' key."
  :group 'reftex-table-of-contents-browser
  :type 'boolean)

(defcustom reftex-toc-include-index-entries nil
  "*Non-nil means, include index entries in *toc* buffer.
This flag can be toggled from within the *toc* buffer with the `i' key."
  :group 'reftex-table-of-contents-browser
  :type 'boolean)

(defcustom reftex-toc-confirm-promotion 2
  "*Non-nil means, promotion/demotion commands first prompt for confirmation.
When nil, the command is executed immediately.  When this is an integer
N, ask for confirmation only if N or more section commands are going to be
changed."
  :group 'reftex-table-of-contents-browser
  :type '(choice
          (const  :tag "Never" nil)
          (const  :tag "Always" t)
          (number :tag "When more than N sections" :value 2)))

(defcustom reftex-toc-include-context nil
  "*Non-nil means, include context with labels in the *toc* buffer.
Context will only be shown when labels are visible as well.
This flag can be toggled from within the *toc* buffer with the `c' key."
  :group 'reftex-table-of-contents-browser
  :type 'boolean)

(defcustom reftex-toc-follow-mode nil
  "*Non-nil means, point in *toc* buffer will cause other window to follow.
The other window will show the corresponding part of the document.
This flag can be toggled from within the *toc* buffer with the `f' key."
  :group 'reftex-table-of-contents-browser
  :type 'boolean)

(defcustom reftex-revisit-to-follow nil
  "*Non-nil means, follow-mode will revisit files if necessary.
When nil, follow-mode will be suspended for stuff in unvisited files."
  :group 'reftex-table-of-contents-browser
  :group 'reftex-referencing-labels
  :type 'boolean)

(defcustom reftex-toc-mode-hook nil
  "Mode hook for `reftex-toc-mode'."
  :group 'reftex-table-of-contents-browser
  :type 'hook)

;; Label Support Configuration

(defgroup reftex-label-support nil
  "Support for creation, insertion and referencing of labels in LaTeX."
  :group 'reftex)

(defgroup reftex-defining-label-environments nil
  "Definition of environments and macros to do with label."
  :group 'reftex-label-support)

(defcustom reftex-default-label-alist-entries
  '(amsmath endnotes fancybox floatfig longtable picinpar
            rotating sidecap subfigure supertab wrapfig LaTeX)
  "Default label alist specifications.  LaTeX should always be the last entry.
The value of this variable is a list of symbols with associations in the
constant `reftex-label-alist-builtin'.  Check that constant for a full list
of options."
  :group 'reftex-defining-label-environments
  :set   'reftex-set-dirty
  :type `(set
          :indent 4
          :inline t
          :greedy t
          ,@(mapcar
             (lambda (x)
               (list 'const :tag (concat (symbol-name (nth 0 x))
                                         ": " (nth 1 x))
                     (nth 0 x)))
             reftex-label-alist-builtin)))

(defcustom reftex-label-alist nil
  "Alist with information on environments for \\label-\\ref use.

This docstring is easier to understand after reading the configuration
examples in `reftex.el'.  Looking at the builtin defaults in the constant
`reftex-label-alist-builtin' may also be instructive.

Set this variable to define additions and changes to the default.  The only
things you MUST NOT change is that `?s' is the type indicator for section
labels, and SPC for the `any' label type.  These are hard-coded at other
places in the code.

The value of the variable must be a list of items.  Each item is a list
itself and has the following structure:

 (ENV-OR-MACRO TYPE-KEY LABEL-PREFIX REFERENCE-FORMAT CONTEXT-METHOD
           (MAGIC-WORD ... ) TOC-LEVEL)

Each list entry describes either an environment carrying a counter for use
with \\label and \\ref, or a LaTeX macro defining a label as (or inside)
one of its arguments.  The elements of each list entry are:

ENV-OR-MACRO
    Name of the environment (like \"table\") or macro (like \"\\\\myfig\").
    For macros, indicate the macro arguments for best results, as in
    \"\\\\myfig[]{}{}{*}{}\".  Use square brackets for optional arguments,
    a star to mark the label argument, if any.  The macro does not have to
    have a label argument - you could also use \\label{..} inside one of
    its arguments.
    Special names: `section' for section labels, `any' to define a group
    which contains all labels.

    This may also be a function to do local parsing and identify point
    to be in a non-standard label environment.  The function must take
    an argument BOUND and limit backward searches to this value.  It
    should return either nil or a cons cell (FUNCTION . POSITION) with
    the function symbol and the position where the special environment
    starts.  See the Info documentation for an example.

    Finally this may also be nil if the entry is only meant to change
    some settings associated with the type indicator character (see below).

TYPE-KEY
    Type indicator character, like `?t', must be a printable ASCII character.
    The type indicator is a single character which defines a label type.
    Any label inside the environment or macro is assumed to belong to this
    type.  The same character may occur several times in this list, to cover
    cases in which different environments carry the same label type (like
    `equation' and `eqnarray').
    If the type indicator is nil and the macro has a label argument {*},
    the macro defines neutral labels just like \\label.  In this case
    the reminder of this entry is ignored.

LABEL-PREFIX
    Label prefix string, like \"tab:\".
    The prefix is a short string used as the start of a label.  It may be the
    empty string.  The prefix may contain the following `%' escapes:
       %f   Current file name with directory and extension stripped.
       %F   Current file name relative to directory of master file.
       %m   Master file name, directory and extension stripped.
       %M   Directory name (without path) where master file is located.
       %u   User login name, on systems which support this.
       %S   A section prefix derived with variable `reftex-section-prefixes'.

    Example: In a file `intro.tex', \"eq:%f:\" will become \"eq:intro:\").

REFERENCE-FORMAT
    Format string for reference insert in buffer.  `%s' will be replaced by
    the label.
    When the format starts with `~', the `~' will only be inserted if
    there is not already a whitespace before point.

CONTEXT-METHOD
    Indication on how to find the short context.
    - If nil, use the text following the \\label{...} macro.
    - If t, use
       - the section heading for section labels.
       - text following the \\begin{...} statement of environments.
         (not a good choice for environments like eqnarray or enumerate,
         where one has several labels in a single environment).
       - text after the macro name (starting with the first arg) for macros.
    - If an integer, use the nth argument of the macro.  As a special case,
      1000 means to get text after the last macro argument.
    - If a string, use as regexp to search *backward* from the label.  Context
      is then the text following the end of the match.  E.g. putting this to
      \"\\\\\\\\caption[[{]\" will use the caption in a figure or table
      environment.
      \"\\\\\\\\begin{eqnarray}\\\\|\\\\\\\\\\\\\\\\\" works for eqnarrays.
    - If any of `caption', `item', `eqnarray-like', `alignat-like', this
      symbol will internally be translated into an appropriate regexp
      (see also the variable `reftex-default-context-regexps').
    - If a function, call this function with the name of the environment/macro
      as argument.  On call, point will be just after the \\label macro.  The
      function is expected to return a suitable context string.  It should
      throw an exception (error) when failing to find context.
      As an example, here is a function returning the 10 chars following
      the label macro as context:

        (defun my-context-function (env-or-mac)
          (if (> (point-max) (+ 10 (point)))
              (buffer-substring (point) (+ 10 (point)))
            (error \"Buffer too small\")))

    Label context is used in two ways by RefTeX: For display in the label
    menu, and to derive a label string.  If you want to use a different
    method for each of these, specify them as a dotted pair.
    E.g. `(nil . t)' uses the text after the label (nil) for display, and
    text from the default position (t) to derive a label string.  This is
    actually used for section labels.

MAGIC-WORDS
    List of magic words which identify a reference to be of this type.
    If the word before point is equal to one of these words when calling
    `reftex-reference', the label list offered will be automatically
    restricted to labels of the correct type.
    If the first element of this wordlist is the symbol `regexp', the
    strings are interpreted as regular expressions.  RefTeX will add
    a \"\\\\W\" to the beginning and other stuff to the end of the regexp.

TOC-LEVEL
    The integer level at which this environment should be added to the
    table of contents.  See also `reftex-section-levels'.  A positive
    value will number the entries mixed with the sectioning commands of
    the same level.  A negative value will make unnumbered entries.
    Useful only for theorem-like environments, will be ignored for macros.
    When omitted or nil, no TOC entries will be made.

If the type indicator characters of two or more entries are the same, RefTeX
will use
 - the first non-nil format and prefix
 - the magic words of all involved entries.

Any list entry may also be a symbol.  If that has an association in
`reftex-label-alist-builtin', the cddr of that association is spliced into the
list.  However, builtin defaults should normally be set with the variable
`reftex-default-label-alist-entries."
  :group 'reftex-defining-label-environments
  :set 'reftex-set-dirty
  :type
  `(repeat
    (choice :tag "Package or Detailed   "
     :value ("" ?a nil nil nil nil)
     (list :tag "Detailed Entry"
           :value ("" ?a nil nil nil nil)
           (choice    :tag "Environment or \\macro "
                      (const  :tag "Ignore, just use typekey" nil)
                      (string "")
                      (symbol :tag "Special parser" my-parser))
           (choice    :tag "Type specification    "
                      (const :tag "unspecified, like in \\label" nil)
                      (character :tag "Char  " ?a))
           (choice    :tag "Label prefix string   "
                      (const  :tag "Default" nil)
                      (string :tag "String" "lab:"))
           (choice    :tag "Label reference format"
                      (const  :tag "Default" nil)
                      (string :tag "String" "~\\ref{%s}"))
           (choice    :tag "Context method        "
                      (const  :tag "Default position" t)
                      (const  :tag "After label"      nil)
                      (number :tag "Macro arg nr" 1)
                      (regexp :tag "Regexp" "")
                      (const  :tag "Caption in float" caption)
                      (const  :tag "Item in list" item)
                      (const  :tag "Eqnarray-like" eqnarray-like)
                      (const  :tag "Alignat-like" alignat-like)
                      (symbol :tag "Function" my-func))
           (repeat :tag "Magic words" :extra-offset 2 (string))
           (option (choice :tag "Make TOC entry     "
                           (const :tag "No entry" nil)
                           (integer :tag "Level" :value -3))))
     (choice
      :tag "Package"
      :value AMSTeX
      ,@(mapcar
         (lambda (x)
           (list 'const :tag (concat (symbol-name (nth 0 x)))
                 (nth 0 x)))
         reftex-label-alist-builtin)))))

(defcustom reftex-section-prefixes '((0 . "part:") (1 . "cha:") (t . "sec:"))
  "Prefixes for section labels.
When the label prefix given in an entry in `reftex-label-alist' contains `%S',
this list is used to determine the correct prefix string depending on the
current section level.
The list is an alist, with each entry of the form (KEY . PREFIX)
Possible keys are sectioning macro names like `chapter', section levels
\(as given in `reftex-section-levels'), and t for the default."
  :group 'reftex-defining-label-environments
  :type '(repeat
          (cons :value (0 . "")
                (choice
                 (string :tag  "macro name")
                 (integer :tag "section level")
                 (const :tag "default" t))
                (string :tag "Prefix"))))

(defcustom reftex-default-context-regexps
  '((caption       . "\\\\\\(rot\\)?caption\\*?[[{]")
    (item          . "\\\\item\\(\\[[^]]*\\]\\)?")
    (eqnarray-like . "\\\\begin{%s}\\|\\\\\\\\")
    (alignat-like  . "\\\\begin{%s}{[0-9]*}\\|\\\\\\\\"))
"Alist with default regular expressions for finding context.
The form (format regexp (regexp-quote environment)) is used to calculate
the final regular expression - so %s will be replaced with the environment
or macro."
  :group 'reftex-defining-label-environments
  :type '(repeat (cons (symbol) (regexp))))

(defcustom reftex-trust-label-prefix nil
  "Non-nil means, trust the label prefix when determining label type.
It is customary to use special label prefixes to distinguish different label
types.  The label prefixes have no syntactic meaning in LaTeX (unless
special packages like fancyref are being used).  RefTeX can and by
default does parse around each label to detect the correct label type,
but this process can be slow when a document contains thousands of
labels.  If you use label prefixes consistently, you may speed up
document parsing by setting this variable to a non-nil value.  RefTeX
will then compare the label prefix with the prefixes found in
`reftex-label-alist' and derive the correct label type in this way.
Possible values for this option are:

t          This means to trust any label prefixes found.
regexp     If a regexp, only prefixes matched by the regexp are trusted.
list       List of accepted prefixes, as strings.  The colon is part of
           the prefix, e.g. (\"fn:\" \"eqn:\" \"item:\").
nil        Never trust a label prefix.

The only disadvantage of using this feature is that the label context
displayed in the label selection buffer along with each label is
simply some text after the label definition.  This is no problem if you
place labels keeping this in mind (e.g. *before* the equation, *at
the beginning* of a fig/tab caption ...).  Anyway, it is probably best
to use the regexp or the list value types to fine-tune this feature.
For example, if your document contains thousands of footnotes with
labels fn:xxx, you may want to set this variable to the value \"^fn:$\" or
\(\"fn:\").  Then RefTeX will still do extensive parsing for any
non-footnote labels."
  :group 'reftex-defining-label-environments
  :type '(choice
          (const :tag "Always" t)
          (const :tag "Never" nil)
          (regexp)
          (repeat :tag "List"
                  (string :tag "prefix (with colon)"))))

(defcustom reftex-special-environment-functions nil
  "List of functions to be called when trying to figure out current environment.
These are special functions to detect \"environments\" which do not
start with \\begin and end with \\end.  Some LaTeX packages seem to
use such non-standard ways to set up environment-like constructs.  The
purpose of each function in this list is to detect if point is
currently inside such a special \"environment\".  If the environment
carries a label, you must also set up an entry for it in
`reftex-label-alist'.

The function should check if point is currently in the special
environment it was written to detect.  If so, the function must return
a cons cell (NAME . POSITION).  NAME is the name of the environment
detected and POSITION is the buffer position where the environment
starts.  The function must return nil on failure to detect the
environment.

The function must take an argument BOUND.  If non-nil, BOUND is a
boundary for backwards searches which should be observed.

Here is an example.  The LaTeX package linguex.sty defines list macros
`\\ex.', `\\a.', etc for lists which are terminated by `\\z.' or an empty
line.

    \\ex.  \\label{ex:12} Some text in an exotic language ...
          \\a. \\label{ex:13} more stuff
          \\b. \\label{ex:14} still more stuff

    ... more text after the empty line terminating all lists

And here is the setup for RefTeX:

1. Define a dummy environment for this in `reftex-label-alist'.  Dummy means,
   make up an environment name even though it is not used with \\begin and
   \\end.  Here we use \"linguex\" as this name.

   (setq reftex-label-alist
         '((\"linguex\" ?x \"ex:\" \"~\\\\ref{%s}\" nil (\"Example\" \"Ex.\"))))

2. Write a function to detect the list macros and the determinators as well.

   (defun my-detect-linguex-list (bound)
     (let ((pos (point)) p1)
       (save-excursion
         ;; Search for any of the linguex item macros at the beginning of a line
         (if (re-search-backward
              \"^[ \\t]*\\\\(\\\\\\\\\\\\(ex\\\\|a\\\\|b\\\\|c\\\\|d\\\\|e\\\\|f\\\\)g?\\\\.\\\\)\" bound t)
             (progn
               (setq p1 (match-beginning 1))
               ;; Make sure no empty line or \\z. is between us and the item macro
               (if (re-search-forward \"\\n[ \\t]*\\n\\\\|\\\\\\\\z\\\\.\" pos t)
                   ;; Return nil because list was already closed
                   nil
                 ;; OK, we got it
                 (cons \"linguex\" p1)))
           ;; Return nil for not found
           nil))))

3. Tell RefTeX to use this function

   (setq reftex-special-environment-functions '(my-detect-linguex-list))
"
  :group 'reftex-defining-label-environments
  :type 'hook)

;; Label insertion

(defgroup reftex-making-and-inserting-labels nil
  "Options on how to create new labels."
  :group 'reftex-label-support)

(defcustom reftex-insert-label-flags '("s" "sft")
  "Flags governing label insertion.  First flag DERIVE, second flag PROMPT.

If DERIVE is t, RefTeX will try to derive a sensible label from context.
A section label for example will be derived from the section heading.
The conversion of the context to a valid label is governed by the
specifications given in `reftex-derive-label-parameters'.
If RefTeX fails to derive a label, it will prompt the user.
If DERIVE is nil, the label generated will consist of the prefix and a
unique number, like `eq:23'.

If PROMPT is t, the user will be prompted for a label string.  The prompt will
already contain the prefix, and (if DERIVE is t) a default label derived from
context.  When PROMPT is nil, the default label will be inserted without
query.

So the combination of DERIVE and PROMPT controls label insertion.  Here is a
table describing all four possibilities:

DERIVE   PROMPT      ACTION
-------------------------------------------------------------------------
 nil     nil     Insert simple label, like eq:22 or sec:13.  No query.
 nil     t       Prompt for label.
 t       nil     Derive a label from context and insert without query.
 t       t       Derive a label from context and prompt for confirmation.

Each flag may be set to t, nil, or a string of label type letters
indicating the label types for which it should be true.  The strings work
like character classes.
Thus, the combination may be set differently for each label type.  The
default settings \"s\" and \"sft\" mean: Derive section labels from headings
\(with confirmation).  Prompt for figure and table labels.  Use simple labels
without confirmation for everything else.
The available label types are: s (section), f (figure), t (table), i (item),
e (equation), n (footnote), N (endnote), plus any definitions in
`reftex-label-alist'."
  :group 'reftex-making-and-inserting-labels
  :type  '(list (choice :tag "Derive label from context"
                         (const  :tag "always" t)
                         (const  :tag "never" nil)
                         (string :tag "selected label types" ""))
                (choice :tag "Prompt for label string  "
                        :entry-format "  %b %v"
                        (const  :tag "always" t)
                        (const  :tag "never" nil)
                        (string :tag "selected label types" ""))))

(defcustom reftex-string-to-label-function 'reftex-string-to-label
  "Function to turn an arbitrary string into a valid label.
RefTeX's default function uses the variable `reftex-derive-label-parameters'."
  :group 'reftex-making-and-inserting-labels
  :type 'symbol)

(defcustom reftex-translate-to-ascii-function 'reftex-latin1-to-ascii
  "Filter function which will process a context string before it is used
to derive a label from it.  The intended application is to convert ISO or
Mule characters into something valid in labels.  The default function
removes the accents from Latin-1 characters.  X-Symbol (>=2.6) sets this
variable to the much more general `x-symbol-translate-to-ascii'."
  :group 'reftex-making-and-inserting-labels
  :type 'symbol)

(defcustom reftex-derive-label-parameters '(3 20 t 1 "-"
         ("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to") t)
  "Parameters for converting a string into a label.
This variable is a list of the following items.

NWORDS      Number of words to use.
MAXCHAR     Maximum number of characters in a label string.
INVALID     nil: Throw away any words containing characters invalid in labels.
            t:   Throw away only the invalid characters, not the whole word.
ABBREV      nil: Never abbreviate words.
            t:   Always abbreviate words (see `reftex-abbrev-parameters').
            not t and not nil: Abbreviate words if necessary to shorten
                               label string below MAXCHAR.
SEPARATOR   String separating different words in the label.
IGNOREWORDS List of words which should not be part of labels.
DOWNCASE    t:   Downcase words before using them."
  :group 'reftex-making-and-inserting-labels
  :type  '(list (integer :tag "Number of words            "  3)
                (integer :tag "Maximum label length       " 20)
                (choice  :tag "Invalid characters in words"
                         (const :tag "throw away entire word" nil)
                         (const :tag "throw away single chars" t))
                (choice  :tag "Abbreviate words           "
                         (const :tag "never" nil)
                         (const :tag "always" t)
                         (const :tag "when label is too long" 1))
                (string  :tag "Separator between words    " "-")
                (repeat  :tag "Ignore words"
                         :entry-format "           %i %d %v"
                         (string :tag ""))
                (option (boolean :tag "Downcase words          "))))

(defcustom reftex-label-illegal-re "[^-a-zA-Z0-9_+=:;,.]"
  "Regexp matching characters not valid in labels."
  :group 'reftex-making-and-inserting-labels
  :type '(regexp :tag "Regular Expression"))

(defcustom reftex-abbrev-parameters '(4 2 "^aeiou" "aeiou")
  "Parameters for abbreviation of words.
This variable is a list of the following items.

MIN-CHARS    Minimum number of characters remaining after abbreviation.
MIN-KILL     Minimum number of characters to remove when abbreviating words.
BEFORE       Character class before abbrev point in word.
AFTER        Character class after  abbrev point in word."
  :group 'reftex-making-and-inserting-labels
  :type '(list
          (integer :tag "Minimum chars per word" 4)
          (integer :tag "Shorten by at least   " 2)
          (string  :tag "cut before char class " "^saeiou")
          (string  :tag "cut after  char class " "aeiou")))

(defcustom reftex-format-label-function nil
  "Function which produces the string to insert as a label definition.
Normally should be nil, unless you want to do something fancy.
The function will be called with two arguments, the LABEL and the DEFAULT
FORMAT, which usually is `\\label{%s}'.  The function should return the
string to insert into the buffer."
  :group 'reftex-making-and-inserting-labels
  :type 'function)

;; Label referencing

(defgroup reftex-referencing-labels nil
  "Options on how to reference labels."
  :group 'reftex-label-support)

(eval-and-compile
  (defconst reftex-tmp
    '((const :tag "on" t)
      (const :tag "off" nil)
      (string :tag "Selected label types"))))

(defcustom reftex-label-menu-flags '(t t nil nil nil nil t nil)
  "List of flags governing the label menu makeup.
The flags are:

TABLE-OF-CONTENTS  Show the labels embedded in a table of context.
SECTION-NUMBERS    Include section numbers (like 4.1.3) in table of contents.
COUNTERS           Show counters.  This just numbers the labels in the menu.
NO-CONTEXT         Non-nil means do NOT show the short context.
FOLLOW             Follow full context in other window.
SHOW-COMMENTED     Show labels from regions which are commented out.
MATCH-IN-TOC       Obsolete flag.
SHOW FILES         Show begin and end of included files.

Each of these flags can be set to t or nil, or to a string of type letters
indicating the label types for which it should be true.  These strings work
like character classes in regular expressions.  Thus, setting one of the
flags to \"sf\" makes the flag true for section and figure labels, nil
for everything else.  Setting it to \"^sf\" makes it the other way round.
The available label types are: s (section), f (figure), t (table), i (item),
e (equation), n (footnote), plus any definitions in `reftex-label-alist'.

Most options can also be switched from the label menu itself - so if you
decide here to not have a table of contents in the label menu, you can still
get one interactively during selection from the label menu."
  :group 'reftex-referencing-labels
  :type
  `(list
    (choice :tag "Embed in table of contents      " ,@reftex-tmp)
    (choice :tag "Show section numbers            " ,@reftex-tmp)
    (choice :tag "Show individual counters        " ,@reftex-tmp)
    (choice :tag "Hide short context              " ,@reftex-tmp)
    (choice :tag "Follow context in other window  " ,@reftex-tmp)
    (choice :tag "Show commented labels           " ,@reftex-tmp)
    (choice :tag "Obsolete flag,  Don't use.      " ,@reftex-tmp)
    (choice :tag "Show begin/end of included files" ,@reftex-tmp)))

(defcustom reftex-multiref-punctuation '((?, . ", ") (?- . "--") (?+ . " and "))
  "Punctuation strings for multiple references.
When marking is used in the selection buffer to select several references,
this variable associates the 3 marking characters `,-+' with prefix strings
to be inserted into the buffer before the corresponding \\ref macro.
This is used to string together whole reference sets, like
`eqs. 1,2,3-5,6 and 7' in a single call to `reftex-reference'. See manual."
  :group 'reftex-referencing-labels
  :type '(repeat (cons (character) (string))))

(defcustom reftex-vref-is-default nil
  "*Non-nil means, the varioref macro \\vref is used as default.
In the selection buffer, the `v' key toggles the reference macro between
`\\ref' and `\\vref'.  The value of this variable determines the default
which is active when entering the selection process.
Instead of nil or t, this may also be a string of type letters indicating
the label types for which it should be true."
  :group  'reftex-referencing-labels
  :type `(choice :tag "\\vref is default macro" ,@reftex-tmp))
;;;###autoload(put 'reftex-vref-is-default 'safe-local-variable (lambda (x) (or (stringp x) (symbolp x))))

(defcustom reftex-fref-is-default nil
  "*Non-nil means, the fancyref macro \\fref is used as default.
In the selection buffer, the `V' key toggles the reference macro between
`\\ref', `\\fref' and `\\Fref'.  The value of this variable determines
the default which is active when entering the selection process.
Instead of nil or t, this may also be a string of type letters indicating
the label types for which it should be true."
  :group  'reftex-referencing-labels
  :type `(choice :tag "\\fref is default macro" ,@reftex-tmp))
;;;###autoload(put 'reftex-fref-is-default 'safe-local-variable (lambda (x) (or (stringp x) (symbolp x))))

(defcustom reftex-level-indent 2
  "*Number of spaces to be used for indentation per section level."
  :group 'reftex-referencing-labels
  :type 'integer)
;;;###autoload(put 'reftex-level-indent 'safe-local-variable 'integerp)

(defcustom reftex-guess-label-type t
  "*Non-nil means, `reftex-reference' will try to guess the label type.
To do that, RefTeX will look at the word before the cursor and compare it with
the words given in `reftex-label-alist'.  When it finds a match, RefTeX will
immediately offer the correct label menu - otherwise it will prompt you for
a label type.  If you set this variable to nil, RefTeX will always prompt."
  :group 'reftex-referencing-labels
  :type 'boolean)
;;;###autoload(put 'reftex-guess-label-type 'safe-local-variable (lambda (x) (memq x '(nil t))))

(defcustom reftex-format-ref-function nil
  "Function which produces the string to insert as a reference.
Normally should be nil, because the format to insert a reference can
already be specified in `reftex-label-alist'.
This hook also is used by the special commands to insert `\\vref' and `\\fref'
references, so even if you set this, your setting will be ignored by
the special commands.
The function will be called with two arguments, the LABEL and the DEFAULT
FORMAT, which normally is `~\\ref{%s}'.  The function should return the
string to insert into the buffer."
  :group 'reftex-referencing-labels
  :type 'function)

(defcustom reftex-select-label-mode-hook nil
  "Mode hook for reftex-select-label-mode."
  :group 'reftex-referencing-labels
  :type 'hook)

;; BibteX citation configuration ----------------------------------------

(defgroup reftex-citation-support nil
  "Support for referencing bibliographic data with BibTeX."
  :group 'reftex)

(defcustom reftex-bibliography-commands '("bibliography" "nobibliography")
  "LaTeX commands which specify the BibTeX databases to use with the document."
  :group 'reftex-citation-support
  :type '(repeat string))


(defvar reftex-bibfile-ignore-list nil) ; compatibility
(defcustom reftex-bibfile-ignore-regexps nil
  "*List of regular expressions to exclude files in \\bibliography{..}.
File names matched by these regexps will not be parsed by RefTeX.
Intended for files which contain only `@string' macro definitions and the
like, which are ignored by RefTeX anyway."
  :group 'reftex-citation-support
  :set 'reftex-set-dirty
  :type '(repeat (regexp)))

(defcustom reftex-default-bibliography nil
  "*List of BibTeX database files which should be used if none are specified.
When `reftex-citation' is called from a document which has neither a
`\\bibliography{..}' statement nor a `thebibliography' environment,
RefTeX will scan these files instead.  Intended for using `reftex-citation'
in non-LaTeX files.  The files will be searched along the BIBINPUTS or TEXBIB
path."
  :group 'reftex-citation-support
  :type '(repeat (file)))

(defcustom reftex-sort-bibtex-matches 'reverse-year
  "*Sorting of the entries found in BibTeX databases by reftex-citation.
Possible values:
nil            Do not sort entries.
'author        Sort entries by author name.
'year          Sort entries by increasing year.
'reverse-year  Sort entries by decreasing year."
  :group 'reftex-citation-support
  :type '(choice (const :tag "not" nil)
                 (const :tag "by author" author)
                 (const :tag "by year"   year)
                 (const :tag "by year, reversed" reverse-year)))

(defcustom reftex-cite-format 'default
  "*The format of citations to be inserted into the buffer.
It can be a string or an alist or a symbol.  In the simplest case this
is just the string \"\\cite{%l}\", which is also the default.  See the
definition of `reftex-cite-format-builtin' for more complex examples.

If `reftex-cite-format' is a string, it will be used as the format.
In the format, the following percent escapes will be expanded.

%l   The BibTeX label of the citation.
%a   List of author names, see also `reftex-cite-punctuation'.
%2a  Like %a, but abbreviate more than 2 authors like Jones et al.
%A   First author name only.
%e   Works like %a, but on list of editor names. (%2e and %E work a well)

It is also possible to access all other BibTeX database fields:
%b booktitle     %c chapter        %d edition    %h howpublished
%i institution   %j journal        %k key        %m month
%n number        %o organization   %p pages      %P first page
%r address       %s school         %u publisher  %t title
%v volume        %y year
%B booktitle, abbreviated          %T title, abbreviated

Usually, only %l is needed.  The other stuff is mainly for the echo area
display, and for (setq reftex-comment-citations t).

%< as a special operator kills punctuation and space around it after the
string has been formatted.

A pair of square brackets indicates an optional argument, and RefTeX
will prompt for the values of these arguments.

Beware that all this only works with BibTeX database files.  When
citations are made from the \\bibitems in an explicit thebibliography
environment, only %l is available.

If `reftex-cite-format' is an alist of characters and strings, the user
will be prompted for a character to select one of the possible format
strings.
  In order to configure this variable, you can either set
`reftex-cite-format' directly yourself or set it to the SYMBOL of one of
the predefined styles.  The predefined symbols are those which have an
association in the constant `reftex-cite-format-builtin'.
E.g.: (setq reftex-cite-format 'natbib)"
  :group 'reftex-citation-support
  :type
  `(choice
    :format "%{%t%}: \n%[Value Menu%] %v"
    (radio :tag "Symbolic Builtins"
           :indent 4
           :value default
           ,@(mapcar
              (lambda (x)
                (list 'const :tag (concat (symbol-name (nth 0 x))
                                           ": " (nth 1 x))
                      (nth 0 x)))
              reftex-cite-format-builtin))
    (string :tag "format string" "\\cite{%l}")
    (repeat :tag "key-ed format strings"
            :value ((?\r . "\\cite{%l}")
                    (?t  . "\\cite{%l}") (?p . "\\cite{%l}"))
            (cons (character :tag "Key character" ?\r)
                  (string    :tag "Format string" "")))))

(defcustom reftex-cite-prompt-optional-args 'maybe
  "*Non-nil means, prompt for empty optional arguments in cite macros.
When an entry in `reftex-cite-format' ist given with square brackets to
indicate optional arguments (for example \\cite[][]{%l}), RefTeX can
prompt for values.  Possible values are:

nil     Never prompt for optional arguments
t       Always prompt
maybe   Prompt only if `reftex-citation' was called with C-u prefix arg

Unnecessary empty optional arguments are removed before insertion into
the buffer.  See `reftex-cite-cleanup-optional-args'."
  :group 'reftex-citation-support
  :type '(choice
          (const :tag "Always" t)
          (const :tag "When called with prefix arg" maybe)
          (const :tag "Never" nil)))

(defcustom reftex-cite-cleanup-optional-args t
  "*Non-nil means, remove unnecessary empty optional arguments in cite macros.
The cite macros provided by some packages (for example
natbib) allow specifying two optional arguments, one for a prefix to
the citation, and a second for a postfix.  When only one optional
argument is given, it is interpreted as postfix.  When this option is
t, RefTeX removes unnecessary empty optional arguments from the cite
macro before insertion.  For example, it will change
    \\cite[][]{Jones}              -> \\cite{Jones}
    \\cite[][Chapter 1]{Jones}     -> \\cite[Chapter 1]{Jones}
    \\cite[see][]{Jones}           -> \\cite[see][]{Jones}
    \\cite[see][Chapter 1]{Jones}  -> \\cite{Jones}
Is is possible that other packages have other conventions about which
optional argument is interpreted how - that is why this cleaning up
can be turned off."
  :group 'reftex-citation-support
  :type 'boolean)

(defcustom reftex-comment-citations nil
  "*Non-nil means add a comment for each citation describing the full entry.
The comment is formatted according to `reftex-cite-comment-format'."
  :group 'reftex-citation-support
  :type 'boolean)

(defcustom reftex-cite-comment-format
  "%% %2a %y, %j %v, %P, %b, %e, %u, %s %<\n"
  "Citation format used for commented citations.  Must NOT contain %l.
See the variable `reftex-cite-format' for possible percent escapes."
  :group 'reftex-citation-support
  :type 'string)

(defcustom reftex-cite-view-format "%2a %y, %T, %B, %j %v:%P, %s %<"
  "Citation format used to display citation info in the message area.
Must NOT contain %l.  See the variable `reftex-cite-format' for
possible percent escapes."
  :group 'reftex-citation-support
  :group 'reftex-viewing-cross-references
  :type 'string)

(defcustom reftex-cite-punctuation '(", " " \\& " " {\\it et al.}")
  "Punctuation for formatting of name lists in citations.
This is a list of 3 strings.
1. Normal names separator, like \", \"     in Jones, Brown and Miller
2. Final names separator,  like \" and \"  in Jones, Brown and Miller
3. The \"et al\" string,   like \" {\\it et al.}\" in Jones {\\it et al.}"
  :group 'reftex-citation-support
  :type '(list
          (string :tag "Separator for names            ")
          (string :tag "Separator for last name in list")
          (string :tag "string used as et al.          ")))

(defcustom reftex-format-cite-function nil
  "Function which produces the string to insert as a citation.
Normally should be nil, because the format to insert a reference can
already be specified in `reftex-cite-format'.
The function will be called with two arguments, the CITATION KEY and the
DEFAULT FORMAT, which is taken from `reftex-cite-format'.  The function
should return the string to insert into the buffer."
  :group 'reftex-citation-support
  :type 'function)

(defcustom reftex-select-bib-mode-hook nil
  "Mode hook for reftex-select-bib-mode."
  :group 'reftex-citation-support
  :type 'hook)

;; Index Support Configuration

(defgroup reftex-index-support nil
  "Support for viewing and editing the index."
  :group 'reftex)

(defcustom reftex-support-index t
  "*Non-nil means, index entries are parsed as well.
Index support is resource intensive and the internal structure holding the
parsed information can become quite big.  Therefore it can be turned off.
When this is nil and you execute a command which requires index support,
you will be asked for confirmation to turn it on and rescan the document."
  :group 'reftex-index-support
  :type 'boolean)

(defcustom reftex-index-special-chars '("!" "|" "@" "\"" "\\")
  "Special characters in index entries.  The value is a list of five strings.
These correspond to the makeindex keywords LEVEL ENCAP ACTUAL QUOTE ESCAPE."
  :group 'reftex-index-support
  :type '(list
          (string :tag "LEVEL  separator")
          (string :tag "ENCAP  char     ")
          (string :tag "ACTUAL char     ")
          (string :tag "QUOTE  char     ")
          (string :tag "ESCAPE char     ")))

(defcustom reftex-index-macros nil
  "Macros which define index entries.  The structure is

\(MACRO INDEX-TAG KEY PREFIX EXCLUDE REPEAT)

MACRO is the macro.  Arguments should be denoted by empty braces like
\\index[]{*}.  Use square brackets to denote optional arguments.  The star
marks where the index key is.

INDEX-TAG is a short name of the index.  \"idx\" and \"glo\" are
reserved for the default index and the glossary.  Other indices can be
defined as well.  If this is an integer, the Nth argument of the macro
holds the index tag.

KEY is a character which is used to identify the macro for input with
\\[reftex-index].  ?i, ?I, and ?g are reserved for default index and glossary.

PREFIX can be a prefix which is added to the KEY part of the index entry.
If you have a macro \\newcommand{\\molec}[1]{#1\\index{Molecules!#1}}, this
prefix should be \"Molecules!\".  See the manual for details.

EXCLUDE can be a function.  If this function exists and returns a non-nil
value, the index entry at point is ignored.  This was implemented to support
the (deprecated) `^' and `_' shortcuts in the LaTeX2e `index' package.

REPEAT, if non-nil, means the index macro does not typeset the entry in
the text, so that the text has to be repeated outside the index macro.
Needed for `reftex-index-selection-or-word' and for indexing from the
phrase buffer.

The final entry may also be a symbol if this entry has a association
in the variable `reftex-index-macros-builtin' to specify the main
indexing package you are using.  Valid values are currently
default         The LaTeX default - unnecessary to specify this one
multind         The multind.sty package
index           The index.sty package
index-shortcut  The index.sty packages with the ^ and _ shortcuts.
                Should not be used - only for old documents.
Note that AUCTeX sets these things internally for RefTeX as well, so
with a sufficiently new version of AUCTeX, you should not set the
package here."
  :group 'reftex-index-support
  :set 'reftex-set-dirty
  :type `(list
          (repeat
           :inline t
           (list :value ("" "idx" ?a "" nil)
                 (string  :tag "Macro with args")
                 (choice  :tag "Index Tag      "
                         (string)
                         (integer :tag "Macro arg Nr" :value 1))
                 (character :tag "Access Key     ")
                 (string  :tag "Key Prefix     ")
                 (symbol  :tag "Exclusion hook ")
                 (boolean :tag "Repeat Outside ")))
          (option
           :tag "Package:"
           (choice :tag "Package"
                   :value index
                   ,@(mapcar
                      (lambda (x)
                        (list 'const :tag (concat (symbol-name (nth 0 x))
                                                  ": " (nth 1 x))
                              (nth 0 x)))
                      reftex-index-macros-builtin)))))

(defcustom reftex-index-default-macro '(?i "idx")
  "The default index macro for \\[reftex-index-selection-or-word].
This is a list with (MACRO-KEY DEFAULT-TAG).

MACRO-KEY:   Character identifying an index macro - see `reftex-index-macros'.
DEFAULT-TAG: This is the tag to be used if the macro requires a TAG argument.
             When this is nil and a TAG is needed, RefTeX will ask for it.
             When this is the empty string and the TAG argument of the index
             macro is optional, the TAG argument will be omitted."
  :group 'reftex-index-support
  :type '(list
          (character :tag "Character identifying default macro")
          (choice    :tag "Default index tag                  "
                  (const nil)
                  (string))))

(defcustom reftex-index-default-tag "idx"
  "Default index tag.
When working with multiple indexes, RefTeX queries for an index tag when
creating index entries or displaying a specific index.  This variable controls
the default offered for these queries.  The default can be selected with RET
during selection or completion.  Valid values of this variable are:

nil       Do not provide a default index
\"tag\"     The default index tag given as a string, e.g. \"idx\".
last      The last used index tag will be offered as default."
  :group 'reftex-index-support
  :type '(choice
          (const :tag  "no default" nil)
          (const :tag  "last used " 'last)
          (string :tag "index tag " "idx")))

(defcustom reftex-index-math-format "$%s$"
  "Format of index entries when copied from inside math mode.
When `reftex-index-selection-or-word' is executed inside TeX math mode,
the index key copied from the buffer is processed with this format string
through the `format' function.  This can be used to add the math delimiters
\(e.g. `$') to the string.
Requires the `texmathp.el' library which is part of AUCTeX."
  :group 'reftex-index-support
  :type 'string)

(defcustom reftex-index-phrase-file-extension ".rip"
  "File extension for the index phrase file.
This extension will be added to the base name of the master file."
  :group 'reftex-index-support
  :type 'string)

(defcustom reftex-index-phrases-logical-and-regexp " *&& *"
  "Regexp matching the `and' operator for index arguments in phrases file.
When several index arguments in a phrase line are separated by this
operator, each part will generate an index macro.  So each match of
the search phrase will produce *several* different index entries.

Note: make sure this does no match things which are not separators.
This logical `and' has higher priority than the logical `or' specified in
`reftex-index-phrases-logical-or-regexp'."
  :group 'reftex-index-support
  :type 'regexp)

(defcustom reftex-index-phrases-logical-or-regexp " *|| *"
  "Regexp matching the `or' operator for index arguments in phrases file.
When several index arguments in a phrase line are separated by this
operator, the user will be asked to select one of them at each match
of the search phrase.  The first index arg will be the default - a
number key 1-9 must be pressed to switch to another.

Note: make sure this does no match things which are not separators.
The logical `and' specified in `reftex-index-phrases-logical-or-regexp'
has higher priority than this logical `or'."
  :group 'reftex-index-support
  :type 'regexp)

(defcustom reftex-index-phrases-search-whole-words t
  "*Non-nil means phrases search will look for whole words, not subwords.
This works by requiring word boundaries at the beginning and end of
the search string.  When the search phrase already has a non-word-char
at one of these points, no word boundary is required there."
  :group 'reftex-index-support
  :type 'boolean)

(defcustom reftex-index-phrases-case-fold-search t
  "*Non-nil means, searching for index phrases will ignore case."
  :group 'reftex-index-support
  :type 'boolean)

(defcustom reftex-index-verify-function nil
  "A function which is called  at each match during global indexing.
If the function returns nil, the current match is skipped."
  :group 'reftex-index-support
  :type '(choice
          (const :tag "No verification" nil)
          (function)))

(defcustom reftex-index-phrases-skip-indexed-matches nil
  "*Non-nil means, skip matches which appear to be indexed already.
When doing global indexing from the phrases buffer, searches for some
phrases may match at places where that phrase was already indexed.  In
particular when indexing an already processed document again, this
will even be the norm.  When this variable is non-nil, RefTeX checks if
the match is inside an index macro argument, or if an index macro is directly
before or after the phrase.  If that is the case, that match will
be ignored."
  :group 'reftex-index-support
  :type 'boolean)

(defcustom reftex-index-phrases-wrap-long-lines nil
  "*Non-nil means, when indexing from the phrases buffer, wrap lines.
Inserting indexing commands in a line makes the line longer - often
so long that it does not fit onto the screen.  When this variable is
non-nil, newlines will be added as necessary before and/or after the
indexing command to keep lines short.  However, the matched text
phrase and its index command will always end up on a single line."
  :group 'reftex-index-support
  :type 'boolean)

(defcustom reftex-index-phrases-sort-prefers-entry nil
  "*Non-nil means when sorting phrase lines, the explicit index entry is used.
Phrase lines in the phrases buffer contain a search phrase, and
sorting is normally based on these.  Some phrase lines also have
an explicit index argument specified.  When this variable is non-nil,
the index argument will be used for sorting."
  :group 'reftex-index-support
  :type 'boolean)

(defcustom reftex-index-phrases-sort-in-blocks t
  "*Non-nil means, empty and comment lines separate phrase buffer into blocks.
Sorting will then preserve blocks, so that lines are re-arranged only
within blocks."
  :group 'reftex-index-support
  :type 'boolean)

(defcustom reftex-index-section-letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "The letters which denote sections in the index.
Usually these are all capital letters.  Don't use any downcase letters.
Order is not significant, the index will be sorted by whatever the sort
function thinks is correct.
In addition to these letters, RefTeX will create a group `!' which
contains all entries sorted below the lowest specified letter.
In the index buffer, pressing any of these capital letters or `!' will jump
to that section."
  :group 'reftex-index-support
  :type '(string :tag "Capital letters"))

(defcustom reftex-index-include-context nil
  "*Non-nil means, display the index definition context in the index buffer.
This flag may also be toggled from the index buffer with the `c' key."
  :group 'reftex-index-support
  :type 'boolean)

(defcustom reftex-index-follow-mode nil
  "*Non-nil means, point in *Index* buffer will cause other window to follow.
The other window will show the corresponding part of the document.
This flag can be toggled from within the *Index* buffer with the `f' key."
  :group 'reftex-table-of-contents-browser
  :type 'boolean)

;; Viewing Cross References

(defgroup reftex-viewing-cross-references nil
  "Displaying cross references and citations."
  :group 'reftex)

(defcustom reftex-view-crossref-extra nil
  "Macros which can be used for the display of cross references.
This is used when `reftex-view-crossref' is called with point in an
argument of a macro.  Note that crossref viewing for citations,
references (both ways) and index entries is hard-coded.  This variable
is only to configure additional structures for which crossreference
viewing can be useful.  Each entry has the structure

\(MACRO-RE SEARCH-RE HIGHLIGHT).

MACRO-RE is matched against the macro.  SEARCH-RE is the regexp used
to search for cross references.  `%s' in this regexp is replaced with
with the macro argument at point.  HIGHLIGHT is an integer indicating
which subgroup of the match should be highlighted."
  :group 'reftex-viewing-cross-references
  :type '(repeat (group (regexp  :tag "Macro  Regexp  ")
                        (string  :tag "Search Regexp  ")
                        (integer :tag "Highlight Group"))))

(defcustom reftex-auto-view-crossref t
  "*Non-nil means, initially turn automatic viewing of crossref info on.
Automatic viewing of crossref info normally uses the echo area.
Whenever point is idle for more than `reftex-idle-time' seconds on the
argument of a \\ref or \\cite macro, and no other message is being
displayed, the echo area will display information about that cross
reference.  You can also set the variable to the symbol `window'.  In
this case a small temporary window is used for the display.
This feature can be turned on and off from the menu
\(Ref->Options)."
  :group 'reftex-viewing-cross-references
  :type '(choice (const :tag "off" nil)
                 (const :tag "in Echo Area" t)
                 (const :tag "in Other Window" window)))

(defcustom reftex-idle-time 1.2
  "*Time (secs) Emacs has to be idle before automatic crossref display is done.
Applies also to toc recentering."
  :group 'reftex-viewing-cross-references
  :type 'number)

(defcustom reftex-revisit-to-echo nil
  "*Non-nil means, automatic citation display will revisit files if necessary.
When nil, citation display in echo area will only be active for cached
entries and for BibTeX database files with live associated buffers."
  :group 'reftex-viewing-cross-references
  :type 'boolean)

(defcustom reftex-cache-cite-echo t
  "*Non-nil means, the information displayed in the echo area for cite macros
is cached and even saved along with the parsing information.  The cache
survives document scans.  In order to clear it, use M-x reftex-reset-mode."
  :group 'reftex-viewing-cross-references
  :type 'boolean)

(defcustom reftex-display-copied-context-hook nil
  "Normal Hook which is run before context is displayed anywhere.  Designed
for X-Symbol, but may have other uses as well."
  :group 'reftex-viewing-cross-references
  :group 'reftex-referencing-labels
  :type 'hook)

;; Finding Files --------------------------------------------------------

(defgroup reftex-finding-files nil
  "Finding files on search paths."
  :group 'reftex)

(defcustom reftex-texpath-environment-variables '("TEXINPUTS")
  "*List of specifications how to retrieve the search path for TeX files.
Several entries are possible.
- If an element is the name of an environment variable, its content is used.
- If an element starts with an exclamation mark, it is used as a command
  to retrieve the path.  A typical command with the kpathsearch library would
  be `!kpsewhich -show-path=.tex'.
- Otherwise the element itself is interpreted as a path.
Multiple directories can be separated by the system dependent `path-separator'.
Directories ending in `//' or `!!' will be expanded recursively.
See also `reftex-use-external-file-finders'."
  :group 'reftex-finding-files
  :set 'reftex-set-dirty
  :type '(repeat (string :tag "Specification")))

(defcustom reftex-bibpath-environment-variables '("BIBINPUTS" "TEXBIB")
  "*List of specifications how to retrieve search path for .bib database files.
Several entries are possible.
- If an element is the name of an environment variable, its content is used.
- If an element starts with an exclamation mark, it is used as a command
  to retrieve the path.  A typical command with the kpathsearch library would
  be `!kpsewhich -show-path=.bib'.
- Otherwise the element itself is interpreted as a path.
Multiple directories can be separated by the system dependent `path-separator'.
Directories ending in `//' or `!!' will be expanded recursively.
See also `reftex-use-external-file-finders'."
  :group 'reftex-citation-support
  :group 'reftex-finding-files
  :set 'reftex-set-dirty
  :type '(repeat (string :tag "Specification")))

(defcustom reftex-file-extensions '(("tex" . (".tex" ".ltx"))
                                    ("bib" . (".bib")))
  "*Association list with file extensions for different file types.
This is a list of items, each item is like: (TYPE . (DEF-EXT OTHER-EXT ...))

TYPE:       File type like \"bib\" or \"tex\".
DEF-EXT:    The default extension for that file type, like \".tex\" or \".bib\".
OTHER-EXT:  Any number of other valid extensions for this file type.

When a files is searched and it does not have any of the legal extensions,
we try the default extension first, and then the naked file name.

If you are using AUCTeX, you also need to add new extensions to
TeX-file-extensions."
  :group 'reftex-finding-files
  :type '(repeat (cons (string :tag "File type")
                       (repeat (string :tag "Extension")))))

(defcustom reftex-try-all-extensions nil
  "Non-nil means, try all extensions listed in `reftex-file-extensions'.
When searching for a file, LaTeX uses only the default extension.  However,
if you are working with a noweb system that produces the .tex files from
some other file, and you want RefTeX to scan the web file instead of the
tex file, you need to set this option.  You also need to make the noweb
extension the default extension, i.e. the first in the list in
`reftex-file-extensions'.
Note that if you are using external file finders, this option has no effect."
  :group 'reftex-finding-files
  :type 'boolean)

(defcustom reftex-search-unrecursed-path-first t
  "*Non-nil means, search all specified directories before trying recursion.
Thus, in a path \".//:/tex/\", search first \"./\", then \"/tex/\" and then
all subdirectories of \"./\".  If this option is nil, the subdirectories of
\"./\" are searched before \"/tex/\".  This is mainly for speed - most of the
time the recursive path is for the system files and not for the user files.
Set this to nil if the default makes RefTeX finding files with equal names
in wrong sequence."
  :group 'reftex-finding-files
  :type 'boolean)

(defcustom reftex-use-external-file-finders nil
  "*Non-nil means, use external programs to find files.
Normally, RefTeX searches the paths given in the environment variables
TEXINPUTS and BIBINPUTS to find TeX files and BibTeX database files.
With this option turned on, it calls an external program specified in the
option `reftex-external-file-finders' instead.  As a side effect,
the variables `reftex-texpath-environment-variables' and
`reftex-bibpath-environment-variables' will be ignored."
  :group 'reftex-finding-files
  :type 'boolean)

(defcustom reftex-external-file-finders '(("tex" . "kpsewhich -format=.tex %f")
                                          ("bib" . "kpsewhich -format=.bib %f"))
  "*Association list with external programs to call for finding files.
Each entry is a cons cell (TYPE . PROGRAM).
TYPE is either \"tex\" or \"bib\".  PROGRAM is the external program to use with
any arguments.  %f will be replaced by the name of the file to be found.
Note that these commands will be executed directly, not via a shell.
Only relevant when `reftex-use-external-file-finders' is non-nil."
  :group 'reftex-finding-files
  :type '(repeat (cons (string :tag "File type")
                       (string :tag "Program  "))))

;; Tuning the parser ----------------------------------------------------

(defgroup reftex-optimizations-for-large-documents nil
  "Configuration of parser speed and memory usage."
  :group 'reftex)

(defcustom reftex-keep-temporary-buffers 1
  "*Non-nil means, keep buffers created for parsing and lookup.
RefTeX sometimes needs to visit files related to the current document.
We distinguish files visited for
PARSING: Parts of a multifile document loaded when (re)-parsing the document.
LOOKUP:  BibTeX database files and TeX files loaded to find a reference,
         to display label context, etc.
The created buffers can be kept for later use, or be thrown away immediately
after use, depending on the value of this variable:

nil  Throw away as much as possible.
t    Keep everything.
1    Throw away buffers created for parsing, but keep the ones created
     for lookup.

If a buffer is to be kept, the file is visited normally (which is potentially
slow but will happen only once).
If a buffer is to be thrown away, the initialization of the buffer depends
upon the variable `reftex-initialize-temporary-buffers'."
  :group 'reftex-optimizations-for-large-documents
  :type '(choice
          (const :tag "Throw away everything" nil)
          (const :tag "Keep everything" t)
          (const :tag "Keep lookup buffers only" 1)))

(defcustom reftex-initialize-temporary-buffers nil
  "*Non-nil means do initializations even when visiting file temporarily.
When nil, RefTeX may turn off find-file hooks and other stuff to briefly
visit a file.
When t, the full default initializations are done (find-file-hook etc.).
Instead of t or nil, this variable may also be a list of hook functions to
do a minimal initialization."
  :group 'reftex-optimizations-for-large-documents
  :type '(choice
          (const :tag "Read files literally" nil)
          (const :tag "Fully initialize buffers" t)
          (repeat :tag "Hook functions" :value (nil)
           (function-item))))

(defcustom reftex-no-include-regexps '("\\.pstex_t\\'")
  "*List of regular expressions to exclude certain input files from parsing.
If the name of a file included via \\include or \\input is matched by any
of the regular expressions in this list, that file is not parsed by RefTeX."
  :group 'reftex-optimizations-for-large-documents
  :type '(repeat (regexp)))

(defcustom reftex-enable-partial-scans nil
  "*Non-nil means, re-parse only 1 file when asked to re-parse.
Re-parsing is normally requested with a `C-u' prefix to many RefTeX commands,
or with the `r' key in menus.  When this option is t in a multifile document,
we will only parse the current buffer, or the file associated with the label
or section heading near point in a menu.  Requesting re-parsing of an entire
multifile document then requires a `C-u C-u' prefix or the capital `R' key
in menus."
  :group 'reftex-optimizations-for-large-documents
  :type 'boolean)

(defcustom reftex-allow-automatic-rescan t
  "*Non-nil means, RefTeX may rescan the document when this seems necessary.
Currently this applies only to rescanning after label insertion, when
the new label cannot be inserted correctly into the internal label
list."
  :group 'reftex-optimizations-for-large-documents
  :type 'boolean)

(defcustom reftex-save-parse-info nil
  "*Non-nil means, save information gathered with parsing in a file.
The file MASTER.rel in the same directory as MASTER.tex is used to save the
information.  When this variable is t,
- accessing the parsing information for the first time in an editing session
  will read that file (if available) instead of parsing the document.
- exiting Emacs or killing a buffer in reftex-mode will cause a new version
  of the file to be written."
  :group 'reftex-optimizations-for-large-documents
  :type 'boolean)

(defcustom reftex-parse-file-extension ".rel"
  "*File extension for the file in which parser information is stored.
This extension is added to the base name of the master file."
  :group 'reftex-optimizations-for-large-documents
  :type 'string)

(defcustom reftex-use-multiple-selection-buffers nil
  "*Non-nil means use a separate selection buffer for each label type.
These buffers are kept from one selection to the next and need not to be
created for each use - so the menu generally comes up faster.  The
selection buffers will be erased (and therefore updated) automatically
when new labels in its category are added.  See the variable
`reftex-auto-update-selection-buffers'."
  :group 'reftex-optimizations-for-large-documents
  :group 'reftex-referencing-labels
  :type 'boolean)

(defcustom reftex-auto-update-selection-buffers t
  "*Non-nil means, selection buffers will be updated automatically.
When a new label is defined with `reftex-label', all selection buffers
associated with that label category are emptied, in order to force an
update upon next use.  When nil, the buffers are left alone and have to be
updated by hand, with the `g' key from the label selection process.
The value of this variable will only have any effect when
`reftex-use-multiple-selection-buffers' is non-nil."
  :group 'reftex-optimizations-for-large-documents
  :group 'reftex-referencing-labels
  :type 'boolean)

;; Fontification and Faces ----------------------------------------------

(defgroup reftex-fontification-configurations nil
  "Options concerning the faces used in RefTeX."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'reftex)

(defcustom reftex-use-fonts t
  "*Non-nil means, use fonts in *toc* and selection buffers.
Font-lock must be loaded as well to actually get fontified display.
When changing this option, a rescan may be necessary to activate the change."
  :group 'reftex-fontification-configurations
  :type 'boolean)

(defcustom reftex-refontify-context 1
  "*Non-nil means, re-fontify the context in the label menu with font-lock.
This slightly slows down the creation of the label menu.  It is only necessary
when you definitely want the context fontified.

This option may have 3 different values:
nil  Never refontify.
t    Always refontify.
1    Refontify when absolutely necessary, e.g. when old versions of X-Symbol.
The option is ignored when `reftex-use-fonts' is nil."
  :group 'reftex-fontification-configurations
  :group 'reftex-referencing-labels
  :type '(choice
          (const :tag "Never" nil)
          (const :tag "Always" t)
          (const :tag "When necessary" 1)))

(defcustom reftex-highlight-selection 'cursor
  "*Non-nil mean, highlight selected text in selection and *toc* buffers.
Normally, the text near the cursor is the selected text, and it is
highlighted.  This is the entry most keys in the selection and *toc*
buffers act on.  However, if you mainly use the mouse to select an
item, you may find it nice to have mouse-triggered highlighting
instead or as well.  The variable may have one of these values:

   nil      No highlighting.
   cursor   Highlighting is cursor driven.
   mouse    Highlighting is mouse driven.
   both     Both cursor and mouse trigger highlighting.

Changing this variable requires to rebuild the selection and *toc* buffers
to become effective (keys `g' or `r')."
  :group 'reftex-fontification-configurations
  :type '(choice
          (const :tag "Never" nil)
          (const :tag "Cursor driven" cursor)
          (const :tag "Mouse driven" mouse)
          (const :tag "Mouse and Cursor driven." both)))

(defcustom reftex-cursor-selected-face 'highlight
  "Face name to highlight cursor selected item in toc and selection buffers.
See also the variable `reftex-highlight-selection'."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-mouse-selected-face 'secondary-selection
  "Face name to highlight mouse selected item in toc and selection buffers.
See also the variable `reftex-highlight-selection'."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-file-boundary-face 'font-lock-comment-face
  "Face name for file boundaries in selection buffer."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-label-face 'font-lock-constant-face
  "Face name for labels in selection buffer."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-section-heading-face 'font-lock-function-name-face
  "Face name for section headings in toc and selection buffers."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-toc-header-face 'font-lock-comment-face
  "Face name for the header of a toc buffer."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-bib-author-face 'font-lock-keyword-face
  "Face name for author names in bib selection buffer."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-bib-year-face 'font-lock-comment-face
  "Face name for year in bib selection buffer."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-bib-title-face 'font-lock-function-name-face
  "Face name for article title in bib selection buffer."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-bib-extra-face 'font-lock-comment-face
  "Face name for bibliographic information in bib selection buffer."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-select-mark-face 'bold
  "Face name for marked entries in the selection buffers."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-index-header-face 'font-lock-comment-face
  "Face name for the header of an index buffer."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-index-section-face 'font-lock-function-name-face
  "Face name for the start of a new letter section in the index."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-index-tag-face 'font-lock-keyword-face
  "Face name for index names (for multiple indices)."
  :group 'reftex-fontification-configurations
  :type 'symbol)
(defcustom reftex-index-face 'font-lock-constant-face
  "Face name for index entries."
  :group 'reftex-fontification-configurations
  :type 'symbol)

(defcustom reftex-pre-refontification-functions nil
  "X-Symbol specific hook.
Functions get two arguments, the buffer from where the command started and a
symbol indicating in what context the hook is called."
  :group 'reftex-fontification-configurations
  :type 'hook)

;; Miscellaneous configurations -----------------------------------------

(defgroup reftex-miscellaneous-configurations nil
  "Collection of further configurations."
  :group 'reftex)

(defcustom reftex-extra-bindings nil
  "Non-nil means, make additional key bindings on startup.
These extra bindings are located in the
`reftex-extra-bindings-map' map, bound to
`reftex-extra-bindings-prefix'."
  :group 'reftex-miscellaneous-configurations
  :type 'boolean)

;; below, default is C-c C-y because it is free in LaTeX mode.
(defcustom reftex-extra-bindings-prefix "\C-c\C-y"
  "When `reftex-extra-bindings' is set to non-nil, use extra
bindings with this prefix bound to `reftex-extra-bindings-map'."
  :group 'reftex-miscellaneous-configurations
  :type 'boolean)

(defcustom reftex-plug-into-AUCTeX nil
  "*Plug-in flags for AUCTeX interface.
This variable is a list of 4 boolean flags.  When a flag is non-nil,
RefTeX will

  - supply labels in new sections and environments  (flag 1)
  - supply arguments for macros like `\\label'.      (flag 2)
  - supply arguments for macros like `\\ref'.        (flag 3)
  - supply arguments for macros like `\\cite'.       (flag 4)
  - supply arguments for macros like `\\index'.      (flag 5)

You may also set the variable itself to t or nil in order to turn all
plug-ins on or off, respectively.
\\<LaTeX-mode-map>Supplying labels in new sections and environments applies when creating
sections with \\[LaTeX-section] and environments with \\[LaTeX-environment].
Supplying macro arguments applies when you insert such a macro interactively
with \\[TeX-insert-macro].
See the AUCTeX documentation for more information.
RefTeX uses `fset' to take over the function calls.  Changing the variable
may require a restart of Emacs in order to become effective."
  :group 'reftex-miscellaneous-configurations
  :group 'LaTeX
  :type '(choice
          (const :tag "No plug-ins" nil)
          (const :tag "All possible plug-ins" t)
          (list
           :tag "Individual choice"
           :value (t t t t t)
           (boolean :tag "supply label in new sections and environments")
           (boolean :tag "supply argument for macros like `\\label'     ")
           (boolean :tag "supply argument for macros like `\\ref'       ")
           (boolean :tag "supply argument for macros like `\\cite'      ")
           (boolean :tag "supply argument for macros like `\\index'     ")
           )))

(defcustom reftex-allow-detached-macro-args nil
  "*Non-nil means, allow arguments of macros to be detached by whitespace.
When this is t, `aaa' will be considered as argument of \\bb in the following
construct:  \\bbb [xxx] {aaa}."
  :group 'reftex-miscellaneous-configurations
  :type 'boolean)


(defcustom reftex-load-hook nil
  "Hook which is being run when loading reftex.el."
  :group 'reftex-miscellaneous-configurations
  :type 'hook)

(defcustom reftex-mode-hook nil
  "Hook which is being run when turning on RefTeX mode."
  :group 'reftex-miscellaneous-configurations
  :type 'hook)


(provide 'reftex-vars)

;;; reftex-vars.el ends here
