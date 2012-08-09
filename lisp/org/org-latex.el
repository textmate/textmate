;;; org-latex.el --- LaTeX exporter for org-mode
;;
;; Copyright (C) 2007-2012 Free Software Foundation, Inc.
;;
;; Emacs Lisp Archive Entry
;; Filename: org-latex.el
;; Author: Bastien Guerry <bzg AT gnu DOT org>
;; Maintainer: Carsten Dominik <carsten.dominik AT gmail DOT com>
;; Keywords: org, wp, tex
;; Description: Converts an org-mode buffer into LaTeX

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
;; This library implements a LaTeX exporter for org-mode.
;;
;; It is part of Org and will be autoloaded
;;
;; The interactive functions are similar to those of the HTML exporter:
;;
;; M-x `org-export-as-latex'
;; M-x `org-export-as-pdf'
;; M-x `org-export-as-pdf-and-open'
;; M-x `org-export-as-latex-batch'
;; M-x `org-export-as-latex-to-buffer'
;; M-x `org-export-region-as-latex'
;; M-x `org-replace-region-by-latex'
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'footnote)
(require 'org)
(require 'org-exp)
(require 'org-macs)
(require 'org-beamer)

;;; Variables:
(defvar org-export-latex-class nil)
(defvar org-export-latex-class-options nil)
(defvar org-export-latex-header nil)
(defvar org-export-latex-append-header nil)
(defvar org-export-latex-options-plist nil)
(defvar org-export-latex-todo-keywords-1 nil)
(defvar org-export-latex-complex-heading-re nil)
(defvar org-export-latex-not-done-keywords nil)
(defvar org-export-latex-done-keywords nil)
(defvar org-export-latex-display-custom-times nil)
(defvar org-export-latex-all-targets-re nil)
(defvar org-export-latex-add-level 0)
(defvar org-export-latex-footmark-seen nil
  "List of footnotes markers seen so far by exporter.")
(defvar org-export-latex-sectioning "")
(defvar org-export-latex-sectioning-depth 0)
(defvar org-export-latex-special-keyword-regexp
  (concat "\\<\\(" org-scheduled-string "\\|"
	  org-deadline-string "\\|"
	  org-closed-string"\\)")
  "Regexp matching special time planning keywords plus the time after it.")
(defvar org-re-quote)  ; dynamically scoped from org.el
(defvar org-commentsp) ; dynamically scoped from org.el

;;; User variables:

(defgroup org-export-latex nil
  "Options for exporting Org-mode files to LaTeX."
  :tag "Org Export LaTeX"
  :group 'org-export)

(defcustom org-export-latex-default-class "article"
  "The default LaTeX class."
  :group 'org-export-latex
  :type '(string :tag "LaTeX class"))

(defcustom org-export-latex-classes
  '(("article"
     "\\documentclass[11pt]{article}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ("report"
     "\\documentclass[11pt]{report}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ("book"
     "\\documentclass[11pt]{book}"
     ("\\part{%s}" . "\\part*{%s}")
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
    ("beamer"
     "\\documentclass{beamer}"
     org-beamer-sectioning
     ))
  "Alist of LaTeX classes and associated header and structure.
If #+LaTeX_CLASS is set in the buffer, use its value and the
associated information.  Here is the structure of each cell:

  \(class-name
    header-string
    (numbered-section . unnumbered-section\)
    ...\)

The header string
-----------------

The HEADER-STRING is the header that will be inserted into the LaTeX file.
It should contain the \\documentclass macro, and anything else that is needed
for this setup.  To this header, the following commands will be added:

- Calls to \\usepackage for all packages mentioned in the variables
  `org-export-latex-default-packages-alist' and
  `org-export-latex-packages-alist'.  Thus, your header definitions should
  avoid to also request these packages.

- Lines specified via \"#+LaTeX_HEADER:\"

If you need more control about the sequence in which the header is built
up, or if you want to exclude one of these building blocks for a particular
class, you can use the following macro-like placeholders.

 [DEFAULT-PACKAGES]      \\usepackage statements for default packages
 [NO-DEFAULT-PACKAGES]   do not include any of the default packages
 [PACKAGES]              \\usepackage statements for packages
 [NO-PACKAGES]           do not include the packages
 [EXTRA]                 the stuff from #+LaTeX_HEADER
 [NO-EXTRA]              do not include #+LaTeX_HEADER stuff
 [BEAMER-HEADER-EXTRA]   the beamer extra headers

So a header like

  \\documentclass{article}
  [NO-DEFAULT-PACKAGES]
  [EXTRA]
  \\providecommand{\\alert}[1]{\\textbf{#1}}
  [PACKAGES]

will omit the default packages, and will include the #+LaTeX_HEADER lines,
then have a call to \\providecommand, and then place \\usepackage commands
based on the content of `org-export-latex-packages-alist'.

If your header or `org-export-latex-default-packages-alist' inserts
\"\\usepackage[AUTO]{inputenc}\", AUTO will automatically be replaced with
a coding system derived from `buffer-file-coding-system'.  See also the
variable `org-export-latex-inputenc-alist' for a way to influence this
mechanism.

The sectioning structure
------------------------

The sectioning structure of the class is given by the elements following
the header string.  For each sectioning level, a number of strings is
specified.  A %s formatter is mandatory in each section string and will
be replaced by the title of the section.

Instead of a cons cell (numbered . unnumbered), you can also provide a list
of 2 or 4 elements,

  (numbered-open numbered-close)

or

  (numbered-open numbered-close unnumbered-open unnumbered-close)

providing opening and closing strings for a LaTeX environment that should
represent the document section.  The opening clause should have a %s
to represent the section title.

Instead of a list of sectioning commands, you can also specify a
function name.  That function will be called with two parameters,
the (reduced) level of the headline, and the headline text.  The function
must return a cons cell with the (possibly modified) headline text, and the
sectioning list in the cdr."
  :group 'org-export-latex
  :type '(repeat
	  (list (string :tag "LaTeX class")
		(string :tag "LaTeX header")
		(repeat :tag "Levels" :inline t
			(choice
			 (cons :tag "Heading"
			       (string :tag "  numbered")
			       (string :tag "unnumbered"))
			 (list :tag "Environment"
			       (string :tag "Opening   (numbered)")
			       (string :tag "Closing   (numbered)")
			       (string :tag "Opening (unnumbered)")
			       (string :tag "Closing (unnumbered)"))
			 (function :tag "Hook computing sectioning"))))))

(defcustom org-export-latex-inputenc-alist nil
  "Alist of inputenc coding system names, and what should really be used.
For example, adding an entry

      (\"utf8\" . \"utf8x\")

will cause \\usepackage[utf8x]{inputenc} to be used for buffers that
are written as utf8 files."
  :group 'org-export-latex
  :version "24.1"
  :type '(repeat
	  (cons
	   (string :tag "Derived from buffer")
	   (string :tag "Use this instead"))))


(defcustom org-export-latex-emphasis-alist
  '(("*" "\\textbf{%s}" nil)
    ("/" "\\emph{%s}" nil)
    ("_" "\\underline{%s}" nil)
    ("+" "\\st{%s}" nil)
    ("=" "\\protectedtexttt" t)
    ("~" "\\verb" t))
  "Alist of LaTeX expressions to convert emphasis fontifiers.
Each element of the list is a list of three elements.
The first element is the character used as a marker for fontification.
The second element is a formatting string to wrap fontified text with.
If it is \"\\verb\", Org will automatically select a delimiter
character that is not in the string.  \"\\protectedtexttt\" will use \\texttt
to typeset and try to protect special characters.
The third element decides whether to protect converted text from other
conversions."
  :group 'org-export-latex
  :type 'alist)

(defcustom org-export-latex-title-command "\\maketitle"
  "The command used to insert the title just after \\begin{document}.
If this string contains the formatting specification \"%s\" then
it will be used as a formatting string, passing the title as an
argument."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-import-inbuffer-stuff nil
  "Non-nil means define TeX macros for Org's inbuffer definitions.
For example \orgTITLE for #+TITLE."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-latex-date-format
  "\\today"
  "Format string for \\date{...}."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-todo-keyword-markup "\\textbf{%s}"
  "Markup for TODO keywords, as a printf format.
This can be a single format for all keywords, a cons cell with separate
formats for not-done and done states, or an association list with setup
for individual keywords.  If a keyword shows up for which there is no
markup defined, the first one in the association list will be used."
  :group 'org-export-latex
  :type '(choice
	  (string :tag "Default")
	  (cons :tag "Distinguish undone and done"
		(string :tag "Not-DONE states")
		(string :tag "DONE states"))
	  (repeat :tag "Per keyword markup"
		  (cons
		   (string :tag "Keyword")
		   (string :tag "Markup")))))

(defcustom org-export-latex-tag-markup "\\textbf{%s}"
  "Markup for tags, as a printf format."
  :group 'org-export-latex
  :version "24.1"
  :type 'string)

(defcustom org-export-latex-timestamp-markup "\\textit{%s}"
  "A printf format string to be applied to time stamps."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-timestamp-inactive-markup "\\textit{%s}"
  "A printf format string to be applied to inactive time stamps."
  :group 'org-export-latex
  :version "24.1"
  :type 'string)

(defcustom org-export-latex-timestamp-keyword-markup "\\texttt{%s}"
  "A printf format string to be applied to time stamps."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-href-format "\\href{%s}{%s}"
  "A printf format string to be applied to href links.
The format must contain either two %s instances or just one.
If it contains two %s instances, the first will be filled with
the link, the second with the link description.  If it contains
only one, the %s will be filled with the link."
  :group 'org-export-latex
  :version "24.1"
  :type 'string)

(defcustom org-export-latex-hyperref-format "\\hyperref[%s]{%s}"
  "A printf format string to be applied to hyperref links.
The format must contain one or two %s instances.  The first one
will be filled with the link, the second with its description."
  :group 'org-export-latex
  :version "24.1"
  :type 'string)

(defcustom org-export-latex-footnote-separator "\\textsuperscript{,}\\,"
  "Text used to separate footnotes."
  :group 'org-export-latex
  :version "24.1"
  :type 'string)

(defcustom org-export-latex-quotes
  '(("fr" ("\\(\\s-\\|[[(]\\)\"" . "«~") ("\\(\\S-\\)\"" . "~»") ("\\(\\s-\\|(\\)'" . "'"))
    ("en" ("\\(\\s-\\|[[(]\\)\"" . "``") ("\\(\\S-\\)\"" . "''") ("\\(\\s-\\|(\\)'" . "`")))
  "Alist for quotes to use when converting english double-quotes.

The CAR of each item in this alist is the language code.
The CDR of each item in this alist is a list of three CONS:
- the first CONS defines the opening quote;
- the second CONS defines the closing quote;
- the last CONS defines single quotes.

For each item in a CONS, the first string is a regexp
for allowed characters before/after the quote, the second
string defines the replacement string for this quote."
  :group 'org-export-latex
  :version "24.1"
  :type '(list
	  (cons :tag "Opening quote"
		(string :tag "Regexp for char before")
		(string :tag "Replacement quote     "))
	  (cons :tag "Closing quote"
		(string :tag "Regexp for char after ")
		(string :tag "Replacement quote     "))
	  (cons :tag "Single quote"
		(string :tag "Regexp for char before")
		(string :tag "Replacement quote     "))))

(defcustom org-export-latex-tables-verbatim nil
  "When non-nil, tables are exported verbatim."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-latex-tables-centered t
  "When non-nil, tables are exported in a center environment."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-latex-table-caption-above t
  "When non-nil, the caption is set above the table.  When nil,
the caption is set below the table."
  :group 'org-export-latex
  :version "24.1"
  :type 'boolean)

(defcustom org-export-latex-tables-column-borders nil
  "When non-nil, grouping columns can cause outer vertical lines in tables.
When nil, grouping causes only separation lines between groups."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-latex-low-levels 'itemize
  "How to convert sections below the current level of sectioning.
This is specified by the `org-export-headline-levels' option or the
value of \"H:\" in Org's #+OPTION line.

This can be either nil (skip the sections), `description', `itemize',
or `enumerate' (convert the sections as the corresponding list type), or
a string to be used instead of \\section{%s}.  In this latter case,
the %s stands here for the inserted headline and is mandatory.

It may also be a list of three string to define a user-defined environment
that should be used.  The first string should be the like
\"\\begin{itemize}\", the second should be like \"\\item %s %s\" with up
to two occurrences of %s for the title and a label, respectively.  The third
string should be like \"\\end{itemize\"."
  :group 'org-export-latex
  :type '(choice (const :tag "Ignore" nil)
		 (const :tag "Convert as descriptive list" description)
		 (const :tag "Convert as itemized list" itemize)
		 (const :tag "Convert as enumerated list" enumerate)
		 (list  :tag "User-defined environment"
			:value ("\\begin{itemize}" "\\end{itemize}" "\\item %s")
			(string :tag "Start")
			(string :tag "End")
			(string :tag "item"))
		 (string :tag "Use a section string" :value "\\subparagraph{%s}")))

(defcustom org-export-latex-list-parameters
  '(:cbon "$\\boxtimes$" :cboff "$\\Box$" :cbtrans "$\\boxminus$")
  "Parameters for the LaTeX list exporter.
These parameters will be passed on to `org-list-to-latex', which in turn
will pass them (combined with the LaTeX default list parameters) to
`org-list-to-generic'."
  :group 'org-export-latex
  :type 'plist)

(defcustom org-export-latex-verbatim-wrap
  '("\\begin{verbatim}\n" . "\\end{verbatim}")
  "Environment to be wrapped around a fixed-width section in LaTeX export.
This is a cons with two strings, to be added before and after the
fixed-with text.

Defaults to \\begin{verbatim} and \\end{verbatim}."
  :group 'org-export-translation
  :group 'org-export-latex
  :type '(cons (string :tag "Open")
	       (string :tag "Close")))

(defcustom org-export-latex-listings nil
  "Non-nil means export source code using the listings package.
This package will fontify source code, possibly even with color.
If you want to use this, you also need to make LaTeX use the
listings package, and if you want to have color, the color
package.  Just add these to `org-export-latex-packages-alist',
for example using customize, or with something like

  (require 'org-latex)
  (add-to-list 'org-export-latex-packages-alist '(\"\" \"listings\"))
  (add-to-list 'org-export-latex-packages-alist '(\"\" \"color\"))

Alternatively,

  (setq org-export-latex-listings 'minted)

causes source code to be exported using the minted package as
opposed to listings.  If you want to use minted, you need to add
the minted package to `org-export-latex-packages-alist', for
example using customize, or with

  (require 'org-latex)
  (add-to-list 'org-export-latex-packages-alist '(\"\" \"minted\"))

In addition, it is necessary to install
pygments (http://pygments.org), and to configure the variable
`org-latex-to-pdf-process' so that the -shell-escape option is
passed to pdflatex.
"
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-latex-listings-langs
  '((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp")
    (c "C") (cc "C++")
    (fortran "fortran")
    (perl "Perl") (cperl "Perl") (python "Python") (ruby "Ruby")
    (html "HTML") (xml "XML")
    (tex "TeX") (latex "TeX")
    (shell-script "bash")
    (gnuplot "Gnuplot")
    (ocaml "Caml") (caml "Caml")
    (sql "SQL") (sqlite "sql"))
  "Alist mapping languages to their listing language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language parameter
for the listings package.  If the mode name and the listings name are
the same, the language does not need an entry in this list - but it does not
hurt if it is present."
  :group 'org-export-latex
  :type '(repeat
	  (list
	   (symbol :tag "Major mode       ")
	   (string :tag "Listings language"))))

(defcustom org-export-latex-listings-w-names t
  "Non-nil means export names of named code blocks.
Code blocks exported with the listings package (controlled by the
`org-export-latex-listings' variable) can be named in the style
of noweb."
  :group 'org-export-latex
  :version "24.1"
  :type 'boolean)

(defcustom org-export-latex-minted-langs
  '((emacs-lisp "common-lisp")
    (cc "c++")
    (cperl "perl")
    (shell-script "bash")
    (caml "ocaml"))
  "Alist mapping languages to their minted language counterpart.
The key is a symbol, the major mode symbol without the \"-mode\".
The value is the string that should be inserted as the language parameter
for the minted package.  If the mode name and the listings name are
the same, the language does not need an entry in this list - but it does not
hurt if it is present.

Note that minted uses all lower case for language identifiers,
and that the full list of language identifiers can be obtained
with:
pygmentize -L lexers
"
  :group 'org-export-latex
  :version "24.1"
  :type '(repeat
	  (list
	   (symbol :tag "Major mode       ")
	   (string :tag "Listings language"))))

(defcustom org-export-latex-listings-options nil
  "Association list of options for the latex listings package.

These options are supplied as a comma-separated list to the
\\lstset command. Each element of the association list should be
a list containing two strings: the name of the option, and the
value. For example,

  (setq org-export-latex-listings-options
    '((\"basicstyle\" \"\\small\")
      (\"keywordstyle\" \"\\color{black}\\bfseries\\underbar\")))

will typeset the code in a small size font with underlined, bold
black keywords.

Note that the same options will be applied to blocks of all
languages."
  :group 'org-export-latex
  :version "24.1"
  :type '(repeat
	  (list
	   (string :tag "Listings option name ")
	   (string :tag "Listings option value"))))

(defcustom org-export-latex-minted-options nil
  "Association list of options for the latex minted package.

These options are supplied within square brackets in
\\begin{minted} environments. Each element of the alist should be
a list containing two strings: the name of the option, and the
value. For example,

  (setq org-export-latex-minted-options
    '((\"bgcolor\" \"bg\") (\"frame\" \"lines\")))

will result in src blocks being exported with

\\begin{minted}[bgcolor=bg,frame=lines]{<LANG>}

as the start of the minted environment. Note that the same
options will be applied to blocks of all languages."
  :group 'org-export-latex
  :version "24.1"
  :type '(repeat
	  (list
	   (string :tag "Minted option name ")
	   (string :tag "Minted option value"))))

(defvar org-export-latex-custom-lang-environments nil
  "Association list mapping languages to language-specific latex
  environments used during export of src blocks by the listings
  and minted latex packages. For example,

  (setq org-export-latex-custom-lang-environments
     '((python \"pythoncode\")))

  would have the effect that if org encounters begin_src python
  during latex export it will output

  \\begin{pythoncode}
  <src block body>
  \\end{pythoncode}")

(defcustom org-export-latex-remove-from-headlines
  '(:todo nil :priority nil :tags nil)
  "A plist of keywords to remove from headlines.  OBSOLETE.
Non-nil means remove this keyword type from the headline.

Don't remove the keys, just change their values.

Obsolete, this variable is no longer used.  Use the separate
variables `org-export-with-todo-keywords', `org-export-with-priority',
and `org-export-with-tags' instead."
  :type 'plist
  :group 'org-export-latex)

(defcustom org-export-latex-image-default-option "width=.9\\linewidth"
  "Default option for images."
  :group 'org-export-latex
  :type 'string)

(defcustom org-latex-default-figure-position "htb"
  "Default position for latex figures."
  :group 'org-export-latex
  :version "24.1"
  :type 'string)

(defcustom org-export-latex-tabular-environment "tabular"
  "Default environment used to build tables."
  :group 'org-export-latex
  :version "24.1"
  :type 'string)

(defcustom org-export-latex-inline-image-extensions
  '("pdf" "jpeg" "jpg" "png" "ps" "eps")
  "Extensions of image files that can be inlined into LaTeX.
Note that the image extension *actually* allowed depend on the way the
LaTeX file is processed.  When used with pdflatex, pdf, jpg and png images
are OK.  When processing through dvi to Postscript, only ps and eps are
allowed.  The default we use here encompasses both."
  :group 'org-export-latex
  :type '(repeat (string :tag "Extension")))

(defcustom org-export-latex-coding-system nil
  "Coding system for the exported LaTeX file."
  :group 'org-export-latex
  :type 'coding-system)

(defgroup org-export-pdf nil
  "Options for exporting Org-mode files to PDF, via LaTeX."
  :tag "Org Export PDF"
  :group 'org-export-latex
  :group 'org-export)

(defcustom org-latex-to-pdf-process
  '("pdflatex -interaction nonstopmode -output-directory %o %f"
    "pdflatex -interaction nonstopmode -output-directory %o %f"
    "pdflatex -interaction nonstopmode -output-directory %o %f")
  "Commands to process a LaTeX file to a PDF file.
This is a list of strings, each of them will be given to the shell
as a command.  %f in the command will be replaced by the full file name, %b
by the file base name (i.e. without extension) and %o by the base directory
of the file.

The reason why this is a list is that it usually takes several runs of
`pdflatex', maybe mixed with a call to `bibtex'.  Org does not have a clever
mechanism to detect which of these commands have to be run to get to a stable
result, and it also does not do any error checking.

By default, Org uses 3 runs of `pdflatex' to do the processing.  If you
have texi2dvi on your system and if that does not cause the infamous
egrep/locale bug:

     http://lists.gnu.org/archive/html/bug-texinfo/2010-03/msg00031.html

then `texi2dvi' is the superior choice.  Org does offer it as one
of the customize options.

Alternatively, this may be a Lisp function that does the processing, so you
could use this to apply the machinery of AUCTeX or the Emacs LaTeX mode.
This function should accept the file name as its single argument."
  :group 'org-export-pdf
  :type '(choice
	  (repeat :tag "Shell command sequence"
		  (string :tag "Shell command"))
	  (const :tag "2 runs of pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "3 runs of pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "pdflatex,bibtex,pdflatex,pdflatex"
		 ("pdflatex -interaction nonstopmode -output-directory %o %f"
		   "bibtex %b"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"
		   "pdflatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "2 runs of xelatex"
		 ("xelatex -interaction nonstopmode -output-directory %o %f"
		   "xelatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "3 runs of xelatex"
		 ("xelatex -interaction nonstopmode -output-directory %o %f"
		   "xelatex -interaction nonstopmode -output-directory %o %f"
		   "xelatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "xelatex,bibtex,xelatex,xelatex"
		 ("xelatex -interaction nonstopmode -output-directory %o %f"
		   "bibtex %b"
		   "xelatex -interaction nonstopmode -output-directory %o %f"
		   "xelatex -interaction nonstopmode -output-directory %o %f"))
	  (const :tag "texi2dvi"
		 ("texi2dvi -p -b -c -V %f"))
	  (const :tag "rubber"
		 ("rubber -d --into %o %f"))
	  (function)))

(defcustom org-export-pdf-logfiles
  '("aux" "idx" "log" "out" "toc" "nav" "snm" "vrb")
  "The list of file extensions to consider as LaTeX logfiles."
  :group 'org-export-pdf
  :version "24.1"
  :type '(repeat (string :tag "Extension")))

(defcustom org-export-pdf-remove-logfiles t
  "Non-nil means remove the logfiles produced by PDF production.
These are the .aux, .log, .out, and .toc files."
  :group 'org-export-pdf
  :type 'boolean)

;;; Hooks

(defvar org-export-latex-after-initial-vars-hook nil
  "Hook run before LaTeX export.
The exact moment is after the initial variables like org-export-latex-class
have been determined from the environment.")

(defvar org-export-latex-after-blockquotes-hook nil
  "Hook run during LaTeX export, after blockquote, verse, center are done.")

(defvar org-export-latex-final-hook nil
  "Hook run in the finalized LaTeX buffer.")

(defvar org-export-latex-after-save-hook nil
  "Hook run in the finalized LaTeX buffer, after it has been saved.")

;;; Autoload functions:

;;;###autoload
(defun org-export-as-latex-batch ()
  "Call `org-export-as-latex', may be used in batch processing.
For example:

emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-latex-batch"
  (org-export-as-latex org-export-headline-levels 'hidden))

;;;###autoload
(defun org-export-as-latex-to-buffer (arg)
  "Call `org-export-as-latex` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-latex'."
  (interactive "P")
  (org-export-as-latex arg nil nil "*Org LaTeX Export*")
  (when org-export-show-temporary-export-buffer
    (switch-to-buffer-other-window "*Org LaTeX Export*")))

;;;###autoload
(defun org-replace-region-by-latex (beg end)
  "Replace the region from BEG to END with its LaTeX export.
It assumes the region has `org-mode' syntax, and then convert it to
LaTeX.  This can be used in any buffer.  For example, you could
write an itemized list in `org-mode' syntax in an LaTeX buffer and
then use this command to convert it."
  (interactive "r")
  (let (reg latex buf)
    (save-window-excursion
      (if (eq major-mode 'org-mode)
	  (setq latex (org-export-region-as-latex
		       beg end t 'string))
	(setq reg (buffer-substring beg end)
	      buf (get-buffer-create "*Org tmp*"))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert reg)
	  (org-mode)
	  (setq latex (org-export-region-as-latex
		       (point-min) (point-max) t 'string)))
	(kill-buffer buf)))
    (delete-region beg end)
    (insert latex)))

;;;###autoload
(defun org-export-region-as-latex (beg end &optional body-only buffer)
  "Convert region from BEG to END in `org-mode' buffer to LaTeX.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted LaTeX.  If BUFFER is the symbol `string', return the
produced LaTeX as a string and leave no buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq latex (org-export-region-as-latex beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer."
  (interactive "r\nP")
  (when (org-called-interactively-p 'any)
    (setq buffer "*Org LaTeX Export*"))
  (let ((transient-mark-mode t) (zmacs-regions t)
	ext-plist rtn)
    (setq ext-plist (plist-put ext-plist :ignore-subtree-p t))
    (goto-char end)
    (set-mark (point)) ;; to activate the region
    (goto-char beg)
    (setq rtn (org-export-as-latex
	       nil nil ext-plist
	       buffer body-only))
    (if (fboundp 'deactivate-mark) (deactivate-mark))
    (if (and (org-called-interactively-p 'any) (bufferp rtn))
	(switch-to-buffer-other-window rtn)
      rtn)))

;;;###autoload
(defun org-export-as-latex (arg &optional hidden ext-plist
				to-buffer body-only pub-dir)
  "Export current buffer to a LaTeX file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will be exported
depending on `org-export-latex-low-levels'.  The default is to
convert them as description lists.
HIDDEN is obsolete and does nothing.
EXT-PLIST is a property list with
external parameters overriding org-mode's default settings, but
still inferior to file-local settings.  When TO-BUFFER is
non-nil, create a buffer with that name and export to that
buffer.  If TO-BUFFER is the symbol `string', don't leave any
buffer behind but just return the resulting LaTeX as a string.
When BODY-ONLY is set, don't produce the file header and footer,
simply return the content of \\begin{document}...\\end{document},
without even the \\begin{document} and \\end{document} commands.
when PUB-DIR is set, use this as the publishing directory."
  (interactive "P")
  (when (and (not body-only) arg (listp arg)) (setq body-only t))
  (run-hooks 'org-export-first-hook)

  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
	     (not buffer-file-name))
    (if (buffer-base-buffer)
	(org-set-local 'buffer-file-name
		       (with-current-buffer (buffer-base-buffer)
			 buffer-file-name))
      (error "Need a file name to be able to export")))

  (message "Exporting to LaTeX...")
  (org-unmodified
   (let ((inhibit-read-only t))
     (remove-text-properties (point-min) (point-max)
			     '(:org-license-to-kill nil))))
  (org-update-radio-target-regexp)
  (org-export-latex-set-initial-vars ext-plist arg)
  (setq org-export-opt-plist org-export-latex-options-plist
	org-export-footnotes-data (org-footnote-all-labels 'with-defs)
	org-export-footnotes-seen nil
	org-export-latex-footmark-seen nil)
  (org-install-letbind)
  (run-hooks 'org-export-latex-after-initial-vars-hook)
  (let* ((wcf (current-window-configuration))
	 (opt-plist
	  (org-export-process-option-filters org-export-latex-options-plist))
	 (region-p (org-region-active-p))
	 (rbeg (and region-p (region-beginning)))
	 (rend (and region-p (region-end)))
	 (subtree-p
	  (if (plist-get opt-plist :ignore-subtree-p)
	      nil
	    (when region-p
	      (save-excursion
		(goto-char rbeg)
		(and (org-at-heading-p)
		     (>= (org-end-of-subtree t t) rend))))))
	 (opt-plist (setq org-export-opt-plist
			  (if subtree-p
			      (org-export-add-subtree-options opt-plist rbeg)
			    opt-plist)))
	 ;; Make sure the variable contains the updated values.
	 (org-export-latex-options-plist (setq org-export-opt-plist opt-plist))
	 ;; The following two are dynamically scoped into other
	 ;; routines below.
	 (org-current-export-dir
	  (or pub-dir (org-export-directory :html opt-plist)))
	 (org-current-export-file buffer-file-name)
	 (title (or (and subtree-p (org-export-get-title-from-subtree))
		    (plist-get opt-plist :title)
		    (and (not
			  (plist-get opt-plist :skip-before-1st-heading))
			 (org-export-grab-title-from-buffer))
		    (and buffer-file-name
			 (file-name-sans-extension
			  (file-name-nondirectory buffer-file-name)))
		    "No Title"))
	 (filename
	  (and (not to-buffer)
	       (concat
		(file-name-as-directory
		 (or pub-dir
		     (org-export-directory :LaTeX org-export-latex-options-plist)))
		(file-name-sans-extension
		 (or (and subtree-p
			  (org-entry-get rbeg "EXPORT_FILE_NAME" t))
		     (file-name-nondirectory ;sans-extension
		      (or buffer-file-name
			  (error "Don't know which export file to use")))))
		".tex")))
	 (filename
	  (and filename
	       (if (equal (file-truename filename)
			  (file-truename (or buffer-file-name "dummy.org")))
		   (concat filename ".tex")
		 filename)))
	 (auto-insert nil); Avoid any auto-insert stuff for the new file
	 (TeX-master (boundp 'TeX-master))
	 (buffer (if to-buffer
		     (cond
		      ((eq to-buffer 'string) (get-buffer-create
					       "*Org LaTeX Export*"))
		      (t (get-buffer-create to-buffer)))
		   (find-file-noselect filename)))
	 (odd org-odd-levels-only)
	 (header (org-export-latex-make-header title opt-plist))
	 (skip (cond (subtree-p nil)
		     (region-p nil)
		     (t (plist-get opt-plist :skip-before-1st-heading))))
	 (text (plist-get opt-plist :text))
	 (org-export-preprocess-hook
	  (cons
	   `(lambda () (org-set-local 'org-complex-heading-regexp
				      ,org-export-latex-complex-heading-re))
	   org-export-preprocess-hook))
	 (first-lines (if skip "" (org-export-latex-first-lines
				   opt-plist
				   (if subtree-p
				       (save-excursion
					 (goto-char rbeg)
					 (point-at-bol 2))
				     rbeg)
				   (if region-p rend))))
	 (coding-system (and (boundp 'buffer-file-coding-system)
			     buffer-file-coding-system))
	 (coding-system-for-write (or org-export-latex-coding-system
				      coding-system))
	 (save-buffer-coding-system (or org-export-latex-coding-system
					coding-system))
	 (region (buffer-substring
		  (if region-p (region-beginning) (point-min))
		  (if region-p (region-end) (point-max))))
	 (text
	  (and text (string-match "\\S-" text)
	       (org-export-preprocess-string
		text
		:emph-multiline t
		:for-backend 'latex
		:comments nil
		:tags (plist-get opt-plist :tags)
		:priority (plist-get opt-plist :priority)
		:footnotes (plist-get opt-plist :footnotes)
		:drawers (plist-get opt-plist :drawers)
		:timestamps (plist-get opt-plist :timestamps)
		:todo-keywords (plist-get opt-plist :todo-keywords)
		:tasks (plist-get opt-plist :tasks)
		:add-text nil
		:skip-before-1st-heading skip
		:select-tags nil
		:exclude-tags nil
		:LaTeX-fragments nil)))
	 (string-for-export
	  (org-export-preprocess-string
	   region
	   :emph-multiline t
	   :for-backend 'latex
	   :comments nil
	   :tags (plist-get opt-plist :tags)
	   :priority (plist-get opt-plist :priority)
	   :footnotes (plist-get opt-plist :footnotes)
	   :drawers (plist-get opt-plist :drawers)
	   :timestamps (plist-get opt-plist :timestamps)
	   :todo-keywords (plist-get opt-plist :todo-keywords)
	   :tasks (plist-get opt-plist :tasks)
	   :add-text (if (eq to-buffer 'string) nil text)
	   :skip-before-1st-heading skip
	   :select-tags (plist-get opt-plist :select-tags)
	   :exclude-tags (plist-get opt-plist :exclude-tags)
	   :LaTeX-fragments nil)))

    (set-buffer buffer)
    (erase-buffer)
    (org-install-letbind)

    (and (fboundp 'set-buffer-file-coding-system)
	 (set-buffer-file-coding-system coding-system-for-write))

    ;; insert the header and initial document commands
    (unless (or (eq to-buffer 'string) body-only)
      (insert header))

    ;; insert text found in #+TEXT
    (when (and text (not (eq to-buffer 'string)))
      (insert (org-export-latex-content
	       text '(lists tables fixed-width keywords))
	       "\n\n"))

    ;; insert lines before the first headline
    (unless (or skip (string-match "^\\*" first-lines))
      (insert first-lines))

    ;; export the content of headlines
    (org-export-latex-global
     (with-temp-buffer
       (insert string-for-export)
       (goto-char (point-min))
       (when (re-search-forward "^\\(\\*+\\) " nil t)
	 (let* ((asters (length (match-string 1)))
		(level (if odd (- asters 2) (- asters 1))))
	   (setq org-export-latex-add-level
		 (if odd (1- (/ (1+ asters) 2)) (1- asters)))
	   (org-export-latex-parse-global level odd)))))

    ;; finalization
    (unless body-only (insert "\n\\end{document}"))

    ;; Attach description terms to the \item macro
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*\\\\item\\([ \t]+\\)\\[" nil t)
      (delete-region (match-beginning 1) (match-end 1)))

    ;; Relocate the table of contents
    (goto-char (point-min))
    (when (re-search-forward "\\[TABLE-OF-CONTENTS\\]" nil t)
      (goto-char (point-min))
      (while (re-search-forward "\\\\tableofcontents\\>[ \t]*\n?" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (and (re-search-forward "\\[TABLE-OF-CONTENTS\\]" nil t)
	   (replace-match "\\tableofcontents" t t)))

    ;; Cleanup forced line ends in items where they are not needed
    (goto-char (point-min))
    (while (re-search-forward
	    "^[ \t]*\\\\item\\>.*\\(\\\\\\\\\\)[ \t]*\\(\n\\\\label.*\\)*\n\\\\begin"
	    nil t)
      (delete-region (match-beginning 1) (match-end 1)))
    (goto-char (point-min))
    (while (re-search-forward
	    "^[ \t]*\\\\item\\>.*\\(\\\\\\\\\\)[ \t]*\\(\n\\\\label.*\\)*"
	    nil t)
      (if (looking-at "[\n \t]+")
	  (replace-match "\n")))

    (run-hooks 'org-export-latex-final-hook)
    (if to-buffer
	(unless (eq major-mode 'latex-mode) (latex-mode))
      (save-buffer))
    (org-export-latex-fix-inputenc)
    (run-hooks 'org-export-latex-after-save-hook)
    (goto-char (point-min))
    (or (org-export-push-to-kill-ring "LaTeX")
	(message "Exporting to LaTeX...done"))
    (prog1
	(if (eq to-buffer 'string)
	    (prog1 (buffer-substring (point-min) (point-max))
	      (kill-buffer (current-buffer)))
	  (current-buffer))
      (set-window-configuration wcf))))

;;;###autoload
(defun org-export-as-pdf (arg &optional hidden ext-plist
			      to-buffer body-only pub-dir)
  "Export as LaTeX, then process through to PDF."
  (interactive "P")
  (message "Exporting to PDF...")
  (let* ((wconfig (current-window-configuration))
	 (lbuf (org-export-as-latex arg hidden ext-plist
				    to-buffer body-only pub-dir))
	 (file (buffer-file-name lbuf))
	 (base (file-name-sans-extension (buffer-file-name lbuf)))
	 (pdffile (concat base ".pdf"))
	 (cmds (if (eq org-export-latex-listings 'minted)
		   ;; automatically add -shell-escape when needed
		   (mapcar (lambda (cmd)
			     (replace-regexp-in-string
			      "pdflatex " "pdflatex -shell-escape " cmd))
			   org-latex-to-pdf-process)
		 org-latex-to-pdf-process))
	 (outbuf (get-buffer-create "*Org PDF LaTeX Output*"))
	 (bibtex-p (with-current-buffer lbuf
		     (save-excursion
		       (goto-char (point-min))
		       (re-search-forward "\\\\bibliography{" nil t))))
	 cmd output-dir errors)
    (with-current-buffer outbuf (erase-buffer))
    (message (concat "Processing LaTeX file " file "..."))
    (setq output-dir (file-name-directory file))
    (with-current-buffer lbuf
      (save-excursion
	(if (and cmds (symbolp cmds))
	    (funcall cmds (shell-quote-argument file))
	  (while cmds
	    (setq cmd (pop cmds))
	    (while (string-match "%b" cmd)
	      (setq cmd (replace-match
			 (save-match-data
			   (shell-quote-argument base))
			 t t cmd)))
	    (while (string-match "%f" cmd)
	      (setq cmd (replace-match
			 (save-match-data
			   (shell-quote-argument file))
			 t t cmd)))
	    (while (string-match "%o" cmd)
	      (setq cmd (replace-match
			 (save-match-data
			   (shell-quote-argument output-dir))
			 t t cmd)))
	    (shell-command cmd outbuf)))))
    (message (concat "Processing LaTeX file " file "...done"))
    (setq errors (org-export-latex-get-error outbuf))
    (if (not (file-exists-p pdffile))
	(error (concat "PDF file " pdffile " was not produced"
		       (if errors (concat ":" errors "") "")))
      (set-window-configuration wconfig)
      (when org-export-pdf-remove-logfiles
	(dolist (ext org-export-pdf-logfiles)
	  (setq file (concat base "." ext))
	  (and (file-exists-p file) (delete-file file))))
      (message (concat
		"Exporting to PDF...done"
		(if errors
		    (concat ", with some errors:" errors)
		  "")))
      pdffile)))

(defun org-export-latex-get-error (buf)
  "Collect the kinds of errors that remain in pdflatex processing."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward "^[ \t]*This is pdf.*?TeX.*?Version" nil t)
	;; OK, we are at the location of the final run
	(let ((pos (point)) (errors "") (case-fold-search t))
	  (if (re-search-forward "Reference.*?undefined" nil t)
	      (setq errors (concat errors " [undefined reference]")))
	  (goto-char pos)
	  (if (re-search-forward "Citation.*?undefined" nil t)
	      (setq errors (concat errors " [undefined citation]")))
	  (goto-char pos)
	  (if (re-search-forward "Undefined control sequence" nil t)
	      (setq errors (concat errors " [undefined control sequence]")))
	  (and (org-string-nw-p errors) errors))))))

;;;###autoload
(defun org-export-as-pdf-and-open (arg)
  "Export as LaTeX, then process through to PDF, and open."
  (interactive "P")
  (let ((pdffile (org-export-as-pdf arg)))
    (if pdffile
	(progn
	  (org-open-file pdffile)
	  (when org-export-kill-product-buffer-when-displayed
	    (kill-buffer (find-buffer-visiting
			  (concat (file-name-sans-extension (buffer-file-name))
				  ".tex")))))
      (error "PDF file was not produced"))))

;;; Parsing functions:

(defun org-export-latex-parse-global (level odd)
  "Parse the current buffer recursively, starting at LEVEL.
If ODD is non-nil, assume the buffer only contains odd sections.
Return a list reflecting the document structure."
  (save-excursion
    (goto-char (point-min))
    (let* ((cnt 0) output
	   (depth org-export-latex-sectioning-depth))
      (while (org-re-search-forward-unprotected
	      (concat "^\\(\\(?:\\*\\)\\{"
		      (number-to-string (+ (if odd 2 1) level))
		      "\\}\\) \\(.*\\)$")
	      ;; make sure that there is no upper heading
	      (when (> level 0)
		(save-excursion
		  (save-match-data
		    (org-re-search-forward-unprotected
		     (concat "^\\(\\(?:\\*\\)\\{"
			     (number-to-string level)
			     "\\}\\) \\(.*\\)$") nil t)))) t)
	(setq cnt (1+ cnt))
	(let* ((pos (match-beginning 0))
	       (heading (match-string 2))
	       (nlevel (if odd (/ (+ 3 level) 2) (1+ level))))
	  (save-excursion
	    (narrow-to-region
	     (point)
	     (save-match-data
	       (if (org-re-search-forward-unprotected
		    (concat "^\\(\\(?:\\*\\)\\{"
			    (number-to-string (+ (if odd 2 1) level))
			    "\\}\\) \\(.*\\)$") nil t)
		   (match-beginning 0)
		 (point-max))))
	    (goto-char (point-min))
	    (setq output
		  (append output
			  (list
			   (list
			    `(pos . ,pos)
			    `(level . ,nlevel)
			    `(occur . ,cnt)
			    `(heading . ,heading)
			    `(content . ,(org-export-latex-parse-content))
			    `(subcontent . ,(org-export-latex-parse-subcontent
					     level odd)))))))
	  (widen)))
      (list output))))

(defun org-export-latex-parse-content ()
  "Extract the content of a section."
  (let ((beg (point))
	(end (if (org-re-search-forward-unprotected "^\\(\\*\\)+ .*$" nil t)
		 (progn (beginning-of-line) (point))
	       (point-max))))
    (buffer-substring beg end)))

(defun org-export-latex-parse-subcontent (level odd)
  "Extract the subcontent of a section at LEVEL.
If ODD Is non-nil, assume subcontent only contains odd sections."
  (if (not (org-re-search-forward-unprotected
	    (concat "^\\(\\(?:\\*\\)\\{"
		    (number-to-string (+ (if odd 4 2) level))
		    "\\}\\) \\(.*\\)$")
	    nil t))
      nil ; subcontent is nil
    (org-export-latex-parse-global (+ (if odd 2 1) level) odd)))

;;; Rendering functions:
(defun org-export-latex-global (content)
  "Export CONTENT to LaTeX.
CONTENT is an element of the list produced by
`org-export-latex-parse-global'."
  (if (eq (car content) 'subcontent)
      (mapc 'org-export-latex-sub (cdr content))
    (org-export-latex-sub (car content))))

(defun org-export-latex-sub (subcontent)
  "Export the list SUBCONTENT to LaTeX.
SUBCONTENT is an alist containing information about the headline
and its content."
  (let ((num (plist-get org-export-latex-options-plist :section-numbers)))
    (mapc (lambda(x) (org-export-latex-subcontent x num)) subcontent)))

(defun org-export-latex-subcontent (subcontent num)
  "Export each cell of SUBCONTENT to LaTeX.
If NUM is non-nil export numbered sections, otherwise use unnumbered
sections.  If NUM is an integer, export the highest NUM levels as
numbered sections and lower levels as unnumbered sections."
  (let* ((heading (cdr (assoc 'heading subcontent)))
	 (level (- (cdr (assoc 'level subcontent))
		   org-export-latex-add-level))
	 (occur (number-to-string (cdr (assoc 'occur subcontent))))
	 (content (cdr (assoc 'content subcontent)))
	 (subcontent (cadr (assoc 'subcontent subcontent)))
	 (label (org-get-text-property-any 0 'target heading))
	 (label-list (cons label (cdr (assoc label
					     org-export-target-aliases))))
	 (sectioning org-export-latex-sectioning)
	 (depth org-export-latex-sectioning-depth)
	 main-heading sub-heading ctnt)
    (when (symbolp (car sectioning))
      (setq sectioning (funcall (car sectioning) level heading))
      (when sectioning
	(setq heading (car sectioning)
	      sectioning (cdr sectioning)
	      ;; target property migh have changed...
	      label (org-get-text-property-any 0 'target heading)
	      label-list (cons label (cdr (assoc label
						 org-export-target-aliases)))))
      (if sectioning (setq sectioning (make-list 10 sectioning)))
      (setq depth (if sectioning 10000 0)))
    (if (string-match "[ \t]*\\\\\\\\[ \t]*" heading)
	(setq main-heading (substring heading 0 (match-beginning 0))
	      sub-heading (substring heading (match-end 0))))
    (setq heading (org-export-latex-fontify-headline heading)
	  sub-heading (and sub-heading
			   (org-export-latex-fontify-headline sub-heading))
	  main-heading (and main-heading
			    (org-export-latex-fontify-headline main-heading)))
    (cond
     ;; Normal conversion
     ((<= level depth)
      (let* ((sec (nth (1- level) sectioning))
	     (num (if (integerp num)
		      (>= num level)
		    num))
	     start end)
	(if (consp (cdr sec))
	    (setq start (nth (if num 0 2) sec)
		  end (nth (if num 1 3) sec))
	  (setq start (if num (car sec) (cdr sec))))
	(insert (format start (if main-heading main-heading heading)
			(or sub-heading "")))
	(insert	"\n")
	(when label
	  (insert (mapconcat (lambda (l) (format "\\label{%s}" l))
			     label-list "\n") "\n"))
	(insert (org-export-latex-content content))
	(cond ((stringp subcontent) (insert subcontent))
	      ((listp subcontent)
	       (while (org-looking-back "\n\n") (backward-delete-char 1))
	       (org-export-latex-sub subcontent)))
	(when (and end (string-match "[^ \t]" end))
	  (let ((hook (org-get-text-property-any 0 'org-insert-hook end)))
	    (and (functionp hook) (funcall hook)))
	  (insert end "\n"))))
     ;; At a level under the hl option: we can drop this subsection
     ((> level depth)
      (cond ((eq org-export-latex-low-levels 'description)
	     (if (string-match "% ends low level$"
			       (buffer-substring (point-at-bol 0) (point)))
		 (delete-region (point-at-bol 0) (point))
	       (insert "\\begin{description}\n"))
	     (insert (format "\n\\item[%s]%s~\n"
			     heading
			     (if label (format "\\label{%s}" label) "")))
	     (insert (org-export-latex-content content))
	     (cond ((stringp subcontent) (insert subcontent))
		   ((listp subcontent) (org-export-latex-sub subcontent)))
	     (insert "\\end{description} % ends low level\n"))
	    ((memq org-export-latex-low-levels '(itemize enumerate))
	     (if (string-match "% ends low level$"
			       (buffer-substring (point-at-bol 0) (point)))
		 (delete-region (point-at-bol 0) (point))
	       (insert (format "\\begin{%s}\n"
			       (symbol-name org-export-latex-low-levels))))
	     (let ((ctnt (org-export-latex-content content)))
	       (insert (format (if (not (equal (replace-regexp-in-string "\n" "" ctnt) ""))
				   "\n\\item %s\\\\\n%s%%"
				 "\n\\item %s\n%s%%")
			       heading
			       (if label (format "\\label{%s}" label) "")))
	       (insert ctnt))
	     (cond ((stringp subcontent) (insert subcontent))
		   ((listp subcontent) (org-export-latex-sub subcontent)))
	     (insert (format "\\end{%s} %% ends low level\n"
			     (symbol-name org-export-latex-low-levels))))

	    ((and (listp org-export-latex-low-levels)
		  org-export-latex-low-levels)
	     (if (string-match "% ends low level$"
			       (buffer-substring (point-at-bol 0) (point)))
		 (delete-region (point-at-bol 0) (point))
	       (insert (car org-export-latex-low-levels) "\n"))
	     (insert (format (nth 2 org-export-latex-low-levels)
			     heading
			     (if label (format "\\label{%s}" label) "")))
	     (insert (org-export-latex-content content))
	     (cond ((stringp subcontent) (insert subcontent))
		   ((listp subcontent) (org-export-latex-sub subcontent)))
	     (insert (nth 1 org-export-latex-low-levels)
		     " %% ends low level\n"))

	    ((stringp org-export-latex-low-levels)
	     (insert (format org-export-latex-low-levels heading) "\n")
	     (when label (insert (format "\\label{%s}\n" label)))
	     (insert (org-export-latex-content content))
	     (cond ((stringp subcontent) (insert subcontent))
		   ((listp subcontent) (org-export-latex-sub subcontent)))))))))

;;; Exporting internals:
(defun org-export-latex-set-initial-vars (ext-plist level)
  "Store org local variables required for LaTeX export.
EXT-PLIST is an optional additional plist.
LEVEL indicates the default depth for export."
  (setq org-export-latex-todo-keywords-1 org-todo-keywords-1
	org-export-latex-done-keywords org-done-keywords
	org-export-latex-not-done-keywords org-not-done-keywords
	org-export-latex-complex-heading-re org-complex-heading-regexp
	org-export-latex-display-custom-times org-display-custom-times
	org-export-latex-all-targets-re
	(org-make-target-link-regexp (org-all-targets))
	org-export-latex-options-plist
	(org-combine-plists (org-default-export-plist) ext-plist
			    (org-infile-export-plist))
	org-export-latex-class
	(or (and (org-region-active-p)
		 (save-excursion
		   (goto-char (region-beginning))
		   (and (looking-at org-complex-heading-regexp)
			(org-entry-get nil "LaTeX_CLASS" 'selective))))
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(and (re-search-forward "^#\\+LaTeX_CLASS:[ \t]*\\([-/a-zA-Z]+\\)" nil t)
		     (match-string 1))))
	    (plist-get org-export-latex-options-plist :latex-class)
	    org-export-latex-default-class)
	org-export-latex-class-options
	(or (and (org-region-active-p)
		 (save-excursion
		   (goto-char (region-beginning))
		   (and (looking-at org-complex-heading-regexp)
			(org-entry-get nil "LaTeX_CLASS_OPTIONS" 'selective))))
	    (save-excursion
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(and (re-search-forward "^#\\+LaTeX_CLASS_OPTIONS:[ \t]*\\(.*?\\)[ \t]*$" nil t)
		     (match-string 1))))
	    (plist-get org-export-latex-options-plist :latex-class-options))
	org-export-latex-class
	(or (car (assoc org-export-latex-class org-export-latex-classes))
	    (error "No definition for class `%s' in `org-export-latex-classes'"
		   org-export-latex-class))
	org-export-latex-header
	(cadr (assoc org-export-latex-class org-export-latex-classes))
	org-export-latex-sectioning
	(cddr (assoc org-export-latex-class org-export-latex-classes))
	org-export-latex-sectioning-depth
	(or level
	    (let ((hl-levels
		   (plist-get org-export-latex-options-plist :headline-levels))
		  (sec-depth (length org-export-latex-sectioning)))
	      (if (> hl-levels sec-depth) sec-depth hl-levels))))
  (when (and org-export-latex-class-options
	     (string-match "\\S-" org-export-latex-class-options)
	     (string-match "^[ \t]*\\(\\\\documentclass\\)\\(\\[.*?\\]\\)?"
			   org-export-latex-header))
    (setq org-export-latex-header
	  (concat (substring org-export-latex-header 0 (match-end 1))
		  org-export-latex-class-options
		  (substring org-export-latex-header (match-end 0))))))

(defvar org-export-latex-format-toc-function
  'org-export-latex-format-toc-default
  "The function formatting returning the string to create the table of contents.
The function mus take one parameter, the depth of the table of contents.")

(defun org-export-latex-make-header (title opt-plist)
  "Make the LaTeX header and return it as a string.
TITLE is the current title from the buffer or region.
OPT-PLIST is the options plist for current buffer."
  (let ((toc (plist-get opt-plist :table-of-contents))
	(author (org-export-apply-macros-in-string
		 (plist-get opt-plist :author)))
	(email (replace-regexp-in-string
		"_" "\\\\_"
		(org-export-apply-macros-in-string
		 (plist-get opt-plist :email))))
	(description (org-export-apply-macros-in-string
		      (plist-get opt-plist :description)))
	(keywords (org-export-apply-macros-in-string
		   (plist-get opt-plist :keywords))))
    (concat
     (if (plist-get opt-plist :time-stamp-file)
	 (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
     ;; insert LaTeX custom header and packages from the list
     (org-splice-latex-header
      (org-export-apply-macros-in-string org-export-latex-header)
      org-export-latex-default-packages-alist
      org-export-latex-packages-alist nil
      (org-export-apply-macros-in-string
       (plist-get opt-plist :latex-header-extra)))
     ;; append another special variable
     (org-export-apply-macros-in-string org-export-latex-append-header)
     ;; define alert if not yet defined
     "\n\\providecommand{\\alert}[1]{\\textbf{#1}}"
     ;; insert the title
     (format
      "\n\n\\title{%s}\n"
      (org-export-latex-fontify-headline title))
     ;; insert author info
     (if (plist-get opt-plist :author-info)
	 (format "\\author{%s%s}\n"
		 (org-export-latex-fontify-headline (or author user-full-name))
		 (if (and (plist-get opt-plist :email-info) email
			  (string-match "\\S-" email))
		     (format "\\thanks{%s}" email)
		   ""))
       (format "%%\\author{%s}\n"
	       (org-export-latex-fontify-headline (or author user-full-name))))
     ;; insert the date
     (format "\\date{%s}\n"
	     (format-time-string
	      (or (plist-get opt-plist :date)
		  org-export-latex-date-format)))
     ;; add some hyperref options
     ;; FIXME: let's have a defcustom for this?
     (format "\\hypersetup{\n  pdfkeywords={%s},\n  pdfsubject={%s},\n  pdfcreator={%s}}\n"
         (org-export-latex-fontify-headline keywords)
         (org-export-latex-fontify-headline description)
	 (concat "Emacs Org-mode version " org-version))
     ;; beginning of the document
     "\n\\begin{document}\n\n"
     ;; insert the title command
     (when (string-match "\\S-" title)
       (if (string-match "%s" org-export-latex-title-command)
	   (format org-export-latex-title-command title)
	 org-export-latex-title-command))
     "\n\n"
     ;; table of contents
     (when (and org-export-with-toc
		(plist-get opt-plist :section-numbers))
       (funcall org-export-latex-format-toc-function
		(cond ((numberp toc)
		       (min toc (plist-get opt-plist :headline-levels)))
		      (toc  (plist-get opt-plist :headline-levels))))))))

(defun org-export-latex-format-toc-default (depth)
  (when depth
    (format "\\setcounter{tocdepth}{%s}\n\\tableofcontents\n\\vspace*{1cm}\n"
	    depth)))

(defun org-export-latex-first-lines (opt-plist &optional beg end)
  "Export the first lines before first headline.
If BEG is non-nil, it is the beginning of the region.
If END is non-nil, it is the end of the region."
  (save-excursion
    (goto-char (or beg (point-min)))
    (let* ((pt (point))
	   (end (if (re-search-forward
		     (concat "^" (org-get-limited-outline-regexp)) end t)
		    (goto-char (match-beginning 0))
		  (goto-char (or end (point-max))))))
      (prog1
	  (org-export-latex-content
	   (org-export-preprocess-string
	    (buffer-substring pt end)
	    :for-backend 'latex
	    :emph-multiline t
	    :add-text nil
	    :comments nil
	    :skip-before-1st-heading nil
	    :LaTeX-fragments nil
	    :timestamps (plist-get opt-plist :timestamps)
	    :footnotes (plist-get opt-plist :footnotes)))
	(org-unmodified
	 (let ((inhibit-read-only t)
	       (limit (max pt (1- end))))
	   (add-text-properties pt limit
				'(:org-license-to-kill t))
	   (save-excursion
	     (goto-char pt)
	     (while (re-search-forward "^[ \t]*#\\+.*\n?" limit t)
	       (let ((case-fold-search t))
		 (unless (org-string-match-p
			  "^[ \t]*#\\+\\(attr_\\|caption\\>\\|label\\>\\)"
			  (match-string 0))
		   (remove-text-properties (match-beginning 0) (match-end 0)
					   '(:org-license-to-kill t))))))))))))


(defvar org-export-latex-header-defs nil
  "The header definitions that might be used in the LaTeX body.")

(defun org-export-latex-content (content &optional exclude-list)
  "Convert CONTENT string to LaTeX.
Don't perform conversions that are in EXCLUDE-LIST.  Recognized
conversion types are: quotation-marks, emphasis, sub-superscript,
links, keywords, lists, tables, fixed-width"
  (with-temp-buffer
    (org-install-letbind)
    (insert content)
    (unless (memq 'timestamps exclude-list)
      (org-export-latex-time-stamps))
    (unless (memq 'quotation-marks exclude-list)
      (org-export-latex-quotation-marks))
    (unless (memq 'emphasis exclude-list)
      (when (plist-get org-export-latex-options-plist :emphasize)
	(org-export-latex-fontify)))
    (unless (memq 'sub-superscript exclude-list)
      (org-export-latex-special-chars
       (plist-get org-export-latex-options-plist :sub-superscript)))
    (unless (memq 'links exclude-list)
      (org-export-latex-links))
    (unless (memq 'keywords exclude-list)
      (org-export-latex-keywords))
    (unless (memq 'lists exclude-list)
      (org-export-latex-lists))
    (unless (memq 'tables exclude-list)
      (org-export-latex-tables
       (plist-get org-export-latex-options-plist :tables)))
    (unless (memq 'fixed-width exclude-list)
      (org-export-latex-fixed-width
       (plist-get org-export-latex-options-plist :fixed-width)))
   ;; return string
    (buffer-substring (point-min) (point-max))))

(defun org-export-latex-protect-string (s)
  "Add the org-protected property to string S."
  (add-text-properties 0 (length s) '(org-protected t) s) s)

(defun org-export-latex-protect-char-in-string (char-list string)
  "Add org-protected text-property to char from CHAR-LIST in STRING."
  (with-temp-buffer
    (save-match-data
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward (regexp-opt char-list) nil t)
	(add-text-properties (match-beginning 0)
			     (match-end 0) '(org-protected t)))
      (buffer-string))))

(defun org-export-latex-keywords-maybe (&optional remove-list)
  "Maybe remove keywords depending on rules in REMOVE-LIST."
  (goto-char (point-min))
  (let ((re-todo (mapconcat 'identity org-export-latex-todo-keywords-1 "\\|"))
	(case-fold-search nil)
	(todo-markup org-export-latex-todo-keyword-markup)
	fmt)
    ;; convert TODO keywords
    (when (re-search-forward (concat "^\\(" re-todo "\\)") nil t)
      (if (plist-get remove-list :todo)
	  (replace-match "")
	(setq fmt (cond
		   ((stringp todo-markup) todo-markup)
		   ((and (consp todo-markup) (stringp (car todo-markup)))
		    (if (member (match-string 1) org-export-latex-done-keywords)
			(cdr todo-markup) (car todo-markup)))
		   (t (cdr (or (assoc (match-string 1) todo-markup)
			       (car todo-markup))))))
	(replace-match (org-export-latex-protect-string
			(format fmt (match-string 1))) t t)))
    ;; convert priority string
    (when (re-search-forward "\\[\\\\#.\\]" nil t)
      (if (plist-get remove-list :priority)
	  (replace-match "")
	(replace-match (format "\\textbf{%s}" (match-string 0)) t t)))
    ;; convert tags
    (when (re-search-forward "\\(:[a-zA-Z0-9_@#%]+\\)+:" nil t)
      (if (or (not org-export-with-tags)
	      (plist-get remove-list :tags))
	  (replace-match "")
	(replace-match
	 (org-export-latex-protect-string
	  (format org-export-latex-tag-markup
		  (save-match-data
		    (replace-regexp-in-string
		     "\\([_#]\\)" "\\\\\\1" (match-string 0)))))
	 t t)))))

(defun org-export-latex-fontify-headline (string)
  "Fontify special words in STRING."
  (with-temp-buffer
    ;; FIXME: org-inside-LaTeX-fragment-p doesn't work when the $...$ is at
    ;; the beginning of the buffer - inserting "\n" is safe here though.
    (insert "\n" string)

    ;; Preserve math snippets

    (let* ((matchers (plist-get org-format-latex-options :matchers))
	   (re-list org-latex-regexps)
	   beg end re e m n block off)
      ;; Check the different regular expressions
      (while (setq e (pop re-list))
	(setq m (car e) re (nth 1 e) n (nth 2 e)
	      block (if (nth 3 e) "\n\n" ""))
	(setq off (if (member m '("$" "$1")) 1 0))
	(when (and (member m matchers) (not (equal m "begin")))
	  (goto-char (point-min))
	  (while (re-search-forward re nil t)
	    (setq beg (+ (match-beginning 0) off) end (- (match-end 0) 0))
	    (add-text-properties beg end
				 '(org-protected t org-latex-math t))))))

    ;; Convert LaTeX to \LaTeX{} and TeX to \TeX{}
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (re-search-forward "\\<\\(\\(La\\)?TeX\\)\\>" nil t)
	(unless (eq (char-before (match-beginning 1)) ?\\)
	  (org-if-unprotected-1
	   (replace-match (org-export-latex-protect-string
			   (concat "\\" (match-string 1)
				   "{}")) t t)))))
    (goto-char (point-min))
    (let ((re (concat "\\\\\\([a-zA-Z]+\\)"
		      "\\(?:<[^<>\n]*>\\)*"
		      "\\(?:\\[[^][\n]*?\\]\\)*"
		      "\\(?:<[^<>\n]*>\\)*"
		      "\\("
		      (org-create-multibrace-regexp "{" "}" 3)
		      "\\)\\{1,3\\}")))
      (while (re-search-forward re nil t)
	(unless (or
		 ;; check for comment line
		 (save-excursion (goto-char (match-beginning 0))
				 (org-in-indented-comment-line))
		 ;; Check if this is a defined entity, so that is may need conversion
		 (org-entity-get (match-string 1)))
	  (add-text-properties (match-beginning 0) (match-end 0)
			       '(org-protected t)))))
    (when (plist-get org-export-latex-options-plist :emphasize)
      (org-export-latex-fontify))
    (org-export-latex-time-stamps)
    (org-export-latex-quotation-marks)
    (org-export-latex-keywords-maybe)
    (org-export-latex-special-chars
     (plist-get org-export-latex-options-plist :sub-superscript))
    (org-export-latex-links)
    (org-trim (buffer-string))))

(defun org-export-latex-time-stamps ()
  "Format time stamps."
  (goto-char (point-min))
  (let ((org-display-custom-times org-export-latex-display-custom-times))
    (while (re-search-forward org-ts-regexp-both nil t)
      (org-if-unprotected-at (1- (point))
       (replace-match
	(org-export-latex-protect-string
	 (format (if (string= "<" (substring (match-string 0) 0 1))
		     org-export-latex-timestamp-markup
		   org-export-latex-timestamp-inactive-markup)
		 (substring (org-translate-time (match-string 0)) 1 -1)))
	t t)))))

(defun org-export-latex-quotation-marks ()
  "Export quotation marks depending on language conventions."
  (mapc (lambda(l)
	  (goto-char (point-min))
	  (while (re-search-forward (car l) nil t)
	    (let ((rpl (concat (match-string 1)
			       (org-export-latex-protect-string
				(copy-sequence (cdr l))))))
	      (org-if-unprotected-1
	       (replace-match rpl t t)))))
	(cdr (or (assoc (plist-get org-export-latex-options-plist :language)
			org-export-latex-quotes)
		 ;; falls back on english
		 (assoc "en" org-export-latex-quotes)))))

(defun org-export-latex-special-chars (sub-superscript)
  "Export special characters to LaTeX.
If SUB-SUPERSCRIPT is non-nil, convert \\ and ^.
See the `org-export-latex.el' code for a complete conversion table."
  (goto-char (point-min))
  (mapc (lambda(c)
	  (goto-char (point-min))
	  (while (re-search-forward c nil t)
	    ;; Put the point where to check for org-protected
	    (unless (or (get-text-property (match-beginning 2) 'org-protected)
			(save-match-data (org-at-table.el-p)))
	      (cond ((member (match-string 2) '("\\$" "$"))
		     (if (equal (match-string 2) "\\$")
			 nil
		       (replace-match "\\$" t t)))
		    ((member (match-string 2) '("&" "%" "#"))
		     (if (equal (match-string 1) "\\")
			 (replace-match (match-string 2) t t)
		       (replace-match (concat (match-string 1) "\\"
					      (match-string 2)) t t)
		       (backward-char 1)))
		    ((equal (match-string 2) "...")
		     (replace-match
		      (concat (match-string 1)
			      (org-export-latex-protect-string "\\ldots{}")) t t))
		    ((equal (match-string 2) "~")
		     (cond ((equal (match-string 1) "\\") nil)
			   ((eq 'org-link (get-text-property 0 'face (match-string 2)))
			    (replace-match (concat (match-string 1) "\\~") t t))
			   (t (replace-match
			       (org-export-latex-protect-string
				(concat (match-string 1) "\\~{}")) t t))))
		    ((member (match-string 2) '("{" "}"))
		     (unless (save-match-data (org-inside-latex-math-p))
		       (if (equal (match-string 1) "\\")
			   (replace-match (match-string 2) t t)
			 (replace-match (concat (match-string 1) "\\"
						(match-string 2)) t t)))))
	      (unless (save-match-data (org-inside-latex-math-p))
		(cond ((equal (match-string 2) "\\")
		       (replace-match (or (save-match-data
					    (org-export-latex-treat-backslash-char
					     (match-string 1)
					     (or (match-string 3) "")))
					  "") t t)
		       (when (and (get-text-property (1- (point)) 'org-entity)
				  (looking-at "{}"))
			 ;; OK, this was an entity replacement, and the user
			 ;; had terminated the entity with {}.  Make sure
			 ;; {} is protected as well, and remove the extra {}
			 ;; inserted by the conversion.
			 (put-text-property (point) (+ 2 (point)) 'org-protected t)
			 (if (save-excursion (goto-char (max (- (point) 2) (point-min)))
					     (looking-at "{}"))
			     (replace-match ""))
			 (forward-char 2))
		       (backward-char 1))
		      ((member (match-string 2) '("_" "^"))
		       (replace-match (or (save-match-data
					    (org-export-latex-treat-sub-super-char
					     sub-superscript
					     (match-string 2)
					     (match-string 1)
					     (match-string 3))) "") t t)
		       (backward-char 1)))))))
	'(;"^\\([^\n$]*?\\|^\\)\\(\\\\?\\$\\)\\([^\n$]*\\)$"
	  "\\(\\(\\\\?\\$\\)\\)"
	  "\\([a-zA-Z0-9()]+\\|[ \t\n]\\|\\b\\|\\\\\\)\\(_\\|\\^\\)\\({[^{}]+}\\|[a-zA-Z0-9]+\\|[ \t\n]\\|[:punct:]\\|)\\|{[a-zA-Z0-9]+}\\|([a-zA-Z0-9]+)\\)"
	  "\\(.\\|^\\)\\(\\\\\\)\\([ \t\n]\\|\\([&#%{}\"]\\|[a-zA-Z][a-zA-Z0-9]*\\)\\)"
	  "\\(^\\|.\\)\\([&#%{}~]\\|\\.\\.\\.\\)"
	  ;; (?\< . "\\textless{}")
	  ;; (?\> . "\\textgreater{}")
	  )))

(defun org-inside-latex-math-p ()
  (get-text-property (point) 'org-latex-math))

(defun org-export-latex-treat-sub-super-char
  (subsup char string-before string-after)
  "Convert the \"_\" and \"^\" characters to LaTeX.
SUBSUP corresponds to the ^: option in the #+OPTIONS line.
Convert CHAR depending on STRING-BEFORE and STRING-AFTER."
  (cond ((equal string-before "\\")
	 (concat string-before char string-after))
	((and (string-match "\\S-+" string-after))
	 ;; this is part of a math formula
	 (cond ((eq 'org-link (get-text-property 0 'face char))
		(concat string-before "\\" char string-after))
	       ((save-match-data (org-inside-latex-math-p))
		(if subsup
		    (cond ((eq 1 (length string-after))
			   (concat string-before char string-after))
			  ((string-match "[({]?\\([^)}]+\\)[)}]?" string-after)
			   (format "%s%s{%s}" string-before char
				   (match-string 1 string-after))))))
	       ((and (> (length string-after) 1)
		     (or (eq subsup t)
			 (and (equal subsup '{}) (eq (string-to-char string-after) ?\{)))
		     (or (string-match "[{]?\\([^}]+\\)[}]?" string-after)
			 (string-match "[(]?\\([^)]+\\)[)]?" string-after)))

		(org-export-latex-protect-string
		 (format "%s$%s{%s}$" string-before char
			 (if (and (> (match-end 1) (1+ (match-beginning 1)))
				  (not (equal (substring string-after 0 2) "{\\")))
			     (concat "\\mathrm{" (match-string 1 string-after) "}")
			   (match-string 1 string-after)))))
	       ((eq subsup t) (concat string-before "$" char string-after "$"))
	       (t (org-export-latex-protect-string
		   (concat string-before "\\" char "{}" string-after)))))
	(t (org-export-latex-protect-string
	    (concat string-before "\\" char "{}" string-after)))))

(defun org-export-latex-treat-backslash-char (string-before string-after)
  "Convert the \"$\" special character to LaTeX.
The conversion is made depending of STRING-BEFORE and STRING-AFTER."
  (let  ((ass (org-entity-get string-after)))
    (cond
     (ass (org-add-props
	      (if (nth 2 ass)
		  (concat string-before
			  (org-export-latex-protect-string
			   (concat "$" (nth 1 ass) "$")))
		(concat string-before (org-export-latex-protect-string
				       (nth 1 ass))))
	      nil 'org-entity t))
     ((and (not (string-match "^[ \n\t]" string-after))
	   (not (string-match "[ \t]\\'\\|^" string-before)))
      ;; backslash is inside a word
      (concat string-before
	      (org-export-latex-protect-string
	       (concat "\\textbackslash{}" string-after))))
     ((not (or (equal string-after "")
	       (string-match "^[ \t\n]" string-after)))
      ;; backslash might escape a character (like \#) or a user TeX
      ;; macro (like \setcounter)
      (concat string-before
	      (org-export-latex-protect-string (concat "\\" string-after))))
     ((and (string-match "^[ \t\n]" string-after)
	   (string-match "[ \t\n]\\'" string-before))
      ;; backslash is alone, convert it to $\backslash$
      (org-export-latex-protect-string
       (concat string-before "\\textbackslash{}" string-after)))
     (t (org-export-latex-protect-string
	 (concat string-before "\\textbackslash{}" string-after))))))

(defun org-export-latex-keywords ()
  "Convert special keywords to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-export-latex-special-keyword-regexp nil t)
    (replace-match (format org-export-latex-timestamp-keyword-markup
			   (match-string 0)) t t)
    (save-excursion
      (beginning-of-line 1)
      (unless (looking-at ".*\n[ \t]*\n")
	(end-of-line 1)
	(insert "\n")))))

(defun org-export-latex-fixed-width (opt)
  "When OPT is non-nil convert fixed-width sections to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*:\\([ \t]\\|$\\)" nil t)
    (unless (get-text-property (point) 'org-example)
     (if opt
	 (progn (goto-char (match-beginning 0))
		(insert "\\begin{verbatim}\n")
		(while (looking-at "^\\([ \t]*\\):\\(\\([ \t]\\|$\\).*\\)$")
		  (replace-match (concat (match-string 1)
					 (match-string 2)) t t)
		  (forward-line))
		(insert "\\end{verbatim}\n"))
       (progn (goto-char (match-beginning 0))
	      (while (looking-at "^\\([ \t]*\\):\\(\\([ \t]\\|$\\).*\\)$")
		(replace-match (concat "%" (match-string 1)
				       (match-string 2)) t t)
		(forward-line)))))))

(defvar org-table-last-alignment) ; defined in org-table.el
(defvar org-table-last-column-widths) ; defined in org-table.el
(declare-function orgtbl-to-latex "org-table" (table params) t)
(defun org-export-latex-tables (insert)
  "Convert tables to LaTeX and INSERT it."
  ;; First, get the table.el tables
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*\\(\\+-[-+]*\\+\\)[ \t]*\n[ \t]*|" nil t)
    (org-if-unprotected
     (require 'table)
     (org-export-latex-convert-table.el-table)))

  ;; And now the Org-mode tables
  (goto-char (point-min))
  (while (re-search-forward "^\\([ \t]*\\)|" nil t)
    (org-if-unprotected-at (1- (point))
      (org-table-align)
      (let* ((beg (org-table-begin))
             (end (org-table-end))
             (raw-table (buffer-substring beg end))
             (org-table-last-alignment (copy-sequence org-table-last-alignment))
             (org-table-last-column-widths (copy-sequence
                                            org-table-last-column-widths))
             fnum fields line lines olines gr colgropen line-fmt align
             caption width shortn label attr floatp placement
	     longtblp tblenv tabular-env)
        (if org-export-latex-tables-verbatim
            (let* ((tbl (concat "\\begin{verbatim}\n" raw-table
                                "\\end{verbatim}\n")))
              (apply 'delete-region (list beg end))
              (insert (org-export-latex-protect-string tbl)))
          (progn
            (setq caption (org-find-text-property-in-string
                           'org-caption raw-table)
		  shortn (org-find-text-property-in-string
			  'org-caption-shortn raw-table)
                  attr (org-find-text-property-in-string
                        'org-attributes raw-table)
                  label (org-find-text-property-in-string
                         'org-label raw-table)
                  longtblp (and attr (stringp attr)
                                (string-match "\\<longtable\\>" attr))
		  tblenv (if (and attr (stringp attr))
			     (cond ((string-match "\\<sidewaystable\\>" attr)
				    "sidewaystable")
				   ((or (string-match (regexp-quote "table*") attr)
					(string-match "\\<multicolumn\\>" attr))
				    "table*")
				   (t "table"))
			   "table")
		  tabular-env
		  (if (and attr (stringp attr)
			   (string-match "\\(tabular.\\)" attr))
		      (match-string 1 attr)
		    org-export-latex-tabular-environment)
		  width (and attr (stringp attr)
                             (string-match "\\<width=\\([^ \t\n\r]+\\)" attr)
                             (match-string 1 attr))
                  align (and attr (stringp attr)
                             (string-match "\\<align=\\([^ \t\n\r]+\\)" attr)
                             (match-string 1 attr))
                  floatp (or caption label (string= "table*" tblenv))
		  placement     (if (and attr
					 (stringp attr)
					 (string-match "[ \t]*\\<placement=\\(\\S-+\\)" attr))
				    (match-string 1 attr)
				  (concat
				   "[" org-latex-default-figure-position "]")))
	    (setq caption (and caption (org-export-latex-fontify-headline caption)))
            (setq lines (org-split-string raw-table "\n"))
            (apply 'delete-region (list beg end))
            (when org-export-table-remove-special-lines
              (setq lines (org-table-clean-before-export lines 'maybe-quoted)))
            (when org-table-clean-did-remove-column
	      (pop org-table-last-alignment)
	      (pop org-table-last-column-widths))
            ;; make a formatting string to reflect alignment
            (setq olines lines)
            (while (and (not line-fmt) (setq line (pop olines)))
              (unless (string-match "^[ \t]*|-" line)
                (setq fields (org-split-string line "[ \t]*|[ \t]*"))
                (setq fnum (make-vector (length fields) 0))
                (setq line-fmt
                      (mapconcat
                       (lambda (x)
                         (setq gr (pop org-table-colgroup-info))
                         (format "%s%%s%s"
                                 (cond ((eq gr :start)
                                        (prog1 (if colgropen "|" "|")
                                          (setq colgropen t)))
                                       ((eq gr :startend)
                                        (prog1 (if colgropen "|" "|")
                                          (setq colgropen nil)))
                                       (t ""))
                                 (if (memq gr '(:end :startend))
                                     (progn (setq colgropen nil) "|")
                                   "")))
                       fnum ""))))
            ;; fix double || in line-fmt
            (setq line-fmt (replace-regexp-in-string "||" "|" line-fmt))
            ;; maybe remove the first and last "|"
            (when (and (not org-export-latex-tables-column-borders)
                       (string-match "^\\(|\\)?\\(.+\\)|$" line-fmt))
              (setq line-fmt (match-string 2 line-fmt)))
            ;; format alignment
            (unless align
              (setq align (apply 'format
                                 (cons line-fmt
                                       (mapcar (lambda (x) (if x "r" "l"))
                                               org-table-last-alignment)))))
            ;; prepare the table to send to orgtbl-to-latex
            (setq lines
                  (mapcar
                   (lambda(elem)
                     (or (and (string-match "[ \t]*|-+" elem) 'hline)
                         (org-split-string
			  (progn (set-text-properties 0 (length elem) nil elem)
				 (org-trim elem)) "|")))
                   lines))
            (when insert
              (insert (org-export-latex-protect-string
                       (concat
                        (if longtblp
                            (concat "\\begin{longtable}{" align "}\n")
                          (if floatp
			      (format "\\begin{%s}%s\n" tblenv placement)))
                        (if (and floatp org-export-latex-table-caption-above)
                            (format
                             "\\caption%s{%s} %s"
                             (if shortn (concat "[" shortn "]") "")
                             (or caption "")
			     (if label (format "\\label{%s}" label) "")))
			(if (and longtblp caption org-export-latex-table-caption-above)
			    "\\\\\n" "\n")
                        (if (and org-export-latex-tables-centered (not longtblp))
                            "\\begin{center}\n")
                        (if (not longtblp)
			    (format "\\begin{%s}%s{%s}\n"
				    tabular-env
				    (if width (format "{%s}" width) "")
				    align))
                        (orgtbl-to-latex
                         lines
                         `(:tstart nil :tend nil
                                   :hlend ,(if longtblp
                                               (format "\\\\
\\hline
\\endhead
\\hline\\multicolumn{%d}{r}{Continued on next page}\\
\\endfoot
\\endlastfoot" (length org-table-last-alignment))
                                             nil)))
                        (if (not longtblp) (format "\n\\end{%s}" tabular-env))
                        (if longtblp "\n" (if org-export-latex-tables-centered
                                              "\n\\end{center}\n" "\n"))
                        (if (and floatp (not org-export-latex-table-caption-above))
                            (format
                             "\\caption%s{%s} %s"
                             (if shortn (concat "[" shortn "]") "")
                             (or caption "")
			     (if label (format "\\label{%s}" label) "")))
                        (if longtblp
                            "\\end{longtable}"
                          (if floatp (format "\\end{%s}" tblenv)))))
                      "\n\n"))))))))

(defun org-export-latex-convert-table.el-table ()
  "Replace table.el table at point with LaTeX code."
  (let (tbl caption shortn label line floatp attr align rmlines)
    (setq line (buffer-substring (point-at-bol) (point-at-eol))
	  label (org-get-text-property-any 0 'org-label line)
	  caption (org-get-text-property-any 0 'org-caption line)
	  shortn (org-get-text-property-any 0 'org-caption-shortn line)
	  attr (org-get-text-property-any 0 'org-attributes line)
	  align (and attr (stringp attr)
		     (string-match "\\<align=\\([^ \t\n\r,]+\\)" attr)
		     (match-string 1 attr))
	  rmlines (and attr (stringp attr)
		       (string-match "\\<rmlines\\>" attr))
	  floatp (or label caption))
    (and (get-buffer "*org-export-table*")
	 (kill-buffer (get-buffer "*org-export-table*")))
    (table-generate-source 'latex "*org-export-table*" "caption")
    (setq tbl (with-current-buffer "*org-export-table*"
		(buffer-string)))
    (while (string-match "^%.*\n" tbl)
      (setq tbl (replace-match "" t t tbl)))
    ;; fix the hlines
    (when rmlines
      (let ((n 0) lines)
	(setq lines (mapcar (lambda (x)
			      (if (string-match "^\\\\hline$" x)
				  (progn
				    (setq n (1+ n))
				    (if (= n 2) x nil))
				x))
			    (org-split-string tbl "\n")))
	(setq tbl (mapconcat 'identity (delq nil lines) "\n"))))
    (when (and align (string-match "\\\\begin{tabular}{.*}" tbl))
      (setq tbl (replace-match (concat "\\begin{tabular}{" align "}")
			       t t tbl)))
    (and (get-buffer "*org-export-table*")
	 (kill-buffer (get-buffer "*org-export-table*")))
    (beginning-of-line 0)
    (while (looking-at "[ \t]*\\(|\\|\\+-\\)")
      (delete-region (point) (1+ (point-at-eol))))
    (when org-export-latex-tables-centered
      (setq tbl (concat "\\begin{center}\n" tbl "\\end{center}")))
    (when floatp
      (setq tbl (concat "\\begin{table}\n"
			(if (not org-export-latex-table-caption-above) tbl)
			(format "\\caption%s{%s%s}\n"
				(if shortn (format "[%s]" shortn) "")
				(if label (format "\\label{%s}" label) "")
				(or caption ""))
			(if org-export-latex-table-caption-above tbl)
			"\n\\end{table}\n")))
    (insert (org-export-latex-protect-string tbl))))

(defun org-export-latex-fontify ()
  "Convert fontification to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-emph-re nil t)
    ;; The match goes one char after the *string*, except at the end of a line
    (let ((emph (assoc (match-string 3)
		       org-export-latex-emphasis-alist))
	  (beg (match-beginning 0))
	  (end (match-end 0))
	  rpl s)
      (unless emph
	(message "`org-export-latex-emphasis-alist' has no entry for formatting triggered by \"%s\""
		 (match-string 3)))
      (unless (or (and (get-text-property (- (point) 2) 'org-protected)
		       (not (get-text-property
			     (- (point) 2) 'org-verbatim-emph)))
		  (equal (char-after (match-beginning 3))
			 (char-after (1+ (match-beginning 3))))
		  (save-excursion
		    (goto-char (match-beginning 1))
		    (save-match-data
		      (and (org-at-table-p)
			   (string-match
			    "[|\n]" (buffer-substring beg end)))))
		  (and (equal (match-string 3) "+")
		       (save-match-data
			 (string-match "\\`-+\\'" (match-string 4)))))
	(setq s (match-string 4))
	(setq rpl (concat (match-string 1)
			  (org-export-latex-emph-format (cadr emph)
							(match-string 4))
			  (match-string 5)))
	(if (caddr emph)
	    (setq rpl (org-export-latex-protect-string rpl))
	  (save-match-data
	    (if (string-match "\\`.?\\(\\\\[a-z]+{\\)\\(.*\\)\\(}\\).?\\'" rpl)
		(progn
		  (add-text-properties (match-beginning 1) (match-end 1)
				       '(org-protected t) rpl)
		  (add-text-properties (match-beginning 3) (match-end 3)
				       '(org-protected t) rpl)))))
	(replace-match rpl t t)))
    (backward-char)))

(defun org-export-latex-emph-format (format string)
  "Format an emphasis string and handle the \\verb special case."
  (when (member format '("\\verb" "\\protectedtexttt"))
    (save-match-data
      (if (equal format "\\verb")
	  (let ((ll "~,./?;':\"|!@#%^&-_=+abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ<>()[]{}"))
	    (catch 'exit
	      (loop for i from 0 to (1- (length ll)) do
		    (if (not (string-match (regexp-quote (substring ll i (1+ i)))
					   string))
			(progn
			  (setq format (concat "\\verb" (substring ll i (1+ i))
					       "%s" (substring ll i (1+ i))))
			  (throw 'exit nil))))))
	(let ((start 0)
	      (trans '(("\\" . "\\textbackslash{}")
		       ("~" . "\\textasciitilde{}")
		       ("^" . "\\textasciicircum{}")))
	      (rtn "") char)
	  (while (string-match "[\\{}$%&_#~^]" string)
	    (setq char (match-string 0 string))
	    (if (> (match-beginning 0) 0)
		(setq rtn (concat rtn (substring string
						 0 (match-beginning 0)))))
	    (setq string (substring string (1+ (match-beginning 0))))
	    (setq char (or (cdr (assoc char trans)) (concat "\\" char))
		  rtn (concat rtn char)))
	  (setq string (concat rtn string) format "\\texttt{%s}")
	  (while (string-match "--" string)
	    (setq string (replace-match "-{}-" t t string)))))))
  (format format string))

(defun org-export-latex-links ()
  ;; Make sure to use the LaTeX hyperref and graphicx package
  ;; or send some warnings.
  "Convert links to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-bracket-link-analytic-regexp++ nil t)
    (org-if-unprotected-1
     (goto-char (match-beginning 0))
     (let* ((re-radio org-export-latex-all-targets-re)
	    (remove (list (match-beginning 0) (match-end 0)))
	    (raw-path (org-extract-attributes (match-string 3)))
	    (full-raw-path (concat (match-string 1) raw-path))
	    (desc (match-string 5))
	    (type (or (match-string 2)
		      (if (or (file-name-absolute-p raw-path)
			      (string-match "^\\.\\.?/" raw-path))
			  "file")))
	    (coderefp (equal type "coderef"))
	    (caption (org-find-text-property-in-string 'org-caption raw-path))
	    (shortn (org-find-text-property-in-string 'org-caption-shortn raw-path))
	    (attr (or (org-find-text-property-in-string 'org-attributes raw-path)
		      (plist-get org-export-latex-options-plist :latex-image-options)))
	    (label (org-find-text-property-in-string 'org-label raw-path))
	    imgp radiop fnc
	    ;; define the path of the link
	    (path (cond
		   ((member type '("coderef"))
		    raw-path)
		   ((member type '("http" "https" "ftp"))
		    (concat type ":" raw-path))
		   ((and re-radio (string-match re-radio raw-path))
		    (setq radiop t))
		   ((equal type "mailto")
		    (concat type ":" raw-path))
		   ((equal type "file")
		    (if (and (org-file-image-p
			      (expand-file-name
			       raw-path)
			      org-export-latex-inline-image-extensions)
			     (or (get-text-property 0 'org-no-description
						    raw-path)
				 (equal desc full-raw-path)))
			(setq imgp t)
		      (progn (when (string-match "\\(.+\\)::.+" raw-path)
			       (setq raw-path (match-string 1 raw-path)))
			     (if (file-exists-p raw-path)
				 (concat type "://" (expand-file-name raw-path))
			       (concat type "://" (org-export-directory
						   :LaTeX org-export-latex-options-plist)
				       raw-path))))))))
       ;; process with link inserting
       (apply 'delete-region remove)
       (setq caption (and caption (org-export-latex-fontify-headline caption)))
       (cond ((and imgp
		   (plist-get org-export-latex-options-plist :inline-images))
	      ;; OK, we need to inline an image
	      (insert
	       (org-export-latex-format-image raw-path caption label attr shortn)))
	     (coderefp
	      (insert (format
		       (org-export-get-coderef-format path desc)
		       (cdr (assoc path org-export-code-refs)))))
	     (radiop (insert (format org-export-latex-hyperref-format
				     (org-solidify-link-text raw-path) desc)))
	     ((not type)
	      (insert (format org-export-latex-hyperref-format
			      (org-remove-initial-hash
			       (org-solidify-link-text raw-path))
			      desc)))
	     (path
	      (when (org-at-table-p)
		;; There is a strange problem when we have a link in a table,
		;; ampersands then cause a problem.  I think this must be
		;; a LaTeX issue, but we here implement a work-around anyway.
		(setq path (org-export-latex-protect-amp path)
		      desc (org-export-latex-protect-amp desc)))
	      (insert
	       (if (string-match "%s.*%s" org-export-latex-href-format)
		   (format org-export-latex-href-format path desc)
		 (format org-export-latex-href-format path))))

	     ((functionp (setq fnc (nth 2 (assoc type org-link-protocols))))
	      ;; The link protocol has a function for formatting the link
	      (insert
	       (save-match-data
		 (funcall fnc (org-link-unescape raw-path) desc 'latex))))

	     (t (insert "\\texttt{" desc "}")))))))


(defun org-export-latex-format-image (path caption label attr &optional shortn)
  "Format the image element, depending on user settings."
  (let (ind floatp wrapp multicolumnp placement figenv)
    (setq floatp (or caption label))
    (setq ind (org-get-text-property-any 0 'original-indentation path))
    (when (and attr (stringp attr))
      (if (string-match "[ \t]*\\<wrap\\>" attr)
	  (setq wrapp t floatp nil attr (replace-match "" t t attr)))
      (if (string-match "[ \t]*\\<float\\>" attr)
	  (setq wrapp nil floatp t attr (replace-match "" t t attr)))
      (if (string-match "[ \t]*\\<multicolumn\\>" attr)
	  (setq multicolumnp t attr (replace-match "" t t attr))))

    (setq placement
	  (cond
	   (wrapp "{l}{0.5\\textwidth}")
	   (floatp (concat "[" org-latex-default-figure-position "]"))
	   (t "")))

    (when (and attr (stringp attr)
	       (string-match "[ \t]*\\<placement=\\(\\S-+\\)" attr))
      (setq placement (match-string 1 attr)
	    attr (replace-match "" t t attr)))
    (setq attr (and attr (org-trim attr)))
    (when (or (not attr) (= (length attr) 0))
      (setq attr (cond (floatp "width=0.7\\textwidth")
		       (wrapp "width=0.48\\textwidth")
		       (t attr))))
    (setq figenv
	  (cond
	   (wrapp "\\begin{wrapfigure}%placement
\\centering
\\includegraphics[%attr]{%path}
\\caption%shortn{%labelcmd%caption}
\\end{wrapfigure}")
	   (multicolumnp "\\begin{figure*}%placement
\\centering
\\includegraphics[%attr]{%path}
\\caption%shortn{%labelcmd%caption}
\\end{figure*}")
	   (floatp "\\begin{figure}%placement
\\centering
\\includegraphics[%attr]{%path}
\\caption%shortn{%labelcmd%caption}
\\end{figure}")
	   (t "\\includegraphics[%attr]{%path}")))


    (setq figenv (mapconcat 'identity (split-string figenv "\n")
			    (save-excursion (beginning-of-line 1)
					    (looking-at "[ \t]*")
					    (concat "\n" (match-string 0)))))

    (if (and (not label) (not caption)
	     (string-match "^\\\\caption{.*\n" figenv))
	(setq figenv (replace-match "" t t figenv)))
    (org-add-props
	(org-fill-template
	 figenv
	 (list (cons "path"
		     (if (file-name-absolute-p path)
			 (expand-file-name path)
		       path))
	       (cons "attr" attr)
	       (cons "shortn" (if shortn (format "[%s]" shortn) ""))
	       (cons "labelcmd" (if label (format "\\label{%s}"
						  label)""))
	       (cons "caption" (or caption ""))
	       (cons "placement" (or placement ""))))
	nil 'original-indentation ind)))

(defun org-export-latex-protect-amp (s)
  (while (string-match "\\([^\\\\]\\)\\(&\\)" s)
    (setq s (replace-match (concat (match-string 1 s) "\\" (match-string 2 s))
			   t t s)))
  s)

(defun org-remove-initial-hash (s)
  (if (string-match "\\`#" s)
      (substring s 1)
    s))
(defvar org-latex-entities)   ; defined below
(defvar org-latex-entities-regexp)   ; defined below

(defun org-export-latex-preprocess (parameters)
  "Clean stuff in the LaTeX export."
  ;; Replace footnotes.
  (when (plist-get parameters :footnotes)
    (goto-char (point-min))
    (let (ref)
      (while (setq ref (org-footnote-get-next-reference))
	(let* ((beg (nth 1 ref))
	       (lbl (car ref))
	       (def (nth 1 (assoc (string-to-number lbl)
				  (mapcar (lambda (e) (cdr e))
					  org-export-footnotes-seen)))))
	  ;; Fix body for footnotes ending on a link or a list and
	  ;; remove definition from buffer.
	  (setq def
		(concat def
			(if (string-match "ORG-LIST-END-MARKER\\'" def)
			    "\n" " ")))
	  (org-footnote-delete-definitions lbl)
	  ;; Compute string to insert (FNOTE), and protect the outside
	  ;; macro from further transformation.  When footnote at
	  ;; point is referring to a previously defined footnote, use
	  ;; \footnotemark. Otherwise, use \footnote.
	  (let ((fnote (if (member lbl org-export-latex-footmark-seen)
			   (org-export-latex-protect-string
			    (format "\\footnotemark[%s]" lbl))
			 (push lbl org-export-latex-footmark-seen)
			 (concat (org-export-latex-protect-string "\\footnote{")
				 def
				 (org-export-latex-protect-string "}"))))
		;; Check if another footnote is immediately following.
		;; If so, add a separator in-between.
		(sep (org-export-latex-protect-string
		      (if (save-excursion (goto-char (1- (nth 2 ref)))
					  (let ((next (org-footnote-get-next-reference)))
					    (and next (= (nth 1 next) (nth 2 ref)))))
			  org-export-latex-footnote-separator ""))))
	    (when (org-at-heading-p)
	      (setq fnote (concat (org-export-latex-protect-string "\\protect")
				  fnote)))
	    ;; Ensure a footnote at column 0 cannot end a list
	    ;; containing it.
	    (put-text-property 0 (length fnote) 'original-indentation 1000 fnote)
	    ;; Replace footnote reference with FNOTE and, maybe, SEP.
	    ;; `save-excursion' is required if there are two footnotes
	    ;; in a row.  In that case, point would be left at the
	    ;; beginning of the second one, and
	    ;; `org-footnote-get-next-reference' would then skip it.
	    (goto-char beg)
	    (delete-region beg (nth 2 ref))
	    (save-excursion (insert fnote sep)))))))

  ;; Remove footnote section tag for LaTeX
  (goto-char (point-min))
  (while (re-search-forward
	  (concat "^" footnote-section-tag-regexp) nil t)
    (org-if-unprotected
     (replace-match "")))
  ;; Remove any left-over footnote definition.
  (mapc (lambda (fn) (org-footnote-delete-definitions (car fn)))
	org-export-footnotes-data)
  (mapc (lambda (fn) (org-footnote-delete-definitions fn))
	org-export-latex-footmark-seen)

  ;; Preserve line breaks
  (goto-char (point-min))
  (while (re-search-forward "\\\\\\\\" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(org-protected t)))

  ;; Preserve latex environments
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*\\\\begin{\\([a-zA-Z]+\\*?\\)}" nil t)
    (org-if-unprotected
     (let* ((start (progn (beginning-of-line) (point)))
	    (end (and (re-search-forward
		       (concat "^[ \t]*\\\\end{"
			       (regexp-quote (match-string 1))
			       "}") nil t)
		      (point-at-eol))))
       (if end
	   (add-text-properties start end '(org-protected t))
	 (goto-char (point-at-eol))))))

  ;; Preserve math snippets
  (let* ((matchers (plist-get org-format-latex-options :matchers))
	 (re-list org-latex-regexps)
	 beg end re e m n block off)
    ;; Check the different regular expressions
    (while (setq e (pop re-list))
      (setq m (car e) re (nth 1 e) n (nth 2 e)
	    block (if (nth 3 e) "\n\n" ""))
      (setq off (if (member m '("$" "$1")) 1 0))
      (when (and (member m matchers) (not (equal m "begin")))
	(goto-char (point-min))
	(while (re-search-forward re nil t)
	  (setq beg (+ (match-beginning 0) off) end (- (match-end 0) 0))
	  (add-text-properties beg end '(org-protected t org-latex-math t))))))

  ;; Convert LaTeX to \LaTeX{} and TeX to \TeX{}
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (while (re-search-forward "\\<\\(\\(La\\)?TeX\\)\\>" nil t)
      (unless (eq (char-before (match-beginning 1)) ?\\)
	(org-if-unprotected-1
	 (replace-match (org-export-latex-protect-string
			 (concat "\\" (match-string 1)
				 "{}")) t t)))))

  ;; Convert blockquotes
  (goto-char (point-min))
  (while (search-forward "ORG-BLOCKQUOTE-START" nil t)
    (org-replace-match-keep-properties "\\begin{quote}" t t))
  (goto-char (point-min))
  (while (search-forward "ORG-BLOCKQUOTE-END" nil t)
    (org-replace-match-keep-properties "\\end{quote}" t t))

  ;; Convert verse
  (goto-char (point-min))
  (while (search-forward "ORG-VERSE-START" nil t)
    (org-replace-match-keep-properties "\\begin{verse}" t t)
    (beginning-of-line 2)
    (while (and (not (looking-at "[ \t]*ORG-VERSE-END.*")) (not (eobp)))
      (when (looking-at "\\([ \t]+\\)\\([^ \t\n]\\)")
	(goto-char (match-end 1))
	(org-replace-match-keep-properties
	 (org-export-latex-protect-string
	  (concat "\\hspace*{1cm}" (match-string 2))) t t)
	(beginning-of-line 1))
      (if (looking-at "[ \t]*$")
	  (insert (org-export-latex-protect-string "\\vspace*{1em}"))
	(unless (looking-at ".*?[^ \t\n].*?\\\\\\\\[ \t]*$")
	  (end-of-line 1)
	  (insert "\\\\")))
      (beginning-of-line 2))
    (and (looking-at "[ \t]*ORG-VERSE-END.*")
	 (org-replace-match-keep-properties "\\end{verse}" t t)))

  ;; Convert #+INDEX to LaTeX \\index.
  (goto-char (point-min))
  (let ((case-fold-search t) entry)
    (while (re-search-forward
	    "^[ \t]*#\\+index:[ \t]*\\([^ \t\r\n].*?\\)[ \t]*$"
	    nil t)
      (setq entry
	    (save-match-data
	      (org-export-latex-protect-string
	       (org-export-latex-fontify-headline (match-string 1)))))
      (replace-match (format "\\index{%s}" entry) t t)))

  ;; Convert center
  (goto-char (point-min))
  (while (search-forward "ORG-CENTER-START" nil t)
    (org-replace-match-keep-properties "\\begin{center}" t t))
  (goto-char (point-min))
  (while (search-forward "ORG-CENTER-END" nil t)
    (org-replace-match-keep-properties "\\end{center}" t t))

  (run-hooks 'org-export-latex-after-blockquotes-hook)

  ;; Convert horizontal rules
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*-\\{5,\\}[ \t]*$" nil t)
    (org-if-unprotected
     (replace-match (org-export-latex-protect-string "\\hrule") t t)))

  ;; Protect LaTeX commands like \command[...]{...} or \command{...}
  (goto-char (point-min))
  (let ((re (concat
	     "\\\\\\([a-zA-Z]+\\*?\\)"
	     "\\(?:<[^<>\n]*>\\)*"
	     "\\(?:\\[[^][\n]*?\\]\\)*"
	     "\\(?:<[^<>\n]*>\\)*"
	     "\\(" (org-create-multibrace-regexp "{" "}" 3) "\\)\\{1,3\\}")))
    (while (re-search-forward re nil t)
      (unless (or
	       ;; Check for comment line.
	       (save-excursion (goto-char (match-beginning 0))
			       (org-in-indented-comment-line))
	       ;; Check if this is a defined entity, so that is may
	       ;; need conversion.
	       (org-entity-get (match-string 1))
	       ;; Do not protect interior of footnotes.  Those have
	       ;; already been taken care of earlier in the function.
	       ;; Yet, keep looking inside them for more commands.
	       (and (equal (match-string 1) "footnote")
		    (goto-char (match-end 1))))
	(add-text-properties (match-beginning 0) (match-end 0)
			     '(org-protected t)))))

  ;; Special case for \nbsp
  (goto-char (point-min))
  (while (re-search-forward "\\\\nbsp\\({}\\|\\>\\)" nil t)
    (org-if-unprotected
     (replace-match (org-export-latex-protect-string "~"))))

  ;; Protect LaTeX entities
  (goto-char (point-min))
  (while (re-search-forward org-latex-entities-regexp nil t)
    (org-if-unprotected
     (add-text-properties (match-beginning 0) (match-end 0)
			  '(org-protected t))))

  ;; Replace radio links
  (goto-char (point-min))
  (while (re-search-forward
	  (concat "<<<?" org-export-latex-all-targets-re
		  ">>>?\\((INVISIBLE)\\)?") nil t)
    (org-if-unprotected-at (+ (match-beginning 0) 2)
      (replace-match
       (concat
	(org-export-latex-protect-string
	 (format "\\label{%s}" (save-match-data (org-solidify-link-text
						 (match-string 1)))))
	(if (match-string 2) "" (match-string 1)))
       t t)))

  ;; Delete @<...> constructs
  ;; Thanks to Daniel Clemente for this regexp
  (goto-char (point-min))
  (while (re-search-forward "@<\\(?:[^\"\n]\\|\".*\"\\)*?>" nil t)
    (org-if-unprotected
     (replace-match ""))))

(defun org-export-latex-fix-inputenc ()
  "Set the coding system in inputenc to what the buffer is."
  (let* ((cs buffer-file-coding-system)
	 (opt (or (ignore-errors (latexenc-coding-system-to-inputenc cs))
		  "utf8")))
    (when opt
      ;; Translate if that is requested
      (setq opt (or (cdr (assoc opt org-export-latex-inputenc-alist)) opt))
      ;; find the \usepackage statement and replace the option
      (goto-char (point-min))
      (while (re-search-forward "\\\\usepackage\\[\\(AUTO\\)\\]{inputenc}"
				nil t)
	(goto-char (match-beginning 1))
	(delete-region (match-beginning 1) (match-end 1))
	(insert opt))
      (and buffer-file-name
	   (save-buffer)))))

;;; List handling:

(defun org-export-latex-lists ()
  "Convert plain text lists in current buffer into LaTeX lists."
  ;; `org-list-end-re' output has changed since preprocess from
  ;; org-exp.el. Make sure it is taken into account.
  (let ((org-list-end-re "^ORG-LIST-END-MARKER\n"))
    (mapc
     (lambda (e)
       ;; For each type of context allowed for list export (E), find
       ;; every list, parse it, delete it and insert resulting
       ;; conversion to latex (RES), while keeping the same
       ;; `original-indentation' property.
       (let (res)
	 (goto-char (point-min))
	 (while (re-search-forward (org-item-beginning-re) nil t)
	   (when (and (eq (get-text-property (point) 'list-context) e)
		      (not (get-text-property (point) 'org-example)))
	     (beginning-of-line)
	     (setq res
		   (org-list-to-latex
		    ;; Narrowing is needed because we're converting
		    ;; from inner functions to outer ones.
		    (save-restriction
		      (narrow-to-region (point) (point-max))
		      (org-list-parse-list t))
		    org-export-latex-list-parameters))
	     ;; Extend previous value of original-indentation to the
	     ;; whole string
	     (insert (org-add-props res nil 'original-indentation
				    (org-find-text-property-in-string
				     'original-indentation res)))))))
     ;; List of allowed contexts for export, and the default one.
     (append org-list-export-context '(nil)))))

(defconst org-latex-entities
 '("\\!"
   "\\'"
   "\\+"
   "\\,"
   "\\-"
   "\\:"
   "\\;"
   "\\<"
   "\\="
   "\\>"
   "\\Huge"
   "\\LARGE"
   "\\Large"
   "\\Styles"
   "\\\\"
   "\\`"
   "\\\""
   "\\addcontentsline"
   "\\address"
   "\\addtocontents"
   "\\addtocounter"
   "\\addtolength"
   "\\addvspace"
   "\\alph"
   "\\appendix"
   "\\arabic"
   "\\author"
   "\\begin{array}"
   "\\begin{center}"
   "\\begin{description}"
   "\\begin{enumerate}"
   "\\begin{eqnarray}"
   "\\begin{equation}"
   "\\begin{figure}"
   "\\begin{flushleft}"
   "\\begin{flushright}"
   "\\begin{itemize}"
   "\\begin{list}"
   "\\begin{minipage}"
   "\\begin{picture}"
   "\\begin{quotation}"
   "\\begin{quote}"
   "\\begin{tabbing}"
   "\\begin{table}"
   "\\begin{tabular}"
   "\\begin{thebibliography}"
   "\\begin{theorem}"
   "\\begin{titlepage}"
   "\\begin{verbatim}"
   "\\begin{verse}"
   "\\bf"
   "\\bf"
   "\\bibitem"
   "\\bigskip"
   "\\cdots"
   "\\centering"
   "\\circle"
   "\\cite"
   "\\cleardoublepage"
   "\\clearpage"
   "\\cline"
   "\\closing"
   "\\dashbox"
   "\\date"
   "\\ddots"
   "\\dotfill"
   "\\em"
   "\\fbox"
   "\\flushbottom"
   "\\fnsymbol"
   "\\footnote"
   "\\footnotemark"
   "\\footnotesize"
   "\\footnotetext"
   "\\frac"
   "\\frame"
   "\\framebox"
   "\\hfill"
   "\\hline"
   "\\hrulespace"
   "\\hspace"
   "\\huge"
   "\\hyphenation"
   "\\include"
   "\\includeonly"
   "\\indent"
   "\\input"
   "\\it"
   "\\kill"
   "\\label"
   "\\large"
   "\\ldots"
   "\\line"
   "\\linebreak"
   "\\linethickness"
   "\\listoffigures"
   "\\listoftables"
   "\\location"
   "\\makebox"
   "\\maketitle"
   "\\mark"
   "\\mbox"
   "\\medskip"
   "\\multicolumn"
   "\\multiput"
   "\\newcommand"
   "\\newcounter"
   "\\newenvironment"
   "\\newfont"
   "\\newlength"
   "\\newline"
   "\\newpage"
   "\\newsavebox"
   "\\newtheorem"
   "\\nocite"
   "\\nofiles"
   "\\noindent"
   "\\nolinebreak"
   "\\nopagebreak"
   "\\normalsize"
   "\\onecolumn"
   "\\opening"
   "\\oval"
   "\\overbrace"
   "\\overline"
   "\\pagebreak"
   "\\pagenumbering"
   "\\pageref"
   "\\pagestyle"
   "\\par"
   "\\parbox"
   "\\put"
   "\\raggedbottom"
   "\\raggedleft"
   "\\raggedright"
   "\\raisebox"
   "\\ref"
   "\\rm"
   "\\roman"
   "\\rule"
   "\\savebox"
   "\\sc"
   "\\scriptsize"
   "\\setcounter"
   "\\setlength"
   "\\settowidth"
   "\\sf"
   "\\shortstack"
   "\\signature"
   "\\sl"
   "\\small"
   "\\smallskip"
   "\\sqrt"
   "\\tableofcontents"
   "\\telephone"
   "\\thanks"
   "\\thispagestyle"
   "\\tiny"
   "\\title"
   "\\tt"
   "\\twocolumn"
   "\\typein"
   "\\typeout"
   "\\underbrace"
   "\\underline"
   "\\usebox"
   "\\usecounter"
   "\\value"
   "\\vdots"
   "\\vector"
   "\\verb"
   "\\vfill"
   "\\vline"
   "\\vspace")
 "A list of LaTeX commands to be protected when performing conversion.")

(defconst org-latex-entities-regexp
  (let (names rest)
    (dolist (x org-latex-entities)
      (if (string-match "[a-zA-Z]$" x)
	  (push x names)
	(push x rest)))
    (concat "\\(" (regexp-opt (nreverse names)) "\\>\\)"
	    "\\|\\(" (regexp-opt (nreverse rest)) "\\)")))

(provide 'org-export-latex)
(provide 'org-latex)

;;; org-latex.el ends here
