;;; bibtex.el --- BibTeX mode for GNU Emacs -*- lexical-binding: t -*-

;; Copyright (C) 1992, 1994-1999, 2001-2012  Free Software Foundation, Inc.

;; Author: Stefan Schoef <schoef@offis.uni-oldenburg.de>
;;      Bengt Martensson <bengt@mathematik.uni-Bremen.de>
;;      Marc Shapiro <marc.shapiro@acm.org>
;;      Mike Newton <newton@gumby.cs.caltech.edu>
;;      Aaron Larson <alarson@src.honeywell.com>
;;      Dirk Herrmann <D.Herrmann@tu-bs.de>
;; Maintainer: Roland Winkler <winkler@gnu.org>
;; Keywords: BibTeX, LaTeX, TeX

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

;;  Major mode for editing and validating BibTeX files.

;;  Usage:
;;  See documentation for `bibtex-mode' or type "M-x describe-mode"
;;  when you are in BibTeX mode.

;;  Todo:
;;  Distribute texinfo file.

;;; Code:

(require 'button)


;; User Options:

(defgroup bibtex nil
  "BibTeX mode."
  :group 'tex
  :prefix "bibtex-")

(defgroup bibtex-autokey nil
  "Generate automatically a key from the author/editor and the title field."
  :group 'bibtex
  :prefix "bibtex-autokey-")

(defcustom bibtex-mode-hook nil
  "List of functions to call on entry to BibTeX mode."
  :group 'bibtex
  :type 'hook)

(defcustom bibtex-field-delimiters 'braces
  "Type of field delimiters.  Allowed values are `braces' or `double-quotes'."
  :group 'bibtex
  :type '(choice (const braces)
                 (const double-quotes)))

(defcustom bibtex-entry-delimiters 'braces
  "Type of entry delimiters.  Allowed values are `braces' or `parentheses'."
  :group 'bibtex
  :type '(choice (const braces)
                 (const parentheses)))

(defcustom bibtex-include-OPTcrossref '("InProceedings" "InCollection")
  "List of BibTeX entries that get an OPTcrossref field."
  :group 'bibtex
  :type '(repeat string))

(defcustom bibtex-include-OPTkey t
  "If non-nil, all newly created entries get an OPTkey field.
If this is a string, use it as the initial field text.
If this is a function, call it to generate the initial field text."
  :group 'bibtex
  :type '(choice (const :tag "None" nil)
                 (string :tag "Initial text")
                 (function :tag "Initialize Function")
                 (const :tag "Default" t)))
(put 'bibtex-include-OPTkey 'risky-local-variable t)

(defcustom bibtex-user-optional-fields
  '(("annote" "Personal annotation (ignored)"))
  "List of optional fields the user wants to have always present.
Entries should be of the same form as the OPTIONAL list
in `bibtex-BibTeX-entry-alist' (which see)."
  :group 'bibtex
  :type '(repeat (group (string :tag "Field")
                        (string :tag "Comment")
                        (option (choice :tag "Init"
                                        (const nil) string function)))))
(put 'bibtex-user-optional-fields 'risky-local-variable t)

(defcustom bibtex-entry-format
  '(opts-or-alts required-fields numerical-fields)
  "Type of formatting performed by `bibtex-clean-entry'.
It may be t, nil, or a list of symbols out of the following:
opts-or-alts        Delete empty optional and alternative fields and
                      remove OPT and ALT prefixes from used fields.
required-fields     Signal an error if a required field is missing.
numerical-fields    Delete delimiters around numeral fields.
page-dashes         Change double dashes in page field to single dash
                      (for scribe compatibility).
whitespace          Delete whitespace at the beginning and end of fields.
inherit-booktitle   If entry contains a crossref field and the booktitle
                      field is empty, set the booktitle field to the content
                      of the title field of the crossreferenced entry.
realign             Realign entries, so that field texts and perhaps equal
                      signs (depending on the value of
                      `bibtex-align-at-equal-sign') begin in the same column.
                      Also fill fields.
last-comma          Add or delete comma on end of last field in entry,
                      according to value of `bibtex-comma-after-last-field'.
delimiters          Change delimiters according to variables
                      `bibtex-field-delimiters' and `bibtex-entry-delimiters'.
unify-case          Change case of entry types and field names.
braces              Enclose parts of field entries by braces according to
                      `bibtex-field-braces-alist'.
strings             Replace parts of field entries by string constants
                      according to `bibtex-field-strings-alist'.
sort-fields         Sort fields to match the field order in
                    `bibtex-BibTeX-entry-alist'.

The value t means do all of the above formatting actions.
The value nil means do no formatting at all."
  :group 'bibtex
  :type '(choice (const :tag "None" nil)
                 (const :tag "All" t)
                 (set :menu-tag "Some"
                      (const opts-or-alts)
                      (const required-fields)
                      (const numerical-fields)
                      (const page-dashes)
                      (const whitespace)
                      (const inherit-booktitle)
                      (const realign)
                      (const last-comma)
                      (const delimiters)
                      (const unify-case)
                      (const braces)
                      (const strings)
                      (const sort-fields))))
(put 'bibtex-entry-format 'safe-local-variable
     (lambda (x)
       (or (eq x t)
           (let ((OK t))
             (while (consp x)
               (unless (memq (pop x)
                             '(opts-or-alts required-fields numerical-fields
                               page-dashes whitespace inherit-booktitle realign
                               last-comma delimiters unify-case braces strings
                               sort-fields))
                 (setq OK nil)))
             (unless (null x) (setq OK nil))
             OK))))

(defcustom bibtex-field-braces-alist nil
 "Alist of field regexps that \\[bibtex-clean-entry] encloses by braces.
Each element has the form (FIELDS REGEXP), where FIELDS is a list
of BibTeX field names and REGEXP is a regexp.
Space characters in REGEXP will be replaced by \"[ \\t\\n]+\"."
  :group 'bibtex
  :type '(repeat (list (repeat (string :tag "field name"))
                       (choice (regexp :tag "regexp")
                               (sexp :tag "sexp")))))

(defcustom bibtex-field-strings-alist nil
 "Alist of regexps that \\[bibtex-clean-entry] replaces by string constants.
Each element has the form (FIELDS REGEXP TO-STR), where FIELDS is a list
of BibTeX field names.  In FIELDS search for REGEXP, which are replaced
by the BibTeX string constant TO-STR.
Space characters in REGEXP will be replaced by \"[ \\t\\n]+\"."
  :group 'bibtex
  :type '(repeat (list (repeat (string :tag "field name"))
                       (regexp :tag "From regexp")
                       (regexp :tag "To string constant"))))

(defcustom bibtex-clean-entry-hook nil
  "List of functions to call when entry has been cleaned.
Functions are called with point inside the cleaned entry, and the buffer
narrowed to just the entry."
  :group 'bibtex
  :type 'hook)

(defcustom bibtex-maintain-sorted-entries nil
  "If non-nil, BibTeX mode maintains all entries in sorted order.
Allowed non-nil values are:
plain or t   All entries are sorted alphabetically.
crossref     All entries are sorted alphabetically unless an entry has a
             crossref field.  These crossrefed entries are placed in
             alphabetical order immediately preceding the main entry.
entry-class  The entries are divided into classes according to their
             entry type, see `bibtex-sort-entry-class'.  Within each class
             the entries are sorted alphabetically.
See also `bibtex-sort-ignore-string-entries'."
  :group 'bibtex
  :type '(choice (const nil)
                 (const plain)
                 (const crossref)
                 (const entry-class)
                 (const t)))
(put 'bibtex-maintain-sorted-entries 'safe-local-variable
     (lambda (a) (memq a '(nil t plain crossref entry-class))))

(defcustom bibtex-sort-entry-class
  '(("String")
    (catch-all)
    ("Book" "Proceedings"))
  "List of classes of BibTeX entry types, used for sorting entries.
If value of `bibtex-maintain-sorted-entries' is `entry-class'
entries are ordered according to the classes they belong to.  Each
class contains a list of entry types.  An entry `catch-all' applies
to all entries not explicitly mentioned."
  :group 'bibtex
  :type '(repeat (choice :tag "Class"
                         (const :tag "catch-all" (catch-all))
                         (repeat :tag "Entry type" string))))
(put 'bibtex-sort-entry-class 'safe-local-variable
     (lambda (x) (let ((OK t))
              (while (consp x)
                (let ((y (pop x)))
                  (while (consp y)
                    (let ((z (pop y)))
                      (unless (or (stringp z) (eq z 'catch-all))
                        (setq OK nil))))
                  (unless (null y) (setq OK nil))))
              (unless (null x) (setq OK nil))
              OK)))

(defcustom bibtex-sort-ignore-string-entries t
  "If non-nil, BibTeX @String entries are not sort-significant.
That means they are ignored when determining ordering of the buffer
\(e.g., sorting, locating alphabetical position for new entries, etc.)."
  :group 'bibtex
  :type 'boolean)

(defcustom bibtex-field-kill-ring-max 20
  "Max length of `bibtex-field-kill-ring' before discarding oldest elements."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-entry-kill-ring-max 20
  "Max length of `bibtex-entry-kill-ring' before discarding oldest elements."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-parse-keys-timeout 60
  "Time interval in seconds for parsing BibTeX buffers during idle time.
Parsing initializes `bibtex-reference-keys' and `bibtex-strings'."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-parse-keys-fast t
  "If non-nil, use fast but simplified algorithm for parsing BibTeX keys.
If parsing fails, try to set this variable to nil."
  :group 'bibtex
  :type 'boolean)

(define-widget 'bibtex-entry-alist 'lazy
  "Format of `bibtex-BibTeX-entry-alist' and friends."
  :type '(repeat (group (string :tag "Entry type")
                        (string :tag "Documentation")
                        (repeat :tag "Required fields"
                                (group (string :tag "Field")
                                       (option (choice :tag "Comment" :value nil
                                                       (const nil) string))
                                       (option (choice :tag "Init" :value nil
                                                       (const nil) string function))
                                       (option (choice :tag "Alternative" :value nil
                                                       (const nil) integer))))
                        (repeat :tag "Crossref fields"
                                (group (string :tag "Field")
                                       (option (choice :tag "Comment" :value nil
                                                       (const nil) string))
                                       (option (choice :tag "Init" :value nil
                                                       (const nil) string function))
                                       (option (choice :tag "Alternative" :value nil
                                                       (const nil) integer))))
                        (repeat :tag "Optional fields"
                                (group (string :tag "Field")
                                       (option (choice :tag "Comment" :value nil
                                                       (const nil) string))
                                       (option (choice :tag "Init" :value nil
                                                       (const nil) string function)))))))

(define-obsolete-variable-alias 'bibtex-entry-field-alist
  'bibtex-BibTeX-entry-alist "24.1")
(defcustom bibtex-BibTeX-entry-alist
  '(("Article" "Article in Journal"
     (("author")
      ("title" "Title of the article (BibTeX converts it to lowercase)"))
     (("journal") ("year"))
     (("volume" "Volume of the journal")
      ("number" "Number of the journal (only allowed if entry contains volume)")
      ("pages" "Pages in the journal")
      ("month") ("note")))
    ("InProceedings" "Article in Conference Proceedings"
     (("author")
      ("title" "Title of the article in proceedings (BibTeX converts it to lowercase)"))
     (("booktitle" "Name of the conference proceedings")
      ("year"))
     (("editor")
      ("volume" "Volume of the conference proceedings in the series")
      ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
      ("series" "Series in which the conference proceedings appeared")
      ("pages" "Pages in the conference proceedings")
      ("month") ("address")
      ("organization" "Sponsoring organization of the conference")
      ("publisher" "Publishing company, its location")
      ("note")))
    ("InCollection" "Article in a Collection"
     (("author")
      ("title" "Title of the article in book (BibTeX converts it to lowercase)")
      ("booktitle" "Name of the book"))
     (("publisher") ("year"))
     (("editor")
      ("volume" "Volume of the book in the series")
      ("number" "Number of the book in a small series (overwritten by volume)")
      ("series" "Series in which the book appeared")
      ("type" "Word to use instead of \"chapter\"")
      ("chapter" "Chapter in the book")
      ("pages" "Pages in the book")
      ("edition" "Edition of the book as a capitalized English word")
      ("month") ("address") ("note")))
    ("InBook" "Chapter or Pages in a Book"
     (("author" nil nil 0)
      ("editor" nil nil 0)
      ("title" "Title of the book")
      ("chapter" "Chapter in the book"))
     (("publisher") ("year"))
     (("volume" "Volume of the book in the series")
      ("number" "Number of the book in a small series (overwritten by volume)")
      ("series" "Series in which the book appeared")
      ("type" "Word to use instead of \"chapter\"")
      ("address")
      ("edition" "Edition of the book as a capitalized English word")
      ("month")
      ("pages" "Pages in the book")
      ("note")))
    ("Proceedings" "Conference Proceedings"
     (("title" "Title of the conference proceedings")
      ("year"))
     nil
     (("booktitle" "Title of the proceedings for cross references")
      ("editor")
      ("volume" "Volume of the conference proceedings in the series")
      ("number" "Number of the conference proceedings in a small series (overwritten by volume)")
      ("series" "Series in which the conference proceedings appeared")
      ("address")
      ("month")
      ("organization" "Sponsoring organization of the conference")
      ("publisher" "Publishing company, its location")
      ("note")))
    ("Book" "Book"
     (("author" nil nil 0)
      ("editor" nil nil 0)
      ("title" "Title of the book"))
     (("publisher") ("year"))
     (("volume" "Volume of the book in the series")
      ("number" "Number of the book in a small series (overwritten by volume)")
      ("series" "Series in which the book appeared")
      ("address")
      ("edition" "Edition of the book as a capitalized English word")
      ("month") ("note")))
    ("Booklet" "Booklet (Bound, but no Publisher)"
     (("title" "Title of the booklet (BibTeX converts it to lowercase)"))
     nil
     (("author")
      ("howpublished" "The way in which the booklet was published")
      ("address") ("month") ("year") ("note")))
    ("PhdThesis" "PhD. Thesis"
     (("author")
      ("title" "Title of the PhD. thesis")
      ("school" "School where the PhD. thesis was written")
      ("year"))
     nil
     (("type" "Type of the PhD. thesis")
      ("address" "Address of the school (if not part of field \"school\") or country")
      ("month") ("note")))
    ("MastersThesis" "Master's Thesis"
     (("author")
      ("title" "Title of the master's thesis (BibTeX converts it to lowercase)")
      ("school" "School where the master's thesis was written")
      ("year"))
     nil
     (("type" "Type of the master's thesis (if other than \"Master's thesis\")")
      ("address" "Address of the school (if not part of field \"school\") or country")
      ("month") ("note")))
    ("TechReport" "Technical Report"
     (("author")
      ("title" "Title of the technical report (BibTeX converts it to lowercase)")
      ("institution" "Sponsoring institution of the report")
      ("year"))
     nil
     (("type" "Type of the report (if other than \"technical report\")")
      ("number" "Number of the technical report")
      ("address") ("month") ("note")))
    ("Manual" "Technical Manual"
     (("title" "Title of the manual"))
     nil
     (("author")
      ("organization" "Publishing organization of the manual")
      ("address")
      ("edition" "Edition of the manual as a capitalized English word")
      ("month") ("year") ("note")))
    ("Unpublished" "Unpublished"
     (("author")
      ("title" "Title of the unpublished work (BibTeX converts it to lowercase)")
      ("note"))
     nil
     (("month") ("year")))
    ("Misc" "Miscellaneous" nil nil
     (("author")
      ("title" "Title of the work (BibTeX converts it to lowercase)")
      ("howpublished" "The way in which the work was published")
      ("month") ("year") ("note"))))
  "Alist of BibTeX entry types and their associated fields.
Elements are lists (ENTRY-TYPE DOC REQUIRED CROSSREF OPTIONAL).
ENTRY-TYPE is the type of a BibTeX entry.
DOC is a brief doc string used for menus. If nil ENTRY-TYPE is used.
REQUIRED is a list of required fields.
CROSSREF is a list of fields that are optional if a crossref field
is present; but these fields are required otherwise.
OPTIONAL is a list of optional fields.

Each element of these lists is a list of the form
  \(FIELD COMMENT INIT ALTERNATIVE).
COMMENT, INIT, and ALTERNATIVE are optional.

FIELD is the name of the field.
COMMENT is the comment string that appears in the echo area.
If COMMENT is nil use `bibtex-BibTeX-field-alist' if possible.
INIT is either the initial content of the field or a function,
which is called to determine the initial content of the field.
ALTERNATIVE if non-nil is an integer that numbers sets of
alternatives, starting from zero."
  :group 'BibTeX
  :version "24.1"
  :type 'bibtex-entry-alist)
(put 'bibtex-BibTeX-entry-alist 'risky-local-variable t)

(defcustom bibtex-biblatex-entry-alist
  ;; Compare in biblatex documentation:
  ;; Sec. 2.1.1  Regular types (required and optional fields)
  ;; Appendix A  Default Crossref setup
  '(("Article" "Article in Journal"
     (("author") ("title") ("journaltitle")
      ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("translator") ("annotator") ("commentator") ("subtitle") ("titleaddon")
      ("editor") ("editora") ("editorb") ("editorc")
      ("journalsubtitle") ("issuetitle") ("issuesubtitle")
      ("language") ("origlanguage") ("series") ("volume") ("number") ("eid")
      ("issue") ("month") ("pages") ("version") ("note") ("issn")
      ("addendum") ("pubstate") ("doi") ("eprint") ("eprintclass")
      ("eprinttype") ("url") ("urldate")))
    ("Book" "Single-Volume Book"
     (("author") ("title") ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("editor") ("editora") ("editorb") ("editorc")
      ("translator") ("annotator") ("commentator")
      ("introduction") ("foreword") ("afterword") ("titleaddon")
      ("maintitle") ("mainsubtitle") ("maintitleaddon")
      ("language") ("origlanguage") ("volume") ("part") ("edition") ("volumes")
      ("series") ("number") ("note") ("publisher") ("location") ("isbn")
      ("chapter") ("pages") ("pagetotal") ("addendum") ("pubstate") ("doi")
      ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("MVBook" "Multi-Volume Book"
     (("author") ("title") ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("editor") ("editora") ("editorb") ("editorc")
      ("translator") ("annotator") ("commentator")
      ("introduction") ("foreword") ("afterword") ("subtitle")
      ("titleaddon") ("language") ("origlanguage") ("edition") ("volumes")
      ("series") ("number") ("note") ("publisher")
      ("location") ("isbn") ("pagetotal") ("addendum") ("pubstate") ("doi")
      ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("InBook" "Chapter or Pages in a Book"
     (("title") ("year" nil nil 0) ("date" nil nil 0))
     (("author") ("booktitle"))
     (("bookauthor") ("editor") ("editora") ("editorb") ("editorc")
      ("translator") ("annotator") ("commentator") ("introduction") ("foreword")
      ("afterword") ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
      ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
      ("language") ("origlanguage") ("volume") ("part") ("edition") ("volumes")
      ("series") ("number") ("note") ("publisher") ("location") ("isbn")
      ("chapter") ("pages") ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("BookInBook" "Book in Collection" ; same as @inbook
     (("title") ("year" nil nil 0) ("date" nil nil 0))
     (("author") ("booktitle"))
     (("bookauthor") ("editor") ("editora") ("editorb") ("editorc")
      ("translator") ("annotator") ("commentator") ("introduction") ("foreword")
      ("afterword") ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
      ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
      ("language") ("origlanguage") ("volume") ("part") ("edition") ("volumes")
      ("series") ("number") ("note") ("publisher") ("location") ("isbn")
      ("chapter") ("pages") ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("SuppBook" "Supplemental Material in a Book" ; same as @inbook
     (("title") ("year" nil nil 0) ("date" nil nil 0))
     (("author") ("booktitle"))
     (("bookauthor") ("editor") ("editora") ("editorb") ("editorc")
      ("translator") ("annotator") ("commentator") ("introduction") ("foreword")
      ("afterword") ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
      ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
      ("language") ("origlanguage") ("volume") ("part") ("edition") ("volumes")
      ("series") ("number") ("note") ("publisher") ("location") ("isbn")
      ("chapter") ("pages") ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("Booklet" "Booklet (Bound, but no Publisher)"
     (("author" nil nil 0) ("editor" nil nil 0) ("title")
      ("year" nil nil 1) ("date" nil nil 1))
     nil
     (("subtitle") ("titleaddon") ("language") ("howpublished") ("type")
      ("note") ("location") ("chapter") ("pages") ("pagetotal") ("addendum")
      ("pubstate") ("doi") ("eprint") ("eprintclass") ("eprinttype")
      ("url") ("urldate")))
    ("Collection" "Single-Volume Collection"
     (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("editora") ("editorb") ("editorc") ("translator") ("annotator")
      ("commentator") ("introduction") ("foreword") ("afterword")
      ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
      ("maintitleaddon") ("language") ("origlanguage") ("volume")
      ("part") ("edition") ("volumes") ("series") ("number") ("note")
      ("publisher") ("location") ("isbn") ("chapter") ("pages") ("pagetotal")
      ("addendum") ("pubstate") ("doi") ("eprint") ("eprintclass")
      ("eprinttype") ("url") ("urldate")))
    ("MVCollection" "Multi-Volume Collection"
     (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("editora") ("editorb") ("editorc") ("translator") ("annotator")
      ("commentator") ("introduction") ("foreword") ("afterword")
      ("subtitle") ("titleaddon") ("language") ("origlanguage") ("edition")
      ("volumes") ("series") ("number") ("note") ("publisher")
      ("location") ("isbn") ("pagetotal") ("addendum") ("pubstate") ("doi")
      ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("InCollection" "Article in a Collection"
     (("author") ("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
     (("booktitle"))
     (("editora") ("editorb") ("editorc") ("translator") ("annotator")
      ("commentator") ("introduction") ("foreword") ("afterword")
      ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
      ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
      ("language") ("origlanguage") ("volume") ("part") ("edition")
      ("volumes") ("series") ("number") ("note") ("publisher") ("location")
      ("isbn") ("chapter") ("pages") ("addendum") ("pubstate") ("doi")
      ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("SuppCollection" "Supplemental Material in a Collection" ; same as @incollection
     (("author") ("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
     (("booktitle"))
     (("editora") ("editorb") ("editorc") ("translator") ("annotator")
      ("commentator") ("introduction") ("foreword") ("afterword")
      ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
      ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
      ("language") ("origlanguage") ("volume") ("part") ("edition")
      ("volumes") ("series") ("number") ("note") ("publisher") ("location")
      ("isbn") ("chapter") ("pages") ("addendum") ("pubstate") ("doi")
      ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("Manual" "Technical Manual"
     (("author" nil nil 0) ("editor" nil nil 0) ("title")
      ("year" nil nil 1) ("date" nil nil 1))
     nil
     (("subtitle") ("titleaddon") ("language") ("edition")
      ("type") ("series") ("number") ("version") ("note")
      ("organization") ("publisher") ("location") ("isbn") ("chapter")
      ("pages") ("pagetotal") ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("Misc" "Miscellaneous"
     (("author" nil nil 0) ("editor" nil nil 0) ("title")
      ("year" nil nil 1) ("date" nil nil 1))
     nil
     (("subtitle") ("titleaddon") ("language") ("howpublished") ("type")
      ("version") ("note") ("organization") ("location")
      ("date") ("month") ("year") ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("Online" "Online Resource"
     (("author" nil nil 0) ("editor" nil nil 0) ("title")
      ("year" nil nil 1) ("date" nil nil 1) ("url"))
     nil
     (("subtitle") ("titleaddon") ("language") ("version") ("note")
      ("organization") ("date") ("month") ("year") ("addendum")
      ("pubstate") ("urldate")))
    ("Patent" "Patent"
     (("author") ("title") ("number") ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("holder") ("subtitle") ("titleaddon") ("type") ("version") ("location")
      ("note") ("date") ("month") ("year") ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("Periodical" "Complete Issue of a Periodical"
     (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("editora") ("editorb") ("editorc") ("subtitle") ("issuetitle")
      ("issuesubtitle") ("language") ("series") ("volume") ("number") ("issue")
      ("date") ("month") ("year") ("note") ("issn") ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("SuppPeriodical" "Supplemental Material in a Periodical" ; same as @article
     (("author") ("title") ("journaltitle")
      ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("translator") ("annotator") ("commentator") ("subtitle") ("titleaddon")
      ("editor") ("editora") ("editorb") ("editorc")
      ("journalsubtitle") ("issuetitle") ("issuesubtitle")
      ("language") ("origlanguage") ("series") ("volume") ("number") ("eid")
      ("issue") ("month") ("pages") ("version") ("note") ("issn")
      ("addendum") ("pubstate") ("doi") ("eprint") ("eprintclass")
      ("eprinttype") ("url") ("urldate")))
    ("Proceedings" "Single-Volume Conference Proceedings"
     (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
      ("maintitleaddon") ("eventtitle") ("eventdate") ("venue") ("language")
      ("volume") ("part") ("volumes") ("series") ("number") ("note")
      ("organization") ("publisher") ("location") ("month")
      ("isbn") ("chapter") ("pages") ("pagetotal") ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("MVProceedings" "Multi-Volume Conference Proceedings"
     (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("subtitle") ("titleaddon") ("eventtitle") ("eventdate") ("venue")
      ("language") ("volumes") ("series") ("number") ("note")
      ("organization") ("publisher") ("location") ("month")
      ("isbn") ("pagetotal") ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("InProceedings" "Article in Conference Proceedings"
     (("author") ("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
     (("booktitle"))
     (("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
      ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
      ("eventtitle") ("eventdate") ("venue") ("language")
      ("volume") ("part") ("volumes") ("series") ("number") ("note")
      ("organization") ("publisher") ("location") ("month") ("isbn")
      ("chapter") ("pages") ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("Reference" "Single-Volume Work of Reference" ; same as @collection
     (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("editora") ("editorb") ("editorc") ("translator") ("annotator")
      ("commentator") ("introduction") ("foreword") ("afterword")
      ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
      ("maintitleaddon") ("language") ("origlanguage") ("volume")
      ("part") ("edition") ("volumes") ("series") ("number") ("note")
      ("publisher") ("location") ("isbn") ("chapter") ("pages") ("pagetotal")
      ("addendum") ("pubstate") ("doi") ("eprint") ("eprintclass")
      ("eprinttype") ("url") ("urldate")))
    ("MVReference" "Multi-Volume Work of Reference" ; same as @mvcollection
     (("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("editora") ("editorb") ("editorc") ("translator") ("annotator")
      ("commentator") ("introduction") ("foreword") ("afterword")
      ("subtitle") ("titleaddon") ("language") ("origlanguage") ("edition")
      ("volumes") ("series") ("number") ("note") ("publisher")
      ("location") ("isbn") ("pagetotal") ("addendum") ("pubstate") ("doi")
      ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("InReference" "Article in a Work of Reference" ; same as @incollection
     (("author") ("editor") ("title") ("year" nil nil 0) ("date" nil nil 0))
     (("booktitle"))
     (("editora") ("editorb") ("editorc") ("translator") ("annotator")
      ("commentator") ("introduction") ("foreword") ("afterword")
      ("subtitle") ("titleaddon") ("maintitle") ("mainsubtitle")
      ("maintitleaddon") ("booksubtitle") ("booktitleaddon")
      ("language") ("origlanguage") ("volume") ("part") ("edition")
      ("volumes") ("series") ("number") ("note") ("publisher") ("location")
      ("isbn") ("chapter") ("pages") ("addendum") ("pubstate") ("doi")
      ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("Report" "Technical or Research Report"
     (("author") ("title") ("type") ("institution")
      ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("subtitle") ("titleaddon") ("language") ("number") ("version") ("note")
      ("location") ("month") ("isrn") ("chapter") ("pages") ("pagetotal")
      ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("Thesis" "PhD. or Master's Thesis"
     (("author") ("title") ("type") ("institution")
      ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("subtitle") ("titleaddon") ("language") ("note") ("location")
      ("month") ("isbn") ("chapter") ("pages") ("pagetotal")
      ("addendum") ("pubstate")
      ("doi") ("eprint") ("eprintclass") ("eprinttype") ("url") ("urldate")))
    ("Unpublished" "Unpublished"
     (("author") ("title") ("year" nil nil 0) ("date" nil nil 0))
     nil
     (("subtitle") ("titleaddon") ("language") ("howpublished")
      ("note") ("location") ("isbn") ("date") ("month") ("year")
      ("addendum") ("pubstate") ("url") ("urldate"))))
  "Alist of biblatex entry types and their associated fields.
It has the same format as `bibtex-BibTeX-entry-alist'."
  :group 'bibtex
  :version "24.1"
  :type 'bibtex-entry-alist)
(put 'bibtex-biblatex-entry-alist 'risky-local-variable t)

(define-widget 'bibtex-field-alist 'lazy
  "Format of `bibtex-BibTeX-entry-alist' and friends."
  :type '(repeat (group (string :tag "Field type")
                        (string :tag "Comment"))))

(defcustom bibtex-BibTeX-field-alist
  '(("author" "Author1 [and Author2 ...] [and others]")
    ("editor" "Editor1 [and Editor2 ...] [and others]")
    ("journal" "Name of the journal (use string, remove braces)")
    ("year" "Year of publication")
    ("month" "Month of the publication as a string (remove braces)")
    ("note" "Remarks to be put at the end of the \\bibitem")
    ("publisher" "Publishing company")
    ("address" "Address of the publisher"))
    "Alist of BibTeX fields.
Each element is a list (FIELD COMMENT).  COMMENT is used as a default
if `bibtex-BibTeX-entry-alist' does not define a comment for FIELD."
  :group 'bibtex
  :version "24.1"
  :type 'bibtex-field-alist)

(defcustom bibtex-biblatex-field-alist
  ;; See 2.2.2 Data Fields
  '(("abstract" "Abstract of the work")
    ("addendum" "Miscellaneous bibliographic data")
    ("afterword" "Author(s) of an afterword to the work")
    ("annotation" "Annotation")
    ("annotator" "Author(s) of annotations to the work")
    ("author" "Author(s) of the title")
    ("bookauthor" "Author(s) of the booktitle.")
    ("bookpagination" "Pagination scheme of the enclosing work")
    ("booksubtitle" "Subtitle related to the booktitle")
    ("booktitle" "Title of the book")
    ("booktitleaddon" "Annex to the booktitle")
    ("chapter" "Chapter, section, or any other unit of a work")
    ("commentator" "Author(s) of a commentary to the work")
    ("date" "Publication date")
    ("doi" "Digital Object Identifier")
    ("edition" "Edition of a printed publication")
    ("editor" "Editor(s) of the title, booktitle, or maintitle")
    ("editora" "Secondary editor")
    ("editorb" "Secondary editor")
    ("editorc" "Secondary editor")
    ("editortype" "Type of editorial role performed by the editor")
    ("editoratype" "Type of editorial role performed by editora")
    ("editorbtype" "Type of editorial role performed by editorb")
    ("editorctype" "Type of editorial role performed by editorc")
    ("eid" "Electronic identifier of an article")
    ("eprint" "Electronic identifier of an online publication")
    ("eprintclass" "Additional information related to the eprinttype")
    ("eprinttype" "Type of eprint identifier")
    ("eventdate" "Date of a conference or some other event")
    ("eventtitle" "Title of a conference or some other event")
    ("file" "Local link to an electronic version of the work")
    ("foreword" "Author(s) of a foreword to the work")
    ("holder" "Holder(s) of a patent")
    ("howpublished" "Publication notice for unusual publications")
    ("indextitle" "Title to use for indexing instead of the regular title")
    ("institution" "Name of a university or some other institution")
    ("introduction" "Author(s) of an introduction to the work")
    ("isan" "International Standard Audiovisual Number of an audiovisual work")
    ("isbn" "International Standard Book Number of a book.")
    ("ismn" "International Standard Music Number for printed music")
    ("isrn" "International Standard Technical Report Number")
    ("issn" "International Standard Serial Number of a periodical.")
    ("issue" "Issue of a journal")
    ("issuesubtitle" "Subtitle of a specific issue of a journal or other periodical.")
    ("issuetitle" "Title of a specific issue of a journal or other periodical.")
    ("iswc" "International Standard Work Code of a musical work")
    ("journalsubtitle" "Subtitle of a journal, a newspaper, or some other periodical.")
    ("journaltitle" "Name of a journal, a newspaper, or some other periodical.")
    ("label" "Substitute for the regular label to be used by the citation style")
    ("language" "Language(s) of the work")
    ("library" "Library name and a call number")
    ("location" "Place(s) of publication")
    ("mainsubtitle" "Subtitle related to the maintitle")
    ("maintitle" "Main title of a multi-volume book, such as Collected Works")
    ("maintitleaddon" "Annex to the maintitle")
    ("month" "Publication month")
    ("nameaddon" "Addon to be printed immediately after the author name")
    ("note" "Miscellaneous bibliographic data")
    ("number" "Number of a journal or the volume/number of a book in a series")
    ("organization" "Organization(s) that published a work")
    ("origdate" "Publication date of the original edition")
    ("origlanguage" "Original publication language of a translated edition")
    ("origlocation" "Location of the original edition")
    ("origpublisher" "Publisher of the original edition")
    ("origtitle" "Title of the original work")
    ("pages" "Page number(s) or page range(s)")
    ("pagetotal" "Total number of pages of the work.")
    ("pagination" "Pagination of the work")
    ("part" "Number of a partial volume")
    ("publisher" "Name(s) of the publisher(s)")
    ("pubstate" "Publication state of the work, e. g.,'in press'")
    ("reprinttitle" "Title of a reprint of the work")
    ("series" "Name of a publication series")
    ("shortauthor" "Author(s) of the work, given in an abbreviated form")
    ("shorteditor" "Editor(s) of the work, given in an abbreviated form")
    ("shortjournal" "Short version or an acronym of the journal title")
    ("shortseries" "Short version or an acronym of the series field")
    ("shorttitle" "Title in an abridged form")
    ("subtitle" "Subtitle of the work")
    ("title" "Title of the work")
    ("titleaddon" "Annex to the title")
    ("translator" "Translator(s) of the work")
    ("type" "Type of a manual, patent, report, or thesis")
    ("url" " URL of an online publication.")
    ("urldate" "Access date of the address specified in the url field")
    ("venue" "Location of a conference, a symposium, or some other event")
    ("version" "Revision number of a piece of software, a manual, etc.")
    ("volume" "Volume of a multi-volume book or a periodical")
    ("volumes" "Total number of volumes of a multi-volume work")
    ("year" "Year of publication"))
    "Alist of biblatex fields.
It has the same format as `bibtex-BibTeX-entry-alist'."
  :group 'bibtex
  :version "24.1"
  :type 'bibtex-field-alist)

(defcustom bibtex-dialect-list '(BibTeX biblatex)
  "List of BibTeX dialects known to BibTeX mode.
For each DIALECT (a symbol) a variable bibtex-DIALECT-entry-alist defines
the allowed entries and bibtex-DIALECT-field-alist defines known field types.
Predefined dialects include BibTeX and biblatex."
  :group 'bibtex
  :version "24.1"
  :type '(repeat (symbol :tag "Dialect")))

(defcustom bibtex-dialect 'BibTeX
  "Current BibTeX dialect.  For allowed values see `bibtex-dialect-list'.
To interactively change the dialect use the command `bibtex-set-dialect'."
  :group 'bibtex
  :version "24.1"
  :set '(lambda (symbol value)
          (set-default symbol value)
          ;; `bibtex-set-dialect' is undefined during loading (no problem)
          (if (fboundp 'bibtex-set-dialect)
              (bibtex-set-dialect value)))
  :type '(choice (const BibTeX)
                 (const biblatex)
                 (symbol :tag "Custom")))
(put 'bibtex-dialect 'safe-local-variable 'symbolp)

(defcustom bibtex-no-opt-remove-re "\\`option"
  "If a field name matches this regexp, the prefix OPT is not removed.
If nil prefix OPT is always removed"
  :group 'bibtex
  :version "24.1"
  :type '(choice (regexp) (const nil)))

(defcustom bibtex-comment-start "@Comment"
  "String starting a BibTeX comment."
  :group 'bibtex
  :type 'string)

(defcustom bibtex-add-entry-hook nil
  "List of functions to call when BibTeX entry has been inserted."
  :group 'bibtex
  :type 'hook)

(defcustom bibtex-predefined-month-strings
  '(("jan" . "January")
    ("feb" . "February")
    ("mar" . "March")
    ("apr" . "April")
    ("may" . "May")
    ("jun" . "June")
    ("jul" . "July")
    ("aug" . "August")
    ("sep" . "September")
    ("oct" . "October")
    ("nov" . "November")
    ("dec" . "December"))
  "Alist of month string definitions used in the BibTeX style files.
Each element is a pair of strings (ABBREVIATION . EXPANSION)."
  :group 'bibtex
  :type '(repeat (cons (string :tag "Month abbreviation")
                       (string :tag "Month expansion"))))

(defcustom bibtex-predefined-strings
  (append
   bibtex-predefined-month-strings
   '(("acmcs"    . "ACM Computing Surveys")
     ("acta"     . "Acta Informatica")
     ("cacm"     . "Communications of the ACM")
     ("ibmjrd"   . "IBM Journal of Research and Development")
     ("ibmsj"    . "IBM Systems Journal")
     ("ieeese"   . "IEEE Transactions on Software Engineering")
     ("ieeetc"   . "IEEE Transactions on Computers")
     ("ieeetcad" . "IEEE Transactions on Computer-Aided Design of Integrated Circuits")
     ("ipl"      . "Information Processing Letters")
     ("jacm"     . "Journal of the ACM")
     ("jcss"     . "Journal of Computer and System Sciences")
     ("scp"      . "Science of Computer Programming")
     ("sicomp"   . "SIAM Journal on Computing")
     ("tcs"      . "Theoretical Computer Science")
     ("tocs"     . "ACM Transactions on Computer Systems")
     ("tods"     . "ACM Transactions on Database Systems")
     ("tog"      . "ACM Transactions on Graphics")
     ("toms"     . "ACM Transactions on Mathematical Software")
     ("toois"    . "ACM Transactions on Office Information Systems")
     ("toplas"   . "ACM Transactions on Programming Languages and Systems")))
  "Alist of string definitions used in the BibTeX style files.
Each element is a pair of strings (ABBREVIATION . EXPANSION)."
  :group 'bibtex
  :type '(repeat (cons (string :tag "String")
                       (string :tag "String expansion"))))

(defcustom bibtex-string-files nil
  "List of BibTeX files containing string definitions.
List elements can be absolute file names or file names relative
to the directories specified in `bibtex-string-file-path'."
  :group 'bibtex
  :type '(repeat file))

(defvar bibtex-string-file-path (getenv "BIBINPUTS")
  "*Colon separated list of paths to search for `bibtex-string-files'.")

(defcustom bibtex-files nil
  "List of BibTeX files that are searched for entry keys.
List elements can be absolute file names or file names relative to the
directories specified in `bibtex-file-path'.  If an element is a directory,
check all BibTeX files in this directory.  If an element is the symbol
`bibtex-file-path', check all BibTeX files in `bibtex-file-path'.
See also `bibtex-search-entry-globally'."
  :group 'bibtex
  :type '(repeat (choice (const :tag "bibtex-file-path" bibtex-file-path)
                         directory file)))

(defvar bibtex-file-path (getenv "BIBINPUTS")
  "*Colon separated list of paths to search for `bibtex-files'.")

(defcustom bibtex-search-entry-globally nil
  "If non-nil, interactive calls of `bibtex-search-entry' search globally.
A global search includes all files in `bibtex-files'."
  :group 'bibtex
  :version "24.1"
  :type 'boolean)

(defcustom bibtex-help-message t
  "If non-nil print help messages in the echo area on entering a new field."
  :group 'bibtex
  :type 'boolean)

(defcustom bibtex-autokey-prefix-string ""
  "String prefix for automatically generated reference keys.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'string)

(defcustom bibtex-autokey-names 1
  "Number of names to use for the automatically generated reference key.
Possibly more names are used according to `bibtex-autokey-names-stretch'.
If this variable is nil, all names are used.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(choice (const :tag "All" infty)
                 integer))

(defcustom bibtex-autokey-names-stretch 0
  "Number of names that can additionally be used for reference keys.
These names are used only, if all names are used then.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'integer)

(defcustom bibtex-autokey-additional-names ""
  "String to append to the generated key if not all names could be used.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'string)

(defcustom bibtex-autokey-expand-strings nil
  "If non-nil, expand strings when extracting the content of a BibTeX field.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'boolean)

(defvar bibtex-autokey-transcriptions
  '(;; language specific characters
    ("\\\\aa" . "a")                      ; \aa           -> a
    ("\\\\AA" . "A")                      ; \AA           -> A
    ("\\\"a\\|\\\\\\\"a\\|\\\\ae" . "ae") ; "a,\"a,\ae    -> ae
    ("\\\"A\\|\\\\\\\"A\\|\\\\AE" . "Ae") ; "A,\"A,\AE    -> Ae
    ("\\\\i" . "i")                       ; \i            -> i
    ("\\\\j" . "j")                       ; \j            -> j
    ("\\\\l" . "l")                       ; \l            -> l
    ("\\\\L" . "L")                       ; \L            -> L
    ("\\\"o\\|\\\\\\\"o\\|\\\\o\\|\\\\oe" . "oe") ; "o,\"o,\o,\oe -> oe
    ("\\\"O\\|\\\\\\\"O\\|\\\\O\\|\\\\OE" . "Oe") ; "O,\"O,\O,\OE -> Oe
    ("\\\"s\\|\\\\\\\"s\\|\\\\3" . "ss")  ; "s,\"s,\3     -> ss
    ("\\\"u\\|\\\\\\\"u" . "ue")          ; "u,\"u        -> ue
    ("\\\"U\\|\\\\\\\"U" . "Ue")          ; "U,\"U        -> Ue
    ;; accents
    ("\\\\`\\|\\\\'\\|\\\\\\^\\|\\\\~\\|\\\\=\\|\\\\\\.\\|\\\\u\\|\\\\v\\|\\\\H\\|\\\\t\\|\\\\c\\|\\\\d\\|\\\\b" . "")
    ;; braces, quotes, concatenation.
    ("[`'\"{}#]" . "")
    ;; spaces
    ("\\\\?[ \t\n]+\\|~" . " "))
  "Alist of (OLD-REGEXP . NEW-STRING) pairs.
Used by the default values of `bibtex-autokey-name-change-strings' and
`bibtex-autokey-titleword-change-strings'.  Defaults to translating some
language specific characters to their ASCII transcriptions, and
removing any character accents.")

(defcustom bibtex-autokey-name-change-strings
  bibtex-autokey-transcriptions
  "Alist of (OLD-REGEXP . NEW-STRING) pairs.
Any part of a name matching OLD-REGEXP is replaced by NEW-STRING.
Case is significant in OLD-REGEXP.  All regexps are tried in the
order in which they appear in the list.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(repeat (cons (regexp :tag "Old")
                       (string :tag "New"))))

(defcustom bibtex-autokey-name-case-convert-function 'downcase
  "Function called for each name to perform case conversion.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(choice (const :tag "Preserve case" identity)
                 (const :tag "Downcase" downcase)
                 (const :tag "Capitalize" capitalize)
                 (const :tag "Upcase" upcase)
                 (function :tag "Conversion function")))
(put 'bibtex-autokey-name-case-convert-function 'safe-local-variable
     (lambda (x) (memq x '(upcase downcase capitalize identity))))
(defvaralias 'bibtex-autokey-name-case-convert
  'bibtex-autokey-name-case-convert-function)

(defcustom bibtex-autokey-name-length 'infty
  "Number of characters from name to incorporate into key.
If this is set to anything but a number, all characters are used.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(choice (const :tag "All" infty)
                 integer))

(defcustom bibtex-autokey-name-separator ""
  "String that comes between any two names in the key.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'string)

(defcustom bibtex-autokey-year-length 2
  "Number of rightmost digits from the year field to incorporate into key.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'integer)

(defcustom bibtex-autokey-use-crossref t
  "If non-nil use fields from crossreferenced entry if necessary.
If this variable is non-nil and some field has no entry, but a
valid crossref entry, the field from the crossreferenced entry is used.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'boolean)

(defcustom bibtex-autokey-titlewords 5
  "Number of title words to use for the automatically generated reference key.
If this is set to anything but a number, all title words are used.
Possibly more words from the title are used according to
`bibtex-autokey-titlewords-stretch'.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(choice (const :tag "All" infty)
                 integer))

(defcustom bibtex-autokey-title-terminators "[.!?:;]\\|--"
  "Regexp defining the termination of the main part of the title.
Case of the regexps is ignored.  See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'regexp)

(defcustom bibtex-autokey-titlewords-stretch 2
  "Number of words that can additionally be used from the title.
These words are used only, if a sentence from the title can be ended then.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'integer)

(defcustom bibtex-autokey-titleword-ignore
  '("A" "An" "On" "The" "Eine?" "Der" "Die" "Das"
    "[^[:upper:]].*" ".*[^[:upper:][:lower:]0-9].*")
  "Determines words from the title that are not to be used in the key.
Each item of the list is a regexp.  If a word of the title matches a
regexp from that list, it is not included in the title part of the key.
Case is significant.  See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(repeat regexp))

(defcustom bibtex-autokey-titleword-case-convert-function 'downcase
  "Function called for each titleword to perform case conversion.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(choice (const :tag "Preserve case" identity)
                 (const :tag "Downcase" downcase)
                 (const :tag "Capitalize" capitalize)
                 (const :tag "Upcase" upcase)
                 (function :tag "Conversion function")))
(defvaralias 'bibtex-autokey-titleword-case-convert
  'bibtex-autokey-titleword-case-convert-function)

(defcustom bibtex-autokey-titleword-abbrevs nil
  "Determines exceptions to the usual abbreviation mechanism.
An alist of (OLD-REGEXP . NEW-STRING) pairs.  Case is ignored
in matching against OLD-REGEXP, and the first matching pair is used.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(repeat (cons (regexp :tag "Old")
                       (string :tag "New"))))

(defcustom bibtex-autokey-titleword-change-strings
  bibtex-autokey-transcriptions
  "Alist of (OLD-REGEXP . NEW-STRING) pairs.
Any part of title word matching a OLD-REGEXP is replaced by NEW-STRING.
Case is significant in OLD-REGEXP.  All regexps are tried in the
order in which they appear in the list.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(repeat (cons (regexp :tag "Old")
                       (string :tag "New"))))

(defcustom bibtex-autokey-titleword-length 5
  "Number of characters from title words to incorporate into key.
If this is set to anything but a number, all characters are used.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type '(choice (const :tag "All" infty)
                 integer))

(defcustom bibtex-autokey-titleword-separator "_"
  "String to be put between the title words.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'string)

(defcustom bibtex-autokey-name-year-separator ""
  "String to be put between name part and year part of key.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'string)

(defcustom bibtex-autokey-year-title-separator ":_"
  "String to be put between year part and title part of key.
See `bibtex-generate-autokey' for details."
  :group 'bibtex-autokey
  :type 'string)

(defcustom bibtex-autokey-edit-before-use t
  "If non-nil, user is allowed to edit the generated key before it is used."
  :group 'bibtex-autokey
  :type 'boolean)

(defcustom bibtex-autokey-before-presentation-function nil
  "If non-nil, function to call before generated key is presented.
The function must take one argument (the automatically generated key),
and must return a string (the key to use)."
  :group 'bibtex-autokey
  :type '(choice (const nil) function))

(defcustom bibtex-entry-offset 0
  "Offset for BibTeX entries.
Added to the value of all other variables which determine columns."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-field-indentation 2
  "Starting column for the name part in BibTeX fields."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-text-indentation
  (+ bibtex-field-indentation
     (length "organization = "))
  "Starting column for the text part in BibTeX fields.
Should be equal to the space needed for the longest name part."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-contline-indentation
  (+ bibtex-text-indentation 1)
  "Starting column for continuation lines of BibTeX fields."
  :group 'bibtex
  :type 'integer)

(defcustom bibtex-align-at-equal-sign nil
  "If non-nil, align fields at equal sign instead of field text.
If non-nil, the column for the equal sign is the value of
`bibtex-text-indentation', minus 2."
  :group 'bibtex
  :type 'boolean)

(defcustom bibtex-comma-after-last-field nil
  "If non-nil, a comma is put at end of last field in the entry template."
  :group 'bibtex
  :type 'boolean)

(defcustom bibtex-autoadd-commas t
  "If non-nil automatically add missing commas at end of BibTeX fields."
  :group 'bibtex
  :type 'boolean)

(defcustom bibtex-autofill-types '("Proceedings")
  "Automatically fill fields if possible for those BibTeX entry types."
  :group 'bibtex
  :type '(repeat string))

(defcustom bibtex-summary-function 'bibtex-summary
  "Function to call for generating a summary of current BibTeX entry.
It takes no arguments.  Point must be at beginning of entry.
Used by `bibtex-complete-crossref-cleanup' and `bibtex-copy-summary-as-kill'."
  :group 'bibtex
  :type '(choice (const :tag "Default" bibtex-summary)
                 (function :tag "Personalized function")))

(defcustom bibtex-generate-url-list
  '((("url" . ".*:.*")))
  "List of schemes for generating the URL of a BibTeX entry.
These schemes are used by `bibtex-url'.

Each scheme should have one of these forms:

  ((FIELD . REGEXP))
  ((FIELD . REGEXP) STEP...)
  ((FIELD . REGEXP) STRING STEP...)

FIELD is a field name as returned by `bibtex-parse-entry'.
REGEXP is matched against the text of FIELD.  If the match succeeds,
then this scheme is used.  If no STRING and STEPs are specified
the matched text is used as the URL, otherwise the URL is built
by evaluating STEPs.  If no STRING is specified the STEPs must result
in strings which are concatenated.  Otherwise the resulting objects
are passed through `format' using STRING as format control string.

A STEP is a list (FIELD REGEXP REPLACE).  The text of FIELD
is matched against REGEXP, and is replaced with REPLACE.
REPLACE can be a string, or a number (which selects the corresponding
submatch), or a function called with the field's text as argument
and with the `match-data' properly set.

Case is always ignored.  Always remove the field delimiters.
If `bibtex-expand-strings' is non-nil, BibTeX strings are expanded
for generating the URL.
Set this variable before loading BibTeX mode.

The following is a complex example, see URL `http://link.aps.org/'.

   (((\"journal\" . \"\\\\=<\\(PR[ABCDEL]?\\|RMP\\)\\\\=>\")
     \"http://link.aps.org/abstract/%s/v%s/p%s\"
     (\"journal\" \".*\" upcase)
     (\"volume\" \".*\" 0)
     (\"pages\" \"\\`[A-Z]?[0-9]+\" 0)))"
  :group 'bibtex
  :type '(repeat
          (cons :tag "Scheme"
                (cons :tag "Matcher" :extra-offset 4
                      (string :tag "BibTeX field")
		      (regexp :tag "Regexp"))
                (choice
                 (const :tag "Take match as is" nil)
                 (cons :tag "Formatted"
                  (string :tag "Format control string")
                  (repeat :tag "Steps to generate URL"
                          (list (string :tag "BibTeX field")
                                (regexp :tag "Regexp")
                                (choice (string :tag "Replacement")
                                        (integer :tag "Sub-match")
                                        (function :tag "Filter")))))
                 (repeat :tag "Concatenated"
                         (list (string :tag "BibTeX field")
			       (regexp :tag "Regexp")
                               (choice (string :tag "Replacement")
				       (integer :tag "Sub-match")
				       (function :tag "Filter"))))))))
(put 'bibtex-generate-url-list 'risky-local-variable t)

(defcustom bibtex-cite-matcher-alist
  '(("\\\\cite[ \t\n]*{\\([^}]+\\)}" . 1))
  "Alist of rules to identify cited keys in a BibTeX entry.
Each rule should be of the form (REGEXP . SUBEXP), where SUBEXP
specifies which parenthesized expression in REGEXP is a cited key.
Case is significant.
Used by `bibtex-search-crossref' and for font-locking.
Set this variable before loading BibTeX mode."
  :group 'bibtex
  :type '(repeat (cons (regexp :tag "Regexp")
                       (integer :tag "Number")))
  :version "23.1")

(defcustom bibtex-expand-strings nil
  "If non-nil, expand strings when extracting the content of a BibTeX field."
  :group 'bibtex
  :type 'boolean)

(defcustom bibtex-search-buffer "*BibTeX Search*"
  "Buffer for BibTeX search results."
  :group 'bibtex
  :version "24.1"
  :type 'string)

;; `bibtex-font-lock-keywords' is a user option, too.  But since the
;; patterns used to define this variable are defined in a later
;; section of this file, it is defined later.


;; Syntax Table and Keybindings
(defvar bibtex-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?$ "$$  " st)
    (modify-syntax-entry ?% "<   " st)
    (modify-syntax-entry ?' "w   " st)	;FIXME: Not allowed in @string keys.
    (modify-syntax-entry ?@ "w   " st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\f ">   " st)
    (modify-syntax-entry ?\n ">   " st)
    ;; Keys cannot have = in them (wrong font-lock of @string{foo=bar}).
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?~ " " st)
    st)
  "Syntax table used in BibTeX mode buffers.")

(defvar bibtex-mode-map
  (let ((km (make-sparse-keymap)))
    ;; The Key `C-c&' is reserved for reftex.el
    (define-key km "\t" 'bibtex-find-text)
    (define-key km "\n" 'bibtex-next-field)
    (define-key km "\M-\t" 'completion-at-point)
    (define-key km "\C-c\"" 'bibtex-remove-delimiters)
    (define-key km "\C-c{" 'bibtex-remove-delimiters)
    (define-key km "\C-c}" 'bibtex-remove-delimiters)
    (define-key km "\C-c\C-c" 'bibtex-clean-entry)
    (define-key km "\C-c\C-q" 'bibtex-fill-entry)
    (define-key km "\C-c\C-s" 'bibtex-search-entry)
    (define-key km "\C-c\C-x" 'bibtex-search-crossref)
    (define-key km "\C-c\C-t" 'bibtex-copy-summary-as-kill)
    (define-key km "\C-c?" 'bibtex-print-help-message)
    (define-key km "\C-c\C-p" 'bibtex-pop-previous)
    (define-key km "\C-c\C-n" 'bibtex-pop-next)
    (define-key km "\C-c\C-k" 'bibtex-kill-field)
    (define-key km "\C-c\M-k" 'bibtex-copy-field-as-kill)
    (define-key km "\C-c\C-w" 'bibtex-kill-entry)
    (define-key km "\C-c\M-w" 'bibtex-copy-entry-as-kill)
    (define-key km "\C-c\C-y" 'bibtex-yank)
    (define-key km "\C-c\M-y" 'bibtex-yank-pop)
    (define-key km "\C-c\C-d" 'bibtex-empty-field)
    (define-key km "\C-c\C-f" 'bibtex-make-field)
    (define-key km "\C-c\C-u" 'bibtex-entry-update)
    (define-key km "\C-c$" 'bibtex-ispell-abstract)
    (define-key km "\M-\C-a" 'bibtex-beginning-of-entry)
    (define-key km "\M-\C-e" 'bibtex-end-of-entry)
    (define-key km "\C-\M-l" 'bibtex-reposition-window)
    (define-key km "\C-\M-h" 'bibtex-mark-entry)
    (define-key km "\C-c\C-b" 'bibtex-entry)
    (define-key km "\C-c\C-rn" 'bibtex-narrow-to-entry)
    (define-key km "\C-c\C-rw" 'widen)
    (define-key km "\C-c\C-l" 'bibtex-url)
    (define-key km "\C-c\C-a" 'bibtex-search-entries)
    (define-key km "\C-c\C-o" 'bibtex-remove-OPT-or-ALT)
    (define-key km "\C-c\C-e\C-i" 'bibtex-InProceedings)
    (define-key km "\C-c\C-ei" 'bibtex-InCollection)
    (define-key km "\C-c\C-eI" 'bibtex-InBook)
    (define-key km "\C-c\C-e\C-a" 'bibtex-Article)
    (define-key km "\C-c\C-e\C-b" 'bibtex-InBook)
    (define-key km "\C-c\C-eb" 'bibtex-Book)
    (define-key km "\C-c\C-eB" 'bibtex-Booklet)
    (define-key km "\C-c\C-e\C-c" 'bibtex-InCollection)
    (define-key km "\C-c\C-e\C-m" 'bibtex-Manual)
    (define-key km "\C-c\C-em" 'bibtex-MastersThesis)
    (define-key km "\C-c\C-eM" 'bibtex-Misc)
    (define-key km "\C-c\C-e\C-p" 'bibtex-InProceedings)
    (define-key km "\C-c\C-ep" 'bibtex-Proceedings)
    (define-key km "\C-c\C-eP" 'bibtex-PhdThesis)
    (define-key km "\C-c\C-e\M-p" 'bibtex-Preamble)
    (define-key km "\C-c\C-e\C-s" 'bibtex-String)
    (define-key km "\C-c\C-e\C-t" 'bibtex-TechReport)
    (define-key km "\C-c\C-e\C-u" 'bibtex-Unpublished)
    km)
  "Keymap used in BibTeX mode.")

(easy-menu-define
  bibtex-edit-menu bibtex-mode-map "BibTeX-Edit Menu in BibTeX mode"
  '("BibTeX-Edit"
    ("Moving inside an Entry"
     ["End of Field" bibtex-find-text t]
     ["Next Field" bibtex-next-field t]
     ["Beginning of Entry" bibtex-beginning-of-entry t]
     ["End of Entry" bibtex-end-of-entry t]
    "--"
     ["Make Entry Visible" bibtex-reposition-window t])
    ("Moving in BibTeX Buffers"
     ["Search Entry" bibtex-search-entry t]
     ["Search Crossref Entry" bibtex-search-crossref t])
    "--"
    ("Operating on Current Field"
     ["Fill Field" fill-paragraph t]
     ["Remove Delimiters" bibtex-remove-delimiters t]
     ["Remove OPT or ALT Prefix" bibtex-remove-OPT-or-ALT t]
     ["Clear Field" bibtex-empty-field t]
     "--"
     ["Kill Field" bibtex-kill-field t]
     ["Copy Field to Kill Ring" bibtex-copy-field-as-kill t]
     ["Paste Most Recently Killed Field" bibtex-yank t]
     ["Paste Previously Killed Field" bibtex-yank-pop t]
     "--"
     ["Make New Field" bibtex-make-field t]
     "--"
     ["Snatch from Similar Following Field" bibtex-pop-next t]
     ["Snatch from Similar Preceding Field" bibtex-pop-previous t]
     "--"
     ["String or Key Complete" bibtex-complete t]
     "--"
     ["Help about Current Field" bibtex-print-help-message t])
    ("Operating on Current Entry"
     ["Fill Entry" bibtex-fill-entry t]
     ["Clean Entry" bibtex-clean-entry t]
     ["Update Entry" bibtex-entry-update t]
     "--"
     ["Kill Entry" bibtex-kill-entry t]
     ["Copy Entry to Kill Ring" bibtex-copy-entry-as-kill t]
     ["Paste Most Recently Killed Entry" bibtex-yank t]
     ["Paste Previously Killed Entry" bibtex-yank-pop t]
     "--"
     ["Copy Summary to Kill Ring" bibtex-copy-summary-as-kill t]
     ["Browse URL" bibtex-url t]
     "--"
     ["Ispell Entry" bibtex-ispell-entry t]
     ["Ispell Entry Abstract" bibtex-ispell-abstract t]
     "--"
     ["Narrow to Entry" bibtex-narrow-to-entry t]
     ["Mark Entry" bibtex-mark-entry t]
     "--"
     ["View Cite Locations (RefTeX)" reftex-view-crossref-from-bibtex
      (fboundp 'reftex-view-crossref-from-bibtex)])
    ("Operating on Buffer or Region"
     ["Search Entries" bibtex-search-entries t]
     "--"
     ["Validate Entries" bibtex-validate t]
     ["Sort Entries" bibtex-sort-buffer t]
     ["Reformat Entries" bibtex-reformat t]
     ["Count Entries" bibtex-count-entries t]
     "--"
     ["Convert Alien Buffer" bibtex-convert-alien t])
    ("Operating on Multiple Buffers"
     ["(Re)Initialize BibTeX Buffers" bibtex-initialize t]
     ["Validate Entries" bibtex-validate-globally t])))


;; Internal Variables

(defvar bibtex-entry-alist nil
  "Alist of currently active entry types.
Initialized by `bibtex-set-dialect'.")

(defvar bibtex-field-alist nil
  "Alist of currently active field types.
Initialized by `bibtex-set-dialect'.")

(defvar bibtex-field-braces-opt nil
  "Optimized value of `bibtex-field-braces-alist'.
Created by `bibtex-field-re-init'.
It is an alist with elements (FIELD . REGEXP).")

(defvar bibtex-field-strings-opt nil
  "Optimized value of `bibtex-field-strings-alist'.
Created by `bibtex-field-re-init'.
It is an alist with elements (FIELD RULE1 RULE2 ...),
where each RULE is (REGEXP . TO-STR).")

(defvar bibtex-pop-previous-search-point nil
  "Next point where `bibtex-pop-previous' starts looking for a similar entry.")

(defvar bibtex-pop-next-search-point nil
  "Next point where `bibtex-pop-next' starts looking for a similar entry.")

(defvar bibtex-field-kill-ring nil
  "Ring of least recently killed fields.
At most `bibtex-field-kill-ring-max' items are kept here.")

(defvar bibtex-field-kill-ring-yank-pointer nil
  "The tail of `bibtex-field-kill-ring' whose car is the last item yanked.")

(defvar bibtex-entry-kill-ring nil
  "Ring of least recently killed entries.
At most `bibtex-entry-kill-ring-max' items are kept here.")

(defvar bibtex-entry-kill-ring-yank-pointer nil
  "The tail of `bibtex-entry-kill-ring' whose car is the last item yanked.")

(defvar bibtex-last-kill-command nil
  "Type of the last kill command (either 'field or 'entry).")

(defvar bibtex-strings
  (lazy-completion-table bibtex-strings
                         (lambda ()
                           (bibtex-parse-strings (bibtex-string-files-init))))
  "Completion table for BibTeX string keys.
Initialized from `bibtex-predefined-strings' and `bibtex-string-files'.")
(make-variable-buffer-local 'bibtex-strings)
(put 'bibtex-strings 'risky-local-variable t)

(defvar bibtex-reference-keys
  (lazy-completion-table bibtex-reference-keys
                         (lambda () (bibtex-parse-keys nil t)))
  "Completion table for BibTeX reference keys.
The CDRs of the elements are t for header keys and nil for crossref keys.")
(make-variable-buffer-local 'bibtex-reference-keys)
(put 'bibtex-reference-keys 'risky-local-variable t)

(defvar bibtex-buffer-last-parsed-tick nil
  "Value of `buffer-modified-tick' last time buffer was parsed for keys.")

(defvar bibtex-parse-idle-timer nil
  "Stores if timer is already installed.")

(defvar bibtex-progress-lastperc nil
  "Last reported percentage for the progress message.")

(defvar bibtex-progress-lastmes nil
  "Last reported progress message.")

(defvar bibtex-progress-interval nil
  "Interval for progress messages.")

(defvar bibtex-key-history nil
  "History list for reading keys.")

(defvar bibtex-entry-type-history nil
  "History list for reading entry types.")

(defvar bibtex-field-history nil
  "History list for reading field names.")

(defvar bibtex-reformat-previous-options nil
  "Last reformat options given.")

(defvar bibtex-reformat-previous-reference-keys nil
  "Last reformat reference keys option given.")

(defconst bibtex-field-name "[^\"#%'(),={} \t\n0-9][^\"#%'(),={} \t\n]*"
  "Regexp matching the name of a BibTeX field.")

(defconst bibtex-name-part
  (concat ",[ \t\n]*\\(" bibtex-field-name "\\)")
  "Regexp matching the name part of a BibTeX field.")

(defconst bibtex-reference-key "[][[:alnum:].:;?!`'/*@+|()<>&_^$-]+"
  "Regexp matching the reference key part of a BibTeX entry.")

(defconst bibtex-field-const "[][[:alnum:].:;?!`'/*@+=|<>&_^$-]+"
  "Regexp matching a BibTeX field constant.")

(defvar bibtex-entry-type nil
  "Regexp matching the type of a BibTeX entry.
Initialized by `bibtex-set-dialect'.")

(defvar bibtex-entry-head nil
  "Regexp matching the header line of a BibTeX entry (including key).
Initialized by `bibtex-set-dialect'.")

(defvar bibtex-entry-maybe-empty-head nil
  "Regexp matching the header line of a BibTeX entry (possibly without key).
Initialized by `bibtex-set-dialect'.")

(defconst bibtex-any-entry-maybe-empty-head
  (concat "^[ \t]*\\(@[ \t]*" bibtex-field-name "\\)[ \t]*[({][ \t\n]*\\("
          bibtex-reference-key "\\)?")
  "Regexp matching the header line of any BibTeX entry (possibly without key).")

(defvar bibtex-any-valid-entry-type nil
  "Regexp matching any valid BibTeX entry (including String and Preamble).
Initialized by `bibtex-set-dialect'.")

(defconst bibtex-type-in-head 1
  "Regexp subexpression number of the type part in `bibtex-entry-head'.")

(defconst bibtex-key-in-head 2
  "Regexp subexpression number of the key part in `bibtex-entry-head'.")

(defconst bibtex-string-type "^[ \t]*\\(@[ \t]*String\\)[ \t]*[({][ \t\n]*"
   "Regexp matching the name of a BibTeX String entry.")

(defconst bibtex-string-maybe-empty-head
  (concat bibtex-string-type "\\(" bibtex-reference-key "\\)?")
  "Regexp matching the header line of a BibTeX String entry.")

(defconst bibtex-preamble-prefix
  "[ \t]*\\(@[ \t]*Preamble\\)[ \t]*[({][ \t\n]*"
  "Regexp matching the prefix part of a BibTeX Preamble entry.")

(defconst bibtex-font-lock-syntactic-keywords
  `((,(concat "^[ \t]*\\(" (substring bibtex-comment-start 0 1) "\\)"
              (substring bibtex-comment-start 1) "\\>")
     1 '(11))))

(defvar bibtex-font-lock-keywords
  ;; entry type and reference key
  `((,bibtex-any-entry-maybe-empty-head
     (,bibtex-type-in-head font-lock-function-name-face)
     (,bibtex-key-in-head font-lock-constant-face nil t))
    ;; optional field names (treated as comments)
    (,(concat "^[ \t]*\\(OPT" bibtex-field-name "\\)[ \t]*=")
     1 font-lock-comment-face)
    ;; field names
    (,(concat "^[ \t]*\\(" bibtex-field-name "\\)[ \t]*=")
     1 font-lock-variable-name-face)
    ;; url
    (bibtex-font-lock-url) (bibtex-font-lock-crossref)
    ;; cite
    ,@(mapcar (lambda (matcher)
                `((lambda (bound) (bibtex-font-lock-cite ',matcher bound))))
              bibtex-cite-matcher-alist))
  "*Default expressions to highlight in BibTeX mode.")

(defvar bibtex-font-lock-url-regexp
  ;; Assume that field names begin at the beginning of a line.
  (concat "^[ \t]*"
          (regexp-opt (delete-dups (mapcar 'caar bibtex-generate-url-list)) t)
          "[ \t]*=[ \t]*")
  "Regexp for `bibtex-font-lock-url' derived from `bibtex-generate-url-list'.")

(defvar bibtex-string-empty-key nil
  "If non-nil, `bibtex-parse-string' accepts empty key.")

(defvar bibtex-sort-entry-class-alist nil
  "Alist mapping entry types to their sorting index.
Auto-generated from `bibtex-sort-entry-class'.
Used when `bibtex-maintain-sorted-entries' is `entry-class'.")


(defun bibtex-parse-association (parse-lhs parse-rhs)
  "Parse a string of the format <left-hand-side = right-hand-side>.
The functions PARSE-LHS and PARSE-RHS are used to parse the corresponding
substrings.  These functions are expected to return nil if parsing is not
successful.  If the returned values of both functions are non-nil,
return a cons pair of these values.  Do not move point."
  (save-match-data
    (save-excursion
      (let ((left (funcall parse-lhs))
            right)
        (if (and left
                 (looking-at "[ \t\n]*=[ \t\n]*")
                 (goto-char (match-end 0))
                 (setq right (funcall parse-rhs)))
            (cons left right))))))

(defun bibtex-parse-field-name ()
  "Parse the name part of a BibTeX field.
If the field name is found, return a triple consisting of the position of the
very first character of the match, the actual starting position of the name
part and end position of the match.  Move point to end of field name.
If `bibtex-autoadd-commas' is non-nil add missing comma at end of preceding
BibTeX field as necessary."
  (cond ((looking-at bibtex-name-part)
         (goto-char (match-end 0))
         (list (match-beginning 0) (match-beginning 1) (match-end 0)))
        ;; Maybe add a missing comma.
        ((and bibtex-autoadd-commas
              (looking-at (concat "[ \t\n]*\\(?:" bibtex-field-name
                                  "\\)[ \t\n]*=")))
         (skip-chars-backward " \t\n")
         ;; It can be confusing if non-editing commands try to
         ;; modify the buffer.
         (if buffer-read-only
             (error "Comma missing at buffer position %s" (point)))
         (insert ",")
         (forward-char -1)
         ;; Now try again.
         (bibtex-parse-field-name))))

(defconst bibtex-braced-string-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "." st)
    (modify-syntax-entry ?\] "." st)
    (modify-syntax-entry ?\( "." st)
    (modify-syntax-entry ?\) "." st)
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?\" "." st)
    st)
  "Syntax-table to parse matched braces.")

(defconst bibtex-quoted-string-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax-table to parse matched quotes.")

(defun bibtex-parse-field-string ()
  "Parse a BibTeX field string enclosed by braces or quotes.
If a syntactically correct string is found, a pair containing the start and
end position of the field string is returned, nil otherwise.
Do not move point."
  (let ((end-point
         (or (and (eq (following-char) ?\")
                  (save-excursion
                    (with-syntax-table bibtex-quoted-string-syntax-table
                      (forward-sexp 1))
                    (point)))
             (and (eq (following-char) ?\{)
                  (save-excursion
                    (with-syntax-table bibtex-braced-string-syntax-table
                      (forward-sexp 1))
                    (point))))))
    (if end-point
        (cons (point) end-point))))

(defun bibtex-parse-field-text ()
  "Parse the text part of a BibTeX field.
The text part is either a string, or an empty string, or a constant followed
by one or more <# (string|constant)> pairs.  If a syntactically correct text
is found, a pair containing the start and end position of the text is
returned, nil otherwise.  Move point to end of field text."
  (let ((starting-point (point))
        end-point failure boundaries)
    (while (not (or end-point failure))
      (cond ((looking-at bibtex-field-const)
             (goto-char (match-end 0)))
            ((setq boundaries (bibtex-parse-field-string))
             (goto-char (cdr boundaries)))
            ((setq failure t)))
      (if (looking-at "[ \t\n]*#[ \t\n]*")
          (goto-char (match-end 0))
        (setq end-point (point))))
    (skip-chars-forward " \t\n")
    (if (and (not failure)
             end-point)
        (list starting-point end-point (point)))))

(defun bibtex-parse-field ()
  "Parse the BibTeX field beginning at the position of point.
If a syntactically correct field is found, return a cons pair containing
the boundaries of the name and text parts of the field.  Do not move point."
  (bibtex-parse-association 'bibtex-parse-field-name
                            'bibtex-parse-field-text))

(defsubst bibtex-start-of-field (bounds)
  (nth 0 (car bounds)))
(defsubst bibtex-start-of-name-in-field (bounds)
  (nth 1 (car bounds)))
(defsubst bibtex-end-of-name-in-field (bounds)
  (nth 2 (car bounds)))
(defsubst bibtex-start-of-text-in-field (bounds)
  (nth 1 bounds))
(defsubst bibtex-end-of-text-in-field (bounds)
  (nth 2 bounds))
(defsubst bibtex-end-of-field (bounds)
  (nth 3 bounds))

(defun bibtex-search-forward-field (name &optional bound)
  "Search forward to find a BibTeX field of name NAME.
If a syntactically correct field is found, return a pair containing
the boundaries of the name and text parts of the field.  The search
is limited by optional arg BOUND.  If BOUND is t the search is limited
by the end of the current entry.  Do not move point."
  (save-match-data
    (save-excursion
      (if (eq bound t)
          (let ((regexp (concat bibtex-name-part "[ \t\n]*=\\|"
                                bibtex-any-entry-maybe-empty-head))
                (case-fold-search t) bounds)
            (catch 'done
              (if (looking-at "[ \t]*@") (goto-char (match-end 0)))
              (while (and (not bounds)
                          (re-search-forward regexp nil t))
                (if (match-beginning 2)
                    ;; We found a new entry
                    (throw 'done nil)
                  ;; We found a field
                  (goto-char (match-beginning 0))
                  (setq bounds (bibtex-parse-field))))
              ;; Step through all fields so that we cannot overshoot.
              (while bounds
                (goto-char (bibtex-start-of-name-in-field bounds))
                (if (looking-at name) (throw 'done bounds))
                (goto-char (bibtex-end-of-field bounds))
                (setq bounds (bibtex-parse-field)))))
        ;; Bounded search or bound is nil (i.e. we cannot overshoot).
        ;; Indeed, the search is bounded when `bibtex-search-forward-field'
        ;; is called many times.  So we optimize this part of this function.
        (let ((name-part (concat ",[ \t\n]*\\(" name "\\)[ \t\n]*=[ \t\n]*"))
              (case-fold-search t) left right)
          (while (and (not right)
                      (re-search-forward name-part bound t))
            (setq left (list (match-beginning 0) (match-beginning 1)
                             (match-end 1))
                  ;; Don't worry that the field text could be past bound.
                  right (bibtex-parse-field-text)))
          (if right (cons left right)))))))

(defun bibtex-search-backward-field (name &optional bound)
  "Search backward to find a BibTeX field of name NAME.
If a syntactically correct field is found, return a pair containing
the boundaries of the name and text parts of the field.  The search
is limited by the optional arg BOUND.  If BOUND is t the search is
limited by the beginning of the current entry.  Do not move point."
  (save-match-data
    (if (eq bound t)
        (setq bound (save-excursion (bibtex-beginning-of-entry))))
    (let ((name-part (concat ",[ \t\n]*\\(" name "\\)[ \t\n]*=[ \t\n]*"))
          (case-fold-search t) left right)
      (save-excursion
        ;; the parsing functions are not designed for parsing backwards :-(
        (when (search-backward "," bound t)
          (or (save-excursion
                (when (looking-at name-part)
                  (setq left (list (match-beginning 0) (match-beginning 1)
                                   (match-end 1)))
                  (goto-char (match-end 0))
                  (setq right (bibtex-parse-field-text))))
              (while (and (not right)
                          (re-search-backward name-part bound t))
                (setq left (list (match-beginning 0) (match-beginning 1)
                                 (match-end 1)))
                (save-excursion
                  (goto-char (match-end 0))
                  (setq right (bibtex-parse-field-text)))))
          (if right (cons left right)))))))

(defun bibtex-name-in-field (bounds &optional remove-opt-alt)
  "Get content of name in BibTeX field defined via BOUNDS.
If optional arg REMOVE-OPT-ALT is non-nil remove \"OPT\" and \"ALT\"."
  (let ((name (buffer-substring-no-properties
               (bibtex-start-of-name-in-field bounds)
               (bibtex-end-of-name-in-field bounds))))
    (if (and remove-opt-alt
             (string-match "\\`\\(OPT\\|ALT\\)" name)
             (not (and bibtex-no-opt-remove-re
                       (string-match bibtex-no-opt-remove-re name))))
        (substring name 3)
      name)))

(defun bibtex-text-in-field-bounds (bounds &optional content)
  "Get text in BibTeX field defined via BOUNDS.
If optional arg CONTENT is non-nil extract content of field
by removing field delimiters and concatenating the resulting string.
If `bibtex-expand-strings' is non-nil, also expand BibTeX strings."
  (if content
      (save-excursion
        (goto-char (bibtex-start-of-text-in-field bounds))
        (let ((epoint (bibtex-end-of-text-in-field bounds))
              content)
          (while (< (point) epoint)
            (if (looking-at bibtex-field-const)
                (let ((mtch (match-string-no-properties 0)))
                  (push (or (if bibtex-expand-strings
                                (cdr (assoc-string mtch (bibtex-strings) t)))
                            mtch) content)
                  (goto-char (match-end 0)))
              (let ((bounds (bibtex-parse-field-string)))
                (push (buffer-substring-no-properties
                       (1+ (car bounds)) (1- (cdr bounds))) content)
                (goto-char (cdr bounds))))
            (re-search-forward "\\=[ \t\n]*#[ \t\n]*" nil t))
          (apply 'concat (nreverse content))))
    (buffer-substring-no-properties (bibtex-start-of-text-in-field bounds)
                                    (bibtex-end-of-text-in-field bounds))))

(defun bibtex-text-in-field (field &optional follow-crossref)
  "Get content of field FIELD of current BibTeX entry.
Return nil if not found.
If optional arg FOLLOW-CROSSREF is non-nil, follow crossref."
  (save-excursion
    (let* ((end (if follow-crossref (bibtex-end-of-entry) t))
           (beg (bibtex-beginning-of-entry)) ; move point
           (bounds (bibtex-search-forward-field field end)))
      (cond (bounds (bibtex-text-in-field-bounds bounds t))
            ((and follow-crossref
                  (progn (goto-char beg)
                         (setq bounds (bibtex-search-forward-field
                                       "\\(OPT\\)?crossref" end))))
             (let ((crossref-field (bibtex-text-in-field-bounds bounds t)))
               (if (bibtex-search-crossref crossref-field)
                   ;; Do not pass FOLLOW-CROSSREF because we want
                   ;; to follow crossrefs only one level of recursion.
                   (bibtex-text-in-field field))))))))

(defun bibtex-parse-string-prefix ()
  "Parse the prefix part of a BibTeX string entry, including reference key.
If the string prefix is found, return a triple consisting of the position of
the very first character of the match, the actual starting position of the
reference key and the end position of the match.
If `bibtex-string-empty-key' is non-nil accept empty string key."
  (let ((case-fold-search t))
    (if (looking-at bibtex-string-type)
        (let ((start (point)))
          (goto-char (match-end 0))
          (cond ((looking-at bibtex-reference-key)
                 (goto-char (match-end 0))
                 (list start
                       (match-beginning 0)
                       (match-end 0)))
                ((and bibtex-string-empty-key
                      (looking-at "="))
                 (skip-chars-backward " \t\n")
                 (list start (point) (point))))))))

(defun bibtex-parse-string-postfix ()
  "Parse the postfix part of a BibTeX string entry, including the text.
If the string postfix is found, return a triple consisting of the position of
the actual starting and ending position of the text and the very last
character of the string entry.  Move point past BibTeX string entry."
  (let* ((case-fold-search t)
         (bounds (bibtex-parse-field-text)))
    (when bounds
      (goto-char (nth 1 bounds))
      (when (looking-at "[ \t\n]*[})]")
        (goto-char (match-end 0))
        (list (car bounds)
              (nth 1 bounds)
              (match-end 0))))))

(defun bibtex-parse-string (&optional empty-key)
  "Parse a BibTeX string entry beginning at the position of point.
If a syntactically correct entry is found, return a cons pair containing
the boundaries of the reference key and text parts of the entry.
If EMPTY-KEY is non-nil, key may be empty.  Do not move point."
  (let ((bibtex-string-empty-key empty-key))
    (bibtex-parse-association 'bibtex-parse-string-prefix
                              'bibtex-parse-string-postfix)))

(defun bibtex-search-forward-string (&optional empty-key)
  "Search forward to find a BibTeX string entry.
If a syntactically correct entry is found, a pair containing the boundaries of
the reference key and text parts of the string is returned.
If EMPTY-KEY is non-nil, key may be empty.  Do not move point."
  (save-excursion
    (save-match-data
      (let ((case-fold-search t) bounds)
        (while (and (not bounds)
                    (search-forward-regexp bibtex-string-type nil t))
          (save-excursion (goto-char (match-beginning 0))
                          (setq bounds (bibtex-parse-string empty-key))))
        bounds))))

(defun bibtex-reference-key-in-string (bounds)
  "Return the key part of a BibTeX string defined via BOUNDS."
  (buffer-substring-no-properties (nth 1 (car bounds))
                                  (nth 2 (car bounds))))

(defun bibtex-text-in-string (bounds &optional content)
  "Get text in BibTeX string field defined via BOUNDS.
If optional arg CONTENT is non-nil extract content
by removing field delimiters and concatenating the resulting string.
If `bibtex-expand-strings' is non-nil, also expand BibTeX strings."
  (bibtex-text-in-field-bounds bounds content))

(defsubst bibtex-start-of-text-in-string (bounds)
  (nth 0 (cdr bounds)))
(defsubst bibtex-end-of-text-in-string (bounds)
  (nth 1 (cdr bounds)))
(defsubst bibtex-end-of-string (bounds)
  (nth 2 (cdr bounds)))

(defsubst bibtex-type-in-head ()
  "Extract BibTeX type in head."
  ;;                              ignore @
  (buffer-substring-no-properties (1+ (match-beginning bibtex-type-in-head))
                                  (match-end bibtex-type-in-head)))

(defsubst bibtex-key-in-head (&optional empty)
  "Extract BibTeX key in head.  Return optional arg EMPTY if key is empty."
  (or (match-string-no-properties bibtex-key-in-head)
      empty))

(defun bibtex-parse-preamble ()
  "Parse BibTeX preamble.
Point must be at beginning of preamble.  Do not move point."
  (let ((case-fold-search t))
    (when (looking-at bibtex-preamble-prefix)
      (let ((start (match-beginning 0)) (pref-start (match-beginning 1))
            (bounds (save-excursion (goto-char (match-end 0))
                                    (bibtex-parse-string-postfix))))
        (if bounds (cons (list start pref-start) bounds))))))

;; Helper Functions

(defsubst bibtex-string= (str1 str2)
  "Return t if STR1 and STR2 are equal, ignoring case."
  (eq t (compare-strings str1 0 nil str2 0 nil t)))

(defun bibtex-delete-whitespace ()
  "Delete all whitespace starting at point."
  (if (looking-at "[ \t\n]+")
      (delete-region (point) (match-end 0))))

(defun bibtex-current-line ()
  "Compute line number of point regardless whether the buffer is narrowed."
  (+ (count-lines 1 (point))
     (if (bolp) 1 0)))

(defun bibtex-valid-entry (&optional empty-key)
  "Parse a valid BibTeX entry (maybe without key if EMPTY-KEY is t).
A valid entry is a syntactical correct one with type contained in
`bibtex-BibTeX-entry-alist'.  Ignore @String and @Preamble entries.
Return a cons pair with buffer positions of beginning and end of entry
if a valid entry is found, nil otherwise.  Do not move point.
After a call to this function `match-data' corresponds to the header
of the entry, see regexp `bibtex-entry-head'."
  (let ((case-fold-search t) end)
    (if (looking-at (if empty-key bibtex-entry-maybe-empty-head
                    bibtex-entry-head))
        (save-excursion
          (save-match-data
            (goto-char (match-end 0))
            (let ((entry-closer
                   (if (save-excursion
                         (goto-char (match-end bibtex-type-in-head))
                         (looking-at "[ \t]*("))
                       ",?[ \t\n]*)" ; entry opened with `('
                     ",?[ \t\n]*}")) ; entry opened with `{'
                  bounds)
              (skip-chars-forward " \t\n")
              ;; loop over all BibTeX fields
              (while (setq bounds (bibtex-parse-field))
                (goto-char (bibtex-end-of-field bounds)))
              ;; This matches the infix* part.
              (if (looking-at entry-closer) (setq end (match-end 0)))))
          (if end (cons (match-beginning 0) end))))))

(defun bibtex-skip-to-valid-entry (&optional backward)
  "Move point to beginning of the next valid BibTeX entry.
Do not move if we are already at beginning of a valid BibTeX entry.
With optional argument BACKWARD non-nil, move backward to
beginning of previous valid one.  A valid entry is a syntactical correct one
with type contained in `bibtex-BibTeX-entry-alist' or, if
`bibtex-sort-ignore-string-entries' is nil, a syntactical correct string
entry.  Return buffer position of beginning and end of entry if a valid
entry is found, nil otherwise."
  (interactive "P")
  (let ((case-fold-search t)
        found bounds)
    (beginning-of-line)
    ;; Loop till we look at a valid entry.
    (while (not (or found (if backward (bobp) (eobp))))
      (cond ((setq found (or (bibtex-valid-entry)
                             (and (not bibtex-sort-ignore-string-entries)
                                  (setq bounds (bibtex-parse-string))
                                  (cons (bibtex-start-of-field bounds)
                                        (bibtex-end-of-string bounds))))))
            (backward (re-search-backward "^[ \t]*@" nil 'move))
            (t (if (re-search-forward "\n\\([ \t]*@\\)" nil 'move)
                   (goto-char (match-beginning 1))))))
    found))

(defun bibtex-map-entries (fun)
  "Call FUN for each BibTeX entry in buffer (possibly narrowed).
FUN is called with three arguments, the key of the entry and the buffer
positions of beginning and end of entry.  Also, point is at beginning of
entry and `match-data' corresponds to the header of the entry,
see regexp `bibtex-entry-head'.  If `bibtex-sort-ignore-string-entries'
is non-nil, FUN is not called for @String entries."
  (let ((case-fold-search t)
        (end-marker (make-marker))
        found)
    ;; Use marker to keep track of the buffer position of the end of
    ;; a BibTeX entry as this position may change during reformatting.
    (set-marker-insertion-type end-marker t)
    (save-excursion
      (goto-char (point-min))
      (while (setq found (bibtex-skip-to-valid-entry))
        (set-marker end-marker (cdr found))
        (looking-at bibtex-any-entry-maybe-empty-head)
        (funcall fun (bibtex-key-in-head "") (car found) end-marker)
        (goto-char end-marker)))))

(defun bibtex-progress-message (&optional flag interval)
  "Echo a message about progress of current buffer.
If FLAG is a string, the message is initialized (in this case a
value for INTERVAL may be given as well (if not this is set to 5)).
If FLAG is `done', the message is deinitialized.
If FLAG is nil, a message is echoed if point was incremented at least
`bibtex-progress-interval' percent since last message was echoed."
  (cond ((stringp flag)
         (setq bibtex-progress-lastmes flag
               bibtex-progress-interval (or interval 5)
               bibtex-progress-lastperc 0))
        ((eq flag 'done)
         (message  "%s (done)" bibtex-progress-lastmes)
         (setq bibtex-progress-lastmes nil))
        (t
         (let* ((size (- (point-max) (point-min)))
                (perc (if (= size 0)
                          100
                        (/ (* 100 (- (point) (point-min))) size))))
           (when (>= perc (+ bibtex-progress-lastperc
                             bibtex-progress-interval))
             (setq bibtex-progress-lastperc perc)
             (message "%s (%d%%)" bibtex-progress-lastmes perc))))))

(defun bibtex-field-left-delimiter ()
  "Return a string dependent on `bibtex-field-delimiters'."
  (if (eq bibtex-field-delimiters 'braces)
      "{"
    "\""))

(defun bibtex-field-right-delimiter ()
  "Return a string dependent on `bibtex-field-delimiters'."
  (if (eq bibtex-field-delimiters 'braces)
      "}"
    "\""))

(defun bibtex-entry-left-delimiter ()
  "Return a string dependent on `bibtex-entry-delimiters'."
  (if (eq bibtex-entry-delimiters 'braces)
      "{"
    "("))

(defun bibtex-entry-right-delimiter ()
  "Return a string dependent on `bibtex-entry-delimiters'."
  (if (eq bibtex-entry-delimiters 'braces)
      "}"
    ")"))

(defun bibtex-flash-head (prompt)
  "Flash at BibTeX entry head before point, if it exists."
  (let ((case-fold-search t)
        (pnt (point)))
    (save-excursion
      (bibtex-beginning-of-entry)
      (when (and (looking-at bibtex-any-entry-maybe-empty-head)
                 (< (point) pnt))
        (goto-char (match-beginning bibtex-type-in-head))
        (if (and (< 0 blink-matching-delay)
                 (pos-visible-in-window-p (point)))
            (sit-for blink-matching-delay)
          (message "%s%s" prompt (buffer-substring-no-properties
                                  (point) (match-end bibtex-key-in-head))))))))

(defun bibtex-make-optional-field (field)
  "Make an optional field named FIELD in current BibTeX entry."
  (if (consp field)
      (bibtex-make-field (cons (concat "OPT" (car field)) (cdr field)))
    (bibtex-make-field (concat "OPT" field))))

(defun bibtex-move-outside-of-entry ()
  "Make sure point is outside of a BibTeX entry."
  (let ((orig-point (point)))
    (bibtex-end-of-entry)
    (when (< (point) orig-point)
      ;; We moved backward, so we weren't inside an entry to begin with.
      ;; Leave point at the beginning of a line, and preferably
      ;; at the beginning of a paragraph.
      (goto-char orig-point)
      (beginning-of-line 1)
      (unless (= ?\n (char-before (1- (point))))
        (re-search-forward "^[ \t]*[@\n]" nil 'move)
        (backward-char 1)))
    (skip-chars-forward " \t\n")))

(defun bibtex-beginning-of-first-entry ()
  "Go to beginning of line of first BibTeX entry in buffer.
If `bibtex-sort-ignore-string-entries' is non-nil, @String entries
are ignored.  Return point"
  (goto-char (point-min))
  (bibtex-skip-to-valid-entry)
  (point))

(defun bibtex-enclosing-field (&optional comma noerr)
  "Search for BibTeX field enclosing point.
For `bibtex-mode''s internal algorithms, a field begins at the comma
following the preceding field.  Usually, this is not what the user expects.
Thus if COMMA is non-nil, the \"current field\" includes the terminating comma
as well as the entry delimiter if it appears on the same line.
Unless NOERR is non-nil, signal an error if no enclosing field is found.
On success return bounds, nil otherwise.  Do not move point."
  (save-excursion
    (when comma
      (end-of-line)
      (skip-chars-backward " \t")
      ;; Ignore entry delimiter and comma at end of line.
      (if (memq (preceding-char) '(?} ?\))) (forward-char -1))
      (if (= (preceding-char) ?,) (forward-char -1)))

    (let ((bounds (bibtex-search-backward-field bibtex-field-name t)))
      (cond ((and bounds
                  (<= (bibtex-start-of-field bounds) (point))
                  (>= (bibtex-end-of-field bounds) (point)))
             bounds)
            ((not noerr)
             (error "Can't find enclosing BibTeX field"))))))

(defun bibtex-beginning-first-field (&optional beg)
  "Move point to beginning of first field.
Optional arg BEG is beginning of entry."
  (if beg (goto-char beg) (bibtex-beginning-of-entry))
  (looking-at bibtex-any-entry-maybe-empty-head)
  (goto-char (match-end 0)))

(defun bibtex-insert-kill (n &optional comma)
  "Reinsert the Nth stretch of killed BibTeX text (field or entry).
Optional arg COMMA is as in `bibtex-enclosing-field'."
  (unless bibtex-last-kill-command (error "BibTeX kill ring is empty"))
  (let ((fun (lambda (kryp kr) ; adapted from `current-kill'
               (car (set kryp (nthcdr (mod (- n (length (eval kryp)))
                                           (length kr)) kr))))))
    ;; We put the mark at the beginning of the inserted field or entry
    ;; and point at its end - a behavior similar to what `yank' does.
    ;; The mark is then used by `bibtex-yank-pop', which needs to know
    ;; what we have inserted.
    (if (eq bibtex-last-kill-command 'field)
        (progn
          ;; insert past the current field
          (goto-char (bibtex-end-of-field (bibtex-enclosing-field comma)))
          (push-mark)
          (bibtex-make-field (funcall fun 'bibtex-field-kill-ring-yank-pointer
                                      bibtex-field-kill-ring) t nil t))
      ;; insert past the current entry
      (bibtex-skip-to-valid-entry)
      (push-mark)
      (insert (funcall fun 'bibtex-entry-kill-ring-yank-pointer
                       bibtex-entry-kill-ring))
      ;; If we copied an entry from a buffer containing only this one entry,
      ;; it can be missing the second "\n".
      (unless (looking-back "\n\n") (insert "\n"))
      (unless (functionp bibtex-reference-keys)
        ;; update `bibtex-reference-keys'
        (save-excursion
          (goto-char (mark t))
          (looking-at bibtex-any-entry-maybe-empty-head)
          (let ((key (bibtex-key-in-head)))
            (if key (push (cons key t) bibtex-reference-keys))))))))

(defsubst bibtex-vec-push (vec idx newelt)
  "Add NEWELT to the list stored in VEC at index IDX."
  (aset vec idx (cons newelt (aref vec idx))))

(defsubst bibtex-vec-incr (vec idx)
  "Increment by 1 the counter which is stored in VEC at index IDX."
  (aset vec idx (1+ (aref vec idx))))

(defun bibtex-format-entry ()
  "Helper function for `bibtex-clean-entry'.
Formats current entry according to variable `bibtex-entry-format'."
  ;; initialize `bibtex-field-braces-opt' if necessary
  (if (and bibtex-field-braces-alist (not bibtex-field-braces-opt))
      (setq bibtex-field-braces-opt
            (bibtex-field-re-init bibtex-field-braces-alist 'braces)))
  ;; initialize `bibtex-field-strings-opt' if necessary
  (if (and bibtex-field-strings-alist (not bibtex-field-strings-opt))
      (setq bibtex-field-strings-opt
            (bibtex-field-re-init bibtex-field-strings-alist 'strings)))

  (let ((case-fold-search t)
        (format (if (eq bibtex-entry-format t)
                    '(realign opts-or-alts required-fields numerical-fields
                              page-dashes whitespace inherit-booktitle
                              last-comma delimiters unify-case braces
                              strings sort-fields)
                  bibtex-entry-format))
        (left-delim-re (regexp-quote (bibtex-field-left-delimiter)))
        bounds crossref-key req-field-list default-field-list field-list
        num-alt alt-fields idx error-field-name)
    (unwind-protect
        ;; formatting (undone if error occurs)
        (atomic-change-group
          (save-excursion
            (save-restriction
              (bibtex-narrow-to-entry)

              ;; There are more elegant high-level functions for several tasks
              ;; done by `bibtex-format-entry'.  However, they contain some
              ;; redundancy compared with what we need to do anyway.
              ;; So for speed-up we avoid using them.
              ;; (`bibtex-format-entry' is called often by `bibtex-reformat'.)

              ;; identify entry type
              (goto-char (point-min))
              (or (re-search-forward bibtex-entry-type nil t)
                  (error "Not inside a BibTeX entry"))
              (let* ((beg-type (1+ (match-beginning 0)))
                     (end-type (match-end 0))
                     (entry-list (assoc-string (buffer-substring-no-properties
                                                beg-type end-type)
                                               bibtex-entry-alist t)))

                ;; unify case of entry type
                (when (memq 'unify-case format)
                  (delete-region beg-type end-type)
                  (insert (car entry-list)))

                ;; update left entry delimiter
                (when (memq 'delimiters format)
                  (goto-char end-type)
                  (skip-chars-forward " \t\n")
                  (delete-char 1)
                  (insert (bibtex-entry-left-delimiter)))

                ;; Do we have a crossref key?
                (goto-char (point-min))
                (if (setq bounds (bibtex-search-forward-field
                                  "\\(OPT\\)?crossref"))
                    (let ((text (bibtex-text-in-field-bounds bounds t)))
                      (unless (equal "" text)
                        (setq crossref-key text))))

                ;; list of required fields appropriate for an entry with
                ;; or without crossref key.
                (setq req-field-list (if crossref-key (nth 2 entry-list)
                                       (append (nth 2 entry-list) (nth 3 entry-list)))
                      ;; default list of fields that may appear in this entry
                      default-field-list (append (nth 2 entry-list) (nth 3 entry-list)
                                                 (nth 4 entry-list)
                                                 bibtex-user-optional-fields)
                      ;; number of ALT fields we expect to find
                      num-alt (length (delq nil (delete-dups
                                                 (mapcar (lambda (x) (nth 3 x))
                                                         req-field-list))))
                      ;; ALT fields of respective groups
                      alt-fields (make-vector num-alt nil))

                (when (memq 'sort-fields format)
                  (goto-char (point-min))
                  (let ((beg-fields (save-excursion (bibtex-beginning-first-field)))
                        (fields-alist (bibtex-parse-entry))
                        bibtex-help-message elt)
                    (delete-region beg-fields (point))
                    (dolist (field default-field-list)
                      (when (setq elt (assoc-string (car field) fields-alist t))
                        (setq fields-alist (delete elt fields-alist))
                        (bibtex-make-field (list (car elt) nil (cdr elt)) nil nil t)))
                    (dolist (field fields-alist)
                      (unless (member (car field) '("=key=" "=type="))
                        (bibtex-make-field (list (car field) nil (cdr field)) nil nil t))))))

              ;; process all fields
              (bibtex-beginning-first-field (point-min))
              (while (setq bounds (bibtex-parse-field))
                (let* ((beg-field (copy-marker (bibtex-start-of-field bounds)))
                       (end-field (copy-marker (bibtex-end-of-field bounds) t))
                       (beg-name  (copy-marker (bibtex-start-of-name-in-field bounds)))
                       (end-name  (copy-marker (bibtex-end-of-name-in-field bounds)))
                       (beg-text  (copy-marker (bibtex-start-of-text-in-field bounds)))
                       (end-text  (copy-marker (bibtex-end-of-text-in-field bounds) t))
                       (empty-field (equal "" (bibtex-text-in-field-bounds bounds t)))
                       (field-name (buffer-substring-no-properties beg-name end-name))
                       (opt-alt   (and (string-match "\\`\\(OPT\\|ALT\\)" field-name)
                                       (not (and bibtex-no-opt-remove-re
                                                 (string-match bibtex-no-opt-remove-re
                                                               field-name)))))
                       deleted)
                  (if opt-alt (setq field-name (substring field-name 3)))

                  ;; keep track of alternatives
                  (if (setq idx (nth 3 (assoc-string field-name req-field-list t)))
                      (bibtex-vec-push alt-fields idx field-name))

                  (if (memq 'opts-or-alts format)
                      ;; delete empty optional and alternative fields
                      ;; (but keep empty required fields)
                      (cond ((and empty-field
                                  (or opt-alt
                                      (let ((field (assoc-string
                                                    field-name req-field-list t)))
                                        (or (not field) ; OPT field
                                            (nth 3 field))))) ; ALT field
                             (delete-region beg-field end-field)
                             (setq deleted t))
                            ;; otherwise nonempty field: delete "OPT" or "ALT"
                            (opt-alt
                             (goto-char beg-name)
                             (delete-char 3))))

                  (unless deleted
                    (push field-name field-list)

                    ;; Remove whitespace at beginning and end of field.
                    ;; We do not look at individual parts of the field
                    ;; as {foo } # bar # { baz} is a fine field.
                    (when (memq 'whitespace format)
                      (goto-char beg-text)
                      (if (looking-at "\\([{\"]\\)[ \t\n]+")
                          (replace-match "\\1"))
                      (goto-char end-text)
                      (if (looking-back "[ \t\n]+\\([}\"]\\)" beg-text t)
                          (replace-match "\\1")))

                    ;; remove delimiters from purely numerical fields
                    (when (and (memq 'numerical-fields format)
                               (progn (goto-char beg-text)
                                      (looking-at "\"[0-9]+\"\\|{[0-9]+}")))
                      (goto-char end-text)
                      (delete-char -1)
                      (goto-char beg-text)
                      (delete-char 1))

                    ;; update delimiters
                    (when (memq 'delimiters format)
                      (goto-char beg-text)
                      ;; simplified from `bibtex-parse-field-text', as we
                      ;; already checked that the field format is correct
                      (while (< (point) end-text)
                        (if (looking-at bibtex-field-const)
                            (goto-char (match-end 0))
                          (let ((boundaries (bibtex-parse-field-string)))
                            (if (looking-at left-delim-re)
                                (goto-char (cdr boundaries))
                              (delete-char 1)
                              (insert (bibtex-field-left-delimiter))
                              (goto-char (1- (cdr boundaries)))
                              (delete-char 1)
                              (insert (bibtex-field-right-delimiter)))))
                        (if (looking-at "[ \t\n]*#[ \t\n]*")
                            (goto-char (match-end 0)))))

                    ;; update page dashes
                    (if (and (memq 'page-dashes format)
                             (bibtex-string= field-name "pages")
                             (progn (goto-char beg-text)
                                    (looking-at
                                     "\\([\"{][0-9]+\\)[ \t\n]*--?[ \t\n]*\\([0-9]+[\"}]\\)")))
                        (replace-match "\\1-\\2"))

                    ;; enclose field text by braces according to
                    ;; `bibtex-field-braces-alist'.
                    (let (case-fold-search temp) ; Case-sensitive search
                      (when (and (memq 'braces format)
                                 (setq temp (cdr (assoc-string field-name
                                                               bibtex-field-braces-opt t))))
                        (goto-char beg-text)
                        (while (re-search-forward temp end-text t)
                          (let ((beg (match-beginning 0))
                                (bounds (bibtex-find-text-internal nil t)))
                            (unless (or (nth 4 bounds) ; string constant
                                        ;; match already surrounded by braces
                                        ;; (braces are inside field delimiters)
                                        (and (< (point) (1- (nth 2 bounds)))
                                             (< (1+ (nth 1 bounds)) beg)
                                             (looking-at "}")
                                             (save-excursion (goto-char (1- beg))
                                                             (looking-at "{"))))
                              (insert "}")
                              (goto-char beg)
                              (insert "{")))))

                      ;; replace field text by BibTeX string constants
                      ;; according to `bibtex-field-strings-alist'.
                      (when (and (memq 'strings format)
                                 (setq temp (cdr (assoc-string field-name
                                                               bibtex-field-strings-opt t))))
                        (goto-char beg-text)
                        (dolist (re temp)
                          (while (re-search-forward (car re) end-text t)
                            (let ((bounds (save-match-data
                                            (bibtex-find-text-internal nil t))))
                              (unless (nth 4 bounds)
                                ;; if match not at right subfield boundary...
                                (if (< (match-end 0) (1- (nth 2 bounds)))
                                    (insert " # " (bibtex-field-left-delimiter))
                                  (delete-char 1))
                                (replace-match (cdr re))
                                (goto-char (match-beginning 0))
                                ;; if match not at left subfield boundary...
                                (if (< (1+ (nth 1 bounds)) (match-beginning 0))
                                    (insert (bibtex-field-right-delimiter) " # ")
                                  (delete-char -1))))))))

                    ;; use book title of crossref'd entry
                    (if (and (memq 'inherit-booktitle format)
                             empty-field
                             (bibtex-string= field-name "booktitle")
                             crossref-key)
                        (let ((title (save-excursion
                                       (save-restriction
                                         (widen)
                                         (if (bibtex-search-entry crossref-key t)
                                             (bibtex-text-in-field "title"))))))
                          (when title
                            (setq empty-field nil)
                            (goto-char (1+ beg-text))
                            (insert title))))

                    ;; if empty field is a required field, complain
                    (when (and empty-field
                               (memq 'required-fields format)
                               (assoc-string field-name req-field-list t))
                      (setq error-field-name field-name)
                      (error "Mandatory field `%s' is empty" field-name))

                    ;; unify case of field name
                    (if (memq 'unify-case format)
                        (let ((fname (car (assoc-string field-name
                                                        default-field-list t))))
                          (if fname
                              (progn
                                (delete-region beg-name end-name)
                                (goto-char beg-name)
                                (insert fname))
                            ;; there are no rules we could follow
                            (downcase-region beg-name end-name))))

                    ;; update point
                    (goto-char end-field))))

              ;; check whether all required fields are present
              (if (memq 'required-fields format)
                  (let ((alt-expect (make-vector num-alt nil))
                        (alt-found (make-vector num-alt 0)))
                    (dolist (fname req-field-list)
                      (cond ((setq idx (nth 3 fname))
                             ;; t if field has alternative flag
                             (bibtex-vec-push alt-expect idx (car fname))
                             (if (member-ignore-case (car fname) field-list)
                                 (bibtex-vec-incr alt-found idx)))
                            ((not (member-ignore-case (car fname) field-list))
                             ;; If we use the crossref field, a required field
                             ;; can have the OPT prefix.  So if it was empty,
                             ;; we have deleted by now.  Nonetheless we can
                             ;; move point on this empty field.
                             (setq error-field-name (car fname))
                             (error "Mandatory field `%s' is missing" (car fname)))))
                    (dotimes (idx num-alt)
                      (cond ((= 0 (aref alt-found idx))
                             (setq error-field-name (car (last (aref alt-fields idx))))
                             (error "Alternative mandatory field `%s' is missing"
                                    (aref alt-expect idx)))
                            ((< 1 (aref alt-found idx))
                             (setq error-field-name (car (last (aref alt-fields idx))))
                             (error "Alternative fields `%s' are defined %s times"
                                    (aref alt-expect idx)
                                    (length (aref alt-fields idx))))))))

              ;; update comma after last field
              (if (memq 'last-comma format)
                  (cond ((and bibtex-comma-after-last-field
                              (not (looking-at ",")))
                         (insert ","))
                        ((and (not bibtex-comma-after-last-field)
                              (looking-at ","))
                         (delete-char 1))))

              ;; update right entry delimiter
              (if (looking-at ",")
                  (forward-char))
              (when (memq 'delimiters format)
                (skip-chars-forward " \t\n")
                (delete-char 1)
                (insert (bibtex-entry-right-delimiter)))

              ;; realign and fill entry
              (if (memq 'realign format)
                  (bibtex-fill-entry)))))

      ;; Unwindform: move point to location where error occurred if possible
      (if error-field-name
          (let (bounds)
            (when (save-excursion
                    (bibtex-beginning-of-entry)
                    (setq bounds
                          (bibtex-search-forward-field
                           ;; If we use the crossref field, a required field
                           ;; can have the OPT prefix
                           (concat "\\(OPT\\|ALT\\)?" error-field-name) t)))
              (goto-char (bibtex-start-of-text-in-field bounds))
              (bibtex-find-text)))))))

(defun bibtex-field-re-init (regexp-alist type)
  "Calculate optimized value for bibtex-regexp-TYPE-opt.
This value is based on bibtex-regexp-TYPE-alist.  TYPE is 'braces or 'strings.
Return optimized value to be used by `bibtex-format-entry'."
  (setq regexp-alist
        (mapcar (lambda (e)
                  (list (car e)
                        (replace-regexp-in-string " +" "[ \t\n]+" (nth 1 e))
                        (nth 2 e))) ; nil for 'braces'.
                regexp-alist))
  (let (opt-list)
    ;; Loop over field names
    (dolist (field (delete-dups (apply 'append (mapcar 'car regexp-alist))))
      (let (rules)
        ;; Collect all matches we have for this field name
        (dolist (e regexp-alist)
          (if (assoc-string field (car e) t)
              (push (cons (nth 1 e) (nth 2 e)) rules)))
        (if (eq type 'braces)
            ;; concatenate all regexps to a single regexp
            (setq rules (concat "\\(?:" (mapconcat 'car rules "\\|") "\\)")))
        ;; create list of replacement rules.
        (push (cons field rules) opt-list)))
    opt-list))


(defun bibtex-autokey-abbrev (string len)
  "Return an abbreviation of STRING with at least LEN characters.
If LEN is positive the abbreviation is terminated only after a consonant
or at the word end.  If LEN is negative the abbreviation is strictly
enforced using abs (LEN) characters.  If LEN is not a number, STRING
is returned unchanged."
  (cond ((or (not (numberp len))
             (<= (length string) (abs len)))
         string)
        ((equal len 0)
         "")
        ((< len 0)
         (substring string 0 (abs len)))
        (t (let* ((case-fold-search t)
                  (abort-char (string-match "[^aeiou]" string (1- len))))
             (if abort-char
                 (substring string 0 (1+ abort-char))
               string)))))

(defun bibtex-autokey-get-field (field &optional change-list)
  "Get content of BibTeX field FIELD.  Return empty string if not found.
Optional arg CHANGE-LIST is a list of substitution patterns that is
applied to the content of FIELD.  It is an alist with pairs
\(OLD-REGEXP . NEW-STRING\)."
  (let* ((bibtex-expand-strings bibtex-autokey-expand-strings)
         (content (bibtex-text-in-field field bibtex-autokey-use-crossref))
        case-fold-search)
    (unless content (setq content ""))
    (dolist (pattern change-list)
      (setq content (replace-regexp-in-string (car pattern)
                                              (cdr pattern)
                                              content t)))
    content))

(defun bibtex-autokey-get-names ()
  "Get contents of the name field of the current entry.
Do some modifications based on `bibtex-autokey-name-change-strings'.
Return the names as a concatenated string obeying `bibtex-autokey-names'
and `bibtex-autokey-names-stretch'."
  (let ((names (bibtex-autokey-get-field "author\\|editor"
                                         bibtex-autokey-name-change-strings)))
    ;; Some entries do not have a name field.
    (if (string= "" names)
        names
      (let* ((case-fold-search t)
             (name-list (mapcar 'bibtex-autokey-demangle-name
                                (split-string names "[ \t\n]+and[ \t\n]+")))
             additional-names)
        (unless (or (not (numberp bibtex-autokey-names))
                    (<= (length name-list)
                        (+ bibtex-autokey-names
                           bibtex-autokey-names-stretch)))
          ;; Take `bibtex-autokey-names' elements from beginning of name-list
          (setq name-list (nreverse (nthcdr (- (length name-list)
                                               bibtex-autokey-names)
                                            (nreverse name-list)))
                additional-names bibtex-autokey-additional-names))
        (concat (mapconcat 'identity name-list
                           bibtex-autokey-name-separator)
                additional-names)))))

(defun bibtex-autokey-demangle-name (fullname)
  "Get the last part from a well-formed FULLNAME and perform abbreviations."
  (let* (case-fold-search
         (name (cond ((string-match "\\([[:upper:]][^, ]*\\)[^,]*," fullname)
                      ;; Name is of the form "von Last, First" or
                      ;; "von Last, Jr, First"
                      ;; --> Take the first capital part before the comma
                      (match-string 1 fullname))
                     ((string-match "\\([^, ]*\\)," fullname)
                      ;; Strange name: we have a comma, but nothing capital
                      ;; So we accept even lowercase names
                      (match-string 1 fullname))
                     ((string-match "\\(\\<[[:lower:]][^ ]* +\\)+\\([[:upper:]][^ ]*\\)"
                                    fullname)
                      ;; name is of the form "First von Last", "von Last",
                      ;; "First von von Last", or "d'Last"
                      ;; --> take the first capital part after the "von" parts
                      (match-string 2 fullname))
                     ((string-match "\\([^ ]+\\) *\\'" fullname)
                      ;; name is of the form "First Middle Last" or "Last"
                      ;; --> take the last token
                      (match-string 1 fullname))
                     (t (error "Name `%s' is incorrectly formed" fullname)))))
    (funcall bibtex-autokey-name-case-convert-function
             (bibtex-autokey-abbrev name bibtex-autokey-name-length))))

(defun bibtex-autokey-get-year ()
  "Return year field contents as a string obeying `bibtex-autokey-year-length'."
  (let ((yearfield (bibtex-autokey-get-field "year")))
    (substring yearfield (max 0 (- (length yearfield)
                                   bibtex-autokey-year-length)))))

(defun bibtex-autokey-get-title ()
  "Get title field contents up to a terminator.
Return the result as a string"
  (let ((case-fold-search t)
        (titlestring
         (bibtex-autokey-get-field "title"
                                   bibtex-autokey-titleword-change-strings)))
    ;; ignore everything past a terminator
    (if (string-match bibtex-autokey-title-terminators titlestring)
        (setq titlestring (substring titlestring 0 (match-beginning 0))))
    ;; gather words from titlestring into a list.  Ignore
    ;; specific words and use only a specific amount of words.
    (let ((counter 0)
	  (ignore-re (concat "\\`\\(?:"
                             (mapconcat 'identity
                                        bibtex-autokey-titleword-ignore "\\|")
                             "\\)\\'"))
          titlewords titlewords-extra word)
      (while (and (or (not (numberp bibtex-autokey-titlewords))
                      (< counter (+ bibtex-autokey-titlewords
                                    bibtex-autokey-titlewords-stretch)))
                  (string-match "\\b\\w+" titlestring))
        (setq word (match-string 0 titlestring)
              titlestring (substring titlestring (match-end 0)))
        ;; Ignore words matched by one of the elements of
        ;; `bibtex-autokey-titleword-ignore'.  Case is significant.
        (unless (let (case-fold-search)
		  (string-match ignore-re word))
          (setq counter (1+ counter))
          (if (or (not (numberp bibtex-autokey-titlewords))
                  (<= counter bibtex-autokey-titlewords))
              (push word titlewords)
            (push word titlewords-extra))))
      ;; Obey `bibtex-autokey-titlewords-stretch':
      ;; If by now we have processed all words in titlestring, we include
      ;; titlewords-extra in titlewords.  Otherwise, we ignore titlewords-extra.
      (unless (string-match "\\b\\w+" titlestring)
        (setq titlewords (append titlewords-extra titlewords)))
      (mapconcat 'bibtex-autokey-demangle-title (nreverse titlewords)
                 bibtex-autokey-titleword-separator))))

(defun bibtex-autokey-demangle-title (titleword)
  "Do some abbreviations on TITLEWORD.
The rules are defined in `bibtex-autokey-titleword-abbrevs'
and `bibtex-autokey-titleword-length'."
  (let ((case-fold-search t)
        (alist bibtex-autokey-titleword-abbrevs))
    (while (and alist
                (not (string-match (concat "\\`\\(?:" (caar alist) "\\)\\'")
                                   titleword)))
      (setq alist (cdr alist)))
    (if alist
        (cdar alist)
      (funcall bibtex-autokey-titleword-case-convert-function
               (bibtex-autokey-abbrev titleword bibtex-autokey-titleword-length)))))

(defun bibtex-generate-autokey ()
  "Generate automatically a key for a BibTeX entry.
Use the author/editor, the year and the title field.
The algorithm works as follows.

The name part:
 1. Use the author or editor field to generate the name part of the key.
    Expand BibTeX strings if `bibtex-autokey-expand-strings' is non-nil.
 2. Change the content of the name field according to
    `bibtex-autokey-name-change-strings' (see there for further detail).
 3. Use the first `bibtex-autokey-names' names in the name field.  If there
    are up to `bibtex-autokey-names' + `bibtex-autokey-names-stretch' names,
    use all names.
 4. Use only the last names to form the name part.  From these last names,
    take at least `bibtex-autokey-name-length' characters (truncate only
    after a consonant or at a word end).
 5. Convert all last names using the function
    `bibtex-autokey-name-case-convert-function'.
 6. Build the name part of the key by concatenating all abbreviated last
    names with the string `bibtex-autokey-name-separator' between any two.
    If there are more names in the name field than names used in the name
    part, append the string `bibtex-autokey-additional-names'.

The year part:
 1. Build the year part of the key by truncating the content of the year
    field to the rightmost `bibtex-autokey-year-length' digits (useful
    values are 2 and 4).
 2. If the year field (or any other field required to generate the key)
    is absent, but the entry has a valid crossref field and
    `bibtex-autokey-use-crossref' is non-nil, use the field of the
    crossreferenced entry instead.

The title part
 1. Change the content of the title field according to
    `bibtex-autokey-titleword-change-strings' (see there for further detail).
 2. Truncate the title before the first match of
    `bibtex-autokey-title-terminators' and delete those words which appear
    in `bibtex-autokey-titleword-ignore'.  Build the title part using the
    first `bibtex-autokey-titlewords' words from this truncated title.
    If the truncated title ends after up to `bibtex-autokey-titlewords' +
    `bibtex-autokey-titlewords-stretch' words, use all words from the
    truncated title.
 3. For every title word that appears in `bibtex-autokey-titleword-abbrevs'
    use the corresponding abbreviation (see documentation of this variable
    for further detail).
 4. From every title word not generated by an abbreviation, take at least
    `bibtex-autokey-titleword-length' characters (truncate only after
    a consonant or at a word end).
 5. Convert all title words using the function
    `bibtex-autokey-titleword-case-convert-function'.
 6. Build the title part by concatenating all abbreviated title words with
    the string `bibtex-autokey-titleword-separator' between any two.

Concatenate the key:
 1. Concatenate `bibtex-autokey-prefix-string', the name part, the year
    part and the title part.  If the name part and the year part are both
    non-empty insert `bibtex-autokey-name-year-separator' between the two.
    If the title part and the year (or name) part are non-empty, insert
    `bibtex-autokey-year-title-separator' between the two.
 2. If `bibtex-autokey-before-presentation-function' is non-nil, it must be
    a function taking one argument.  Call this function with the generated
    key as the argument.  Use the return value of this function (a string)
    as the key.
 3. If `bibtex-autokey-edit-before-use' is non-nil, present the key in the
    minibuffer to the user for editing.  Insert the key given by the user."
  (let* ((names (bibtex-autokey-get-names))
         (year (bibtex-autokey-get-year))
         (title (bibtex-autokey-get-title))
         (autokey (concat bibtex-autokey-prefix-string
                          names
                          (unless (or (equal names "")
                                      (equal year ""))
                            bibtex-autokey-name-year-separator)
                          year
                          (unless (or (and (equal names "")
                                           (equal year ""))
                                      (equal title ""))
                            bibtex-autokey-year-title-separator)
                          title)))
    (if bibtex-autokey-before-presentation-function
        (funcall bibtex-autokey-before-presentation-function autokey)
      autokey)))


(defun bibtex-global-key-alist ()
  "Return global key alist based on `bibtex-files'."
  (if bibtex-files
      (apply 'append
             (mapcar (lambda (buf)
                       (with-current-buffer buf bibtex-reference-keys))
                     ;; include current buffer only if it uses `bibtex-mode'
                     (bibtex-initialize (eq major-mode 'bibtex-mode))))
    (if (eq major-mode 'bibtex-mode)
        bibtex-reference-keys)))

(defun bibtex-read-key (prompt &optional key global)
  "Read BibTeX key from minibuffer using PROMPT and default KEY.
If optional arg GLOBAL is non-nil, completion is based on the keys in
`bibtex-reference-keys' of `bibtex-files',"
  (let (completion-ignore-case)
    (completing-read prompt (if global (bibtex-global-key-alist)
                              bibtex-reference-keys)
                     nil nil key 'bibtex-key-history)))

(defun bibtex-read-string-key (&optional key)
  "Read BibTeX string key from minibuffer using default KEY."
  (let ((completion-ignore-case t))
    (completing-read "String key: " bibtex-strings
                     nil nil key 'bibtex-key-history)))

(defun bibtex-parse-keys (&optional abortable verbose)
  "Set `bibtex-reference-keys' to the keys used in the whole buffer.
Find both entry keys and crossref entries.  If ABORTABLE is non-nil abort
on user input.  If VERBOSE is non-nil give messages about progress.
Return alist of keys if parsing was completed, `aborted' otherwise.
If `bibtex-parse-keys-fast' is non-nil, use fast but simplified algorithm
for parsing BibTeX keys.  If parsing fails, try to set this variable to nil."
  (if (eq major-mode 'bibtex-mode)
      (let (ref-keys crossref-keys)
        (save-excursion
          (save-match-data
            (if verbose
                (bibtex-progress-message
                 (concat (buffer-name) ": parsing reference keys")))
            (catch 'userkey
              (goto-char (point-min))
              (if bibtex-parse-keys-fast
                  (let ((case-fold-search t)
                        (re (concat bibtex-entry-head "\\|"
                                    ",[ \t\n]*crossref[ \t\n]*=[ \t\n]*"
                                    "\\(\"[^\"]*\"\\|{[^}]*}\\)[ \t\n]*[,})]")))
                    (while (re-search-forward re nil t)
                      (if (and abortable (input-pending-p))
                          ;; user has aborted by typing a key: return `aborted'
                          (throw 'userkey 'aborted))
                      (cond ((match-end 3)
                             ;; This is a crossref.
                             (let ((key (buffer-substring-no-properties
                                         (1+ (match-beginning 3)) (1- (match-end 3)))))
                               (unless (assoc key crossref-keys)
                                 (push (list key) crossref-keys))))
                            ;; only keys of known entries
                            ((assoc-string (bibtex-type-in-head)
                                           bibtex-entry-alist t)
                             ;; This is an entry.
                             (let ((key (bibtex-key-in-head)))
                               (unless (assoc key ref-keys)
                                 (push (cons key t) ref-keys)))))))

                (let (;; ignore @String entries because they are handled
                      ;; separately by `bibtex-parse-strings'
                      (bibtex-sort-ignore-string-entries t)
                      bounds)
                  (bibtex-map-entries
                   (lambda (key _beg end)
                     (if (and abortable
                              (input-pending-p))
                         ;; user has aborted by typing a key: return `aborted'
                         (throw 'userkey 'aborted))
                     (if verbose (bibtex-progress-message))
                     (unless (assoc key ref-keys)
                       (push (cons key t) ref-keys))
                     (if (and (setq bounds (bibtex-search-forward-field "crossref" end))
                              (setq key (bibtex-text-in-field-bounds bounds t))
                              (not (assoc key crossref-keys)))
                         (push (list key) crossref-keys))))))

              (dolist (key crossref-keys)
                (unless (assoc (car key) ref-keys) (push key ref-keys)))
              (if verbose
                  (bibtex-progress-message 'done))
              ;; successful operation --> return `bibtex-reference-keys'
              (setq bibtex-reference-keys ref-keys)))))))

(defun bibtex-parse-strings (&optional add abortable)
  "Set `bibtex-strings' to the string definitions in the whole buffer.
If ADD is non-nil add the new strings to `bibtex-strings' instead of
simply resetting it.  If ADD is an alist of strings, also add ADD to
`bibtex-strings'.  If ABORTABLE is non-nil abort on user input.
Return alist of strings if parsing was completed, `aborted' otherwise."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((strings (if (and add (not (functionp bibtex-strings)))
                         bibtex-strings))
            bounds key)
        (if (listp add)
            (dolist (string add)
              (unless (assoc-string (car string) strings t)
                (push string strings))))
        (catch 'userkey
          (while (setq bounds (bibtex-search-forward-string))
            (if (and abortable
                     (input-pending-p))
                ;; user has aborted by typing a key --> return `aborted'
                (throw 'userkey 'aborted))
            (setq key (bibtex-reference-key-in-string bounds))
            (unless (assoc-string key strings t)
              (push (cons key (bibtex-text-in-string bounds t))
                    strings))
            (goto-char (bibtex-end-of-text-in-string bounds)))
          ;; successful operation --> return `bibtex-strings'
          (setq bibtex-strings strings))))))

(defun bibtex-strings ()
  "Return `bibtex-strings'.  Initialize this variable if necessary."
  (if (functionp bibtex-strings)
      (bibtex-parse-strings (bibtex-string-files-init))
    bibtex-strings))

(defun bibtex-string-files-init ()
  "Return initialization for `bibtex-strings'.
Use `bibtex-predefined-strings' and BibTeX files `bibtex-string-files'."
  (save-match-data
    (let ((dirlist (split-string (or bibtex-string-file-path default-directory)
                                 ":+"))
          (case-fold-search)
          string-files fullfilename compl bounds found)
      ;; collect absolute file names of valid string files
      (dolist (filename bibtex-string-files)
        (unless (string-match "\\.bib\\'" filename)
          (setq filename (concat filename ".bib")))
        ;; test filenames
        (if (file-name-absolute-p filename)
            (if (file-readable-p filename)
                (push filename string-files)
              (error "BibTeX strings file %s not found" filename))
          (dolist (dir dirlist)
            (when (file-readable-p
                   (setq fullfilename (expand-file-name filename dir)))
              (push fullfilename string-files)
              (setq found t)))
          (unless found
            (error "File %s not in paths defined via bibtex-string-file-path"
                   filename))))
      ;; parse string files
      (dolist (filename string-files)
        (with-temp-buffer
          (insert-file-contents filename)
          (goto-char (point-min))
          (while (setq bounds (bibtex-search-forward-string))
            (push (cons (bibtex-reference-key-in-string bounds)
                        (bibtex-text-in-string bounds t))
                  compl)
            (goto-char (bibtex-end-of-string bounds)))))
      (append bibtex-predefined-strings (nreverse compl)))))

(defun bibtex-parse-buffers-stealthily ()
  "Parse buffer in the background during idle time.
Called by `run-with-idle-timer'.  Whenever Emacs has been idle
for `bibtex-parse-keys-timeout' seconds, parse all BibTeX buffers
which have been modified after last parsing.
Parsing initializes `bibtex-reference-keys' and `bibtex-strings'."
  (save-excursion
    (let ((buffers (buffer-list))
          (strings-init (bibtex-string-files-init)))
      (while (and buffers (not (input-pending-p)))
        (set-buffer (car buffers))
        (if (and (eq major-mode 'bibtex-mode)
                 (not (eq (buffer-modified-tick)
                          bibtex-buffer-last-parsed-tick)))
            (save-restriction
              (widen)
              ;; Output no progress messages in `bibtex-parse-keys'
              ;; because when in `y-or-n-p' that can hide the question.
              (if (and (listp (bibtex-parse-keys t))
                       ;; update `bibtex-strings'
                       (listp (bibtex-parse-strings strings-init t)))

                  ;; remember that parsing was successful
                  (setq bibtex-buffer-last-parsed-tick (buffer-modified-tick)))))
        (setq buffers (cdr buffers))))))

;;;###autoload
(defun bibtex-initialize (&optional current force select)
  "(Re)Initialize BibTeX buffers.
Visit the BibTeX files defined by `bibtex-files' and return a list
of corresponding buffers.
Initialize in these buffers `bibtex-reference-keys' if not yet set.
List of BibTeX buffers includes current buffer if CURRENT is non-nil.
If FORCE is non-nil, (re)initialize `bibtex-reference-keys' even if
already set.  If SELECT is non-nil interactively select a BibTeX buffer.
When called interactively, FORCE is t, CURRENT is t if current buffer uses
`bibtex-mode', and SELECT is t if current buffer does not use `bibtex-mode',"
  (interactive (list (eq major-mode 'bibtex-mode) t
                     (not (eq major-mode 'bibtex-mode))))
  (let ((file-path (split-string (or bibtex-file-path default-directory) ":+"))
        file-list dir-list buffer-list)
    ;; generate list of BibTeX files
    (dolist (file bibtex-files)
      (cond ((eq file 'bibtex-file-path)
             (setq dir-list (append dir-list file-path)))
            ((file-accessible-directory-p file)
             (push file dir-list))
            ((progn (unless (string-match "\\.bib\\'" file)
                      (setq file (concat file ".bib")))
                    (file-name-absolute-p file))
             (push file file-list))
            (t
             (let (expanded-file-name found)
               (dolist (dir file-path)
                 (when (file-readable-p
                        (setq expanded-file-name (expand-file-name file dir)))
                   (push expanded-file-name file-list)
                   (setq found t)))
               (unless found
                 (error "File `%s' not in paths defined via bibtex-file-path"
                        file))))))
    (dolist (file file-list)
      (unless (file-readable-p file)
        (error "BibTeX file `%s' not found" file)))
    ;; expand dir-list
    (dolist (dir dir-list)
      (setq file-list
            (append file-list (directory-files dir t "\\.bib\\'" t))))
    (delete-dups file-list)
    ;; visit files in FILE-LIST
    (dolist (file file-list)
      (if (file-readable-p file)
        (push (find-file-noselect file) buffer-list)))
    ;; Include current buffer iff we want it.
    ;; Exclude current buffer if it doesn't use `bibtex-mode'.
    ;; Thus calling `bibtex-initialize' gives meaningful results for
    ;; any current buffer.
    (unless (and current (eq major-mode 'bibtex-mode)) (setq current nil))
    (cond ((and current (not (memq (current-buffer) buffer-list)))
           (push (current-buffer) buffer-list))
          ((and (not current) (memq (current-buffer) buffer-list))
           (setq buffer-list (delq (current-buffer) buffer-list))))
    ;; parse keys
    (let (string-init)
      (dolist (buffer buffer-list)
        (with-current-buffer buffer
          (if (or force (functionp bibtex-reference-keys))
              (bibtex-parse-keys))
          (when (or force (functionp bibtex-strings))
            (unless string-init (setq string-init (bibtex-string-files-init)))
            (bibtex-parse-strings string-init)))))
    ;; select BibTeX buffer
    (if select
        (if buffer-list
            (switch-to-buffer
             (completing-read "Switch to BibTeX buffer: "
                              (mapcar 'buffer-name buffer-list)
                              nil t
                              (if current (buffer-name (current-buffer)))))
          (message "No BibTeX buffers defined")))
    buffer-list))

(defun bibtex-complete-string-cleanup (compl) (lambda (str status) ;Curried.
  "Cleanup after inserting string STR.
Remove enclosing field delimiters for STR.  Display message with
expansion of STR using expansion list COMPL."
  (when (memq status '(exact finished sole))
    (let ((abbr (cdr (assoc-string str compl t))))
      (when abbr
        (message "%s = abbreviation for `%s'" str abbr)))
    (when (eq status 'finished)
      (save-excursion (bibtex-remove-delimiters))))))

(defun bibtex-complete-crossref-cleanup (buf) (lambda (key status) ;Curried.
  "Display summary message on entry KEY after completion of a crossref key.
Use `bibtex-summary-function' to generate summary."
  (when (memq status '(exact sole finished))
    (let ((summary
           (with-current-buffer buf
             (save-excursion
               (if (bibtex-search-entry key t)
                   (funcall bibtex-summary-function))))))
      (when summary
        (message "%s %s" key summary))))))

(defun bibtex-copy-summary-as-kill (&optional arg)
  "Push summery of current BibTeX entry to kill ring.
Use `bibtex-summary-function' to generate summary.
If prefix ARG is non-nil push BibTeX entry's URL to kill ring
that is generated by calling `bibtex-url'."
  (interactive "P")
  (if arg (let ((url (bibtex-url nil t)))
            (if url (kill-new (message "%s" url))
              (message "No URL known")))
    (save-excursion
      (bibtex-beginning-of-entry)
      (if (looking-at bibtex-entry-maybe-empty-head)
          (kill-new (message "%s" (funcall bibtex-summary-function)))
        (error "No entry found")))))

(defun bibtex-summary ()
  "Return summary of current BibTeX entry.
Used as default value of `bibtex-summary-function'."
  ;; It would be neat to make this function customizable.  How?
  (if (looking-at bibtex-entry-maybe-empty-head)
      (let* ((bibtex-autokey-name-case-convert-function 'identity)
             (bibtex-autokey-name-length 'infty)
             (bibtex-autokey-names 1)
             (bibtex-autokey-names-stretch 0)
             (bibtex-autokey-name-separator " ")
             (bibtex-autokey-additional-names " etal")
             (names (bibtex-autokey-get-names))
             (bibtex-autokey-year-length 4)
             (year (bibtex-autokey-get-year))
             (bibtex-autokey-titlewords 5)
             (bibtex-autokey-titlewords-stretch 2)
             (bibtex-autokey-titleword-case-convert-function 'identity)
             (bibtex-autokey-titleword-length 5)
             (bibtex-autokey-titleword-separator " ")
             (title (bibtex-autokey-get-title))
             (journal (bibtex-autokey-get-field
                       "journal" bibtex-autokey-transcriptions))
             (volume (bibtex-autokey-get-field "volume"))
             (pages (bibtex-autokey-get-field "pages" '(("-.*\\'" . "")))))
        (mapconcat (lambda (arg)
                     (if (not (string= "" (cdr arg)))
                         (concat (car arg) (cdr arg))))
                   `((" " . ,names) (" " . ,year) (": " . ,title)
                     (", " . ,journal) (" " . ,volume) (":" . ,pages))
                   ""))
    (error "Entry not found")))

(defun bibtex-pop (arg direction)
  "Fill current field from the ARGth same field's text in DIRECTION.
Generic function used by `bibtex-pop-previous' and `bibtex-pop-next'."
  ;; parse current field
  (let* ((bounds (bibtex-enclosing-field t))
         (start-old-field (bibtex-start-of-field bounds))
         (start-old-text (bibtex-start-of-text-in-field bounds))
         (end-old-text (bibtex-end-of-text-in-field bounds))
         (field-name (bibtex-name-in-field bounds t))
         failure)
    (save-excursion
      ;; if executed several times in a row, start each search where
      ;; the last one was finished
      (cond ((eq last-command 'bibtex-pop)
             (goto-char (if (eq direction 'previous)
                            bibtex-pop-previous-search-point
                          bibtex-pop-next-search-point)))
            ((eq direction 'previous)
             (bibtex-beginning-of-entry))
            (t (bibtex-end-of-entry)))
      ;; Search for arg'th previous/next similar field
      (while (and (not failure)
                  (>= (setq arg (1- arg)) 0))
        ;; The search of BibTeX fields is not bounded by entry boundaries
        (if (eq direction 'previous)
            (if (setq bounds (bibtex-search-backward-field field-name))
                (goto-char (bibtex-start-of-field bounds))
              (setq failure t))
          (if (setq bounds (bibtex-search-forward-field field-name))
              (goto-char (bibtex-end-of-field bounds))
            (setq failure t))))
      (if failure
          (error "No %s matching BibTeX field"
                 (if (eq direction 'previous) "previous" "next"))
        ;; Found a matching field.  Remember boundaries.
        (let ((new-text (bibtex-text-in-field-bounds bounds))
              (nbeg (copy-marker (bibtex-start-of-field bounds)))
              (nend (copy-marker (bibtex-end-of-field bounds))))
          (bibtex-flash-head "From: ")
          ;; Go back to where we started, delete old text, and pop new.
          (goto-char end-old-text)
          (delete-region start-old-text end-old-text)
          (if (= nbeg start-old-field)
              (insert (bibtex-field-left-delimiter)
                      (bibtex-field-right-delimiter))
            (insert new-text))
          (setq bibtex-pop-previous-search-point (marker-position nbeg)
                bibtex-pop-next-search-point (marker-position nend))))))
  (bibtex-find-text nil nil nil t)
  (setq this-command 'bibtex-pop))

(defun bibtex-beginning-of-field ()
  "Move point backward to beginning of field.
This function uses a simple, fast algorithm assuming that the field
begins at the beginning of a line.  We use this function for font-locking."
  (let ((field-reg (concat "^[ \t]*" bibtex-field-name "[ \t]*=")))
    (beginning-of-line)
    (unless (looking-at field-reg)
      (re-search-backward field-reg nil t))))

(defun bibtex-font-lock-url (bound &optional no-button)
  "Font-lock for URLs.  BOUND limits the search.
If NO-BUTTON is non-nil do not generate buttons."
  (let ((case-fold-search t)
        (pnt (point))
        name bounds start end found)
    (bibtex-beginning-of-field)
    (while (and (not found)
                (<= (point) bound)
		(prog1 (re-search-forward bibtex-font-lock-url-regexp bound t)
		  (setq name (match-string-no-properties 1)))
		(setq bounds (bibtex-parse-field-text))
                (progn
                  (setq start (car bounds) end (nth 1 bounds))
                  ;; Always ignore field delimiters
                  (if (memq (char-before end) '(?\} ?\"))
                      (setq end (1- end)))
                  (if (memq (char-after start) '(?\{ ?\"))
                      (setq start (1+ start)))
                  (if (< start pnt) (setq start (min pnt end)))
                  (<= start bound)))
      (if (<= pnt start)
          (let ((lst bibtex-generate-url-list) url)
            (while (and (not found) (setq url (car (pop lst))))
              (goto-char start)
              (setq found (and (bibtex-string= name (car url))
                               (re-search-forward (cdr url) end t))))))
      (unless found (goto-char end)))
    (if (and found (not no-button))
        (bibtex-button (match-beginning 0) (match-end 0)
                       'bibtex-url (match-beginning 0)))
    found))

(defun bibtex-font-lock-crossref (bound)
  "Font-lock for crossref fields.  BOUND limits the search."
  (let ((case-fold-search t)
        (pnt (point))
        (crossref-reg (concat "^[ \t]*crossref[ \t]*=[ \t\n]*"
                              "\\(\"[^\"]*\"\\|{[^}]*}\\)[ \t\n]*[,})]"))
	start end found)
    (bibtex-beginning-of-field)
    (while (and (not found)
		(re-search-forward crossref-reg bound t))
      (setq start (1+ (match-beginning 1))
            end (1- (match-end 1))
            found (>= start pnt)))
    (if found (bibtex-button start end 'bibtex-search-crossref
                             (buffer-substring-no-properties start end)
                             start t))
    found))

(defun bibtex-font-lock-cite (matcher bound)
  "Font-lock for cited keys.
MATCHER identifies the cited key, see `bibtex-cite-matcher-alist'.
BOUND limits the search."
  (let (case-fold-search)
    (if (re-search-forward (car matcher) bound t)
        (let ((start (match-beginning (cdr matcher)))
              (end (match-end (cdr matcher))))
          (bibtex-button start end 'bibtex-search-crossref
                         (buffer-substring-no-properties start end)
                         start t t)
          t))))

(defun bibtex-button-action (button)
  "Call BUTTON's BibTeX function."
  (apply (button-get button 'bibtex-function)
         (button-get button 'bibtex-args)))

(define-button-type 'bibtex-url
  'action 'bibtex-button-action
  'bibtex-function 'bibtex-url
  'help-echo (purecopy "mouse-2, RET: follow URL"))

(define-button-type 'bibtex-search-crossref
  'action 'bibtex-button-action
  'bibtex-function 'bibtex-search-crossref
  'help-echo (purecopy "mouse-2, RET: follow crossref"))

(defun bibtex-button (beg end type &rest args)
  "Make a BibTeX button from BEG to END of type TYPE in the current buffer."
  (make-text-button beg end 'type type 'bibtex-args args))


;; Interactive Functions:

;;;###autoload
(define-derived-mode bibtex-mode nil "BibTeX"
  "Major mode for editing BibTeX files.

General information on working with BibTeX mode:

Use commands such as \\<bibtex-mode-map>\\[bibtex-Book] to get a template for a specific entry.
Then fill in all desired fields using \\[bibtex-next-field] to jump from field
to field.  After having filled in all desired fields in the entry, clean the
new entry with the command \\[bibtex-clean-entry].

Some features of BibTeX mode are available only by setting the variable
`bibtex-maintain-sorted-entries' to non-nil.  However, then BibTeX mode
works only with buffers containing valid (syntactically correct) and sorted
entries.  This is usually the case, if you have created a buffer completely
with BibTeX mode and finished every new entry with \\[bibtex-clean-entry].

For third party BibTeX files, call the command \\[bibtex-convert-alien]
to fully take advantage of all features of BibTeX mode.


Special information:

A command such as \\[bibtex-Book] outlines the fields for a BibTeX book entry.

The names of optional fields start with the string OPT, and are thus ignored
by BibTeX.  The names of alternative fields from which only one is required
start with the string ALT.  The OPT or ALT string may be removed from
the name of a field with \\[bibtex-remove-OPT-or-ALT].
\\[bibtex-make-field] inserts a new field after the current one.
\\[bibtex-kill-field] kills the current field entirely.
\\[bibtex-yank] yanks the last recently killed field after the current field.
\\[bibtex-remove-delimiters] removes the double-quotes or braces around the text of the current field.
\\[bibtex-empty-field] replaces the text of the current field with the default \"\" or {}.
\\[bibtex-find-text] moves point to the end of the current field.
\\[completion-at-point] completes word fragment before point according to context.

The command \\[bibtex-clean-entry] cleans the current entry, i.e. it removes OPT/ALT
from the names of all non-empty optional or alternative fields, checks that
no required fields are empty, and does some formatting dependent on the value
of `bibtex-entry-format'.  Furthermore, it can automatically generate a key
for the BibTeX entry, see `bibtex-generate-autokey'.
Note: some functions in BibTeX mode depend on entries being in a special
format (all fields beginning on separate lines), so it is usually a bad
idea to remove `realign' from `bibtex-entry-format'.

BibTeX mode supports Imenu and hideshow minor mode (`hs-minor-mode').

----------------------------------------------------------
Entry to BibTeX mode calls the value of `bibtex-mode-hook'
if that value is non-nil.

\\{bibtex-mode-map}"
  (add-hook 'completion-at-point-functions
            'bibtex-completion-at-point-function nil 'local)
  (make-local-variable 'bibtex-buffer-last-parsed-tick)
  ;; Install stealthy parse function if not already installed
  (unless bibtex-parse-idle-timer
    (setq bibtex-parse-idle-timer (run-with-idle-timer
                                   bibtex-parse-keys-timeout t
                                   'bibtex-parse-buffers-stealthily)))
  (set (make-local-variable 'paragraph-start) "[ \f\n\t]*$")
  (set (make-local-variable 'comment-start) bibtex-comment-start)
  (set (make-local-variable 'comment-start-skip)
       (concat (regexp-quote bibtex-comment-start) "\\>[ \t]*"))
  (set (make-local-variable 'comment-column) 0)
  (set (make-local-variable 'defun-prompt-regexp) "^[ \t]*@[[:alnum:]]+[ \t]*")
  (set (make-local-variable 'outline-regexp) "[ \t]*@")
  (set (make-local-variable 'fill-paragraph-function) 'bibtex-fill-field)
  (set (make-local-variable 'fill-prefix)
       (make-string (+ bibtex-entry-offset bibtex-contline-indentation) ?\s))
  (set (make-local-variable 'font-lock-defaults)
       '(bibtex-font-lock-keywords
         nil t ((?$ . "\"")
                ;; Mathematical expressions should be fontified as strings
                (?\" . ".")
                ;; Quotes are field delimiters and quote-delimited
                ;; entries should be fontified in the same way as
                ;; brace-delimited ones
                )
         nil
         (font-lock-extra-managed-props . (category))
	 (font-lock-mark-block-function
	  . (lambda ()
              (set-mark (bibtex-end-of-entry))
	      (bibtex-beginning-of-entry)))))
  (set (make-local-variable 'syntax-propertize-function)
       (syntax-propertize-via-font-lock
        bibtex-font-lock-syntactic-keywords))
  (setq imenu-generic-expression
        (list (list nil bibtex-entry-head bibtex-key-in-head))
        imenu-case-fold-search t)
  ;; Allow `bibtex-dialect' as a file-local variable.
  (add-hook 'hack-local-variables-hook 'bibtex-set-dialect nil t))

(defun bibtex-entry-alist (dialect)
  "Return entry-alist for DIALECT."
  (let ((var (intern (format "bibtex-%s-entry-alist" dialect)))
        entry-alist)
    (if (boundp var)
        (setq entry-alist (symbol-value var))
      (error "BibTeX dialect `%s' undefined" dialect))
    (if (not (consp (nth 1 (car entry-alist))))
        ;; new format
        entry-alist
      ;; Convert old format of `bibtex-entry-field-alist'
      (unless (get var 'entry-list-format)
        (put var 'entry-list-format "pre-24")
        (message "Old format of `%s' (pre GNU Emacs 24).
Please convert to the new format."
                 (if (eq (indirect-variable 'bibtex-entry-field-alist) var)
                     'bibtex-entry-field-alist var))
        (sit-for 3))
      (let (lst)
        (dolist (entry entry-alist)
          (let ((fl (nth 1 entry)) req xref opt)
            (dolist (field (copy-tree (car fl)))
              (if (nth 3 field) (setcar (nthcdr 3 field) 0))
              (if (or (not (nth 2 entry))
                      (assoc-string (car field) (car (nth 2 entry)) t))
                  (push field req)
                (push field xref)))
            (dolist (field (nth 1 fl))
              (push field opt))
            (push (list (car entry) nil (nreverse req)
                        (nreverse xref) (nreverse opt))
                  lst)))
        (nreverse lst)))))

(defun bibtex-set-dialect (&optional dialect local)
  "Select BibTeX DIALECT for editing BibTeX files.
This sets the user variable `bibtex-dialect' as well as the dialect-dependent
internal variables.  Allowed dialects are listed in `bibtex-dialect-list'.
If DIALECT is nil use current value of `bibtex-dialect'.
If LOCAL is non-nil make buffer-local bindings for these variables rather than
setting the global values.  The dialect-dependent internal variables
are also bound buffer-locally if `bibtex-dialect' is already buffer-local
in the current buffer (for example, as a file-local variable).
LOCAL is t for interactive calls."
  (interactive (list (intern (completing-read "Dialect: "
                                              (mapcar 'list bibtex-dialect-list)
                                              nil t)) t))
  (let ((setfun (if (or local (local-variable-p 'bibtex-dialect))
                    (lambda (var val) (set (make-local-variable var) val))
                  'set)))
    (if dialect (funcall setfun 'bibtex-dialect dialect))

    ;; Set internal variables
    (funcall setfun 'bibtex-entry-alist (bibtex-entry-alist bibtex-dialect))
    (funcall setfun 'bibtex-field-alist
             (let ((var (intern (format "bibtex-%s-field-alist"
                                        bibtex-dialect))))
               (if (boundp var)
                   (symbol-value var)
                 (error "Field types for BibTeX dialect `%s' undefined"
                        bibtex-dialect))))
    (funcall setfun 'bibtex-entry-type
             (concat "@[ \t]*\\(?:"
                     (regexp-opt (mapcar 'car bibtex-entry-alist)) "\\)"))
    (funcall setfun 'bibtex-entry-head
             (concat "^[ \t]*\\(" bibtex-entry-type "\\)[ \t]*[({][ \t\n]*\\("
                     bibtex-reference-key "\\)"))
    (funcall setfun 'bibtex-entry-maybe-empty-head
             (concat bibtex-entry-head "?"))
    (funcall setfun 'bibtex-any-valid-entry-type
             (concat "^[ \t]*@[ \t]*\\(?:"
                     (regexp-opt
                      (append '("String" "Preamble")
                              (mapcar 'car bibtex-entry-alist))) "\\)"))))

;; Entry commands and menus for BibTeX dialects
;; We do not use `easy-menu-define' here because this gets confused
;; if we want to have multiple versions of the "same" menu.
(let ((select-map (make-sparse-keymap)))
  ;; Submenu for selecting the dialect
  (dolist (dialect (reverse bibtex-dialect-list))
    (define-key select-map (vector dialect)
      `(menu-item ,(symbol-name dialect)
                  (lambda () (interactive) (bibtex-set-dialect ',dialect t))
                  :button (:radio . (eq bibtex-dialect ',dialect)))))
  ;; We define a menu for each dialect.
  ;; Then we select the menu we want via the :visible keyword
  (dolist (dialect bibtex-dialect-list)
    (let ((entry-alist (bibtex-entry-alist dialect))
          (menu-map (make-sparse-keymap)))
      (define-key menu-map [select]
        `(menu-item "BibTeX dialect" ,select-map))
      (define-key menu-map [nil-2] '(menu-item "--"))
      (define-key menu-map [bibtex-preamble]
        '(menu-item "Preamble" bibtex-Preamble))
      (define-key menu-map [bibtex-String]
        '(menu-item "String" bibtex-String))
      (define-key menu-map [nil-1] '(menu-item "--"))
      (dolist (elt (reverse entry-alist))
        ;; Entry commands
        (let* ((entry (car elt))
               (fname (intern (format "bibtex-%s" entry))))
          (unless (fboundp fname)
            (eval (list 'defun fname nil
                        (format "Insert a template for a @%s entry; see also `bibtex-entry'."
                                entry)
                        '(interactive "*")
                        `(bibtex-entry ,entry))))
          ;; Menu entries
          (define-key menu-map (vector fname)
            `(menu-item ,(or (nth 1 elt) (car elt)) ,fname))))
      (define-key bibtex-mode-map
        (vector 'menu-bar dialect)
        `(menu-item "Entry-Types" ,menu-map
                    :visible (eq bibtex-dialect ',dialect))))))

(defun bibtex-field-list (entry-type)
  "Return list of allowed fields for entry ENTRY-TYPE.
More specifically, the return value is a cons pair (REQUIRED . OPTIONAL),
where REQUIRED and OPTIONAL are lists of the required and optional field
names for ENTRY-TYPE according to `bibtex-BibTeX-entry-alist' and friends,
`bibtex-include-OPTkey', `bibtex-include-OPTcrossref',
and `bibtex-user-optional-fields'."
  (let ((e-list (assoc-string entry-type bibtex-entry-alist t))
        required optional)
    (unless e-list
      (error "Fields for BibTeX entry type %s not defined" entry-type))
    (if (member-ignore-case entry-type bibtex-include-OPTcrossref)
        (setq required (nth 2 e-list)
              optional (append (nth 3 e-list) (nth 4 e-list)))
      (setq required  (append (nth 2 e-list) (nth 3 e-list))
            optional (nth 4 e-list)))
    (if bibtex-include-OPTkey
        (push (list "key"
                    "Used for reference key creation if author and editor fields are missing"
                    (if (or (stringp bibtex-include-OPTkey)
                            (functionp bibtex-include-OPTkey))
                        bibtex-include-OPTkey))
              optional))
    (if (member-ignore-case entry-type bibtex-include-OPTcrossref)
        (push '("crossref" "Reference key of the cross-referenced entry")
              optional))
    (setq optional (append optional bibtex-user-optional-fields))
    (cons required optional)))

(defun bibtex-entry (entry-type)
  "Insert a template for a BibTeX entry of type ENTRY-TYPE.
After insertion call the value of `bibtex-add-entry-hook' if that value
is non-nil."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read "Entry Type: " bibtex-entry-alist
                            nil t nil 'bibtex-entry-type-history))))
  (let ((key (if bibtex-maintain-sorted-entries
                 (bibtex-read-key (format "%s key: " entry-type))))
        (field-list (bibtex-field-list entry-type)))
    (unless (bibtex-prepare-new-entry (list key nil entry-type))
      (error "Entry with key `%s' already exists" key))
    (indent-to-column bibtex-entry-offset)
    (insert "@" entry-type (bibtex-entry-left-delimiter))
    (if key (insert key))
    (save-excursion
      (mapc 'bibtex-make-field (car field-list))
      (mapc 'bibtex-make-optional-field (cdr field-list))
      (if bibtex-comma-after-last-field
          (insert ","))
      (insert "\n")
      (indent-to-column bibtex-entry-offset)
      (insert (bibtex-entry-right-delimiter) "\n\n"))
    (bibtex-next-field t)
    (if (member-ignore-case entry-type bibtex-autofill-types)
	(bibtex-autofill-entry))
    (run-hooks 'bibtex-add-entry-hook)))

(defun bibtex-entry-update (&optional entry-type)
  "Update an existing BibTeX entry.
In the BibTeX entry at point, make new fields for those items that may occur
according to `bibtex-field-list', but are not yet present.
Also, add field delimiters to numerical fields if they are not present.
If ENTRY-TYPE is non-nil, change first the entry type to ENTRY-TYPE.
When called interactively with a prefix arg, query for a value of ENTRY-TYPE."
  (interactive
   (list (if current-prefix-arg
             (let ((completion-ignore-case t))
               (completing-read "New entry type: " bibtex-entry-alist
                                nil t nil 'bibtex-entry-type-history)))))
  (save-excursion
    (bibtex-beginning-of-entry)
    (when (looking-at bibtex-entry-maybe-empty-head)
      (goto-char (match-end 0))
      (if entry-type
          (save-excursion
            (replace-match (concat "@" entry-type) nil nil nil 1))
        (setq entry-type (bibtex-type-in-head)))
      (let* ((field-list (bibtex-field-list entry-type))
             (required (copy-tree (car field-list)))
             (optional (copy-tree (cdr field-list)))
             bounds)
        (while (setq bounds (bibtex-parse-field))
          (let ((fname (bibtex-name-in-field bounds t))
                (end (copy-marker (bibtex-end-of-field bounds) t)))
            (setq required (delete (assoc-string fname required t) required)
                  optional (delete (assoc-string fname optional t) optional))
            (when (string-match "\\`[0-9]+\\'"
                                (bibtex-text-in-field-bounds bounds))
              (goto-char (bibtex-end-of-text-in-field bounds))
              (insert (bibtex-field-right-delimiter))
              (goto-char (bibtex-start-of-text-in-field bounds))
              (insert (bibtex-field-left-delimiter)))
            (goto-char end)))
        (skip-chars-backward " \t\n")
        (mapc 'bibtex-make-field required)
        (mapc 'bibtex-make-optional-field optional)))))

(defun bibtex-parse-entry (&optional content)
  "Parse entry at point, return an alist.
The alist elements have the form (FIELD . TEXT), where FIELD can also be
the special strings \"=type=\" and \"=key=\".  For the FIELD \"=key=\"
TEXT may be nil.  Remove \"OPT\" and \"ALT\" from FIELD.
Move point to the end of the last field.
If optional arg CONTENT is non-nil extract content of text fields."
  (let (alist bounds)
    (when (looking-at bibtex-entry-maybe-empty-head)
      (push (cons "=type=" (bibtex-type-in-head)) alist)
      (push (cons "=key=" (bibtex-key-in-head)) alist)
      (goto-char (match-end 0))
      (while (setq bounds (bibtex-parse-field))
	(push (cons (bibtex-name-in-field bounds t)
		    (bibtex-text-in-field-bounds bounds content))
	      alist)
	(goto-char (bibtex-end-of-field bounds))))
    (nreverse alist)))

(defun bibtex-autofill-entry ()
  "Try to fill fields of current BibTeX entry based on neighboring entries.
The current entry must have a key.  Determine the neighboring entry
\(previous or next\) whose key is more similar to the key of the current
entry.  For all empty fields of the current entry insert the corresponding
field contents of the neighboring entry.  Finally try to update the text
based on the difference between the keys of the neighboring and the current
entry (for example, the year parts of the keys)."
  (interactive)
  (bibtex-beginning-of-entry)
  (when (looking-at bibtex-entry-head)
    (let ((type (bibtex-type-in-head))
	  (key (bibtex-key-in-head))
	  (key-end (match-end bibtex-key-in-head))
          (case-fold-search t)
          (bibtex-sort-ignore-string-entries t)
	  tmp other-key other bounds)
      ;; The fields we want to change start right after the key.
      (goto-char key-end)
      ;; First see whether to use the previous or the next entry
      ;; for "inspiration".
      (save-excursion
	(goto-char (1- (match-beginning 0)))
	(bibtex-beginning-of-entry)
	(if (and (looking-at bibtex-entry-head)
                 (bibtex-string= type (bibtex-type-in-head))
                 ;; In case we found ourselves :-(
                 (not (equal key (setq tmp (bibtex-key-in-head)))))
	  (setq other-key tmp
                other (point))))
      (save-excursion
	(bibtex-end-of-entry)
	(bibtex-skip-to-valid-entry)
	(if (and (looking-at bibtex-entry-head)
                 (bibtex-string= type (bibtex-type-in-head))
                 ;; In case we found ourselves :-(
                 (not (equal key (setq tmp (bibtex-key-in-head))))
                 (or (not other-key)
                     ;; Check which is the best match.
                     (< (length (try-completion "" (list key other-key)))
                        (length (try-completion "" (list key tmp))))))
            (setq other-key tmp
                  other (point))))
      ;; Then fill the new entry's fields with the chosen other entry.
      (when other
	(setq other (save-excursion (goto-char other) (bibtex-parse-entry)))
	(setq key-end (point))	    ;In case parse-entry changed the buffer.
	(while (setq bounds (bibtex-parse-field))
	  (let ((text (assoc-string (bibtex-name-in-field bounds t)
                                    other t)))
	    (if (not (and text
                          (equal "" (bibtex-text-in-field-bounds bounds t))))
		(goto-char (bibtex-end-of-field bounds))
              (goto-char (bibtex-start-of-text-in-field bounds))
	      (delete-region (point) (bibtex-end-of-text-in-field bounds))
	      (insert (cdr text)))))
	;; Finally try to update the text based on the difference between
	;; the two keys.
	(let* ((prefix (try-completion "" (list key other-key)))
	       ;; If the keys are foo91 and foo92, don't replace 1 for 2
	       ;; but 91 for 92 instead.
	       (_ (if (string-match "[0-9]+\\'" prefix)
		      (setq prefix (substring prefix 0 (match-beginning 0)))))
	       (suffix (substring key (length prefix)))
	       (other-suffix (substring other-key (length prefix))))
	  (while (re-search-backward (regexp-quote other-suffix) key-end 'move)
	    (replace-match suffix)))))))

(defun bibtex-print-help-message (&optional field comma)
  "Print helpful information about current FIELD in current BibTeX entry.
Optional arg COMMA is as in `bibtex-enclosing-field'.  It is t for
interactive calls."
  (interactive (list nil t))
  (unless field (setq field (car (bibtex-find-text-internal nil nil comma))))
  (if (string-match "@" field)
      (cond ((bibtex-string= field "@string")
             (message "String definition"))
            ((bibtex-string= field "@preamble")
             (message "Preamble definition"))
            (t (message "Entry key")))
    (let* ((case-fold-search t)
           (type (save-excursion
                   (bibtex-beginning-of-entry)
                   (looking-at bibtex-entry-maybe-empty-head)
                   (bibtex-type-in-head)))
           (field-list (bibtex-field-list type))
           (comment (assoc-string field (append (car field-list)
                                                (cdr field-list)) t)))
      (message "%s" (cond ((nth 1 comment) (nth 1 comment))
                          ((setq comment (assoc-string field bibtex-field-alist t))
                           (nth 1 comment))
                          (t "No comment available"))))))

(defun bibtex-make-field (field &optional move interactive nodelim)
  "Make a field named FIELD in current BibTeX entry.
FIELD is either a string or a list of the form
\(FIELD-NAME COMMENT-STRING INIT ALTERNATIVE-FLAG) as in
`bibtex-BibTeX-entry-alist' and friends.
If MOVE is non-nil, move point past the present field before making
the new field.  If INTERACTIVE is non-nil, move point to the end of
the new field.  Otherwise move point past the new field.
MOVE and INTERACTIVE are t when called interactively.
INIT is surrounded by field delimiters, unless NODELIM is non-nil."
  (interactive
   (list (let ((completion-ignore-case t)
               (field-list (bibtex-field-list
                            (save-excursion
                              (bibtex-beginning-of-entry)
                              (looking-at bibtex-any-entry-maybe-empty-head)
                              (bibtex-type-in-head)))))
           (completing-read "BibTeX field name: "
                            (append (car field-list) (cdr field-list))
                            nil nil nil bibtex-field-history))
         t t))
  (unless (consp field)
    (setq field (list field)))
  (when move
    (bibtex-find-text)
    (if (looking-at "[}\"]")
        (forward-char)))
  (insert ",\n")
  (indent-to-column (+ bibtex-entry-offset bibtex-field-indentation))
  ;; If there are multiple sets of alternatives, we could use
  ;; the numeric value of (nth 3 field) to number these sets.  Useful??
  (if (nth 3 field) (insert "ALT"))
  (insert (car field) " ")
  (if bibtex-align-at-equal-sign
      (indent-to-column (+ bibtex-entry-offset
                           (- bibtex-text-indentation 2))))
  (insert "= ")
  (unless bibtex-align-at-equal-sign
    (indent-to-column (+ bibtex-entry-offset
                         bibtex-text-indentation)))
  (let ((init (nth 2 field)))
    (if (not init) (setq init "")
      (if (functionp init) (setq init (funcall init)))
      (unless (stringp init) (error "`%s' is not a string" init)))
    ;; NODELIM is required by `bibtex-insert-kill'
    (if nodelim (insert init)
      (insert (bibtex-field-left-delimiter) init
              (bibtex-field-right-delimiter))))
  (when interactive
    ;; (bibtex-find-text nil nil bibtex-help-message)
    (if (memq (preceding-char) '(?} ?\")) (forward-char -1))
    (if bibtex-help-message (bibtex-print-help-message (car field)))))

(defun bibtex-beginning-of-entry ()
  "Move to beginning of BibTeX entry (beginning of line).
If inside an entry, move to the beginning of it, otherwise move to the
beginning of the previous entry.  If point is ahead of all BibTeX entries
move point to the beginning of buffer.  Return the new location of point."
  (interactive)
  (skip-chars-forward " \t")
  (if (looking-at "@")
      (forward-char))
  (re-search-backward "^[ \t]*@" nil 'move)
  (point))

(defun bibtex-end-of-entry ()
  "Move to end of BibTeX entry (past the closing brace).
If inside an entry, move to the end of it, otherwise move to the end
of the previous entry.  Do not move if ahead of first entry.
Return the new location of point."
  (interactive)
  (let ((case-fold-search t)
        (pnt (point))
        (_ (bibtex-beginning-of-entry))
        (bounds (bibtex-valid-entry t)))
    (cond (bounds (goto-char (cdr bounds))) ; regular entry
          ;; @String or @Preamble
          ((setq bounds (or (bibtex-parse-string t) (bibtex-parse-preamble)))
           (goto-char (bibtex-end-of-string bounds)))
          ((looking-at bibtex-any-valid-entry-type)
           ;; Parsing of entry failed
           (error "Syntactically incorrect BibTeX entry starts here"))
          (t (if (called-interactively-p 'interactive)
		 (message "Not on a known BibTeX entry."))
             (goto-char pnt)))
    (point)))

(defun bibtex-goto-line (arg)
  "Goto line ARG, counting from beginning of (narrowed) buffer."
  ;; code adapted from `goto-line'
  (goto-char (point-min))
  (if (eq selective-display t)
      (re-search-forward "[\n\C-m]" nil 'end (1- arg))
    (forward-line (1- arg))))

(defun bibtex-reposition-window ()
  "Make the current BibTeX entry visible.
If entry is smaller than `window-body-height', entry is centered in window.
Otherwise display the beginning of entry."
  (interactive)
  (let ((pnt (point))
        (beg (line-number-at-pos (bibtex-beginning-of-entry)))
        (end (line-number-at-pos (bibtex-end-of-entry))))
    (if (> (window-body-height) (- end beg))
        ;; entry fits in current window
        (progn
          (bibtex-goto-line (/ (+ 1 beg end) 2))
          (recenter)
          (goto-char pnt))
      ;; entry too large for current window
      (bibtex-goto-line beg)
      (recenter 0)
      (if (> (1+ (- (line-number-at-pos pnt) beg))
             (window-body-height))
          (bibtex-goto-line beg)
        (goto-char pnt)))))

(defun bibtex-mark-entry ()
  "Put mark at beginning, point at end of current BibTeX entry."
  (interactive)
  (push-mark (bibtex-beginning-of-entry))
  (bibtex-end-of-entry))

(defun bibtex-count-entries (&optional count-string-entries)
  "Count number of entries in current buffer or region.
With prefix argument COUNT-STRING-ENTRIES count all entries,
otherwise count all entries except @String entries.
If mark is active count entries in region, if not in whole buffer."
  (interactive "P")
  (let ((number 0)
        (bibtex-sort-ignore-string-entries (not count-string-entries)))
    (save-restriction
      (if mark-active (narrow-to-region (region-beginning) (region-end)))
      (bibtex-map-entries (lambda (_key _beg _end) (setq number (1+ number)))))
    (message "%s contains %d entries."
             (if mark-active "Region" "Buffer")
             number)))

(defun bibtex-ispell-entry ()
  "Check BibTeX entry for spelling errors."
  (interactive)
  (ispell-region (save-excursion (bibtex-beginning-of-entry))
                 (save-excursion (bibtex-end-of-entry))))

(defun bibtex-ispell-abstract ()
  "Check abstract of BibTeX entry for spelling errors."
  (interactive)
  (let ((bounds (save-excursion
                  (bibtex-beginning-of-entry)
                  (bibtex-search-forward-field "abstract" t))))
    (if bounds
        (ispell-region (bibtex-start-of-text-in-field bounds)
                       (bibtex-end-of-text-in-field bounds))
      (error "No abstract in entry"))))

(defun bibtex-narrow-to-entry ()
  "Narrow buffer to current BibTeX entry."
  (interactive)
  (save-excursion
    (widen)
    (narrow-to-region (bibtex-beginning-of-entry)
                      (bibtex-end-of-entry))))

(defun bibtex-entry-index ()
  "Return index of BibTeX entry head at or past position of point.
The index is a list (KEY CROSSREF-KEY ENTRY-TYPE) that is used for sorting
the entries of the BibTeX buffer.  CROSSREF-KEY is nil unless the value
of `bibtex-maintain-sorted-entries' is `crossref'.  Move point to the end
of the head of the entry found.  Return nil if no entry found."
  (let ((case-fold-search t))
    (if (re-search-forward bibtex-entry-maybe-empty-head nil t)
        (let ((key (bibtex-key-in-head))
              ;; all entry types should be downcase (for ease of comparison)
              (entry-type (downcase (bibtex-type-in-head))))
          ;; Don't search CROSSREF-KEY if we don't need it.
          (if (eq bibtex-maintain-sorted-entries 'crossref)
              (let ((bounds (bibtex-search-forward-field
                             "\\(OPT\\)?crossref" t)))
                (list key
                      (if bounds (bibtex-text-in-field-bounds bounds t))
                      entry-type))
            (list key nil entry-type))))))

(defun bibtex-init-sort-entry-class-alist ()
  "Initialize `bibtex-sort-entry-class-alist' (buffer-local)."
  (unless (local-variable-p 'bibtex-sort-entry-class-alist)
    (set (make-local-variable 'bibtex-sort-entry-class-alist)
         (let ((i -1) alist)
           (dolist (class bibtex-sort-entry-class)
             (setq i (1+ i))
             (dolist (entry class)
               ;; All entry types should be downcase (for ease of comparison).
               (push (cons (if (stringp entry) (downcase entry) entry) i)
                     alist)))
           alist))))

(defun bibtex-lessp (index1 index2)
  "Predicate for sorting BibTeX entries with indices INDEX1 and INDEX2.
Each index is a list (KEY CROSSREF-KEY ENTRY-TYPE).
The predicate depends on the variable `bibtex-maintain-sorted-entries'.
If its value is nil use plain sorting."
  (cond ((not index1) (not index2)) ; indices can be nil
        ((not index2) nil)
        ((eq bibtex-maintain-sorted-entries 'crossref)
         ;; CROSSREF-KEY may be nil or it can point to an entry
         ;; in another BibTeX file.  In both cases we ignore CROSSREF-KEY.
         (if (and (nth 1 index1)
                  (cdr (assoc-string (nth 1 index1) bibtex-reference-keys)))
             (if (and (nth 1 index2)
                      (cdr (assoc-string (nth 1 index2) bibtex-reference-keys)))
                 (or (string-lessp (nth 1 index1) (nth 1 index2))
                     (and (string-equal (nth 1 index1) (nth 1 index2))
                          (string-lessp (nth 0 index1) (nth 0 index2))))
               (not (string-lessp (nth 0 index2) (nth 1 index1))))
           (if (and (nth 1 index2)
                    (cdr (assoc-string (nth 1 index2) bibtex-reference-keys)))
               (string-lessp (nth 0 index1) (nth 1 index2))
             (string-lessp (nth 0 index1) (nth 0 index2)))))
        ((eq bibtex-maintain-sorted-entries 'entry-class)
         (let ((n1 (cdr (or (assoc (nth 2 index1) bibtex-sort-entry-class-alist)
                            (assoc 'catch-all bibtex-sort-entry-class-alist)
                            '(nil . 1000))))  ; if there is nothing else
               (n2 (cdr (or (assoc (nth 2 index2) bibtex-sort-entry-class-alist)
                            (assoc 'catch-all bibtex-sort-entry-class-alist)
                            '(nil . 1000))))) ; if there is nothing else
           (or (< n1 n2)
               (and (= n1 n2)
                    (string-lessp (car index1) (car index2))))))
        (t ; (eq bibtex-maintain-sorted-entries 'plain)
         (string-lessp (car index1) (car index2)))))

(defun bibtex-sort-buffer ()
  "Sort BibTeX buffer alphabetically by key.
The predicate for sorting is defined via `bibtex-maintain-sorted-entries'.
If its value is nil use plain sorting.  Text outside of BibTeX entries is not
affected.  If `bibtex-sort-ignore-string-entries' is non-nil, @String entries
are ignored."
  (interactive)
  (bibtex-beginning-of-first-entry)     ; Needed by `sort-subr'
  (bibtex-init-sort-entry-class-alist)  ; Needed by `bibtex-lessp'.
  (if (and (eq bibtex-maintain-sorted-entries 'crossref)
           (functionp bibtex-reference-keys))
      (bibtex-parse-keys))              ; Needed by `bibtex-lessp'.
  (sort-subr nil
             'bibtex-skip-to-valid-entry   ; NEXTREC function
             'bibtex-end-of-entry          ; ENDREC function
             'bibtex-entry-index           ; STARTKEY function
             nil                           ; ENDKEY function
             'bibtex-lessp))               ; PREDICATE

(defun bibtex-search-crossref (crossref-key &optional pnt split noerror)
  "Move point to the beginning of BibTeX entry CROSSREF-KEY.
If `bibtex-files' is non-nil, search all these files.
Otherwise the search is limited to the current buffer.
Return position of entry if CROSSREF-KEY is found or nil otherwise.
If CROSSREF-KEY is in the same buffer like current entry but before it
an error is signaled.  If NOERROR is non-nil this error is suppressed.
Optional arg PNT is the position of the referencing entry.  It defaults
to position of point.  If optional arg SPLIT is non-nil, split window
so that both the referencing and the crossrefed entry are displayed.

If called interactively, CROSSREF-KEY defaults to either the crossref key
of current entry or a key matched by `bibtex-cite-matcher-alist',
whatever is nearer to the position of point.  SPLIT is t.  NOERROR is nil
for a crossref key, t otherwise."
  (interactive
   (save-excursion
     (let* ((pnt (point))
            (_ (bibtex-beginning-of-entry))
            (end (cdr (bibtex-valid-entry t)))
            (_ (unless end (error "Not inside valid entry")))
            (beg (match-end 0)) ; set by `bibtex-valid-entry'
            (bounds (bibtex-search-forward-field "\\(OPT\\)?crossref" end))
            case-fold-search best temp crossref-key)
       (if bounds
           (setq crossref-key (bibtex-text-in-field-bounds bounds t)
                 best (cons (bibtex-dist pnt (bibtex-end-of-field bounds)
                                         (bibtex-start-of-field bounds))
                            crossref-key)))
       (dolist (matcher bibtex-cite-matcher-alist)
         (goto-char beg)
         (while (re-search-forward (car matcher) end t)
           (setq temp (bibtex-dist pnt (match-end (cdr matcher))
                                   (match-beginning (cdr matcher))))
           ;; Accept the key closest to the position of point.
           (if (or (not best) (< temp (car best)))
               (setq best (cons temp (match-string-no-properties
                                      (cdr matcher)))))))
       (goto-char pnt)
       (setq temp (bibtex-read-key "Find crossref key: " (cdr best) t))
       (list temp (point) t (not (and crossref-key
                                      (string= temp crossref-key)))))))

  (let (buffer pos eqb)
    (save-excursion
      (setq pos (bibtex-search-entry crossref-key t)
            buffer (current-buffer)))
    (setq eqb (eq buffer (current-buffer)))
    (cond ((not pos)
           (if split (message "Crossref key `%s' not found" crossref-key)))
          (split ; called (quasi) interactively
           (unless pnt (setq pnt (point)))
           (goto-char pnt)
           (if (and eqb (= pos (save-excursion (bibtex-beginning-of-entry))))
               (message "Key `%s' is current entry" crossref-key)
             (if eqb (select-window (split-window))
               (pop-to-buffer buffer))
             (goto-char pos)
             (bibtex-reposition-window)
             (beginning-of-line)
             (if (and eqb (> pnt pos) (not noerror))
                 (error "The referencing entry must precede the crossrefed entry!"))))
          ;; `bibtex-search-crossref' is called noninteractively during
          ;; clean-up of an entry.  Then it is not possible to check
          ;; whether the current entry and the crossrefed entry have
          ;; the correct sorting order.
          (eqb (goto-char pos))
          (t (set-buffer buffer) (goto-char pos)))
    pos))
;; backward compatibility
(defalias 'bibtex-find-crossref 'bibtex-search-crossref)

(defun bibtex-dist (pos beg end)
  "Return distance between POS and region delimited by BEG and END."
  (cond ((and (<= beg pos) (<= pos end)) 0)
        ((< pos beg) (- beg pos))
        (t (- pos end))))

;;;###autoload
(defun bibtex-search-entry (key &optional global start display)
  "Move point to the beginning of BibTeX entry named KEY.
Return position of entry if KEY is found or nil if not found.
With GLOBAL non-nil, search KEY in `bibtex-files'.  Otherwise the search
is limited to the current buffer.  Optional arg START is buffer position
where the search starts.  If it is nil, start search at beginning of buffer.
If DISPLAY is non-nil, display the buffer containing KEY.
Otherwise, use `set-buffer'.
When called interactively, START is nil, DISPLAY is t.
Also, GLOBAL is t if the current mode is not `bibtex-mode'
or `bibtex-search-entry-globally' is non-nil.
A prefix arg negates the value of `bibtex-search-entry-globally'."
  (interactive
   (let ((global (or (not (eq major-mode 'bibtex-mode))
                     (if bibtex-search-entry-globally
                         (not current-prefix-arg)
                       current-prefix-arg))))
     (list (bibtex-read-key "Find key: " nil global) global nil t)))
  (if (and global bibtex-files)
      (let ((buffer-list (bibtex-initialize t))
            buffer found)
        (while (and (not found)
                    (setq buffer (pop buffer-list)))
          (with-current-buffer buffer
            (if (cdr (assoc-string key bibtex-reference-keys))
                ;; `bibtex-search-entry' moves point if key found
                (setq found (bibtex-search-entry key)))))
        (cond ((and found display)
	       (switch-to-buffer buffer)
	       (bibtex-reposition-window))
              (found (set-buffer buffer))
              (display (message "Key `%s' not found" key)))
        found)

    (let* ((case-fold-search t)
           (pnt (save-excursion
                  (goto-char (or start (point-min)))
                  (if (re-search-forward (concat "^[ \t]*\\("
                                                 bibtex-entry-type
                                                 "\\)[ \t]*[({][ \t\n]*\\("
                                                 (regexp-quote key)
                                                 "\\)[ \t\n]*[,=]")
                                         nil t)
                      (match-beginning 0)))))
      (cond (pnt
             (goto-char pnt)
             (if display (bibtex-reposition-window)))
            (display (message "Key `%s' not found" key)))
      pnt)))
;; backward compatibility
(defalias 'bibtex-find-entry 'bibtex-search-entry)

(defun bibtex-prepare-new-entry (index)
  "Prepare a new BibTeX entry with index INDEX.
INDEX is a list (KEY CROSSREF-KEY ENTRY-TYPE).
Move point where the entry KEY should be placed.
If `bibtex-maintain-sorted-entries' is non-nil, perform a binary
search to look for place for KEY.  This requires that buffer is sorted,
see `bibtex-validate'.
Return t if preparation was successful or nil if entry KEY already exists."
  (bibtex-init-sort-entry-class-alist)  ; Needed by `bibtex-lessp'.
  (if (and (eq bibtex-maintain-sorted-entries 'crossref)
           (functionp bibtex-reference-keys))
      (bibtex-parse-keys))              ; Needed by `bibtex-lessp'.
  (let ((key (nth 0 index))
        key-exist)
    (cond ((or (null key)
               (and (stringp key)
                    (string-equal key ""))
               (and (not (setq key-exist (bibtex-search-entry key)))
                    (not bibtex-maintain-sorted-entries)))
           (bibtex-move-outside-of-entry))
          ;; if key-exist is non-nil due to the previous cond clause
          ;; then point will be at beginning of entry named key.
          (key-exist)
          (t             ; `bibtex-maintain-sorted-entries' is non-nil
           (let* ((case-fold-search t)
                  (left (save-excursion (bibtex-beginning-of-first-entry)))
                  (bounds (save-excursion (goto-char (point-max))
                                          (bibtex-skip-to-valid-entry t)))
                  (right (if bounds (cdr bounds) (point-min)))
                  (found (if (>= left right) left))
                  actual-index new)
             (save-excursion
               ;; Binary search
               (while (not found)
                 (goto-char (/ (+ left right) 2))
                 (bibtex-skip-to-valid-entry t)
                 (setq actual-index (bibtex-entry-index))
                 (cond ((bibtex-lessp index actual-index)
                        (setq new (bibtex-beginning-of-entry))
                        (if (equal right new)
                            (setq found right)
                          (setq right new)))
                       (t
                        (bibtex-end-of-entry)
                        (bibtex-skip-to-valid-entry)
                        (setq new (point))
                        (if (equal left new)
                            (setq found right)
                          (setq left new))))))
             (goto-char found)
             (bibtex-beginning-of-entry)
             (setq actual-index (save-excursion (bibtex-entry-index)))
             (when (or (not actual-index)
                       (bibtex-lessp actual-index index))
               ;; buffer contains no valid entries or
               ;; greater than last entry --> append
               (bibtex-end-of-entry)
               (unless (bobp) (newline (forward-line 2)))
               (beginning-of-line)))))
    (unless key-exist t)))

(defun bibtex-validate (&optional test-thoroughly)
  "Validate if buffer or region is syntactically correct.
Check also for duplicate keys and correct sort order provided
`bibtex-maintain-sorted-entries' is non-nil.
With optional argument TEST-THOROUGHLY non-nil check also for
the absence of required fields and for questionable month fields.
If mark is active, validate current region, if not the whole buffer.
Only check known entry types, so you can put comments outside of entries.
Return t if test was successful, nil otherwise."
  (interactive "P")
  (let* ((case-fold-search t)
         error-list syntax-error)
    (save-excursion
      (save-restriction
        (if mark-active (narrow-to-region (region-beginning) (region-end)))

        ;; Check syntactical structure of entries
        (goto-char (point-min))
        (bibtex-progress-message "Checking syntactical structure")
        (let (bounds end)
          (while (setq end (re-search-forward "^[ \t]*@" nil t))
            (bibtex-progress-message)
            (goto-char (match-beginning 0))
            (cond ((setq bounds (bibtex-valid-entry))
                   (goto-char (cdr bounds)))
                  ((setq bounds (or (bibtex-parse-string)
                                    (bibtex-parse-preamble)))
                   (goto-char (bibtex-end-of-string bounds)))
                  ((looking-at bibtex-any-valid-entry-type)
                   (push (cons (bibtex-current-line)
                               "Syntax error (check esp. commas, braces, and quotes)")
                         error-list)
                   (goto-char (match-end 0)))
                  (t (goto-char end)))))
        (bibtex-progress-message 'done)

        (if error-list
            ;; Continue only if there were no syntax errors.
            (setq syntax-error t)

          ;; Check for duplicate keys and correct sort order
          (bibtex-init-sort-entry-class-alist)  ; Needed by `bibtex-lessp'.
          (bibtex-parse-keys) ; Possibly needed by `bibtex-lessp'.
                              ; Always needed by subsequent global key check.
          (let (previous current key-list)
            (bibtex-progress-message "Checking for duplicate keys")
            (bibtex-map-entries
             (lambda (key _beg _end)
               (bibtex-progress-message)
               (setq current (bibtex-entry-index))
               (cond ((not previous))
                     ((member key key-list)
                      (push (cons (bibtex-current-line)
                                  (format "Duplicate key `%s'" key))
                            error-list))
                     ((and bibtex-maintain-sorted-entries
                           (not (bibtex-lessp previous current)))
                      (push (cons (bibtex-current-line)
                                  "Entries out of order")
                            error-list)))
               (push key key-list)
               (setq previous current)))
            (bibtex-progress-message 'done))

          ;; Check for duplicate keys in `bibtex-files'.
          ;; `bibtex-validate' only compares keys in current buffer with keys
          ;; in `bibtex-files'. `bibtex-validate-globally' compares keys for
          ;; each file in `bibtex-files' with keys of all other files in
          ;; `bibtex-files'.
          ;; We don't want to be fooled by outdated `bibtex-reference-keys'.
          (dolist (buffer (bibtex-initialize nil t))
            (dolist (key (with-current-buffer buffer bibtex-reference-keys))
              (when (and (cdr key)
                         (cdr (assoc-string (car key) bibtex-reference-keys)))
                (bibtex-search-entry (car key))
                (push (cons (bibtex-current-line)
                            (format "Duplicate key `%s' in %s" (car key)
                                    (abbreviate-file-name (buffer-file-name buffer))))
                      error-list))))

          (when test-thoroughly
            (bibtex-progress-message
             "Checking required fields and month fields")
            (let ((bibtex-sort-ignore-string-entries t))
              (bibtex-map-entries
               (lambda (_key beg end)
                 (bibtex-progress-message)
                 (bibtex-beginning-first-field beg)
                 (let* ((beg-line (save-excursion (goto-char beg)
                                                  (bibtex-current-line)))
                        (entry-list (assoc-string (bibtex-type-in-head)
                                                  bibtex-entry-alist t))
                        (crossref (bibtex-search-forward-field "crossref" end))
                        (req (if crossref (copy-sequence (nth 2 entry-list))
                               (append (nth 2 entry-list)
                                       (copy-sequence (nth 3 entry-list)))))
                        (num-alt (length (delq nil (delete-dups
                                                    (mapcar (lambda (x) (nth 3 x))
                                                            req)))))
                        (alt-fields (make-vector num-alt nil))
                        bounds field idx)
                   (while (setq bounds (bibtex-parse-field))
                     (let ((field-name (bibtex-name-in-field bounds)))
                       (if (and (bibtex-string= field-name "month")
                                ;; Check only abbreviated month fields.
                                (let ((month (bibtex-text-in-field-bounds bounds)))
                                  (not (or (string-match "\\`[\"{].+[\"}]\\'" month)
                                           (assoc-string
                                            month
                                            bibtex-predefined-month-strings t)))))
                           (push (cons (bibtex-current-line)
                                       "Questionable month field")
                                 error-list))
                       (setq field (assoc-string field-name req t)
                             req (delete field req))
                       (if (setq idx (nth 3 field))
                           (if (aref alt-fields idx)
                               (push (cons (bibtex-current-line)
                                           "More than one non-empty alternative")
                                     error-list)
                             (aset alt-fields idx t))))
                     (goto-char (bibtex-end-of-field bounds)))
                   (let ((alt-expect (make-vector num-alt nil)))
                     (dolist (field req) ; absent required fields
                       (if (setq idx (nth 3 field))
                           (bibtex-vec-push alt-expect idx (car field))
                         (push (cons beg-line
                                     (format "Required field `%s' missing"
                                             (car field)))
                               error-list)))
                     (dotimes (idx num-alt)
                       (unless (aref alt-fields idx)
                         (push (cons beg-line
                                     (format "Alternative fields `%s' missing"
                                             (aref alt-expect idx)))
                               error-list))))))))
            (bibtex-progress-message 'done)))))

    (if error-list
        (let ((file (file-name-nondirectory (buffer-file-name)))
              (dir default-directory)
              (err-buf "*BibTeX validation errors*"))
          (setq error-list (sort error-list 'car-less-than-car))
          (with-current-buffer (get-buffer-create err-buf)
            (setq default-directory dir)
            (unless (eq major-mode 'compilation-mode) (compilation-mode))
            (let ((inhibit-read-only t))
              (delete-region (point-min) (point-max))
              (insert "BibTeX mode command `bibtex-validate'\n"
                      (if syntax-error
                          "Maybe undetected errors due to syntax errors.  \
Correct and validate again.\n"
                        "\n"))
              (dolist (err error-list)
                (insert (format "%s:%d: %s\n" file (car err) (cdr err))))
              (set-buffer-modified-p nil))
            (goto-char (point-min))
            (forward-line 2)) ; first error message
          (display-buffer err-buf)
          nil) ; return `nil' (i.e., buffer is invalid)
      (message "%s is syntactically correct"
               (if mark-active "Region" "Buffer"))
      t))) ; return `t' (i.e., buffer is valid)

(defun bibtex-validate-globally (&optional strings)
  "Check for duplicate keys in `bibtex-files'.
With optional prefix arg STRINGS, check for duplicate strings, too.
Return t if test was successful, nil otherwise."
  (interactive "P")
  (let ((buffer-list (bibtex-initialize t))
        buffer-key-list current-buf current-keys error-list)
    ;; Check for duplicate keys within BibTeX buffer
    (dolist (buffer buffer-list)
      (with-current-buffer buffer
        (save-excursion
          (let (entry-type key key-list)
            (goto-char (point-min))
            (while (re-search-forward bibtex-entry-head nil t)
              (setq entry-type (bibtex-type-in-head)
                    key (bibtex-key-in-head))
              (if (or (and strings (bibtex-string= entry-type "string"))
                      (assoc-string entry-type bibtex-entry-alist t))
                  (if (member key key-list)
                      (push (format "%s:%d: Duplicate key `%s'\n"
                                    (buffer-file-name)
                                    (bibtex-current-line) key)
                            error-list)
                    (push key key-list))))
            (push (cons buffer key-list) buffer-key-list)))))

    ;; Check for duplicate keys among BibTeX buffers
    (while (setq current-buf (pop buffer-list))
      (setq current-keys (cdr (assq current-buf buffer-key-list)))
      (with-current-buffer current-buf
        (dolist (buffer buffer-list)
          (dolist (key (cdr (assq buffer buffer-key-list)))
            (when (assoc-string key current-keys)
              (bibtex-search-entry key)
              (push (format "%s:%d: Duplicate key `%s' in %s\n"
                            (buffer-file-name) (bibtex-current-line) key
                            (abbreviate-file-name (buffer-file-name buffer)))
                    error-list))))))

    ;; Process error list
    (if error-list
        (let ((err-buf "*BibTeX validation errors*"))
          (with-current-buffer (get-buffer-create err-buf)
            (unless (eq major-mode 'compilation-mode) (compilation-mode))
            (let ((inhibit-read-only t))
              (delete-region (point-min) (point-max))
              (insert "BibTeX mode command `bibtex-validate-globally'\n\n")
              (dolist (err (sort error-list 'string-lessp)) (insert err))
              (set-buffer-modified-p nil))
            (goto-char (point-min))
            (forward-line 2)) ; first error message
          (display-buffer err-buf)
          nil) ; return `nil' (i.e., buffer is invalid)
      (message "No duplicate keys.")
      t))) ; return `t' (i.e., buffer is valid)

(defun bibtex-next-field (begin &optional comma)
  "Move point to end of text of next BibTeX field or entry head.
With prefix BEGIN non-nil, move point to its beginning.  Optional arg COMMA
is as in `bibtex-enclosing-field'.  It is t for interactive calls."
  (interactive (list current-prefix-arg t))
  (let ((bounds (bibtex-find-text-internal t nil comma))
        end-of-entry)
    (if (not bounds)
        (setq end-of-entry t)
      (goto-char (nth 3 bounds))
      (if (assoc-string (car bounds) '("@String" "@Preamble") t)
          (setq end-of-entry t)
        ;; BibTeX key or field
        (if (looking-at ",[ \t\n]*") (goto-char (match-end 0)))
        ;; end of entry
        (if (looking-at "[)}][ \t\n]*") (setq end-of-entry t))))
    (if (and end-of-entry
             (re-search-forward bibtex-any-entry-maybe-empty-head nil t))
      (goto-char (match-beginning 0)))
    (bibtex-find-text begin nil bibtex-help-message)))

(defun bibtex-find-text (&optional begin noerror help comma)
  "Move point to end of text of current BibTeX field or entry head.
With optional prefix BEGIN non-nil, move point to its beginning.
Unless NOERROR is non-nil, an error is signaled if point is not
on a BibTeX field.  If optional arg HELP is non-nil print help message.
When called interactively, the value of HELP is `bibtex-help-message'.
Optional arg COMMA is as in `bibtex-enclosing-field'.  It is t for
interactive calls."
  (interactive (list current-prefix-arg nil bibtex-help-message t))
  (let ((bounds (bibtex-find-text-internal t nil comma)))
    (cond (bounds
           (if begin
               (progn (goto-char (nth 1 bounds))
                      (if (looking-at "[{\"]")
                          (forward-char)))
             (goto-char (nth 2 bounds))
             (if (memq (preceding-char) '(?} ?\"))
                 (forward-char -1)))
           (if help (bibtex-print-help-message (car bounds))))
          ((not noerror) (error "Not on BibTeX field")))))

(defun bibtex-find-text-internal (&optional noerror subfield comma)
  "Find text part of current BibTeX field or entry head.
Return list (NAME START-TEXT END-TEXT END STRING-CONST) with field name
or entry type, start and end of text, and end of field or entry head.
STRING-CONST is a flag which is non-nil if current subfield delimited by #
is a BibTeX string constant.  Return value is nil if field or entry head
are not found.
If optional arg NOERROR is non-nil, an error message is suppressed
if text is not found.  If optional arg SUBFIELD is non-nil START-TEXT
and END-TEXT correspond to the current subfield delimited by #.
Optional arg COMMA is as in `bibtex-enclosing-field'."
  (save-excursion
    (let ((pnt (point))
          (bounds (bibtex-enclosing-field comma t))
          (case-fold-search t)
          name start-text end-text end failure done no-sub string-const)
      (bibtex-beginning-of-entry)
      (cond (bounds
             (setq name (bibtex-name-in-field bounds t)
                   start-text (bibtex-start-of-text-in-field bounds)
                   end-text (bibtex-end-of-text-in-field bounds)
                   end (bibtex-end-of-field bounds)))
            ;; @String
            ((setq bounds (bibtex-parse-string t))
             (if (<= pnt (bibtex-end-of-string bounds))
                 (setq name "@String" ;; not a field name!
                       start-text (bibtex-start-of-text-in-string bounds)
                       end-text (bibtex-end-of-text-in-string bounds)
                       end (bibtex-end-of-string bounds))
               (setq failure t)))
            ;; @Preamble
            ((setq bounds (bibtex-parse-preamble))
             (if (<= pnt (bibtex-end-of-string bounds))
                 (setq name "@Preamble" ;; not a field name!
                       start-text (bibtex-start-of-text-in-string bounds)
                       end-text (bibtex-end-of-text-in-string bounds)
                       end (bibtex-end-of-string bounds))
               (setq failure t)))
            ;; BibTeX head
            ((looking-at bibtex-entry-maybe-empty-head)
             (goto-char (match-end 0))
             (if comma (save-match-data
                         (re-search-forward "\\=[ \t\n]*," nil t)))
             (if (<= pnt (point))
                 (setq name (match-string-no-properties bibtex-type-in-head)
                       start-text (or (match-beginning bibtex-key-in-head)
                                 (match-end 0))
                       end-text (or (match-end bibtex-key-in-head)
                               (match-end 0))
                       end end-text
                       no-sub t) ; subfields do not make sense
               (setq failure t)))
            (t (setq failure t)))
      (when (and subfield (not failure))
        (setq failure no-sub)
        (unless failure
          (goto-char start-text)
          (while (not done)
            (if (or (prog1 (looking-at bibtex-field-const)
                      (setq end-text (match-end 0)
                            string-const t))
                    (prog1 (setq bounds (bibtex-parse-field-string))
                      (setq end-text (cdr bounds)
                            string-const nil)))
                (progn
                  (if (and (<= start-text pnt) (<= pnt end-text))
                      (setq done t)
                    (goto-char end-text))
                  (if (looking-at "[ \t\n]*#[ \t\n]*")
                      (setq start-text (goto-char (match-end 0)))))
              (setq done t failure t)))))
      (cond ((not failure)
             (list name start-text end-text end string-const))
            ((and no-sub (not noerror))
             (error "Not on text part of BibTeX field"))
            ((not noerror) (error "Not on BibTeX field"))))))

(defun bibtex-remove-OPT-or-ALT (&optional comma)
  "Remove the string starting optional/alternative fields.
Align text and go thereafter to end of text.  Optional arg COMMA
is as in `bibtex-enclosing-field'.  It is t for interactive calls."
  (interactive (list t))
  (let ((case-fold-search t)
        (bounds (bibtex-enclosing-field comma)))
    (save-excursion
      (goto-char (bibtex-start-of-name-in-field bounds))
      (when (and (looking-at "OPT\\|ALT")
                 (not (and bibtex-no-opt-remove-re
                           (string-match
                            bibtex-no-opt-remove-re
                            (buffer-substring-no-properties
                             (bibtex-start-of-name-in-field bounds)
                             (bibtex-end-of-name-in-field bounds))))))
        (delete-region (match-beginning 0) (match-end 0))
        ;; make field non-OPT
        (search-forward "=")
        (forward-char -1)
        (delete-horizontal-space)
        (if bibtex-align-at-equal-sign
            (indent-to-column (- bibtex-text-indentation 2))
          (insert " "))
        (search-forward "=")
        (delete-horizontal-space)
        (if bibtex-align-at-equal-sign
            (insert " ")
          (indent-to-column bibtex-text-indentation))))))

(defun bibtex-remove-delimiters (&optional comma)
  "Remove \"\" or {} around current BibTeX field text.
Optional arg COMMA is as in `bibtex-enclosing-field'.  It is t for
interactive calls."
  (interactive (list t))
  (let ((bounds (bibtex-find-text-internal nil t comma)))
    (unless (nth 4 bounds)
      (delete-region (1- (nth 2 bounds)) (nth 2 bounds))
      (delete-region (nth 1 bounds) (1+ (nth 1 bounds))))))

(defun bibtex-kill-field (&optional copy-only comma)
  "Kill the entire enclosing BibTeX field.
With prefix arg COPY-ONLY, copy the current field to `bibtex-field-kill-ring',
but do not actually kill it.  Optional arg COMMA is as in
`bibtex-enclosing-field'.  It is t for interactive calls."
  (interactive (list current-prefix-arg t))
  (save-excursion
    (let* ((case-fold-search t)
           (bounds (bibtex-enclosing-field comma))
           (end (bibtex-end-of-field bounds))
           (beg (bibtex-start-of-field bounds)))
      (goto-char end)
      ;; Preserve white space at end of BibTeX entry
      (if (looking-at "[ \t\n]*[)}]")
          (progn (skip-chars-backward " \t\n")
                 (setq end (point)))
        (skip-chars-forward ","))
      (push (list (bibtex-name-in-field bounds) nil
                  (bibtex-text-in-field-bounds bounds))
            bibtex-field-kill-ring)
      (if (> (length bibtex-field-kill-ring) bibtex-field-kill-ring-max)
          (setcdr (nthcdr (1- bibtex-field-kill-ring-max)
                          bibtex-field-kill-ring)
                  nil))
      (setq bibtex-field-kill-ring-yank-pointer bibtex-field-kill-ring)
      (unless copy-only
        (delete-region beg end))))
  (setq bibtex-last-kill-command 'field))

(defun bibtex-copy-field-as-kill (&optional comma)
  "Copy the BibTeX field at point to `bibtex-field-kill-ring'.
Optional arg COMMA is as in `bibtex-enclosing-field'.  It is t for
interactive calls."
  (interactive (list t))
  (bibtex-kill-field t comma))

(defun bibtex-kill-entry (&optional copy-only)
  "Kill the entire enclosing BibTeX entry.
With prefix arg COPY-ONLY, copy the current entry to `bibtex-entry-kill-ring',
but do not actually kill it."
  (interactive "P")
  (save-excursion
    (let* ((case-fold-search t)
           (beg (bibtex-beginning-of-entry))
           (key (progn (looking-at bibtex-any-entry-maybe-empty-head)
                       (bibtex-key-in-head)))
           (end (progn (bibtex-end-of-entry)
                       (if (re-search-forward
                            bibtex-any-entry-maybe-empty-head nil 'move)
                           (goto-char (match-beginning 0)))
                       (point))))
      (push (buffer-substring-no-properties beg end)
            bibtex-entry-kill-ring)
      (if (> (length bibtex-entry-kill-ring) bibtex-entry-kill-ring-max)
          (setcdr (nthcdr (1- bibtex-entry-kill-ring-max)
                          bibtex-entry-kill-ring)
                  nil))
      (setq bibtex-entry-kill-ring-yank-pointer bibtex-entry-kill-ring)
      (unless copy-only
        (delete-region beg end)
        ;; remove key from `bibtex-reference-keys'.
        (unless (functionp bibtex-reference-keys)
          (setq bibtex-reference-keys
                (delete (cons key t) bibtex-reference-keys))))))
  (setq bibtex-last-kill-command 'entry))

(defun bibtex-copy-entry-as-kill ()
  "Copy the entire enclosing BibTeX entry to `bibtex-entry-kill-ring'."
  (interactive)
  (bibtex-kill-entry t))

(defun bibtex-yank (&optional n)
  "Reinsert the last BibTeX item.
More precisely, reinsert the field or entry killed or yanked most recently.
With argument N, reinsert the Nth most recently killed BibTeX item.
See also the command \\[bibtex-yank-pop]."
  (interactive "*p")
  (unless n (setq n 1))
  (bibtex-insert-kill (1- n) t)
  (setq this-command 'bibtex-yank))

(defun bibtex-yank-pop (n)
  "Replace just-yanked killed BibTeX item with a different item.
This command is allowed only immediately after a `bibtex-yank' or a
`bibtex-yank-pop'.  In this case, the region contains a reinserted
previously killed BibTeX item.  `bibtex-yank-pop' deletes that item
and inserts in its place a different killed BibTeX item.

With no argument, the previous kill is inserted.
With argument N, insert the Nth previous kill.
If N is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  (interactive "*p")
  (unless (eq last-command 'bibtex-yank)
    (error "Previous command was not a BibTeX yank"))
  (setq this-command 'bibtex-yank)
  (let ((inhibit-read-only t) key)
    ;; point is at end of yanked entry
    (unless (functionp bibtex-reference-keys)
      ;; remove key of yanked entry from `bibtex-reference-keys'
      (save-excursion
        (goto-char (mark t))
        (if (and (looking-at bibtex-any-entry-maybe-empty-head)
                 (setq key (bibtex-key-in-head)))
            (setq bibtex-reference-keys
                  (delete (cons key t) bibtex-reference-keys)))))
    (delete-region (point) (mark t))
    (bibtex-insert-kill n t)))

(defun bibtex-empty-field (&optional comma)
  "Delete the text part of the current field, replace with empty text.
Optional arg COMMA is as in `bibtex-enclosing-field'.  It is t for
interactive calls."
  (interactive (list t))
  (let ((bounds (bibtex-enclosing-field comma)))
    (goto-char (bibtex-start-of-text-in-field bounds))
    (delete-region (point) (bibtex-end-of-text-in-field bounds))
    (insert (bibtex-field-left-delimiter)
            (bibtex-field-right-delimiter))
    (bibtex-find-text t nil bibtex-help-message)))

(defun bibtex-pop-previous (arg)
  "Replace text of current field with the similar field in previous entry.
With arg, goes up ARG entries.  Repeated, goes up so many times.  May be
intermixed with \\[bibtex-pop-next] (bibtex-pop-next)."
  (interactive "p")
  (bibtex-pop arg 'previous))

(defun bibtex-pop-next (arg)
  "Replace text of current field with the text of similar field in next entry.
With arg, goes down ARG entries.  Repeated, goes down so many times.  May be
intermixed with \\[bibtex-pop-previous] (bibtex-pop-previous)."
  (interactive "p")
  (bibtex-pop arg 'next))

(defun bibtex-clean-entry (&optional new-key called-by-reformat)
  "Finish editing the current BibTeX entry and clean it up.
Check that no required fields are empty and format entry dependent
on the value of `bibtex-entry-format'.
If the reference key of the entry is empty or a prefix argument is given,
calculate a new reference key.  (Note: this works only if fields in entry
begin on separate lines prior to calling `bibtex-clean-entry' or if
'realign is contained in `bibtex-entry-format'.)
Don't call `bibtex-clean-entry' on @Preamble entries.
At end of the cleaning process, the functions in
`bibtex-clean-entry-hook' are called with region narrowed to entry."
  ;; Opt. arg CALLED-BY-REFORMAT is t if `bibtex-clean-entry'
  ;; is called by `bibtex-reformat'
  (interactive "P")
  (let ((case-fold-search t)
        (start (bibtex-beginning-of-entry))
        (_ (or (looking-at bibtex-any-entry-maybe-empty-head)
	       (error "Not inside a BibTeX entry")))
        (entry-type (bibtex-type-in-head))
        (key (bibtex-key-in-head)))
    (cond ((bibtex-string= entry-type "preamble")
           ;; (bibtex-format-preamble)
           (error "No clean up of @Preamble entries"))
          ((bibtex-string= entry-type "string")
           (setq entry-type 'string))
          ;; (bibtex-format-string)
          (t (bibtex-format-entry)))
    ;; set key
    (if (or new-key (not key))
        (save-excursion
          ;; First delete the old key so that a customized algorithm
          ;; for generating the new key does not get confused by the
          ;; old key.
          (re-search-forward (if (eq entry-type 'string)
                                 bibtex-string-maybe-empty-head
                               bibtex-entry-maybe-empty-head))
          (if (match-beginning bibtex-key-in-head)
              (delete-region (match-beginning bibtex-key-in-head)
                             (match-end bibtex-key-in-head)))
          (setq key (bibtex-generate-autokey))
          ;; Sometimes `bibtex-generate-autokey' returns an empty string
          (if (or bibtex-autokey-edit-before-use (string= "" key))
              (setq key (if (eq entry-type 'string)
                            (bibtex-read-string-key key)
                          (bibtex-read-key "Key to use: " key))))
          (insert key)))

    (unless called-by-reformat
      (let* ((end (save-excursion
                    (bibtex-end-of-entry)
                    (if (re-search-forward
                         bibtex-entry-maybe-empty-head nil 'move)
                        (goto-char (match-beginning 0)))
                    (point)))
             (entry (buffer-substring start end))
             ;; include the crossref key in index
             (index (let ((bibtex-maintain-sorted-entries 'crossref))
                      (bibtex-entry-index))) ; moves point to end of head
             error)
        ;; sorting
        (if (and bibtex-maintain-sorted-entries
                 (not (and bibtex-sort-ignore-string-entries
                           (eq entry-type 'string))))
            (progn
              (delete-region start end)
              (setq error (not (bibtex-prepare-new-entry index))
                    start (point)) ; update start
              (save-excursion (insert entry)))
          (bibtex-search-entry key)
          (setq error (or (/= (point) start)
                          (bibtex-search-entry key nil end))))
        (if error
            (error "New inserted entry yields duplicate key"))
        (dolist (buffer (bibtex-initialize))
          (with-current-buffer buffer
            (if (cdr (assoc-string key bibtex-reference-keys))
                (error "Duplicate key in %s" (buffer-file-name)))))

        ;; Only update `bibtex-strings' and `bibtex-reference-keys'
        ;; if they have been built already.
        (cond ((eq entry-type 'string)
               ;; We have a @String entry.
               (unless (or (functionp bibtex-strings)
                           (assoc key bibtex-strings))
                 (push (cons key (bibtex-text-in-string
                                  (bibtex-parse-string) t))
                       bibtex-strings)))
              ;; We have a normal entry.
              ((not (functionp bibtex-reference-keys))
               (let ((found (assoc key bibtex-reference-keys)))
                 (cond ((not found)
                        (push (cons key t) bibtex-reference-keys))
                       ((not (cdr found))
                        ;; Turn a crossref key into a header key
                        (setq bibtex-reference-keys
                              (cons (cons key t)
                                    (delete (list key) bibtex-reference-keys))))))
               ;; If entry has a crossref key, it goes into the list
               ;; `bibtex-reference-keys', too.
               (if (and (nth 1 index)
                        (not (assoc (nth 1 index) bibtex-reference-keys)))
                   (push (list (nth 1 index)) bibtex-reference-keys)))))

      ;; final clean up
      (if bibtex-clean-entry-hook
          (save-excursion
            (save-restriction
              (bibtex-narrow-to-entry)
              (run-hooks 'bibtex-clean-entry-hook)))))))

(defun bibtex-fill-field-bounds (bounds justify &optional move)
  "Fill BibTeX field delimited by BOUNDS.
If JUSTIFY is non-nil justify as well.
If optional arg MOVE is non-nil move point to end of field."
  (let ((end-field (copy-marker (bibtex-end-of-field bounds))))
    (if (not justify)
        (goto-char (bibtex-start-of-text-in-field bounds))
      (goto-char (bibtex-start-of-field bounds))
      (forward-char) ; leading comma
      (bibtex-delete-whitespace)
      (insert "\n")
      (indent-to-column (+ bibtex-entry-offset
                           bibtex-field-indentation))
      (re-search-forward "[ \t\n]*=" end-field)
      (replace-match "=")
      (forward-char -1)
      (if bibtex-align-at-equal-sign
          (indent-to-column
           (+ bibtex-entry-offset (- bibtex-text-indentation 2)))
        (insert " "))
      (forward-char)
      (bibtex-delete-whitespace)
      (if bibtex-align-at-equal-sign
          (insert " ")
        (indent-to-column bibtex-text-indentation)))
    ;; Paragraphs within fields are not preserved.  Bother?
    (fill-region-as-paragraph (line-beginning-position) end-field
                              default-justification nil (point))
    (if move (goto-char end-field))))

(defun bibtex-fill-field (&optional justify)
  "Like \\[fill-paragraph], but fill current BibTeX field.
If optional prefix JUSTIFY is non-nil justify as well.
In BibTeX mode this function is bound to `fill-paragraph-function'."
  (interactive "*P")
  (let ((pnt (copy-marker (point)))
        (bounds (bibtex-enclosing-field t)))
    (bibtex-fill-field-bounds bounds justify)
    (goto-char pnt)))

(defun bibtex-fill-entry ()
  "Fill current BibTeX entry.
Realign entry, so that every field starts on a separate line.  Field
names appear in column `bibtex-field-indentation', field text starts in
column `bibtex-text-indentation' and continuation lines start here, too.
If `bibtex-align-at-equal-sign' is non-nil, align equal signs, too."
  (interactive "*")
  (let ((pnt (copy-marker (point)))
        (beg (bibtex-beginning-of-entry)) ; move point
        bounds)
    (bibtex-delete-whitespace)
    (indent-to-column bibtex-entry-offset)
    (bibtex-beginning-first-field beg)
    (while (setq bounds (bibtex-parse-field))
      (bibtex-fill-field-bounds bounds t t))
    (if (looking-at ",")
        (forward-char))
    (skip-chars-backward " \t\n")
    (bibtex-delete-whitespace)
    (insert "\n")
    (indent-to-column bibtex-entry-offset)
    (goto-char pnt)))

(defun bibtex-realign ()
  "Realign BibTeX entries such that they are separated by one blank line."
  (goto-char (point-min))
  (let ((case-fold-search t)
        (entry-type (concat "[ \t\n]*\\(" bibtex-entry-type "\\)")))
    ;; No blank lines prior to the first entry if there no
    ;; non-white characters in front of it.
    (when (looking-at entry-type)
      (replace-match "\\1"))
    ;; Entries are separated by one blank line.
    (while (re-search-forward entry-type nil t)
      (replace-match "\n\n\\1"))
    ;; One blank line past the last entry if it is followed by
    ;; non-white characters, no blank line otherwise.
    (beginning-of-line)
    (when (re-search-forward bibtex-entry-type nil t)
      (bibtex-end-of-entry)
      (bibtex-delete-whitespace)
      (open-line (if (eobp) 1 2)))))

(defun bibtex-reformat (&optional read-options)
  "Reformat all BibTeX entries in buffer or region.
Without prefix argument, reformatting is based on `bibtex-entry-format'.
With prefix argument, read options for reformatting from minibuffer.
With \\[universal-argument] \\[universal-argument] prefix argument, reuse previous answers (if any) again.
If mark is active reformat entries in region, if not in whole buffer."
  (interactive "*P")
  (let* ((pnt (point))
         (use-previous-options
          (and (equal (prefix-numeric-value read-options) 16)
               (or bibtex-reformat-previous-options
                   bibtex-reformat-previous-reference-keys)))
         (bibtex-entry-format
          (cond (read-options
                 (if use-previous-options
                     bibtex-reformat-previous-options
                   (setq bibtex-reformat-previous-options
                         (mapcar (lambda (option)
                                   (if (y-or-n-p (car option)) (cdr option)))
                                 `(("Realign entries (recommended)? " . 'realign)
                                   ("Remove empty optional and alternative fields? " . 'opts-or-alts)
                                   ("Remove delimiters around pure numerical fields? " . 'numerical-fields)
                                   (,(concat (if bibtex-comma-after-last-field "Insert" "Remove")
                                             " comma at end of entry? ") . 'last-comma)
                                   ("Replace double page dashes by single ones? " . 'page-dashes)
                                   ("Delete whitespace at the beginning and end of fields? " . 'whitespace)
                                   ("Inherit booktitle? " . 'inherit-booktitle)
                                   ("Force delimiters? " . 'delimiters)
                                   ("Unify case of entry types and field names? " . 'unify-case)
                                   ("Enclose parts of field entries by braces? " . 'braces)
                                   ("Replace parts of field entries by string constants? " . 'strings)
                                   ("Sort fields? " . 'sort-fields))))))
                ;; Do not include required-fields because `bibtex-reformat'
                ;; cannot handle the error messages of `bibtex-format-entry'.
                ;; Use `bibtex-validate' to check for required fields.
                ((eq t bibtex-entry-format)
                 '(realign opts-or-alts numerical-fields delimiters
                           last-comma page-dashes unify-case inherit-booktitle
                           whitespace braces strings sort-fields))
                (t
                 (cons 'realign (remove 'required-fields bibtex-entry-format)))))
         (reformat-reference-keys
          (if read-options
              (if use-previous-options
                  bibtex-reformat-previous-reference-keys
                (setq bibtex-reformat-previous-reference-keys
                      (y-or-n-p "Generate new reference keys automatically? ")))))
         (bibtex-sort-ignore-string-entries t)
         bibtex-autokey-edit-before-use)

    (save-restriction
      (if mark-active (narrow-to-region (region-beginning) (region-end)))
      (if (memq 'realign bibtex-entry-format)
          (bibtex-realign))
      (bibtex-progress-message "Formatting" 1)
      (bibtex-map-entries (lambda (_key _beg _end)
                            (bibtex-progress-message)
                            (bibtex-clean-entry reformat-reference-keys t)))
      (bibtex-progress-message 'done))
    (when reformat-reference-keys
      (kill-local-variable 'bibtex-reference-keys)
      (when bibtex-maintain-sorted-entries
        (bibtex-progress-message "Sorting" 1)
        (bibtex-sort-buffer)
        (bibtex-progress-message 'done)))
    (goto-char pnt)))

(defun bibtex-convert-alien (&optional read-options)
  "Make an alien BibTeX buffer fully usable by BibTeX mode.
If a file does not conform with all standards used by BibTeX mode,
some of the high-level features of BibTeX mode are not available.
This function tries to convert current buffer to conform with these standards.
With prefix argument READ-OPTIONS non-nil, read options for reformatting
entries from minibuffer."
  (interactive "*P")
  (message "Starting to validate buffer...")
  (sit-for 1)
  (bibtex-realign)
  (deactivate-mark)  ; So `bibtex-validate' works on the whole buffer.
  (if (not (let (bibtex-maintain-sorted-entries)
             (bibtex-validate)))
      (message "Correct errors and call `bibtex-convert-alien' again")
    (message "Starting to reformat entries...")
    (sit-for 2)
    (bibtex-reformat read-options)
    (goto-char (point-max))
    (message "Buffer is now parsable.  Please save it.")))

(define-obsolete-function-alias 'bibtex-complete 'completion-at-point "24.1")
(defun bibtex-completion-at-point-function ()
  (let ((pnt (point))
        (case-fold-search t)
        (beg (save-excursion
               (re-search-backward "[ \t{\"]")
               (forward-char)
               (point)))
        (end (point))
        bounds name compl)
    (save-excursion
      (if (and (setq bounds (bibtex-enclosing-field nil t))
               (>= pnt (bibtex-start-of-text-in-field bounds))
               (<= pnt (bibtex-end-of-text-in-field bounds)))
          (setq name (bibtex-name-in-field bounds t)
                compl (cond ((bibtex-string= name "crossref")
                             ;; point is in crossref field
                             'crossref-key)
                            ((bibtex-string= name "month")
                             ;; point is in month field
                             bibtex-predefined-month-strings)
                            ;; point is in other field
                            (t (bibtex-strings))))
        (bibtex-beginning-of-entry)
        (cond ((setq bounds (bibtex-parse-string t))
               ;; point is inside a @String key
               (cond ((and (>= pnt (nth 1 (car bounds)))
                           (<= pnt (nth 2 (car bounds))))
                      (setq compl 'string))
                     ;; point is inside a @String field
                     ((and (>= pnt (bibtex-start-of-text-in-string bounds))
                           (<= pnt (bibtex-end-of-text-in-string bounds)))
                      (setq compl (bibtex-strings)))))
              ;; point is inside a @Preamble field
              ((setq bounds (bibtex-parse-preamble))
               (if (and (>= pnt (bibtex-start-of-text-in-string bounds))
                        (<= pnt (bibtex-end-of-text-in-string bounds)))
                   (setq compl (bibtex-strings))))
              ((and (looking-at bibtex-entry-maybe-empty-head)
                    ;; point is inside a key
                    (or (and (match-beginning bibtex-key-in-head)
                             (>= pnt (match-beginning bibtex-key-in-head))
                             (<= pnt (match-end bibtex-key-in-head)))
                        ;; or point is on empty key
                        (and (not (match-beginning bibtex-key-in-head))
                             (= pnt (match-end 0)))))
               (setq compl 'key)))))

    (cond ((eq compl 'key)
           ;; Key completion: no cleanup needed.
           (list beg end
                 (lambda (s p a)
                   (let (completion-ignore-case)
                     (complete-with-action a (bibtex-global-key-alist) s p)))))

          ((eq compl 'crossref-key)
           ;; Crossref key completion.
           (let* ((buf (current-buffer)))
             (list beg end
                   (lambda (s p a)
                     (cond
                      ((eq a 'metadata) `(metadata (category . bibtex-key)))
                      (t (let ((completion-ignore-case nil))
                           (complete-with-action
                            a (bibtex-global-key-alist) s p)))))
                   :exit-function (bibtex-complete-crossref-cleanup buf))))

          ((eq compl 'string)
           ;; String key completion: no cleanup needed.
           (list beg end
                 (lambda (s p a)
                   (let ((completion-ignore-case t))
                     (complete-with-action a bibtex-strings s p)))))

          (compl
           ;; String completion.
           (list beg end
                 (lambda (s p a)
                   (cond
                    ((eq a 'metadata) `(metadata (category . bibtex-string)))
                    (t (let ((completion-ignore-case t))
                         (complete-with-action a compl s p)))))
                 :exit-function (bibtex-complete-string-cleanup compl))))))

(defun bibtex-String (&optional key)
  "Insert a new BibTeX @String entry with key KEY."
  (interactive (list (bibtex-read-string-key)))
  (let ((bibtex-maintain-sorted-entries
         (unless bibtex-sort-ignore-string-entries
           bibtex-maintain-sorted-entries))
        endpos)
    (unless (bibtex-prepare-new-entry (list key nil "String"))
      (error "Entry with key `%s' already exists" key))
    (if (zerop (length key)) (setq key nil))
    (indent-to-column bibtex-entry-offset)
    (insert "@String"
            (bibtex-entry-left-delimiter))
    (if key
        (insert key)
      (setq endpos (point)))
    (insert " = "
            (bibtex-field-left-delimiter))
    (if key
        (setq endpos (point)))
    (insert (bibtex-field-right-delimiter)
            (bibtex-entry-right-delimiter)
            "\n")
    (goto-char endpos)))

(defun bibtex-Preamble ()
  "Insert a new BibTeX @Preamble entry."
  (interactive "*")
  (bibtex-move-outside-of-entry)
  (indent-to-column bibtex-entry-offset)
  (insert "@Preamble"
          (bibtex-entry-left-delimiter)
          (bibtex-field-left-delimiter))
  (let ((endpos (point)))
    (insert (bibtex-field-right-delimiter)
            (bibtex-entry-right-delimiter)
            "\n")
    (goto-char endpos)))

(defun bibtex-url (&optional pos no-browse)
  "Browse a URL for the BibTeX entry at point.
Optional POS is the location of the BibTeX entry.
The URL is generated using the schemes defined in `bibtex-generate-url-list'
\(see there\).  If multiple schemes match for this entry, or the same scheme
matches more than once, use the one for which the first step's match is the
closest to POS.  The URL is passed to `browse-url' unless NO-BROWSE is t.
Return the URL or nil if none can be generated."
  (interactive)
  (unless pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (bibtex-beginning-of-entry)
    (let ((end (save-excursion (bibtex-end-of-entry)))
          (fields-alist (save-excursion (bibtex-parse-entry t)))
          ;; Always ignore case,
          (case-fold-search t)
          text url scheme obj fmt fl-match)
      ;; The return value of `bibtex-parse-entry' (i.e., FIELDS-ALIST)
      ;; is always used to generate the URL.  However, if the BibTeX
      ;; entry contains more than one URL, we have multiple matches
      ;; for the first step defining the generation of the URL.
      ;; Therefore, we try to initiate the generation of the URL
      ;; based on the match of `bibtex-font-lock-url' that is the
      ;; closest to POS.  If that fails (no match found) we try to
      ;; initiate the generation of the URL based on the properly
      ;; concatenated CONTENT of the field as returned by
      ;; `bibtex-text-in-field-bounds'.  The latter approach can
      ;; differ from the former because `bibtex-font-lock-url' uses
      ;; the buffer itself.
      (while (bibtex-font-lock-url end t)
        (push (list (bibtex-dist pos (match-beginning 0) (match-end 0))
                    (match-beginning 0)
                    (buffer-substring-no-properties
                     (match-beginning 0) (match-end 0)))
              fl-match)
        ;; `bibtex-font-lock-url' moves point to end of match.
        (forward-char))
      (when fl-match
        (setq fl-match (car (sort fl-match (lambda (x y) (< (car x) (car y))))))
        (goto-char (nth 1 fl-match))
        (bibtex-beginning-of-field) (re-search-backward ",")
        (let* ((bounds (bibtex-parse-field))
               (name (bibtex-name-in-field bounds))
               (content (bibtex-text-in-field-bounds bounds t))
               (lst bibtex-generate-url-list))
          ;; This match can fail when CONTENT differs from text in buffer.
          (when (string-match (regexp-quote (nth 2 fl-match)) content)
            ;; TEXT is the part of CONTENT that starts with the match
            ;; of `bibtex-font-lock-url' we are looking for.
            (setq text (substring content (match-beginning 0)))
            (while (and (not url) (setq scheme (pop lst)))
              ;; Verify the match of `bibtex-font-lock-url' by
              ;; comparing with TEXT.
              (when (and (bibtex-string= (caar scheme) name)
                         (string-match (cdar scheme) text))
                (setq url t scheme (cdr scheme)))))))

      ;; If the match of `bibtex-font-lock-url' was not approved
      ;; parse FIELDS-ALIST, i.e., the output of `bibtex-parse-entry'.
      (unless url
        (let ((lst bibtex-generate-url-list))
          (while (and (not url) (setq scheme (pop lst)))
            (when (and (setq text (cdr (assoc-string (caar scheme)
                                                      fields-alist t)))
                       (string-match (cdar scheme) text))
              (setq url t scheme (cdr scheme))))))

      (when url
        (setq url (if (null scheme) (match-string 0 text)
                    (if (stringp (car scheme))
                        (setq fmt (pop scheme)))
                    (dolist (step scheme)
                      (setq text (cdr (assoc-string (car step) fields-alist t)))
                      (if (string-match (nth 1 step) text)
                          (push (cond ((functionp (nth 2 step))
                                       (funcall (nth 2 step) text))
                                      ((numberp (nth 2 step))
                                       (match-string (nth 2 step) text))
                                      (t
                                       (replace-match (nth 2 step) t nil text)))
                                obj)
                        ;; If SCHEME is set up correctly,
                        ;; we should never reach this point
                        (error "Match failed: %s" text)))
                    (if fmt (apply 'format fmt (nreverse obj))
                      (apply 'concat (nreverse obj)))))
        (if (called-interactively-p 'interactive) (message "%s" url))
        (unless no-browse (browse-url url)))
      (if (and (not url) (called-interactively-p 'interactive))
	  (message "No URL known."))
      url)))

;; We could combine multiple search results with set operations
;; AND, OR, MINUS, and NOT.  Would this be useful?
;; How complicated are searches in real life?
;; We could also have other searches such as "publication year newer than...".
(defun bibtex-search-entries (field regexp &optional global display)
  "Search BibTeX entries for FIELD matching REGEXP.
REGEXP may be a regexp to search for.
If REGEXP is a function, it is called for each entry with two args,
the buffer positions of beginning and end of entry.  Then an entry
is accepted if this function returns non-nil.
If FIELD is an empty string perform search for REGEXP in whole entry.
With GLOBAL non-nil, search in `bibtex-files'.  Otherwise the search
is limited to the current buffer.
If DISPLAY is non-nil, display search results in `bibtex-search-buffer'.
When called interactively, DISPLAY is t.
Also, GLOBAL is t if `bibtex-search-entry-globally' is non-nil.
A prefix arg negates the value of `bibtex-search-entry-globally'.
Return alist with elements (KEY FILE ENTRY),
where FILE is the BibTeX file of ENTRY."
  (interactive
   (list (completing-read
          "Field: "
          (delete-dups
           (apply 'append
                  bibtex-user-optional-fields
                  (mapcar (lambda (x) (mapcar 'car (apply 'append (nthcdr 2 x))))
                          bibtex-entry-alist))) nil t)
         (read-string "Regexp: ")
         (if bibtex-search-entry-globally
             (not current-prefix-arg)
           current-prefix-arg)
         t))
  (let ((funp (functionp regexp))
        entries text file)
    ;; If REGEXP is a function, the value of FIELD is ignored anyway.
    ;; Yet to ensure the code below does not fail, we make FIELD
    ;; a non-empty string.
    (if (and funp (string= "" field)) (setq field "unrestricted"))
    (dolist (buffer (if (and global bibtex-files)
                        (bibtex-initialize t)
                      (list (current-buffer))))
      (with-current-buffer buffer
        (setq file (if buffer-file-name
                       (file-name-nondirectory buffer-file-name)
                     (buffer-name buffer)))
        (save-excursion
          (goto-char (point-min))
          (if (string= "" field)
              ;; Unrestricted search.
              (while (re-search-forward regexp nil t)
                (let ((beg (bibtex-beginning-of-entry))
                      (end (bibtex-end-of-entry))
                      key)
                  (if (and (<= beg (match-beginning 0))
                           (<= (match-end 0) end)
                           (save-excursion
                             (goto-char beg)
                             (and (looking-at bibtex-entry-head)
                                  (setq key (bibtex-key-in-head))))
                           (not (assoc key entries)))
                      (push (list key file
                                  (buffer-substring-no-properties beg end))
                            entries))))
            ;; The following is slow.  But it works reliably even in more
            ;; complicated cases with BibTeX string constants and crossrefed
            ;; entries.  If you prefer speed over reliability, perform an
            ;; unrestricted search.
            (bibtex-map-entries
             (lambda (key beg end)
               (if (and (cond (funp (funcall regexp beg end))
                              ((and (setq text (bibtex-text-in-field field t))
                                    (string-match regexp text))))
                        (not (assoc key entries)))
                   (push (list key file
                               (buffer-substring-no-properties beg end))
                         entries))))))))
    (if display
        (if entries
            (bibtex-display-entries entries)
          (message "No BibTeX entries %smatching `%s'"
                   (if (string= "" field) ""
                     (format "with field `%s' " field))
                   regexp)))
    entries))

(defun bibtex-display-entries (entries &optional append)
  "Display BibTeX ENTRIES in `bibtex-search-buffer'.
ENTRIES is an alist with elements (KEY FILE ENTRY),
where FILE is the BibTeX file of ENTRY.
If APPEND is non-nil, append ENTRIES to those already displayed."
  (pop-to-buffer (get-buffer-create bibtex-search-buffer))
  ;; It would be nice if this buffer was editable, though editing
  ;; can be meaningful only for individual existing entries
  ;; (unlike reordering or creating new entries).
  ;; Fancy workaround: Editing commands in the virtual buffer could
  ;; jump to the real entry in the real buffer.
  (let (buffer-read-only)
    (if append (goto-char (point-max)) (erase-buffer))
    (dolist (entry (sort entries (lambda (x y) (string< (car x) (car y)))))
      (insert "% " (nth 1 entry) "\n" (nth 2 entry) "\n\n")))
    ;; `bibtex-sort-buffer' fails with the file names associated with
    ;; each entry.  Prior to sorting we could make the file name
    ;; a BibTeX field of each entry (using `bibtex-make-field').
    ;; Or we could make it a text property that we unfold afterwards.
    ;; (bibtex-sort-buffer)
  (bibtex-mode)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (goto-char (point-min)))


;; Make BibTeX a Feature

(provide 'bibtex)
;;; bibtex.el ends here
