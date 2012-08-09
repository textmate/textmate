;;; diary-lib.el --- diary functions

;; Copyright (C) 1989-1990, 1992-1995, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar

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

;; See calendar.el.

;;; Code:

(require 'calendar)
(eval-and-compile (load "diary-loaddefs" nil t))

(defgroup diary nil
  "Emacs diary."
  :prefix "diary-"
  :group 'calendar)

(defcustom diary-include-string "#include"
  "The string indicating inclusion of another file of diary entries.
See the documentation for the function `diary-include-other-diary-files'."
  :type 'string
  :group 'diary)

(defcustom diary-list-include-blanks nil
  "If nil, do not include days with no diary entry in the list of diary entries.
Such days will then not be shown in the fancy diary buffer, even if they
are holidays."
  :type 'boolean
  :group 'diary)

(defcustom diary-face 'diary
  "Face name to use for diary entries."
  :type 'face
  :group 'calendar-faces)
(make-obsolete-variable 'diary-face "customize the face `diary' instead."
                        "23.1")

(defface diary-anniversary '((t :inherit font-lock-keyword-face))
  "Face used for anniversaries in the fancy diary display."
  :version "22.1"
  :group 'calendar-faces)

(defface diary-time '((t :inherit font-lock-variable-name-face))
  "Face used for times of day in the fancy diary display."
  :version "22.1"
  :group 'calendar-faces)

(defface diary-button '((((type pc) (class color))
                         (:foreground "lightblue")))
  "Face used for buttons in the fancy diary display."
  :version "22.1"
  :group 'calendar-faces)

(define-obsolete-face-alias 'diary-button-face 'diary-button "22.1")

;; Face markup of calendar and diary displays: Any entry line that
;; ends with [foo:value] where foo is a face attribute (except :box
;; :stipple) or with [face:blah] tags, will have these values applied
;; to the calendar and fancy diary displays.  These attributes "stack"
;; on calendar displays.  File-wide attributes can be defined as
;; follows: the first line matching "^# [tag:value]" defines the value
;; for that particular tag.
(defcustom diary-face-attrs
  '((" *\\[foreground:\\([-a-z]+\\)\\]$" 1 :foreground string)
    (" *\\[background:\\([-a-z]+\\)\\]$" 1 :background string)
    (" *\\[width:\\([-a-z]+\\)\\]$" 1 :width symbol)
    (" *\\[height:\\([.0-9]+\\)\\]$" 1 :height int)
    (" *\\[weight:\\([-a-z]+\\)\\]$" 1 :weight symbol)
    (" *\\[slant:\\([-a-z]+\\)\\]$" 1 :slant symbol)
    (" *\\[underline:\\([-a-z]+\\)\\]$" 1 :underline stringtnil)
    (" *\\[overline:\\([-a-z]+\\)\\]$" 1 :overline stringtnil)
    (" *\\[strike-through:\\([-a-z]+\\)\\]$" 1 :strike-through stringtnil)
    (" *\\[inverse-video:\\([-a-z]+\\)\\]$" 1 :inverse-video tnil)
    (" *\\[face:\\([-0-9a-z]+\\)\\]$" 1 :face string)
    (" *\\[font:\\([-a-z0-9]+\\)\\]$" 1 :font string)
    ;; Unsupported.
;;;    (" *\\[box:\\([-a-z]+\\)\\]$" 1 :box)
;;;    (" *\\[stipple:\\([-a-z]+\\)\\]$" 1 :stipple)
    )
  "Alist of (REGEXP SUBEXP ATTRIBUTE TYPE) elements.
This is used by `diary-pull-attrs' to fontify certain diary
elements.  REGEXP is a regular expression to for, and SUBEXP is
the numbered sub-expression to extract.  `diary-glob-file-regexp-prefix'
is pre-pended to REGEXP for file-wide specifiers.  ATTRIBUTE
specifies which face attribute (e.g. `:foreground') to modify, or
that this is a face (`:face') to apply.  TYPE is the type of
attribute being applied.  Available TYPES (see `diary-attrtype-convert')
are: `string', `symbol', `int', `tnil', `stringtnil.'"
  :type '(repeat (list (string :tag "Regular expression")
                       (integer :tag "Sub-expression")
                       (symbol :tag "Attribute (e.g. :foreground)")
                       (choice (const string :tag "A string")
                               (const symbol :tag "A symbol")
                               (const int :tag "An integer")
                               (const tnil :tag "`t' or `nil'")
                               (const stringtnil
                                      :tag "A string, `t', or `nil'"))))
  :group 'diary)

(defcustom diary-glob-file-regexp-prefix "^\\#"
  "Regular expression pre-pended to `diary-face-attrs' for file-wide specifiers."
  :type 'regexp
  :group 'diary)

(defcustom diary-file-name-prefix nil
  "Non-nil means prefix each diary entry with the name of the file defining it."
  :type 'boolean
  :group 'diary)

(defcustom diary-file-name-prefix-function 'identity
  "The function that will take a diary file name and return the desired prefix."
  :type 'function
  :group 'diary)

(define-obsolete-variable-alias 'sexp-diary-entry-symbol
  'diary-sexp-entry-symbol "23.1")

(defcustom diary-sexp-entry-symbol "%%"
  "The string used to indicate a sexp diary entry in `diary-file'.
See the documentation for the function `diary-list-sexp-entries'."
  :type 'string
  :group 'diary)

(defcustom diary-comment-start nil
  "String marking the start of a comment in the diary, or nil.
Nil means there are no comments.  The diary does not display
parts of entries that are inside comments.  You can use comments
for whatever you like, e.g. for meta-data that packages such as
`appt.el' can use.  Comments may not span multiple lines, and there
can be only one comment on any line.
See also `diary-comment-end'."
  :version "24.1"
  :type '(choice (const :tag "No comment" nil) string)
  :group 'diary)

(defcustom diary-comment-end ""
  "String marking the end of a comment in the diary.
The empty string means comments finish at the end of a line.
See also `diary-comment-start'."
  :version "24.1"
  :type 'string
  :group 'diary)

(defcustom diary-hook nil
  "List of functions called after the display of the diary.
Used for example by the appointment package - see `appt-activate'."
  :type 'hook
  :group 'diary)

(define-obsolete-variable-alias 'diary-display-hook 'diary-display-function
  "23.1")

(defcustom diary-display-function 'diary-fancy-display
  "Function used to display the diary.
The two standard options are `diary-fancy-display' and `diary-simple-display'.

For historical reasons, `nil' is the same as `diary-simple-display'
\(so you must use `ignore' for no display).  Also for historical
reasons, this variable can be a list of functions to run.  These
uses are not recommended and may be removed at some point.

When this function is called, the variable `diary-entries-list'
is a list, in order by date, of all relevant diary entries in the
form of ((MONTH DAY YEAR) STRING), where string is the diary
entry for the given date.  This can be used, for example, to
produce a different buffer for display (perhaps combined with
holidays), or hard copy output."
  :type '(choice (const diary-fancy-display :tag "Fancy display")
                 (const diary-simple-display :tag "Basic display")
                 (const ignore :tag "No display")
                 (const nil :tag "Obsolete way to choose basic display")
                 (hook :tag "Obsolete form with list of display functions"))
  :initialize 'custom-initialize-default
  :set 'diary-set-maybe-redraw
  :version "23.2"                       ; simple->fancy
  :group 'diary)

(define-obsolete-variable-alias 'list-diary-entries-hook
  'diary-list-entries-hook "23.1")

(defcustom diary-list-entries-hook nil
  "List of functions called after diary file is culled for relevant entries.
You might wish to add `diary-include-other-diary-files', in which case
you will probably also want to add `diary-mark-included-diary-files' to
`diary-mark-entries-hook'.  For example, you could use

     (setq diary-display-function 'diary-fancy-display)
     (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
     (add-hook 'diary-list-entries-hook 'diary-sort-entries t)

in your `.emacs' file to cause the fancy diary buffer to be displayed with
diary entries from various included files, each day's entries sorted into
lexicographic order.  Note how the sort function is placed last,
so that it can sort the entries included from other files.

This hook runs after `diary-nongregorian-listing-hook'.  These two hooks
differ only if you are using included diary files.  In that case,
`diary-nongregorian-listing-hook' runs for each file, whereas
`diary-list-entries-hook' only runs once, for the main diary file.
So for example, to sort the complete list of diary entries you would
use the list-entries hook, whereas to process e.g. Islamic entries in
the main file and all included files, you would use the nongregorian hook."
  :type 'hook
  :options '(diary-include-other-diary-files diary-sort-entries)
  :group 'diary)

(define-obsolete-variable-alias 'mark-diary-entries-hook
  'diary-mark-entries-hook "23.1")

(defcustom diary-mark-entries-hook nil
  "List of functions called after marking diary entries in the calendar.
You might wish to add `diary-mark-included-diary-files', in which case
you will probably also want to add `diary-include-other-diary-files' to
`diary-list-entries-hook'.

This hook runs after `diary-nongregorian-marking-hook'.  These two hooks
differ only if you are using included diary files.  In that case,
`diary-nongregorian-marking-hook' runs for each file, whereas
`diary-mark-entries-hook' only runs once, for the main diary file."
  :type 'hook
  :options '(diary-mark-included-diary-files)
  :group 'diary)

(define-obsolete-variable-alias 'nongregorian-diary-listing-hook
  'diary-nongregorian-listing-hook "23.1")

(defcustom diary-nongregorian-listing-hook nil
  "List of functions called for listing diary file and included files.
As the files are processed for diary entries, these functions are used
to cull relevant entries.  You can use any or all of
`diary-bahai-list-entries', `diary-hebrew-list-entries', and
`diary-islamic-list-entries'.  The documentation for these functions
describes the style of such diary entries.

You can use this hook for other functions as well, if you want them to
be run on the main diary file and any included diary files.  Otherwise,
use `diary-list-entries-hook', which runs only for the main diary file."
  :type 'hook
  :options '(diary-bahai-list-entries
             diary-hebrew-list-entries
             diary-islamic-list-entries)
  :group 'diary)

(define-obsolete-variable-alias 'nongregorian-diary-marking-hook
  'diary-nongregorian-marking-hook "23.1")

(defcustom diary-nongregorian-marking-hook nil
  "List of functions called for marking diary file and included files.
As the files are processed for diary entries, these functions are used
to cull relevant entries.  You can use any or all of
`diary-bahai-mark-entries', `diary-hebrew-mark-entries' and
`diary-islamic-mark-entries'.  The documentation for these functions
describes the style of such diary entries.

You can use this hook for other functions as well, if you want them to
be run on the main diary file and any included diary files.  Otherwise,
use `diary-mark-entries-hook', which runs only for the main diary file."
  :type 'hook
  :options '(diary-bahai-mark-entries
             diary-hebrew-mark-entries
             diary-islamic-mark-entries)
  :group 'diary)

(define-obsolete-variable-alias 'print-diary-entries-hook
  'diary-print-entries-hook "23.1")

(defcustom diary-print-entries-hook 'lpr-buffer
  "Run by `diary-print-entries' after preparing a temporary diary buffer.
The buffer shows only the diary entries currently visible in the
diary buffer.  The default just does the printing.  Other uses
might include, for example, rearranging the lines into order by
day and time, saving the buffer instead of deleting it, or
changing the function used to do the printing."
  :type 'hook
  :group 'diary)

(defcustom diary-unknown-time -9999
  "Value returned by `diary-entry-time' when no time is found.
The default value -9999 causes entries with no recognizable time
to be placed before those with times; 9999 would place entries
with no recognizable time after those with times."
  :type 'integer
  :group 'diary
  :version "20.3")

(defcustom diary-mail-addr
  (or (bound-and-true-p user-mail-address) "")
  "Email address that `diary-mail-entries' will send email to."
  :group 'diary
  :type  'string
  :version "20.3")

(defcustom diary-mail-days 7
  "Default number of days for `diary-mail-entries' to check."
  :group 'diary
  :type 'integer
  :version "20.3")

(defcustom diary-remind-message
  '("Reminder: Only "
    (if (zerop (% days 7))
        (format "%d week%s" (/ days 7) (if (= 7 days) "" "s"))
      (format "%d day%s" days (if (= 1 days) "" "s")))
    " until "
    diary-entry)
  "Pseudo-pattern giving form of reminder messages in the fancy diary display.

Used by the function `diary-remind', a pseudo-pattern is a list of
expressions that can involve the keywords `days' (a number), `date'
\(a list of month, day, year), and `diary-entry' (a string)."
  :type 'sexp
  :group 'diary)

(define-obsolete-variable-alias 'abbreviated-calendar-year
  'diary-abbreviated-year-flag "23.1")

(defcustom diary-abbreviated-year-flag t
  "Interpret a two-digit year DD in a diary entry as either 19DD or 20DD.
This applies to the Gregorian, Hebrew, Islamic, and Bahá'í calendars.
When the current century is added to a two-digit year, if the result
is more than 50 years in the future, the previous century is assumed.
If the result is more than 50 years in the past, the next century is assumed.
If this variable is nil, years must be written in full."
  :type 'boolean
  :group 'diary)

(defun diary-outlook-format-1 (body)
  "Return a replace-match template for an element of `diary-outlook-formats'.
Returns a string using match elements 1-5, where:
1 = month name, 2 = day, 3 = year, 4 = time, 5 = location; also uses
%s = message subject.  BODY is the string from which the matches derive."
  (let* ((monthname (match-string 1 body))
        (day (match-string 2 body))
        (year (match-string 3 body))
        ;; Blech.
        (month (catch 'found
                 (dotimes (i (length calendar-month-name-array))
                   (if (string-equal (aref calendar-month-name-array i)
                                     monthname)
                       (throw 'found (1+ i))))
                 nil)))
    ;; If we could convert the monthname to a numeric month, we can
    ;; use the standard function calendar-date-string.
    (concat (if month
                (calendar-date-string (list month (string-to-number day)
                                            (string-to-number year)))
              (cond ((eq calendar-date-style 'iso) "\\3 \\1 \\2") ; YMD
                    ((eq calendar-date-style 'european) "\\2 \\1 \\3") ; DMY
                    (t "\\1 \\2 \\3"))) ; MDY
            "\n \\4 %s, \\5")))
;; TODO Sometimes the time is in a different time-zone to the one you
;; are in.  Eg in PST, you might still get an email referring to:
;; "7:00 PM-8:00 PM. Greenwich Standard Time".
;; Note that it doesn't use a standard abbreviation for the timezone,
;; or anything helpful like that.
;; Sigh, this could cause the meeting to even be on a different day
;; to that given in the When: string.
;; These things seem to come in a multipart mail with a calendar part,
;; it's probably better to use that rather than this whole thing.
;; So this is unlikely to get improved.

;; TODO Is the format of these messages actually documented anywhere?
(defcustom diary-outlook-formats
  '(;; When: Tuesday, November 9, 2010 7:00 PM-8:00 PM. Greenwich Standard Time
    ;; Where: Meeting room B
    ("[ \t\n]*When: [[:alpha:]]+, \\([[:alpha:]]+\\) \\([0-9][0-9]*\\), \
\\([0-9]\\{4\\}\\),? \\(.+\\)\n\
\\(?:Where: \\(.+\n\\)\\)?" . diary-outlook-format-1))
  "Alist of regexps matching message text and replacement text.

The regexp must match the start of the message text containing an
appointment, but need not include a leading `^'.  If it matches the
current message, a diary entry is made from the corresponding
template.  If the template is a string, it should be suitable for
passing to `replace-match', and so will have occurrences of `\\D' to
substitute the match for the Dth subexpression.  It must also contain
a single `%s' which will be replaced with the text of the message's
Subject field.  Any other `%' characters must be doubled, so that the
template can be passed to `format'.

If the template is actually a function, it is called with the message
body text as argument, and may use `match-string' etc. to make a
template following the rules above."
  :type '(alist :key-type (regexp :tag "Regexp matching time/place")
                :value-type (choice
                             (string :tag "Template for entry")
                             (function :tag
                                       "Unary function providing template")))
  :version "22.1"
  :group 'diary)

(defvar diary-header-line-flag)
(defvar diary-header-line-format)

(defun diary-set-header (symbol value)
  "Set SYMBOL's value to VALUE, and redraw the diary header if necessary."
  (let ((oldvalue (symbol-value symbol))
        (dbuff (and diary-file (find-buffer-visiting diary-file))))
    (custom-set-default symbol value)
    (and dbuff
         (not (equal value oldvalue))
         (with-current-buffer dbuff
           (if (eq major-mode 'diary-mode)
               (setq header-line-format (and diary-header-line-flag
                                             diary-header-line-format)))))))

;; This can be removed once the kill/yank treatment of invisible text
;; (see etc/TODO) is fixed. -- gm
(defcustom diary-header-line-flag t
  "Non-nil means `diary-simple-display' will show a header line.
The format of the header is specified by `diary-header-line-format'."
  :group   'diary
  :type    'boolean
  :initialize 'custom-initialize-default
  :set 'diary-set-header
  :version "22.1")

(defvar diary-selective-display nil
  "Internal diary variable; non-nil if some diary text is hidden.")

(defcustom diary-header-line-format
  '(:eval (calendar-string-spread
           (list (if diary-selective-display
                     "Some text is hidden - press \"s\" in calendar \
before edit/copy"
                   "Diary"))
           ?\s (window-width)))
  "Format of the header line displayed by `diary-simple-display'.
Only used if `diary-header-line-flag' is non-nil."
  :group 'diary
  :type 'sexp
  :initialize 'custom-initialize-default
  :set 'diary-set-header
  :version "23.3")                      ; frame-width -> window-width

;; The first version of this also checked for diary-selective-display
;; in the non-fancy case. This was an attempt to distinguish between
;; displaying the diary and just visiting the diary file. However,
;; when using fancy diary, calling diary when there are no entries to
;; display does not create the fancy buffer, nor does it set
;; diary-selective-display in the diary buffer. This means some
;; customizations will not take effect, eg:
;; http://lists.gnu.org/archive/html/emacs-pretest-bug/2007-03/msg00466.html
;; So the check for diary-selective-display was dropped. This means the
;; diary will be displayed if one customizes a diary variable while
;; just visiting the diary-file. This is i) unlikely, and ii) no great loss.
;;;###cal-autoload
(defun diary-live-p ()
  "Return non-nil if the diary is being displayed."
  (or (get-buffer diary-fancy-buffer)
      (and diary-file (find-buffer-visiting diary-file))))

;;;###cal-autoload
(defun diary-set-maybe-redraw (symbol value)
  "Set SYMBOL's value to VALUE, and redraw the diary if necessary.
Redraws the diary if it is being displayed (note this is not the same as
just visiting the `diary-file'), and SYMBOL's value is to be changed."
  (let ((oldvalue (symbol-value symbol)))
    (custom-set-default symbol value)
    (and (not (equal value oldvalue))
         (diary-live-p)
         ;; Note this assumes diary was called without prefix arg.
         (diary))))

(define-obsolete-variable-alias 'number-of-diary-entries
  'diary-number-of-entries "23.1")

(defcustom diary-number-of-entries 1
  "Specifies how many days of diary entries are to be displayed initially.
This variable affects the diary display when the command \\[diary] is
used, or if the value of the variable `calendar-view-diary-initially-flag'
is non-nil.  For example, if the default value 1 is used, then only the
current day's diary entries will be displayed.  If the value 2 is used,
then both the current day's and the next day's entries will be displayed.

The value can also be a vector such as [0 2 2 2 2 4 1]; this value says
to display no diary entries on Sunday, the entries for the current date
and the day after on Monday through Thursday, Friday through Monday's
entries on Friday, and only Saturday's entries on Saturday.

This variable does not affect the diary display with the `d' command
from the calendar; in that case, the prefix argument controls the number
of days of diary entries displayed."
  :type '(choice (integer :tag "Entries")
                 (vector :value [0 0 0 0 0 0 0]
                         (integer :tag "Sunday")
                         (integer :tag "Monday")
                         (integer :tag "Tuesday")
                         (integer :tag "Wednesday")
                         (integer :tag "Thursday")
                         (integer :tag "Friday")
                         (integer :tag "Saturday")))
  :initialize 'custom-initialize-default
  :set 'diary-set-maybe-redraw
  :group 'diary)

;;; More user options in calendar.el, holidays.el.


(defun diary-check-diary-file ()
  "Check that the file specified by `diary-file' exists and is readable.
If so, return the expanded file name, otherwise signal an error."
  (if (and diary-file (file-exists-p diary-file))
      (if (file-readable-p diary-file)
          diary-file
        (error "Diary file `%s' is not readable" diary-file))
    (error "Diary file `%s' does not exist" diary-file)))

;;;###autoload
(defun diary (&optional arg)
  "Generate the diary window for ARG days starting with the current date.
If no argument is provided, the number of days of diary entries is governed
by the variable `diary-number-of-entries'.  A value of ARG less than 1
does nothing.  This function is suitable for execution in a `.emacs' file."
  (interactive "P")
  (diary-check-diary-file)
  (diary-list-entries (calendar-current-date)
                      (if arg (prefix-numeric-value arg))))

;;;###cal-autoload
(defun diary-view-entries (&optional arg)
  "Prepare and display a buffer with diary entries.
Searches the file named in `diary-file' for entries that match
ARG days starting with the date indicated by the cursor position
in the displayed three-month calendar."
  (interactive "p")
  (diary-check-diary-file)
  (diary-list-entries (calendar-cursor-to-date t) arg))


;;;###cal-autoload
(defun diary-view-other-diary-entries (arg dfile)
  "Prepare and display buffer of diary entries from an alternative diary file.
Searches for entries that match ARG days, starting with the date indicated
by the cursor position in the displayed three-month calendar.
DFILE specifies the file to use as the diary file."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-file-name "Enter diary file name: " default-directory nil t)))
  (let ((diary-file dfile))
    (diary-view-entries arg)))

;;;###cal-autoload
(define-obsolete-function-alias 'view-other-diary-entries
  'diary-view-other-diary-entries "23.1")

(defvar diary-syntax-table
  (let ((st (copy-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?* "w" st)
    (modify-syntax-entry ?: "w" st)
    st)
  "The syntax table used when parsing dates in the diary file.
It is the standard syntax table used in Fundamental mode, but with the
syntax of `*' and `:' changed to be word constituents.")

(defun diary-attrtype-convert (attrvalue type)
  "Convert string ATTRVALUE to TYPE appropriate for a face description.
Valid TYPEs are: string, symbol, int, stringtnil, tnil."
  (cond ((eq type 'string) attrvalue)
        ((eq type 'symbol) (intern-soft attrvalue))
        ((eq type 'int) (string-to-number attrvalue))
        ((eq type 'stringtnil)
         (cond ((string-equal "t" attrvalue) t)
               ((string-equal "nil" attrvalue) nil)
               (t attrvalue)))
        ((eq type 'tnil) (string-equal "t" attrvalue))))

(defun diary-pull-attrs (entry fileglobattrs)
  "Search for matches for regexps from `diary-face-attrs'.
If ENTRY is nil, searches from the start of the current buffer, and
prepends all regexps with `diary-glob-file-regexp-prefix'.
If ENTRY is a string, search for matches in that string, and remove them.
Returns a list of ENTRY followed by (ATTRIBUTE VALUE) pairs.
When ENTRY is non-nil, FILEGLOBATTRS forms the start of the (ATTRIBUTE VALUE)
pairs."
  (let (regexp regnum attrname attrname attrvalue type ret-attr)
    (if (null entry)
        (save-excursion
          (dolist (attr diary-face-attrs)
            ;; FIXME inefficient searching.
            (goto-char (point-min))
            (setq regexp (concat diary-glob-file-regexp-prefix (car attr))
                  regnum (cadr attr)
                  attrname (nth 2 attr)
                  type (nth 3 attr)
                  attrvalue (if (re-search-forward regexp nil t)
                                (match-string-no-properties regnum)))
            (and attrvalue
                 (setq attrvalue (diary-attrtype-convert attrvalue type))
                 (setq ret-attr (append ret-attr
                                        (list attrname attrvalue))))))
      (setq ret-attr fileglobattrs)
      (dolist (attr diary-face-attrs)
        (setq regexp (car attr)
              regnum (cadr attr)
              attrname (nth 2 attr)
              type (nth 3 attr)
              attrvalue nil)
        ;; If multiple matches, replace all, use the last (which may
        ;; be the first instance in the line, if the regexp is
        ;; anchored with $).
        (while (string-match regexp entry)
          (setq attrvalue (match-string-no-properties regnum entry)
                entry (replace-match "" t t entry)))
        (and attrvalue
             (setq attrvalue (diary-attrtype-convert attrvalue type))
             (setq ret-attr (append ret-attr (list attrname attrvalue))))))
    (list entry ret-attr)))



(defvar diary-modify-entry-list-string-function nil
  "Function applied to entry string before putting it into the entries list.
Can be used by programs integrating a diary list into other buffers (e.g.
org.el and planner.el) to modify the string or add properties to it.
The function takes a string argument and must return a string.")

(defvar diary-entries-list)             ; bound in diary-list-entries

(defun diary-add-to-list (date string specifier &optional marker
                               globcolor literal)
  "Add an entry to `diary-entries-list'.
Do nothing if DATE or STRING are nil.  DATE is the (MONTH DAY
YEAR) for which the entry applies; STRING is the text of the
entry as it will appear in the diary (i.e. with any format
strings such as \"%d\" expanded); SPECIFIER is the date part of
the entry as it appears in the diary-file; LITERAL is the entry
as it appears in the diary-file (i.e. before expansion).
If LITERAL is nil, it is taken to be the same as STRING.

The entry is added to the list as (DATE STRING SPECIFIER LOCATOR
GLOBCOLOR), where LOCATOR has the form (MARKER FILENAME LITERAL),
FILENAME being the file containing the diary entry.

Modifies STRING using `diary-modify-entry-list-string-function', if non-nil.
Also removes the region between `diary-comment-start' and
`diary-comment-end', if the former is non-nil."
  (when (and date string)
    ;; b-f-n is nil if we are visiting an include file in a temp-buffer.
    (let ((dfile (or (buffer-file-name) diary-file))
          cstart)
      (if diary-file-name-prefix
          (let ((prefix (funcall diary-file-name-prefix-function dfile)))
            (or (string-equal prefix "")
                (setq string (format "[%s] %s" prefix string)))))
      (and diary-modify-entry-list-string-function
           (setq string (funcall diary-modify-entry-list-string-function
                                 string)))
      (when (and diary-comment-start
                 (string-match (setq cstart (regexp-quote diary-comment-start))
                               string))
        ;; Preserve the value with the comments.
        (or literal (setq literal string))
        ;; Handles multiple comments per entry, so long as each is on
        ;; a single line, and each line has no more than one comment.
        (setq string (replace-regexp-in-string
                      (format "%s.*%s" cstart (regexp-quote diary-comment-end))
                      "" string)))
      (setq diary-entries-list
            (append diary-entries-list
                    (list (list date string specifier
                                (list marker dfile literal)
                                globcolor)))))))

(define-obsolete-function-alias 'add-to-diary-list 'diary-add-to-list "23.1")

(defun diary-list-entries-2 (date mark globattr list-only
                                  &optional months symbol gdate)
  "Internal subroutine of `diary-list-entries'.
Find diary entries applying to DATE, by searching from point-min for
each element of `diary-date-forms'.  MARK indicates an entry is non-marking.
GLOBATTR is the list of global file attributes.  If LIST-ONLY is
non-nil, don't change the buffer, only return a list of entries.
Optional array MONTHS replaces `calendar-month-name-array', and
means months cannot be abbreviated.  Optional string SYMBOL marks diary
entries of the desired type.  If DATE is not Gregorian, then the
Gregorian equivalent should be provided via GDATE.  Returns non-nil if
any entries were found."
  (let* ((month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (dayname (format "%s\\|%s\\.?" (calendar-day-name date)
                          (calendar-day-name date 'abbrev)))
         (calendar-month-name-array (or months calendar-month-name-array))
         (monthname (format "\\*\\|%s%s" (calendar-month-name month)
                            (if months ""
                              (format "\\|%s\\.?"
                                      (calendar-month-name month 'abbrev)))))
         (month (format "\\*\\|0*%d" month))
         (day (format "\\*\\|0*%d" day))
         (year (format "\\*\\|0*%d%s" year
                       (if diary-abbreviated-year-flag
                           (format "\\|%02d" (% year 100))
                         "")))
        (case-fold-search t)
        entry-found)
    (dolist (date-form diary-date-forms)
      (let ((backup (when (eq (car date-form) 'backup)
                      (setq date-form (cdr date-form))
                      t))
            ;; date-form uses day etc as set above.
            (regexp (format "^%s?%s\\(%s\\)" (regexp-quote mark)
                            (if symbol (regexp-quote symbol) "")
                            (mapconcat 'eval date-form "\\)\\(?:")))
            entry-start date-start temp)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (if backup (re-search-backward "\\<" nil t))
          ;; regexp moves us past the end of date, onto the next line.
          ;; Trailing whitespace after date not allowed (see diary-file).
          (if (and (bolp) (not (looking-at "[ \t]")))
              ;; Diary entry that consists only of date.
              (backward-char 1)
            ;; Found a nonempty diary entry--make it
            ;; visible and add it to the list.
            (setq date-start (line-end-position 0))
            ;; Actual entry starts on the next-line?
            (if (looking-at "[ \t]*\n[ \t]") (forward-line 1))
            (setq entry-found t
                  entry-start (point))
            (forward-line 1)
            (while (looking-at "[ \t]") ; continued entry
              (forward-line 1))
            (unless (and (eobp) (not (bolp)))
              (backward-char 1))
            (unless list-only
              (remove-overlays date-start (point) 'invisible 'diary))
            (setq temp (diary-pull-attrs
                        (buffer-substring-no-properties
                         entry-start (point)) globattr))
            (diary-add-to-list
             (or gdate date) (car temp)
             (buffer-substring-no-properties (1+ date-start) (1- entry-start))
             (copy-marker entry-start) (cadr temp))))))
    entry-found))

(defvar original-date)                  ; from diary-list-entries
(defvar file-glob-attrs)
(defvar list-only)
(defvar number)

(defun diary-list-entries-1 (months symbol absfunc)
  "List diary entries of a certain type.
MONTHS is an array of month names.  SYMBOL marks diary entries of the type
in question.  ABSFUNC is a function that converts absolute dates to dates
of the appropriate type."
  (let ((gdate original-date))
    (dotimes (_idummy number)
      (diary-list-entries-2
       (funcall absfunc (calendar-absolute-from-gregorian gdate))
       diary-nonmarking-symbol file-glob-attrs list-only months symbol gdate)
      (setq gdate
            (calendar-gregorian-from-absolute
             (1+ (calendar-absolute-from-gregorian gdate))))))
  (goto-char (point-min)))

(defvar diary-included-files nil
  "List of any diary files included in the last call to `diary-list-entries'.
Or to `diary-mark-entries'.")

(defun diary-list-entries (date number &optional list-only)
  "Create and display a buffer containing the relevant lines in `diary-file'.
Selects entries for NUMBER days starting with date DATE.  Hides any
other entries using overlays.  If NUMBER is less than 1, this function
does nothing.

Returns a list of all relevant diary entries found.
The list entries have the form ((MONTH DAY YEAR) STRING SPECIFIER) where
\(MONTH DAY YEAR) is the date of the entry, STRING is the entry text, and
SPECIFIER is the applicability.  If the variable `diary-list-include-blanks'
is non-nil, this list includes a dummy diary entry consisting of the empty
string for a date with no diary entries.

If producing entries for multiple dates (i.e., NUMBER > 1), then
this function normally returns the entries from any given diary
file in date order.  The entries for any given day are in the
order in which they were found in the file, not necessarily in
time-of-day order.  Note that any functions present on the
hooks (see below) may add entries, or change the order.  For
example, `diary-include-other-diary-files' adds entries from any
include files that it finds to the end of the original list.  The
entries from each file will be in date order, but the overall
list will not be.  If you want the entire list to be in time
order, add `diary-sort-entries' to the end of `diary-list-entries-hook'.

After preparing the initial list, hooks run in this order:

  `diary-nongregorian-listing-hook' runs for the main diary file,
      and each included file.  For example, this is the appropriate hook
      to process Islamic entries in all diary files.

  `diary-list-entries-hook' runs once only, for the main diary file.
      For example, this is appropriate for sorting all the entries.
      If not using include files, there is no difference from the previous
      hook.

  `diary-hook' runs last, after the diary is displayed.
      This is used e.g. by `appt-check'.

Functions called by these hooks may use the variables ORIGINAL-DATE
and NUMBER, which are the arguments with which this function was called.
Note that hook functions should _not_ use DATE, but ORIGINAL-DATE.
\(Sexp diary entries may use DATE - see `diary-list-sexp-entries'.)

This function displays the list using `diary-display-function', unless
LIST-ONLY is non-nil, in which case it just returns the list."
  (unless number
    (setq number (if (vectorp diary-number-of-entries)
                     (aref diary-number-of-entries (calendar-day-of-week date))
                   diary-number-of-entries)))
  (when (> number 0)
    (let* ((original-date date)    ; save for possible use in the hooks
           (date-string (calendar-date-string date))
           (diary-buffer (find-buffer-visiting diary-file))
           ;; Dynamically bound in diary-include-files.
           (d-incp (and (boundp 'diary-including) diary-including))
           diary-entries-list file-glob-attrs temp-buff)
      (unless d-incp
        (setq diary-included-files nil)
        (message "Preparing diary..."))
      (unwind-protect
          (with-current-buffer (or diary-buffer
                                   (if list-only
                                       (setq temp-buff (generate-new-buffer
                                                        " *diary-temp*"))
                                     (find-file-noselect diary-file t)))
            (if diary-buffer
                (or (verify-visited-file-modtime diary-buffer)
                    (revert-buffer t t)))
            (if temp-buff
                ;; If including, caller has already verified it is readable.
                (insert-file-contents diary-file)
              ;; Setup things like the header-line-format and invisibility-spec.
              (if (eq major-mode (default-value 'major-mode))
                  (diary-mode)
                ;; This kludge is to make customizations to
                ;; diary-header-line-flag after diary has been displayed
                ;; take effect. Unconditionally calling (diary-mode)
                ;; clobbers file local variables.
                ;; http://lists.gnu.org/archive/html/emacs-pretest-bug/2007-03/msg00363.html
                ;; http://lists.gnu.org/archive/html/emacs-pretest-bug/2007-04/msg00404.html
                (if (eq major-mode 'diary-mode)
                    (setq header-line-format (and diary-header-line-flag
                                                  diary-header-line-format)))))
            ;; d-s-p is passed to the diary display function.
            (let ((diary-saved-point (point)))
              (save-excursion
                (save-restriction
                  (widen)                   ; bug#5093
                  (setq file-glob-attrs (cadr (diary-pull-attrs nil "")))
                  (with-syntax-table diary-syntax-table
                    (goto-char (point-min))
                    (unless list-only
                      (let ((ol (make-overlay (point-min) (point-max) nil t nil)))
                        (set (make-local-variable 'diary-selective-display) t)
                        (overlay-put ol 'invisible 'diary)
                        (overlay-put ol 'evaporate t)))
                    (dotimes (_idummy number)
                      (let ((sexp-found (diary-list-sexp-entries date))
                            (entry-found (diary-list-entries-2
                                          date diary-nonmarking-symbol
                                          file-glob-attrs list-only)))
                        (if diary-list-include-blanks
                            (or sexp-found entry-found
                                (diary-add-to-list date "" "" "" "")))
                        (setq date
                              (calendar-gregorian-from-absolute
                               (1+ (calendar-absolute-from-gregorian date)))))))
                  (goto-char (point-min))
                  ;; Although it looks like list-entries-hook runs
                  ;; every time, diary-include-other-diary-files
                  ;; binds it to nil (essentially) when it runs
                  ;; in included files.
                  (run-hooks 'diary-nongregorian-listing-hook
                             'diary-list-entries-hook)
                  ;; We could make this explicit:
                  ;;; (run-hooks 'diary-nongregorian-listing-hook)
                  ;;; (if d-incp
                  ;;;     (diary-include-other-diary-files) ; recurse
                  ;;;   (run-hooks 'diary-list-entries-hook))
                  (unless list-only
                    (if (and diary-display-function
                             (listp diary-display-function))
                        ;; Backwards compatibility.
                        (run-hooks 'diary-display-function)
                      (funcall (or diary-display-function
                                   'diary-simple-display))))
                  (run-hooks 'diary-hook)))))
        (and temp-buff (buffer-name temp-buff) (kill-buffer temp-buff)))
      (or d-incp (message "Preparing diary...done"))
      diary-entries-list)))

(defun diary-unhide-everything ()
  "Show all invisible text in the diary."
  (kill-local-variable 'diary-selective-display)
  (save-restriction                     ; bug#5477
    (widen)
    (remove-overlays (point-min) (point-max) 'invisible 'diary))
  (kill-local-variable 'mode-line-format))

(defvar original-date)                  ; bound in diary-list-entries
;(defvar number)                         ; already declared above

(defun diary-include-files (&optional mark)
  "Process diary entries from included diary files.
By default, lists included entries, but if optional argument MARK is non-nil
marks entries instead.
For example, this enables you to share common diary files.
Specify include files using lines matching `diary-include-string', e.g.
    #include \"filename\"
This is recursive; that is, included files may include other files."
  (goto-char (point-min))
  (while (re-search-forward
          (format "^%s \"\\([^\"]*\\)\"" (regexp-quote diary-include-string))
          nil t)
    (let ((diary-file (match-string-no-properties 1))
          (diary-mark-entries-hook 'diary-mark-included-diary-files)
          (diary-list-entries-hook 'diary-include-other-diary-files)
          (diary-including t)
          diary-hook diary-list-include-blanks efile)
      (if (file-exists-p diary-file)
          (if (file-readable-p diary-file)
              (if (member (setq efile (expand-file-name diary-file))
                          diary-included-files)
                  (error "Recursive diary include for %s" diary-file)
                (setq diary-included-files
                      (append diary-included-files (list efile)))
                (if mark
                    (diary-mark-entries)
                  (setq diary-entries-list
                        (append diary-entries-list
                                (diary-list-entries original-date number t)))))
            (beep)
            (message "Can't read included diary file %s" diary-file)
            (sleep-for 2))
        (beep)
        (message "Can't find included diary file %s" diary-file)
        (sleep-for 2))))
  (goto-char (point-min)))

(defun diary-include-other-diary-files ()
  "Add diary entries from included diary files to `diary-entries-list'.
To use, add this function to `diary-list-entries-hook'.
For details, see `diary-include-files'.
See also `diary-mark-included-diary-files'."
  (diary-include-files))

(define-obsolete-function-alias 'include-other-diary-files
  'diary-include-other-diary-files "23.1")

(defvar date-string)                    ; bound in diary-list-entries

(defun diary-display-no-entries ()
  "Common subroutine of `diary-simple-display' and `diary-fancy-display'.
Handles the case where there are no diary entries.
Returns a cons (NOENTRIES . HOLIDAY-STRING)."
    (let* ((holiday-list (if diary-show-holidays-flag
                             (calendar-check-holidays original-date)))
           (hol-string (format "%s%s%s"
                               date-string
                               (if holiday-list ": " "")
                               (mapconcat 'identity holiday-list "; ")))
           (msg (format "No diary entries for %s" hol-string))
           ;; Empty list, or single item with no text.
           ;; FIXME multiple items with no text?
           (noentries (or (not diary-entries-list)
                          (and (not (cdr diary-entries-list))
                               (string-equal "" (cadr
                                                 (car diary-entries-list)))))))
      ;; Inconsistency: whether or not the holidays are displayed in a
      ;; separate buffer depends on if there are diary entries.
      (when noentries
        (if (or (< (length msg) (frame-width))
                (not holiday-list))
            (message "%s" msg)
          ;; holiday-list which is too wide for a message gets a buffer.
          (calendar-in-read-only-buffer holiday-buffer
            (calendar-set-mode-line (format "Holidays for %s" date-string))
            (insert (mapconcat 'identity holiday-list "\n")))
          (message "No diary entries for %s" date-string)))
      (cons noentries hol-string)))


(defvar diary-saved-point)              ; bound in diary-list-entries

(defun diary-simple-display ()
  "Display the diary buffer if there are any relevant entries or holidays.
Entries that do not apply are made invisible.  Holidays are shown
in the mode line.  This is an option for `diary-display-function'."
  ;; If selected window is dedicated (to the calendar), need a new one
  ;; to display the diary.
  (let* ((pop-up-frames (or pop-up-frames
                            (window-dedicated-p (selected-window))))
         (dbuff (find-buffer-visiting diary-file))
         (empty (diary-display-no-entries)))
    ;; This may be too wide, but when simple diary is used there is
    ;; nowhere else for the holidays to go.  Also, it is documented in
    ;; diary-show-holidays-flag that the holidays go in the mode-line.
    ;; FIXME however if there are no diary entries a separate buffer
    ;; is displayed - this is inconsistent.
    (with-current-buffer dbuff
      (calendar-set-mode-line (format "Diary for %s" (cdr empty))))
    (unless (car empty)                 ; no entries
      (with-current-buffer dbuff
        (let ((window (display-buffer (current-buffer))))
          ;; d-s-p is passed from diary-list-entries.
          (set-window-point window diary-saved-point)
          (set-window-start window (point-min)))))))

(define-obsolete-function-alias 'simple-diary-display
  'diary-simple-display "23.1")

(define-button-type 'diary-entry 'action #'diary-goto-entry
  'face 'diary-button 'help-echo "Find this diary entry"
  'follow-link t)

(defun diary-goto-entry (button)
  "Jump to the diary entry for the BUTTON at point."
  (let* ((locator (button-get button 'locator))
         (marker (car locator))
         markbuf file)
    ;; If marker pointing to diary location is valid, use that.
    (if (and marker (setq markbuf (marker-buffer marker)))
        (progn
          (pop-to-buffer markbuf)
          (goto-char (marker-position marker)))
      ;; Marker is invalid (eg buffer has been killed).
      (or (and (setq file (cadr locator))
               (file-exists-p file)
               (find-file-other-window file)
               (progn
                 (when (eq major-mode (default-value 'major-mode)) (diary-mode))
                 (goto-char (point-min))
                 (if (re-search-forward (format "%s.*\\(%s\\)"
                                                (regexp-quote (nth 2 locator))
                                                (regexp-quote (nth 3 locator)))
                                        nil t)
                     (goto-char (match-beginning 1)))))
          (message "Unable to locate this diary entry")))))

(defun diary-fancy-display ()
  "Prepare a diary buffer with relevant entries in a fancy, noneditable form.
Holidays are shown unless `diary-show-holidays-flag' is nil.
Days with no diary entries are not shown (even if that day is a
holiday), unless `diary-list-include-blanks' is non-nil.

This is an option for `diary-display-function'."
  ;; Turn off selective-display in the diary file's buffer.
  (with-current-buffer (find-buffer-visiting diary-file)
    (diary-unhide-everything))
  (unless (car (diary-display-no-entries)) ; no entries
    ;; Prepare the fancy diary buffer.
    (calendar-in-read-only-buffer diary-fancy-buffer
      (calendar-set-mode-line "Diary Entries")
      (let ((holiday-list-last-month 1)
            (holiday-list-last-year 1)
            (date (list 0 0 0))
            holiday-list)
        (dolist (entry diary-entries-list)
          (unless (calendar-date-equal date (car entry))
            (setq date (car entry))
            (and diary-show-holidays-flag
                 (calendar-date-compare
                  (list (list holiday-list-last-month
                              (calendar-last-day-of-month
                               holiday-list-last-month
                               holiday-list-last-year)
                              holiday-list-last-year))
                  (list date))
                 ;; We need to get the holidays for the next 3 months.
                 (setq holiday-list-last-month
                       (calendar-extract-month date)
                       holiday-list-last-year
                       (calendar-extract-year date))
                 (progn
                   (calendar-increment-month
                    holiday-list-last-month holiday-list-last-year 1)
                   t)
                 (setq holiday-list
                       (let ((displayed-month holiday-list-last-month)
                             (displayed-year holiday-list-last-year))
                         (calendar-holiday-list)))
                 (calendar-increment-month
                  holiday-list-last-month holiday-list-last-year 1))
            (let ((longest 0)
                  date-holiday-list cc)
              ;; Make a list of all holidays for date.
              (dolist (h holiday-list)
                (if (calendar-date-equal date (car h))
                    (setq date-holiday-list (append date-holiday-list
                                                    (cdr h)))))
              (insert (if (bobp) "" ?\n) (calendar-date-string date))
              (if date-holiday-list (insert ":  "))
              (setq cc (current-column))
              (insert (mapconcat (lambda (x)
                                   (setq longest (max longest (length x)))
                                   x)
                                 date-holiday-list
                                 (concat "\n" (make-string cc ?\s))))
              (insert ?\n (make-string (+ cc longest) ?=) ?\n)))
          (let ((this-entry (cadr entry))
                this-loc marks temp-face)
            (unless (zerop (length this-entry))
              (if (setq this-loc (nth 3 entry))
                  (insert-button this-entry
                                 ;; (MARKER FILENAME SPECIFIER LITERAL)
                                 'locator (list (car this-loc)
                                                (cadr this-loc)
                                                (nth 2 entry)
                                                (or (nth 2 this-loc)
                                                    (nth 1 entry)))
                                 :type 'diary-entry)
                (insert this-entry))
              (insert ?\n)
              ;; Doesn't make sense to check font-lock-mode - see
              ;; comments above diary-entry-marker in calendar.el.
              (and ; font-lock-mode
                   (setq marks (nth 4 entry))
                   (save-excursion
                     (setq temp-face (calendar-make-temp-face marks))
                     (search-backward this-entry)
                     (overlay-put
                      (make-overlay (match-beginning 0) (match-end 0))
                      'face temp-face)))))))
      ;; FIXME can't remember what this check was for.
      ;; To prevent something looping, or a minor optimization?
      (if (eq major-mode 'diary-fancy-display-mode)
          (run-hooks 'diary-fancy-display-mode-hook)
        (diary-fancy-display-mode))
      (calendar-set-mode-line date-string))))

(define-obsolete-function-alias 'fancy-diary-display
  'diary-fancy-display "23.1")

;; FIXME modernize?
(defun diary-print-entries ()
  "Print a hard copy of the diary display.

If the simple diary display is being used, prepare a temp buffer with the
visible lines of the diary buffer, add a heading line composed from the mode
line, print the temp buffer, and destroy it.

If the fancy diary display is being used, just print the buffer.

The hooks given by the variable `diary-print-entries-hook' are called to do
the actual printing."
  (interactive)
  (let ((diary-buffer (get-buffer diary-fancy-buffer))
        temp-buffer heading start end)
    (if diary-buffer
        (with-current-buffer diary-buffer
          (run-hooks 'diary-print-entries-hook))
      (or (setq diary-buffer (find-buffer-visiting diary-file))
          (error "You don't have a diary buffer!"))
      ;; Name affects printing?
      (setq temp-buffer (get-buffer-create " *Printable Diary Entries*"))
      (with-current-buffer diary-buffer
        (setq heading
              (if (not (stringp mode-line-format))
                  "All Diary Entries"
                (string-match "^-*\\([^-].*[^-]\\)-*$" mode-line-format)
                (match-string 1 mode-line-format))
              start (point-min))
        (while
            (progn
              (setq end (next-single-char-property-change start 'invisible))
              (unless (get-char-property start 'invisible)
                (with-current-buffer temp-buffer
                  (insert-buffer-substring diary-buffer start end)))
              (setq start end)
              (and end (< end (point-max))))))
      (set-buffer temp-buffer)
      (goto-char (point-min))
      (insert heading "\n"
              (make-string (length heading) ?=) "\n")
      (run-hooks 'diary-print-entries-hook)
      (kill-buffer temp-buffer))))

(define-obsolete-function-alias 'print-diary-entries
  'diary-print-entries "23.1")

;;;###cal-autoload
(defun diary-show-all-entries ()
  "Show all of the diary entries in the diary file.
This function gets rid of the selective display of the diary file so that
all entries, not just some, are visible.  If there is no diary buffer, one
is created."
  (interactive)
  (let* ((d-file (diary-check-diary-file))
         (pop-up-frames (or pop-up-frames
                            (window-dedicated-p (selected-window))))
         (win (selected-window))
         (height (window-height)))
    (with-current-buffer (or (find-buffer-visiting d-file)
                             (find-file-noselect d-file t))
      (when (eq major-mode (default-value 'major-mode)) (diary-mode))
      (diary-unhide-everything)
      (display-buffer (current-buffer))
      (when (and (/= height (window-height win))
                 (with-current-buffer (window-buffer win)
                   (derived-mode-p 'calendar-mode)))
        (fit-window-to-buffer win)))))

;;;###autoload
(defun diary-mail-entries (&optional ndays)
  "Send a mail message showing diary entries for next NDAYS days.
If no prefix argument is given, NDAYS is set to `diary-mail-days'.
Mail is sent to the address specified by `diary-mail-addr'.

Here is an example of a script to call `diary-mail-entries',
suitable for regular scheduling using cron (or at).  Note that
since `emacs -script' does not load your `.emacs' file, you
should ensure that all relevant variables are set.

#!/usr/bin/emacs -script
;; diary-rem.el - run the Emacs diary-reminder

\(setq diary-mail-days 3
      diary-file \"/path/to/diary.file\"
      calendar-date-style 'european
      diary-mail-addr \"user@host.name\")

\(diary-mail-entries)

# diary-rem.el ends here
"
  (interactive "P")
  (if (string-equal diary-mail-addr "")
      (error "You must set `diary-mail-addr' to use this command")
    (let ((diary-display-function 'diary-fancy-display))
      (diary-list-entries (calendar-current-date) (or ndays diary-mail-days)))
    (compose-mail diary-mail-addr
                  (concat "Diary entries generated "
                          (calendar-date-string (calendar-current-date))))
    (insert
     (if (get-buffer diary-fancy-buffer)
         (with-current-buffer diary-fancy-buffer (buffer-string))
       "No entries found"))
    (call-interactively (get mail-user-agent 'sendfunc))))

(defun diary-name-pattern (string-array &optional abbrev-array paren)
  "Return a regexp matching the strings in the array STRING-ARRAY.
If the optional argument ABBREV-ARRAY is present, the regexp
also matches the supplied abbreviations, with or without final `.'
characters.  If the optional argument PAREN is non-nil, surrounds
the regexp with parentheses."
  (regexp-opt (append string-array
                      abbrev-array
                      (if abbrev-array
                          (mapcar (lambda (e) (format "%s." e))
                                  abbrev-array))
                      nil)
              paren))

(defvar diary-marking-entries-flag nil
  "True during the marking of diary entries, nil otherwise.")

(defvar diary-marking-entry-flag nil
  "True during the marking of diary entries, if current entry is marking.")

;; file-glob-attrs bound in diary-mark-entries.
(defun diary-mark-entries-1 (markfunc &optional months symbol absfunc)
  "Mark diary entries of a certain type.
MARKFUNC is a function that marks entries of the appropriate type
matching a given date pattern.  MONTHS is an array of month names.
SYMBOL marks diary entries of the type in question.  ABSFUNC is a
function that converts absolute dates to dates of the appropriate type.  "
  (let ((dayname (diary-name-pattern calendar-day-name-array
                                     calendar-day-abbrev-array))
        (monthname (format "%s\\|\\*"
                           (if months
                               (diary-name-pattern months)
                             (diary-name-pattern calendar-month-name-array
                                                 calendar-month-abbrev-array))))
        (month "[0-9]+\\|\\*")
        (day "[0-9]+\\|\\*")
        (year "[0-9]+\\|\\*")
        (case-fold-search t)
        marks)
    (dolist (date-form diary-date-forms)
      (if (eq (car date-form) 'backup)  ; ignore 'backup directive
          (setq date-form (cdr date-form)))
      (let* ((l (length date-form))
             (d-name-pos (- l (length (memq 'dayname date-form))))
             (d-name-pos (if (/= l d-name-pos) (1+ d-name-pos)))
             (m-name-pos (- l (length (memq 'monthname date-form))))
             (m-name-pos (if (/= l m-name-pos) (1+ m-name-pos)))
             (d-pos (- l (length (memq 'day date-form))))
             (d-pos (if (/= l d-pos) (1+ d-pos)))
             (m-pos (- l (length (memq 'month date-form))))
             (m-pos (if (/= l m-pos) (1+ m-pos)))
             (y-pos (- l (length (memq 'year date-form))))
             (y-pos (if (/= l y-pos) (1+ y-pos)))
             (regexp (format "^%s\\(%s\\)"
                             (if symbol (regexp-quote symbol) "")
                             (mapconcat 'eval date-form "\\)\\("))))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let* ((dd-name
                  (if d-name-pos
                      (match-string-no-properties d-name-pos)))
                 (mm-name
                  (if m-name-pos
                      (match-string-no-properties m-name-pos)))
                 (mm (string-to-number
                      (if m-pos
                          (match-string-no-properties m-pos)
                        "")))
                 (dd (string-to-number
                      (if d-pos
                          (match-string-no-properties d-pos)
                        "")))
                 (y-str (if y-pos
                            (match-string-no-properties y-pos)))
                 (yy (if (not y-str)
                         0
                       (if (and (= (length y-str) 2)
                                diary-abbreviated-year-flag)
                           (let* ((current-y
                                   (calendar-extract-year
                                    (if absfunc
                                        (funcall
                                         absfunc
                                         (calendar-absolute-from-gregorian
                                          (calendar-current-date)))
                                      (calendar-current-date))))
                                  (y (+ (string-to-number y-str)
                                        ;; Current century, eg 2000.
                                        (* 100 (/ current-y 100))))
                                  (offset (- y current-y)))
                             ;; Add 2-digit year to current century.
                             ;; If more than 50 years in the future,
                             ;; assume last century. If more than 50
                             ;; years in the past, assume next century.
                             (if (> offset 50)
                                 (- y 100)
                               (if (< offset -50)
                                   (+ y 100)
                                 y)))
                         (string-to-number y-str)))))
            (setq marks (cadr (diary-pull-attrs
                               (buffer-substring-no-properties
                                (point) (line-end-position))
                               file-glob-attrs)))
            ;; Only mark all days of a given name if the pattern
            ;; contains no more specific elements.
            (if (and dd-name (not (or d-pos m-pos y-pos)))
                (calendar-mark-days-named
                 (cdr (assoc-string dd-name
                                    (calendar-make-alist
                                     calendar-day-name-array
                                     0 nil calendar-day-abbrev-array
                                     (mapcar (lambda (e)
                                               (format "%s." e))
                                             calendar-day-abbrev-array))
                                    t)) marks)
              (if mm-name
                  (setq mm
                        (if (string-equal mm-name "*") 0
                          (cdr (assoc-string
                                mm-name
                                (if months (calendar-make-alist months)
                                  (calendar-make-alist
                                   calendar-month-name-array
                                   1 nil calendar-month-abbrev-array
                                   (mapcar (lambda (e)
                                             (format "%s." e))
                                           calendar-month-abbrev-array)))
                                t)))))
              (funcall markfunc mm dd yy marks))))))))

;;;###cal-autoload
(defun diary-mark-entries (&optional redraw)
  "Mark days in the calendar window that have diary entries.
Marks each entry in the diary that is visible in the calendar window.

After marking the entries, runs `diary-nongregorian-marking-hook'
for the main diary file, and each included file.  For example,
this is the appropriate hook to process Islamic entries in all
diary files.  Next `diary-mark-entries-hook' runs, for the main diary
file only.  If not using include files, there is no difference between
these two hooks.

If the optional argument REDRAW is non-nil (which is the case
interactively, for example) then this first removes any existing diary
marks.  This is intended to deal with deleted diary entries."
  (interactive "p")
  ;; To remove any deleted diary entries. Do not redraw when:
  ;; i) processing #include diary files (else only get the marks from
  ;; the last #include file processed).
  ;; ii) called via calendar-redraw (since calendar has already been
  ;; erased).
  ;; Use of REDRAW handles both of these cases.
  (when (and redraw calendar-mark-diary-entries-flag)
    (setq calendar-mark-diary-entries-flag nil)
    (calendar-redraw))
  (let ((diary-marking-entries-flag t)
        (diary-buffer (find-buffer-visiting diary-file))
        ;; Dynamically bound in diary-include-files.
        (d-incp (and (boundp 'diary-including) diary-including))
        file-glob-attrs temp-buff)
    (unless d-incp
      (setq diary-included-files nil)
      (message "Marking diary entries..."))
    (unwind-protect
        (with-current-buffer (or diary-buffer
                                 (if d-incp
                                     (setq temp-buff (generate-new-buffer
                                                        " *diary-temp*"))
                                   (find-file-noselect
                                    (diary-check-diary-file) t)))
          (if temp-buff
              ;; If including, caller has already verified it is readable.
              (insert-file-contents diary-file)
            (if (eq major-mode (default-value 'major-mode)) (diary-mode)))
          (setq calendar-mark-diary-entries-flag t)
          (setq file-glob-attrs (nth 1 (diary-pull-attrs nil '())))
          (with-syntax-table diary-syntax-table
            (save-excursion
              (diary-mark-entries-1 'calendar-mark-date-pattern)
              (diary-mark-sexp-entries)
              ;; Although it looks like mark-entries-hook runs every time,
              ;; diary-mark-included-diary-files binds it to nil
              ;; (essentially) when it runs in included files.
              (run-hooks 'diary-nongregorian-marking-hook
                         'diary-mark-entries-hook))))
      (and temp-buff (buffer-name temp-buff) (kill-buffer temp-buff)))
    (or d-incp (message "Marking diary entries...done"))))

;;;###cal-autoload
(define-obsolete-function-alias 'mark-diary-entries 'diary-mark-entries "23.1")

(defun diary-sexp-entry (sexp entry date)
  "Process a SEXP diary ENTRY for DATE."
  (let ((result (if calendar-debug-sexp
                    (let ((debug-on-error t))
                      (eval (car (read-from-string sexp))))
                  (condition-case nil
                      (eval (car (read-from-string sexp)))
                    (error
                     (beep)
                     (message "Bad sexp at line %d in %s: %s"
                              (count-lines (point-min) (point))
                              diary-file sexp)
                     (sleep-for 2))))))
    (cond ((stringp result) result)
          ((and (consp result)
                (stringp (cdr result))) result)
          (result entry)
          (t nil))))

(defvar displayed-year)                 ; bound in calendar-generate
(defvar displayed-month)

(defun diary-mark-sexp-entries ()
  "Mark days in the calendar window that have sexp diary entries.
Each entry in the diary file (or included files) visible in the calendar window
is marked.  See the documentation for the function `diary-list-sexp-entries'."
  (let* ((sexp-mark (regexp-quote diary-sexp-entry-symbol))
         (s-entry (format "^\\(%s(\\)\\|\\(%s%s(diary-remind\\)" sexp-mark
                          (regexp-quote diary-nonmarking-symbol)
                          sexp-mark))
         (file-glob-attrs (nth 1 (diary-pull-attrs nil '())))
         m y first-date last-date date mark file-glob-attrs
         sexp-start sexp entry entry-start)
    (with-current-buffer calendar-buffer
      (setq m displayed-month
            y displayed-year))
    (calendar-increment-month m y -1)
    (setq first-date (calendar-absolute-from-gregorian (list m 1 y)))
    (calendar-increment-month m y 2)
    (setq last-date
          (calendar-absolute-from-gregorian
           (list m (calendar-last-day-of-month m y) y)))
    (goto-char (point-min))
    (while (re-search-forward s-entry nil t)
      (setq diary-marking-entry-flag (char-equal (preceding-char) ?\())
      (re-search-backward "(")
      (setq sexp-start (point))
      (forward-sexp)
      (setq sexp (buffer-substring-no-properties sexp-start (point)))
      (forward-char 1)
      (if (and (bolp) (not (looking-at "[ \t]")))
          ;; Diary entry consists only of the sexp.
          (progn
            (backward-char 1)
            (setq entry ""))
        (setq entry-start (point))
        ;; Find end of entry.
        (forward-line 1)
        (while (looking-at "[ \t]")
          (forward-line 1))
        (if (bolp) (backward-char 1))
        (setq entry (buffer-substring-no-properties entry-start (point))))
      (setq date (1- first-date))
      ;; FIXME this loops over all visible dates.
      ;; Could be optimized in many cases. Depends on whether t or * present.
      (while (<= (setq date (1+ date)) last-date)
        (when (setq mark (diary-sexp-entry
                          sexp entry
                          (calendar-gregorian-from-absolute date)))
          (calendar-mark-visible-date
           (calendar-gregorian-from-absolute date)
           (or (cadr (diary-pull-attrs entry file-glob-attrs))
               (if (consp mark) (car mark)))))))))

(define-obsolete-function-alias 'mark-sexp-diary-entries
  'diary-mark-sexp-entries "23.1")

(defun diary-mark-included-diary-files ()
  "Mark diary entries from included diary files.
To use, add this function to `diary-mark-entries-hook'.
For details, see `diary-include-files'.
See also `diary-include-other-diary-files'."
  (diary-include-files t))

(define-obsolete-function-alias 'mark-included-diary-files
  'diary-mark-included-diary-files "23.1")

(defun calendar-mark-days-named (dayname &optional color)
  "Mark all dates in the calendar window that are day DAYNAME of the week.
0 means all Sundays, 1 means all Mondays, and so on.
Optional argument COLOR is passed to `calendar-mark-visible-date' as MARK."
  (with-current-buffer calendar-buffer
    (let ((prev-month displayed-month)
          (prev-year displayed-year)
          (succ-month displayed-month)
          (succ-year displayed-year)
          (last-day)
          (day))
      (calendar-increment-month succ-month succ-year 1)
      (calendar-increment-month prev-month prev-year -1)
      (setq day (calendar-absolute-from-gregorian
                 (calendar-nth-named-day 1 dayname prev-month prev-year))
            last-day (calendar-absolute-from-gregorian
                      (calendar-nth-named-day -1 dayname succ-month succ-year)))
      (while (<= day last-day)
        (calendar-mark-visible-date (calendar-gregorian-from-absolute day)
                                    color)
        (setq day (+ day 7))))))

(define-obsolete-function-alias 'mark-calendar-days-named
  'calendar-mark-days-named "23.1")

(defun calendar-mark-month (month year p-month p-day p-year &optional color)
  "Mark dates in the MONTH/YEAR that conform to pattern P-MONTH/P-DAY/P-YEAR.
A value of 0 in any position of the pattern is a wildcard.
Optional argument COLOR is passed to `calendar-mark-visible-date' as MARK."
  (if (or (and (= month p-month)
               (or (zerop p-year) (= year p-year)))
          (and (zerop p-month)
               (or (zerop p-year) (= year p-year))))
      (if (zerop p-day)
          (dotimes (i (calendar-last-day-of-month month year))
            (calendar-mark-visible-date (list month (1+ i) year) color))
        (calendar-mark-visible-date (list month p-day year) color))))

(define-obsolete-function-alias 'mark-calendar-month
  'calendar-mark-month "23.1")

(defun calendar-mark-date-pattern (month day year &optional color)
  "Mark all dates in the calendar window that conform to MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard.  Optional argument COLOR is
passed to `calendar-mark-visible-date' as MARK."
  (with-current-buffer calendar-buffer
    (let ((m displayed-month)
          (y displayed-year))
      (calendar-increment-month m y -1)
      (dotimes (_idummy 3)
        (calendar-mark-month m y month day year color)
        (calendar-increment-month m y 1)))))

(define-obsolete-function-alias 'mark-calendar-date-pattern
  'calendar-mark-date-pattern "23.1")

;; Bahai, Hebrew, Islamic.
(defun calendar-mark-complex (month day year fromabs &optional color)
  "Mark dates in the calendar conforming to MONTH DAY YEAR of some system.
The function FROMABS converts absolute dates to the appropriate date system.
Optional argument COLOR is passed to `calendar-mark-visible-date' as MARK."
  ;; Not one of the simple cases--check all visible dates for match.
  ;; Actually, the following code takes care of ALL of the cases, but
  ;; it's much too slow to be used for the simple (common) cases.
  (let* ((m displayed-month)
         (y displayed-year)
         (first-date (progn
                       (calendar-increment-month m y -1)
                       (calendar-absolute-from-gregorian (list m 1 y))))
         (last-date (progn
                      (calendar-increment-month m y 2)
                      (calendar-absolute-from-gregorian
                       (list m (calendar-last-day-of-month m y) y))))
         (date (1- first-date))
         local-date)
    (while (<= (setq date (1+ date)) last-date)
      (setq local-date (funcall fromabs date))
      (and (or (zerop month)
               (= month (calendar-extract-month local-date)))
           (or (zerop day)
               (= day (calendar-extract-day local-date)))
           (or (zerop year)
               (= year (calendar-extract-year local-date)))
           (calendar-mark-visible-date
            (calendar-gregorian-from-absolute date) color)))))

;; Bahai, Islamic.
(defun calendar-mark-1 (month day year fromabs toabs &optional color)
  "Mark dates in the calendar conforming to MONTH DAY YEAR of some system.
The function FROMABS converts absolute dates to the appropriate date system.
The function TOABS carries out the inverse operation.  Optional argument
COLOR is passed to `calendar-mark-visible-date' as MARK."
  (with-current-buffer calendar-buffer
    (if (and (not (zerop month)) (not (zerop day)))
        (if (not (zerop year))
            ;; Fully specified date.
            (let ((date (calendar-gregorian-from-absolute
                         (funcall toabs (list month day year)))))
              (if (calendar-date-is-visible-p date)
                  (calendar-mark-visible-date date color)))
          ;; Month and day in any year--this taken from the holiday stuff.
          (let* ((i-date (funcall fromabs
                                  (calendar-absolute-from-gregorian
                                   (list displayed-month 15 displayed-year))))
                 (m (calendar-extract-month i-date))
                 (y (calendar-extract-year i-date))
                 date)
            (unless (< m 1)             ; calendar doesn't apply
              (calendar-increment-month m y (- 10 month))
              (and (> m 7)              ; date might be visible
                   (calendar-date-is-visible-p
                    (setq date (calendar-gregorian-from-absolute
                                (funcall toabs (list month day y)))))
                   (calendar-mark-visible-date date color)))))
      (calendar-mark-complex month day year
                             'calendar-bahai-from-absolute color))))


(defun diary-entry-time (s)
  "Return time at the beginning of the string S as a military-style integer.
For example, returns 1325 for 1:25pm.

Returns `diary-unknown-time' (default value -9999) if no time is recognized.
The recognized forms are XXXX, X:XX, or XX:XX (military time), and XXam,
XXAM, XXpm, XXPM, XX:XXam, XX:XXAM, XX:XXpm, or XX:XXPM.  A period (.) can
be used instead of a colon (:) to separate the hour and minute parts."
  (let (case-fold-search)
    (cond ((string-match                ; military time
            "\\`[ \t\n]*\\([0-9]?[0-9]\\)[:.]?\\([0-9][0-9]\\)\\(\\>\\|[^ap]\\)"
            s)
           (+ (* 100 (string-to-number (match-string 1 s)))
              (string-to-number (match-string 2 s))))
          ((string-match                ; hour only (XXam or XXpm)
            "\\`[ \t\n]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
           (+ (* 100 (% (string-to-number (match-string 1 s)) 12))
              (if (equal ?a (downcase (aref s (match-beginning 2))))
                  0 1200)))
          ((string-match        ; hour and minute (XX:XXam or XX:XXpm)
            "\\`[ \t\n]*\\([0-9]?[0-9]\\)[:.]\\([0-9][0-9]\\)\\([ap]\\)m\\>" s)
           (+ (* 100 (% (string-to-number (match-string 1 s)) 12))
              (string-to-number (match-string 2 s))
              (if (equal ?a (downcase (aref s (match-beginning 3))))
                  0 1200)))
          (t diary-unknown-time))))     ; unrecognizable

(defun diary-entry-compare (e1 e2)
  "Return t if E1 is earlier than E2."
  (or (calendar-date-compare e1 e2)
      (and (calendar-date-equal (car e1) (car e2))
           (let* ((ts1 (cadr e1)) (t1 (diary-entry-time ts1))
                  (ts2 (cadr e2)) (t2 (diary-entry-time ts2)))
             (or (< t1 t2)
                 (and (= t1 t2)
                      (string-lessp ts1 ts2)))))))

(defun diary-sort-entries ()
  "Sort the list of diary entries by time of day.
If you add this function to `diary-list-entries-hook', it should
be the last item in the hook, in case earlier items add diary
entries, or change the order."
  (setq diary-entries-list (sort diary-entries-list 'diary-entry-compare)))

(define-obsolete-function-alias 'sort-diary-entries 'diary-sort-entries "23.1")


(defun diary-list-sexp-entries (date)
  "Add sexp entries for DATE from the diary file to `diary-entries-list'.
Also, make them visible in the diary.  Returns t if any entries are found.

Sexp diary entries must be prefaced by a `diary-sexp-entry-symbol'
\(normally `%%').  The form of a sexp diary entry is

                  %%(SEXP) ENTRY

Both ENTRY and DATE are available when the SEXP is evaluated.  If
the SEXP returns nil, the diary entry does not apply.  If it
returns a non-nil value, ENTRY will be taken to apply to DATE; if
the value is a string, that string will be the diary entry in the
fancy diary display.

For example, the following diary entry will apply to the 21st of
the month if it is a weekday and the Friday before if the 21st is
on a weekend:

      &%%(let ((dayname (calendar-day-of-week date))
               (day (calendar-extract-day date)))
           (or
             (and (= day 21) (memq dayname '(1 2 3 4 5)))
             (and (memq day '(19 20)) (= dayname 5)))
         ) UIUC pay checks deposited

A number of built-in functions are available for this type of
diary entry.  In the following, the optional parameter MARK
specifies a face or single-character string to use when
highlighting the day in the calendar.  For those functions that
take MONTH, DAY, and YEAR as arguments, the order of the input
parameters changes according to `calendar-date-style' (e.g. to
DAY MONTH YEAR in the European style).

  %%(diary-date MONTH DAY YEAR &optional MARK) text
    Entry applies if date is MONTH, DAY, YEAR.  DAY, MONTH, and YEAR can
    be a list of integers, `t' (meaning all values), or an integer.

  %%(diary-float MONTH DAYNAME N &optional DAY MARK) text
    Entry will appear on the Nth DAYNAME after/before MONTH DAY.
    DAYNAME=0 means Sunday, DAYNAME=1 means Monday, and so on.
    If N>0, use the Nth DAYNAME after MONTH DAY.
    If N<0, use the Nth DAYNAME before MONTH DAY.
    DAY defaults to 1 if N>0, and MONTH's last day otherwise.
    MONTH can be a list of months, a single month, or `t' to
    specify all months.

  %%(diary-block M1 D1 Y1 M2 D2 Y2 &optional MARK) text
    Entry will appear on dates between M1/D1/Y1 and M2/D2/Y2,
    inclusive.

  %%(diary-anniversary MONTH DAY YEAR &optional MARK) text
    Entry will appear on anniversary dates of MONTH DAY, YEAR.
    Text can contain `%d' or `%d%s'; `%d' will be replaced by the
    number of years since the MONTH DAY, YEAR, and `%s' by the
    ordinal ending of that number (i.e. `st', `nd', `rd' or `th',
    as appropriate).  The anniversary of February 29 is
    considered to be March 1 in a non-leap year.

  %%(diary-cyclic N MONTH DAY YEAR &optional MARK) text
    Entry will appear every N days, starting MONTH DAY, YEAR.
    Text can contain `%d' or `%d%s'; `%d' will be replaced by the
    number of repetitions since the MONTH DAY, YEAR and `%s' by
    the ordinal ending of that number (i.e. `st', `nd', `rd' or
    `th', as appropriate).

  %%(diary-remind SEXP DAYS &optional MARKING) text
    Entry is a reminder for diary sexp SEXP.  DAYS is either a
    single number or a list of numbers indicating the number(s)
    of days before the event that the warning(s) should occur.
    A negative number -DAYS has the same meaning as a list (1 2 ... DAYS).
    If the current date is (one of) DAYS before the event indicated
    by EXPR, then a suitable message (as specified by
    `diary-remind-message') appears.  In addition to the
    reminders beforehand, the diary entry also appears on the
    date itself.  If optional MARKING is non-nil then the
    *reminders* are marked on the calendar.  Marking of reminders
    is independent of whether the entry *itself* is a marking or
    non-marking one.

  %%(diary-hebrew-yahrzeit MONTH DAY YEAR) text
    Text is assumed to be the name of the person; the date is the
    date of death on the *civil* calendar.  The diary entry will
    appear on the proper Hebrew-date anniversary and on the day
    before.

All the remaining functions do not accept any text, and so only
make sense with `diary-fancy-display'.  Most produce output every day.

`diary-day-of-year'      - day of year and number of days remaining
`diary-iso-date'         - ISO commercial date
`diary-astro-day-number' - astronomical (Julian) day number
`diary-sunrise-sunset'   - local times of sunrise and sunset

These functions give the date in alternative calendrical systems:

`diary-bahai-date', `diary-chinese-date', `diary-coptic-date',
`diary-ethiopic-date', `diary-french-date', `diary-hebrew-date',
`diary-islamic-date', `diary-julian-date', `diary-mayan-date',
`diary-persian-date'

Theses functions only produce output on certain dates:

`diary-lunar-phases'           - phases of moon (on the appropriate days)
`diary-hebrew-omer'            - Omer count, within 50 days after Passover
`diary-hebrew-parasha'         - weekly parasha, every Saturday
`diary-hebrew-rosh-hodesh'     - Rosh Hodesh, or the day or Saturday before
`diary-hebrew-sabbath-candles' - local time of candle lighting, on Fridays


Marking these entries is *extremely* time consuming, so it is
best if they are non-marking."
  (let ((s-entry (format "^%s?%s(" (regexp-quote diary-nonmarking-symbol)
                         (regexp-quote diary-sexp-entry-symbol)))
        entry-found file-glob-attrs marks
        sexp-start sexp entry specifier entry-start line-start
        diary-entry temp literal)
    (goto-char (point-min))
    (setq file-glob-attrs (nth 1 (diary-pull-attrs nil '())))
    (while (re-search-forward s-entry nil t)
      (backward-char 1)
      (setq sexp-start (point))
      (forward-sexp)
      (setq sexp (buffer-substring-no-properties sexp-start (point))
            line-start (line-end-position 0)
            specifier
            (buffer-substring-no-properties (1+ line-start) (point))
            entry-start (1+ line-start))
      (forward-char 1)
      (if (and (bolp) (not (looking-at "[ \t]")))
          ;; Diary entry consists only of the sexp.
          (progn
            (backward-char 1)
            (setq entry ""))
        (setq entry-start (point))
        (forward-line 1)
        (while (looking-at "[ \t]")
          (forward-line 1))
        (if (bolp) (backward-char 1))
        (setq entry (buffer-substring-no-properties entry-start (point))))
      (setq diary-entry (diary-sexp-entry sexp entry date)
            literal entry               ; before evaluation
            entry (if (consp diary-entry)
                      (cdr diary-entry)
                    diary-entry))
      (when diary-entry
        (remove-overlays line-start (point) 'invisible 'diary)
        (if (< 0 (length entry))
            (setq temp (diary-pull-attrs entry file-glob-attrs)
                  entry (nth 0 temp)
                  marks (nth 1 temp))))
      (diary-add-to-list date entry specifier
                         (if entry-start (copy-marker entry-start))
                         marks literal)
      (setq entry-found (or entry-found diary-entry)))
    entry-found))

(define-obsolete-function-alias 'list-sexp-diary-entries
  'diary-list-sexp-entries "23.1")

(defun diary-make-date (a b c)
  "Convert A B C into the internal calendar date form.
The expected order of the inputs depends on `calendar-date-style',
e.g. in the European case, A = day, B = month, C = year.  Returns
a list (MONTH DAY YEAR), i.e. the American style, which is the
form used internally by the calendar and diary."
  (cond ((eq calendar-date-style 'iso)  ; YMD
         (list b c a))
        ((eq calendar-date-style 'european) ; DMY
         (list b a c))
        (t (list a b c))))


;;; Sexp diary functions.

(defvar date)
(defvar entry)

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(defun diary-date (month day year &optional mark)
  "Specific date(s) diary entry.
Entry applies if date is MONTH, DAY, YEAR.  Each parameter can be a
list of integers, `t' (meaning all values), or an integer.  The order
of the input parameters changes according to `calendar-date-style'
\(e.g. to DAY MONTH YEAR in the European style).

An optional parameter MARK specifies a face or single-character string
to use when highlighting the day in the calendar."
  (let* ((ddate (diary-make-date month day year))
         (dd (calendar-extract-day ddate))
         (mm (calendar-extract-month ddate))
         (yy (calendar-extract-year ddate))
         (m (calendar-extract-month date))
         (y (calendar-extract-year date))
         (d (calendar-extract-day date)))
    (and
     (or (and (listp dd) (memq d dd))
         (equal d dd)
         (eq dd t))
     (or (and (listp mm) (memq m mm))
         (equal m mm)
         (eq mm t))
     (or (and (listp yy) (memq y yy))
         (equal y yy)
         (eq yy t))
     (cons mark entry))))

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(defun diary-block (m1 d1 y1 m2 d2 y2 &optional mark)
  "Block diary entry.
Entry applies if date is between, or on one of, two dates.  The order
of the input parameters changes according to `calendar-date-style'
\(e.g. to D1, M1, Y1, D2, M2, Y2 in the European style).

An optional parameter MARK specifies a face or single-character string
to use when highlighting the day in the calendar."
  (let ((date1 (calendar-absolute-from-gregorian
                (diary-make-date m1 d1 y1)))
        (date2 (calendar-absolute-from-gregorian
                (diary-make-date m2 d2 y2)))
        (d (calendar-absolute-from-gregorian date)))
    (and (<= date1 d) (<= d date2)
         (cons mark entry))))

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(defun diary-float (month dayname n &optional day mark)
  "Diary entry for the Nth DAYNAME after/before MONTH DAY.
DAYNAME=0 means Sunday, DAYNAME=1 means Monday, and so on.
If N>0, use the Nth DAYNAME after MONTH DAY.
If N<0, use the Nth DAYNAME before MONTH DAY.
DAY defaults to 1 if N>0, and MONTH's last day otherwise.
MONTH can be a list of months, an integer, or `t' (meaning all months).
Optional MARK specifies a face or single-character string to use when
highlighting the day in the calendar."
  ;; This is messy because the diary entry may apply, but the date on which it
  ;; is based can be in a different month/year.  For example, asking for the
  ;; first Monday after December 30.  For large values of |n| the problem is
  ;; more grotesque.
  (and (= dayname (calendar-day-of-week date))
       (let* ((m (calendar-extract-month date))
              (d (calendar-extract-day date))
              (y (calendar-extract-year date))
              ;; Last (n>0) or first (n<0) possible base date for entry.
              (limit
               (calendar-nth-named-absday (- n) dayname m y d))
              (last-abs (if (> n 0) limit (+ limit 6)))
              (first-abs (if (> n 0) (- limit 6) limit))
              (last (calendar-gregorian-from-absolute last-abs))
              (first (calendar-gregorian-from-absolute first-abs))
              ;; m1, d1 is first possible base date.
              (m1 (calendar-extract-month first))
              (d1 (calendar-extract-day first))
              (y1 (calendar-extract-year first))
              ;; m2, d2 is last possible base date.
              (m2 (calendar-extract-month last))
              (d2 (calendar-extract-day last))
              (y2 (calendar-extract-year last)))
         (if (or (and (= m1 m2) ; only possible base dates in one month
                      (or (eq month t)
                          (if (listp month)
                              (memq m1 month)
                            (= m1 month)))
                      (let ((d (or day (if (> n 0)
                                           1
                                         (calendar-last-day-of-month m1 y1)))))
                        (and (<= d1 d) (<= d d2))))
                 ;; Only possible base dates straddle two months.
                 (and (or (< y1 y2)
                          (and (= y1 y2) (< m1 m2)))
                      (or
                       ;; m1, d1 works as a base date.
                       (and
                        (or (eq month t)
                            (if (listp month)
                                (memq m1 month)
                              (= m1 month)))
                        (<= d1 (or day (if (> n 0)
                                           1
                                         (calendar-last-day-of-month m1 y1)))))
                       ;; m2, d2 works as a base date.
                       (and (or (eq month t)
                                (if (listp month)
                                    (memq m2 month)
                                  (= m2 month)))
                            (<= (or day (if (> n 0)
                                            1
                                          (calendar-last-day-of-month m2 y2)))
                                d2)))))
             (cons mark entry)))))

(defun diary-ordinal-suffix (n)
  "Ordinal suffix for N. (That is, `st', `nd', `rd', or `th', as appropriate.)"
  (if (or (memq (% n 100) '(11 12 13))
          (< 3 (% n 10)))
      "th"
    (aref ["th" "st" "nd" "rd"] (% n 10))))

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(defun diary-anniversary (month day &optional year mark)
  "Anniversary diary entry.
Entry applies if date is the anniversary of MONTH, DAY, YEAR.
The order of the input parameters changes according to
`calendar-date-style' (e.g. to DAY MONTH YEAR in the European style).

The diary entry can contain `%d' or `%d%s'; the %d will be replaced
by the number of years since the MONTH, DAY, YEAR, and the %s will
be replaced by the ordinal ending of that number (that is, `st',
`nd', `rd' or `th', as appropriate).  The anniversary of February 29
is considered to be March 1 in non-leap years.

An optional parameter MARK specifies a face or single-character
string to use when highlighting the day in the calendar."
  (let* ((ddate (diary-make-date month day year))
         (dd (calendar-extract-day ddate))
         (mm (calendar-extract-month ddate))
         (yy (calendar-extract-year ddate))
         (y (calendar-extract-year date))
         (diff (if yy (- y yy) 100)))
    (and (= mm 2) (= dd 29) (not (calendar-leap-year-p y))
         (setq mm 3
               dd 1))
    (and (> diff 0) (calendar-date-equal (list mm dd y) date)
         (cons mark (format entry diff (diary-ordinal-suffix diff))))))

;; To be called from diary-sexp-entry, where DATE, ENTRY are bound.
(defun diary-cyclic (n month day year &optional mark)
  "Cycle diary entry--entry applies every N days starting at MONTH, DAY, YEAR.
The order of the input parameters changes according to
`calendar-date-style' (e.g. to N DAY MONTH YEAR in the European
style).  The entry can contain `%d' or `%d%s'; the %d will be
replaced by the number of repetitions since the MONTH DAY YEAR,
and %s by the ordinal ending of that number (that is, `st', `nd',
`rd' or `th', as appropriate).

An optional parameter MARK specifies a face or single-character
string to use when highlighting the day in the calendar."
  (or (> n 0)
      (error "Day count must be positive"))
  (let* ((diff (- (calendar-absolute-from-gregorian date)
                  (calendar-absolute-from-gregorian
                   (diary-make-date month day year))))
         (cycle (/ diff n)))
    (and (>= diff 0) (zerop (% diff n))
         (cons mark (format entry cycle (diary-ordinal-suffix cycle))))))

(defun diary-day-of-year ()
  "Day of year and number of days remaining in the year of date diary entry."
  (calendar-day-of-year-string date))

(defun diary-remind (sexp days &optional marking)
  "Provide a reminder of a diary entry.
SEXP is a diary-sexp.  DAYS is either a single number or a list
of numbers indicating the number(s) of days before the event that
the warning(s) should occur on.  A negative number -DAYS has the
same meaning as a list (1 2 ... DAYS).  If the current date
is (one of) DAYS before the event indicated by SEXP, then this function
returns a suitable message (as specified by `diary-remind-message').

In addition to the reminders beforehand, the diary entry also
appears on the date itself.

A `diary-nonmarking-symbol' at the beginning of the line of the
`diary-remind' entry specifies that the diary entry (not the
reminder) is non-marking.  Marking of reminders is independent of
whether the entry itself is a marking or nonmarking; if optional
parameter MARKING is non-nil then the reminders are marked on the
calendar."
  ;; `date' has a value at this point, from diary-sexp-entry.
  ;; Convert a negative number to a list of days.
  (and (integerp days)
       (< days 0)
       (setq days (number-sequence 1 (- days))))
  (let ((diary-entry (eval sexp)))
    (cond
     ;; Diary entry applies on date.
     ((and diary-entry
           (or (not diary-marking-entries-flag) diary-marking-entry-flag))
      diary-entry)
     ;; Diary entry may apply to `days' before date.
     ((and (integerp days)
           (not diary-entry)      ; diary entry does not apply to date
           (or (not diary-marking-entries-flag) marking))
      ;; Adjust date, and re-evaluate.
      (let ((date (calendar-gregorian-from-absolute
                   (+ (calendar-absolute-from-gregorian date) days))))
        (when (setq diary-entry (eval sexp))
          ;; Discard any mark portion from diary-anniversary, etc.
          (if (consp diary-entry) (setq diary-entry (cdr diary-entry)))
          (mapconcat 'eval diary-remind-message ""))))
     ;; Diary entry may apply to one of a list of days before date.
     ((and (listp days) days)
      (or (diary-remind sexp (car days) marking)
          (diary-remind sexp (cdr days) marking))))))


;;; Diary insertion functions.

;;;###cal-autoload
(defun diary-make-entry (string &optional nonmarking file)
  "Insert a diary entry STRING which may be NONMARKING in FILE.
If omitted, NONMARKING defaults to nil and FILE defaults to
`diary-file'."
  (let ((pop-up-frames (or pop-up-frames
                           (window-dedicated-p (selected-window)))))
    (find-file-other-window (or file diary-file)))
  (when (eq major-mode (default-value 'major-mode)) (diary-mode))
  (widen)
  (diary-unhide-everything)
  (goto-char (point-max))
  (when (let ((case-fold-search t))
          (search-backward "Local Variables:"
                           (max (- (point-max) 3000) (point-min))
                           t))
    (beginning-of-line)
    (insert "\n")
    (forward-line -1))
  (insert
   (if (bolp) "" "\n")
   (if nonmarking diary-nonmarking-symbol "")
   string " "))

;;;###cal-autoload
(define-obsolete-function-alias 'make-diary-entry 'diary-make-entry "23.1")

;;;###cal-autoload
(defun diary-insert-entry (arg &optional event)
  "Insert a diary entry for the date indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (diary-make-entry (calendar-date-string (calendar-cursor-to-date t event) t t)
                    arg))

;;;###cal-autoload
(define-obsolete-function-alias 'insert-diary-entry 'diary-insert-entry "23.1")

;;;###cal-autoload
(defun diary-insert-weekly-entry (arg)
  "Insert a weekly diary entry for the day of the week indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-make-entry (calendar-day-name (calendar-cursor-to-date t))
                    arg))

;;;###cal-autoload
(define-obsolete-function-alias 'insert-weekly-diary-entry
  'diary-insert-weekly-entry "23.1")

(defun diary-date-display-form (&optional type)
  "Return value for `calendar-date-display-form' using `calendar-date-style'.
Optional symbol TYPE is either `monthly' or `yearly'."
  (cond ((eq type 'monthly) (cond ((eq calendar-date-style 'iso)
                                   '((format "*-*-%.2d"
                                             (string-to-number day))))
                                  ((eq calendar-date-style 'european)
                                   '(day " * "))
                                  (t '("* " day ))))
        ((eq type 'yearly) (cond ((eq calendar-date-style 'iso)
                                  '((format "*-%.2d-%.2d"
                                            (string-to-number month)
                                            (string-to-number day))))
                                 ((eq calendar-date-style 'european)
                                  '(day " " monthname))
                                 (t '(monthname " " day))))
        ;; Iso cannot contain "-", because this form used eg by
        ;; diary-insert-anniversary-entry.
        (t (cond ((eq calendar-date-style 'iso)
                 '((format "%s %.2d %.2d" year
                           (string-to-number month) (string-to-number day))))
                 ((eq calendar-date-style 'european)
                  '(day " " month " " year))
                 (t '(month " " day " " year))))))

(defun diary-insert-entry-1 (&optional type nomark months symbol absfunc)
  "Subroutine to insert a diary entry related to the date at point.
TYPE is the type of entry (`monthly' or `yearly').  NOMARK non-nil
means make the entry non-marking.  Array MONTHS is used in place
of `calendar-month-name-array'.  String SYMBOL marks the type of
diary entry.  Function ABSFUNC converts absolute dates to dates of
the appropriate type."
  (let ((calendar-date-display-form (if type
                                        (diary-date-display-form type)
                                      calendar-date-display-form))
        (calendar-month-name-array (or months calendar-month-name-array))
        (date (calendar-cursor-to-date t)))
    (diary-make-entry
     (format "%s%s" (or symbol "")
             (calendar-date-string
              (if absfunc
                  (funcall absfunc (calendar-absolute-from-gregorian date))
                date)
              (not absfunc)
              (not type)))
     nomark)))

;;;###cal-autoload
(defun diary-insert-monthly-entry (arg)
  "Insert a monthly diary entry for the day of the month indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 'monthly arg))

;;;###cal-autoload
(define-obsolete-function-alias 'insert-monthly-diary-entry
  'diary-insert-monthly-entry "23.1")

;;;###cal-autoload
(defun diary-insert-yearly-entry (arg)
  "Insert an annual diary entry for the day of the year indicated by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (diary-insert-entry-1 'yearly arg))

;;;###cal-autoload
(define-obsolete-function-alias 'insert-yearly-diary-entry
  'diary-insert-yearly-entry "23.1")

;;;###cal-autoload
(defun diary-insert-anniversary-entry (arg)
  "Insert an anniversary diary entry for the date given by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form (diary-date-display-form)))
    (diary-make-entry
     (format "%s(diary-anniversary %s)"
             diary-sexp-entry-symbol
             (calendar-date-string (calendar-cursor-to-date t) nil t))
     arg)))

;;;###cal-autoload
(define-obsolete-function-alias 'insert-anniversary-diary-entry
  'diary-insert-anniversary-entry "23.1")

;;;###cal-autoload
(defun diary-insert-block-entry (arg)
  "Insert a block diary entry for the days between the point and marked date.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form (diary-date-display-form))
        (cursor (calendar-cursor-to-date t))
        (mark (or (car calendar-mark-ring)
                  (error "No mark set in this buffer")))
        start end)
    (if (< (calendar-absolute-from-gregorian mark)
           (calendar-absolute-from-gregorian cursor))
        (setq start mark
              end cursor)
      (setq start cursor
            end mark))
    (diary-make-entry
     (format "%s(diary-block %s %s)"
             diary-sexp-entry-symbol
             (calendar-date-string start nil t)
             (calendar-date-string end nil t))
     arg)))

;;;###cal-autoload
(define-obsolete-function-alias 'insert-block-diary-entry
  'diary-insert-block-entry "23.1")

;;;###cal-autoload
(defun diary-insert-cyclic-entry (arg)
  "Insert a cyclic diary entry starting at the date given by point.
Prefix argument ARG makes the entry nonmarking."
  (interactive "P")
  (let ((calendar-date-display-form (diary-date-display-form)))
    (diary-make-entry
     (format "%s(diary-cyclic %d %s)"
             diary-sexp-entry-symbol
             (calendar-read "Repeat every how many days: "
                            (lambda (x) (> x 0)))
             (calendar-date-string (calendar-cursor-to-date t) nil t))
     arg)))

;;;###cal-autoload
(define-obsolete-function-alias 'insert-cyclic-diary-entry
  'diary-insert-cyclic-entry "23.1")

;;; Diary mode.

(defun diary-redraw-calendar ()
  "If `calendar-buffer' is live and diary entries are marked, redraw it."
  (and calendar-mark-diary-entries-flag
       (save-excursion
         (calendar-redraw)))
  ;; Return value suitable for `write-contents-functions'.
  nil)

(defvar diary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-s" 'diary-show-all-entries)
    (define-key map "\C-c\C-q" 'quit-window)
    map)
  "Keymap for `diary-mode'.")

(defun diary-font-lock-sexps (limit)
  "Recognize sexp diary entry up to LIMIT for font-locking."
  (if (re-search-forward
       (format "^%s?\\(%s\\)" (regexp-quote diary-nonmarking-symbol)
               (regexp-quote diary-sexp-entry-symbol))
       limit t)
      (condition-case nil
          (save-restriction
            (narrow-to-region (point-min) limit)
            (let ((start (point)))
              (forward-sexp 1)
              (store-match-data (list start (point)))
              t))
        (error t))))

(defun diary-font-lock-date-forms (month-array &optional symbol abbrev-array)
  "Create font-lock patterns for `diary-date-forms' using MONTH-ARRAY.
If given, optional SYMBOL must be a prefix to entries.  If
optional ABBREV-ARRAY is present, also matches the abbreviations
from this array (with or without a final `.'), in addition to the
full month names."
  (let ((dayname (diary-name-pattern calendar-day-name-array
                                     calendar-day-abbrev-array t))
        (monthname (format "\\(%s\\|\\*\\)"
                           (diary-name-pattern month-array abbrev-array)))
        (month "\\([0-9]+\\|\\*\\)")
        (day "\\([0-9]+\\|\\*\\)")
        (year "-?\\([0-9]+\\|\\*\\)"))
    (mapcar (lambda (x)
              (cons
               (concat "^" (regexp-quote diary-nonmarking-symbol) "?"
                       (if symbol (regexp-quote symbol) "") "\\("
                       (mapconcat 'eval
                                  ;; If backup, omit first item (backup)
                                  ;; and last item (not part of date).
                                  (if (equal (car x) 'backup)
                                      (nreverse (cdr (reverse (cdr x))))
                                    x)
                                  "")
                       ;; With backup, last item is not part of date.
                       (if (equal (car x) 'backup)
                           (concat "\\)" (eval (car (reverse x))))
                         "\\)"))
               '(1 diary-face)))
            diary-date-forms)))

(defmacro diary-font-lock-keywords-1 (markfunc listfunc feature months symbol)
  "Subroutine of the function `diary-font-lock-keywords'.
If MARKFUNC is a member of `diary-nongregorian-marking-hook', or
LISTFUNC of `diary-nongregorian-listing-hook', then require FEATURE and
return a font-lock pattern matching array of MONTHS and marking SYMBOL."
  `(when (or (memq ',markfunc diary-nongregorian-marking-hook)
             (memq ',listfunc diary-nongregorian-listing-hook))
     (require ',feature)
     (diary-font-lock-date-forms ,months ,symbol)))

(defconst diary-time-regexp
  ;; Accepted formats: 10:00 10.00 10h00 10h 10am 10:00am 10.00am
  ;; Use of "." as a separator annoyingly matches numbers, eg "123.45".
  ;; Hence often prefix this with "\\(^\\|\\s-\\)."
  (concat "[0-9]?[0-9]\\([AaPp][mM]\\|\\("
          "[Hh]\\([0-9][0-9]\\)?\\|[:.][0-9][0-9]"
          "\\)\\([AaPp][Mm]\\)?\\)")
  "Regular expression matching a time of day.")

(defvar calendar-hebrew-month-name-array-leap-year)
(defvar calendar-islamic-month-name-array)
(defvar calendar-bahai-month-name-array)

;;;###cal-autoload
(defun diary-font-lock-keywords ()
  "Return a value for the variable `diary-font-lock-keywords'."
  (append
   (diary-font-lock-date-forms calendar-month-name-array
                               nil calendar-month-abbrev-array)
   (diary-font-lock-keywords-1 diary-hebrew-mark-entries
                               diary-hebrew-list-entries
                               cal-hebrew
                               calendar-hebrew-month-name-array-leap-year
                               diary-hebrew-entry-symbol)
   (diary-font-lock-keywords-1 diary-islamic-mark-entries
                               diary-islamic-list-entries
                               cal-islam
                               calendar-islamic-month-name-array
                               diary-islamic-entry-symbol)
   (diary-font-lock-keywords-1 diary-bahai-mark-entries
                               diary-bahai-list-entries
                               cal-bahai
                               calendar-bahai-month-name-array
                               diary-bahai-entry-symbol)
   (list
    (cons
     (format "^%s.*$" (regexp-quote diary-include-string))
     'font-lock-keyword-face)
    (cons
     (format "^%s?\\(%s\\)" (regexp-quote diary-nonmarking-symbol)
             (regexp-quote diary-sexp-entry-symbol))
     '(1 font-lock-reference-face))
    (cons
     (format "^%s" (regexp-quote diary-nonmarking-symbol))
     'font-lock-reference-face)
    (cons
     (format "^%s?%s" (regexp-quote diary-nonmarking-symbol)
             (regexp-opt (mapcar 'regexp-quote
                                 (list diary-hebrew-entry-symbol
                                       diary-islamic-entry-symbol
                                       diary-bahai-entry-symbol))
                         t))
     '(1 font-lock-reference-face))
    '(diary-font-lock-sexps . font-lock-keyword-face)
    ;; Don't need to worry about space around "-" because the first
    ;; match takes care of that.  It does mean the "-" itself may or
    ;; may not be fontified though.
    ;; diary-date-forms often include a final character that is not
    ;; part of the date (eg a non-digit to mark the end of the year).
    ;; This can use up the only space char between a date and time (b#7891).
    ;; Hence we use OVERRIDE, which can only override whitespace.
    ;; FIXME it's probably better to tighten up the diary-time-regexp
    ;; and drop the whitespace requirement below.
    `(,(format "\\(^\\|\\s-\\)%s\\(-%s\\)?" diary-time-regexp
               diary-time-regexp)
      . (0 'diary-time t)))))
;      . 'diary-time))))

(defvar diary-font-lock-keywords (diary-font-lock-keywords)
  "Forms to highlight in `diary-mode'.")

;;;###autoload
(define-derived-mode diary-mode fundamental-mode "Diary"
  "Major mode for editing the diary file."
  (set (make-local-variable 'font-lock-defaults)
       '(diary-font-lock-keywords t))
  (set (make-local-variable 'comment-start) diary-comment-start)
  (set (make-local-variable 'comment-end) diary-comment-end)
  (add-to-invisibility-spec '(diary . nil))
  (add-hook 'after-save-hook 'diary-redraw-calendar nil t)
  ;; In case the file was modified externally, refresh the calendar
  ;; after refreshing the diary buffer.
  (add-hook 'after-revert-hook 'diary-redraw-calendar nil t)
  (if diary-header-line-flag
      (setq header-line-format diary-header-line-format)))


;;; Fancy Diary Mode.

(defun diary-fancy-date-pattern ()
  "Return a regexp matching the first line of a fancy diary date header.
This depends on the calendar date style."
  (concat
   (let ((dayname (diary-name-pattern calendar-day-name-array nil t))
         (monthname (diary-name-pattern calendar-month-name-array nil t))
         (day "1")
         (month "2")
         ;; FIXME? This used to be "-?[0-9]+" - what was the "-?" for?
         (year "3"))
     ;; This is ugly.  c-d-d-form expects `day' etc to be "numbers in
     ;; string form"; eg the iso version calls string-to-number on some.
     ;; Therefore we cannot eg just let day = "[0-9]+".  (Bug#8583).
     ;; Assumes no integers in c-day/month-name-array.
     (replace-regexp-in-string "[0-9]+" "[0-9]+"
                               (mapconcat 'eval calendar-date-display-form "")
                               nil t))
   ;; Optional ": holiday name" after the date.
   "\\(: .*\\)?"))

(defun diary-fancy-date-matcher (limit)
  "Search for a fancy diary data header, up to LIMIT."
  ;; Any number of " other holiday name" lines, followed by "==" line.
  (when (re-search-forward
         (format "%s\\(\n +.*\\)*\n=+$" (diary-fancy-date-pattern)) limit t)
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-multiline t)
    t))

(define-obsolete-variable-alias 'fancy-diary-font-lock-keywords
  'diary-fancy-font-lock-keywords "23.1")

(defvar diary-fancy-font-lock-keywords
  `((diary-fancy-date-matcher . diary-face)
    ("^.*\\([aA]nniversary\\|[bB]irthday\\).*$" . 'diary-anniversary)
    ("^.*Yahrzeit.*$" . font-lock-reference-face)
    ("^\\(Erev \\)?Rosh Hodesh.*" . font-lock-function-name-face)
    ("^Day.*omer.*$" . font-lock-builtin-face)
    ("^Parashat.*$" . font-lock-comment-face)
    (,(format "\\(^\\|\\s-\\)%s\\(-%s\\)?" diary-time-regexp
              diary-time-regexp) . 'diary-time))
  "Keywords to highlight in fancy diary display.")

;; If region looks like it might start or end in the middle of a
;; multiline pattern, extend the region to encompass the whole pattern.
(defun diary-fancy-font-lock-fontify-region-function (beg end &optional verbose)
  "Function to use for `font-lock-fontify-region-function' in Fancy Diary.
Needed to handle multiline keyword in `diary-fancy-font-lock-keywords'.
Fontify the region between BEG and END, quietly unless VERBOSE is non-nil."
  (goto-char beg)
  (forward-line 0)
  (if (looking-at "=+$") (forward-line -1))
  (while (and (looking-at " +[^ ]")
              (zerop (forward-line -1))))
  ;; This check not essential.
  (if (looking-at (diary-fancy-date-pattern))
      (setq beg (line-beginning-position)))
  (goto-char end)
  (forward-line 0)
  (while (and (looking-at " +[^ ]")
              (zerop (forward-line 1))))
  (if (looking-at "=+$")
      (setq end (line-beginning-position 2)))
  (font-lock-default-fontify-region beg end verbose))

(defvar diary-fancy-overriding-map (make-sparse-keymap)
  "Keymap overriding minor-mode maps in `diary-fancy-display-mode'.")

(define-derived-mode diary-fancy-display-mode special-mode
  "Diary"
  "Major mode used while displaying diary entries using Fancy Display."
  (set (make-local-variable 'font-lock-defaults)
       '(diary-fancy-font-lock-keywords
         t nil nil nil
         (font-lock-fontify-region-function
          . diary-fancy-font-lock-fontify-region-function)))
  (set (make-local-variable 'minor-mode-overriding-map-alist)
       (list (cons t diary-fancy-overriding-map)))
  (view-mode 1))

(define-obsolete-function-alias 'fancy-diary-display-mode
  'diary-fancy-display-mode "23.1")

;; Following code from Dave Love <fx@gnu.org>.
;; Import Outlook-format appointments from mail messages in Gnus or
;; Rmail using command `diary-from-outlook'.  This, or the specialized
;; functions `diary-from-outlook-gnus' and `diary-from-outlook-rmail',
;; could be run from hooks to notice appointments automatically (in
;; which case they will prompt about adding to the diary).  The
;; message formats recognized are customizable through `diary-outlook-formats'.

(defun diary-from-outlook-internal (subject body &optional test-only)
  "Snarf a diary entry from a message assumed to be from MS Outlook.
SUBJECT and BODY are strings giving the message subject and body.
Arg TEST-ONLY non-nil means return non-nil if and only if the
message contains an appointment, don't make a diary entry."
  (catch 'finished
    (let (format-string)
      (dolist (fmt diary-outlook-formats)
        (when (eq 0 (string-match (car fmt) body))
          (unless test-only
            (setq format-string (cdr fmt))
            (save-excursion
              (save-window-excursion
                (diary-make-entry
                 (format (replace-match (if (functionp format-string)
                                            (funcall format-string body)
                                          format-string)
                                        t nil (match-string 0 body))
                         subject)))))
          (throw 'finished t))))
    nil))

(defvar gnus-article-mime-handles)
(defvar gnus-article-buffer)

(autoload 'gnus-fetch-field "gnus-util")
(autoload 'gnus-narrow-to-body "gnus")
(autoload 'mm-get-part "mm-decode")

(defun diary-from-outlook-gnus (&optional noconfirm)
  "Maybe snarf diary entry from Outlook-generated message in Gnus.
Unless the optional argument NOCONFIRM is non-nil (which is the case when
this function is called interactively), then if an entry is found the
user is asked to confirm its addition.
Add this function to `gnus-article-prepare-hook' to notice appointments
automatically."
  (interactive "p")
  (with-current-buffer gnus-article-buffer
    (let ((subject (gnus-fetch-field "subject"))
          (body (if gnus-article-mime-handles
                    ;; We're multipart.  Don't get confused by part
                    ;; buttons &c.  Assume info is in first part.
                    (mm-get-part (nth 1 gnus-article-mime-handles))
                  (save-restriction
                    (gnus-narrow-to-body)
                    (buffer-string)))))
      (when (diary-from-outlook-internal subject body t)
        (when (or noconfirm (y-or-n-p "Snarf diary entry? "))
          (diary-from-outlook-internal subject body)
          (message "Diary entry added"))))))

(custom-add-option 'gnus-article-prepare-hook 'diary-from-outlook-gnus)

(defvar rmail-buffer)

(defun diary-from-outlook-rmail (&optional noconfirm)
  "Maybe snarf diary entry from Outlook-generated message in Rmail.
Unless the optional argument NOCONFIRM is non-nil (which is the case when
this function is called interactively), then if an entry is found the
user is asked to confirm its addition."
  (interactive "p")
  ;; FIXME maybe the body needs rmail-mm decoding, in which case
  ;; there is no single buffer with both body and subject, sigh.
  (with-current-buffer rmail-buffer
    (let ((subject (mail-fetch-field "subject"))
          (body (buffer-substring (save-excursion
                                    (rfc822-goto-eoh)
                                    (point))
                                  (point-max))))
      (when (diary-from-outlook-internal subject body t)
        (when (or noconfirm (y-or-n-p "Snarf diary entry? "))
          (diary-from-outlook-internal subject body)
          (message "Diary entry added"))))))

(defun diary-from-outlook (&optional noconfirm)
  "Maybe snarf diary entry from current Outlook-generated message.
Currently knows about Gnus and Rmail modes.  Unless the optional
argument NOCONFIRM is non-nil (which is the case when this
function is called interactively), then if an entry is found the
user is asked to confirm its addition."
  (interactive "p")
  (let ((func (cond
               ((eq major-mode 'rmail-mode)
                #'diary-from-outlook-rmail)
               ((memq major-mode '(gnus-summary-mode gnus-article-mode))
                #'diary-from-outlook-gnus)
               (t (error "Don't know how to snarf in `%s'" major-mode)))))
    (funcall func noconfirm)))

(provide 'diary-lib)

;; Local Variables:
;; coding: utf-8
;; End:

;;; diary-lib.el ends here
