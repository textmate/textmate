;;; calendar.el --- calendar functions

;; Copyright (C) 1988-1995, 1997, 2000-2012  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: calendar, Gregorian calendar, diary, holidays

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

;; This collection of functions implements a calendar window.  It
;; generates a calendar for the current month, together with the
;; previous and coming months, or for any other three-month period.
;; The calendar can be scrolled forward and backward in the window to
;; show months in the past or future; the cursor can move forward and
;; backward by days, weeks, or months, making it possible, for
;; instance, to jump to the date a specified number of days, weeks, or
;; months from the date under the cursor.  The user can display a list
;; of holidays and other notable days for the period shown; the
;; notable days can be marked on the calendar, if desired.  The user
;; can also specify that dates having corresponding diary entries (in
;; a file that the user specifies) be marked; the diary entries for
;; any date can be viewed in a separate window.  The diary and the
;; notable days can be viewed independently of the calendar.  Dates
;; can be translated from the (usual) Gregorian calendar to the day of
;; the year/days remaining in year, to the ISO commercial calendar, to
;; the Julian (old style) calendar, to the Hebrew calendar, to the
;; Islamic calendar, to the Bahá'í calendar, to the French
;; Revolutionary calendar, to the Mayan calendar, to the Chinese
;; calendar, to the Coptic calendar, to the Ethiopic calendar, and to
;; the astronomical (Julian) day number.  Times of sunrise/sunset can
;; be displayed, as can the phases of the moon.  Appointment
;; notification for diary entries is available.  Calendar printing via
;; LaTeX is available.

;; The following files are part of the calendar/diary code:

;;    appt.el                    Appointment notification
;;    cal-bahai.el               Bahá'í calendar
;;    cal-china.el               Chinese calendar
;;    cal-coptic.el              Coptic/Ethiopic calendars
;;    cal-dst.el                 Daylight saving time rules
;;    cal-french.el              French revolutionary calendar
;;    cal-hebrew.el              Hebrew calendar
;;    cal-html.el                Calendars in HTML
;;    cal-islam.el               Islamic calendar
;;    cal-iso.el                 ISO calendar
;;    cal-julian.el              Julian/astronomical calendars
;;    cal-mayan.el               Mayan calendars
;;    cal-menu.el                Menu support
;;    cal-move.el                Movement in the calendar
;;    cal-persia.el              Persian calendar
;;    cal-tex.el                 Calendars in LaTeX
;;    cal-x.el                   Dedicated frame functions
;;    calendar.el                This file
;;    diary-lib.el               Diary functions
;;    holidays.el                Holiday functions
;;    lunar.el                   Phases of the moon
;;    solar.el                   Sunrise/sunset, equinoxes/solstices

;; Technical details of all the calendrical calculations can be found in
;; ``Calendrical Calculations: The Millennium Edition'' by Edward M. Reingold
;; and Nachum Dershowitz, Cambridge University Press (2001).

;; An earlier version of the technical details appeared in
;; ``Calendrical Calculations'' by Nachum Dershowitz and Edward M. Reingold,
;; Software--Practice and Experience, Volume 20, Number 9 (September, 1990),
;; pages 899-928, and in ``Calendrical Calculations, Part II: Three Historical
;; Calendars'' by E. M. Reingold,  N. Dershowitz, and S. M. Clamen,
;; Software--Practice and Experience, Volume 23, Number 4 (April, 1993),
;; pages 383-404.

;; Hard copies of these two papers can be obtained by sending email to
;; reingold@cs.uiuc.edu with the SUBJECT "send-paper-cal" (no quotes) and
;; the message BODY containing your mailing address (snail).


;; A note on free variables:

;; The calendar passes around a few dynamically bound variables, which
;; unfortunately have rather common names.  They are meant to be
;; available for external functions, so the names can't be changed.

;; displayed-month, displayed-year: bound in calendar-generate, the
;;   central month of the 3 month calendar window
;; original-date, number: bound in diary-list-entries, the arguments
;;   with which that function was called.
;; date, entry: bound in diary-list-sexp-entries (qv)

;; Bound in diary-list-entries:
;; diary-entries-list: use in d-l, appt.el, and by diary-add-to-list
;; diary-saved-point: only used in diary-lib.el, passed to the display func
;; date-string: only used in diary-lib.el
;; list-only: don't modify the diary-buffer, just return a list of entries
;; file-glob-attrs: yuck

;;; Code:

(load "cal-loaddefs" nil t)

;; Avoid recursive load of calendar when loading cal-menu.  Yuck.
(provide 'calendar)
(require 'cal-menu)

(defgroup calendar nil
  "Calendar and time management support."
  :prefix "calendar-"
  :group 'applications)

(defgroup calendar-hooks nil
  "Calendar hooks."
  :prefix "calendar-"
  :group 'calendar)

(defgroup calendar-faces nil
  "Calendar faces."
  :prefix "calendar-"
  :group 'calendar)

(defcustom calendar-offset 0
  "The offset of the principal month from the center of the calendar window.
0 means the principal month is in the center (default), -1 means on the left,
+1 means on the right.  Larger (or smaller) values push the principal month off
the screen."
  :type 'integer
  :group 'calendar)

(defcustom calendar-setup nil
  "The frame setup of the calendar.
The choices are: `one-frame' (calendar and diary together in one separate,
dedicated frame); `two-frames' (calendar and diary in separate, dedicated
frames); `calendar-only' (calendar in a separate, dedicated frame); with
any other value the current frame is used.  Using any of the first
three options overrides the value of `calendar-view-diary-initially-flag'."
  :type '(choice
          (const :tag "calendar and diary in separate frame" one-frame)
          (const :tag "calendar and diary each in own frame" two-frames)
          (const :tag "calendar in separate frame" calendar-only)
          (const :tag "use current frame" nil))
  :group 'calendar)

(defcustom calendar-minimum-window-height 8
  "Minimum height `calendar-generate-window' should use for calendar window."
  :type 'integer
  :version "22.1"
  :group 'calendar)

;; See discussion in bug#1806.
(defcustom calendar-split-width-threshold nil
  "Value to use for `split-width-threshold' when creating a calendar.
This only affects frames wider than the default value of
`split-width-threshold'."
  :type '(choice (const nil)
                 (integer))
  :version "23.2"
  :group 'calendar)

(defcustom calendar-week-start-day 0
  "The day of the week on which a week in the calendar begins.
0 means Sunday (default), 1 means Monday, and so on.

If you change this variable directly (without using customize)
after starting `calendar', you should call `calendar-redraw' to
update the calendar display to reflect the change, otherwise
movement commands will not work correctly."
  :type 'integer
  ;; Change the initialize so that if you reload calendar.el, it will not
  ;; cause a redraw (which may fail, e.g. with "invalid byte-code in
  ;; calendar.elc" because of the "byte-compile-dynamic").
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set sym val)
         (calendar-redraw))
  :group 'calendar)

(define-obsolete-variable-alias 'view-diary-entries-initially
  'calendar-view-diary-initially-flag "23.1")

(defcustom calendar-view-diary-initially-flag nil
  "Non-nil means display current date's diary entries on entry to calendar.
The diary is displayed in another window when the calendar is first displayed,
if the current date is visible.  The number of days of diary entries displayed
is governed by the variable `diary-number-of-entries'.  This variable can
be overridden by the value of `calendar-setup'."
  :type 'boolean
  :group 'diary)

(define-obsolete-variable-alias 'mark-diary-entries-in-calendar
  'calendar-mark-diary-entries-flag "23.1")

;; FIXME :set
(defcustom calendar-mark-diary-entries-flag nil
  "Non-nil means mark dates with diary entries, in the calendar window.
The marking symbol is specified by the variable `diary-entry-marker'."
  :type 'boolean
  :group 'diary)

(defcustom calendar-remove-frame-by-deleting t
  "Determine how the calendar mode removes a frame no longer needed.
If nil, make an icon of the frame.  If non-nil, delete the frame."
  :type 'boolean
  :version "23.1"                       ; changed from nil to t
  :group 'view
  :group 'calendar)

(defface calendar-today
  '((t (:underline t)))
  "Face for indicating today's date in the calendar.
See the variable `calendar-today-marker'."
  :group 'calendar-faces)

(define-obsolete-face-alias 'calendar-today-face 'calendar-today "22.1")

(defface diary
  '((((min-colors 88) (class color) (background light))
     :foreground "red1")
    (((class color) (background light))
     :foreground "red")
    (((min-colors 88) (class color) (background dark))
     :foreground "yellow1")
    (((class color) (background dark))
     :foreground "yellow")
    (t
     :weight bold))
  "Face for highlighting diary entries.
Used to mark diary entries in the calendar (see `diary-entry-marker'),
and to highlight the date header in the fancy diary."
  :group 'calendar-faces)

(define-obsolete-face-alias 'diary-face 'diary "22.1")

(defface holiday
  '((((class color) (background light))
     :background "pink")
    (((class color) (background dark))
     :background "chocolate4")
    (t
     :inverse-video t))
  "Face for indicating in the calendar dates that have holidays.
See `calendar-holiday-marker'."
  :group 'calendar-faces)

(define-obsolete-face-alias 'holiday-face 'holiday "22.1")

;; These briefly checked font-lock-mode, but that is broken, since it
;; is a buffer-local variable, and which buffer happens to be current
;; when this file is loaded shouldn't make a difference.  One could
;; perhaps check global-font-lock-mode, or font-lock-global-modes; but
;; this feature doesn't use font-lock, so there's no real reason it
;; should respect those either.  See bug#2199.
;; They also used to check display-color-p, but that is a problem if
;; loaded from --daemon.  Since BW displays are rare now, this was
;; also taken out.  The way to keep it would be to have nil mean do a
;; runtime check whenever this variable is used.
(defcustom diary-entry-marker 'diary
  "How to mark dates that have diary entries.
The value can be either a single-character string (e.g. \"+\") or a face."
  :type '(choice (string :tag "Single character string") face)
  :group 'diary
  :version "23.1")

(defcustom calendar-today-marker 'calendar-today
  "How to mark today's date in the calendar.
The value can be either a single-character string (e.g. \"=\") or a face.
Used by `calendar-mark-today'."
  :type '(choice (string :tag "Single character string") face)
  :group 'calendar
  :version "23.1")

(defcustom calendar-holiday-marker 'holiday
  "How to mark notable dates in the calendar.
The value can be either a single-character string (e.g. \"*\") or a face."
  :type '(choice (string :tag "Single character string") face)
  :group 'holidays
  :version "23.1")

(define-obsolete-variable-alias 'view-calendar-holidays-initially
  'calendar-view-holidays-initially-flag "23.1")

(defcustom calendar-view-holidays-initially-flag nil
  "Non-nil means display holidays for current three month period on entry.
The holidays are displayed in another window when the calendar is first
displayed."
  :type 'boolean
  :group 'holidays)

(define-obsolete-variable-alias 'mark-holidays-in-calendar
  'calendar-mark-holidays-flag "23.1")

;; FIXME :set
(defcustom calendar-mark-holidays-flag nil
  "Non-nil means mark dates of holidays in the calendar window.
The marking symbol is specified by the variable `calendar-holiday-marker'."
  :type 'boolean
  :group 'holidays)

(defcustom calendar-mode-hook nil
  "Hook run when entering `calendar-mode'."
  :type 'hook
  :group 'calendar-hooks)

(defcustom calendar-load-hook nil
  "List of functions to be called after the calendar is first loaded.
This is the place to add key bindings to `calendar-mode-map'."
  :type 'hook
  :group 'calendar-hooks)

(define-obsolete-variable-alias 'initial-calendar-window-hook
  'calendar-initial-window-hook "23.1")

(defcustom calendar-initial-window-hook nil
  "List of functions to be called when the calendar window is created.
Quitting the calendar and re-entering it will cause these functions
to be called again."
  :type 'hook
  :group 'calendar-hooks)

(define-obsolete-variable-alias 'today-visible-calendar-hook
  'calendar-today-visible-hook "23.1")

(defcustom calendar-today-visible-hook nil
  "List of functions called whenever the current date is visible.
To mark today's date, add the function `calendar-mark-today'.
To replace the date with asterisks, add the function `calendar-star-date'.
See also `calendar-today-invisible-hook'.

In general, be careful about changing characters in the calendar buffer,
since it may cause the movement commands to fail."
  :type 'hook
  :options '(calendar-mark-today calendar-star-date)
  :group 'calendar-hooks)

(define-obsolete-variable-alias 'today-invisible-calendar-hook
  'calendar-today-invisible-hook "23.1")

(defcustom calendar-today-invisible-hook nil
  "List of functions called whenever the current date is not visible.
See also `calendar-today-visible-hook'."
  :type 'hook
  :group 'calendar-hooks)

(defcustom calendar-move-hook nil
  "List of functions called whenever the cursor moves in the calendar.
For example,

  (add-hook 'calendar-move-hook (lambda () (diary-view-entries 1)))

redisplays the diary for whatever date the cursor is moved to."
  :type 'hook
  :options '(calendar-update-mode-line)
  :group 'calendar-hooks)

(defcustom calendar-date-echo-text
  "mouse-2: general menu\nmouse-3: menu for this date"
  "String displayed when the cursor is over a date in the calendar.
Can be either a fixed string, or a lisp expression that returns one.
When this expression is evaluated, DAY, MONTH, and YEAR are
integers appropriate to the relevant date.  For example, to
display the ISO date:

  (setq calendar-date-echo-text '(format \"ISO date: %s\"
                                         (calendar-iso-date-string
                                          (list month day year))))
Changing this variable without using customize has no effect on
pre-existing calendar windows."
  :group 'calendar
  :initialize 'custom-initialize-default
  :risky t
  :set (lambda (sym val)
         (set sym val)
         (calendar-redraw))
  :type '(choice (string :tag "Fixed string")
                 (sexp :value
                       (format "ISO date: %s"
                                (calendar-iso-date-string
                                 (list month day year)))))
  :version "23.1")


(defvar calendar-month-digit-width nil
  "Width of the region with numbers in each month in the calendar.")

(defvar calendar-month-width nil
  "Full width of each month in the calendar.")

(defvar calendar-right-margin nil
  "Right margin of the calendar.")

(defvar calendar-month-edges nil
  "Alist of month edge columns.
Each element has the form (N LEFT FIRST LAST RIGHT), where
LEFT is the leftmost column associated with month segment N,
FIRST and LAST are the first and last columns with day digits in,
and LAST is the rightmost column.")

(defun calendar-month-edges (segment)
  "Compute the month edge columns for month SEGMENT.
Returns a list (LEFT FIRST LAST RIGHT), where LEFT is the
leftmost column associated with a month, FIRST and LAST are the
first and last columns with day digits in, and LAST is the
rightmost column."
  ;; The leftmost column with a digit in it in this month segment.
  (let* ((first (+ calendar-left-margin
                        (* segment calendar-month-width)))
         ;; The rightmost column with a digit in it in this month segment.
         (last (+ first (1- calendar-month-digit-width)))
         (left (if (eq segment 0)
                   0
                 (+ calendar-left-margin
                    (* segment calendar-month-width)
                    (- (/ calendar-intermonth-spacing 2)))))
         ;; The rightmost edge of this month segment, dividing the
         ;; space between months in two.
         (right (+ calendar-left-margin
                  (* (1+ segment) calendar-month-width)
                  (- (/ calendar-intermonth-spacing 2)))))
    (list left first last right)))

(defun calendar-recompute-layout-variables ()
  "Recompute some layout-related calendar \"constants\"."
  (setq calendar-month-digit-width (+ (* 6 calendar-column-width)
                                      calendar-day-digit-width)
        calendar-month-width (+ (* 7 calendar-column-width)
                                calendar-intermonth-spacing)
        calendar-right-margin (+ calendar-left-margin
                                   (* 3 (* 7 calendar-column-width))
                                   (* 2 calendar-intermonth-spacing))
        calendar-month-edges nil)
  (dotimes (i 3)
    (push (cons i (calendar-month-edges i)) calendar-month-edges))
  (setq calendar-month-edges (reverse calendar-month-edges)))

;; FIXME add font-lock-keywords.
(defun calendar-set-layout-variable (symbol value &optional minmax)
  "Set SYMBOL's value to VALUE, an integer.
A positive/negative MINMAX enforces a minimum/maximum value.
Then redraw the calendar, if necessary."
  (let ((oldvalue (symbol-value symbol)))
    (custom-set-default symbol (if minmax
                                   (if (< minmax 0)
                                       (min value (- minmax))
                                     (max value minmax))
                                 value))
    (unless (equal value oldvalue)
      (calendar-recompute-layout-variables)
      (calendar-redraw))))

(defcustom calendar-left-margin 5
  "Empty space to the left of the first month in the calendar."
  :group 'calendar
  :initialize 'custom-initialize-default
  :set 'calendar-set-layout-variable
  :type 'integer
  :version "23.1")

;; Or you can view it as columns of width 2, with 1 space, no space
;; after the last column, and a 5 space gap between month.
;; FIXME check things work if this is odd.
(defcustom calendar-intermonth-spacing 4
  "Space between months in the calendar.  Minimum value is 1."
  :group 'calendar
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (calendar-set-layout-variable sym val 1))
  :type 'integer
  :version "23.1")

;; FIXME calendar-month-column-width?
(defcustom calendar-column-width 3
  "Width of each day column in the calendar.  Minimum value is 3."
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (calendar-set-layout-variable sym val 3))
  :type 'integer
  :version "23.1")

(defcustom calendar-day-header-width 2
  "Width of the day column headers in the calendar.
Must be at least one less than `calendar-column-width'."
  :group 'calendar
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (calendar-set-layout-variable sym val (- 1 calendar-column-width)))
  :type 'integer
  :version "23.1")

;; FIXME a format specifier instead?
(defcustom calendar-day-digit-width 2
  "Width of the day digits in the calendar.  Minimum value is 2."
  :group 'calendar
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (calendar-set-layout-variable sym val 2))
  :type 'integer
  :version "23.1")

(defcustom calendar-intermonth-header nil
  "Header text display in the space to the left of each calendar month.
See `calendar-intermonth-text'."
  :group 'calendar
  :initialize 'custom-initialize-default
  :risky t
  :set (lambda (sym val)
         (set sym val)
         (calendar-redraw))
  :type '(choice (const nil :tag "Nothing")
                 (string :tag "Fixed string")
                 (sexp :value
                       (propertize "WK" 'font-lock-face
                                   'font-lock-function-name-face)))
  :version "23.1")

(defcustom calendar-intermonth-text nil
  "Text to display in the space to the left of each calendar month.
Can be nil, a fixed string, or a lisp expression that returns a string.
When the expression is evaluated, the variables DAY, MONTH and YEAR
are integers appropriate for the first day in each week.
Will be truncated to the smaller of `calendar-left-margin' and
`calendar-intermonth-spacing'.  The last character is forced to be a space.
For example, to display the ISO week numbers:

  (setq calendar-week-start-day 1
        calendar-intermonth-text
        '(propertize
          (format \"%2d\"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-function-name-face))

See also `calendar-intermonth-header'."
  :group 'calendar
  :initialize 'custom-initialize-default
  :risky t
  :set (lambda (sym val)
         (set sym val)
         (calendar-redraw))
  :type '(choice (const nil :tag "Nothing")
                 (string :tag "Fixed string")
                 (sexp :value
                       (propertize
                        (format "%2d"
                                (car
                                 (calendar-iso-from-absolute
                                  (calendar-absolute-from-gregorian
                                   (list month day year)))))
                        'font-lock-face 'font-lock-function-name-face)))
  :version "23.1")

(defcustom diary-file "~/diary"
  "Name of the file in which one's personal diary of dates is kept.

The file's entries are lines beginning with any of the forms
specified by the variable `diary-date-forms', which by default
uses the forms of `diary-american-date-forms':

            MONTH/DAY
            MONTH/DAY/YEAR
            MONTHNAME DAY
            MONTHNAME DAY, YEAR
            DAYNAME

with the remainder of the line being the diary entry string for
that date.  MONTH and DAY are one or two digit numbers, YEAR is a
number and may be written in full or abbreviated to the final two
digits (if `diary-abbreviated-year-flag' is non-nil).  MONTHNAME
and DAYNAME can be spelled in full (as specified by the variables
`calendar-month-name-array' and `calendar-day-name-array'), or
abbreviated (as specified by `calendar-month-abbrev-array' and
`calendar-day-abbrev-array') with or without a period.  Case is
ignored.  Any of DAY, MONTH, or MONTHNAME, YEAR can be `*' which
matches any day, month, or year, respectively.  If the date does
not contain a year, it is generic and applies to any year.  A
DAYNAME entry applies to the appropriate day of the week in every week.

You can customize `diary-date-forms' to your preferred format.
Three default styles are provided: `diary-american-date-forms',
`diary-european-date-forms', and `diary-iso-date-forms'.
You can choose between these by setting `calendar-date-style' in your
.emacs file, or by using `calendar-set-date-style' when in the calendar.

A diary entry can be preceded by the character `diary-nonmarking-symbol'
\(ordinarily `&') to make that entry nonmarking--that is, it will not be
marked on dates in the calendar window but will appear in a diary window.

Multiline diary entries are made by indenting lines after the first with
either a TAB or one or more spaces.

Lines not in one the above formats are ignored.  Here are some sample diary
entries (in the default American style):

     12/22/1988 Twentieth wedding anniversary!!
     &1/1. Happy New Year!
     10/22 Ruth's birthday.
     21: Payday
     Tuesday--weekly meeting with grad students at 10am
              Supowit, Shen, Bitner, and Kapoor to attend.
     1/13/89 Friday the thirteenth!!
     &thu 4pm squash game with Lloyd.
     mar 16 Dad's birthday
     April 15, 1989 Income tax due.
     &* 15 time cards due.

If the first line of a diary entry consists only of the date or day name with
no trailing blanks or punctuation, then that line is not displayed in the
diary window; only the continuation lines is shown.  For example, the
single diary entry

     02/11/1989
      Bill Blattner visits Princeton today
      2pm Cognitive Studies Committee meeting
      2:30-5:30 Lizzie at Lawrenceville for `Group Initiative'
      4:00pm Jamie Tappenden
      7:30pm Dinner at George and Ed's for Alan Ryan
      7:30-10:00pm dance at Stewart Country Day School

will appear in the diary window without the date line at the beginning.  This
facility allows the diary window to look neater, but can cause confusion if
used with more than one day's entries displayed.

Diary entries can be based on Lisp sexps.  For example, the diary entry

      %%(diary-block 11 1 1990 11 10 1990) Vacation

causes the diary entry \"Vacation\" to appear from November 1 through
November 10, 1990.  See the documentation for the function
`diary-list-sexp-entries' for more details.

Diary entries based on the Hebrew, the Islamic and/or the Bahá'í
calendar are also possible, but because these are somewhat slow, they
are ignored unless you set the `diary-nongregorian-listing-hook' and
the `diary-nongregorian-marking-hook' appropriately.  See the
documentation of these hooks for details.

Diary files can contain directives to include the contents of other files; for
details, see the documentation for the variable `diary-list-entries-hook'."
  :type 'file
  :group 'diary)

;; FIXME do these have to be single characters?
(defcustom diary-nonmarking-symbol "&"
  "Symbol indicating that a diary entry is not to be marked in the calendar."
  :type 'string
  :group 'diary)

(define-obsolete-variable-alias 'hebrew-diary-entry-symbol
  'diary-hebrew-entry-symbol "23.1")

(defcustom diary-hebrew-entry-symbol "H"
  "Symbol indicating a diary entry according to the Hebrew calendar."
  :type 'string
  :group 'diary)

(define-obsolete-variable-alias 'islamic-diary-entry-symbol
  'diary-islamic-entry-symbol "23.1")

(defcustom diary-islamic-entry-symbol "I"
  "Symbol indicating a diary entry according to the Islamic calendar."
  :type 'string
  :group 'diary)

(define-obsolete-variable-alias 'bahai-diary-entry-symbol
  'diary-bahai-entry-symbol "23.1")

(defcustom diary-bahai-entry-symbol "B"
  "Symbol indicating a diary entry according to the Bahá'í calendar."
  :type 'string
  :group 'diary)

(defcustom european-calendar-style nil
  "Non-nil means use the European style of dates in the diary and display.
In this case, a date like 1/2/1990 would be interpreted as
February 1, 1990.  See `diary-european-date-forms' for the
default European diary date styles.

Setting this variable directly does not take effect (if the
calendar package is already loaded).  Rather, use either
\\[customize] or the function `calendar-set-date-style'."
  :type 'boolean
  ;; Without :initialize (require 'calendar) throws an error because
  ;; calendar-set-date-style is undefined at this point.
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (if value
             (calendar-set-date-style 'european)
           (calendar-set-date-style 'american)))
  :group 'calendar)

(make-obsolete-variable 'european-calendar-style 'calendar-date-style "23.1")

;; If this is autoloaded, c-d-s gets set before any customization of e-c-s.
(defcustom calendar-date-style (if european-calendar-style 'european
                                 'american)
  "Your preferred style for writing dates.
The options are:
`american' - month/day/year
`european' - day/month/year
`iso'      - year/month/day
This affects how dates written in your diary are interpreted.
It also affects date display, as well as those calendar and diary
functions that take a date as an argument, e.g. `diary-date', by
changing the order in which the arguments are interpreted.

Setting this variable directly does not take effect (if the
calendar package is already loaded).  Rather, use either
\\[customize] or the function `calendar-set-date-style'."
  :version "23.1"
  :type '(choice (const american :tag "Month/Day/Year")
                 (const european :tag "Day/Month/Year")
                 (const iso      :tag "Year/Month/Day"))
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (calendar-set-date-style value))
  :group 'calendar)

;; Next three are provided to aid in setting diary-date-forms.
;; FIXME move to diary-lib?
(defcustom diary-iso-date-forms
  '((month "[-/]" day "[^-/0-9]")
    (year "[-/]" month "[-/]" day "[^0-9]")
    ;; Cannot allow [-/] as separators here, since it would also match
    ;; the first element (bug#7377).
    (monthname " *" day "[^-0-9]")
    (year " *" monthname " *" day "[^0-9]")
    (dayname "\\W"))
    "List of pseudo-patterns describing the ISO style of dates.
The defaults are: MONTH[-/]DAY; YEAR[-/]MONTH[-/]DAY; MONTHNAME DAY;
YEAR MONTHNAME DAY; DAYNAME.  Normally you should not customize this,
but `diary-date-forms' (which see)."
    :version "23.3"                     ; bug#7377
    :type '(repeat (choice (cons :tag "Backup"
                               :value (backup . nil)
                               (const backup)
                               (repeat (list :inline t :format "%v"
                                             (symbol :tag "Keyword")
                                             (choice symbol regexp))))
                         (repeat (list :inline t :format "%v"
                                       (symbol :tag "Keyword")
                                       (choice symbol regexp)))))
    :group 'diary)

(define-obsolete-variable-alias 'american-date-diary-pattern
  'diary-american-date-forms "23.1")

(defcustom diary-american-date-forms
  '((month "/" day "[^/0-9]")
    (month "/" day "/" year "[^0-9]")
    (monthname " *" day "[^,0-9]")
    (monthname " *" day ", *" year "[^0-9]")
    (dayname "\\W"))
  "List of pseudo-patterns describing the American style of dates.
The defaults are: MONTH/DAY; MONTH/DAY/YEAR; MONTHNAME DAY;
MONTHNAME DAY, YEAR; DAYNAME.  Normally you should not customize this,
but `diary-date-forms' (which see)."
  :type '(repeat (choice (cons :tag "Backup"
                               :value (backup . nil)
                               (const backup)
                               (repeat (list :inline t :format "%v"
                                             (symbol :tag "Keyword")
                                             (choice symbol regexp))))
                         (repeat (list :inline t :format "%v"
                                       (symbol :tag "Keyword")
                                       (choice symbol regexp)))))
  :group 'diary)

(define-obsolete-variable-alias 'european-date-diary-pattern
  'diary-european-date-forms "23.1")

(defcustom diary-european-date-forms
  '((day "/" month "[^/0-9]")
    (day "/" month "/" year "[^0-9]")
    (backup day " *" monthname "\\W+\\<\\([^*0-9]\\|\\([0-9]+[:aApP]\\)\\)")
    (day " *" monthname " *" year "[^0-9]")
    (dayname "\\W"))
  "List of pseudo-patterns describing the European style of dates.
The defaults are: DAY/MONTH; DAY/MONTH/YEAR; DAY MONTHNAME;
DAY MONTHNAME YEAR; DAYNAME.  Normally you should not customize this, but
`diary-date-forms' (which see)."
  :type '(repeat (choice (cons :tag "Backup"
                               :value (backup . nil)
                               (const backup)
                               (repeat (list :inline t :format "%v"
                                             (symbol :tag "Keyword")
                                             (choice symbol regexp))))
                         (repeat (list :inline t :format "%v"
                                       (symbol :tag "Keyword")
                                       (choice symbol regexp)))))
  :group 'diary)

(defvar diary-font-lock-keywords)

(defcustom diary-date-forms (cond ((eq calendar-date-style 'iso)
                                   diary-iso-date-forms)
                                  ((eq calendar-date-style 'european)
                                   diary-european-date-forms)
                                  (t diary-american-date-forms))
  "List of pseudo-patterns describing the forms of date used in the diary.
The patterns on the list must be MUTUALLY EXCLUSIVE and should not match
any portion of the diary entry itself, just the date component.

A pseudo-pattern is a list of regular expressions and the keywords `month',
`day', `year', `monthname', and `dayname'.  The keyword `monthname' will
match the name of the month (see `calendar-month-name-array'), capitalized
or not, or its user-specified abbreviation (see `calendar-month-abbrev-array'),
followed by a period or not; it will also match `*'.  Similarly, `dayname'
will match the name of the day (see `calendar-day-name-array'), capitalized or
not, or its user-specified abbreviation (see `calendar-day-abbrev-array'),
followed by a period or not.  The keywords `month', `day', and `year' will
match those numerical values, preceded by arbitrarily many zeros; they will
also match `*'.

The matching of the diary entries with the date forms is done with the
standard syntax table from Fundamental mode, but with the `*' changed so
that it is a word constituent.

If, to be mutually exclusive, a pseudo-pattern must match a portion of the
diary entry itself, the first element of the pattern MUST be `backup'.  This
directive causes the date recognizer to back up to the beginning of the
current word of the diary entry, so in no case can the pattern match more than
a portion of the first word of the diary entry.

For examples of three common styles, see `diary-american-date-forms',
`diary-european-date-forms', and `diary-iso-date-forms'."
  :type '(repeat (choice (cons :tag "Backup"
                               :value (backup . nil)
                               (const backup)
                               (repeat (list :inline t :format "%v"
                                             (symbol :tag "Keyword")
                                             (choice symbol regexp))))
                         (repeat (list :inline t :format "%v"
                                       (symbol :tag "Keyword")
                                       (choice symbol regexp)))))
  :set-after '(calendar-date-style diary-iso-date-forms
                                   diary-european-date-forms
                                   diary-american-date-forms)
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (unless (equal value (eval symbol))
           (custom-set-default symbol value)
           (setq diary-font-lock-keywords (diary-font-lock-keywords))
           ;; Need to redraw not just to get new font-locking, but also
           ;; to pick up any newly recognized entries.
           (and (diary-live-p)
                (diary))))
  :group 'diary)

;; Next three are provided to aid in setting calendar-date-display-form.
(defcustom calendar-iso-date-display-form '((format "%s-%.2d-%.2d" year
                                               (string-to-number month)
                                               (string-to-number day)))
  "Pseudo-pattern governing the way a date appears in the ISO style.
Normally you should not customize this, but `calendar-date-display-form'
\(which see)."
  :type 'sexp
  :version "23.1"
  :group 'calendar)

(define-obsolete-variable-alias 'european-calendar-display-form
  'calendar-european-date-display-form "23.1")

(defcustom calendar-european-date-display-form
  '((if dayname (concat dayname ", ")) day " " monthname " " year)
  "Pseudo-pattern governing the way a date appears in the European style.
Normally you should not customize this, but `calendar-date-display-form'
\(which see)."
  :type 'sexp
  :group 'calendar)

(define-obsolete-variable-alias 'american-calendar-display-form
  'calendar-american-date-display-form "23.1")

(defcustom calendar-american-date-display-form
  '((if dayname (concat dayname ", ")) monthname " " day ", " year)
  "Pseudo-pattern governing the way a date appears in the American style.
Normally you should not customize this, but `calendar-date-display-form'
\(which see)."
  :type 'sexp
  :group 'calendar)

(defcustom calendar-date-display-form
  (cond ((eq calendar-date-style 'iso)
         calendar-iso-date-display-form)
        ((eq calendar-date-style 'european)
         calendar-european-date-display-form)
        (t calendar-american-date-display-form))
  "Pseudo-pattern governing the way a calendar date appears.
Used by the function `calendar-date-string' (which see), a pseudo-pattern
is a list of expressions that can involve the keywords `month', `day',
and `year' (all numbers in string form), and `monthname' and `dayname'
\(both alphabetic strings).  For example, a typical American form would be

       '(month \"/\" day \"/\" (substring year -2))

whereas

       '((format \"%9s, %9s %2s, %4s\" dayname monthname day year))

would give the usual American style in fixed-length fields.  The variables
`calendar-iso-date-display-form', `calendar-european-date-display-form', and
`calendar-american-date-display-form' provide some defaults for three common
styles."
  :type 'sexp
  :set-after '(calendar-date-style calendar-iso-date-display-form
                                   calendar-european-date-display-form
                                   calendar-american-date-display-form)
  :group 'calendar)

(defun calendar-set-date-style (style)
  "Set the style of calendar and diary dates to STYLE (a symbol).
The valid styles are described in the documentation of `calendar-date-style'."
  (interactive (list (intern
                      (completing-read "Date style: "
                                       '("american" "european" "iso") nil t
                                       nil nil "american"))))
  (or (memq style '(american european iso))
      (setq style 'american))
  (setq calendar-date-style style
        calendar-date-display-form
        (symbol-value (intern-soft
                       (format "calendar-%s-date-display-form" style)))
        diary-date-forms
        (symbol-value (intern-soft (format "diary-%s-date-forms" style))))
  (calendar-update-mode-line))

(defun european-calendar ()
  "Set the interpretation and display of dates to the European style."
  (interactive)
  (calendar-set-date-style 'european))

(make-obsolete 'european-calendar 'calendar-set-date-style "23.1")

(defun american-calendar ()
  "Set the interpretation and display of dates to the American style."
  (interactive)
  (calendar-set-date-style 'american))

(make-obsolete 'american-calendar 'calendar-set-date-style "23.1")

(define-obsolete-variable-alias 'holidays-in-diary-buffer
  'diary-show-holidays-flag "23.1")

(defcustom diary-show-holidays-flag t
  "Non-nil means include holidays in the diary display.
The holidays appear in the mode line of the diary buffer, or in the
fancy diary buffer next to the date.  This slows down the diary functions
somewhat; setting it to nil makes the diary display faster."
  :type 'boolean
  :group 'holidays)

(defcustom calendar-debug-sexp nil
  "Turn debugging on when evaluating a sexp in the diary or holiday list."
  :type 'boolean
  :group 'calendar)

(define-obsolete-variable-alias 'all-hebrew-calendar-holidays
  'calendar-hebrew-all-holidays-flag "23.1")

(defcustom calendar-hebrew-all-holidays-flag nil
  "If nil, show only major holidays from the Hebrew calendar.
This means only those Jewish holidays that appear on secular calendars.
Otherwise, show all the holidays that would appear in a complete Hebrew
calendar."
  :type 'boolean
  :group 'holidays)

(define-obsolete-variable-alias 'all-christian-calendar-holidays
  'calendar-christian-all-holidays-flag "23.1")

(defcustom calendar-christian-all-holidays-flag nil
  "If nil, show only major holidays from the Christian calendar.
This means only those Christian holidays that appear on secular calendars.
Otherwise, show all the holidays that would appear in a complete Christian
calendar."
  :type 'boolean
  :group 'holidays)

(define-obsolete-variable-alias 'all-islamic-calendar-holidays
  'calendar-islamic-all-holidays-flag "23.1")

(defcustom calendar-islamic-all-holidays-flag nil
  "If nil, show only major holidays from the Islamic calendar.
This means only those Islamic holidays that appear on secular calendars.
Otherwise, show all the holidays that would appear in a complete Islamic
calendar."
  :type 'boolean
  :group 'holidays)

(define-obsolete-variable-alias 'all-bahai-calendar-holidays
  'calendar-bahai-all-holidays-flag "23.1")

(defcustom calendar-bahai-all-holidays-flag nil
  "If nil, show only major holidays from the Bahá'í calendar.
These are the days on which work and school must be suspended.
Otherwise, show all the holidays that would appear in a complete Bahá'í
calendar."
  :type 'boolean
  :group 'holidays)

(defcustom calendar-chinese-all-holidays-flag nil
  "If nil, show only the major holidays from the Chinese calendar."
  :version "23.1"
  :type 'boolean
  :group 'holidays)

;;; End of user options.

(calendar-recompute-layout-variables)

(defconst calendar-first-date-row 3
  "First row in the calendar with actual dates.")

(defconst calendar-buffer "*Calendar*"
  "Name of the buffer used for the calendar.")

(defconst holiday-buffer "*Holidays*"
  "Name of the buffer used for the displaying the holidays.")

(defconst diary-fancy-buffer "*Fancy Diary Entries*"
  "Name of the buffer used for the optional fancy display of the diary.")

(define-obsolete-variable-alias 'fancy-diary-buffer 'diary-fancy-buffer "23.1")

(defconst calendar-other-calendars-buffer "*Other Calendars*"
  "Name of the buffer used for the display of date on other calendars.")

(defconst lunar-phases-buffer "*Phases of Moon*"
  "Name of the buffer used for the lunar phases.")

(defconst solar-sunrises-buffer "*Sunrise/Sunset Times*"
  "Name of buffer used for sunrise/sunset times.")

(defconst calendar-hebrew-yahrzeit-buffer "*Yahrzeits*"
  "Name of the buffer used by `list-yahrzeit-dates'.")

(defmacro calendar-increment-month (mon yr n &optional nmonths)
  "Increment the variables MON and YR by N months.
Forward if N is positive or backward if N is negative.
A negative YR is interpreted as BC; -1 being 1 BC, and so on.
Optional NMONTHS is the number of months per year (default 12)."
  ;; Can view this as a form of base-nmonths arithmetic, in which "a
  ;; year" = "ten", and we never bother to use hundreds.
  `(let ((nmonths (or ,nmonths 12))
         macro-y)
     (if (< ,yr 0) (setq ,yr (1+ ,yr))) ; -1 BC -> 0 AD, etc
     (setq macro-y (+ (* ,yr nmonths) ,mon -1 ,n)
           ,mon (1+ (mod macro-y nmonths))
           ,yr (/ macro-y nmonths))
     ;; Alternative:
;;;      (setq macro-y (+ (* ,yr nmonths) ,mon -1 ,n)
;;;            ,yr (/ macro-y nmonths)
;;;            ,mon (- macro-y (* ,yr nmonths)))
     (and (< macro-y 0) (> ,mon 1) (setq ,yr (1- ,yr)))
     (if (< ,yr 1) (setq ,yr (1- ,yr))))) ; 0 AD -> -1 BC, etc

(define-obsolete-function-alias 'increment-calendar-month
  'calendar-increment-month "23.1")

(defvar displayed-month)
(defvar displayed-year)

(defun calendar-increment-month-cons (n &optional mon yr)
  "Return the Nth month after MON/YR.
The return value is a pair (MONTH . YEAR).
MON defaults to `displayed-month'.  YR defaults to `displayed-year'."
  (unless mon (setq mon displayed-month))
  (unless yr (setq yr displayed-year))
  (calendar-increment-month mon yr n)
  (cons mon yr))

(defmacro calendar-for-loop (var from init to final do &rest body)
  "Execute a for loop.
Evaluate BODY with VAR bound to successive integers from INIT to FINAL,
inclusive.  The standard macro `dotimes' is preferable in most cases."
  (declare (debug (symbolp "from" form "to" form "do" body))
           (indent defun))
  `(let ((,var (1- ,init)))
    (while (>= ,final (setq ,var (1+ ,var)))
      ,@body)))

(make-obsolete 'calendar-for-loop "use `dotimes' or `while' instead." "23.1")

(defmacro calendar-sum (index initial condition expression)
  "For INDEX = INITIAL, +1, ... (as long as CONDITION holds), sum EXPRESSION."
  (declare (debug (symbolp form form form)))
  `(let ((,index ,initial)
         (sum 0))
    (while ,condition
      (setq sum (+ sum ,expression)
            ,index (1+ ,index)))
    sum))

;; FIXME bind q to bury-buffer?
(defmacro calendar-in-read-only-buffer (buffer &rest body)
  "Switch to BUFFER and executes the forms in BODY.
First creates or erases BUFFER as needed.  Leaves BUFFER read-only,
with disabled undo.  Leaves point at point-min, displays BUFFER."
  (declare (indent 1) (debug t))
  `(progn
     (set-buffer (get-buffer-create ,buffer))
     (setq buffer-read-only nil
           buffer-undo-list t)
     (erase-buffer)
     ,@body
     (goto-char (point-min))
     (set-buffer-modified-p nil)
     (setq buffer-read-only t)
     (display-buffer ,buffer)))

;; The following are in-line for speed; they can be called thousands of times
;; when looking up holidays or processing the diary.  Here, for example, are
;; the numbers of calls to calendar/diary/holiday functions in preparing the
;; fancy diary display, for a moderately complex diary file, with functions
;; used instead of macros.  There were a total of 10000 such calls:
;;
;;  1934   calendar-extract-month
;;  1852   calendar-extract-year
;;  1819   calendar-extract-day
;;   845   calendar-leap-year-p
;;   837   calendar-day-number
;;   775   calendar-absolute-from-gregorian
;;   346   calendar-last-day-of-month
;;   286   calendar-hebrew-last-day-of-month
;;   188   calendar-hebrew-leap-year-p
;;   180   calendar-hebrew-elapsed-days
;;   163   calendar-hebrew-last-month-of-year
;;    66   calendar-date-compare
;;    65   calendar-hebrew-days-in-year
;;    60   calendar-julian-to-absolute
;;    50   calendar-hebrew-to-absolute
;;    43   calendar-date-equal
;;    38   calendar-gregorian-from-absolute
;;     .
;;
;; The use of these seven macros eliminates the overhead of 92% of the function
;; calls; it's faster this way.

(defsubst calendar-extract-month (date)
  "Extract the month part of DATE which has the form (month day year)."
  (car date))

(define-obsolete-function-alias 'extract-calendar-month
  'calendar-extract-month "23.1")

;; Note gives wrong answer for result of (calendar-read-date 'noday),
;; but that is only used by `calendar-other-month'.
(defsubst calendar-extract-day (date)
  "Extract the day part of DATE which has the form (month day year)."
  (cadr date))

(define-obsolete-function-alias 'extract-calendar-day
  'calendar-extract-day "23.1")

(defsubst calendar-extract-year (date)
  "Extract the year part of DATE which has the form (month day year)."
  (nth 2 date))

(define-obsolete-function-alias 'extract-calendar-year
  'calendar-extract-year "23.1")

(defsubst calendar-leap-year-p (year)
  "Return t if YEAR is a Gregorian leap year.
A negative year is interpreted as BC; -1 being 1 BC, and so on."
  ;; 1 BC = 0 AD, 2 BC acts like 1 AD, etc.
  (if (< year 0) (setq year (1- (abs year))))
  (and (zerop (% year 4))
       (or (not (zerop (% year 100)))
           (zerop (% year 400)))))

;; The foregoing is a bit faster, but not as clear as the following:
;;
;;(defsubst calendar-leap-year-p (year)
;;  "Return t if YEAR is a Gregorian leap year."
;;  (or
;;   (and (zerop (% year 4))
;;        (not (zerop (% year 100))))
;;   (zerop (% year 400)))

(defsubst calendar-last-day-of-month (month year)
  "The last day in MONTH during YEAR."
  (if (and (= month 2) (calendar-leap-year-p year))
      29
    (aref [31 28 31 30 31 30 31 31 30 31 30 31] (1- month))))

;; An explanation of the calculation can be found in PascAlgorithms by
;; Edward and Ruth Reingold, Scott-Foresman/Little, Brown, 1988.

(defsubst calendar-day-number (date)
  "Return the day number within the year of the date DATE.
For example, (calendar-day-number '(1 1 1987)) returns the value 1,
while (calendar-day-number '(12 31 1980)) returns 366."
  (let* ((month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (day-of-year (+ day (* 31 (1- month)))))
    (when (> month 2)
      (setq day-of-year (- day-of-year (/ (+ 23 (* 4 month)) 10)))
      (if (calendar-leap-year-p year)
          (setq day-of-year (1+ day-of-year))))
    day-of-year))

(defsubst calendar-absolute-from-gregorian (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary.
DATE is a list of the form (month day year).  A negative year is
interpreted as BC; -1 being 1 BC, and so on.  Dates before 12/31/1 BC
return negative results."
  (let ((year (calendar-extract-year date))
        offset-years)
    (cond ((zerop year)
           (error "There was no year zero"))
          ((> year 0)
           (setq offset-years (1- year))
           (+ (calendar-day-number date) ; days this year
              (* 365 offset-years)       ; + days in prior years
              (/ offset-years 4)         ; + Julian leap years
              (- (/ offset-years 100))   ; - century years
              (/ offset-years 400)))     ; + Gregorian leap years
          (t
           ;; Years between date and 1 BC, excluding 1 BC (1 for 2 BC, etc).
           (setq offset-years (abs (1+ year)))
           (- (calendar-day-number date)
              (* 365 offset-years)
              (/ offset-years 4)
              (- (/ offset-years 100))
              (/ offset-years 400)
              (calendar-day-number '(12 31 -1))))))) ; days in year 1 BC

;;;###autoload
(defun calendar (&optional arg)
  "Display a three-month Gregorian calendar.
The three months appear side by side, with the current month in
the middle surrounded by the previous and next months.  The
cursor is put on today's date.  If optional prefix argument ARG
is non-nil, prompts for the central month and year.

Once in the calendar window, future or past months can be moved
into view.  Arbitrary months can be displayed, or the calendar
can be scrolled forward or backward.  The cursor can be moved
forward or backward by one day, one week, one month, or one year.
All of these commands take prefix arguments which, when negative,
cause movement in the opposite direction.  For convenience, the
digit keys and the minus sign are automatically prefixes.  Use
\\[describe-mode] for details of the key bindings in the calendar
window.

Displays the calendar in a separate window, or optionally in a
separate frame, depending on the value of `calendar-setup'.

If `calendar-view-diary-initially-flag' is non-nil, also displays the
diary entries for the current date (or however many days
`diary-number-of-entries' specifies).  This variable can be
overridden by `calendar-setup'.  As well as being displayed,
diary entries can also be marked on the calendar (see
`calendar-mark-diary-entries-flag').

Runs the following hooks:

`calendar-load-hook' - after loading calendar.el
`calendar-today-visible-hook', `calendar-today-invisible-hook' - after
   generating a calendar, if today's date is visible or not, respectively
`calendar-initial-window-hook' - after first creating a calendar

This function is suitable for execution in a .emacs file."
  (interactive "P")
  ;; Avoid loading cal-x unless it will be used.
  (if (and (memq calendar-setup '(one-frame two-frames calendar-only))
           (display-multi-frame-p))
      (calendar-frame-setup calendar-setup arg)
    (calendar-basic-setup arg)))

(defun calendar-basic-setup (&optional arg nodisplay)
  "Create a three-month calendar.
If optional prefix argument ARG is non-nil, prompts for the month
and year, else uses the current date.  If NODISPLAY is non-nil, don't
display the generated calendar."
  (interactive "P")
  (let ((buff (current-buffer)))
    (set-buffer (get-buffer-create calendar-buffer))
    (calendar-mode)
    (let* ((pop-up-windows t)
           ;; Not really needed now, but means we use exactly the same
           ;; behavior as before in the non-wide case (see below).
           (split-height-threshold 1000)
           (split-width-threshold calendar-split-width-threshold)
           (date (if arg (calendar-read-date t)
                   (calendar-current-date)))
           (month (calendar-extract-month date))
           (year (calendar-extract-year date)))
      (calendar-increment-month month year (- calendar-offset))
      ;; Display the buffer before calling calendar-generate-window so that it
      ;; can get a chance to adjust the window sizes to the frame size.
      (unless nodisplay
        ;; We want a window configuration that looks something like
        ;; X        X | Y
        ;; -        -----
        ;; C        Z | C
        ;; where C is the calendar, and the LHS is the traditional,
        ;; non-wide frame, and the RHS is the wide frame case.
        ;; We should end up in the same state regardless of whether the
        ;; windows were initially split or not.
        ;; Previously, we only thought about the non-wide case.
        ;; We could just set split-height-threshold to 1000, relying on
        ;; the fact that the window splitting treated a single window as
        ;; a special case and would always split it (vertically).  The
        ;; same thing does not work in the wide-frame case, so now we do
        ;; the splitting by hand.
        ;; See discussion in bug#1806.
        ;; Actually, this still does not do quite the right thing in the
        ;; wide frame case if started from a configuration like the LHS.
        ;; Eg if you start with a non-wide frame, call calendar, then
        ;; make the frame wider.  This one is problematic because you
        ;; might need to split a totally unrelated window.  Oh well, it
        ;; seems unlikely, and perhaps respecting the original layout is
        ;; the right thing in that case.
        ;;
        ;; Is this a wide frame?  If so, split it horizontally.
        (if (window-splittable-p t) (split-window-right))
        (pop-to-buffer calendar-buffer)
        ;; Has the window already been split vertically?
        (when (and (not (window-dedicated-p))
                   (window-full-height-p))
          (let ((win (split-window-below)))
            ;; In the upper window, show whatever was visible before.
            ;; This looks better than using other-buffer.
            (switch-to-buffer buff)
            ;; Switch to the lower window with the calendar buffer.
            (select-window win))))
      (calendar-generate-window month year)
      (if (and calendar-view-diary-initially-flag
               (calendar-date-is-visible-p date))
          (diary-view-entries))))
  (if calendar-view-holidays-initially-flag
      (let* ((diary-buffer (get-file-buffer diary-file))
             (diary-window (if diary-buffer (get-buffer-window diary-buffer)))
             (split-height-threshold (if diary-window 2 1000)))
        ;; FIXME display buffer?
        (calendar-list-holidays)))
  (run-hooks 'calendar-initial-window-hook))

(defun calendar-generate-window (&optional mon yr)
  "Generate the calendar window for the current date.
Optional integers MON and YR are used instead of today's date."
  (let* ((inhibit-read-only t)
         (today (calendar-current-date))
         (month (calendar-extract-month today))
         (day (calendar-extract-day today))
         (year (calendar-extract-year today))
         (today-visible (or (not mon)
                            (<= (abs (calendar-interval mon yr month year)) 1)))
         (in-calendar-window (eq (window-buffer (selected-window))
                                 (get-buffer calendar-buffer))))
    (calendar-generate (or mon month) (or yr year))
    (calendar-update-mode-line)
    (calendar-cursor-to-visible-date
     (if today-visible today (list displayed-month 1 displayed-year)))
    (set-buffer-modified-p nil)
    ;; Don't do any window-related stuff if we weren't called from a
    ;; window displaying the calendar.
    (when in-calendar-window
      (if (window-combined-p)
	  ;; Adjust the window to exactly fit the displayed calendar.
	  (fit-window-to-buffer nil nil calendar-minimum-window-height)
	;; For a full height window or a window that is horizontally
	;; combined don't fit height to that of its buffer.
	(set-window-vscroll nil 0))
      (sit-for 0))
    (and (bound-and-true-p font-lock-mode)
         (font-lock-fontify-buffer))
    (and calendar-mark-holidays-flag
;;;         (calendar-date-is-valid-p today) ; useful for BC dates
         (calendar-mark-holidays)
         (and in-calendar-window (sit-for 0)))
    (unwind-protect
        (if calendar-mark-diary-entries-flag (diary-mark-entries))
      (if today-visible
          (run-hooks 'calendar-today-visible-hook)
        (run-hooks 'calendar-today-invisible-hook)))))

(defun calendar-generate (month year)
  "Generate a three-month Gregorian calendar centered around MONTH, YEAR."
  ;; A negative YEAR is interpreted as BC; -1 being 1 BC, and so on.
  ;; Note that while calendars for years BC could be displayed as it
  ;; stands, almost all other calendar functions (eg holidays) would
  ;; at best have unpredictable results for such dates.
  (if (< (+ month (* 12 (1- year))) 2)
      (error "Months before January, 1 AD cannot be displayed"))
  (setq displayed-month month
        displayed-year year)
  (erase-buffer)
  (calendar-increment-month month year -1)
  (dotimes (i 3)
    (calendar-generate-month month year
                             (+ calendar-left-margin
                                (* calendar-month-width i)))
    (calendar-increment-month month year 1)))

(defun calendar-move-to-column (indent)
  "Like `move-to-column', but indents if the line is too short."
  (if (< (move-to-column indent) indent)
      (indent-to indent)))

(defun calendar-ensure-newline ()
  "Move to the next line, adding a newline if necessary."
  (or (zerop (forward-line 1))
      (insert "\n")))

(defun calendar-insert-at-column (indent string truncate)
  "Move to column INDENT, adding spaces as needed.
Inserts STRING so that it ends at INDENT.  STRING is either a
literal string, or a sexp to evaluate to return such.  Truncates
STRING to length TRUNCATE, and ensures a trailing space."
  (if (not (ignore-errors (stringp (setq string (eval string)))))
      (calendar-move-to-column indent)
    (if (> (string-width string) truncate)
        (setq string (truncate-string-to-width string truncate)))
    (or (string-match " $" string)
        (setq string (concat (if (= (string-width string) truncate)
                                 (substring string 0 -1)
                               string)
                             ;; Avoid inserting text properties unless
                             ;; we have to (ie, non-unit-width chars).
                             ;; This is by no means essential.
                             (if (= (string-width string) (length string))
                                 " "
                               ;; Cribbed from buff-menu.el.
                               (propertize
                                " " 'display `(space :align-to ,indent))))))
    (calendar-move-to-column (- indent (string-width string)))
    (insert string)))

(defun calendar-generate-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted at the top of the buffer in which point is currently
located, but indented INDENT spaces.  The indentation is done from the first
character on the line and does not disturb the first INDENT characters on the
line."
  (let ((blank-days                     ; at start of month
         (mod
          (- (calendar-day-of-week (list month 1 year))
             calendar-week-start-day)
          7))
         (last (calendar-last-day-of-month month year))
         (trunc (min calendar-intermonth-spacing
                     (1- calendar-left-margin)))
         (day 1)
         string)
   (goto-char (point-min))
   (calendar-move-to-column indent)
   (insert
    (calendar-string-spread
     (list (format "%s %d" (calendar-month-name month) year))
     ?\s calendar-month-digit-width))
   (calendar-ensure-newline)
   (calendar-insert-at-column indent calendar-intermonth-header trunc)
   ;; Use the first two characters of each day to head the columns.
   (dotimes (i 7)
     (insert
      (progn
        (setq string
              (calendar-day-name (mod (+ calendar-week-start-day i) 7) nil t))
        (if enable-multibyte-characters
            (truncate-string-to-width string calendar-day-header-width)
          (substring string 0 calendar-day-header-width)))
      (make-string (- calendar-column-width calendar-day-header-width) ?\s)))
   (calendar-ensure-newline)
   (calendar-insert-at-column indent calendar-intermonth-text trunc)
   ;; Add blank days before the first of the month.
   (insert (make-string (* blank-days calendar-column-width) ?\s))
   ;; Put in the days of the month.
   (dotimes (i last)
     (setq day (1+ i))
     ;; TODO should numbers be left-justified, centered...?
     (insert (format (format "%%%dd%%s" calendar-day-digit-width) day
                     (make-string
                      (- calendar-column-width calendar-day-digit-width) ?\s)))
     ;; 'date property prevents intermonth text confusing re-searches.
     ;; (Tried intangible, it did not really work.)
     (set-text-properties
      (- (point) (1+ calendar-day-digit-width)) (1- (point))
      `(mouse-face highlight help-echo ,(eval calendar-date-echo-text)
                   date t))
     (when (and (zerop (mod (+ day blank-days) 7))
                (/= day last))
       (calendar-ensure-newline)
       (setq day (1+ day))              ; first day of next week
       (calendar-insert-at-column indent calendar-intermonth-text trunc)))))

(defun calendar-redraw ()
  "Redraw the calendar display, if `calendar-buffer' is live."
  (interactive)
  (if (get-buffer calendar-buffer)
      (with-current-buffer calendar-buffer
        (let ((cursor-date (calendar-cursor-to-nearest-date)))
          (calendar-generate-window displayed-month displayed-year)
          (calendar-cursor-to-visible-date cursor-date)))))

(defvar calendar-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (dolist (c '(narrow-to-region mark-word mark-sexp mark-paragraph
                 mark-defun mark-whole-buffer mark-page
                 downcase-region upcase-region kill-region
                 copy-region-as-kill capitalize-region write-region))
      (define-key map (vector 'remap c) 'calendar-not-implemented))
    (define-key map "<"     'calendar-scroll-right)
    (define-key map "\C-x<" 'calendar-scroll-right)
    (define-key map [prior] 'calendar-scroll-right-three-months)
    (define-key map "\ev"   'calendar-scroll-right-three-months)
    (define-key map ">"     'calendar-scroll-left)
    (define-key map "\C-x>" 'calendar-scroll-left)
    (define-key map [next]  'calendar-scroll-left-three-months)
    (define-key map "\C-v"  'calendar-scroll-left-three-months)
    (define-key map "\C-b"  'calendar-backward-day)
    (define-key map "\C-p"  'calendar-backward-week)
    (define-key map "\e{"   'calendar-backward-month)
    (define-key map "\C-x[" 'calendar-backward-year)
    (define-key map "\C-f"  'calendar-forward-day)
    (define-key map "\C-n"  'calendar-forward-week)
    (define-key map [left]  'calendar-backward-day)
    (define-key map [up]    'calendar-backward-week)
    (define-key map [right] 'calendar-forward-day)
    (define-key map [down]  'calendar-forward-week)
    (define-key map "\e}"   'calendar-forward-month)
    (define-key map "\C-x]" 'calendar-forward-year)
    (define-key map "\C-a"  'calendar-beginning-of-week)
    (define-key map "\C-e"  'calendar-end-of-week)
    (define-key map "\ea"   'calendar-beginning-of-month)
    (define-key map "\ee"   'calendar-end-of-month)
    (define-key map "\e<"   'calendar-beginning-of-year)
    (define-key map "\e>"   'calendar-end-of-year)
    (define-key map "\C-@"  'calendar-set-mark)
    ;; Many people are used to typing C-SPC and getting C-@.
    (define-key map [?\C-\s] 'calendar-set-mark)
    (define-key map "\C-x\C-x" 'calendar-exchange-point-and-mark)
    (define-key map "\e=" 'calendar-count-days-region)
    (define-key map "gd"  'calendar-goto-date)
    (define-key map "gD"  'calendar-goto-day-of-year)
    (define-key map "gj"  'calendar-julian-goto-date)
    (define-key map "ga"  'calendar-astro-goto-day-number)
    (define-key map "gh"  'calendar-hebrew-goto-date)
    (define-key map "gi"  'calendar-islamic-goto-date)
    (define-key map "gb"  'calendar-bahai-goto-date)
    (define-key map "gC"  'calendar-chinese-goto-date)
    (define-key map "gk"  'calendar-coptic-goto-date)
    (define-key map "ge"  'calendar-ethiopic-goto-date)
    (define-key map "gp"  'calendar-persian-goto-date)
    (define-key map "gc"  'calendar-iso-goto-date)
    (define-key map "gw"  'calendar-iso-goto-week)
    (define-key map "gf"  'calendar-french-goto-date)
    (define-key map "gml"  'calendar-mayan-goto-long-count-date)
    (define-key map "gmpc" 'calendar-mayan-previous-round-date)
    (define-key map "gmnc" 'calendar-mayan-next-round-date)
    (define-key map "gmph" 'calendar-mayan-previous-haab-date)
    (define-key map "gmnh" 'calendar-mayan-next-haab-date)
    (define-key map "gmpt" 'calendar-mayan-previous-tzolkin-date)
    (define-key map "gmnt" 'calendar-mayan-next-tzolkin-date)
    (define-key map "Aa"   'appt-add)
    (define-key map "Ad"   'appt-delete)
    (define-key map "S"   'calendar-sunrise-sunset)
    (define-key map "M"   'calendar-lunar-phases)
    (define-key map " "   'scroll-other-window)
    (define-key map "\d"  'scroll-other-window-down)
    (define-key map "\C-c\C-l" 'calendar-redraw)
    (define-key map "."   'calendar-goto-today)
    (define-key map "o"   'calendar-other-month)
    (define-key map "q"   'calendar-exit)
    (define-key map "a"   'calendar-list-holidays)
    (define-key map "h"   'calendar-cursor-holidays)
    (define-key map "x"   'calendar-mark-holidays)
    (define-key map "u"   'calendar-unmark)
    (define-key map "m"   'diary-mark-entries)
    (define-key map "d"   'diary-view-entries)
    (define-key map "D"   'diary-view-other-diary-entries)
    (define-key map "s"   'diary-show-all-entries)
    (define-key map "pd"  'calendar-print-day-of-year)
    (define-key map "pC"  'calendar-chinese-print-date)
    (define-key map "pk"  'calendar-coptic-print-date)
    (define-key map "pe"  'calendar-ethiopic-print-date)
    (define-key map "pp"  'calendar-persian-print-date)
    (define-key map "pc"  'calendar-iso-print-date)
    (define-key map "pj"  'calendar-julian-print-date)
    (define-key map "pa"  'calendar-astro-print-day-number)
    (define-key map "ph"  'calendar-hebrew-print-date)
    (define-key map "pi"  'calendar-islamic-print-date)
    (define-key map "pb"  'calendar-bahai-print-date)
    (define-key map "pf"  'calendar-french-print-date)
    (define-key map "pm"  'calendar-mayan-print-date)
    (define-key map "po"  'calendar-print-other-dates)
    (define-key map "id"  'diary-insert-entry)
    (define-key map "iw"  'diary-insert-weekly-entry)
    (define-key map "im"  'diary-insert-monthly-entry)
    (define-key map "iy"  'diary-insert-yearly-entry)
    (define-key map "ia"  'diary-insert-anniversary-entry)
    (define-key map "ib"  'diary-insert-block-entry)
    (define-key map "ic"  'diary-insert-cyclic-entry)
    (define-key map "ihd" 'diary-hebrew-insert-entry)
    (define-key map "ihm" 'diary-hebrew-insert-monthly-entry)
    (define-key map "ihy" 'diary-hebrew-insert-yearly-entry)
    (define-key map "iid" 'diary-islamic-insert-entry)
    (define-key map "iim" 'diary-islamic-insert-monthly-entry)
    (define-key map "iiy" 'diary-islamic-insert-yearly-entry)
    (define-key map "iBd" 'diary-bahai-insert-entry)
    (define-key map "iBm" 'diary-bahai-insert-monthly-entry)
    (define-key map "iBy" 'diary-bahai-insert-yearly-entry)
    (define-key map "?"   'calendar-goto-info-node)
    (define-key map "Hm" 'cal-html-cursor-month)
    (define-key map "Hy" 'cal-html-cursor-year)
    (define-key map "tm" 'cal-tex-cursor-month)
    (define-key map "tM" 'cal-tex-cursor-month-landscape)
    (define-key map "td" 'cal-tex-cursor-day)
    (define-key map "tw1" 'cal-tex-cursor-week)
    (define-key map "tw2" 'cal-tex-cursor-week2)
    (define-key map "tw3" 'cal-tex-cursor-week-iso)
    (define-key map "tw4" 'cal-tex-cursor-week-monday)
    (define-key map "tfd" 'cal-tex-cursor-filofax-daily)
    (define-key map "tfw" 'cal-tex-cursor-filofax-2week)
    (define-key map "tfW" 'cal-tex-cursor-filofax-week)
    (define-key map "tfy" 'cal-tex-cursor-filofax-year)
    (define-key map "ty" 'cal-tex-cursor-year)
    (define-key map "tY" 'cal-tex-cursor-year-landscape)

    (define-key map [menu-bar edit] 'undefined)
    (define-key map [menu-bar search] 'undefined)

    (easy-menu-define nil map nil cal-menu-sunmoon-menu)
    (easy-menu-define nil map nil cal-menu-diary-menu)
    (easy-menu-define nil map nil cal-menu-holidays-menu)
    (easy-menu-define nil map nil cal-menu-goto-menu)
    (easy-menu-define nil map nil cal-menu-scroll-menu)

    ;; These are referenced in the default calendar-date-echo-text.
    (define-key map [down-mouse-3]
      (easy-menu-binding cal-menu-context-mouse-menu))
    (define-key map [down-mouse-2]
      (easy-menu-binding cal-menu-global-mouse-menu))

    ;; cf scroll-bar.el.
    (if (and (boundp 'x-toolkit-scroll-bars) x-toolkit-scroll-bars)
        (define-key map [vertical-scroll-bar mouse-1]
          'calendar-scroll-toolkit-scroll)
      ;; Left-click moves us forward in time, right-click backwards.
      (define-key map [vertical-scroll-bar mouse-1] 'calendar-scroll-left)
      (define-key map [vertical-scroll-bar drag-mouse-1] 'calendar-scroll-left)
      ;; down-mouse-2 stays as scroll-bar-drag.
      (define-key map [vertical-scroll-bar mouse-3] 'calendar-scroll-right)
      (define-key map [vertical-scroll-bar drag-mouse-3]
        'calendar-scroll-right))
    map)
  "Keymap for `calendar-mode'.")

;; Calendar mode is suitable only for specially formatted data.
(put 'calendar-mode 'mode-class 'special)

(defun calendar-mode-line-entry (command echo &optional key string)
  "Return a propertized string for `calendar-mode-line-format'.
COMMAND is a command to run, ECHO is the help-echo text, KEY
is COMMAND's keybinding, STRING describes the binding."
  (propertize (or key
                  (substitute-command-keys
                   (format "\\<calendar-mode-map>\\[%s] %s" command string)))
              'help-echo (format "mouse-1: %s" echo)
              'mouse-face 'mode-line-highlight
              'keymap (make-mode-line-mouse-map 'mouse-1 command)))

;; After calendar-mode-map.
(defcustom calendar-mode-line-format
  (list
   (calendar-mode-line-entry 'calendar-scroll-right "previous month" "<")
   "Calendar"
   (concat
    (calendar-mode-line-entry 'calendar-goto-info-node "read Info on Calendar"
                              nil "info")
    " / "
    (calendar-mode-line-entry 'calendar-other-month "choose another month"
                              nil "other")
    " / "
    (calendar-mode-line-entry 'calendar-goto-today "go to today's date"
                              nil "today"))
   '(calendar-date-string (calendar-current-date) t)
   (calendar-mode-line-entry 'calendar-scroll-left "next month" ">"))
  "The mode line of the calendar buffer.
This is a list of items that evaluate to strings.  The elements
are evaluated and concatenated, evenly separated by blanks.
During evaluation, the variable `date' is available as the date
nearest the cursor (or today's date if that fails).  To update
the mode-line as the cursor moves, add `calendar-update-mode-line'
to `calendar-move-hook'.  Here is an example that has the Hebrew date,
the day number/days remaining in the year, and the ISO week/year numbers:

  (list
   \"\"
   '(calendar-hebrew-date-string date)
   '(let* ((year (calendar-extract-year date))
           (d (calendar-day-number date))
           (days-remaining
            (- (calendar-day-number (list 12 31 year)) d)))
      (format \"%d/%d\" d days-remaining))
   '(let* ((d (calendar-absolute-from-gregorian date))
           (iso-date (calendar-iso-from-absolute d)))
      (format \"ISO week %d of %d\"
        (calendar-extract-month iso-date)
        (calendar-extract-year iso-date)))
   \"\"))"
  :risky t
  :type 'sexp
  :group 'calendar)

(defun calendar-goto-info-node ()
  "Go to the info node for the calendar."
  (interactive)
  (info "(emacs)Calendar/Diary")
  (fit-window-to-buffer))

(defvar calendar-mark-ring nil
  "Used by `calendar-set-mark'.")

(define-derived-mode calendar-mode nil "Calendar"
  "A major mode for the calendar window.
For a complete description, see the info node `Calendar/Diary'.

\\<calendar-mode-map>\\{calendar-mode-map}"
  (setq buffer-read-only t
        buffer-undo-list t
        indent-tabs-mode nil)
  (set (make-local-variable 'scroll-margin) 0) ; bug#10379
  (calendar-update-mode-line)
  (make-local-variable 'calendar-mark-ring)
  (make-local-variable 'displayed-month) ; month in middle of window
  (make-local-variable 'displayed-year)  ; year in middle of window
  ;; Most functions only work if displayed-month and displayed-year are set,
  ;; so let's make sure they're always set.  Most likely, this will be reset
  ;; soon in calendar-generate, but better safe than sorry.
  (unless (boundp 'displayed-month) (setq displayed-month 1))
  (unless (boundp 'displayed-year)  (setq displayed-year  2001))
  (set (make-local-variable 'font-lock-defaults)
       '(calendar-font-lock-keywords t)))

(defun calendar-string-spread (strings char length)
  "Concatenate list of STRINGS separated with copies of CHAR to fill LENGTH.
The effect is like mapconcat but the separating pieces are as balanced as
possible.  Each item of STRINGS is evaluated before concatenation so it can
actually be an expression that evaluates to a string.  If LENGTH is too short,
the STRINGS are just concatenated and the result truncated."
;; The algorithm is based on equation (3.25) on page 85 of Concrete
;; Mathematics by Ronald L. Graham, Donald E. Knuth, and Oren Patashnik,
;; Addison-Wesley, Reading, MA, 1989.
  (let* ((strings (mapcar 'eval
                          (if (< (length strings) 2)
                              (append (list "") strings (list ""))
                            strings)))
         (n (- length (string-width (apply 'concat strings))))
         (m (* (1- (length strings)) (char-width char)))
         (s (car strings))
         (strings (cdr strings))
         (i 0))
    (dolist (string strings)
      (setq s (concat s
                      (make-string (max 0 (/ (+ n i) m)) char)
                      string)
            i (1+ i)))
    (truncate-string-to-width s length)))

(defun calendar-update-mode-line ()
  "Update the calendar mode line with the current date and date style."
  (if (bufferp (get-buffer calendar-buffer))
      (with-current-buffer calendar-buffer
        (let ((start (- calendar-left-margin 2))
              (date (condition-case nil
                        (calendar-cursor-to-nearest-date)
                      (error (calendar-current-date)))))
          (setq mode-line-format
                (concat (make-string (max 0 (+ start
                                               (- (car (window-inside-edges))
                                                  (car (window-edges))))) ?\s)
                        (calendar-string-spread
                         (mapcar 'eval calendar-mode-line-format)
                         ?\s (- calendar-right-margin (1- start))))))
        (force-mode-line-update))))

(defun calendar-window-list ()
  "List of all calendar-related windows."
  (let ((calendar-buffers (calendar-buffer-list))
        list)
    ;; Using 0 rather than t for last argument - see bug#2199.
    ;; This is only used with calendar-hide-window, which ignores
    ;; iconified frames anyway, so could use 'visible rather than 0.
    (walk-windows (lambda (w)
                    (if (memq (window-buffer w) calendar-buffers)
                        (push w list)))
                  nil 0)
    list))

(defun calendar-buffer-list ()
  "List of all calendar-related buffers (as buffers, not strings)."
  (let (buffs)
    (dolist (b (list calendar-hebrew-yahrzeit-buffer lunar-phases-buffer
                     holiday-buffer diary-fancy-buffer solar-sunrises-buffer
                     (get-file-buffer diary-file)
                     calendar-buffer calendar-other-calendars-buffer))
      (and b (setq b (get-buffer b))
           (push b buffs)))
    buffs))

(defun calendar-exit ()
  "Get out of the calendar window and hide it and related buffers."
  (interactive)
  (let ((diary-buffer (get-file-buffer diary-file)))
    (if (or (not diary-buffer)
            (not (buffer-modified-p diary-buffer))
            (yes-or-no-p
             "Diary modified; do you really want to exit the calendar? "))
        ;; Need to do this multiple times because one time can replace some
        ;; calendar-related buffers with other calendar-related buffers.
        (mapc (lambda (x)
                (mapc 'calendar-hide-window (calendar-window-list)))
              (calendar-window-list)))))

(define-obsolete-function-alias 'exit-calendar 'calendar-exit "23.1")

(defun calendar-hide-window (window)
  "Hide WINDOW if it is calendar-related."
  (let ((buffer (if (window-live-p window) (window-buffer window))))
    (if (memq buffer (calendar-buffer-list))
        (cond
         ((and (display-multi-frame-p)
               (eq 'icon (cdr (assoc 'visibility
                                     (frame-parameters
                                      (window-frame window))))))
          nil)
         ((and (display-multi-frame-p) (window-dedicated-p window))
          (if calendar-remove-frame-by-deleting
              (delete-frame (window-frame window))
              (iconify-frame (window-frame window))))
         ((not (and (select-window window) (one-window-p window)))
          (delete-window window))
         (t (set-buffer buffer)
            (bury-buffer))))))

(defun calendar-current-date (&optional offset)
  "Return the current date in a list (month day year).
Optional integer OFFSET is a number of days from the current date."
  (let* ((now (decode-time))
         (now (list (nth 4 now) (nth 3 now) (nth 5 now))))
    (if (zerop (or offset 0))
        now
      (calendar-gregorian-from-absolute
       (+ offset (calendar-absolute-from-gregorian now))))))

(defun calendar-column-to-segment ()
  "Convert current column to calendar month \"segment\".
The left-most month returns 0, the next right 1, and so on."
  (let ((col (max 0 (+ (current-column)
                       (/ calendar-intermonth-spacing 2)
                       (- calendar-left-margin)))))
    (/ col (+ (* 7 calendar-column-width) calendar-intermonth-spacing))))

(defun calendar-cursor-to-date (&optional error event)
  "Return a list (month day year) of current cursor position.
If cursor is not on a specific date, signals an error if optional parameter
ERROR is non-nil, otherwise just returns nil.
If EVENT is non-nil, it's an event indicating the buffer position to
use instead of point."
  (with-current-buffer
      (if event (window-buffer (posn-window (event-start event)))
        (current-buffer))
    (save-excursion
      (and event (setq event (event-start event))
           (goto-char (posn-point event)))
      (let* ((segment (calendar-column-to-segment))
             (month (% (+ displayed-month (1- segment)) 12)))
        ;; Call with point on either of the two digits in a 2-digit date,
        ;; or on or before the digit of a 1-digit date.
        (if (not (and (looking-at "[ 0-9]?[0-9][^0-9]")
                      (get-text-property (point) 'date)))
            (if error (error "Not on a date!"))
          ;; Convert segment to real month and year.
          (if (zerop month) (setq month 12))
          ;; Go back to before the first date digit.
          (or (looking-at " ")
              (re-search-backward "[^0-9]"))
          (list month
                (string-to-number
                 (buffer-substring (1+ (point))
                                   (+ 1 calendar-day-digit-width (point))))
                (cond
                 ((and (= 12 month) (zerop segment)) (1- displayed-year))
                 ((and (= 1 month) (= segment 2)) (1+ displayed-year))
                 (t displayed-year))))))))

(add-to-list 'debug-ignored-errors "Not on a date!")

;; The following version of calendar-gregorian-from-absolute is preferred for
;; reasons of clarity, BUT it's much slower than the version that follows it.

;;(defun calendar-gregorian-from-absolute (date)
;;  "Compute the list (month day year) corresponding to the absolute DATE.
;;The absolute date is the number of days elapsed since the (imaginary)
;;Gregorian date Sunday, December 31, 1 BC."
;;  (let* ((approx (/ date 366)) ; approximation from below
;;         (year                ; search forward from the approximation
;;          (+ approx
;;             (calendar-sum y approx
;;                 (>= date (calendar-absolute-from-gregorian (list 1 1 (1+ y))))
;;                  1)))
;;         (month                         ; search forward from January
;;          (1+ (calendar-sum m 1
;;                   (> date
;;                      (calendar-absolute-from-gregorian
;;                       (list m (calendar-last-day-of-month m year) year)))
;;                   1)))
;;         (day                      ; calculate the day by subtraction
;;          (- date
;;             (1- (calendar-absolute-from-gregorian (list month 1 year))))))
;;    (list month day year)))

(defun calendar-gregorian-from-absolute (date)
  "Compute the list (month day year) corresponding to the absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC.  This function does not
handle dates in years BC."
  ;; See the footnote on page 384 of ``Calendrical Calculations, Part II:
  ;; Three Historical Calendars'' by E. M. Reingold,  N. Dershowitz, and S. M.
  ;; Clamen, Software--Practice and Experience, Volume 23, Number 4
  ;; (April, 1993), pages 383-404 for an explanation.
  (let* ((d0 (1- date))
         (n400 (/ d0 146097))
         (d1 (% d0 146097))
         (n100 (/ d1 36524))
         (d2 (% d1 36524))
         (n4 (/ d2 1461))
         (d3 (% d2 1461))
         (n1 (/ d3 365))
         (day (1+ (% d3 365)))
         (year (+ (* 400 n400) (* 100 n100) (* n4 4) n1))
         (month 1)
         mdays)
    (if (or (= n100 4) (= n1 4))
        (list 12 31 year)
      (setq year (1+ year))
      (while (< (setq mdays (calendar-last-day-of-month month year)) day)
        (setq day (- day mdays)
              month (1+ month)))
      (list month day year))))

(defun calendar-other-month (month year &optional event)
  "Display a three-month calendar centered around MONTH and YEAR.
EVENT is an event like `last-nonmenu-event'."
  (interactive (let ((event (list last-nonmenu-event)))
                 (append (calendar-read-date 'noday) event)))
  (save-selected-window
    (and event
         (setq event (event-start event))
         (select-window (posn-window event)))
    (unless (and (= month displayed-month)
                 (= year displayed-year))
      (let ((old-date (calendar-cursor-to-date))
            (today (calendar-current-date)))
        (calendar-generate-window month year)
        (calendar-cursor-to-visible-date
         (cond
          ((calendar-date-is-visible-p old-date) old-date)
          ((calendar-date-is-visible-p today) today)
          (t (list month 1 year))))))))

(defun calendar-set-mark (arg &optional event)
  "Mark the date under the cursor, or jump to marked date.
With no prefix argument, push current date onto marked date ring.
With argument ARG, jump to mark, pop it, and put point at end of ring."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let ((date (calendar-cursor-to-date t event)))
    (if arg
        (if (null calendar-mark-ring)
            (error "No mark set in this buffer")
          (calendar-goto-date (car calendar-mark-ring))
          (setq calendar-mark-ring
                (cdr (nconc calendar-mark-ring (list date)))))
      (push date calendar-mark-ring)
      ;; Since the top of the mark ring is the marked date in the
      ;; calendar, the mark ring in the calendar is one longer than
      ;; in other buffers to get the same effect.
      (if (> (length calendar-mark-ring) (1+ mark-ring-max))
          (setcdr (nthcdr mark-ring-max calendar-mark-ring) nil))
      (message "Mark set"))))

(defun calendar-exchange-point-and-mark ()
  "Exchange the current cursor position with the marked date."
  (interactive)
  (let ((mark (car calendar-mark-ring))
        (date (calendar-cursor-to-date t)))
    (if (null mark)
        (error "No mark set in this buffer")
      (setq calendar-mark-ring (cons date (cdr calendar-mark-ring)))
      (calendar-goto-date mark))))

(defun calendar-count-days-region ()
  "Count the number of days (inclusive) between point and the mark."
  (interactive)
  (let* ((days (- (calendar-absolute-from-gregorian
                   (calendar-cursor-to-date t))
                  (calendar-absolute-from-gregorian
                   (or (car calendar-mark-ring)
                       (error "No mark set in this buffer")))))
         (days (1+ (if (> days 0) days (- days)))))
    (message "Region has %d day%s (inclusive)"
             days (if (> days 1) "s" ""))))

(defun calendar-not-implemented ()
  "Not implemented."
  (interactive)
  (error "%s not available in the calendar"
         (global-key-binding (this-command-keys))))

(defun calendar-read (prompt acceptable &optional initial-contents)
  "Return an object read from the minibuffer.
Prompt with the string PROMPT and use the function ACCEPTABLE to decide if
entered item is acceptable.  If non-nil, optional third arg INITIAL-CONTENTS
is a string to insert in the minibuffer before reading."
  (let ((value (read-minibuffer prompt initial-contents)))
    (while (not (funcall acceptable value))
      (setq value (read-minibuffer prompt initial-contents)))
    value))


(defun calendar-customized-p (symbol)
  "Return non-nil if SYMBOL has been customized."
  (and (default-boundp symbol)
       (let ((standard (get symbol 'standard-value)))
         (and standard
              (not (equal (eval (car standard)) (default-value symbol)))))))

(defun calendar-abbrev-construct (full)
  "From sequence FULL, return a vector of abbreviations.
Each abbreviation is no longer than `calendar-abbrev-length' characters."
  (apply 'vector (mapcar
                  (lambda (f)
                    (substring f 0 (min calendar-abbrev-length (length f))))
                  full)))

(defcustom calendar-day-name-array
  ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
  "Array of capitalized strings giving, in order from Sunday, the day names.
The first two characters of each string will be used to head the
day columns in the calendar.
If you change this without using customize after the calendar has loaded,
then you may also want to change `calendar-day-abbrev-array'."
  :group 'calendar
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (let ((dcustomized (calendar-customized-p 'calendar-day-abbrev-array))
               (hcustomized (calendar-customized-p 'cal-html-day-abbrev-array)))
           (set symbol value)
           (or dcustomized
               (setq calendar-day-abbrev-array
                     (calendar-abbrev-construct calendar-day-name-array)))
           (and (not hcustomized)
                (boundp 'cal-html-day-abbrev-array)
                (setq cal-html-day-abbrev-array calendar-day-abbrev-array))))
  :type '(vector (string :tag "Sunday")
                 (string :tag "Monday")
                 (string :tag "Tuesday")
                 (string :tag "Wednesday")
                 (string :tag "Thursday")
                 (string :tag "Friday")
                 (string :tag "Saturday")))

(defcustom calendar-abbrev-length 3
  "Default length of abbreviations to use for day and month names.
If you change this without using customize after the calendar has loaded,
then you may also want to change `calendar-day-abbrev-array' and
`calendar-month-abbrev-array'."
  :group 'calendar
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (let ((dcustomized (calendar-customized-p 'calendar-day-abbrev-array))
               (mcustomized (calendar-customized-p
                             'calendar-month-abbrev-array))
               (hcustomized (calendar-customized-p 'cal-html-day-abbrev-array)))
           (set symbol value)
           (or dcustomized
               (setq calendar-day-abbrev-array
                     (calendar-abbrev-construct calendar-day-name-array)))
           (or mcustomized
               (setq calendar-month-abbrev-array
                     (calendar-abbrev-construct calendar-month-name-array)))
           (and (not hcustomized)
                (boundp 'cal-html-day-abbrev-array)
                (setq cal-html-day-abbrev-array calendar-day-abbrev-array))))
  :type 'integer)

(defcustom calendar-day-abbrev-array
  (calendar-abbrev-construct calendar-day-name-array)
  "Array of capitalized strings giving the abbreviated day names.
The order should be the same as that of the full names specified
in `calendar-day-name-array'.  These abbreviations may be used
instead of the full names in the diary file.  Do not include a
trailing `.' in the strings specified in this variable, though
you may use such in the diary file.  By default, each string is
the first `calendar-abbrev-length' characters of the corresponding
full name."
  :group 'calendar
  :initialize 'custom-initialize-default
  :set-after '(calendar-abbrev-length calendar-day-name-array)
  :set (lambda (symbol value)
         (let ((hcustomized (calendar-customized-p 'cal-html-day-abbrev-array)))
           (set symbol value)
           (and (not hcustomized)
                (boundp 'cal-html-day-abbrev-array)
                (setq cal-html-day-abbrev-array calendar-day-abbrev-array))))
  :type '(vector (string :tag "Sun")
                 (string :tag "Mon")
                 (string :tag "Tue")
                 (string :tag "Wed")
                 (string :tag "Thu")
                 (string :tag "Fri")
                 (string :tag "Sat"))
  ;; Made defcustom, changed defaults from nil nil...
  :version "24.1")

(defcustom calendar-month-name-array
  ["January" "February" "March"     "April"   "May"      "June"
   "July"    "August"   "September" "October" "November" "December"]
  "Array of capitalized strings giving, in order, the month names.
If you change this without using customize after the calendar has loaded,
then you may also want to change `calendar-month-abbrev-array'."
  :group 'calendar
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (let ((mcustomized (calendar-customized-p
                            'calendar-month-abbrev-array)))
           (set symbol value)
           (or mcustomized
               (setq calendar-month-abbrev-array
                     (calendar-abbrev-construct calendar-month-name-array)))))
  :type '(vector (string :tag "January")
                 (string :tag "February")
                 (string :tag "March")
                 (string :tag "April")
                 (string :tag "May")
                 (string :tag "June")
                 (string :tag "July")
                 (string :tag "August")
                 (string :tag "September")
                 (string :tag "October")
                 (string :tag "November")
                 (string :tag "December")))

(defcustom calendar-month-abbrev-array
  (calendar-abbrev-construct calendar-month-name-array)
 "Array of capitalized strings giving the abbreviated month names.
The order should be the same as that of the full names specified
in `calendar-month-name-array'.  These abbreviations are used in
the calendar menu entries, and can also be used in the diary
file.  Do not include a trailing `.' in the strings specified in
this variable, though you may use such in the diary file.  By
default, each string is the first ``calendar-abbrev-length'
characters of the corresponding full name."
 :group 'calendar
 :set-after '(calendar-abbrev-length calendar-month-name-array)
 :type '(vector (string :tag "Jan")
                (string :tag "Feb")
                (string :tag "Mar")
                (string :tag "Apr")
                (string :tag "May")
                (string :tag "Jun")
                (string :tag "Jul")
                (string :tag "Aug")
                (string :tag "Sep")
                (string :tag "Oct")
                (string :tag "Nov")
                (string :tag "Dec"))
 ;; Made defcustom, changed defaults from nil nil...
 :version "24.1")

(defun calendar-make-alist (sequence &optional start-index filter
                                     &rest sequences)
  "Return an association list corresponding to SEQUENCE.
Associates each element of SEQUENCE with an incremented integer,
starting from START-INDEX (default 1).  Applies the function FILTER,
if provided, to each key in the alist.  Repeats the process, with
indices starting from START-INDEX each time, for any remaining
arguments SEQUENCES."
  (or start-index (setq start-index 1))
  (let (index alist)
    (mapc (lambda (seq)
            (setq index start-index)
            (mapc (lambda (elem)
                    (setq alist (cons
                                 (cons (if filter (funcall filter elem) elem)
                                       index)
                                 alist)
                          index (1+ index)))
                  seq))
          (append (list sequence) sequences))
    (reverse alist)))

(defun calendar-read-date (&optional noday)
  "Prompt for Gregorian date.  Return a list (month day year).
If optional NODAY is t, does not ask for day, but just returns
\(month 1 year); if NODAY is any other non-nil value the value returned is
\(month year)"
  (let* ((year (calendar-read
                "Year (>0): "
                (lambda (x) (> x 0))
                (number-to-string (calendar-extract-year
                                (calendar-current-date)))))
         (month-array calendar-month-name-array)
         (completion-ignore-case t)
         (month (cdr (assoc-string
                       (completing-read
                        "Month name: "
                        (mapcar 'list (append month-array nil))
                        nil t)
                      (calendar-make-alist month-array 1) t)))
         (last (calendar-last-day-of-month month year)))
    (if noday
        (if (eq noday t)
            (list month 1 year)
          (list month year))
      (list month
            (calendar-read (format "Day (1-%d): " last)
                           (lambda (x) (and (< 0 x) (<= x last))))
            year))))

(defun calendar-interval (mon1 yr1 mon2 yr2)
  "The number of months difference between MON1, YR1 and MON2, YR2.
The result is positive if the second date is later than the first.
Negative years are interpreted as years BC; -1 being 1 BC, and so on."
  (if (< yr1 0) (setq yr1 (1+ yr1)))      ; -1 BC -> 0 AD, etc
  (if (< yr2 0) (setq yr2 (1+ yr2)))
  (+ (* 12 (- yr2 yr1))
     (- mon2 mon1)))

(defvar calendar-font-lock-keywords
  `((,(concat (regexp-opt (mapcar 'identity calendar-month-name-array) t)
              " -?[0-9]+")
     . font-lock-function-name-face) ; month and year
    (,(regexp-opt
       (list (substring (aref calendar-day-name-array 6)
                        0 calendar-day-header-width)
             (substring (aref calendar-day-name-array 0)
                        0 calendar-day-header-width)))
     ;; Saturdays and Sundays are highlighted differently.
     . font-lock-comment-face)
    ;; First two chars of each day are used in the calendar.
    (,(regexp-opt (mapcar (lambda (x) (substring x 0 calendar-day-header-width))
                          calendar-day-name-array))
     . font-lock-reference-face))
  "Default keywords to highlight in Calendar mode.")

(defun calendar-day-name (date &optional abbrev absolute)
  "Return a string with the name of the day of the week of DATE.
DATE should be a list in the format (MONTH DAY YEAR), unless the
optional argument ABSOLUTE is non-nil, in which case DATE should
be an integer in the range 0 to 6 corresponding to the day of the
week.  Day names are taken from the variable `calendar-day-name-array',
unless the optional argument ABBREV is non-nil, in which case
the variable `calendar-day-abbrev-array' is used."
  (aref (if abbrev calendar-day-abbrev-array calendar-day-name-array)
        (if absolute date (calendar-day-of-week date))))

(defun calendar-month-name (month &optional abbrev)
  "Return a string with the name of month number MONTH.
Months are numbered from one.  Month names are taken from the
variable `calendar-month-name-array', unless the optional
argument ABBREV is non-nil, in which case
`calendar-month-abbrev-array' is used."
  (aref (if abbrev calendar-month-abbrev-array calendar-month-name-array)
        (1- month)))

(defun calendar-day-of-week (date)
  "Return the day-of-the-week index of DATE, 0 for Sunday, 1 for Monday, etc.
DATE is a list of the form (month day year).  A negative year is
interpreted as BC; -1 being 1 BC, and so on."
  (mod (calendar-absolute-from-gregorian date) 7))

(defun calendar-week-end-day ()
  "Return the index (0 for Sunday, etc.) of the last day of the week."
  (mod (+ calendar-week-start-day 6) 7))

(defun calendar-unmark ()
  "Delete all diary/holiday marks/highlighting from the calendar."
  (interactive)
  (setq calendar-mark-holidays-flag nil
        calendar-mark-diary-entries-flag nil)
  (with-current-buffer calendar-buffer
    (mapc 'delete-overlay (overlays-in (point-min) (point-max)))))

(defun calendar-date-is-visible-p (date)
  "Return non-nil if DATE is valid and is visible in the calendar window."
  (and (calendar-date-is-valid-p date)
       (< (abs (calendar-interval
                displayed-month displayed-year
                (calendar-extract-month date) (calendar-extract-year date)))
          2)))

;; FIXME can this be generalized for holiday-chinese?
(defun calendar-nongregorian-visible-p (month day toabs fromabs switch)
  "Return non-nil if MONTH, DAY is visible in the calendar window.
MONTH and DAY are in some non-Gregorian calendar system.  The
functions TOABS and FROMABS convert that system to and from
absolute, respectively.  SWITCH is a function that takes a single
argument (a local month number).  It applies when the local year
changes across the calendar window, and returns non-nil if the
specified month should be associated with the higher year.
Returns the corresponding Gregorian date."
  ;; We need to choose the local year associated with month and day
  ;; that might make them visible.
  (let* ((m1 displayed-month)
         (y1 displayed-year)
         (m2 displayed-month)
         (y2 displayed-year)
         ;; Absolute date of first/last dates in calendar window.
         (start-date (progn
                       (calendar-increment-month m1 y1 -1)
                       (calendar-absolute-from-gregorian (list m1 1 y1))))
         (end-date (progn
                     (calendar-increment-month m2 y2 1)
                     (calendar-absolute-from-gregorian
                      (list m2 (calendar-last-day-of-month m2 y2) y2))))
         ;; Local date of first/last date in calendar window.
         (local-start (funcall fromabs start-date))
         (local-end (funcall fromabs end-date))
         ;; Local year of first/last dates.
         ;; Can only differ if displayed-month = 12, 1, 2.
         (local-y1 (calendar-extract-year local-start))
         (local-y2 (calendar-extract-year local-end))
         ;; Choose which year might be visible in the window.
         ;; Obviously it only matters when y1 and y2 differ, ie
         ;; when the _local_ new year is visible.
         (year (if (funcall switch month) local-y2 local-y1))
         (date (calendar-gregorian-from-absolute
                (funcall toabs (list month day year)))))
    (if (calendar-date-is-visible-p date)
        date)))

(defun calendar-date-is-valid-p (date)
  "Return t if DATE is a valid date."
  (let ((month (calendar-extract-month date))
        (day (calendar-extract-day date))
        (year (calendar-extract-year date)))
    (and (<= 1 month) (<= month 12)
         ;; (calendar-read-date t) used to return a date with day = nil.
         ;; Should not be valid (?), since many funcs prob assume integer.
         ;; (calendar-read-date 'noday) returns (month year), which
         ;; currently results in calendar-extract-year returning nil.
         day year (<= 1 day) (<= day (calendar-last-day-of-month month year))
         ;; BC dates left as non-valid, to suppress errors from
         ;; complex holiday algorithms not suitable for years BC.
         ;; Note there are side effects on calendar navigation.
         (<= 1 year))))

(define-obsolete-function-alias 'calendar-date-is-legal-p
    'calendar-date-is-valid-p "23.1")

(defun calendar-date-equal (date1 date2)
  "Return t if the DATE1 and DATE2 are the same."
  (and
   (= (calendar-extract-month date1) (calendar-extract-month date2))
   (= (calendar-extract-day date1) (calendar-extract-day date2))
   (= (calendar-extract-year date1) (calendar-extract-year date2))))

(defun calendar-make-temp-face (attrlist)
  "Return a temporary face based on the attributes in ATTRLIST.
ATTRLIST is a list with elements of the form :face face :foreground color."
  (let ((attrs attrlist)
        faceinfo face temp-face)
    ;; Separate :face from the other attributes.  Use the last :face
    ;; if there are more than one.  FIXME is merging meaningful?
    (while attrs
      (if (eq (car attrs) :face)
          (setq face (intern-soft (cadr attrs))
                attrs (cddr attrs))
        (push (car attrs) faceinfo)
        (setq attrs (cdr attrs))))
    (or (facep face) (setq face 'default))
    (if (not faceinfo)
        ;; No attributes to apply, so just use an existing-face.
        face
      ;; FIXME should we be using numbered temp-faces, re-using where poss?
      (setq temp-face
            (make-symbol
             (concat ":caltemp"
                     (mapconcat (lambda (sym)
                                  (cond
                                   ((symbolp sym) (symbol-name sym))
                                   ((numberp sym) (number-to-string sym))
                                   (t sym)))
                                attrlist ""))))
      (make-face temp-face)
      (copy-face face temp-face)
      ;; Apply the font aspects.
      (apply 'set-face-attribute temp-face nil (nreverse faceinfo))
      temp-face)))

(defun calendar-mark-visible-date (date &optional mark)
  "Mark DATE in the calendar window with MARK.
MARK is a single-character string, a list of face attributes/values, or a face.
MARK defaults to `diary-entry-marker'."
  (if (calendar-date-is-valid-p date)
      (with-current-buffer calendar-buffer
        (save-excursion
          (calendar-cursor-to-visible-date date)
          (setq mark
                (or (and (stringp mark) (= (length mark) 1) mark) ; single-char
                    ;; The next two use to also check font-lock-mode.
                    ;; See comments above diary-entry-marker for why
                    ;; this was dropped.
;;;                    (and font-lock-mode
;;;                         (or
                          (and (listp mark) (> (length mark) 0) mark) ; attrs
                          (and (facep mark) mark) ; )) face-name
                          diary-entry-marker))
          (cond
           ;; Face or an attr-list that contained a face.
           ((facep mark)
            (overlay-put
             (make-overlay (1- (point)) (1+ (point))) 'face mark))
           ;; Single-character mark, goes after the date.
           ((and (stringp mark) (= (length mark) 1))
            (overlay-put
             (make-overlay (1+ (point)) (+ 2 (point))) 'display mark))
           (t                           ; attr list
            (overlay-put
             (make-overlay (1- (point)) (1+ (point))) 'face
             (calendar-make-temp-face mark))))))))

(define-obsolete-function-alias 'mark-visible-calendar-date
  'calendar-mark-visible-date "23.1")

(defun calendar-star-date ()
  "Replace the date under the cursor in the calendar window with asterisks.
You might want to add this function to `calendar-today-visible-hook'."
  (unless (catch 'found
            (dolist (ol (overlays-at (point)))
              (and (overlay-get ol 'calendar-star)
                   (throw 'found t))))
    (let ((ol (make-overlay (1- (point)) (point))))
      (overlay-put ol 'display "*")
      (overlay-put ol 'calendar-star t)
      ;; Use copy-sequence to avoid merging of identical 'display props.
      ;; Use two overlays so as not to mess up
      ;; calendar-cursor-to-nearest-date (and calendar-forward-day).
      (overlay-put (setq ol (make-overlay (point) (1+ (point))))
                   'display (copy-sequence "*"))
      (overlay-put ol 'calendar-star t))))

(defun calendar-mark-today ()
  "Mark the date under the cursor in the calendar window.
The date is marked with `calendar-today-marker'.  You might want to add
this function to `calendar-today-visible-hook'."
  (calendar-mark-visible-date (calendar-cursor-to-date) calendar-today-marker))

;; FIXME why the car? Almost every usage calls list on the args.
(defun calendar-date-compare (date1 date2)
  "Return t if DATE1 is before DATE2, nil otherwise.
The actual dates are in the car of DATE1 and DATE2."
  (< (calendar-absolute-from-gregorian (car date1))
     (calendar-absolute-from-gregorian (car date2))))

(defun calendar-date-string (date &optional abbreviate nodayname)
  "A string form of DATE, driven by the variable `calendar-date-display-form'.
An optional parameter ABBREVIATE, when non-nil, causes the month
and day names to be abbreviated as specified by
`calendar-month-abbrev-array' and `calendar-day-abbrev-array',
respectively.  An optional parameter NODAYNAME, when t, omits the
name of the day of the week."
  (let* ((dayname (unless nodayname (calendar-day-name date abbreviate)))
         (month (calendar-extract-month date))
         (monthname (calendar-month-name month abbreviate))
         (day (number-to-string (calendar-extract-day date)))
         (month (number-to-string month))
         (year (number-to-string (calendar-extract-year date))))
    (mapconcat 'eval calendar-date-display-form "")))

(defun calendar-dayname-on-or-before (dayname date)
  "Return the absolute date of the DAYNAME on or before absolute DATE.
DAYNAME=0 means Sunday, DAYNAME=1 means Monday, and so on.

Note: Applying this function to d+6 gives us the DAYNAME on or after an
absolute day d.  Similarly, applying it to d+3 gives the DAYNAME nearest to
absolute date d, applying it to d-1 gives the DAYNAME previous to absolute
date d, and applying it to d+7 gives the DAYNAME following absolute date d."
  (- date (% (- date dayname) 7)))

(defun calendar-nth-named-absday (n dayname month year &optional day)
  "Absolute date of the Nth DAYNAME after/before MONTH YEAR DAY.
A DAYNAME of 0 means Sunday, 1 means Monday, and so on.
If N>0, return the Nth DAYNAME after MONTH DAY, YEAR (inclusive).
If N<0, return the Nth DAYNAME before MONTH DAY, YEAR (inclusive).
DAY defaults to 1 if N>0, and MONTH's last day otherwise."
  (if (> n 0)
      (+ (* 7 (1- n))
         (calendar-dayname-on-or-before
          dayname
          (+ 6 (calendar-absolute-from-gregorian
                (list month (or day 1) year)))))
    (+ (* 7 (1+ n))
       (calendar-dayname-on-or-before
        dayname
        (calendar-absolute-from-gregorian
         (list month
               (or day (calendar-last-day-of-month month year))
               year))))))

(defun calendar-nth-named-day (n dayname month year &optional day)
  "Date of the Nth DAYNAME after/before MONTH YEAR DAY.
Like `calendar-nth-named-absday', but returns a Gregorian date."
  (calendar-gregorian-from-absolute
   (calendar-nth-named-absday n dayname month year day)))

(defun calendar-day-of-year-string (&optional date)
  "String of day number of year of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((d (or date (calendar-current-date)))
         (year (calendar-extract-year d))
         (day (calendar-day-number d))
         (days-remaining (- (calendar-day-number (list 12 31 year)) day)))
    (format "Day %d of %d; %d day%s remaining in the year"
            day year days-remaining (if (= days-remaining 1) "" "s"))))

(defun calendar-other-dates (date)
  "Return a list of strings giving Gregorian DATE in other calendars.
DATE is (month day year).  Calendars that do not apply are omitted."
  (let (odate)
    (delq nil
          (list
           (calendar-day-of-year-string date)
           (format "ISO date: %s" (calendar-iso-date-string date))
           (format "Julian date: %s"
                   (calendar-julian-date-string date))
           (format "Astronomical (Julian) day number (at noon UTC): %s.0"
                   (calendar-astro-date-string date))
           (format "Fixed (RD) date: %s"
                   (calendar-absolute-from-gregorian date))
           (format "Hebrew date (before sunset): %s"
                   (calendar-hebrew-date-string date))
           (format "Persian date: %s"
                   (calendar-persian-date-string date))
           (unless (string-equal
                    (setq odate (calendar-islamic-date-string date))
                    "")
             (format "Islamic date (before sunset): %s" odate))
           (unless (string-equal
                    (setq odate (calendar-bahai-date-string date))
                    "")
             (format "Bahá'í date: %s" odate))
           (format "Chinese date: %s"
                   (calendar-chinese-date-string date))
           (unless (string-equal
                    (setq odate (calendar-coptic-date-string date))
                    "")
             (format "Coptic date: %s" odate))
           (unless (string-equal
                    (setq odate (calendar-ethiopic-date-string date))
                    "")
             (format "Ethiopic date: %s" odate))
           (unless (string-equal
                    (setq odate (calendar-french-date-string date))
                    "")
             (format "French Revolutionary date: %s" odate))
           (format "Mayan date: %s"
                   (calendar-mayan-date-string date))))))

(declare-function x-popup-menu "menu.c" (position menu))

(defun calendar-print-other-dates (&optional event)
  "Show dates on other calendars for date under the cursor.
If called by a mouse-event, pops up a menu with the result."
  (interactive (list last-nonmenu-event))
  (let* ((date (calendar-cursor-to-date t event))
         (title (format "%s (Gregorian)" (calendar-date-string date)))
         (others (calendar-other-dates date))
         selection)
    (if (mouse-event-p event)
        (and (setq selection (cal-menu-x-popup-menu event title
                               (mapcar 'list others)))
             (call-interactively selection))
      (calendar-in-read-only-buffer calendar-other-calendars-buffer
        (calendar-set-mode-line title)
        (insert (mapconcat 'identity others "\n"))))))

(defun calendar-print-day-of-year ()
  "Show day number in year/days remaining in year for date under the cursor."
  (interactive)
  (message "%s" (calendar-day-of-year-string (calendar-cursor-to-date t))))

(defun calendar-set-mode-line (str)
  "Set mode line to STR, centered, surrounded by dashes."
  (let* ((edges (window-edges))
         ;; As per doc of window-width, total visible mode-line length.
         (width (- (nth 2 edges) (car edges))))
    ;; Hack for --daemon.  See bug #2199.
    ;; If no frame exists yet, we have no idea what width to use.
    (and (= width 10)
         (not window-system)
         (setq width (string-to-number (or (getenv "COLUMNS") "80"))))
    (setq mode-line-format
          (if buffer-file-name
              `("-" mode-line-modified
                ,(calendar-string-spread (list str) ?- (- width 6))
                "---")
            (calendar-string-spread (list str) ?- width)))))

(defun calendar-version ()
  "Display the Calendar version."
  (interactive)
  (message "GNU Emacs %s" emacs-version))

(make-obsolete 'calendar-version 'emacs-version "23.1")


(run-hooks 'calendar-load-hook)

(provide 'calendar)

;; Local variables:
;; byte-compile-dynamic: t
;; coding: utf-8
;; End:

;;; calendar.el ends here
