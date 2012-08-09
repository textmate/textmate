;;; cal-tex.el --- calendar functions for printing calendars with LaTeX

;; Copyright (C) 1995, 2001-2012  Free Software Foundation, Inc.

;; Author: Steve Fisk <fisk@bowdoin.edu>
;;         Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Calendar, LaTeX
;; Package: calendar

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

;; This collection of functions implements the creation of LaTeX calendars
;; based on the user's holiday choices and diary file.

;; The user commands are:
;; cal-tex-cursor-year
;; cal-tex-cursor-year-landscape
;; cal-tex-cursor-filofax-year
;; cal-tex-cursor-month-landscape
;; cal-tex-cursor-month
;; cal-tex-cursor-week
;; cal-tex-cursor-week2
;; cal-tex-cursor-week-iso
;; cal-tex-cursor-week-monday
;; cal-tex-cursor-filofax-2week
;; cal-tex-cursor-filofax-week
;; cal-tex-cursor-filofax-daily
;; cal-tex-cursor-day

;; TO DO
;;
;;     (*)  Add holidays and diary entries to daily calendar.
;;
;;     (*)  Add diary entries to weekly calendar functions.
;;
;;     (*)  Make calendar styles for A4 paper.
;;
;;     (*)  Make monthly styles Filofax paper.

;;; Code:

(require 'calendar)

;;;
;;; Customizable variables
;;;

(defgroup calendar-tex nil
  "Options for printing calendar with LaTeX."
  :prefix "cal-tex-"
  :group 'calendar)

(defcustom cal-tex-which-days '(0 1 2 3 4 5 6)
  "The days of the week that are displayed on the portrait monthly calendar.
Sunday is 0, Monday is 1, and so on.  The default is to print from Sunday to
Saturday.  For example, use '(1 3 5) to only print Monday, Wednesday, Friday."
  :type '(repeat integer)
  :group 'calendar-tex)

(defcustom cal-tex-holidays t
  "Non-nil means holidays are printed in the LaTeX calendars that support it.
Setting this to nil may speed up calendar generation."
  :type 'boolean
  :group 'calendar-tex)

(defcustom cal-tex-diary nil
  "Non-nil means diary entries are printed in LaTeX calendars that support it.
At present, this only affects the monthly, filofax, and iso-week
calendars (i.e. not the yearly, plain weekly, or daily calendars).
Setting this to nil may speed up calendar generation."
  :type 'boolean
  :group 'calendar-tex)

(defcustom cal-tex-rules nil
  "Non-nil means pages will be ruled in some LaTeX calendar styles.
At present, this only affects the daily filofax calendar."
  :type 'boolean
  :group 'calendar-tex)

(defcustom cal-tex-daily-string
  '(let* ((year (calendar-extract-year date))
          (day (calendar-day-number date))
          (days-remaining (- (calendar-day-number (list 12 31 year)) day)))
     (format "%d/%d" day days-remaining))
  "Lisp expression giving the date format to use in the LaTeX calendars.
This should be an expression involving the variable `date'.  When
this expression is called, `date' is a list of the form '(MONTH DAY YEAR)'.

The string resulting from evaluating this expression is placed at
the bottom center of each date in monthly calendars, next to the
date in the weekly calendars, and in the top center of daily calendars.

The default is ordinal day number of the year and the number of
days remaining.  As an example, setting this to

    '(calendar-hebrew-date-string date)

will put the Hebrew date at the bottom of each day."
  :type 'sexp
  :group 'calendar-tex)

(defcustom cal-tex-buffer "calendar.tex"
  "The name for the output LaTeX calendar buffer."
  :type 'string
  :group 'calendar-tex)

(defcustom cal-tex-24 nil
  "Non-nil means use a 24 hour clock in the daily calendar."
  :type 'boolean
  :group 'calendar-tex)

(defcustom cal-tex-daily-start 8
  "The first hour of the daily LaTeX calendar page.
At present, this only affects `cal-tex-cursor-day'."
  :type 'integer
  :group 'calendar-tex)

(defcustom cal-tex-daily-end 20
  "The last hour of the daily LaTeX calendar page.
At present, this only affects `cal-tex-cursor-day'."
  :type 'integer
  :group 'calendar-tex)

(defcustom cal-tex-preamble-extra nil
  "A string giving extra LaTeX commands to insert in the calendar preamble.
For example, to include extra packages:
\"\\\\usepackage{foo}\\n\\\\usepackage{bar}\\n\"."
  :type '(choice (const nil)
                 ;; An example to help people format things in custom.
                 (string :value "\\usepackage{foo}\n\\usepackage{bar}\n"))
  :group 'calendar-tex
  :version "22.1")

(defcustom cal-tex-hook nil
  "List of functions called after any LaTeX calendar buffer is generated.
You can use this to do post-processing on the buffer.  For example, to change
characters with diacritical marks to their LaTeX equivalents, use
    (add-hook 'cal-tex-hook
              (lambda () (iso-iso2tex (point-min) (point-max))))"
  :type 'hook
  :group 'calendar-tex)

(defcustom cal-tex-year-hook nil
  "List of functions called after a LaTeX year calendar buffer is generated."
  :type 'hook
  :group 'calendar-tex)

(defcustom cal-tex-month-hook nil
  "List of functions called after a LaTeX month calendar buffer is generated."
  :type 'hook
  :group 'calendar-tex)

(defcustom cal-tex-week-hook nil
  "List of functions called after a LaTeX week calendar buffer is generated."
  :type 'hook
  :group 'calendar-tex)

(defcustom cal-tex-daily-hook nil
  "List of functions called after a LaTeX daily calendar buffer is generated."
  :type 'hook
  :group 'calendar-tex)

;;;
;;; Definitions for LaTeX code
;;;

(defconst cal-tex-day-prefix "\\caldate{%s}{%s}"
  "The initial LaTeX code for a day.
The holidays, diary entries, bottom string, and the text follow.")

(defconst cal-tex-day-name-format "\\myday{%s}%%"
  "The format for LaTeX code for a day name.
The names are taken from `calendar-day-name-array'.")

(defconst cal-tex-cal-one-month
  "\\def\\calmonth#1#2%
{\\begin{center}%
\\Huge\\bf\\uppercase{#1} #2 \\\\[1cm]%
\\end{center}}%
\\vspace*{-1.5cm}%
%
"
  "LaTeX code for the month header, for a single month calendar.")

(defconst cal-tex-cal-multi-month
  "\\def\\calmonth#1#2#3#4%
{\\begin{center}%
\\Huge\\bf #1 #2---#3 #4\\\\[1cm]%
\\end{center}}%
\\vspace*{-1.5cm}%
%
"
  "LaTeX code for the month header, for a multi-month calendar.")

(defconst cal-tex-myday
  "\\renewcommand{\\myday}[1]%
{\\makebox[\\cellwidth]{\\hfill\\large\\bf#1\\hfill}}
%
"
  "LaTeX code for a day heading.")

(defconst cal-tex-caldate
"\\fboxsep=0pt
\\long\\def\\caldate#1#2#3#4#5#6{%
    \\fbox{\\hbox to\\cellwidth{%
     \\vbox to\\cellheight{%
       \\hbox to\\cellwidth{%
          {\\hspace*{1mm}\\Large \\bf \\strut #2}\\hspace{.05\\cellwidth}%
          \\raisebox{\\holidaymult\\cellheight}%
                   {\\parbox[t]{.75\\cellwidth}{\\tiny \\raggedright #4}}}
       \\hbox to\\cellwidth{%
           \\hspace*{1mm}\\parbox{.95\\cellwidth}{\\tiny \\raggedright #3}}
       \\hspace*{1mm}%
       \\hbox to\\cellwidth{#6}%
       \\vfill%
       \\hbox to\\cellwidth{\\hfill \\tiny #5 \\hfill}%
       \\vskip 1.4pt}%
     \\hskip -0.4pt}}}
"
  "LaTeX code to insert one box with date info in calendar.
This definition is the heart of the calendar!")

(autoload 'calendar-holiday-list "holidays")

(defun cal-tex-list-holidays (d1 d2)
  "Generate a list of all holidays from absolute date D1 to D2."
  (let* ((start (calendar-gregorian-from-absolute d1))
         (displayed-month (calendar-extract-month start))
         (displayed-year (calendar-extract-year start))
         (end (calendar-gregorian-from-absolute d2))
         (end-month (calendar-extract-month end))
         (end-year (calendar-extract-year end))
         (number-of-intervals
          (1+ (/ (calendar-interval displayed-month displayed-year
                                    end-month end-year)
                 3)))
         holidays in-range a)
    (calendar-increment-month displayed-month displayed-year 1)
    (dotimes (_idummy number-of-intervals)
      (setq holidays (append holidays (calendar-holiday-list)))
      (calendar-increment-month displayed-month displayed-year 3))
    (dolist (hol holidays)
      (and (car hol)
           (setq a (calendar-absolute-from-gregorian (car hol)))
           (and (<= d1 a) (<= a d2))
           (setq in-range (append (list hol) in-range))))
    in-range))

(autoload 'diary-list-entries "diary-lib")

(defun cal-tex-list-diary-entries (d1 d2)
  "Generate a list of all diary-entries from absolute date D1 to D2."
  (let (diary-list-include-blanks)
    (diary-list-entries (calendar-gregorian-from-absolute d1)
                        (1+ (- d2 d1)) t)))

(defun cal-tex-preamble (&optional args)
  "Insert the LaTeX calendar preamble into `cal-tex-buffer'.
Preamble includes initial definitions for various LaTeX commands.
Optional string ARGS are included as options for the article document class."
  ;; FIXME use generate-new-buffer, and adjust cal-tex-end-document.
  (set-buffer (get-buffer-create cal-tex-buffer))
  (insert (format "\\documentclass%s{article}\n"
                  (if (stringp args)
                      (format "[%s]" args)
                    "")))
  (if (stringp cal-tex-preamble-extra)
      (insert cal-tex-preamble-extra "\n"))
  (insert "\\hbadness 20000
\\hfuzz=1000pt
\\vbadness 20000
\\lineskip 0pt
\\marginparwidth 0pt
\\oddsidemargin  -2cm
\\evensidemargin -2cm
\\marginparsep   0pt
\\topmargin      0pt
\\textwidth      7.5in
\\textheight     9.5in
\\newlength{\\cellwidth}
\\newlength{\\cellheight}
\\newlength{\\boxwidth}
\\newlength{\\boxheight}
\\newlength{\\cellsize}
\\newcommand{\\myday}[1]{}
\\newcommand{\\caldate}[6]{}
\\newcommand{\\nocaldate}[6]{}
\\newcommand{\\calsmall}[6]{}
%
"))

;;;
;;;  Yearly calendars
;;;

;;;###cal-autoload
(defun cal-tex-cursor-year (&optional n event)
  "Make a buffer with LaTeX commands for the year cursor is on.
Optional prefix argument N specifies number of years.
Optional EVENT indicates a buffer position to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (cal-tex-year (calendar-extract-year (calendar-cursor-to-date t event))
                (or n 1)))

;;;###cal-autoload
(defun cal-tex-cursor-year-landscape (&optional n event)
  "Make a buffer with LaTeX commands for the year cursor is on.
Optional prefix argument N specifies number of years.
Optional EVENT indicates a buffer position to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (cal-tex-year (calendar-extract-year (calendar-cursor-to-date t event))
                (or n 1) t))

(defun cal-tex-year (year n &optional landscape)
  "Make a one page yearly calendar of YEAR; do this for N years.
There are four rows of three months each, unless optional
LANDSCAPE is non-nil, in which case the calendar is printed in
landscape mode with three rows of four months each."
  (cal-tex-insert-preamble 1 landscape "12pt")
  (if landscape
      (cal-tex-vspace "-.6cm")
    (cal-tex-vspace "-3.1cm"))
  (dotimes (j n)
    (insert "\\vfill%\n")
    (cal-tex-b-center)
    (cal-tex-Huge (number-to-string year))
    (cal-tex-e-center)
    (cal-tex-vspace "1cm")
    (cal-tex-b-center)
    (cal-tex-b-parbox "l" (if landscape "5.9in" "4.3in"))
    (insert "\n")
    (cal-tex-noindent)
    (cal-tex-nl)
    (dotimes (i 12)
      (insert (cal-tex-mini-calendar (1+ i) year "month" "1.1in" "1in"))
      (insert "\\month")
      (cal-tex-hspace "0.5in")
      (if (zerop (mod (1+ i) (if landscape 4 3)))
          (cal-tex-nl "0.5in")))
    (cal-tex-e-parbox)
    (cal-tex-e-center)
    (insert "\\vfill%\n")
    (setq year (1+ year))
    (if (= j (1- n))
        (cal-tex-end-document)
      (cal-tex-newpage))
    (run-hooks 'cal-tex-year-hook))
  (run-hooks 'cal-tex-hook))

;;;###cal-autoload
(defun cal-tex-cursor-filofax-year (&optional n event)
  "Make a Filofax one page yearly calendar of year indicated by cursor.
Optional prefix argument N specifies number of years.
Optional EVENT indicates a buffer position to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (or n (setq n 1))
  (let ((year (calendar-extract-year (calendar-cursor-to-date t event))))
    (cal-tex-preamble "twoside")
    (cal-tex-cmd "\\textwidth 3.25in")
    (cal-tex-cmd "\\textheight 6.5in")
    (cal-tex-cmd "\\oddsidemargin 1.675in")
    (cal-tex-cmd "\\evensidemargin 1.675in")
    (cal-tex-cmd "\\topmargin 0pt")
    (cal-tex-cmd "\\headheight -0.875in")
    (cal-tex-cmd "\\fboxsep 0.5mm")
    (cal-tex-cmd "\\pagestyle{empty}")
    (cal-tex-b-document)
    (cal-tex-cmd "\\vspace*{0.25in}")
    (dotimes (j n)
      (insert (format "\\hfil \\textbf{\\Large %s} \\hfil\\\\\n" year))
      (cal-tex-b-center)
      (cal-tex-b-parbox "l" "\\textwidth")
      (insert "\n")
      (cal-tex-noindent)
      (cal-tex-nl)
      (let ((month-names; don't use default in case user changed it
             ;; These are only used to define the command names, not
             ;; the names of the months they insert.
             ["January" "February" "March" "April" "May" "June"
              "July" "August" "September" "October" "November" "December"]))
        (dotimes (i 12)
          (insert (cal-tex-mini-calendar (1+ i) year (aref month-names i)
                                         "1in" ".9in" "tiny" "0.6mm"))))
      (insert
       "\\noindent\\fbox{\\January}\\fbox{\\February}\\fbox{\\March}\\\\
\\noindent\\fbox{\\April}\\fbox{\\May}\\fbox{\\June}\\\\
\\noindent\\fbox{\\July}\\fbox{\\August}\\fbox{\\September}\\\\
\\noindent\\fbox{\\October}\\fbox{\\November}\\fbox{\\December}
")
      (cal-tex-e-parbox)
      (cal-tex-e-center)
      (setq year (1+ year))
      (if (= j (1- n))
          (cal-tex-end-document)
        (cal-tex-newpage)
        (cal-tex-cmd "\\vspace*{0.25in}"))
      (run-hooks 'cal-tex-year-hook))
    (run-hooks 'cal-tex-hook)))

;;;
;;;  Monthly calendars
;;;

;;;###cal-autoload
(defun cal-tex-cursor-month-landscape (&optional n event)
  "Make a LaTeX calendar buffer for the month the cursor is on.
Optional prefix argument N specifies number of months to be
produced (default 1).  The output is in landscape format, one
month to a page.  It shows holiday and diary entries if
`cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.
Optional EVENT indicates a buffer position to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (or n (setq n 1))
  (let* ((date (calendar-cursor-to-date t event))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (end-month month)
         (end-year year)
         (cal-tex-which-days '(0 1 2 3 4 5 6))
         (d1 (calendar-absolute-from-gregorian (list month 1 year)))
         (d2 (progn
               (calendar-increment-month end-month end-year (1- n))
               (calendar-absolute-from-gregorian
                (list end-month
                      (calendar-last-day-of-month end-month end-year)
                      end-year))))
         (diary-list (if cal-tex-diary (cal-tex-list-diary-entries d1 d2)))
         (holidays (if cal-tex-holidays (cal-tex-list-holidays d1 d2)))
         other-month other-year small-months-at-start)
    (cal-tex-insert-preamble (cal-tex-number-weeks month year 1) t "12pt")
    (cal-tex-cmd cal-tex-cal-one-month)
    (dotimes (i n)
      (setq other-month month
            other-year year)
      (calendar-increment-month other-month other-year -1)
      (insert (cal-tex-mini-calendar other-month other-year "lastmonth"
                                     "\\cellwidth" "\\cellheight"))
      (calendar-increment-month other-month other-year 2)
      (insert (cal-tex-mini-calendar other-month other-year "nextmonth"
                                     "\\cellwidth" "\\cellheight"))
      (cal-tex-insert-month-header 1 month year month year)
      (cal-tex-insert-day-names)
      (cal-tex-nl ".2cm")
      (if (setq small-months-at-start
                (< 1 (mod (- (calendar-day-of-week (list month 1 year))
                               calendar-week-start-day)
                          7)))
          (insert "\\lastmonth\\nextmonth\\hspace*{-2\\cellwidth}"))
      (cal-tex-insert-blank-days month year cal-tex-day-prefix)
      (cal-tex-insert-days month year diary-list holidays
                           cal-tex-day-prefix)
      (cal-tex-insert-blank-days-at-end month year cal-tex-day-prefix)
      (if (and (not small-months-at-start)
               (< 1 (mod (- (1- calendar-week-start-day)
                            (calendar-day-of-week
                             (list month
                                   (calendar-last-day-of-month month year)
                                   year)))
                         7)))
          (insert "\\vspace*{-\\cellwidth}\\hspace*{-2\\cellwidth}"
                  "\\lastmonth\\nextmonth%
"))
      (unless (= i (1- n))
        (run-hooks 'cal-tex-month-hook)
        (cal-tex-newpage)
        (calendar-increment-month month year 1)
        (cal-tex-vspace "-2cm")
        (cal-tex-insert-preamble
         (cal-tex-number-weeks month year 1) t "12pt" t))))
  (cal-tex-end-document)
  (run-hooks 'cal-tex-hook))

;;;###cal-autoload
(defun cal-tex-cursor-month (&optional n event)
  "Make a LaTeX calendar buffer for the month the cursor is on.
Optional prefix argument N specifies number of months to be
produced (default 1).  The calendar is condensed onto one page.
It shows holiday and diary entries if `cal-tex-holidays' and
`cal-tex-diary', respectively, are non-nil.  Optional EVENT
indicates a buffer position to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (or n (setq n 1))
  (let* ((date (calendar-cursor-to-date t event))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (end-month month)
         (end-year year)
         ;; FIXME -landscape sets cal-tex-which-days?
         (d1 (calendar-absolute-from-gregorian (list month 1 year)))
         (d2 (progn
               (calendar-increment-month end-month end-year (1- n))
               (calendar-absolute-from-gregorian
                (list end-month
                      (calendar-last-day-of-month end-month end-year)
                      end-year))))
         (diary-list (if cal-tex-diary (cal-tex-list-diary-entries d1 d2)))
         (holidays (if cal-tex-holidays (cal-tex-list-holidays d1 d2))))
    (cal-tex-insert-preamble (cal-tex-number-weeks month year n) nil "12pt")
    (if (> n 1)
        (cal-tex-cmd cal-tex-cal-multi-month)
      (cal-tex-cmd cal-tex-cal-one-month))
    (cal-tex-insert-month-header n month year end-month end-year)
    (cal-tex-insert-day-names)
    (cal-tex-nl ".2cm")
    (cal-tex-insert-blank-days month year cal-tex-day-prefix)
    (dotimes (_idummy n)
      (cal-tex-insert-days month year diary-list holidays cal-tex-day-prefix)
      (when (= (calendar-week-end-day)
               (calendar-day-of-week
                (list month
                      (calendar-last-day-of-month month year)
                      year))) ; last day of month was last day of week
        (cal-tex-hfill)
        (cal-tex-nl))
      (calendar-increment-month month year 1))
    (cal-tex-insert-blank-days-at-end end-month end-year cal-tex-day-prefix))
  (cal-tex-end-document)
  (run-hooks 'cal-tex-hook))

(defun cal-tex-insert-days (month year diary-list holidays day-format)
  "Insert LaTeX commands for a range of days in monthly calendars.
LaTeX commands are inserted for the days of the MONTH in YEAR.
Diary entries on DIARY-LIST are included.  Holidays on HOLIDAYS
are included.  Each day is formatted using format DAY-FORMAT."
  (let ((blank-days                     ; at start of month
         (mod
          (- (calendar-day-of-week (list month 1 year))
             calendar-week-start-day)
          7))
        (last (calendar-last-day-of-month month year))
        date j)
    (dotimes (i last)
      (setq j (1+ i)                    ; 1-last, incl
            date (list month j year))
      (when (memq (calendar-day-of-week date) cal-tex-which-days)
        (insert (format day-format (cal-tex-month-name month) j))
        (cal-tex-arg (cal-tex-latexify-list diary-list date))
        (cal-tex-arg (cal-tex-latexify-list holidays date))
        (cal-tex-arg (eval cal-tex-daily-string))
        (cal-tex-arg)
        (cal-tex-comment))
      (when (and (zerop (mod (+ j blank-days) 7))
                 (/= j last))
        (cal-tex-hfill)
        (cal-tex-nl)))))

(defun cal-tex-insert-day-names ()
  "Insert the names of the days at top of a monthly calendar."
  (let (j)
    (dotimes (i 7)
      (if (memq (setq j (mod (+ calendar-week-start-day i) 7))
                cal-tex-which-days)
          (insert (format cal-tex-day-name-format
                          (cal-tex-LaTeXify-string
                           (aref calendar-day-name-array j)))))
      (cal-tex-comment))))

(defun cal-tex-insert-month-header (n month year end-month end-year)
  "Create a title for a calendar.
A title is inserted for a calendar with N months starting with
MONTH YEAR and ending with END-MONTH END-YEAR."
  (let ((month-name (cal-tex-month-name month))
        (end-month-name (cal-tex-month-name end-month)))
    (if (= 1 n)
        (insert (format "\\calmonth{%s}{%s}\n\\vspace*{-0.5cm}"
                        month-name year) )
      (insert (format "\\calmonth{%s}{%s}{%s}{%s}\n\\vspace*{-0.5cm}"
                      month-name year end-month-name end-year))))
  (cal-tex-comment))

(defun cal-tex-insert-blank-days (month year day-format)
  "Insert code for initial days not in calendar.
Insert LaTeX code for the blank days at the beginning of the MONTH in
YEAR.  The entry is formatted using DAY-FORMAT.  If the entire week is
blank, no days are inserted."
  (if (cal-tex-first-blank-p month year)
      (let ((blank-days                ; at start of month
             (mod
              (- (calendar-day-of-week (list month 1 year))
                 calendar-week-start-day)
              7)))
        (dotimes (i blank-days)
          (if (memq (mod (+ calendar-week-start-day i) 7) cal-tex-which-days)
              (insert (format day-format " " " ") "{}{}{}{}%\n"))))))

(defun cal-tex-insert-blank-days-at-end (month year day-format)
  "Insert code for final days not in calendar.
Insert LaTeX code for the blank days at the end of the MONTH in YEAR.
The entry is formatted using DAY-FORMAT."
  (if (cal-tex-last-blank-p month year)
      (let* ((last-day (calendar-last-day-of-month month year))
             (blank-days                ; at end of month
              (mod
               (- (calendar-day-of-week (list month last-day year))
                  calendar-week-start-day)
               7))
             (i blank-days))
        (while (<= (setq i (1+ i)) 6)
          (if (memq (mod (+ calendar-week-start-day i) 7) cal-tex-which-days)
              (insert (format day-format "" "") "{}{}{}{}%\n"))))))

(defun cal-tex-first-blank-p (month year)
  "Determine if any days of the first week will be printed.
Return t if there will there be any days of the first week printed
in the calendar starting in MONTH YEAR."
  ;; Check days 1-7 of the month, until we find the last day of the week.
  (catch 'found
    (let (dow)
      (dotimes (i 7)
        (if (memq (setq dow (calendar-day-of-week (list month (1+ i) year)))
                  cal-tex-which-days)
            (throw 'found t)
          (if (= dow (calendar-week-end-day)) (throw 'found nil)))))))

(defun cal-tex-last-blank-p (month year)
  "Determine if any days of the last week will be printed.
Return t if there will there be any days of the last week printed
in the calendar starting in MONTH YEAR."
  ;; Check backwards from the last day of the month, until we find the
  ;; start of the last week in the month.
  (catch 'found
    (let ((last-day (calendar-last-day-of-month month year))
          dow)
      (dotimes (i 7)
        (if (memq (setq dow (calendar-day-of-week
                             (list month (- last-day i) year)))
                  cal-tex-which-days)
            (throw 'found t)
          (if (= dow calendar-week-start-day) (throw 'found nil)))))))

(defun cal-tex-number-weeks (month year n)
  "Determine the number of weeks in a range of dates.
Compute the number of weeks in the calendar starting with MONTH and YEAR,
and lasting N months, including only the days in WHICH-DAYS.  As it stands,
this is only an upper bound."
  (let ((d (list month 1 year)))
    (calendar-increment-month month year (1- n))
    (/ (- (calendar-dayname-on-or-before
           calendar-week-start-day
           (+ 7 (calendar-absolute-from-gregorian
                   (list month (calendar-last-day-of-month month year) year))))
          (calendar-dayname-on-or-before
           calendar-week-start-day
           (calendar-absolute-from-gregorian d)))
       7)))

;;;
;;; Weekly calendars
;;;

(defconst cal-tex-LaTeX-hourbox
  "\\newcommand{\\hourbox}[2]%
{\\makebox[2em]{\\rule{0cm}{#2ex}#1}\\rule{3in}{.15mm}}\n"
  "One hour and a line on the right.")

;; TODO cal-tex-diary-support.
;; TODO respect cal-tex-daily-start,end (see cal-tex-week-hours).
;;;###cal-autoload
(defun cal-tex-cursor-week (&optional n event)
  "Make a LaTeX calendar buffer for a two-page one-week calendar.
It applies to the week that point is in.  The optional prefix
argument N specifies number of weeks (default 1).  The calendar
shows holidays if `cal-tex-holidays' is non-nil (note that diary
entries are not shown).  The calendar shows the hours 8-12am, 1-5pm."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (or n (setq n 1))
  (let* ((date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 calendar-week-start-day
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t event)))))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (d1 (calendar-absolute-from-gregorian date))
         (d2 (+ (* 7 n) d1))
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays d1 d2))))
    (cal-tex-preamble "11pt")
    (cal-tex-cmd "\\textwidth   6.5in")
    (cal-tex-cmd "\\textheight 10.5in")
    (cal-tex-cmd "\\oddsidemargin 0in")
    (cal-tex-cmd "\\evensidemargin 0in")
    (insert cal-tex-LaTeX-hourbox)
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (dotimes (i n)
      (cal-tex-vspace "-1.5in")
      (cal-tex-b-center)
      (cal-tex-Huge-bf (format "\\uppercase{%s}"
                               (cal-tex-month-name month)))
      (cal-tex-hspace "2em")
      (cal-tex-Huge-bf (number-to-string year))
      (cal-tex-nl ".5cm")
      (cal-tex-e-center)
      (cal-tex-hspace "-.2in")
      (cal-tex-b-parbox "l" "7in")
      (dotimes (_jdummy 7)
        (cal-tex-week-hours date holidays "3.1")
        (setq date (cal-tex-incr-date date)))
      (cal-tex-e-parbox)
      (setq month (calendar-extract-month date)
            year (calendar-extract-year date))
      (unless (= i (1- n))
        (run-hooks 'cal-tex-week-hook)
        (cal-tex-newpage)))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

;; TODO cal-tex-diary support.
;; TODO respect cal-tex-daily-start,end (see cal-tex-week-hours).
;;;###cal-autoload
(defun cal-tex-cursor-week2 (&optional n event)
  "Make a LaTeX calendar buffer for a two-page one-week calendar.
It applies to the week that point is in.  Optional prefix
argument N specifies number of weeks (default 1).  The calendar
shows holidays if `cal-tex-holidays' is non-nil (note that diary
entries are not shown).  The calendar shows the hours 8-12am, 1-5pm.
Optional EVENT indicates a buffer position to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (or n (setq n 1))
  (let* ((date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 calendar-week-start-day
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t event)))))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (d1 (calendar-absolute-from-gregorian date))
         (d2 (+ (* 7 n) d1))
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays d1 d2))))
    (cal-tex-preamble "12pt")
    (cal-tex-cmd "\\textwidth   6.5in")
    (cal-tex-cmd "\\textheight 10.5in")
    (cal-tex-cmd "\\oddsidemargin 0in")
    (cal-tex-cmd "\\evensidemargin 0in")
    (insert cal-tex-LaTeX-hourbox)
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (dotimes (i n)
      (cal-tex-vspace "-1.5in")
      (cal-tex-b-center)
      (cal-tex-Huge-bf (format "\\uppercase{%s}"
                               (cal-tex-month-name month)))
      (cal-tex-hspace "2em")
      (cal-tex-Huge-bf (number-to-string year))
      (cal-tex-nl ".5cm")
      (cal-tex-e-center)
      (cal-tex-hspace "-.2in")
      (cal-tex-b-parbox "l" "\\textwidth")
      (dotimes (_jdummy 3)
        (cal-tex-week-hours date holidays "5")
        (setq date (cal-tex-incr-date date)))
      (cal-tex-e-parbox)
      (cal-tex-nl)
      (insert (cal-tex-mini-calendar
               (calendar-extract-month (cal-tex-previous-month date))
               (calendar-extract-year (cal-tex-previous-month date))
               "lastmonth" "1.1in" "1in"))
      (insert (cal-tex-mini-calendar
               (calendar-extract-month date)
               (calendar-extract-year date)
               "thismonth" "1.1in" "1in"))
      (insert (cal-tex-mini-calendar
               (calendar-extract-month (cal-tex-next-month date))
               (calendar-extract-year (cal-tex-next-month date))
               "nextmonth" "1.1in" "1in"))
      (insert "\\hbox to \\textwidth{")
      (cal-tex-hfill)
      (insert "\\lastmonth")
      (cal-tex-hfill)
      (insert "\\thismonth")
      (cal-tex-hfill)
      (insert "\\nextmonth")
      (cal-tex-hfill)
      (insert "}")
      (cal-tex-nl)
      (cal-tex-b-parbox "l" "\\textwidth")
      (dotimes (_jdummy 4)
        (cal-tex-week-hours date holidays "5")
        (setq date (cal-tex-incr-date date)))
      (cal-tex-e-parbox)
      (setq month (calendar-extract-month date)
            year (calendar-extract-year date))
      (unless (= i (1- n))
        (run-hooks 'cal-tex-week-hook)
        (cal-tex-newpage)))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

(autoload 'calendar-iso-from-absolute "cal-iso")

;;;###cal-autoload
(defun cal-tex-cursor-week-iso (&optional n event)
  "Make a LaTeX calendar buffer for a one page ISO-style weekly calendar.
Optional prefix argument N specifies number of weeks (default 1).
The calendar shows holiday and diary entries if
`cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.
It does not show hours of the day.  Optional EVENT indicates a buffer
position to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (or n (setq n 1))
  (let* ((date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 1
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t event)))))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (day (calendar-extract-day date))
         (d1 (calendar-absolute-from-gregorian date))
         (d2 (+ (* 7 n) d1))
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays d1 d2)))
         (diary-list (if cal-tex-diary
                         (cal-tex-list-diary-entries
                          ;; FIXME d1?
                          (calendar-absolute-from-gregorian (list month 1 year))
                          d2)))
         s)
    (cal-tex-preamble "11pt")
    (cal-tex-cmd "\\textwidth 6.5in")
    (cal-tex-cmd "\\textheight 10.5in")
    (cal-tex-cmd "\\oddsidemargin 0in")
    (cal-tex-cmd "\\evensidemargin 0in")
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (dotimes (i n)
      (cal-tex-vspace "-1.5in")
      (cal-tex-b-center)
      (cal-tex-Huge-bf
       (let ((d (calendar-iso-from-absolute
                 (calendar-absolute-from-gregorian date))))
         (format "Week %d of %d"
                 (calendar-extract-month d)
                 (calendar-extract-year d))))
      (cal-tex-nl ".5cm")
      (cal-tex-e-center)
      (cal-tex-b-parbox "l" "\\textwidth")
      (dotimes (_j 7)
        (cal-tex-b-parbox "t" "\\textwidth")
        (cal-tex-b-parbox "t" "\\textwidth")
        (cal-tex-rule "0pt" "\\textwidth" ".2mm")
        (cal-tex-nl)
        (cal-tex-b-parbox "t" "\\textwidth")
        (cal-tex-large-bf (cal-tex-LaTeXify-string (calendar-day-name date)))
        (insert ", ")
        (cal-tex-large-bf (cal-tex-month-name month))
        (insert " ")
        (cal-tex-large-bf (number-to-string day))
        (unless (string-equal "" (setq s (cal-tex-latexify-list
                                          holidays date "; ")))
          (insert ": ")
          (cal-tex-large-bf s))
        (cal-tex-hfill)
        (insert " " (eval cal-tex-daily-string))
        (cal-tex-e-parbox)
        (cal-tex-nl)
        (cal-tex-noindent)
        (cal-tex-b-parbox "t" "\\textwidth")
        (unless (string-equal "" (setq s (cal-tex-latexify-list
                                          diary-list date)))
          (insert "\\vbox to 0pt{")
          (cal-tex-large-bf s)
          (insert "}"))
        (cal-tex-e-parbox)
        (cal-tex-nl)
        (setq date (cal-tex-incr-date date)
              month (calendar-extract-month date)
              day (calendar-extract-day date))
        (cal-tex-e-parbox)
        (cal-tex-e-parbox "2cm")
        (cal-tex-nl)
        (setq month (calendar-extract-month date)
              year (calendar-extract-year date)))
      (cal-tex-e-parbox)
      (unless (= i (1- n))
        (run-hooks 'cal-tex-week-hook)
        (cal-tex-newpage)))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

;; TODO respect cal-tex-daily-start,end?
;; Using different numbers of hours will probably break some layouts.
(defun cal-tex-week-hours (date holidays height)
  "Insert hourly entries for DATE with HOLIDAYS, with line height HEIGHT.
Uses the 24-hour clock if `cal-tex-24' is non-nil.  Note that the hours
shown are hard-coded to 8-12, 13-17."
  (let ((month (calendar-extract-month date))
        (day (calendar-extract-day date))
        (year (calendar-extract-year date))
        morning afternoon s)
  (cal-tex-comment "begin cal-tex-week-hours")
  (cal-tex-cmd  "\\ \\\\[-.2cm]")
  (cal-tex-cmd "\\noindent")
  (cal-tex-b-parbox "l" "6.8in")
  (cal-tex-large-bf (cal-tex-LaTeXify-string (calendar-day-name date)))
  (insert ", ")
  (cal-tex-large-bf (cal-tex-month-name month))
  (insert " ")
  (cal-tex-large-bf (number-to-string day))
  (unless (string-equal "" (setq s (cal-tex-latexify-list
                                    holidays date "; ")))
    (insert ": ")
    (cal-tex-large-bf s))
  (cal-tex-hfill)
  (insert " " (eval cal-tex-daily-string))
  (cal-tex-e-parbox)
  (cal-tex-nl "-.3cm")
  (cal-tex-rule "0pt" "6.8in" ".2mm")
  (cal-tex-nl "-.1cm")
  (dotimes (i 5)
    (setq morning (+ i 8)               ; 8-12 incl
          afternoon (if cal-tex-24
                        (+ i 13)        ; 13-17 incl
                      (1+ i)))          ; 1-5 incl
    (cal-tex-cmd "\\hourbox" (number-to-string morning))
    (cal-tex-arg height)
    (cal-tex-hspace ".4cm")
    (cal-tex-cmd "\\hourbox" (number-to-string afternoon))
    (cal-tex-arg height)
    (cal-tex-nl))))

;; TODO cal-tex-diary support.
;; TODO respect cal-tex-daily-start,end (see cal-tex-weekly4-box).
;;;###cal-autoload
(defun cal-tex-cursor-week-monday (&optional n event)
  "Make a LaTeX calendar buffer for a two-page one-week calendar.
It applies to the week that point is in, and starts on Monday.
Optional prefix argument N specifies number of weeks (default 1).
The calendar shows holidays if `cal-tex-holidays' is
non-nil (note that diary entries are not shown).   The calendar shows
the hours 8-12am, 1-5pm.  Optional EVENT indicates a buffer position
to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (or n (setq n 1))
  (let ((date (calendar-gregorian-from-absolute
               (calendar-dayname-on-or-before
                0
                (calendar-absolute-from-gregorian
                 (calendar-cursor-to-date t event))))))
    (cal-tex-preamble "11pt")
    (cal-tex-cmd "\\textwidth   6.5in")
    (cal-tex-cmd "\\textheight 10.5in")
    (cal-tex-cmd "\\oddsidemargin 0in")
    (cal-tex-cmd "\\evensidemargin 0in")
    (cal-tex-b-document)
    (dotimes (i n)
      (cal-tex-vspace "-1cm")
      (insert "\\noindent ")
      (cal-tex-weekly4-box (cal-tex-incr-date date) nil)
      (cal-tex-weekly4-box (cal-tex-incr-date date 4) nil)
      (cal-tex-nl ".2cm")
      (cal-tex-weekly4-box (cal-tex-incr-date date 2) nil)
      (cal-tex-weekly4-box (cal-tex-incr-date date 5) nil)
      (cal-tex-nl ".2cm")
      (cal-tex-weekly4-box (cal-tex-incr-date date 3) nil)
      (cal-tex-weekly4-box (cal-tex-incr-date date 6) t)
      (unless (= i (1- n))
        (run-hooks 'cal-tex-week-hook)
        (setq date (cal-tex-incr-date date 7))
        (cal-tex-newpage)))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

;; TODO respect cal-tex-daily-start,end?
;; Using different numbers of hours will probably break some layouts.
(defun cal-tex-weekly4-box (date weekend)
  "Make one box for DATE, different if WEEKEND.
Uses the 24-hour clock if `cal-tex-24' is non-nil.  Note that the hours
shown are hard-coded to 8-12, 13-17."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (dayname (cal-tex-LaTeXify-string (calendar-day-name date)))
         (date1 (cal-tex-incr-date date))
         (day1 (calendar-extract-day date1))
         (month1 (calendar-extract-month date1))
         (year1 (calendar-extract-year date1))
         (dayname1 (cal-tex-LaTeXify-string (calendar-day-name date1))))
    (cal-tex-b-framebox "8cm" "l")
    (cal-tex-b-parbox "b" "7.5cm")
    (insert (format "\\textbf{\\Large %s,} %s/%s/%s\\\\\n"
                    dayname month day year))
    (cal-tex-rule "0pt" "7.5cm" ".5mm")
    (cal-tex-nl)
    (unless weekend
      (dotimes (i 5)
        (insert (format "\\textsf{\\large %d}\\\\\n" (+ i 8))))
      (dotimes (i 5)
        (insert (format "\\textsf{\\large %d}\\\\\n"
                        (if cal-tex-24
                            (+ i 13)    ; 13-17 incl
                          (1+ i))))))   ; 1-5 incl
    (cal-tex-nl ".5cm")
    (when weekend
      (cal-tex-vspace "1cm")
      (insert "\\ \\vfill")
      (insert (format "\\textbf{\\Large %s,} %s/%s/%s\\\\\n"
                      dayname1 month1 day1 year1))
      (cal-tex-rule "0pt" "7.5cm" ".5mm")
      (cal-tex-nl "1.5cm")
      (cal-tex-vspace "1cm"))
     (cal-tex-e-parbox)
     (cal-tex-e-framebox)
     (cal-tex-hspace "1cm")))

;;;###cal-autoload
(defun cal-tex-cursor-filofax-2week (&optional n event)
  "Two-weeks-at-a-glance Filofax style calendar for week cursor is in.
Optional prefix argument N specifies number of weeks (default 1).
The calendar shows holiday and diary entries if
`cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.
Optional EVENT indicates a buffer position to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (or n (setq n 1))
  (let* ((date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 calendar-week-start-day
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t event)))))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (day (calendar-extract-day date))
         (d1 (calendar-absolute-from-gregorian date))
         (d2 (+ (* 7 n) d1))
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays d1 d2)))
         (diary-list (if cal-tex-diary
                         (cal-tex-list-diary-entries
                          ;; FIXME d1?
                          (calendar-absolute-from-gregorian (list month 1 year))
                          d2))))
    (cal-tex-preamble "twoside")
    (cal-tex-cmd "\\textwidth 3.25in")
    (cal-tex-cmd "\\textheight 6.5in")
    (cal-tex-cmd "\\oddsidemargin 1.75in")
    (cal-tex-cmd "\\evensidemargin 1.5in")
    (cal-tex-cmd "\\topmargin 0pt")
    (cal-tex-cmd "\\headheight -0.875in")
    (cal-tex-cmd "\\headsep 0.125in")
    (cal-tex-cmd "\\footskip .125in")
    (insert "\\def\\righthead#1{\\hfill {\\normalsize \\bf #1}\\\\[-6pt]}
\\long\\def\\rightday#1#2#3#4#5{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox to 0.7in{%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\small #5 \\hfill #1 {\\normalsize \\bf #2}}%
          \\hbox to \\textwidth{\\vbox {\\raggedleft \\footnotesize \\em #4}}%
          \\hbox to \\textwidth{\\vbox to 0pt {\\noindent \\footnotesize #3}}}}\\\\}
\\def\\lefthead#1{\\noindent {\\normalsize \\bf #1}\\hfill\\\\[-6pt]}
\\long\\def\\leftday#1#2#3#4#5{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox to 0.7in{%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\noindent {\\normalsize \\bf #2} \\small #1 \\hfill #5}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\footnotesize \\em #4}}%
          \\hbox to \\textwidth{\\vbox to 0pt {\\noindent \\footnotesize #3}}}}\\\\}
")
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (dotimes (i n)
      (if (zerop (mod i 2))
          (insert "\\righthead")
        (insert "\\lefthead"))
      (cal-tex-arg
       (let ((d (cal-tex-incr-date date 6)))
         (if (= (calendar-extract-month date)
                (calendar-extract-month d))
             (format "%s %s"
                     (cal-tex-month-name (calendar-extract-month date))
                     (calendar-extract-year date))
           (if (= (calendar-extract-year date)
                  (calendar-extract-year d))
               (format "%s---%s %s"
                       (cal-tex-month-name (calendar-extract-month date))
                       (cal-tex-month-name (calendar-extract-month d))
                       (calendar-extract-year date))
              (format "%s %s---%s %s"
                      (cal-tex-month-name (calendar-extract-month date))
                      (calendar-extract-year date)
                      (cal-tex-month-name (calendar-extract-month d))
                      (calendar-extract-year d))))))
      (insert "%\n")
      (dotimes (_jdummy 7)
        (if (zerop (mod i 2))
            (insert "\\rightday")
          (insert "\\leftday"))
        (cal-tex-arg (cal-tex-LaTeXify-string (calendar-day-name date)))
        (cal-tex-arg (number-to-string (calendar-extract-day date)))
        (cal-tex-arg (cal-tex-latexify-list diary-list date))
        (cal-tex-arg (cal-tex-latexify-list holidays date))
        (cal-tex-arg (eval cal-tex-daily-string))
        (insert "%\n")
        (setq date (cal-tex-incr-date date)))
      (unless (= i (1- n))
        (run-hooks 'cal-tex-week-hook)
        (cal-tex-newpage)))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

;;;###cal-autoload
(defun cal-tex-cursor-filofax-week (&optional n event)
  "One-week-at-a-glance Filofax style calendar for week indicated by cursor.
Optional prefix argument N specifies number of weeks (default 1),
starting on Mondays.  The calendar shows holiday and diary entries
if `cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.
Optional EVENT indicates a buffer position to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (or n (setq n 1))
  (let* ((date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 1
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t event)))))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (day (calendar-extract-day date))
         (d1 (calendar-absolute-from-gregorian date))
         (d2 (+ (* 7 n) d1))
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays d1 d2)))
         (diary-list (if cal-tex-diary
                         (cal-tex-list-diary-entries
                          ;; FIXME d1?
                          (calendar-absolute-from-gregorian (list month 1 year))
                          d2))))
    (cal-tex-preamble "twoside")
    (cal-tex-cmd "\\textwidth 3.25in")
    (cal-tex-cmd "\\textheight 6.5in")
    (cal-tex-cmd "\\oddsidemargin 1.75in")
    (cal-tex-cmd "\\evensidemargin 1.5in")
    (cal-tex-cmd "\\topmargin 0pt")
    (cal-tex-cmd "\\headheight -0.875in")
    (cal-tex-cmd "\\headsep 0.125in")
    (cal-tex-cmd "\\footskip .125in")
    (insert "\\def\\righthead#1{\\hfill {\\normalsize \\bf #1}\\\\[-6pt]}
\\long\\def\\rightday#1#2#3#4#5{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox to 1.85in{%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\small #5 \\hfill #1 {\\normalsize \\bf #2}}%
          \\hbox to \\textwidth{\\vbox {\\raggedleft \\footnotesize \\em #4}}%
          \\hbox to \\textwidth{\\vbox to 0pt {\\noindent \\footnotesize #3}}}}\\\\}
\\long\\def\\weekend#1#2#3#4#5{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox to .8in{%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\small #5 \\hfill #1 {\\normalsize \\bf #2}}%
          \\hbox to \\textwidth{\\vbox {\\raggedleft \\footnotesize \\em #4}}%
          \\hbox to \\textwidth{\\vbox to 0pt {\\noindent \\footnotesize #3}}}}\\\\}
\\def\\lefthead#1{\\noindent {\\normalsize \\bf #1}\\hfill\\\\[-6pt]}
\\long\\def\\leftday#1#2#3#4#5{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox to 1.85in{%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\noindent {\\normalsize \\bf #2} \\small #1 \\hfill #5}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\footnotesize \\em #4}}%
          \\hbox to \\textwidth{\\vbox to 0pt {\\noindent \\footnotesize #3}}}}\\\\}
")
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}\\ ")
    (cal-tex-newpage)
    (dotimes (i n)
      (insert "\\lefthead")
      (cal-tex-arg
       (let ((d (cal-tex-incr-date date 2)))
         (if (= (calendar-extract-month date)
                (calendar-extract-month d))
             (format "%s %s"
                     (cal-tex-month-name (calendar-extract-month date))
                     (calendar-extract-year date))
           (if (= (calendar-extract-year date)
                  (calendar-extract-year d))
               (format "%s---%s %s"
                       (cal-tex-month-name (calendar-extract-month date))
                       (cal-tex-month-name (calendar-extract-month d))
                       (calendar-extract-year date))
             (format "%s %s---%s %s"
                     (cal-tex-month-name (calendar-extract-month date))
                     (calendar-extract-year date)
                     (cal-tex-month-name (calendar-extract-month d))
                     (calendar-extract-year d))))))
      (insert "%\n")
      (dotimes (_jdummy 3)
        (insert "\\leftday")
        (cal-tex-arg (cal-tex-LaTeXify-string (calendar-day-name date)))
        (cal-tex-arg (number-to-string (calendar-extract-day date)))
        (cal-tex-arg (cal-tex-latexify-list diary-list date))
        (cal-tex-arg (cal-tex-latexify-list holidays date))
        (cal-tex-arg (eval cal-tex-daily-string))
        (insert "%\n")
        (setq date (cal-tex-incr-date date)))
      (insert "\\noindent\\rule{\\textwidth}{0.3pt}\\\\%\n")
      (cal-tex-newpage)
      (insert "\\righthead")
      (cal-tex-arg
       (let ((d (cal-tex-incr-date date 3)))
         (if (= (calendar-extract-month date)
                 (calendar-extract-month d))
             (format "%s %s"
                     (cal-tex-month-name (calendar-extract-month date))
                     (calendar-extract-year date))
           (if (= (calendar-extract-year date)
                  (calendar-extract-year d))
               (format "%s---%s %s"
                       (cal-tex-month-name (calendar-extract-month date))
                       (cal-tex-month-name (calendar-extract-month d))
                       (calendar-extract-year date))
             (format "%s %s---%s %s"
                     (cal-tex-month-name (calendar-extract-month date))
                     (calendar-extract-year date)
                     (cal-tex-month-name (calendar-extract-month d))
                     (calendar-extract-year d))))))
      (insert "%\n")
      (dotimes (_jdummy 2)
        (insert "\\rightday")
        (cal-tex-arg (cal-tex-LaTeXify-string (calendar-day-name date)))
        (cal-tex-arg (number-to-string (calendar-extract-day date)))
        (cal-tex-arg (cal-tex-latexify-list diary-list date))
        (cal-tex-arg (cal-tex-latexify-list holidays date))
        (cal-tex-arg (eval cal-tex-daily-string))
        (insert "%\n")
        (setq date (cal-tex-incr-date date)))
      (dotimes (_jdummy 2)
        (insert "\\weekend")
        (cal-tex-arg (cal-tex-LaTeXify-string (calendar-day-name date)))
        (cal-tex-arg (number-to-string (calendar-extract-day date)))
        (cal-tex-arg (cal-tex-latexify-list diary-list date))
        (cal-tex-arg (cal-tex-latexify-list holidays date))
        (cal-tex-arg (eval cal-tex-daily-string))
        (insert "%\n")
        (setq date (cal-tex-incr-date date)))
      (unless (= i (1- n))
        (run-hooks 'cal-tex-week-hook)
        (cal-tex-newpage)))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

;;;###cal-autoload
(defun cal-tex-cursor-filofax-daily (&optional n event)
  "Day-per-page Filofax style calendar for week indicated by cursor.
Optional prefix argument N specifies number of weeks (default 1),
starting on Mondays.  The calendar shows holiday and diary
entries if `cal-tex-holidays' and `cal-tex-diary', respectively,
are non-nil.  Pages are ruled if `cal-tex-rules' is non-nil.
Optional EVENT indicates a buffer position to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (or n (setq n 1))
  (let* ((date (calendar-gregorian-from-absolute
                (calendar-dayname-on-or-before
                 1
                 (calendar-absolute-from-gregorian
                  (calendar-cursor-to-date t event)))))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (day (calendar-extract-day date))
         (d1 (calendar-absolute-from-gregorian date))
         (d2 (+ (* 7 n) d1))
         (holidays (if cal-tex-holidays
                       (cal-tex-list-holidays d1 d2)))
         (diary-list (if cal-tex-diary
                         (cal-tex-list-diary-entries
                          ;; FIXME d1?
                          (calendar-absolute-from-gregorian (list month 1 year))
                          d2))))
    (cal-tex-preamble "twoside")
    (cal-tex-cmd "\\textwidth 3.25in")
    (cal-tex-cmd "\\textheight 6.5in")
    (cal-tex-cmd "\\oddsidemargin 1.75in")
    (cal-tex-cmd "\\evensidemargin 1.5in")
    (cal-tex-cmd "\\topmargin 0pt")
    (cal-tex-cmd "\\headheight -0.875in")
    (cal-tex-cmd "\\headsep 0.125in")
    (cal-tex-cmd "\\footskip .125in")
    (insert "\\def\\righthead#1{\\hfill {\\normalsize \\bf #1}\\\\[-6pt]}
\\long\\def\\rightday#1#2#3{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox {%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\hfill \\small #3 \\hfill}%
          \\hbox to \\textwidth{\\vbox {\\raggedleft \\em #2}}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\footnotesize #1}}}}}
\\long\\def\\weekend#1#2#3{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox {%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\hfill \\small #3 \\hfill}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\em #2}}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\footnotesize #1}}}}}
\\def\\lefthead#1{\\noindent {\\normalsize \\bf #1}\\hfill\\\\[-6pt]}
\\long\\def\\leftday#1#2#3{%
   \\rule{\\textwidth}{0.3pt}\\\\%
   \\hbox to \\textwidth{%
     \\vbox {%
          \\vspace*{2pt}%
          \\hbox to \\textwidth{\\hfill \\small #3 \\hfill}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\em #2}}%
          \\hbox to \\textwidth{\\vbox {\\noindent \\footnotesize #1}}}}}
\\newbox\\LineBox
\\setbox\\LineBox=\\hbox to\\textwidth{%
\\vrule height.2in width0pt\\leaders\\hrule\\hfill}
\\def\\linesfill{\\par\\leaders\\copy\\LineBox\\vfill}
")
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (dotimes (i n)
      (dotimes (j 4)
        (let ((even (zerop (% j 2))))
          (insert (if even
                      "\\righthead"
                    "\\lefthead"))
          (cal-tex-arg (calendar-date-string date))
          (insert "%\n")
          (insert (if even
                      "\\rightday"
                    "\\leftday")))
        (cal-tex-arg (cal-tex-latexify-list diary-list date))
        (cal-tex-arg (cal-tex-latexify-list holidays date "\\\\" t))
        (cal-tex-arg (eval cal-tex-daily-string))
        (insert "%\n")
        (if cal-tex-rules
            (insert "\\linesfill\n")
          (insert "\\vfill\\noindent\\rule{\\textwidth}{0.3pt}\\\\%\n"))
        (cal-tex-newpage)
        (setq date (cal-tex-incr-date date)))
      (insert "%\n")
      (dotimes (_jdummy 2)
        (insert "\\lefthead")
        (cal-tex-arg (calendar-date-string date))
        (insert "\\weekend")
        (cal-tex-arg (cal-tex-latexify-list diary-list date))
        (cal-tex-arg (cal-tex-latexify-list holidays date "\\\\" t))
        (cal-tex-arg (eval cal-tex-daily-string))
        (insert "%\n")
        (if cal-tex-rules
            (insert "\\linesfill\n")
          (insert "\\vfill"))
        (setq date (cal-tex-incr-date date)))
      (or cal-tex-rules
          (insert "\\noindent\\rule{\\textwidth}{0.3pt}\\\\%\n"))
      (unless (= i (1- n))
        (run-hooks 'cal-tex-week-hook)
        (cal-tex-newpage)))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))


;;;
;;;  Daily calendars
;;;

;;;###cal-autoload
(defun cal-tex-cursor-day (&optional n event)
  "Make a buffer with LaTeX commands for the day cursor is on.
Optional prefix argument N specifies number of days.  The calendar shows
the hours between `cal-tex-daily-start' and `cal-tex-daily-end', using
the 24-hour clock if `cal-tex-24' is non-nil.  Optional EVENT indicates
a buffer position to use instead of point."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (or n (setq n 1))
  (let ((date (calendar-absolute-from-gregorian
               (calendar-cursor-to-date t event))))
    (cal-tex-preamble "12pt")
    (cal-tex-cmd "\\textwidth 6.5in")
    (cal-tex-cmd "\\textheight 10.5in")
    (cal-tex-b-document)
    (cal-tex-cmd "\\pagestyle{empty}")
    (dotimes (i n)
      (cal-tex-vspace "-1.7in")
      (cal-tex-daily-page (calendar-gregorian-from-absolute date))
      (setq date (1+ date))
      (unless (= i (1- n))
        (cal-tex-newpage)
        (run-hooks 'cal-tex-daily-hook)))
    (cal-tex-end-document)
    (run-hooks 'cal-tex-hook)))

(defun cal-tex-daily-page (date)
  "Make a calendar page for Gregorian DATE on 8.5 by 11 paper.
Uses the 24-hour clock if `cal-tex-24' is non-nil.  Produces
hourly sections for the period specified by `cal-tex-daily-start'
and `cal-tex-daily-end'."
  (let ((month-name (cal-tex-month-name (calendar-extract-month date)))
        (i (1- cal-tex-daily-start))
        hour)
    (cal-tex-banner "cal-tex-daily-page")
    (cal-tex-b-makebox "4cm" "l")
    (cal-tex-b-parbox "b" "3.8cm")
    (cal-tex-rule "0mm" "0mm" "2cm")
    (cal-tex-Huge (number-to-string (calendar-extract-day date)))
    (cal-tex-nl ".5cm")
    (cal-tex-bf month-name )
    (cal-tex-e-parbox)
    (cal-tex-hspace "1cm")
    (cal-tex-scriptsize (eval cal-tex-daily-string))
    (cal-tex-hspace "3.5cm")
    (cal-tex-e-makebox)
    (cal-tex-hfill)
    (cal-tex-b-makebox "4cm" "r")
    (cal-tex-bf (cal-tex-LaTeXify-string (calendar-day-name date)))
    (cal-tex-e-makebox)
    (cal-tex-nl)
    (cal-tex-hspace ".4cm")
    (cal-tex-rule "0mm" "16.1cm" "1mm")
    (cal-tex-nl ".1cm")
    (while (<= (setq i (1+ i)) cal-tex-daily-end)
      (cal-tex-cmd "\\noindent")
      (setq hour (if cal-tex-24
                     i
                   (mod i 12)))
      (if (zerop hour) (setq hour 12))
      (cal-tex-b-makebox "1cm" "c")
      (cal-tex-arg (number-to-string hour))
      (cal-tex-e-makebox)
      (cal-tex-rule "0mm" "15.5cm" ".2mm")
      (cal-tex-nl ".2cm")
      (cal-tex-b-makebox "1cm" "c")
      (cal-tex-arg "$\\diamond$" )
      (cal-tex-e-makebox)
      (cal-tex-rule "0mm" "15.5cm" ".2mm")
      (cal-tex-nl ".2cm"))
    (cal-tex-hfill)
    (insert (cal-tex-mini-calendar
             (calendar-extract-month (cal-tex-previous-month date))
             (calendar-extract-year (cal-tex-previous-month date))
             "lastmonth" "1.1in" "1in"))
    (insert (cal-tex-mini-calendar
             (calendar-extract-month date)
             (calendar-extract-year date)
             "thismonth" "1.1in" "1in"))
    (insert (cal-tex-mini-calendar
             (calendar-extract-month (cal-tex-next-month date))
             (calendar-extract-year (cal-tex-next-month date))
             "nextmonth" "1.1in" "1in"))
    (insert "\\hbox to \\textwidth{")
    (cal-tex-hfill)
    (insert "\\lastmonth")
    (cal-tex-hfill)
    (insert "\\thismonth")
    (cal-tex-hfill)
    (insert "\\nextmonth")
    (cal-tex-hfill)
    (insert "}")
    (cal-tex-banner "end of cal-tex-daily-page")))

;;;
;;;  Mini calendars
;;;

(defun cal-tex-mini-calendar (month year name width height &optional ptsize colsep)
  "Produce mini-calendar for MONTH, YEAR in macro NAME with WIDTH and HEIGHT.
Optional string PTSIZE gives the point size (default \"scriptsize\").
Optional string COLSEP gives the column separation (default \"1mm\")."
  (or colsep (setq colsep "1mm"))
  (or ptsize (setq ptsize "scriptsize"))
  (let ((blank-days                     ; at start of month
         (mod
          (- (calendar-day-of-week (list month 1 year))
             calendar-week-start-day)
          7))
        (last( calendar-last-day-of-month month year))
        (str (concat "\\def\\" name "{\\hbox to" width "{%\n"
                     "\\vbox to" height "{%\n"
                     "\\vfil  \\hbox to" width "{%\n"
                     "\\hfil\\" ptsize
                     "\\begin{tabular}"
                     "{@{\\hspace{0mm}}r@{\\hspace{" colsep
                     "}}r@{\\hspace{" colsep "}}r@{\\hspace{" colsep
                     "}}r@{\\hspace{" colsep "}}r@{\\hspace{" colsep
                     "}}r@{\\hspace{" colsep "}}r@{\\hspace{0mm}}}%\n"
                     "\\multicolumn{7}{c}{"
                     (cal-tex-month-name month)
                     " "
                     (number-to-string year)
                     "}\\\\[1mm]\n")))
    (dotimes (i 7)
      (setq str
            (concat str
                    (cal-tex-LaTeXify-string
                     (substring (aref calendar-day-name-array
                                      (mod (+ calendar-week-start-day i) 7))

                                0 2))
                    (if (= i 6)
                        "\\\\[0.7mm]\n"
                      " & "))))
    (dotimes (_idummy blank-days)
      (setq str (concat str " & ")))
    (dotimes (i last)
      (setq str (concat str (number-to-string (1+ i)))
            str (concat str (if (zerop (mod (+ i 1 blank-days) 7))
                                (if (= i (1- last))
                                    ""
                                  "\\\\[0.5mm]\n")
                              " & "))))
    (setq str (concat str "\n\\end{tabular}\\hfil}\\vfil}}}%\n"))
    str))

;;;
;;;  Various calendar functions
;;;

(defun cal-tex-incr-date (date &optional n)
  "The date of the day following DATE.
If optional N is given, the date of N days after DATE."
  (calendar-gregorian-from-absolute
   (+ (or n 1) (calendar-absolute-from-gregorian date))))

(defun cal-tex-latexify-list (date-list date &optional separator final-separator)
  "Return string with concatenated, LaTeX-ified entries in DATE-LIST for DATE.
Use double backslash as a separator unless optional SEPARATOR is given.
If resulting string is not empty, put separator at end if optional
FINAL-SEPARATOR is non-nil."
  (or separator (setq separator "\\\\"))
  (let (result)
    (setq result
          (mapconcat (lambda (x) (cal-tex-LaTeXify-string x))
                     (dolist (d date-list (reverse result))
                       (and (car d)
                            (calendar-date-equal date (car d))
                            (setq result (cons (cadr d) result))))
                     separator))
    (if (and final-separator
             (not (string-equal result "")))
        (concat result separator)
      result)))

(defun cal-tex-previous-month (date)
  "Return the date of the first day in the month previous to DATE."
  (let ((month (calendar-extract-month date))
        (year (calendar-extract-year date)))
    (calendar-increment-month month year -1)
    (list month 1 year)))

(defun cal-tex-next-month (date)
  "Return the date of the first day in the month following DATE."
  (let ((month (calendar-extract-month date))
        (year (calendar-extract-year date)))
    (calendar-increment-month month year 1)
    (list month 1 year)))

;;;
;;;  LaTeX Code
;;;

(defun cal-tex-end-document ()
  "Finish the LaTeX document.
Insert the trailer to LaTeX document, pop to LaTeX buffer, add
informative header, and run HOOK."
  (cal-tex-e-document)
  (or (and cal-tex-preamble-extra
           (string-match "inputenc" cal-tex-preamble-extra))
      (not (re-search-backward "[^[:ascii:]]" nil 'move))
      (progn
        (goto-char (point-min))
        (when (search-forward "documentclass" nil t)
          (forward-line 1)
          ;; Eg for some Bahai holidays.
          ;; FIXME latin1 might not always be right.
          (insert "\\usepackage[latin1]{inputenc}\n"))))
  (latex-mode)
  (pop-to-buffer cal-tex-buffer)
  (goto-char (point-min))
  ;; FIXME auctex equivalents?
  (cal-tex-comment
   (format "\tThis buffer was produced by cal-tex.el.
\tTo print a calendar, type
\t\tM-x tex-buffer RET
\t\tM-x tex-print  RET")))

(defun cal-tex-insert-preamble (weeks landscape size &optional append)
  "Initialize the output LaTeX calendar buffer, `cal-tex-buffer'.
Select the output buffer, and insert the preamble for a calendar
of WEEKS weeks.  Insert code for landscape mode if LANDSCAPE is
non-nil.  Use point-size SIZE.  Optional argument APPEND, if
non-nil, means add to end of buffer without erasing current contents."
  (let ((width "18cm")
        (height "24cm"))
    (when landscape
      (setq width "24cm"
            height "18cm"))
    (unless append
      (cal-tex-preamble size)
      (if (not landscape)
          (progn
            (cal-tex-cmd "\\oddsidemargin -1.75cm")
            (cal-tex-cmd "\\def\\holidaymult{.06}"))
        (cal-tex-cmd "\\special{landscape}")
        (cal-tex-cmd "\\textwidth 9.5in")
        (cal-tex-cmd "\\textheight 7in")
        (cal-tex-comment)
        (cal-tex-cmd "\\def\\holidaymult{.08}"))
      (cal-tex-cmd cal-tex-caldate)
      (cal-tex-cmd cal-tex-myday)
      (cal-tex-b-document)
      (cal-tex-cmd "\\pagestyle{empty}"))
    (cal-tex-cmd "\\setlength{\\cellwidth}" width)
    (insert (format "\\setlength{\\cellwidth}{%f\\cellwidth}\n"
                    (/ 1.1 (length cal-tex-which-days))))
    (cal-tex-cmd "\\setlength{\\cellheight}" height)
    (insert (format "\\setlength{\\cellheight}{%f\\cellheight}\n"
                    (/ 1.0 weeks)))
    (cal-tex-cmd "\\ \\par")
    (cal-tex-vspace "-3cm")))

(defconst cal-tex-LaTeX-subst-list
  '(("\"". "``")
    ("\"". "''")        ; quote changes meaning when list is reversed
    ;; Don't think this is necessary, and in any case, does not work:
    ;; "LaTeX Error: \verb illegal in command argument".
;;;    ("@" . "\\verb|@|")
    ("&" . "\\&")
    ("%" . "\\%")
    ("$" . "\\$")
    ("#" . "\\#")
    ("_" . "\\_")
    ("{" . "\\{")
    ("}" . "\\}")
    ("<" . "$<$")
    (">" . "$>$")
    ("\n" . "\\ \\\\")) ; \\ needed for e.g \begin{center}\n AA\end{center}
  "Alist of symbols and their LaTeX replacements.")

(defun cal-tex-LaTeXify-string (string)
  "Protect special characters in STRING from LaTeX."
  (if (not string)
      ""
    (let ((head "")
          (tail string)
          (list cal-tex-LaTeX-subst-list)
          ch pair)
      (while (not (string-equal tail ""))
        (setq ch (substring-no-properties tail 0 1)
              pair (assoc ch list))
        (if (and pair (string-equal ch "\""))
            (setq list (reverse list))) ; quote changes meaning each time
        (setq tail (substring-no-properties tail 1)
              head (concat head (if pair (cdr pair) ch))))
      head)))

(defun cal-tex-month-name (month)
  "The name of MONTH, LaTeX-ified."
  (cal-tex-LaTeXify-string (calendar-month-name month)))

(defun cal-tex-hfill ()
  "Insert hfill."
  (insert "\\hfill"))

(defun cal-tex-newpage ()
  "Insert newpage."
  (insert "\\newpage%\n"))

(defun cal-tex-noindent ()
  "Insert noindent."
  (insert "\\noindent"))

(defun cal-tex-vspace (space)
  "Insert vspace command to move SPACE vertically."
  (insert "\\vspace*{" space "}")
  (cal-tex-comment))

(defun cal-tex-hspace (space)
  "Insert hspace command to move SPACE horizontally."
  (insert "\\hspace*{" space "}")
  (cal-tex-comment))

(defun cal-tex-comment (&optional comment)
  "Insert `% ', followed by optional string COMMENT, followed by newline.
COMMENT may contain newlines, which are prefixed by `% ' in the output."
  (insert (format "%% %s\n"
                  (if comment
                      (replace-regexp-in-string "\n" "\n% " comment)
                    ""))))

(defun cal-tex-banner (comment)
  "Insert string COMMENT, separated by blank lines."
  (cal-tex-comment (format "\n\n\n\t\t\t%s\n" comment)))

(defun cal-tex-nl (&optional skip comment)
  "End a line with \\.  If SKIP, then add that much spacing.
Add trailing COMMENT if present."
  (insert (format "\\\\%s"
                  (if skip
                      (format "[%s]" skip)
                    "")))
  (cal-tex-comment comment))

(defun cal-tex-arg (&optional text)
  "Insert a brace {} pair containing the optional string TEXT."
  (insert (format "{%s}" (or text ""))))

(defun cal-tex-cmd (cmd &optional arg)
  "Insert LaTeX CMD, with optional argument ARG, and end with %."
  (insert cmd)
  (cal-tex-arg arg)
  (cal-tex-comment))

;;;
;;;   Environments
;;;

(defun cal-tex-b-document ()
  "Insert beginning of document."
  (cal-tex-cmd "\\begin{document}"))

(defun cal-tex-e-document ()
  "Insert end of document."
  (cal-tex-cmd "\\end{document}"))

(defun cal-tex-b-center ()
  "Insert beginning of centered block."
  (cal-tex-cmd "\\begin{center}"))

(defun cal-tex-e-center ()
  "Insert end of centered block."
  (cal-tex-comment)
  (cal-tex-cmd "\\end{center}"))


;;;
;;;  Boxes
;;;


(defun cal-tex-b-parbox (position width)
  "Insert parbox with parameters POSITION and WIDTH."
  (insert "\\parbox[" position "]{" width "}{")
  (cal-tex-comment))

(defun cal-tex-e-parbox (&optional height)
  "Insert end of parbox.  Optionally, force it to be a given HEIGHT."
  (cal-tex-comment)
  (if height
      (cal-tex-rule "0mm" "0mm" height))
  (insert "}")
  (cal-tex-comment "end parbox"))

(defun cal-tex-b-framebox (width position)
  "Insert framebox with parameters WIDTH and POSITION (clr)."
  (insert "\\framebox[" width "][" position "]{" )
  (cal-tex-comment))

(defun cal-tex-e-framebox ()
  "Insert end of framebox."
  (cal-tex-comment)
  (insert "}")
  (cal-tex-comment "end framebox"))


(defun cal-tex-b-makebox (width position)
  "Insert makebox with parameters WIDTH and POSITION (clr)."
  (insert "\\makebox[" width "][" position "]{" )
  (cal-tex-comment))

(defun cal-tex-e-makebox ()
  "Insert end of makebox."
  (cal-tex-comment)
  (insert "}")
  (cal-tex-comment "end makebox"))


(defun cal-tex-rule (lower width height)
  "Insert a rule with parameters LOWER WIDTH HEIGHT."
  (insert "\\rule[" lower "]{" width "}{" height "}"))

;;;
;;;     Fonts
;;;

(defun cal-tex-em (string)
  "Insert STRING in italic font."
  (insert "\\textit{" string "}"))

(defun cal-tex-bf (string)
  "Insert STRING in bf font."
  (insert "\\textbf{ " string "}"))

(defun cal-tex-scriptsize (string)
  "Insert STRING in scriptsize font."
  (insert "{\\scriptsize " string "}"))

(defun cal-tex-huge (string)
  "Insert STRING in huge font."
  (insert "{\\huge " string "}"))

(defun cal-tex-Huge (string)
  "Insert STRING in Huge font."
  (insert "{\\Huge " string "}"))

(defun cal-tex-Huge-bf (string)
  "Insert STRING in Huge bf font."
  (insert "\\textbf{\\Huge " string "}"))

(defun cal-tex-large (string)
  "Insert STRING in large font."
  (insert "{\\large " string "}"))

(defun cal-tex-large-bf (string)
  "Insert STRING in large bf font."
  (insert "\\textbf{\\large " string "}"))


(provide 'cal-tex)

;;; cal-tex.el ends here
