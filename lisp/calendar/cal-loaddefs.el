;;; cal-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (diary-bahai-insert-yearly-entry diary-bahai-insert-monthly-entry
;;;;;;  diary-bahai-insert-entry calendar-bahai-goto-date calendar-bahai-print-date
;;;;;;  calendar-bahai-date-string) "cal-bahai" "cal-bahai.el" (20352
;;;;;;  65510))
;;; Generated autoloads from cal-bahai.el

(autoload 'calendar-bahai-date-string "cal-bahai" "\
String of Bahá'í date of Gregorian DATE.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-bahai-print-date "cal-bahai" "\
Show the Bahá'í calendar equivalent of the selected date.

\(fn)" t nil)

(autoload 'calendar-bahai-goto-date "cal-bahai" "\
Move cursor to Bahá'í date DATE; echo Bahá'í date unless NOECHO is non-nil.

\(fn DATE &optional NOECHO)" t nil)

(autoload 'diary-bahai-insert-entry "cal-bahai" "\
Insert a diary entry.
For the Bahá'í date corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(autoload 'diary-bahai-insert-monthly-entry "cal-bahai" "\
Insert a monthly diary entry.
For the day of the Bahá'í month corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(autoload 'diary-bahai-insert-yearly-entry "cal-bahai" "\
Insert an annual diary entry.
For the day of the Bahá'í year corresponding to the date indicated by point.
Prefix argument ARG will make the entry nonmarking.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (calendar-chinese-goto-date calendar-chinese-print-date
;;;;;;  calendar-chinese-date-string) "cal-china" "cal-china.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-china.el

(autoload 'calendar-chinese-date-string "cal-china" "\
String of Chinese date of Gregorian DATE.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-chinese-print-date "cal-china" "\
Show the Chinese date equivalents of date.

\(fn)" t nil)

(autoload 'calendar-chinese-goto-date "cal-china" "\
Move cursor to Chinese date DATE.
Echo Chinese date unless NOECHO is non-nil.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-ethiopic-goto-date calendar-ethiopic-print-date
;;;;;;  calendar-ethiopic-date-string calendar-coptic-goto-date calendar-coptic-print-date
;;;;;;  calendar-coptic-date-string) "cal-coptic" "cal-coptic.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-coptic.el

(autoload 'calendar-coptic-date-string "cal-coptic" "\
String of Coptic date of Gregorian DATE.
Returns the empty string if DATE is pre-Coptic calendar.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-coptic-print-date "cal-coptic" "\
Show the Coptic calendar equivalent of the selected date.

\(fn)" t nil)

(autoload 'calendar-coptic-goto-date "cal-coptic" "\
Move cursor to Coptic date DATE.
Echo Coptic date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

(autoload 'calendar-ethiopic-date-string "cal-coptic" "\
String of Ethiopic date of Gregorian DATE.
Returns the empty string if DATE is pre-Ethiopic calendar.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-ethiopic-print-date "cal-coptic" "\
Show the Ethiopic calendar equivalent of the selected date.

\(fn)" t nil)

(autoload 'calendar-ethiopic-goto-date "cal-coptic" "\
Move cursor to Ethiopic date DATE.
Echo Ethiopic date unless NOECHO is t.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-french-goto-date calendar-french-print-date
;;;;;;  calendar-french-date-string) "cal-french" "cal-french.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-french.el

(autoload 'calendar-french-date-string "cal-french" "\
String of French Revolutionary date of Gregorian DATE.
Returns the empty string if DATE is pre-French Revolutionary.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-french-print-date "cal-french" "\
Show the French Revolutionary calendar equivalent of the selected date.

\(fn)" t nil)

(autoload 'calendar-french-goto-date "cal-french" "\
Move cursor to French Revolutionary date DATE.
Echo French Revolutionary date unless NOECHO is non-nil.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (diary-hebrew-insert-yearly-entry diary-hebrew-insert-monthly-entry
;;;;;;  diary-hebrew-insert-entry calendar-hebrew-goto-date calendar-hebrew-print-date
;;;;;;  calendar-hebrew-date-string) "cal-hebrew" "cal-hebrew.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-hebrew.el

(autoload 'calendar-hebrew-date-string "cal-hebrew" "\
String of Hebrew date before sunset of Gregorian DATE.
Defaults to today's date if DATE is not given.
Driven by the variable `calendar-date-display-form'.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-hebrew-print-date "cal-hebrew" "\
Show the Hebrew calendar equivalent of the date under the cursor.

\(fn)" t nil)

(autoload 'calendar-hebrew-goto-date "cal-hebrew" "\
Move cursor to Hebrew DATE; echo Hebrew date unless NOECHO is non-nil.

\(fn DATE &optional NOECHO)" t nil)

(autoload 'diary-hebrew-insert-entry "cal-hebrew" "\
Insert a diary entry for the Hebrew date at point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(autoload 'diary-hebrew-insert-monthly-entry "cal-hebrew" "\
Insert a monthly diary entry.
For the day of the Hebrew month corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(autoload 'diary-hebrew-insert-yearly-entry "cal-hebrew" "\
Insert an annual diary entry.
For the day of the Hebrew year corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (cal-html-cursor-year cal-html-cursor-month) "cal-html"
;;;;;;  "cal-html.el" (20400 62402))
;;; Generated autoloads from cal-html.el

(autoload 'cal-html-cursor-month "cal-html" "\
Write an HTML calendar file for numeric MONTH of four-digit YEAR.
The output directory DIR is created if necessary.  Interactively,
MONTH and YEAR are taken from the calendar cursor position, or from
the position specified by EVENT.  Note that any existing output files
are overwritten.

\(fn MONTH YEAR DIR &optional EVENT)" t nil)

(autoload 'cal-html-cursor-year "cal-html" "\
Write HTML calendar files (index and monthly pages) for four-digit YEAR.
The output directory DIR is created if necessary.  Interactively,
YEAR is taken from the calendar cursor position, or from the position
specified by EVENT.  Note that any existing output files are overwritten.

\(fn YEAR DIR &optional EVENT)" t nil)

;;;***

;;;### (autoloads (diary-islamic-insert-yearly-entry diary-islamic-insert-monthly-entry
;;;;;;  diary-islamic-insert-entry calendar-islamic-goto-date calendar-islamic-print-date
;;;;;;  calendar-islamic-date-string) "cal-islam" "cal-islam.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-islam.el

(autoload 'calendar-islamic-date-string "cal-islam" "\
String of Islamic date before sunset of Gregorian DATE.
Returns the empty string if DATE is pre-Islamic.
Defaults to today's date if DATE is not given.
Driven by the variable `calendar-date-display-form'.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-islamic-print-date "cal-islam" "\
Show the Islamic calendar equivalent of the date under the cursor.

\(fn)" t nil)

(autoload 'calendar-islamic-goto-date "cal-islam" "\
Move cursor to Islamic DATE; echo Islamic date unless NOECHO is non-nil.

\(fn DATE &optional NOECHO)" t nil)

(autoload 'diary-islamic-insert-entry "cal-islam" "\
Insert a diary entry.
For the Islamic date corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(autoload 'diary-islamic-insert-monthly-entry "cal-islam" "\
Insert a monthly diary entry.
For the day of the Islamic month corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(autoload 'diary-islamic-insert-yearly-entry "cal-islam" "\
Insert an annual diary entry.
For the day of the Islamic year corresponding to the date indicated by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (calendar-iso-goto-week calendar-iso-goto-date
;;;;;;  calendar-iso-print-date calendar-iso-date-string calendar-iso-from-absolute)
;;;;;;  "cal-iso" "cal-iso.el" (20352 65510))
;;; Generated autoloads from cal-iso.el

(autoload 'calendar-iso-from-absolute "cal-iso" "\
Compute the `ISO commercial date' corresponding to the absolute DATE.
The ISO year corresponds approximately to the Gregorian year, but weeks
start on Monday and end on Sunday.  The first week of the ISO year is the
first such week in which at least 4 days are in a year.  The ISO commercial
date has the form (week day year) in which week is in the range 1..52 and
day is in the range 0..6 (1 = Monday, 2 = Tuesday, ..., 0 = Sunday).  The
absolute date is the number of days elapsed since the (imaginary) Gregorian
date Sunday, December 31, 1 BC.

\(fn DATE)" nil nil)

(autoload 'calendar-iso-date-string "cal-iso" "\
String of ISO date of Gregorian DATE, default today.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-iso-print-date "cal-iso" "\
Show equivalent ISO date for the date under the cursor.

\(fn)" t nil)

(autoload 'calendar-iso-goto-date "cal-iso" "\
Move cursor to ISO DATE; echo ISO date unless NOECHO is non-nil.

\(fn DATE &optional NOECHO)" t nil)

(autoload 'calendar-iso-goto-week "cal-iso" "\
Move cursor to ISO DATE; echo ISO date unless NOECHO is non-nil.
Interactively, goes to the first day of the specified week.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-astro-goto-day-number calendar-astro-print-day-number
;;;;;;  calendar-astro-date-string calendar-astro-from-absolute calendar-astro-to-absolute
;;;;;;  calendar-julian-goto-date calendar-julian-print-date calendar-julian-date-string
;;;;;;  calendar-julian-from-absolute) "cal-julian" "cal-julian.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-julian.el

(autoload 'calendar-julian-from-absolute "cal-julian" "\
Compute the Julian (month day year) corresponding to the absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC.

\(fn DATE)" nil nil)

(autoload 'calendar-julian-date-string "cal-julian" "\
String of Julian date of Gregorian DATE.
Defaults to today's date if DATE is not given.
Driven by the variable `calendar-date-display-form'.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-julian-print-date "cal-julian" "\
Show the Julian calendar equivalent of the date under the cursor.

\(fn)" t nil)

(autoload 'calendar-julian-goto-date "cal-julian" "\
Move cursor to Julian DATE; echo Julian date unless NOECHO is non-nil.

\(fn DATE &optional NOECHO)" t nil)

(autoload 'calendar-astro-to-absolute "cal-julian" "\
Absolute date of astronomical (Julian) day number D.

\(fn D)" nil nil)

(autoload 'calendar-astro-from-absolute "cal-julian" "\
Astronomical (Julian) day number of absolute date D.

\(fn D)" nil nil)

(autoload 'calendar-astro-date-string "cal-julian" "\
String of astronomical (Julian) day number after noon UTC of Gregorian DATE.
Defaults to today's date if DATE is not given.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-astro-print-day-number "cal-julian" "\
Show astronomical (Julian) day number after noon UTC on cursor date.

\(fn)" t nil)

(autoload 'calendar-astro-goto-day-number "cal-julian" "\
Move cursor to astronomical (Julian) DAYNUMBER.
Echo astronomical (Julian) day number unless NOECHO is non-nil.

\(fn DAYNUMBER &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-mayan-goto-long-count-date calendar-mayan-previous-round-date
;;;;;;  calendar-mayan-next-round-date calendar-mayan-previous-tzolkin-date
;;;;;;  calendar-mayan-next-tzolkin-date calendar-mayan-previous-haab-date
;;;;;;  calendar-mayan-next-haab-date calendar-mayan-print-date calendar-mayan-date-string)
;;;;;;  "cal-mayan" "cal-mayan.el" (20352 65510))
;;; Generated autoloads from cal-mayan.el

(autoload 'calendar-mayan-date-string "cal-mayan" "\
String of Mayan date of Gregorian DATE; default today.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-mayan-print-date "cal-mayan" "\
Show the Mayan long count, tzolkin, and haab equivalents of date.

\(fn)" t nil)

(autoload 'calendar-mayan-next-haab-date "cal-mayan" "\
Move cursor to next instance of Mayan HAAB-DATE.
Echo Mayan date unless NOECHO is non-nil.

\(fn HAAB-DATE &optional NOECHO)" t nil)

(autoload 'calendar-mayan-previous-haab-date "cal-mayan" "\
Move cursor to previous instance of Mayan HAAB-DATE.
Echo Mayan date unless NOECHO is non-nil.

\(fn HAAB-DATE &optional NOECHO)" t nil)

(autoload 'calendar-mayan-next-tzolkin-date "cal-mayan" "\
Move cursor to next instance of Mayan TZOLKIN-DATE.
Echo Mayan date unless NOECHO is non-nil.

\(fn TZOLKIN-DATE &optional NOECHO)" t nil)

(autoload 'calendar-mayan-previous-tzolkin-date "cal-mayan" "\
Move cursor to previous instance of Mayan TZOLKIN-DATE.
Echo Mayan date unless NOECHO is non-nil.

\(fn TZOLKIN-DATE &optional NOECHO)" t nil)

(autoload 'calendar-mayan-next-round-date "cal-mayan" "\
Move cursor to next instance of Mayan TZOLKIN-DATE HAAB-DATE combination.
Echo Mayan date unless NOECHO is non-nil.

\(fn TZOLKIN-DATE HAAB-DATE &optional NOECHO)" t nil)

(autoload 'calendar-mayan-previous-round-date "cal-mayan" "\
Move to previous instance of Mayan TZOLKIN-DATE HAAB-DATE combination.
Echo Mayan date unless NOECHO is non-nil.

\(fn TZOLKIN-DATE HAAB-DATE &optional NOECHO)" t nil)

(autoload 'calendar-mayan-goto-long-count-date "cal-mayan" "\
Move cursor to Mayan long count DATE.
Echo Mayan date unless NOECHO is non-nil.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-goto-day-of-year calendar-goto-date calendar-end-of-year
;;;;;;  calendar-beginning-of-year calendar-end-of-month calendar-beginning-of-month
;;;;;;  calendar-end-of-week calendar-beginning-of-week calendar-backward-week
;;;;;;  calendar-forward-week calendar-backward-day calendar-forward-day
;;;;;;  calendar-scroll-right-three-months calendar-scroll-toolkit-scroll
;;;;;;  calendar-scroll-left-three-months calendar-scroll-right calendar-scroll-left
;;;;;;  calendar-backward-year calendar-backward-month calendar-forward-year
;;;;;;  calendar-forward-month calendar-goto-today calendar-cursor-to-visible-date
;;;;;;  calendar-cursor-to-nearest-date) "cal-move" "cal-move.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-move.el

(autoload 'calendar-cursor-to-nearest-date "cal-move" "\
Move the cursor to the closest date.
The position of the cursor is unchanged if it is already on a date.
Returns the list (month day year) giving the cursor position.

\(fn)" nil nil)

(autoload 'calendar-cursor-to-visible-date "cal-move" "\
Move the cursor to DATE that is on the screen.

\(fn DATE)" nil nil)

(autoload 'calendar-goto-today "cal-move" "\
Reposition the calendar window so the current date is visible.

\(fn)" t nil)

(autoload 'calendar-forward-month "cal-move" "\
Move the cursor forward ARG months.
Movement is backward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-forward-year "cal-move" "\
Move the cursor forward by ARG years.
Movement is backward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-backward-month "cal-move" "\
Move the cursor backward by ARG months.
Movement is forward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-backward-year "cal-move" "\
Move the cursor backward ARG years.
Movement is forward is ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-scroll-left "cal-move" "\
Scroll the displayed calendar left by ARG months.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'.

\(fn &optional ARG EVENT)" t nil)

(autoload 'calendar-scroll-right "cal-move" "\
Scroll the displayed calendar window right by ARG months.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'.

\(fn &optional ARG EVENT)" t nil)

(autoload 'calendar-scroll-left-three-months "cal-move" "\
Scroll the displayed calendar window left by 3*ARG months.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'.

\(fn ARG &optional EVENT)" t nil)

(autoload 'calendar-scroll-toolkit-scroll "cal-move" "\
Function to scroll the calendar after a toolkit scroll-bar click.

\(fn EVENT)" t nil)

(autoload 'calendar-scroll-right-three-months "cal-move" "\
Scroll the displayed calendar window right by 3*ARG months.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'.

\(fn ARG &optional EVENT)" t nil)

(autoload 'calendar-forward-day "cal-move" "\
Move the cursor forward ARG days.
Moves backward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-backward-day "cal-move" "\
Move the cursor back ARG days.
Moves forward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-forward-week "cal-move" "\
Move the cursor forward ARG weeks.
Moves backward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-backward-week "cal-move" "\
Move the cursor back ARG weeks.
Moves forward if ARG is negative.

\(fn ARG)" t nil)

(autoload 'calendar-beginning-of-week "cal-move" "\
Move the cursor back ARG calendar-week-start-day's.

\(fn ARG)" t nil)

(autoload 'calendar-end-of-week "cal-move" "\
Move the cursor forward ARG calendar-week-start-day+6's.

\(fn ARG)" t nil)

(autoload 'calendar-beginning-of-month "cal-move" "\
Move the cursor backward ARG month beginnings.

\(fn ARG)" t nil)

(autoload 'calendar-end-of-month "cal-move" "\
Move the cursor forward ARG month ends.

\(fn ARG)" t nil)

(autoload 'calendar-beginning-of-year "cal-move" "\
Move the cursor backward ARG year beginnings.

\(fn ARG)" t nil)

(autoload 'calendar-end-of-year "cal-move" "\
Move the cursor forward ARG year beginnings.

\(fn ARG)" t nil)

(autoload 'calendar-goto-date "cal-move" "\
Move cursor to DATE.

\(fn DATE)" t nil)

(autoload 'calendar-goto-day-of-year "cal-move" "\
Move cursor to YEAR, DAY number; echo DAY/YEAR unless NOECHO is non-nil.
Negative DAY counts backward from end of year.

\(fn YEAR DAY &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (calendar-persian-goto-date calendar-persian-print-date
;;;;;;  calendar-persian-date-string) "cal-persia" "cal-persia.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-persia.el

(autoload 'calendar-persian-date-string "cal-persia" "\
String of Persian date of Gregorian DATE, default today.

\(fn &optional DATE)" nil nil)

(autoload 'calendar-persian-print-date "cal-persia" "\
Show the Persian calendar equivalent of the selected date.

\(fn)" t nil)

(autoload 'calendar-persian-goto-date "cal-persia" "\
Move cursor to Persian date DATE.
Echo Persian date unless NOECHO is non-nil.

\(fn DATE &optional NOECHO)" t nil)

;;;***

;;;### (autoloads (cal-tex-cursor-day cal-tex-cursor-filofax-daily
;;;;;;  cal-tex-cursor-filofax-week cal-tex-cursor-filofax-2week
;;;;;;  cal-tex-cursor-week-monday cal-tex-cursor-week-iso cal-tex-cursor-week2
;;;;;;  cal-tex-cursor-week cal-tex-cursor-month cal-tex-cursor-month-landscape
;;;;;;  cal-tex-cursor-filofax-year cal-tex-cursor-year-landscape
;;;;;;  cal-tex-cursor-year) "cal-tex" "cal-tex.el" (20400 62402))
;;; Generated autoloads from cal-tex.el

(autoload 'cal-tex-cursor-year "cal-tex" "\
Make a buffer with LaTeX commands for the year cursor is on.
Optional prefix argument N specifies number of years.
Optional EVENT indicates a buffer position to use instead of point.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-year-landscape "cal-tex" "\
Make a buffer with LaTeX commands for the year cursor is on.
Optional prefix argument N specifies number of years.
Optional EVENT indicates a buffer position to use instead of point.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-filofax-year "cal-tex" "\
Make a Filofax one page yearly calendar of year indicated by cursor.
Optional prefix argument N specifies number of years.
Optional EVENT indicates a buffer position to use instead of point.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-month-landscape "cal-tex" "\
Make a LaTeX calendar buffer for the month the cursor is on.
Optional prefix argument N specifies number of months to be
produced (default 1).  The output is in landscape format, one
month to a page.  It shows holiday and diary entries if
`cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.
Optional EVENT indicates a buffer position to use instead of point.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-month "cal-tex" "\
Make a LaTeX calendar buffer for the month the cursor is on.
Optional prefix argument N specifies number of months to be
produced (default 1).  The calendar is condensed onto one page.
It shows holiday and diary entries if `cal-tex-holidays' and
`cal-tex-diary', respectively, are non-nil.  Optional EVENT
indicates a buffer position to use instead of point.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-week "cal-tex" "\
Make a LaTeX calendar buffer for a two-page one-week calendar.
It applies to the week that point is in.  The optional prefix
argument N specifies number of weeks (default 1).  The calendar
shows holidays if `cal-tex-holidays' is non-nil (note that diary
entries are not shown).  The calendar shows the hours 8-12am, 1-5pm.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-week2 "cal-tex" "\
Make a LaTeX calendar buffer for a two-page one-week calendar.
It applies to the week that point is in.  Optional prefix
argument N specifies number of weeks (default 1).  The calendar
shows holidays if `cal-tex-holidays' is non-nil (note that diary
entries are not shown).  The calendar shows the hours 8-12am, 1-5pm.
Optional EVENT indicates a buffer position to use instead of point.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-week-iso "cal-tex" "\
Make a LaTeX calendar buffer for a one page ISO-style weekly calendar.
Optional prefix argument N specifies number of weeks (default 1).
The calendar shows holiday and diary entries if
`cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.
It does not show hours of the day.  Optional EVENT indicates a buffer
position to use instead of point.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-week-monday "cal-tex" "\
Make a LaTeX calendar buffer for a two-page one-week calendar.
It applies to the week that point is in, and starts on Monday.
Optional prefix argument N specifies number of weeks (default 1).
The calendar shows holidays if `cal-tex-holidays' is
non-nil (note that diary entries are not shown).   The calendar shows
the hours 8-12am, 1-5pm.  Optional EVENT indicates a buffer position
to use instead of point.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-filofax-2week "cal-tex" "\
Two-weeks-at-a-glance Filofax style calendar for week cursor is in.
Optional prefix argument N specifies number of weeks (default 1).
The calendar shows holiday and diary entries if
`cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.
Optional EVENT indicates a buffer position to use instead of point.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-filofax-week "cal-tex" "\
One-week-at-a-glance Filofax style calendar for week indicated by cursor.
Optional prefix argument N specifies number of weeks (default 1),
starting on Mondays.  The calendar shows holiday and diary entries
if `cal-tex-holidays' and `cal-tex-diary', respectively, are non-nil.
Optional EVENT indicates a buffer position to use instead of point.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-filofax-daily "cal-tex" "\
Day-per-page Filofax style calendar for week indicated by cursor.
Optional prefix argument N specifies number of weeks (default 1),
starting on Mondays.  The calendar shows holiday and diary
entries if `cal-tex-holidays' and `cal-tex-diary', respectively,
are non-nil.  Pages are ruled if `cal-tex-rules' is non-nil.
Optional EVENT indicates a buffer position to use instead of point.

\(fn &optional N EVENT)" t nil)

(autoload 'cal-tex-cursor-day "cal-tex" "\
Make a buffer with LaTeX commands for the day cursor is on.
Optional prefix argument N specifies number of days.  The calendar shows
the hours between `cal-tex-daily-start' and `cal-tex-daily-end', using
the 24-hour clock if `cal-tex-24' is non-nil.  Optional EVENT indicates
a buffer position to use instead of point.

\(fn &optional N EVENT)" t nil)

;;;***

;;;### (autoloads (calendar-two-frame-setup calendar-only-one-frame-setup
;;;;;;  calendar-one-frame-setup calendar-frame-setup) "cal-x" "cal-x.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-x.el

(autoload 'calendar-frame-setup "cal-x" "\
Display the calendar, and optionally the diary, in a separate frame.
CONFIG should be one of:
`calendar-only' - just the calendar, no diary
`one-frame'     - calendar and diary in a single frame
`two-frames'    - calendar and diary each in a separate frame

If CONFIG has any other value, or if the display is not capable of
multiple frames, then `calendar-basic-setup' is called.

If PROMPT is non-nil, prompt for the month and year to use.

\(fn CONFIG &optional PROMPT)" nil nil)

(autoload 'calendar-one-frame-setup "cal-x" "\
Display calendar and diary in a single dedicated frame.
See `calendar-frame-setup' for more information.

\(fn &optional PROMPT)" nil nil)

(autoload 'calendar-only-one-frame-setup "cal-x" "\
Display calendar in a dedicated frame.
See `calendar-frame-setup' for more information.

\(fn &optional PROMPT)" nil nil)

(autoload 'calendar-two-frame-setup "cal-x" "\
Display calendar and diary in separate, dedicated frames.
See `calendar-frame-setup' for more information.

\(fn &optional PROMPT)" nil nil)

;;;***

;;;### (autoloads (diary-font-lock-keywords diary-insert-cyclic-entry
;;;;;;  diary-insert-block-entry diary-insert-anniversary-entry diary-insert-yearly-entry
;;;;;;  diary-insert-monthly-entry diary-insert-weekly-entry diary-insert-entry
;;;;;;  diary-make-entry diary-mark-entries diary-show-all-entries
;;;;;;  diary-view-other-diary-entries diary-view-entries diary-set-maybe-redraw
;;;;;;  diary-live-p) "diary-lib" "diary-lib.el" (20352 65510))
;;; Generated autoloads from diary-lib.el

(autoload 'diary-live-p "diary-lib" "\
Return non-nil if the diary is being displayed.

\(fn)" nil nil)

(autoload 'diary-set-maybe-redraw "diary-lib" "\
Set SYMBOL's value to VALUE, and redraw the diary if necessary.
Redraws the diary if it is being displayed (note this is not the same as
just visiting the `diary-file'), and SYMBOL's value is to be changed.

\(fn SYMBOL VALUE)" nil nil)

(autoload 'diary-view-entries "diary-lib" "\
Prepare and display a buffer with diary entries.
Searches the file named in `diary-file' for entries that match
ARG days starting with the date indicated by the cursor position
in the displayed three-month calendar.

\(fn &optional ARG)" t nil)

(autoload 'diary-view-other-diary-entries "diary-lib" "\
Prepare and display buffer of diary entries from an alternative diary file.
Searches for entries that match ARG days, starting with the date indicated
by the cursor position in the displayed three-month calendar.
DFILE specifies the file to use as the diary file.

\(fn ARG DFILE)" t nil)

(define-obsolete-function-alias 'view-other-diary-entries 'diary-view-other-diary-entries "23.1")

(autoload 'diary-show-all-entries "diary-lib" "\
Show all of the diary entries in the diary file.
This function gets rid of the selective display of the diary file so that
all entries, not just some, are visible.  If there is no diary buffer, one
is created.

\(fn)" t nil)

(autoload 'diary-mark-entries "diary-lib" "\
Mark days in the calendar window that have diary entries.
Marks each entry in the diary that is visible in the calendar window.

After marking the entries, runs `diary-nongregorian-marking-hook'
for the main diary file, and each included file.  For example,
this is the appropriate hook to process Islamic entries in all
diary files.  Next `diary-mark-entries-hook' runs, for the main diary
file only.  If not using include files, there is no difference between
these two hooks.

If the optional argument REDRAW is non-nil (which is the case
interactively, for example) then this first removes any existing diary
marks.  This is intended to deal with deleted diary entries.

\(fn &optional REDRAW)" t nil)

(define-obsolete-function-alias 'mark-diary-entries 'diary-mark-entries "23.1")

(autoload 'diary-make-entry "diary-lib" "\
Insert a diary entry STRING which may be NONMARKING in FILE.
If omitted, NONMARKING defaults to nil and FILE defaults to
`diary-file'.

\(fn STRING &optional NONMARKING FILE)" nil nil)

(define-obsolete-function-alias 'make-diary-entry 'diary-make-entry "23.1")

(autoload 'diary-insert-entry "diary-lib" "\
Insert a diary entry for the date indicated by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG &optional EVENT)" t nil)

(define-obsolete-function-alias 'insert-diary-entry 'diary-insert-entry "23.1")

(autoload 'diary-insert-weekly-entry "diary-lib" "\
Insert a weekly diary entry for the day of the week indicated by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(define-obsolete-function-alias 'insert-weekly-diary-entry 'diary-insert-weekly-entry "23.1")

(autoload 'diary-insert-monthly-entry "diary-lib" "\
Insert a monthly diary entry for the day of the month indicated by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(define-obsolete-function-alias 'insert-monthly-diary-entry 'diary-insert-monthly-entry "23.1")

(autoload 'diary-insert-yearly-entry "diary-lib" "\
Insert an annual diary entry for the day of the year indicated by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(define-obsolete-function-alias 'insert-yearly-diary-entry 'diary-insert-yearly-entry "23.1")

(autoload 'diary-insert-anniversary-entry "diary-lib" "\
Insert an anniversary diary entry for the date given by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(define-obsolete-function-alias 'insert-anniversary-diary-entry 'diary-insert-anniversary-entry "23.1")

(autoload 'diary-insert-block-entry "diary-lib" "\
Insert a block diary entry for the days between the point and marked date.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(define-obsolete-function-alias 'insert-block-diary-entry 'diary-insert-block-entry "23.1")

(autoload 'diary-insert-cyclic-entry "diary-lib" "\
Insert a cyclic diary entry starting at the date given by point.
Prefix argument ARG makes the entry nonmarking.

\(fn ARG)" t nil)

(define-obsolete-function-alias 'insert-cyclic-diary-entry 'diary-insert-cyclic-entry "23.1")

(autoload 'diary-font-lock-keywords "diary-lib" "\
Return a value for the variable `diary-font-lock-keywords'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (calendar-mark-holidays calendar-cursor-holidays
;;;;;;  calendar-list-holidays) "holidays" "holidays.el" (20400 62402))
;;; Generated autoloads from holidays.el

(autoload 'calendar-list-holidays "holidays" "\
Create a buffer containing the holidays for the current calendar window.
The holidays are those in the list `calendar-notable-days'.
Returns non-nil if any holidays are found.
If EVENT is non-nil, it's an event indicating the buffer position to
use instead of point.

\(fn &optional EVENT)" t nil)

(autoload 'calendar-cursor-holidays "holidays" "\
Find holidays for the date specified by the cursor in the calendar window.
Optional DATE is a list (month day year) to use instead of the
cursor position.  EVENT specifies a buffer position to use for a date.

\(fn &optional DATE EVENT)" t nil)

(autoload 'calendar-mark-holidays "holidays" "\
Mark notable days in the calendar window.
If EVENT is non-nil, it's an event indicating the buffer position to
use instead of point.

\(fn &optional EVENT)" t nil)

;;;***

;;;### (autoloads (calendar-lunar-phases) "lunar" "lunar.el" (20352
;;;;;;  65510))
;;; Generated autoloads from lunar.el

(autoload 'calendar-lunar-phases "lunar" "\
Create a buffer with the lunar phases for the current calendar window.
If EVENT is non-nil, it's an event indicating the buffer position to
use instead of point.

\(fn &optional EVENT)" t nil)

(define-obsolete-function-alias 'calendar-phases-of-moon 'calendar-lunar-phases "23.1")

;;;***

;;;### (autoloads (calendar-sunrise-sunset-month calendar-sunrise-sunset)
;;;;;;  "solar" "solar.el" (20352 65510))
;;; Generated autoloads from solar.el

(autoload 'calendar-sunrise-sunset "solar" "\
Local time of sunrise and sunset for date under cursor.
Accurate to a few seconds.

\(fn &optional EVENT)" t nil)

(autoload 'calendar-sunrise-sunset-month "solar" "\
Local time of sunrise and sunset for month under cursor or at EVENT.

\(fn &optional EVENT)" t nil)

;;;***

;;;### (autoloads nil nil ("appt.el" "cal-dst.el" "cal-menu.el" "calendar.el"
;;;;;;  "icalendar.el" "parse-time.el" "time-date.el" "timeclock.el"
;;;;;;  "todo-mode.el") (20436 19800 821125))

;;;***

(provide 'cal-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cal-loaddefs.el ends here
