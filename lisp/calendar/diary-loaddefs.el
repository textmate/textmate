;;; diary-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (diary-bahai-date diary-bahai-mark-entries calendar-bahai-mark-date-pattern
;;;;;;  diary-bahai-list-entries) "cal-bahai" "cal-bahai.el" (20352
;;;;;;  65510))
;;; Generated autoloads from cal-bahai.el

(autoload 'diary-bahai-list-entries "cal-bahai" "\
Add any Bahá'í date entries from the diary file to `diary-entries-list'.
Bahá'í date diary entries must be prefaced by `diary-bahai-entry-symbol'
\(normally a `B').  The same diary date forms govern the style of the
Bahá'í calendar entries, except that the Bahá'í month names cannot be
abbreviated.  The Bahá'í months are numbered from 1 to 19 with Bahá being
1 and 19 being `Alá.  If a Bahá'í date diary entry begins with
`diary-nonmarking-symbol', the entry will appear in the diary listing, but
will not be marked in the calendar.  This function is provided for use with
`diary-nongregorian-listing-hook'.

\(fn)" nil nil)

(autoload 'calendar-bahai-mark-date-pattern "cal-bahai" "\
Mark dates in calendar window that conform to Bahá'í date MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard.  Optional argument COLOR is
passed to `calendar-mark-visible-date' as MARK.

\(fn MONTH DAY YEAR &optional COLOR)" nil nil)

(autoload 'diary-bahai-mark-entries "cal-bahai" "\
Mark days in the calendar window that have Bahá'í date diary entries.
Marks each entry in `diary-file' (or included files) visible in the calendar
window.  See `diary-bahai-list-entries' for more information.

\(fn)" nil nil)

(autoload 'diary-bahai-date "cal-bahai" "\
Bahá'í calendar equivalent of date diary entry.

\(fn)" nil nil)

;;;***

;;;### (autoloads (diary-chinese-date) "cal-china" "cal-china.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-china.el

(autoload 'diary-chinese-date "cal-china" "\
Chinese calendar equivalent of date diary entry.

\(fn)" nil nil)

;;;***

;;;### (autoloads (diary-ethiopic-date diary-coptic-date) "cal-coptic"
;;;;;;  "cal-coptic.el" (20352 65510))
;;; Generated autoloads from cal-coptic.el

(autoload 'diary-coptic-date "cal-coptic" "\
Coptic calendar equivalent of date diary entry.

\(fn)" nil nil)

(autoload 'diary-ethiopic-date "cal-coptic" "\
Ethiopic calendar equivalent of date diary entry.

\(fn)" nil nil)

;;;***

;;;### (autoloads (diary-french-date) "cal-french" "cal-french.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-french.el

(autoload 'diary-french-date "cal-french" "\
French calendar equivalent of date diary entry.

\(fn)" nil nil)

;;;***

;;;### (autoloads (diary-hebrew-sabbath-candles diary-hebrew-parasha
;;;;;;  diary-hebrew-rosh-hodesh diary-hebrew-yahrzeit diary-hebrew-omer
;;;;;;  diary-hebrew-birthday diary-hebrew-date diary-hebrew-mark-entries
;;;;;;  calendar-hebrew-mark-date-pattern diary-hebrew-list-entries)
;;;;;;  "cal-hebrew" "cal-hebrew.el" (20352 65510))
;;; Generated autoloads from cal-hebrew.el

(autoload 'diary-hebrew-list-entries "cal-hebrew" "\
Add any Hebrew date entries from the diary file to `diary-entries-list'.
Hebrew date diary entries must be prefaced by `diary-hebrew-entry-symbol'
\(normally an `H').  The same diary date forms govern the style
of the Hebrew calendar entries, except that the Hebrew month
names cannot be abbreviated.  The Hebrew months are numbered
from 1 to 13 with Nisan being 1, 12 being Adar I and 13 being
Adar II; you must use `Adar I' if you want Adar of a common
Hebrew year.  If a Hebrew date diary entry begins with
`diary-nonmarking-symbol', the entry will appear in the diary
listing, but will not be marked in the calendar.  This function
is provided for use with `diary-nongregorian-listing-hook'.

\(fn)" nil nil)

(define-obsolete-function-alias 'list-hebrew-diary-entries 'diary-hebrew-list-entries "23.1")

(autoload 'calendar-hebrew-mark-date-pattern "cal-hebrew" "\
Mark dates in calendar window that conform to Hebrew date MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard.  Optional argument COLOR is
passed to `calendar-mark-visible-date' as MARK.

\(fn MONTH DAY YEAR &optional COLOR)" nil nil)

(define-obsolete-function-alias 'mark-hebrew-calendar-date-pattern 'calendar-hebrew-mark-date-pattern "23.1")

(autoload 'diary-hebrew-mark-entries "cal-hebrew" "\
Mark days in the calendar window that have Hebrew date diary entries.
Marks each entry in `diary-file' (or included files) visible in the calendar
window.  See `list-hebrew-diary-entries' for more information.

\(fn)" nil nil)

(define-obsolete-function-alias 'mark-hebrew-diary-entries 'diary-hebrew-mark-entries "23.1")

(define-obsolete-function-alias 'insert-hebrew-diary-entry 'diary-hebrew-insert-entry "23.1")

(define-obsolete-function-alias 'insert-monthly-hebrew-diary-entry 'diary-hebrew-insert-monthly-entry "23.1")

(define-obsolete-function-alias 'insert-yearly-hebrew-diary-entry 'diary-hebrew-insert-yearly-entry "23.1")

(autoload 'diary-hebrew-date "cal-hebrew" "\
Hebrew calendar equivalent of date diary entry.

\(fn)" nil nil)

(autoload 'diary-hebrew-birthday "cal-hebrew" "\
Hebrew birthday diary entry.
Entry applies if date is birthdate (MONTH DAY YEAR), or the day before.
The order of the input parameters changes according to
`calendar-date-style' (e.g. to DAY MONTH YEAR in the European style).

Assumes the associated diary entry is the name of the person.

Although the date of birth is specified by the *civil* calendar,
this function determines the proper Hebrew calendar birthday.
If the optional argument AFTER-SUNSET is non-nil, this means the
birth occurred after local sunset on the given civil date.
In this case, the following civil date corresponds to the Hebrew birthday.

\(fn MONTH DAY YEAR &optional AFTER-SUNSET)" nil nil)

(autoload 'diary-hebrew-omer "cal-hebrew" "\
Omer count diary entry.
Entry applies if date is within 50 days after Passover.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar.

\(fn &optional MARK)" nil nil)

(define-obsolete-function-alias 'diary-omer 'diary-hebrew-omer "23.1")

(autoload 'diary-hebrew-yahrzeit "cal-hebrew" "\
Yahrzeit diary entry--entry applies if date is Yahrzeit or the day before.
Parameters are DEATH-MONTH, DEATH-DAY, DEATH-YEAR; the diary
entry is assumed to be the name of the person.  Although the date
of death is specified by the civil calendar, the proper Hebrew
calendar Yahrzeit is determined.

If the death occurred after local sunset on the given civil date,
the following civil date corresponds to the Hebrew date of
death--set the optional parameter AFTER-SUNSET non-nil in this case.

The order of the input parameters changes according to `calendar-date-style'
\(e.g. to DEATH-DAY, DEATH-MONTH, DEATH-YEAR in the European style).

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar.

\(fn DEATH-MONTH DEATH-DAY DEATH-YEAR &optional MARK AFTER-SUNSET)" nil nil)

(define-obsolete-function-alias 'diary-yahrzeit 'diary-hebrew-yahrzeit "23.1")

(autoload 'diary-hebrew-rosh-hodesh "cal-hebrew" "\
Rosh Hodesh diary entry.
Entry applies if date is Rosh Hodesh, the day before, or the Saturday before.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar.

\(fn &optional MARK)" nil nil)

(define-obsolete-function-alias 'diary-rosh-hodesh 'diary-hebrew-rosh-hodesh "23.1")

(autoload 'diary-hebrew-parasha "cal-hebrew" "\
Parasha diary entry--entry applies if date is a Saturday.
An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar.

\(fn &optional MARK)" nil nil)

(autoload 'diary-hebrew-sabbath-candles "cal-hebrew" "\
Local time of candle lighting diary entry--applies if date is a Friday.
No diary entry if there is no sunset on that date.  Uses
`diary-hebrew-sabbath-candles-minutes'.

An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar.

\(fn &optional MARK)" nil nil)

(define-obsolete-function-alias 'diary-sabbath-candles 'diary-hebrew-sabbath-candles "23.1")

;;;***

;;;### (autoloads (diary-islamic-date diary-islamic-mark-entries
;;;;;;  calendar-islamic-mark-date-pattern diary-islamic-list-entries)
;;;;;;  "cal-islam" "cal-islam.el" (20352 65510))
;;; Generated autoloads from cal-islam.el

(autoload 'diary-islamic-list-entries "cal-islam" "\
Add any Islamic date entries from the diary file to `diary-entries-list'.
Islamic date diary entries must be prefaced by `diary-islamic-entry-symbol'
\(normally an `I').  The same `diary-date-forms' govern the style
of the Islamic calendar entries, except that the Islamic month
names cannot be abbreviated.  The Islamic months are numbered
from 1 to 12 with Muharram being 1 and 12 being Dhu al-Hijjah.
If an Islamic date diary entry begins with `diary-nonmarking-symbol',
the entry will appear in the diary listing, but will not be
marked in the calendar.  This function is provided for use with
`diary-nongregorian-listing-hook'.

\(fn)" nil nil)

(autoload 'calendar-islamic-mark-date-pattern "cal-islam" "\
Mark dates in calendar window that conform to Islamic date MONTH/DAY/YEAR.
A value of 0 in any position is a wildcard.  Optional argument COLOR is
passed to `calendar-mark-visible-date' as MARK.

\(fn MONTH DAY YEAR &optional COLOR)" nil nil)

(autoload 'diary-islamic-mark-entries "cal-islam" "\
Mark days in the calendar window that have Islamic date diary entries.
Marks each entry in `diary-file' (or included files) visible in the calendar
window.  See `diary-islamic-list-entries' for more information.

\(fn)" nil nil)

(autoload 'diary-islamic-date "cal-islam" "\
Islamic calendar equivalent of date diary entry.

\(fn)" nil nil)

;;;***

;;;### (autoloads (diary-iso-date) "cal-iso" "cal-iso.el" (20352
;;;;;;  65510))
;;; Generated autoloads from cal-iso.el

(autoload 'diary-iso-date "cal-iso" "\
ISO calendar equivalent of date diary entry.

\(fn)" nil nil)

;;;***

;;;### (autoloads (diary-astro-day-number diary-julian-date) "cal-julian"
;;;;;;  "cal-julian.el" (20352 65510))
;;; Generated autoloads from cal-julian.el

(autoload 'diary-julian-date "cal-julian" "\
Julian calendar equivalent of date diary entry.

\(fn)" nil nil)

(autoload 'diary-astro-day-number "cal-julian" "\
Astronomical (Julian) day number diary entry.

\(fn)" nil nil)

;;;***

;;;### (autoloads (diary-mayan-date) "cal-mayan" "cal-mayan.el" (20352
;;;;;;  65510))
;;; Generated autoloads from cal-mayan.el

(autoload 'diary-mayan-date "cal-mayan" "\
Show the Mayan long count, haab, and tzolkin dates as a diary entry.

\(fn)" nil nil)

;;;***

;;;### (autoloads (diary-persian-date) "cal-persia" "cal-persia.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from cal-persia.el

(autoload 'diary-persian-date "cal-persia" "\
Persian calendar equivalent of date diary entry.

\(fn)" nil nil)

;;;***

;;;### (autoloads (calendar-check-holidays calendar-holiday-list)
;;;;;;  "holidays" "holidays.el" (20400 62402))
;;; Generated autoloads from holidays.el

(autoload 'calendar-holiday-list "holidays" "\
Form the list of holidays that occur on dates in the calendar window.
The holidays are those in the list `calendar-holidays'.

\(fn)" nil nil)

(autoload 'calendar-check-holidays "holidays" "\
Check the list of holidays for any that occur on DATE.
DATE is a list (month day year).  This function considers the
holidays from the list `calendar-holidays', and returns a list of
strings describing those holidays that apply on DATE, or nil if none do.

\(fn DATE)" nil nil)

;;;***

;;;### (autoloads (diary-lunar-phases) "lunar" "lunar.el" (20352
;;;;;;  65510))
;;; Generated autoloads from lunar.el

(autoload 'diary-lunar-phases "lunar" "\
Moon phases diary entry.
An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar.

\(fn &optional MARK)" nil nil)

(define-obsolete-function-alias 'diary-phases-of-moon 'diary-lunar-phases "23.1")

;;;***

;;;### (autoloads (diary-sunrise-sunset) "solar" "solar.el" (20352
;;;;;;  65510))
;;; Generated autoloads from solar.el

(autoload 'diary-sunrise-sunset "solar" "\
Local time of sunrise and sunset as a diary entry.
Accurate to a few seconds.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("appt.el" "cal-dst.el" "cal-html.el" "cal-loaddefs.el"
;;;;;;  "cal-menu.el" "cal-move.el" "cal-tex.el" "cal-x.el" "calendar.el"
;;;;;;  "diary-lib.el" "icalendar.el" "parse-time.el" "time-date.el"
;;;;;;  "timeclock.el" "todo-mode.el") (20436 19802 347198))

;;;***

(provide 'diary-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; diary-loaddefs.el ends here
