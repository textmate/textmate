;;; hol-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (holiday-bahai-ridvan holiday-bahai-new-year holiday-bahai)
;;;;;;  "cal-bahai" "cal-bahai.el" (20352 65510))
;;; Generated autoloads from cal-bahai.el

(autoload 'holiday-bahai "cal-bahai" "\
Holiday on MONTH, DAY (Bahá'í) called STRING.
If MONTH, DAY (Bahá'í) is visible in the current calendar window,
returns the corresponding Gregorian date in the form of the
list (((month day year) STRING)).  Otherwise, returns nil.

\(fn MONTH DAY STRING)" nil nil)

(autoload 'holiday-bahai-new-year "cal-bahai" "\
Holiday entry for the Bahá'í New Year, if visible in the calendar window.

\(fn)" nil nil)

(autoload 'holiday-bahai-ridvan "cal-bahai" "\
Holidays related to Ridvan, as visible in the calendar window.
Only considers the first, ninth, and twelfth days, unless ALL or
`calendar-bahai-all-holidays-flag' is non-nil.

\(fn &optional ALL)" nil nil)

;;;***

;;;### (autoloads (holiday-chinese holiday-chinese-winter-solstice
;;;;;;  holiday-chinese-qingming holiday-chinese-new-year) "cal-china"
;;;;;;  "cal-china.el" (20352 65510))
;;; Generated autoloads from cal-china.el

(autoload 'holiday-chinese-new-year "cal-china" "\
Date of Chinese New Year, if visible in calendar.
Returns (((MONTH DAY YEAR) TEXT)), where the date is Gregorian.

\(fn)" nil nil)

(autoload 'holiday-chinese-qingming "cal-china" "\
Date of Chinese Qingming Festival, if visible in calendar.
Returns (((MONTH DAY YEAR) TEXT)), where the date is Gregorian.

\(fn)" nil nil)

(autoload 'holiday-chinese-winter-solstice "cal-china" "\
Date of Chinese winter solstice, if visible in calendar.
Returns (((MONTH DAY YEAR) TEXT)), where the date is Gregorian.

\(fn)" nil nil)

(autoload 'holiday-chinese "cal-china" "\
Holiday on Chinese MONTH, DAY called STRING.
If MONTH, DAY (Chinese) is visible, returns the corresponding
Gregorian date as the list (((month day year) STRING)).
Returns nil if it is not visible in the current calendar window.

\(fn MONTH DAY STRING)" nil nil)

;;;***

;;;### (autoloads (holiday-hebrew-misc holiday-hebrew-tisha-b-av
;;;;;;  holiday-hebrew-passover holiday-hebrew-hanukkah holiday-hebrew-rosh-hashanah
;;;;;;  holiday-hebrew) "cal-hebrew" "cal-hebrew.el" (20352 65510))
;;; Generated autoloads from cal-hebrew.el

(autoload 'holiday-hebrew "cal-hebrew" "\
Holiday on MONTH, DAY (Hebrew) called STRING.
If MONTH, DAY (Hebrew) is visible, the value returned is corresponding
Gregorian date in the form of the list (((month day year) STRING)).  Returns
nil if it is not visible in the current calendar window.

\(fn MONTH DAY STRING)" nil nil)

(autoload 'holiday-hebrew-rosh-hashanah "cal-hebrew" "\
List of dates related to Rosh Hashanah, as visible in calendar window.
Shows only the major holidays, unless `calendar-hebrew-all-holidays-flag'
or ALL is non-nil.

\(fn &optional ALL)" nil nil)

(define-obsolete-function-alias 'holiday-rosh-hashanah-etc 'holiday-hebrew-rosh-hashanah "23.1")

(autoload 'holiday-hebrew-hanukkah "cal-hebrew" "\
List of dates related to Hanukkah, as visible in calendar window.
Shows only Hanukkah, unless `calendar-hebrew-all-holidays-flag' or ALL
is non-nil.

\(fn &optional ALL)" nil nil)

(define-obsolete-function-alias 'holiday-hanukkah 'holiday-hebrew-hanukkah "23.1")

(autoload 'holiday-hebrew-passover "cal-hebrew" "\
List of dates related to Passover, as visible in calendar window.
Shows only the major holidays, unless `calendar-hebrew-all-holidays-flag'
or ALL is non-nil.

\(fn &optional ALL)" nil nil)

(define-obsolete-function-alias 'holiday-passover-etc 'holiday-hebrew-passover "23.1")

(autoload 'holiday-hebrew-tisha-b-av "cal-hebrew" "\
List of dates around Tisha B'Av, as visible in calendar window.

\(fn)" nil nil)

(define-obsolete-function-alias 'holiday-tisha-b-av-etc 'holiday-hebrew-tisha-b-av "23.1")

(autoload 'holiday-hebrew-misc "cal-hebrew" "\
Miscellaneous Hebrew holidays, if visible in calendar window.
Includes: Tal Umatar, Tzom Teveth, Tu B'Shevat, Shabbat Shirah, and
Kiddush HaHamah.

\(fn)" nil nil)

;;;***

;;;### (autoloads (holiday-islamic-new-year holiday-islamic) "cal-islam"
;;;;;;  "cal-islam.el" (20352 65510))
;;; Generated autoloads from cal-islam.el

(autoload 'holiday-islamic "cal-islam" "\
Holiday on MONTH, DAY (Islamic) called STRING.
If MONTH, DAY (Islamic) is visible, returns the corresponding
Gregorian date as the list (((month day year) STRING)).
Returns nil if it is not visible in the current calendar window.

\(fn MONTH DAY STRING)" nil nil)

(autoload 'holiday-islamic-new-year "cal-islam" "\
Holiday entry for the Islamic New Year, if visible in the calendar window.

\(fn)" nil nil)

;;;***

;;;### (autoloads (holiday-julian) "cal-julian" "cal-julian.el" (20352
;;;;;;  65510))
;;; Generated autoloads from cal-julian.el

(autoload 'holiday-julian "cal-julian" "\
Holiday on MONTH, DAY (Julian) called STRING.
If MONTH, DAY (Julian) is visible, the value returned is corresponding
Gregorian date in the form of the list (((month day year) STRING)).  Returns
nil if it is not visible in the current calendar window.

\(fn MONTH DAY STRING)" nil nil)

;;;***

;;;### (autoloads (solar-equinoxes-solstices) "solar" "solar.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from solar.el

(autoload 'solar-equinoxes-solstices "solar" "\
Local date and time of equinoxes and solstices, if visible in the calendar.
Requires floating point.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("appt.el" "cal-coptic.el" "cal-dst.el"
;;;;;;  "cal-french.el" "cal-html.el" "cal-iso.el" "cal-loaddefs.el"
;;;;;;  "cal-mayan.el" "cal-menu.el" "cal-move.el" "cal-persia.el"
;;;;;;  "cal-tex.el" "cal-x.el" "calendar.el" "diary-lib.el" "diary-loaddefs.el"
;;;;;;  "holidays.el" "icalendar.el" "lunar.el" "parse-time.el" "time-date.el"
;;;;;;  "timeclock.el" "todo-mode.el") (20436 19803 619433))

;;;***

(provide 'hol-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; hol-loaddefs.el ends here
