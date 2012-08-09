;;; cal-china.el --- calendar functions for the Chinese calendar

;; Copyright (C) 1995, 1997, 2001-2012  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Chinese calendar, calendar, holidays, diary
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

;; See calendar.el.

;; The rules used for the Chinese calendar are those of Baolin Liu
;; (see L. E. Doggett's article "Calendars" in the Explanatory
;; Supplement to the Astronomical Almanac, second edition, 1992) for
;; the calendar as revised at the beginning of the Qing dynasty in
;; 1644.  The nature of the astronomical calculations is such that
;; precise calculations cannot be made without great expense in time,
;; so that the calendars produced may not agree perfectly with
;; published tables--but no two pairs of published tables agree
;; perfectly either!  Liu's rules produce a calendar for 2033 which is
;; not accepted by all authorities.  The date of Chinese New Year is
;; correct from 1644-2051.

;; Note to maintainers:
;; Use `chinese-year-cache-init' every few years to recenter the default
;; value of `chinese-year-cache'.

;;; Code:

(require 'calendar)
(require 'lunar)                        ; lunar-new-moon-on-or-after
;; solar-date-next-longitude brought in by lunar.
;;;(require 'solar)
;; calendar-astro-to-absolute and from-absolute are cal-autoloads.
;;;(require 'cal-julian)


(defgroup calendar-chinese nil
  "Chinese calendar support."
  :prefix "calendar-chinese-"
  :group 'calendar)

(define-obsolete-variable-alias 'chinese-calendar-time-zone
  'calendar-chinese-time-zone "23.1")

(defcustom calendar-chinese-time-zone
  '(if (< year 1928)
       (+ 465 (/ 40.0 60.0))
     480)
  "Minutes difference between local standard time for Chinese calendar and UTC.
Default is for Beijing.  This is an expression in `year' since it changed at
1928-01-01 00:00:00 from UT+7:45:40 to UT+8."
  :type 'sexp
  :group 'calendar-chinese)

;; It gets eval'd.
;;;###autoload
(put 'calendar-chinese-time-zone 'risky-local-variable t)
;;;###autoload
(put 'chinese-calendar-time-zone 'risky-local-variable t)


(define-obsolete-variable-alias 'chinese-calendar-location-name
  'calendar-chinese-location-name "23.1")

;; FIXME unused.
(defcustom calendar-chinese-location-name "Beijing"
  "Name of location used for calculation of Chinese calendar."
  :type 'string
  :group 'calendar-chinese)

(define-obsolete-variable-alias 'chinese-calendar-daylight-time-offset
  'calendar-chinese-daylight-time-offset "23.1")

(defcustom calendar-chinese-daylight-time-offset 0
;; The correct value is as follows, but the Chinese calendrical
;; authorities do NOT use DST in determining astronomical events:
;;  60
  "Minutes difference between daylight saving and standard time.
Default is for no daylight saving time."
  :type 'integer
  :group 'calendar-chinese)

(define-obsolete-variable-alias 'chinese-calendar-standard-time-zone-name
  'calendar-chinese-standard-time-zone-name "23.1")

(defcustom calendar-chinese-standard-time-zone-name
  '(if (< year 1928)
       "PMT"
     "CST")
  "Abbreviated name of standard time zone used for Chinese calendar.
This is an expression depending on `year' because it changed
at 1928-01-01 00:00:00 from `PMT' to `CST'."
  :type 'sexp
  :group 'calendar-chinese)

(define-obsolete-variable-alias 'chinese-calendar-daylight-time-zone-name
  'calendar-chinese-daylight-time-zone-name "23.1")

(defcustom calendar-chinese-daylight-time-zone-name "CDT"
  "Abbreviated name of daylight saving time zone used for Chinese calendar."
  :type 'string
  :group 'calendar-chinese)

(define-obsolete-variable-alias 'chinese-calendar-daylight-savings-starts
  'calendar-chinese-daylight-saving-start "23.1")

(defcustom calendar-chinese-daylight-saving-start nil
;; The correct value is as follows, but the Chinese calendrical
;; authorities do NOT use DST in determining astronomical events:
;;  '(cond ((< 1986 year) (calendar-nth-named-day 1 0 4 year 10))
;;         ((= 1986 year) '(5 4 1986))
;;         (t nil))
  "Sexp giving the date on which daylight saving time starts.
Default is for no daylight saving time.  See documentation of
`calendar-daylight-savings-starts'."
  :type 'sexp
  :group 'calendar-chinese)

(define-obsolete-variable-alias 'chinese-calendar-daylight-savings-ends
  'calendar-chinese-daylight-saving-end "23.1")

(defcustom calendar-chinese-daylight-saving-end nil
;; The correct value is as follows, but the Chinese calendrical
;; authorities do NOT use DST in determining astronomical events:
;;  '(if (<= 1986 year) (calendar-nth-named-day 1 0 9 year 11))
  "Sexp giving the date on which daylight saving time ends.
Default is for no daylight saving time.  See documentation of
`calendar-daylight-savings-ends'."
  :type 'sexp
  :group 'calendar-chinese)

(define-obsolete-variable-alias 'chinese-calendar-daylight-savings-starts-time
  'calendar-chinese-daylight-saving-start-time "23.1")

(defcustom calendar-chinese-daylight-saving-start-time 0
  "Number of minutes after midnight that daylight saving time starts.
Default is for no daylight saving time."
  :type 'integer
  :group 'calendar-chinese)

(define-obsolete-variable-alias 'chinese-calendar-daylight-savings-ends-time
  'calendar-chinese-daylight-saving-end-time "23.1")

(defcustom calendar-chinese-daylight-saving-end-time 0
  "Number of minutes after midnight that daylight saving time ends.
Default is for no daylight saving time."
  :type 'integer
  :group 'calendar-chinese)

(define-obsolete-variable-alias 'chinese-calendar-celestial-stem
  'calendar-chinese-celestial-stem "23.1")

(defcustom calendar-chinese-celestial-stem
  ["Jia" "Yi" "Bing" "Ding" "Wu" "Ji" "Geng" "Xin" "Ren" "Gui"]
  "Prefixes used by `calendar-chinese-sexagesimal-name'."
  :group 'calendar-chinese
  :type '(vector (string :tag "Jia")
                 (string :tag "Yi")
                 (string :tag "Bing")
                 (string :tag "Ding")
                 (string :tag "Wu")
                 (string :tag "Ji")
                 (string :tag "Geng")
                 (string :tag "Xin")
                 (string :tag "Ren")
                 (string :tag "Gui")))

(define-obsolete-variable-alias 'chinese-calendar-terrestrial-branch
  'calendar-chinese-terrestrial-branch "23.1")

(defcustom calendar-chinese-terrestrial-branch
  ["Zi" "Chou" "Yin" "Mao" "Chen" "Si" "Wu" "Wei" "Shen" "You" "Xu" "Hai"]
  "Suffixes used by `calendar-chinese-sexagesimal-name'."
  :group 'calendar-chinese
  :type '(vector (string :tag "Zi")
                 (string :tag "Chou")
                 (string :tag "Yin")
                 (string :tag "Mao")
                 (string :tag "Chen")
                 (string :tag "Si")
                 (string :tag "Wu")
                 (string :tag "Wei")
                 (string :tag "Shen")
                 (string :tag "You")
                 (string :tag "Xu")
                 (string :tag "Hai")))

;;; End of user options.


(defun calendar-chinese-sexagesimal-name (n)
  "The N-th name of the Chinese sexagesimal cycle.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 60 gives the sixtieth name."
  (format "%s-%s"
          (aref calendar-chinese-celestial-stem (% (1- n) 10))
          (aref calendar-chinese-terrestrial-branch (% (1- n) 12))))

(defun calendar-chinese-zodiac-sign-on-or-after (d)
  "Absolute date of first new Zodiac sign on or after absolute date D.
The Zodiac signs begin when the sun's longitude is a multiple of 30 degrees."
 (let* ((year (calendar-extract-year (calendar-gregorian-from-absolute d)))
         (calendar-time-zone (eval calendar-chinese-time-zone)) ; uses year
         (calendar-daylight-time-offset
          calendar-chinese-daylight-time-offset)
         (calendar-standard-time-zone-name
          calendar-chinese-standard-time-zone-name)
         (calendar-daylight-time-zone-name
          calendar-chinese-daylight-time-zone-name)
         (calendar-daylight-savings-starts
          calendar-chinese-daylight-saving-start)
         (calendar-daylight-savings-ends
          calendar-chinese-daylight-saving-end)
         (calendar-daylight-savings-starts-time
          calendar-chinese-daylight-saving-start-time)
         (calendar-daylight-savings-ends-time
          calendar-chinese-daylight-saving-end-time))
   (floor
    (calendar-astro-to-absolute
     (solar-date-next-longitude (calendar-astro-from-absolute d) 30)))))

(defun calendar-chinese-new-moon-on-or-after (d)
  "Absolute date of first new moon on or after absolute date D."
  (let* ((year (calendar-extract-year (calendar-gregorian-from-absolute d)))
         (calendar-time-zone (eval calendar-chinese-time-zone))
         (calendar-daylight-time-offset
          calendar-chinese-daylight-time-offset)
         (calendar-standard-time-zone-name
          calendar-chinese-standard-time-zone-name)
         (calendar-daylight-time-zone-name
          calendar-chinese-daylight-time-zone-name)
         (calendar-daylight-savings-starts
          calendar-chinese-daylight-saving-start)
         (calendar-daylight-savings-ends
          calendar-chinese-daylight-saving-end)
         (calendar-daylight-savings-starts-time
          calendar-chinese-daylight-saving-start-time)
         (calendar-daylight-savings-ends-time
          calendar-chinese-daylight-saving-end-time))
    (floor
     (calendar-astro-to-absolute
      (lunar-new-moon-on-or-after (calendar-astro-from-absolute d))))))

(defun calendar-chinese-month-list (start end)
  "List of starting dates of Chinese months from START to END."
  (if (<= start end)
      (let ((new-moon (calendar-chinese-new-moon-on-or-after start)))
        (if (<= new-moon end)
            (cons new-moon
                  (calendar-chinese-month-list (1+ new-moon) end))))))

(defun calendar-chinese-number-months (list start)
  "Assign month numbers to the lunar months in LIST, starting with START.
Numbers are assigned sequentially, START, START+1, ..., 11, with
half numbers used for leap months.  First and last months of list
are never leap months."
  (when list
    (cons (list start (car list))       ; first month
          ;; Remaining months.
          (if (zerop (- 12 start (length list)))
              ;; List is too short for a leap month.
              (calendar-chinese-number-months (cdr list) (1+ start))
            (if (and (cddr list)        ; at least two more months...
                     (<= (nth 2 list)
                         (calendar-chinese-zodiac-sign-on-or-after
                          (cadr list))))
                ;; Next month is a leap month.
                (cons (list (+ start 0.5) (cadr list))
                      (calendar-chinese-number-months (cddr list) (1+ start)))
              ;; Next month is not a leap month.
              (calendar-chinese-number-months (cdr list) (1+ start)))))))

(defun calendar-chinese-compute-year (y)
  "Compute the structure of the Chinese year for Gregorian year Y.
The result is a list of pairs (i d), where month i begins on absolute date d,
of the Chinese months from the Chinese month following the solstice in
Gregorian year Y-1 to the Chinese month of the solstice of Gregorian year Y."
  (let* ((next-solstice (calendar-chinese-zodiac-sign-on-or-after
                         (calendar-absolute-from-gregorian
                          (list 12 15 y))))
         (list (calendar-chinese-month-list
                (1+ (calendar-chinese-zodiac-sign-on-or-after
                     (calendar-absolute-from-gregorian
                      (list 12 15 (1- y)))))
                next-solstice))
         (next-sign (calendar-chinese-zodiac-sign-on-or-after (car list))))
    (if (= (length list) 12)
        ;; No room for a leap month, just number them 12, 1, 2, ..., 11.
        (cons (list 12 (car list))
              (calendar-chinese-number-months (cdr list) 1))
      ;; Now we can assign numbers to the list for y.
      ;; The first month or two are special.
      (if (or (> (car list) next-sign) (>= next-sign (cadr list)))
          ;; First month on list is a leap month, second is not.
          (append (list (list 11.5 (car list))
                        (list 12 (cadr list)))
                  (calendar-chinese-number-months (cddr list) 1))
        ;; First month on list is not a leap month.
        (append (list (list 12 (car list)))
                (if (>= (calendar-chinese-zodiac-sign-on-or-after (cadr list))
                        (nth 2 list))
                    ;; Second month on list is a leap month.
                    (cons (list 12.5 (cadr list))
                          (calendar-chinese-number-months (cddr list) 1))
                  ;; Second month on list is not a leap month.
                  (calendar-chinese-number-months (cdr list) 1)))))))

(defvar calendar-chinese-year-cache
  ;; Maintainers: delete existing value, position point at start of
  ;; empty line, then call  M-: (calendar-chinese-year-cache-init N)
  '((2000 (12 730126) (1 730155) (2 730185) (3 730215) (4 730244) (5 730273)
          (6 730303) (7 730332) (8 730361) (9 730391) (10 730420) (11 730450))
    (2001 (12 730480) (1 730509) (2 730539) (3 730569) (4 730598) (4.5 730628)
          (5 730657) (6 730687) (7 730716) (8 730745) (9 730775) (10 730804)
          (11 730834))
    (2002 (12 730863) (1 730893) (2 730923) (3 730953) (4 730982) (5 731012)
          (6 731041) (7 731071) (8 731100) (9 731129) (10 731159) (11 731188))
    (2003 (12 731218) (1 731247) (2 731277) (3 731307) (4 731336) (5 731366)
          (6 731396) (7 731425) (8 731455) (9 731484) (10 731513) (11 731543))
    (2004 (12 731572) (1 731602) (2 731631) (2.5 731661) (3 731690) (4 731720)
          (5 731750) (6 731779) (7 731809) (8 731838) (9 731868) (10 731897)
          (11 731927))
    (2005 (12 731956) (1 731986) (2 732015) (3 732045) (4 732074) (5 732104)
          (6 732133) (7 732163) (8 732193) (9 732222) (10 732252) (11 732281))
    (2006 (12 732311) (1 732340) (2 732370) (3 732399) (4 732429) (5 732458)
          (6 732488) (7 732517) (7.5 732547) (8 732576) (9 732606) (10 732636)
          (11 732665))
    (2007 (12 732695) (1 732725) (2 732754) (3 732783) (4 732813) (5 732842)
          (6 732871) (7 732901) (8 732930) (9 732960) (10 732990) (11 733020))
    (2008 (12 733049) (1 733079) (2 733109) (3 733138) (4 733167) (5 733197)
          (6 733226) (7 733255) (8 733285) (9 733314) (10 733344) (11 733374))
    (2009 (12 733403) (1 733433) (2 733463) (3 733493) (4 733522) (5 733551)
          (5.5 733581) (6 733610) (7 733639) (8 733669) (9 733698) (10 733728)
          (11 733757))
    (2010 (12 733787) (1 733817) (2 733847) (3 733876) (4 733906) (5 733935)
          (6 733965) (7 733994) (8 734023) (9 734053) (10 734082) (11 734112))
    (2011 (12 734141) (1 734171) (2 734201) (3 734230) (4 734260) (5 734290)
          (6 734319) (7 734349) (8 734378) (9 734407) (10 734437) (11 734466))
    (2012 (12 734496) (1 734525) (2 734555) (3 734584) (4 734614) (4.5 734644)
          (5 734673) (6 734703) (7 734732) (8 734762) (9 734791) (10 734821)
          (11 734850))
    (2013 (12 734880) (1 734909) (2 734939) (3 734968) (4 734998) (5 735027)
          (6 735057) (7 735087) (8 735116) (9 735146) (10 735175) (11 735205))
    (2014 (12 735234) (1 735264) (2 735293) (3 735323) (4 735352) (5 735382)
          (6 735411) (7 735441) (8 735470) (9 735500) (9.5 735530) (10 735559)
          (11 735589))
    (2015 (12 735618) (1 735648) (2 735677) (3 735707) (4 735736) (5 735765)
          (6 735795) (7 735824) (8 735854) (9 735884) (10 735914) (11 735943))
    (2016 (12 735973) (1 736002) (2 736032) (3 736061) (4 736091) (5 736120)
          (6 736149) (7 736179) (8 736208) (9 736238) (10 736268) (11 736297))
    (2017 (12 736327) (1 736357) (2 736386) (3 736416) (4 736445) (5 736475)
          (6 736504) (6.5 736533) (7 736563) (8 736592) (9 736622) (10 736651)
          (11 736681))
    (2018 (12 736711) (1 736741) (2 736770) (3 736800) (4 736829) (5 736859)
          (6 736888) (7 736917) (8 736947) (9 736976) (10 737006) (11 737035))
    (2019 (12 737065) (1 737095) (2 737125) (3 737154) (4 737184) (5 737213)
          (6 737243) (7 737272) (8 737301) (9 737331) (10 737360) (11 737389))
    (2020 (12 737419) (1 737449) (2 737478) (3 737508) (4 737538) (4.5 737568)
          (5 737597) (6 737627) (7 737656) (8 737685) (9 737715) (10 737744)
          (11 737774)))
  "Alist of Chinese year structures as determined by `chinese-year'.
The default can be nil, but some values are precomputed for efficiency.")

(defun calendar-chinese-year (y)
  "The structure of the Chinese year for Gregorian year Y.
The result is a list of pairs (i d), where month i begins on absolute date d,
of the Chinese months from the Chinese month following the solstice in
Gregorian year Y-1 to the Chinese month of the solstice of Gregorian year Y.
The list is cached in `calendar-chinese-year-cache' for further use."
  (let ((list (cdr (assoc y calendar-chinese-year-cache))))
    (or list
        (setq list (calendar-chinese-compute-year y)
              calendar-chinese-year-cache (append calendar-chinese-year-cache
                                         (list (cons y list)))))
    list))

;; Maintainer use.
(defun calendar-chinese-year-cache-init (year)
  "Insert an initialization value for `calendar-chinese-year-cache' after point.
Computes values for 10 years either side of YEAR."
  (setq year (- year 10))
  (let (calendar-chinese-year-cache end)
    (save-excursion
      (insert "'(")
      (dotimes (n 21)
        (princ (cons year (calendar-chinese-compute-year year))
               (current-buffer))
        (insert (if (= n 20) ")" "\n"))
        (setq year (1+ year)))
      (setq end (point)))
    (save-excursion
      ;; fill-column -/+ 5.
      (while (and (< (point) end)
                  (re-search-forward "^.\\{65,75\\})" end t))
        (delete-char 1)
        (insert "\n")))
    (indent-region (point) end)))

(defun calendar-chinese-to-absolute (date)
  "The number of days elapsed between the Gregorian date 12/31/1 BC and DATE.
DATE is a Chinese date (cycle year month day).  The Gregorian date
Sunday, December 31, 1 BC is imaginary."
  (let* ((cycle (car date))
         (year (cadr date))
         (month (nth 2 date))
         (day (nth 3 date))
         (g-year (+ (* (1- cycle) 60)  ; years in prior cycles
                    (1- year)          ; prior years this cycle
                    -2636)))           ; years before absolute date 0
    (+ (1- day)                        ; prior days this month
       (cadr                    ; absolute date of start of this month
        (assoc month (append (memq (assoc 1 (calendar-chinese-year g-year))
                                   (calendar-chinese-year g-year))
                             (calendar-chinese-year (1+ g-year))))))))

(define-obsolete-function-alias 'calendar-absolute-from-chinese
  'calendar-chinese-to-absolute "23.1")

(defun calendar-chinese-from-absolute (date)
  "Compute Chinese date (cycle year month day) corresponding to absolute DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let* ((g-year (calendar-extract-year
                  (calendar-gregorian-from-absolute date)))
         (c-year (+ g-year 2695))
         (list (append (calendar-chinese-year (1- g-year))
                       (calendar-chinese-year g-year)
                       (calendar-chinese-year (1+ g-year)))))
    (while (<= (cadr (cadr list)) date)
      ;; The first month on the list is in Chinese year c-year.
      ;; Date is on or after start of second month on list...
      (if (= 1 (caar (cdr list)))
          ;; Second month on list is a new Chinese year...
          (setq c-year (1+ c-year)))
      ;; ...so first month on list is of no interest.
      (setq list (cdr list)))
    (list (/ (1- c-year) 60)
          ;; Remainder of c-year/60 with 60 instead of 0.
          (1+ (mod (1- c-year) 60))
          (caar list)
          (1+ (- date (cadr (car list)))))))

;; Bound in calendar-generate.
(defvar displayed-month)
(defvar displayed-year)

;;;###holiday-autoload
(defun holiday-chinese-new-year ()
  "Date of Chinese New Year, if visible in calendar.
Returns (((MONTH DAY YEAR) TEXT)), where the date is Gregorian."
  (let ((m displayed-month)
        (y displayed-year)
        chinese-new-year)
    ;; In the Gregorian calendar, CNY falls between Jan 21 and Feb 20.
    ;; Jan is visible if displayed-month = 12, 1, 2; Feb if d-m = 1, 2, 3.
    ;; If we shift the calendar forward one month, we can do a
    ;; one-sided test, namely: d-m <= 4 means CNY might be visible.
    (calendar-increment-month m y 1)    ; shift forward a month
    (and (< m 5)
         (calendar-date-is-visible-p
          (setq chinese-new-year
                (calendar-gregorian-from-absolute
                 (cadr (assoc 1 (calendar-chinese-year y))))))
         (list
          (list chinese-new-year
                (format "Chinese New Year (%s)"
                        (calendar-chinese-sexagesimal-name (+ y 57))))))))

;;;###holiday-autoload
(defun holiday-chinese-qingming ()
  "Date of Chinese Qingming Festival, if visible in calendar.
Returns (((MONTH DAY YEAR) TEXT)), where the date is Gregorian."
  (when (memq displayed-month '(3 4 5)) ; is April visible?
    (list (list (calendar-gregorian-from-absolute
                 ;; 15 days after Vernal Equinox.
                 (+ 15
                    (calendar-chinese-zodiac-sign-on-or-after
                     (calendar-absolute-from-gregorian
                      (list 3 15 displayed-year)))))
                "Qingming Festival"))))

;;;###holiday-autoload
(defun holiday-chinese-winter-solstice ()
  "Date of Chinese winter solstice, if visible in calendar.
Returns (((MONTH DAY YEAR) TEXT)), where the date is Gregorian."
  (when (memq displayed-month '(11 12 1)) ; is December visible?
    (list (list (calendar-gregorian-from-absolute
                 (calendar-chinese-zodiac-sign-on-or-after
                  (calendar-absolute-from-gregorian
                   (list 12 15 (if (eq displayed-month 1)
                                   (1- displayed-year)
                                 displayed-year)))))
                "Winter Solstice Festival"))))

;;;###holiday-autoload
(defun holiday-chinese (month day string)
  "Holiday on Chinese MONTH, DAY called STRING.
If MONTH, DAY (Chinese) is visible, returns the corresponding
Gregorian date as the list (((month day year) STRING)).
Returns nil if it is not visible in the current calendar window."
  (let ((date
         (calendar-gregorian-from-absolute
          ;; A basic optimization.  Chinese year can only change if
          ;; Jan or Feb are visible.  FIXME can we do more?
          (if (memq displayed-month '(12 1 2 3))
              ;; This is calendar-nongregorian-visible-p adapted for
              ;; the form of chinese dates: (cycle year month day) as
              ;; opposed to (month day year).
              (let* ((m1 displayed-month)
                     (y1 displayed-year)
                     (m2 displayed-month)
                     (y2 displayed-year)
                     ;; Absolute date of first/last dates in calendar window.
                     (start-date (progn
                                   (calendar-increment-month m1 y1 -1)
                                   (calendar-absolute-from-gregorian
                                    (list m1 1 y1))))
                     (end-date (progn
                                 (calendar-increment-month m2 y2 1)
                                 (calendar-absolute-from-gregorian
                                  (list m2 (calendar-last-day-of-month m2 y2)
                                        y2))))
                     ;; Local date of first/last date in calendar window.
                     (local-start (calendar-chinese-from-absolute start-date))
                     (local-end (calendar-chinese-from-absolute end-date))
                     ;; When Chinese New Year is visible on the far
                     ;; right of the calendar, what is the earliest
                     ;; Chinese month in the previous year that might
                     ;; still visible?  This test doesn't have to be precise.
                     (local (if (< month 10) local-end local-start))
                     (cycle (car local))
                     (year (cadr local)))
                (calendar-chinese-to-absolute (list cycle year month day)))
            ;; Simple form for when new years are not visible.
            (+ (cadr (assoc month (calendar-chinese-year displayed-year)))
               (1- day))))))
    (if (calendar-date-is-visible-p date)
        (list (list date string)))))

;;;###cal-autoload
(defun calendar-chinese-date-string (&optional date)
  "String of Chinese date of Gregorian DATE.
Defaults to today's date if DATE is not given."
  (let* ((a-date (calendar-absolute-from-gregorian
                  (or date (calendar-current-date))))
         (c-date (calendar-chinese-from-absolute a-date))
         (cycle (car c-date))
         (year (cadr c-date))
         (month (nth 2 c-date))
         (day (nth 3 c-date))
         (this-month (calendar-chinese-to-absolute
                      (list cycle year month 1)))
         (next-month (calendar-chinese-to-absolute
                      (list (if (= year 60) (1+ cycle) cycle)
                            (if (= (floor month) 12) (1+ year) year)
                            ;; Remainder of (1+(floor month))/12, with
                            ;; 12 instead of 0.
                            (1+ (mod (floor month) 12))
                            1))))
    (format "Cycle %s, year %s (%s), %smonth %s%s, day %s (%s)"
            cycle
            year (calendar-chinese-sexagesimal-name year)
            (if (not (integerp month))
                "second "
              (if (< 30 (- next-month this-month))
                  "first "
                ""))
            (floor month)
            (if (integerp month)
                (format " (%s)" (calendar-chinese-sexagesimal-name
                                 (+ (* 12 year) month 50)))
              "")
            day (calendar-chinese-sexagesimal-name (+ a-date 15)))))

;;;###cal-autoload
(defun calendar-chinese-print-date ()
  "Show the Chinese date equivalents of date."
  (interactive)
  (message "Computing Chinese date...")
  (message "Chinese date: %s"
           (calendar-chinese-date-string (calendar-cursor-to-date t))))

(define-obsolete-function-alias 'calendar-print-chinese-date
  'calendar-chinese-print-date "23.1")

(defun calendar-chinese-months-to-alist (l)
  "Make list of months L into an assoc list."
  (and l (car l)
       (if (and (cdr l) (cadr l))
           (if (= (car l) (floor (cadr l)))
               (append
                (list (cons (format "%s (first)" (car l)) (car l))
                      (cons (format "%s (second)" (car l)) (cadr l)))
                (calendar-chinese-months-to-alist (cddr l)))
             (append
              (list (cons (number-to-string (car l)) (car l)))
              (calendar-chinese-months-to-alist (cdr l))))
         (list (cons (number-to-string (car l)) (car l))))))

(defun calendar-chinese-months (c y)
  "A list of the months in cycle C, year Y of the Chinese calendar."
  (memq 1 (append
           (mapcar (lambda (x)
                     (car x))
                   (calendar-chinese-year (calendar-extract-year
                                           (calendar-gregorian-from-absolute
                                            (calendar-chinese-to-absolute
                                             (list c y 1 1))))))
           (mapcar (lambda (x)
                     (if (> (car x) 11) (car x)))
                   (calendar-chinese-year (calendar-extract-year
                                           (calendar-gregorian-from-absolute
                                            (calendar-chinese-to-absolute
                                             (list (if (= y 60) (1+ c) c)
                                                   (if (= y 60) 1 y)
                                                   1 1)))))))))

;;;###cal-autoload
(defun calendar-chinese-goto-date (date &optional noecho)
  "Move cursor to Chinese date DATE.
Echo Chinese date unless NOECHO is non-nil."
  (interactive
   (let* ((c (calendar-chinese-from-absolute
              (calendar-absolute-from-gregorian (calendar-current-date))))
          (cycle (calendar-read
                  "Chinese calendar cycle number (>44): "
                  (lambda (x) (> x 44))
                  (number-to-string (car c))))
          (year (calendar-read
                 "Year in Chinese cycle (1..60): "
                 (lambda (x) (and (<= 1 x) (<= x 60)))
                 (number-to-string (cadr c))))
          (month-list (calendar-chinese-months-to-alist
                       (calendar-chinese-months cycle year)))
          (month (cdr (assoc
                       (completing-read "Chinese calendar month: "
                                        month-list nil t)
                       month-list)))
          (last (if (= month
                       (nth 2
                            (calendar-chinese-from-absolute
                             (+ 29
                                (calendar-chinese-to-absolute
                                 (list cycle year month 1))))))
                    30
                  29))
          (day (calendar-read
                (format "Chinese calendar day (1-%d): " last)
                (lambda (x) (and (<= 1 x) (<= x last))))))
     (list (list cycle year month day))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-chinese-to-absolute date)))
  (or noecho (calendar-chinese-print-date)))

(define-obsolete-function-alias 'calendar-goto-chinese-date
  'calendar-chinese-goto-date "23.1")

(defvar date)

;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-chinese-date ()
  "Chinese calendar equivalent of date diary entry."
  (format "Chinese date: %s" (calendar-chinese-date-string date)))

(provide 'cal-china)

;;; cal-china.el ends here
