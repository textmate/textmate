;;; holidays.el --- holiday functions for the calendar package

;; Copyright (C) 1989-1990, 1992-1994, 1997, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: holidays, calendar
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

;;; Code:

(require 'calendar)
(load "hol-loaddefs" nil t)

(defgroup holidays nil
  "Holidays support in calendar."
  :group 'calendar
  :prefix "holidays-"
  :group 'local)

;; The various holiday variables are autoloaded because people
;; are used to using them to set calendar-holidays without having to
;; explicitly load this file.

;;;###autoload
(define-obsolete-variable-alias 'general-holidays
  'holiday-general-holidays "23.1")
;;;###autoload
(defcustom holiday-general-holidays
  (mapcar 'purecopy
  '((holiday-fixed 1 1 "New Year's Day")
    (holiday-float 1 1 3 "Martin Luther King Day")
    (holiday-fixed 2 2 "Groundhog Day")
    (holiday-fixed 2 14 "Valentine's Day")
    (holiday-float 2 1 3 "President's Day")
    (holiday-fixed 3 17 "St. Patrick's Day")
    (holiday-fixed 4 1 "April Fools' Day")
    (holiday-float 5 0 2 "Mother's Day")
    (holiday-float 5 1 -1 "Memorial Day")
    (holiday-fixed 6 14 "Flag Day")
    (holiday-float 6 0 3 "Father's Day")
    (holiday-fixed 7 4 "Independence Day")
    (holiday-float 9 1 1 "Labor Day")
    (holiday-float 10 1 2 "Columbus Day")
    (holiday-fixed 10 31 "Halloween")
    (holiday-fixed 11 11 "Veteran's Day")
    (holiday-float 11 4 4 "Thanksgiving")))
  "General holidays.  Default value is for the United States.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'holiday-general-holidays 'risky-local-variable t)

;;;###autoload
(define-obsolete-variable-alias 'oriental-holidays
  'holiday-oriental-holidays "23.1")
;;;###autoload
(defcustom holiday-oriental-holidays
  (mapcar 'purecopy
  '((holiday-chinese-new-year)
    (if calendar-chinese-all-holidays-flag
        (append
         (holiday-chinese 1 15 "Lantern Festival")
         (holiday-chinese-qingming)
         (holiday-chinese 5  5 "Dragon Boat Festival")
         (holiday-chinese 7  7 "Double Seventh Festival")
         (holiday-chinese 8 15 "Mid-Autumn Festival")
         (holiday-chinese 9  9 "Double Ninth Festival")
         (holiday-chinese-winter-solstice)
         ))))
  "Oriental holidays.
See the documentation for `calendar-holidays' for details."
  :version "23.1"                       ; added more holidays
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'holiday-oriental-holidays 'risky-local-variable t)

;;;###autoload
(define-obsolete-variable-alias 'local-holidays 'holiday-local-holidays "23.1")
;;;###autoload
(defcustom holiday-local-holidays nil
  "Local holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'holiday-local-holidays 'risky-local-variable t)

;;;###autoload
(define-obsolete-variable-alias 'other-holidays 'holiday-other-holidays "23.1")
;;;###autoload
(defcustom holiday-other-holidays nil
  "User defined holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'holiday-other-holidays 'risky-local-variable t)

;;;###autoload
(defvar hebrew-holidays-1
  (mapcar 'purecopy
  '((holiday-hebrew-rosh-hashanah)
    (if calendar-hebrew-all-holidays-flag
        (holiday-julian
         11
         (let ((m displayed-month)
               (y displayed-year)
               year)
           (calendar-increment-month m y -1)
           (setq year (calendar-extract-year
                       (calendar-julian-from-absolute
                        (calendar-absolute-from-gregorian (list m 1 y)))))
           (if (zerop (% (1+ year) 4))
               22
             21)) "\"Tal Umatar\" (evening)"))))
  "Component of the old default value of `holiday-hebrew-holidays'.")
;;;###autoload
(put 'hebrew-holidays-1 'risky-local-variable t)
(make-obsolete-variable 'hebrew-holidays-1 'hebrew-holidays "23.1")

;;;###autoload
(defvar hebrew-holidays-2
  (mapcar 'purecopy
  '((holiday-hebrew-hanukkah) ; respects calendar-hebrew-all-holidays-flag
    (if calendar-hebrew-all-holidays-flag
      (holiday-hebrew
       10
       (let ((h-year (calendar-extract-year
                      (calendar-hebrew-from-absolute
                       (calendar-absolute-from-gregorian
                        (list displayed-month 28 displayed-year))))))
         (if (= 6 (% (calendar-hebrew-to-absolute (list 10 10 h-year))
                     7))
             11 10))
       "Tzom Teveth"))
    (if calendar-hebrew-all-holidays-flag
        (holiday-hebrew 11 15 "Tu B'Shevat"))))
  "Component of the old default value of `holiday-hebrew-holidays'.")
;;;###autoload
(put 'hebrew-holidays-2 'risky-local-variable t)
(make-obsolete-variable 'hebrew-holidays-2 'hebrew-holidays "23.1")

;;;###autoload
(defvar hebrew-holidays-3
  (mapcar 'purecopy
  '((if calendar-hebrew-all-holidays-flag
        (holiday-hebrew
         11
         (let* ((m displayed-month)
                (y displayed-year)
                (h-year (progn
                          (calendar-increment-month m y 1)
                          (calendar-extract-year
                           (calendar-hebrew-from-absolute
                            (calendar-absolute-from-gregorian
                             (list m (calendar-last-day-of-month m y) y))))))
                (s-s
                 (calendar-hebrew-from-absolute
                  (if (= 6
                         (% (calendar-hebrew-to-absolute
                             (list 7 1 h-year))
                            7))
                      (calendar-dayname-on-or-before
                       6 (calendar-hebrew-to-absolute
                          (list 11 17 h-year)))
                    (calendar-dayname-on-or-before
                     6 (calendar-hebrew-to-absolute
                        (list 11 16 h-year))))))
                (day (calendar-extract-day s-s)))
           day)
         "Shabbat Shirah"))))
  "Component of the old default value of `holiday-hebrew-holidays'.")
;;;###autoload
(put 'hebrew-holidays-3 'risky-local-variable t)
(make-obsolete-variable 'hebrew-holidays-3 'hebrew-holidays "23.1")

;;;###autoload
(defvar hebrew-holidays-4
  (mapcar 'purecopy
  '((holiday-hebrew-passover)
    (and calendar-hebrew-all-holidays-flag
         (let* ((m displayed-month)
                (y displayed-year)
                (year (progn
                        (calendar-increment-month m y -1)
                        (calendar-extract-year
                         (calendar-julian-from-absolute
                          (calendar-absolute-from-gregorian (list m 1 y)))))))
           (= 21 (% year 28)))
         (holiday-julian 3 26 "Kiddush HaHamah"))
    (if calendar-hebrew-all-holidays-flag
        (holiday-hebrew-tisha-b-av))))
    "Component of the old default value of `holiday-hebrew-holidays'.")
;;;###autoload
(put 'hebrew-holidays-4 'risky-local-variable t)
(make-obsolete-variable 'hebrew-holidays-4 'hebrew-holidays "23.1")

;;;###autoload
(define-obsolete-variable-alias 'hebrew-holidays
  'holiday-hebrew-holidays "23.1")
;;;###autoload
(defcustom holiday-hebrew-holidays
  (mapcar 'purecopy
  '((holiday-hebrew-passover)
    (holiday-hebrew-rosh-hashanah)
    (holiday-hebrew-hanukkah)
    (if calendar-hebrew-all-holidays-flag
        (append
         (holiday-hebrew-tisha-b-av)
         (holiday-hebrew-misc)))))
  "Jewish holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :version "23.1"            ; removed dependency on hebrew-holidays-N
  :group 'holidays)
;;;###autoload
(put 'holiday-hebrew-holidays 'risky-local-variable t)

;;;###autoload
(define-obsolete-variable-alias 'christian-holidays
  'holiday-christian-holidays "23.1")
;;;###autoload
(defcustom holiday-christian-holidays
  (mapcar 'purecopy
  '((holiday-easter-etc)    ; respects calendar-christian-all-holidays-flag
    (holiday-fixed 12 25 "Christmas")
    (if calendar-christian-all-holidays-flag
        (append
         (holiday-fixed 1 6 "Epiphany")
         (holiday-julian 12 25 "Eastern Orthodox Christmas")
         (holiday-greek-orthodox-easter)
         (holiday-fixed 8 15 "Assumption")
         (holiday-advent 0 "Advent")))))
  "Christian holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'holiday-christian-holidays 'risky-local-variable t)

;;;###autoload
(define-obsolete-variable-alias 'islamic-holidays
  'holiday-islamic-holidays "23.1")
;;;###autoload
(defcustom holiday-islamic-holidays
  (mapcar 'purecopy
  '((holiday-islamic-new-year)
    (holiday-islamic 9 1 "Ramadan Begins")
    (if calendar-islamic-all-holidays-flag
        (append
         (holiday-islamic 1 10 "Ashura")
         (holiday-islamic 3 12 "Mulad-al-Nabi")
         (holiday-islamic 7 26 "Shab-e-Mi'raj")
         (holiday-islamic 8 15 "Shab-e-Bara't")
         (holiday-islamic 9 27 "Shab-e Qadr")
         (holiday-islamic 10 1 "Id-al-Fitr")
         (holiday-islamic 12 10 "Id-al-Adha")))))
  "Islamic holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'holiday-islamic-holidays 'risky-local-variable t)

;;;###autoload
(define-obsolete-variable-alias 'bahai-holidays 'holiday-bahai-holidays "23.1")
;;;###autoload
(defcustom holiday-bahai-holidays
  (mapcar 'purecopy
  '((holiday-bahai-new-year)
    (holiday-bahai-ridvan)      ; respects calendar-bahai-all-holidays-flag
    (holiday-fixed  5 23 "Declaration of the Báb")
    (holiday-fixed  5 29 "Ascension of Bahá'u'lláh")
    (holiday-fixed  7  9 "Martyrdom of the Báb")
    (holiday-fixed 10 20 "Birth of the Báb")
    (holiday-fixed 11 12 "Birth of Bahá'u'lláh")
    (if calendar-bahai-all-holidays-flag
        (append
         (holiday-fixed 11 26 "Day of the Covenant")
         (holiday-fixed 11 28 "Ascension of `Abdu'l-Bahá")))))
  "Bahá'í holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'holiday-bahai-holidays 'risky-local-variable t)

;;;###autoload
(define-obsolete-variable-alias 'solar-holidays 'holiday-solar-holidays "23.1")
;;;###autoload
(defcustom holiday-solar-holidays
  (mapcar 'purecopy
  '((solar-equinoxes-solstices)
    (holiday-sexp calendar-daylight-savings-starts
                  (format "Daylight Saving Time Begins %s"
                          (solar-time-string
                           (/ calendar-daylight-savings-starts-time (float 60))
                           calendar-standard-time-zone-name)))
    (holiday-sexp calendar-daylight-savings-ends
                  (format "Daylight Saving Time Ends %s"
                          (solar-time-string
                           (/ calendar-daylight-savings-ends-time (float 60))
                           calendar-daylight-time-zone-name)))))
  "Sun-related holidays.
See the documentation for `calendar-holidays' for details."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'holiday-solar-holidays 'risky-local-variable t)

;; This one should not be autoloaded, else .emacs changes of
;; holiday-general-holidays etc have no effect.
;; FIXME should have some :set-after.
(defcustom calendar-holidays
  (append holiday-general-holidays holiday-local-holidays
          holiday-other-holidays holiday-christian-holidays
          holiday-hebrew-holidays holiday-islamic-holidays
          holiday-bahai-holidays holiday-oriental-holidays
          holiday-solar-holidays)
  "List of notable days for the command \\[holidays].

Additional holidays are easy to add to the list, just put them in the
list `holiday-other-holidays' in your .emacs file.  Similarly, by setting
any of `holiday-general-holidays', `holiday-local-holidays',
`holiday-christian-holidays', `holiday-hebrew-holidays',
`holiday-islamic-holidays', `holiday-bahai-holidays',
`holiday-oriental-holidays', or `holiday-solar-holidays' to nil in your
.emacs file, you can eliminate unwanted categories of holidays.

The aforementioned variables control the holiday choices offered
by the function `holiday-list' when it is called interactively.

They also initialize the default value of `calendar-holidays',
which is the default list of holidays used by the function
`holiday-list' in the non-interactive case.  Note that these
variables have no effect on `calendar-holidays' after it has been
set (e.g. after the calendar is loaded).  In that case, customize
`calendar-holidays' directly.

The intention is that (in the US) `holiday-local-holidays' be set in
site-init.el and `holiday-other-holidays' be set by the user.

Entries on the list are expressions that return (possibly empty) lists of
items of the form ((month day year) string) of a holiday in the
three-month period centered around `displayed-month' of `displayed-year'.
Several basic functions are provided for this purpose:

    (holiday-fixed MONTH DAY STRING) is a fixed date on the Gregorian calendar
    (holiday-float MONTH DAYNAME K STRING &optional DAY) is the Kth DAYNAME
                               (0 for Sunday, etc.) after/before Gregorian
                               MONTH DAY.  K<0 means count back from the end
                               of the month.  Optional DAY defaults to 1 if
                               K>0, and MONTH's last day otherwise.
    (holiday-hebrew MONTH DAY STRING)  a fixed date on the Hebrew calendar
    (holiday-islamic MONTH DAY STRING) a fixed date on the Islamic calendar
    (holiday-bahai MONTH DAY STRING)   a fixed date on the Bahá'í calendar
    (holiday-julian MONTH DAY STRING)  a fixed date on the Julian calendar
    (holiday-sexp SEXP STRING) SEXP is a Gregorian-date-valued expression
                               in the variable `year'; if it evaluates to
                               a visible date, that's the holiday; if it
                               evaluates to nil, there's no holiday.  STRING
                               is an expression in the variable `date'.

For example, to add Bastille Day, celebrated in France on July 14, add

     (holiday-fixed 7 14 \"Bastille Day\")

to the list.  To add Hurricane Supplication Day, celebrated in the Virgin
Islands on the fourth Monday in August, add

     (holiday-float 8 1 4 \"Hurricane Supplication Day\")

to the list (the last Monday would be specified with `-1' instead of `4').
To add the last day of Hanukkah to the list, use

     (holiday-hebrew 10 2 \"Last day of Hanukkah\")

since the Hebrew months are numbered with 1 starting from Nisan.
To add the Islamic feast celebrating Mohammed's birthday, use

     (holiday-islamic 3 12 \"Mohammed's Birthday\")

since the Islamic months are numbered from 1 starting with Muharram.
To add an entry for the Bahá'í festival of Ridvan, use

     (holiday-bahai 2 13 \"Festival of Ridvan\")

since the Bahá'í months are numbered from 1 starting with Bahá.
To add Thomas Jefferson's birthday, April 2, 1743 (Julian), use

     (holiday-julian 4 2 \"Jefferson's Birthday\")

To include a holiday conditionally, use the sexp form or a conditional.  For
example, to include American presidential elections, which occur on the first
Tuesday after the first Monday in November of years divisible by 4, add

     (holiday-sexp
       '(if (zerop (% year 4))
           (calendar-gregorian-from-absolute
             (1+ (calendar-dayname-on-or-before
                   1 (+ 6 (calendar-absolute-from-gregorian
                            (list 11 1 year)))))))
       \"US Presidential Election\")

or

     (if (zerop (% displayed-year 4))
         (holiday-fixed 11
                (calendar-extract-day
                 (calendar-gregorian-from-absolute
                  (1+ (calendar-dayname-on-or-before
                       1 (+ 6 (calendar-absolute-from-gregorian
                               (list 11 1 displayed-year)))))))
                \"US Presidential Election\"))

to the list.  To include the phases of the moon, add

     (lunar-phases)

to the holiday list, where `lunar-phases' is an Emacs-Lisp function that
you've written to return a (possibly empty) list of the relevant VISIBLE dates
with descriptive strings such as

     (((2 6 1989) \"New Moon\") ((2 12 1989) \"First Quarter Moon\") ... )."
  :type 'sexp
  :group 'holidays)
;;;###autoload
(put 'calendar-holidays 'risky-local-variable t)

;;; End of user options.


;; FIXME name that makes sense
;;;###diary-autoload
(defun calendar-holiday-list ()
  "Form the list of holidays that occur on dates in the calendar window.
The holidays are those in the list `calendar-holidays'."
  (let (res h)
    (sort
     (dolist (p calendar-holidays res)
       (if (setq h (if calendar-debug-sexp
                       (let ((debug-on-error t))
                         (eval p))
                     (condition-case nil
                         (eval p)
                       (error (beep)
                              (message "Bad holiday list item: %s" p)
                              (sleep-for 2)))))
           (setq res (append h res))))
     'calendar-date-compare)))

(defvar displayed-month)                ; from calendar-generate
(defvar displayed-year)

;; FIXME name that makes sense
;;;###cal-autoload
(defun calendar-list-holidays (&optional event)
  "Create a buffer containing the holidays for the current calendar window.
The holidays are those in the list `calendar-notable-days'.
Returns non-nil if any holidays are found.
If EVENT is non-nil, it's an event indicating the buffer position to
use instead of point."
  (interactive (list last-nonmenu-event))
  ;; If called from a menu, with the calendar window not selected.
  (with-current-buffer
      (if event (window-buffer (posn-window (event-start event)))
        (current-buffer))
    (message "Looking up holidays...")
    (let ((holiday-list (calendar-holiday-list))
          (m1 displayed-month)
          (y1 displayed-year)
          (m2 displayed-month)
          (y2 displayed-year))
      (if (not holiday-list)
          (message "Looking up holidays...none found")
        (calendar-in-read-only-buffer holiday-buffer
          (calendar-increment-month m1 y1 -1)
          (calendar-increment-month m2 y2 1)
          (calendar-set-mode-line
           (if (= y1 y2)
               (format "Notable Dates from %s to %s, %d%%-"
                       (calendar-month-name m1) (calendar-month-name m2) y2)
             (format "Notable Dates from %s, %d to %s, %d%%-"
                     (calendar-month-name m1) y1 (calendar-month-name m2) y2)))
          (insert
           (mapconcat
            (lambda (x) (concat (calendar-date-string (car x))
                                ": " (cadr x)))
            holiday-list "\n")))
        (message "Looking up holidays...done"))
      holiday-list)))

(define-obsolete-function-alias
  'list-calendar-holidays 'calendar-list-holidays "23.1")

;;;###autoload
(defun holidays (&optional arg)
  "Display the holidays for last month, this month, and next month.
If called with an optional prefix argument ARG, prompts for month and year.
This function is suitable for execution in a .emacs file."
  (interactive "P")
  (save-excursion
    (let* ((completion-ignore-case t)
           (date (if arg (calendar-read-date t)
                   (calendar-current-date)))
           (displayed-month (calendar-extract-month date))
           (displayed-year (calendar-extract-year date)))
      (calendar-list-holidays))))

;; rms: "Emacs commands to display a list of something generally start
;; with `list-'.  Please make `list-holidays' the principal name."
;;;###autoload
(defun list-holidays (y1 &optional y2 l label)
  "Display holidays for years Y1 to Y2 (inclusive).
Y2 defaults to Y1.  The optional list of holidays L defaults to
`calendar-holidays'.  If you want to control what holidays are
displayed, use a different list.  For example,

  (list-holidays 2006 2006
    (append holiday-general-holidays holiday-local-holidays))

will display holidays for the year 2006 defined in the two
mentioned lists, and nothing else.

When called interactively, this command offers a choice of
holidays, based on the variables `holiday-solar-holidays' etc.  See the
documentation of `calendar-holidays' for a list of the variables
that control the choices, as well as a description of the format
of a holiday list.

The optional LABEL is used to label the buffer created."
  (interactive
   (let* ((start-year (calendar-read
                       "Starting year of holidays (>0): "
                       (lambda (x) (> x 0))
                       (number-to-string (calendar-extract-year
                                       (calendar-current-date)))))
          (end-year (calendar-read
                     (format "Ending year (inclusive) of holidays (>=%s): "
                             start-year)
                     (lambda (x) (>= x start-year))
                     (number-to-string start-year)))
          (completion-ignore-case t)
          (lists
           (list
            (cons "All" calendar-holidays)
            (cons "Equinoxes/Solstices"
                  (list (list 'solar-equinoxes-solstices)))
            (if holiday-general-holidays
                (cons "General" holiday-general-holidays))
            (if holiday-local-holidays
                (cons "Local" holiday-local-holidays))
            (if holiday-other-holidays
                (cons "Other" holiday-other-holidays))
            (if holiday-christian-holidays
                (cons "Christian" holiday-christian-holidays))
            (if holiday-hebrew-holidays
                (cons "Hebrew" holiday-hebrew-holidays))
            (if holiday-islamic-holidays
                (cons "Islamic" holiday-islamic-holidays))
            (if holiday-bahai-holidays
                (cons "Bahá'í" holiday-bahai-holidays))
            (if holiday-oriental-holidays
                (cons "Oriental" holiday-oriental-holidays))
            (if holiday-solar-holidays
                (cons "Solar" holiday-solar-holidays))
            (cons "Ask" nil)))
          (choice (capitalize
                   (completing-read "List (TAB for choices): " lists nil t)))
          (which (if (string-equal choice "Ask")
                     (eval (read-variable "Enter list name: "))
                   (cdr (assoc choice lists))))
          (name (if (string-equal choice "Equinoxes/Solstices")
                    choice
                  (if (member choice '("Ask" ""))
                      "Holidays"
                    (format "%s Holidays" choice)))))
     (list start-year end-year which name)))
  (unless y2 (setq y2 y1))
  (message "Computing holidays...")
  (let ((calendar-holidays (or l calendar-holidays))
        (title (or label "Holidays"))
        (s (calendar-absolute-from-gregorian (list 2 1 y1)))
        (e (calendar-absolute-from-gregorian (list 11 1 y2)))
        (displayed-month 2)
        (displayed-year y1)
        holiday-list)
    (while (<= s e)
      (setq holiday-list (append holiday-list (calendar-holiday-list)))
      (calendar-increment-month displayed-month displayed-year 3)
      (setq s (calendar-absolute-from-gregorian
               (list displayed-month 1 displayed-year))))
    (save-excursion
      (calendar-in-read-only-buffer holiday-buffer
        (calendar-set-mode-line
         (if (= y1 y2)
             (format "%s for %s" title y1)
           (format "%s for %s-%s" title y1 y2)))
        (insert
         (mapconcat
          (lambda (x) (concat (calendar-date-string (car x))
                              ": " (cadr x)))
          holiday-list "\n")))
      (message "Computing holidays...done"))))

;;;###autoload
(defalias 'holiday-list 'list-holidays)

;;;###diary-autoload
(defun calendar-check-holidays (date)
  "Check the list of holidays for any that occur on DATE.
DATE is a list (month day year).  This function considers the
holidays from the list `calendar-holidays', and returns a list of
strings describing those holidays that apply on DATE, or nil if none do."
  (let ((displayed-month (calendar-extract-month date))
        (displayed-year (calendar-extract-year date))
        holiday-list)
    (dolist (h (calendar-holiday-list) holiday-list)
      (if (calendar-date-equal date (car h))
          (setq holiday-list (append holiday-list (cdr h)))))))

(define-obsolete-function-alias
  'check-calendar-holidays 'calendar-check-holidays "23.1")

(declare-function x-popup-menu "menu.c" (position menu))

;;;###cal-autoload
(defun calendar-cursor-holidays (&optional date event)
  "Find holidays for the date specified by the cursor in the calendar window.
Optional DATE is a list (month day year) to use instead of the
cursor position.  EVENT specifies a buffer position to use for a date."
  (interactive (list nil last-nonmenu-event))
  (message "Checking holidays...")
  (or date (setq date (calendar-cursor-to-date t event)))
  (let ((date-string (calendar-date-string date))
        (holiday-list (calendar-check-holidays date))
        selection msg)
    (if (mouse-event-p event)
        (and (setq selection (cal-menu-x-popup-menu event
                                 (format "Holidays for %s" date-string)
                               (if holiday-list
                                   (mapcar 'list holiday-list)
                                 '("None"))))
             (call-interactively selection))
    (if (not holiday-list)
        (message "No holidays known for %s" date-string)
      (if (<= (length (setq msg
                            (format "%s:  %s" date-string
                                    (mapconcat 'identity holiday-list ";  "))))
              (frame-width))
          (message "%s" msg)
        (calendar-in-read-only-buffer holiday-buffer
          (calendar-set-mode-line date-string)
          (insert (mapconcat 'identity holiday-list "\n")))
        (message "Checking holidays...done"))))))

;; FIXME move to calendar?
;;;###cal-autoload
(defun calendar-mark-holidays (&optional event)
  "Mark notable days in the calendar window.
If EVENT is non-nil, it's an event indicating the buffer position to
use instead of point."
  (interactive (list last-nonmenu-event))
  ;; If called from a menu, with the calendar window not selected.
  (with-current-buffer
      (if event (window-buffer (posn-window (event-start event)))
        (current-buffer))
    (setq calendar-mark-holidays-flag t)
    (message "Marking holidays...")
    (dolist (holiday (calendar-holiday-list))
      (calendar-mark-visible-date (car holiday) calendar-holiday-marker))
    (message "Marking holidays...done")))

(define-obsolete-function-alias
  'mark-calendar-holidays 'calendar-mark-holidays "23.1")

;; Below are the functions that calculate the dates of holidays; these
;; are eval'ed in the function calendar-holiday-list.  If you
;; write other such functions, be sure to imitate the style used below.
;; Remember that each function must return a list of items of the form
;; ((month day year) string) of VISIBLE dates in the calendar window.

(defun holiday-fixed (month day string)
  "Holiday on MONTH, DAY (Gregorian) called STRING.
If MONTH, DAY is visible, the value returned is the list (((MONTH DAY year)
STRING)).  Returns nil if it is not visible in the current calendar window."
  ;; This determines whether a given month is visible in the calendar.
  ;; cf calendar-date-is-visible-p (which also checks the year part).
  ;; The day is irrelevant since only full months are displayed.
  ;; Since the calendar displays three months at a time, month N
  ;; is visible if displayed-month = N-1, N, N+1.
  ;; In particular, November is visible if d-m = 10, 11, 12.
  ;; This is useful, because we can do a one-sided test:
  ;; November is visible if d-m > 9. (Similarly, February is visible if
  ;; d-m < 4.)
  ;; To determine if December is visible, we can shift the calendar
  ;; back a month and ask if November is visible; to determine if
  ;; October is visible, we can shift it forward a month and ask if
  ;; November is visible; etc.
  (let ((m displayed-month)
        (y displayed-year))
    (calendar-increment-month m y (- 11 month))
    (if (> m 9)                         ; Is November visible?
        (list (list (list month day y) string)))))

(defun holiday-float (month dayname n string &optional day)
  "Holiday called STRING on the Nth DAYNAME after/before MONTH DAY.
DAYNAME=0 means Sunday, DAYNAME=1 means Monday, and so on.
If N>0, use the Nth DAYNAME after MONTH DAY.
If N<0, use the Nth DAYNAME before MONTH DAY.
DAY defaults to 1 if N>0, and MONTH's last day otherwise.
If the holiday is visible in the calendar window, returns a
list (((month day year) STRING)).  Otherwise returns nil."
  ;; This is messy because the holiday may be visible, while the date
  ;; on which it is based is not.  For example, the first Monday after
  ;; December 30 may be visible when January is not.  For large values
  ;; of |n| the problem is more grotesque.  If we didn't have to worry
  ;; about such cases, we could just use the original version of this
  ;; function:
  ;;  (let ((m displayed-month)
  ;;        (y displayed-year))
  ;;    (calendar-increment-month m y (- 11 month))
  ;;    (if (> m 9); month in year y is visible
  ;;      (list (list (calendar-nth-named-day n dayname month y day) string)))))
  (let* ((m1 displayed-month)
         (y1 displayed-year)
         (m2 displayed-month)
         (y2 displayed-year)
         (d1 (progn             ; first possible base date for holiday
               (calendar-increment-month m1 y1 -1)
               (+ (calendar-nth-named-absday 1 dayname m1 y1)
                  (* -7 n)
                  (if (> n 0) 1 -7))))
         (d2                     ; last possible base date for holiday
          (progn
            (calendar-increment-month m2 y2 1)
            (+ (calendar-nth-named-absday -1 dayname m2 y2)
               (* -7 n)
               (if (> n 0) 7 -1))))
         (y1 (calendar-extract-year (calendar-gregorian-from-absolute d1)))
         (y2 (calendar-extract-year (calendar-gregorian-from-absolute d2)))
         (y                             ; year of base date
          (if (or (= y1 y2) (> month 9))
              y1
            y2))
         (d                             ; day of base date
          (or day (if (> n 0)
                      1
                    (calendar-last-day-of-month month y))))
         (date                        ; base date for holiday
          (calendar-absolute-from-gregorian (list month d y))))
    (and (<= d1 date) (<= date d2)
         (list (list (calendar-nth-named-day n dayname month y d)
                     string)))))

(defun holiday-filter-visible-calendar (hlist)
  "Filter list of holidays HLIST, and return only the visible ones.
HLIST is a list of elements of the form (DATE) TEXT."
  (delq nil (mapcar (lambda (p)
                      (and (car p) (calendar-date-is-visible-p (car p)) p))
                    hlist)))

(define-obsolete-function-alias
  'filter-visible-calendar-holidays 'holiday-filter-visible-calendar "23.1")

(defun holiday-sexp (sexp string)
  "Sexp holiday for dates in the calendar window.
SEXP is an expression in variable `year' that is evaluated to
give `date'.  STRING is an expression in `date' that evaluates to
the holiday description of `date'.  If `date' is visible in the
calendar window, the holiday STRING is on that date.  If date is
nil, or if the date is not visible, there is no holiday."
  (let ((m displayed-month)
        (y displayed-year)
        year date)
    (calendar-increment-month m y -1)
    (holiday-filter-visible-calendar
     (list
      (progn
        (setq year y
              date (eval sexp))
        (list date (if date (eval string))))
      (progn
        (setq year (1+ y)
              date (eval sexp))
        (list date (if date (eval string))))))))


(defun holiday-advent (&optional n string)
  "Date of Nth day after advent (named STRING), if visible in calendar window.
Negative values of N are interpreted as days before advent.
STRING is used purely for display purposes.  The return value has
the form ((MONTH DAY YEAR) STRING), where the date is that of the
Nth day before or after advent.

For backwards compatibility, if this function is called with no
arguments, then it returns the value appropriate for advent itself."
  ;; Backwards compatibility layer.
  (if (not n)
      (holiday-advent 0 "Advent")
    (let* ((year displayed-year)
           (month displayed-month)
           (advent (progn
                     (calendar-increment-month month year -1)
                     (calendar-gregorian-from-absolute
                      (+ n
                         (calendar-dayname-on-or-before
                          0
                          (calendar-absolute-from-gregorian
                           (list 12 3 year))))))))
      (if (calendar-date-is-visible-p advent)
          (list (list advent string))))))

(defun holiday-easter-etc (&optional n string)
  "Date of Nth day after Easter (named STRING), if visible in calendar window.
Negative values of N are interpreted as days before Easter.
STRING is used purely for display purposes.  The return value has
the form ((MONTH DAY YEAR) STRING), where the date is that of the
Nth day before or after Easter.

For backwards compatibility, if this function is called with no
arguments, then it returns a list of \"standard\" Easter-related
holidays (with more entries if `calendar-christian-all-holidays-flag'
is non-nil)."
  ;; Backwards compatibility layer.
  (if (not n)
      (apply 'append
             (mapcar (lambda (e)
                       (apply 'holiday-easter-etc e))
                     ;; The combined list is not in order.
                     (append
                      (if calendar-christian-all-holidays-flag
                          '((-63 "Septuagesima Sunday")
                            (-56 "Sexagesima Sunday")
                            (-49 "Shrove Sunday")
                            (-48 "Shrove Monday")
                            (-47 "Shrove Tuesday")
                            (-14 "Passion Sunday")
                            (-7 "Palm Sunday")
                            (-3 "Maundy Thursday")
                            (35 "Rogation Sunday")
                            (39 "Ascension Day")
                            (49 "Pentecost (Whitsunday)")
                            (50 "Whitmonday")
                            (56 "Trinity Sunday")
                            (60 "Corpus Christi")))
                      '((-46 "Ash Wednesday")
                        (-2 "Good Friday")
                        (0 "Easter Sunday")))))
    (let* ((century (1+ (/ displayed-year 100)))
           (shifted-epact               ; age of moon for April 5...
            (% (+ 14 (* 11 (% displayed-year 19)) ; ...by Nicaean rule
                  (-     ; ...corrected for the Gregorian century rule
                   (/ (* 3 century) 4))
                  (/       ; ...corrected for Metonic cycle inaccuracy
                   (+ 5 (* 8 century)) 25)
                  (* 30 century))       ; keeps value positive
               30))
           (adjusted-epact              ; adjust for 29.5 day month
            (if (or (zerop shifted-epact)
                    (and (= shifted-epact 1) (< 10 (% displayed-year 19))))
                (1+ shifted-epact)
              shifted-epact))
           (paschal-moon ; day after the full moon on or after March 21
            (- (calendar-absolute-from-gregorian (list 4 19 displayed-year))
               adjusted-epact))
           (abs-easter (calendar-dayname-on-or-before 0 (+ paschal-moon 7)))
           (greg (calendar-gregorian-from-absolute (+ abs-easter n))))
      (if (calendar-date-is-visible-p greg)
          (list (list greg string))))))

;; Prior call to calendar-julian-from-absolute will autoload cal-julian.
(declare-function calendar-julian-to-absolute "cal-julian" (date))

(defun holiday-greek-orthodox-easter ()
  "Date of Easter according to the rule of the Council of Nicaea."
  (let* ((m displayed-month)
         (y displayed-year)
         (julian-year (progn
                        (calendar-increment-month m y 1)
                        (calendar-extract-year
                         (calendar-julian-from-absolute
                          (calendar-absolute-from-gregorian
                           (list m (calendar-last-day-of-month m y) y))))))
         (shifted-epact                 ; age of moon for April 5
          (% (+ 14
                (* 11 (% julian-year 19)))
             30))
         (paschal-moon      ; day after full moon on or after March 21
          (- (calendar-julian-to-absolute (list 4 19 julian-year))
             shifted-epact))
         (nicaean-easter           ; Sunday following the Paschal moon
          (calendar-gregorian-from-absolute
           (calendar-dayname-on-or-before 0 (+ paschal-moon 7)))))
    (if (calendar-date-is-visible-p nicaean-easter)
        (list (list nicaean-easter "Pascha (Greek Orthodox Easter)")))))

(provide 'holidays)

;; Local Variables:
;; coding: utf-8
;; End:

;;; holidays.el ends here
