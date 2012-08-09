;;; lunar.el --- calendar functions for phases of the moon

;; Copyright (C) 1992-1993, 1995, 1997, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: moon, lunar phases, calendar, diary
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

;; Based on ``Astronomical Formulae for Calculators,'' 3rd ed., by Jean Meeus,
;; Willmann-Bell, Inc., 1985 and ``Astronomical Algorithms'' by Jean Meeus,
;; Willmann-Bell, Inc., 1991.
;;
;; WARNING: The calculations will be accurate only to within a few minutes.

;; The author would be delighted to have an astronomically more sophisticated
;; person rewrite the code for the lunar calculations in this file!

;;; Code:

(require 'calendar)
(require 'solar)
(require 'cal-dst)
;; calendar-astro-to-absolute and v versa are cal-autoloads.
;;;(require 'cal-julian)

(defcustom lunar-phase-names
  '("New Moon" "First Quarter Moon" "Full Moon" "Last Quarter Moon")
  "List of names for the lunar phases."
  :type '(list
          (string :tag "New Moon")
          (string :tag "First Quarter Moon")
          (string :tag "Full Moon")
          (string :tag "Last Quarter Moon"))
  :group 'calendar
  :version "23.2")

(defun lunar-phase (index)
  "Local date and time of lunar phase INDEX.
Integer below INDEX/4 gives the lunation number, counting from Jan 1, 1900;
remainder mod 4 gives the phase: 0 new moon, 1 first quarter, 2 full moon,
3 last quarter.  Returns a list (DATE TIME PHASE)."
  (let* ((phase (mod index 4))
         (index (/ index 4.0))
         (time (/ index 1236.85))
         (date (+ (calendar-absolute-from-gregorian '(1 0.5 1900))
                  0.75933
                  (* 29.53058868 index) ; FIXME 29.530588853?
                  (* 0.0001178 time time)
                  (* -0.000000155 time time time)
                  (* 0.00033
                     (solar-sin-degrees (+ 166.56
                                           (* 132.87 time)
                                           (* -0.009173 time time))))))
         (sun-anomaly (mod
                       (+ 359.2242
                          (* 29.105356 index)
                          (* -0.0000333 time time)
                          (* -0.00000347 time time time))
                       360.0))
         (moon-anomaly (mod
                        (+ 306.0253
                           (* 385.81691806 index)
                           (* 0.0107306 time time)
                           (* 0.00001236 time time time))
                        360.0))
         (moon-lat (mod
                    (+ 21.2964
                       (* 390.67050646 index)
                       (* -0.0016528 time time)
                       (* -0.00000239 time time time))
                    360.0))
         (adjustment
          (if (memq phase '(0 2))
              (+ (* (- 0.1734 (* 0.000393 time))
                    (solar-sin-degrees sun-anomaly))
                 (* 0.0021 (solar-sin-degrees (* 2 sun-anomaly)))
                 (* -0.4068 (solar-sin-degrees moon-anomaly))
                 (* 0.0161 (solar-sin-degrees (* 2 moon-anomaly)))
                 (* -0.0004 (solar-sin-degrees (* 3 moon-anomaly)))
                 (* 0.0104 (solar-sin-degrees (* 2 moon-lat)))
                 (* -0.0051 (solar-sin-degrees (+ sun-anomaly moon-anomaly)))
                 (* -0.0074 (solar-sin-degrees (- sun-anomaly moon-anomaly)))
                 (* 0.0004 (solar-sin-degrees (+ (* 2 moon-lat) sun-anomaly)))
                 (* -0.0004 (solar-sin-degrees (- (* 2 moon-lat) sun-anomaly)))
                 (* -0.0006 (solar-sin-degrees
                             (+ (* 2 moon-lat) moon-anomaly)))
                 (* 0.0010 (solar-sin-degrees (- (* 2 moon-lat) moon-anomaly)))
                 (* 0.0005 (solar-sin-degrees
                            (+ (* 2 moon-anomaly) sun-anomaly))))
            (+ (* (- 0.1721 (* 0.0004 time))
                  (solar-sin-degrees sun-anomaly))
               (* 0.0021 (solar-sin-degrees (* 2 sun-anomaly)))
               (* -0.6280 (solar-sin-degrees moon-anomaly))
               (* 0.0089 (solar-sin-degrees (* 2 moon-anomaly)))
               (* -0.0004 (solar-sin-degrees (* 3 moon-anomaly)))
               (* 0.0079 (solar-sin-degrees (* 2 moon-lat)))
               (* -0.0119 (solar-sin-degrees (+ sun-anomaly moon-anomaly)))
               (* -0.0047 (solar-sin-degrees (- sun-anomaly moon-anomaly)))
               (* 0.0003 (solar-sin-degrees (+ (* 2 moon-lat) sun-anomaly)))
               (* -0.0004 (solar-sin-degrees (- (* 2 moon-lat) sun-anomaly)))
               (* -0.0006 (solar-sin-degrees (+ (* 2 moon-lat) moon-anomaly)))
               (* 0.0021 (solar-sin-degrees (- (* 2 moon-lat) moon-anomaly)))
               (* 0.0003 (solar-sin-degrees
                          (+ (* 2 moon-anomaly) sun-anomaly)))
               (* 0.0004 (solar-sin-degrees
                          (- sun-anomaly (* 2 moon-anomaly))))
               (* -0.0003 (solar-sin-degrees
                           (+ (* 2 sun-anomaly) moon-anomaly))))))
         (adj (+ 0.0028
                 (* -0.0004 (solar-cosine-degrees
                             sun-anomaly))
                 (* 0.0003 (solar-cosine-degrees
                            moon-anomaly))))
         (adjustment (cond ((= phase 1) (+ adjustment adj))
                           ((= phase 2) (- adjustment adj))
                           (t adjustment)))
         (date (+ date adjustment))
         (date (+ date (/ (- calendar-time-zone
                             (solar-ephemeris-correction
                              (calendar-extract-year
                               (calendar-gregorian-from-absolute
                                (truncate date)))))
                          60.0 24.0)))
         (time (* 24 (- date (truncate date))))
         (date (calendar-gregorian-from-absolute (truncate date)))
         (adj (dst-adjust-time date time)))
    (list (car adj) (apply 'solar-time-string (cdr adj)) phase)))

(defconst lunar-cycles-per-year 12.3685 ; 365.25/29.530588853
  "Mean number of lunar cycles per 365.25 day year.")

;; FIXME new-moon index; use in lunar-phase-list implies always below.
(defun lunar-index (date)
  "Return the lunar index for Gregorian date DATE.
This is 4 times the approximate number of new moons since 1 Jan 1900.
The factor of 4 allows (mod INDEX 4) to represent the four quarters."
  (* 4 (truncate
        (* lunar-cycles-per-year
           ;; Years since 1900, as a real.
           (+ (calendar-extract-year date)
              (/ (calendar-day-number date) 366.0)
              -1900)))))

(defun lunar-phase-list (month year)
  "List of lunar phases for three months starting with Gregorian MONTH, YEAR."
  (let* ((index (lunar-index (list month 1 year)))
         (new-moon (lunar-phase index))
         (end-date (let ((end-month month)
                         (end-year year))
                     (calendar-increment-month end-month end-year 3)
                     (list (list end-month 1 end-year))))
         ;; Alternative for start-date:
;;;         (calendar-gregorian-from-absolute
;;;          (1- (calendar-absolute-from-gregorian (list month 1 year))))
         (start-date (progn
                       (calendar-increment-month month year -1)
                       (list (list month
                                   (calendar-last-day-of-month month year)
                                   year))))
         list)
    (while (calendar-date-compare new-moon end-date)
      (if (calendar-date-compare start-date new-moon)
          (setq list (append list (list new-moon))))
      (setq index (1+ index)
            new-moon (lunar-phase index)))
    list))

(defun lunar-phase-name (phase)
  "Name of lunar PHASE.
0 = new moon, 1 = first quarter, 2 = full moon, 3 = last quarter."
  (nth phase lunar-phase-names))

(defvar displayed-month)                ; from calendar-generate
(defvar displayed-year)

;;;###cal-autoload
(defun calendar-lunar-phases (&optional event)
  "Create a buffer with the lunar phases for the current calendar window.
If EVENT is non-nil, it's an event indicating the buffer position to
use instead of point."
  (interactive (list last-nonmenu-event))
  ;; If called from a menu, with the calendar window not selected.
  (with-current-buffer
      (if event (window-buffer (posn-window (event-start event)))
        (current-buffer))
    (message "Computing phases of the moon...")
    (let ((m1 displayed-month)
          (y1 displayed-year)
          (m2 displayed-month)
          (y2 displayed-year))
      (calendar-increment-month m1 y1 -1)
      (calendar-increment-month m2 y2 1)
      (calendar-in-read-only-buffer lunar-phases-buffer
        (calendar-set-mode-line
         (if (= y1 y2)
             (format "Phases of the Moon from %s to %s, %d%%-"
                     (calendar-month-name m1) (calendar-month-name m2) y2)
           (format "Phases of the Moon from %s, %d to %s, %d%%-"
                   (calendar-month-name m1) y1 (calendar-month-name m2) y2)))
        (insert
         (mapconcat
          (lambda (x)
            (format "%s: %s %s" (calendar-date-string (car x))
                    (lunar-phase-name (nth 2 x))
                    (cadr x)))
          (lunar-phase-list m1 y1) "\n")))
      (message "Computing phases of the moon...done"))))

;;;###cal-autoload
(define-obsolete-function-alias 'calendar-phases-of-moon
  'calendar-lunar-phases "23.1")

;;;###autoload
(defun lunar-phases (&optional arg)
  "Display the quarters of the moon for last month, this month, and next month.
If called with an optional prefix argument ARG, prompts for month and year.
This function is suitable for execution in a .emacs file."
  (interactive "P")
  (save-excursion
    (let* ((date (if arg (calendar-read-date t)
                   (calendar-current-date)))
           (displayed-month (calendar-extract-month date))
           (displayed-year (calendar-extract-year date)))
      (calendar-lunar-phases))))

;;;###autoload
(define-obsolete-function-alias 'phases-of-moon 'lunar-phases "23.1")

(defvar date)

;; To be called from diary-list-sexp-entries, where DATE is bound.

;;;###diary-autoload
(defun diary-lunar-phases (&optional mark)
  "Moon phases diary entry.
An optional parameter MARK specifies a face or single-character string to
use when highlighting the day in the calendar."
  (let* ((index (lunar-index date))
         (phase (lunar-phase index)))
    (while (calendar-date-compare phase (list date))
      (setq index (1+ index)
            phase (lunar-phase index)))
    (if (calendar-date-equal (car phase) date)
        (cons mark (concat (lunar-phase-name (nth 2 phase)) " "
                           (cadr phase))))))

;;;###diary-autoload
(define-obsolete-function-alias 'diary-phases-of-moon
  'diary-lunar-phases "23.1")

;; For the Chinese calendar the calculations for the new moon need to be more
;; accurate than those above, so we use more terms in the approximation.
(defun lunar-new-moon-time (k)
  "Astronomical (Julian) day number of K th new moon."
  (let* ((T (/ k 1236.85))
         (T2 (* T T))
         (T3 (* T T T))
         (T4 (* T2 T2))
         (JDE (+ 2451550.09765
                 (* 29.530588853 k)
                 (* 0.0001337 T2)
                 (* -0.000000150 T3)
                 (* 0.00000000073 T4)))
         (E (- 1 (* 0.002516 T) (* 0.0000074 T2)))
         (sun-anomaly (+ 2.5534
                         (* 29.10535669 k)
                         (* -0.0000218 T2)
                         (* -0.00000011 T3)))
         (moon-anomaly (+ 201.5643
                          (* 385.81693528 k)
                          (* 0.0107438 T2)
                          (* 0.00001239 T3)
                          (* -0.000000058 T4)))
         (moon-argument (+ 160.7108
                           (* 390.67050274 k)
                           (* -0.0016341 T2)
                           (* -0.00000227 T3)
                           (* 0.000000011 T4)))
         (omega (+ 124.7746
                   (* -1.56375580 k)
                   (* 0.0020691 T2)
                   (* 0.00000215 T3)))
         (A1  (+ 299.77 (*  0.107408 k) (* -0.009173 T2)))
         (A2  (+ 251.88 (*  0.016321 k)))
         (A3  (+ 251.83 (* 26.641886 k)))
         (A4  (+ 349.42 (* 36.412478 k)))
         (A5  (+  84.66 (* 18.206239 k)))
         (A6  (+ 141.74 (* 53.303771 k)))
         (A7  (+ 207.14 (*  2.453732 k)))
         (A8  (+ 154.84 (*  7.306860 k)))
         (A9  (+  34.52 (* 27.261239 k)))
         (A10 (+ 207.19 (*  0.121824 k)))
         (A11 (+ 291.34 (*  1.844379 k)))
         (A12 (+ 161.72 (* 24.198154 k)))
         (A13 (+ 239.56 (* 25.513099 k)))
         (A14 (+ 331.55 (*  3.592518 k)))
         (correction
          (+ (* -0.40720   (solar-sin-degrees moon-anomaly))
             (*  0.17241 E (solar-sin-degrees sun-anomaly))
             (*  0.01608   (solar-sin-degrees (* 2 moon-anomaly)))
             (*  0.01039   (solar-sin-degrees (* 2 moon-argument)))
             (*  0.00739 E (solar-sin-degrees (- moon-anomaly sun-anomaly)))
             (* -0.00514 E (solar-sin-degrees (+ moon-anomaly sun-anomaly)))
             (*  0.00208 E E (solar-sin-degrees (* 2 sun-anomaly)))
             (* -0.00111   (solar-sin-degrees
                            (- moon-anomaly (* 2 moon-argument))))
             (* -0.00057   (solar-sin-degrees
                            (+ moon-anomaly (* 2 moon-argument))))
             (*  0.00056 E (solar-sin-degrees
                            (+ (* 2 moon-anomaly) sun-anomaly)))
             (* -0.00042   (solar-sin-degrees (* 3 moon-anomaly)))
             (*  0.00042 E (solar-sin-degrees
                            (+ sun-anomaly (* 2 moon-argument))))
             (*  0.00038 E (solar-sin-degrees
                            (- sun-anomaly (* 2 moon-argument))))
             (* -0.00024 E (solar-sin-degrees
                            (- (* 2 moon-anomaly) sun-anomaly)))
             (* -0.00017   (solar-sin-degrees omega))
             (* -0.00007   (solar-sin-degrees
                            (+ moon-anomaly (* 2 sun-anomaly))))
             (*  0.00004   (solar-sin-degrees
                            (- (* 2 moon-anomaly) (* 2 moon-argument))))
             (*  0.00004   (solar-sin-degrees (* 3 sun-anomaly)))
             (*  0.00003   (solar-sin-degrees (+ moon-anomaly sun-anomaly
                                                 (* -2 moon-argument))))
             (*  0.00003   (solar-sin-degrees
                            (+ (* 2 moon-anomaly) (* 2 moon-argument))))
             (* -0.00003   (solar-sin-degrees (+ moon-anomaly sun-anomaly
                                                 (* 2 moon-argument))))
             (*  0.00003   (solar-sin-degrees (- moon-anomaly sun-anomaly
                                                 (* -2 moon-argument))))
             (* -0.00002   (solar-sin-degrees (- moon-anomaly sun-anomaly
                                                 (* 2 moon-argument))))
             (* -0.00002   (solar-sin-degrees
                            (+ (* 3 moon-anomaly) sun-anomaly)))
             (*  0.00002   (solar-sin-degrees (* 4 moon-anomaly)))))
         (additional
          (+ (* 0.000325 (solar-sin-degrees A1))
             (* 0.000165 (solar-sin-degrees A2))
             (* 0.000164 (solar-sin-degrees A3))
             (* 0.000126 (solar-sin-degrees A4))
             (* 0.000110 (solar-sin-degrees A5))
             (* 0.000062 (solar-sin-degrees A6))
             (* 0.000060 (solar-sin-degrees A7))
             (* 0.000056 (solar-sin-degrees A8))
             (* 0.000047 (solar-sin-degrees A9))
             (* 0.000042 (solar-sin-degrees A10))
             (* 0.000040 (solar-sin-degrees A11))
             (* 0.000037 (solar-sin-degrees A12))
             (* 0.000035 (solar-sin-degrees A13))
             (* 0.000023 (solar-sin-degrees A14))))
         (newJDE (+ JDE correction additional)))
    (+ newJDE
       (- (solar-ephemeris-correction
           (calendar-extract-year
            (calendar-gregorian-from-absolute
             (floor (calendar-astro-to-absolute newJDE))))))
       (/ calendar-time-zone 60.0 24.0))))

(defun lunar-new-moon-on-or-after (d)
  "Julian day number of first new moon on or after Julian day number D.
The fractional part is the time of day.

The date and time are local time, including any daylight saving rules,
as governed by the values of `calendar-daylight-savings-starts',
`calendar-daylight-savings-starts-time', `calendar-daylight-savings-ends',
`calendar-daylight-savings-ends-time', `calendar-daylight-time-offset', and
`calendar-time-zone'."
  (let* ((date (calendar-gregorian-from-absolute
                (floor (calendar-astro-to-absolute d))))
         (year (+ (calendar-extract-year date)
                  (/ (calendar-day-number date) 365.25)))
         (k (floor (* (- year 2000.0) lunar-cycles-per-year)))
         (date (lunar-new-moon-time k))
         (a-date (progn
                   (while (< date d)
                     (setq k (1+ k)
                           date (lunar-new-moon-time k)))
                   (calendar-astro-to-absolute date)))
         (time (* 24 (- a-date (truncate a-date))))
         (date (calendar-gregorian-from-absolute (truncate a-date)))
         (adj (dst-adjust-time date time)))
    (calendar-astro-from-absolute
     (+ (calendar-absolute-from-gregorian (car adj))
        (/ (cadr adj) 24.0)))))

(provide 'lunar)

;;; lunar.el ends here
