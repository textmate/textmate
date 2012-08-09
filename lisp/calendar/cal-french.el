;;; cal-french.el --- calendar functions for the French Revolutionary calendar

;; Copyright (C) 1988-1989, 1992, 1994-1995, 1997, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: French Revolutionary calendar, calendar, diary
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

(defconst calendar-french-epoch (calendar-absolute-from-gregorian '(9 22 1792))
  "Absolute date of start of French Revolutionary calendar = Sept 22, 1792.")

(defconst calendar-french-month-name-array
  ["Vende'miaire" "Brumaire" "Frimaire" "Nivo^se" "Pluvio^se" "Vento^se"
   "Germinal" "Flore'al" "Prairial" "Messidor" "Thermidor" "Fructidor"]
  "Array of month names in the French calendar.")

(defconst calendar-french-multibyte-month-name-array
  ["Vendémiaire" "Brumaire" "Frimaire" "Nivôse" "Pluviôse" "Ventôse"
   "Germinal" "Floréal" "Prairial" "Messidor" "Thermidor" "Fructidor"]
  "Array of multibyte month names in the French calendar.")

(defconst calendar-french-day-name-array
  ["Primidi" "Duodi" "Tridi" "Quartidi" "Quintidi" "Sextidi" "Septidi"
   "Octidi" "Nonidi" "Decadi"]
  "Array of day names in the French calendar.")

(defconst calendar-french-special-days-array
  ["de la Vertu" "du Ge'nie" "du Travail" "de la Raison" "des Re'compenses"
   "de la Re'volution"]
  "Array of special day names in the French calendar.")

(defconst calendar-french-multibyte-special-days-array
  ["de la Vertu" "du Génie" "du Travail" "de la Raison" "des Récompenses"
   "de la Révolution"]
  "Array of multibyte special day names in the French calendar.")

(defun calendar-french-accents-p ()
  "Return non-nil if diacritical marks are available."
  (and (or window-system
           (terminal-coding-system))
       (or enable-multibyte-characters
           (and (char-table-p standard-display-table)
                (equal (aref standard-display-table 161) [161])))))

(defun calendar-french-month-name-array ()
  "Return the array of month names, depending on whether accents are available."
  (if (calendar-french-accents-p)
      calendar-french-multibyte-month-name-array
    calendar-french-month-name-array))

(defun calendar-french-day-name-array ()
  "Return the array of day names."
  calendar-french-day-name-array)

(defun calendar-french-special-days-array ()
  "Return the special day names, depending on whether accents are available."
  (if (calendar-french-accents-p)
      calendar-french-multibyte-special-days-array
    calendar-french-special-days-array))

(defun calendar-french-leap-year-p (year)
  "True if YEAR is a leap year on the French Revolutionary calendar.
For Gregorian years 1793 to 1805, the years of actual operation of the
calendar, follows historical practice based on equinoxes (years 3, 7,
and 11 were leap years; 15 and 20 would have been leap years).  For later
years uses the proposed rule of Romme (never adopted)--leap years fall every
four years except century years not divisible 400 and century years that are
multiples of 4000."
  (or (memq year '(3 7 11)) ; actual practice--based on equinoxes
      (memq year '(15 20))  ; anticipated practice--based on equinoxes
      (and (> year 20)      ; Romme's proposal--never adopted
           (zerop (% year 4))
           (not (memq (% year 400) '(100 200 300)))
           (not (zerop (% year 4000))))))

(defun calendar-french-last-day-of-month (month year)
  "Return last day of MONTH, YEAR on the French Revolutionary calendar.
The 13th month is not really a month, but the 5 (6 in leap years) day period of
`sansculottides' at the end of the year."
  (if (< month 13)
      30
    (if (calendar-french-leap-year-p year)
        6
      5)))

(defun calendar-french-to-absolute (date)
  "Compute absolute date from French Revolutionary date DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let ((month (calendar-extract-month date))
        (day (calendar-extract-day date))
        (year (calendar-extract-year date)))
    (+ (* 365 (1- year))                ; days in prior years
       ;; Leap days in prior years.
       (if (< year 20)
           (/ year 4) ; actual and anticipated practice (years 3, 7, 11, 15)
         ;; Romme's proposed rule (using the Principle of Inclusion/Exclusion).
         (+ (/ (1- year) 4) ; luckily, there were 4 leap years before year 20
            (- (/ (1- year) 100))
            (/ (1- year) 400)
            (- (/ (1- year) 4000))))
       (* 30 (1- month))              ; days in prior months this year
       day                            ; days so far this month
       (1- calendar-french-epoch))))  ; days before start of calendar

(define-obsolete-function-alias 'calendar-absolute-from-french
  'calendar-french-to-absolute "23.1")

(defun calendar-french-from-absolute (date)
  "Compute the French Revolutionary equivalent for absolute date DATE.
The result is a list of the form (MONTH DAY YEAR).
The absolute date is the number of days elapsed since the
\(imaginary) Gregorian date Sunday, December 31, 1 BC."
  (if (< date calendar-french-epoch)
      (list 0 0 0)                     ; pre-French Revolutionary date
    (let* ((approx                     ; approximation from below
            (/ (- date calendar-french-epoch) 366))
           (year               ; search forward from the approximation
            (+ approx
               (calendar-sum y approx
                             (>= date (calendar-french-to-absolute
                                       (list 1 1 (1+ y))))
                             1)))
           (month                    ; search forward from Vendemiaire
            (1+ (calendar-sum m 1
                              (> date
                                 (calendar-french-to-absolute
                                  (list m
                                        (calendar-french-last-day-of-month
                                         m year)
                                        year)))
                              1)))
           (day                     ; calculate the day by subtraction
            (- date
               (1- (calendar-french-to-absolute (list month 1 year))))))
      (list month day year))))

;;;###cal-autoload
(defun calendar-french-date-string (&optional date)
  "String of French Revolutionary date of Gregorian DATE.
Returns the empty string if DATE is pre-French Revolutionary.
Defaults to today's date if DATE is not given."
  (let* ((french-date (calendar-french-from-absolute
                       (calendar-absolute-from-gregorian
                        (or date (calendar-current-date)))))
         (y (calendar-extract-year french-date))
         (m (calendar-extract-month french-date))
         (d (calendar-extract-day french-date)))
    (cond
     ((< y 1) "")
     ((= m 13) (format (if (calendar-french-accents-p)
                           "Jour %s de l'Année %d de la Révolution"
                         "Jour %s de l'Anne'e %d de la Re'volution")
                       (aref (calendar-french-special-days-array) (1- d))
                       y))
     (t (format
         (if (calendar-french-accents-p)
             "%d %s an %d de la Révolution"
           "%d %s an %d de la Re'volution")
         d
         (aref (calendar-french-month-name-array) (1- m))
         y)))))

;;;###cal-autoload
(defun calendar-french-print-date ()
  "Show the French Revolutionary calendar equivalent of the selected date."
  (interactive)
  (let ((f (calendar-french-date-string (calendar-cursor-to-date t))))
    (if (string-equal f "")
        (message "Date is pre-French Revolution")
      (message "French Revolutionary date: %s" f))))

(define-obsolete-function-alias 'calendar-print-french-date
  'calendar-french-print-date "23.1")

;;;###cal-autoload
(defun calendar-french-goto-date (date &optional noecho)
  "Move cursor to French Revolutionary date DATE.
Echo French Revolutionary date unless NOECHO is non-nil."
  (interactive
   (let* ((months (calendar-french-month-name-array))
          (special-days (calendar-french-special-days-array))
          (year (progn
                  (calendar-read
                   (if (calendar-french-accents-p)
                       "Année de la Révolution (>0): "
                     "Anne'e de la Re'volution (>0): ")
                   (lambda (x) (> x 0))
                   (number-to-string
                    (calendar-extract-year
                     (calendar-french-from-absolute
                      (calendar-absolute-from-gregorian
                       (calendar-current-date))))))))
          (month-list
           (mapcar 'list
                   (append months
                           (if (calendar-french-leap-year-p year)
                               (mapcar
                                (lambda (x) (concat "Jour " x))
                                calendar-french-special-days-array)
                             (reverse
                              (cdr ; we don't want rev. day in a non-leap yr
                               (reverse
                                (mapcar
                                 (lambda (x)
                                   (concat "Jour " x))
                                 special-days))))))))
          (completion-ignore-case t)
          (month (cdr (assoc-string
                       (completing-read
                        "Mois ou Sansculottide: "
                        month-list
                        nil t)
                       (calendar-make-alist month-list 1 'car) t)))
          (day (if (> month 12)
                   (- month 12)
                 (calendar-read
                  "Jour (1-30): "
                  (lambda (x) (and (<= 1 x) (<= x 30))))))
          (month (if (> month 12) 13 month)))
     (list (list month day year))))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-french-to-absolute date)))
  (or noecho (calendar-french-print-date)))

(define-obsolete-function-alias 'calendar-goto-french-date
  'calendar-french-goto-date "23.1")

(defvar date)

;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-french-date ()
  "French calendar equivalent of date diary entry."
  (let ((f (calendar-french-date-string date)))
    (if (string-equal f "")
        "Date is pre-French Revolution"
      (format "French Revolutionary date: %s" f))))

(provide 'cal-french)

;; Local Variables:
;; coding: utf-8
;; End:

;;; cal-french.el ends here
