;;; cal-persia.el --- calendar functions for the Persian calendar

;; Copyright (C) 1996-1997, 2001-2012  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Persian calendar, calendar, diary
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

(defconst calendar-persian-month-name-array
  ["Farvardin" "Ordibehest" "Xordad" "Tir" "Mordad" "Sahrivar" "Mehr" "Aban"
   "Azar" "Dey" "Bahman" "Esfand"]
  "Names of the months in the Persian calendar.")

(eval-and-compile
  (autoload 'calendar-julian-to-absolute "cal-julian"))

(defconst calendar-persian-epoch
  (eval-when-compile (calendar-julian-to-absolute '(3 19 622)))
  "Absolute date of start of Persian calendar = March 19, 622 AD (Julian).")

(defun calendar-persian-leap-year-p (year)
  "True if YEAR is a leap year on the Persian calendar."
  (< (mod (* (mod (mod (if (<= 0 year)
                           (+ year 2346) ; no year zero
                         (+ year 2347))
                       2820)
                  768)
             683)
          2820)
     683))

(defun calendar-persian-last-day-of-month (month year)
  "Return last day of MONTH, YEAR on the Persian calendar."
  (cond
   ((< month 7) 31)
   ((or (< month 12) (calendar-persian-leap-year-p year)) 30)
   (t 29)))

(defun calendar-persian-to-absolute (date)
  "Compute absolute date from Persian date DATE.
The absolute date is the number of days elapsed since the (imaginary)
Gregorian date Sunday, December 31, 1 BC."
  (let ((month (calendar-extract-month date))
        (day (calendar-extract-day date))
        (year (calendar-extract-year date)))
    (if (< year 0)
        (+ (calendar-persian-to-absolute
            (list month day (1+ (mod year 2820))))
           (* 1029983 (floor year 2820)))
      (+ (1- calendar-persian-epoch)    ; days before epoch
         (* 365 (1- year))              ; days in prior years
         (* 683                  ; leap days in prior 2820-year cycles
            (floor (+ year 2345) 2820))
         (* 186                   ; leap days in prior 768 year cycles
            (floor (mod (+ year 2345) 2820) 768))
         (floor          ; leap years in current 768 or 516 year cycle
          (* 683 (mod (mod (+ year 2345) 2820) 768))
          2820)
         -568                 ; leap years in Persian years -2345...-1
         (calendar-sum        ; days in prior months this year
          m 1 (< m month)
          (calendar-persian-last-day-of-month m year))
         day))))                        ; days so far this month

(define-obsolete-function-alias 'calendar-absolute-from-persian
  'calendar-persian-to-absolute "23.1")

(defun calendar-persian-year-from-absolute (date)
  "Persian year corresponding to the absolute DATE."
  (let* ((d0                   ; prior days since start of 2820 cycles
          (- date (calendar-persian-to-absolute (list 1 1 -2345))))
         (n2820                         ; completed 2820-year cycles
          (floor d0 1029983))
         (d1                            ; prior days not in n2820
          (mod d0 1029983))
         (n768                          ; 768-year cycles not in n2820
          (floor d1 280506))
         (d2                         ; prior days not in n2820 or n768
          (mod d1 280506))
         (n1        ; years not in n2820 or n768
          ;; Want:
          ;; (floor (+ (* 2820 d2) (* 2820 366)) 1029983))
          ;; but that causes overflow, so use the following.
          ;; Use 366 as the divisor because (2820*366 mod 1029983) is small.
          (let ((a (floor d2 366))
                (b (mod d2 366)))
            (+ 1 a (floor (+ (* 2137 a) (* 2820 b) 2137) 1029983))))
         (year (+ (* 2820 n2820)        ; complete 2820 year cycles
                  (* 768 n768)          ; complete 768 year cycles
                  ;; Remaining years.
                  (if (= d1 1029617)    ; last day of 2820 year cycle
                      (1- n1)
                    n1)
                  -2345)))              ; years before year 1
    (if (< year 1)
        (1- year)                       ; no year zero
      year)))

(defun calendar-persian-from-absolute (date)
  "Compute the Persian equivalent for absolute date DATE.
The result is a list of the form (MONTH DAY YEAR).
The absolute date is the number of days elapsed since the imaginary
Gregorian date Sunday, December 31, 1 BC."
  (let* ((year (calendar-persian-year-from-absolute date))
         (month                        ; search forward from Farvardin
          (1+ (calendar-sum m 1
                            (> date
                               (calendar-persian-to-absolute
                                (list
                                 m
                                 (calendar-persian-last-day-of-month m year)
                                 year)))
                            1)))
         (day                       ; calculate the day by subtraction
          (- date (1- (calendar-persian-to-absolute
                       (list month 1 year))))))
    (list month day year)))

;;;###cal-autoload
(defun calendar-persian-date-string (&optional date)
  "String of Persian date of Gregorian DATE, default today."
  (let* ((persian-date (calendar-persian-from-absolute
                        (calendar-absolute-from-gregorian
                         (or date (calendar-current-date)))))
         (y (calendar-extract-year persian-date))
         (m (calendar-extract-month persian-date))
         (monthname (aref calendar-persian-month-name-array (1- m)))
         (day (number-to-string (calendar-extract-day persian-date)))
         (year (number-to-string y))
         (month (number-to-string m))
         dayname)
    (mapconcat 'eval calendar-date-display-form "")))

;;;###cal-autoload
(defun calendar-persian-print-date ()
  "Show the Persian calendar equivalent of the selected date."
  (interactive)
  (message "Persian date: %s"
           (calendar-persian-date-string (calendar-cursor-to-date t))))

(define-obsolete-function-alias 'calendar-print-persian-date
  'calendar-persian-print-date "23.1")

(defun calendar-persian-read-date ()
  "Interactively read the arguments for a Persian date command.
Reads a year, month, and day."
  (let* ((year (calendar-read
                "Persian calendar year (not 0): "
                (lambda (x) (not (zerop x)))
                (number-to-string
                 (calendar-extract-year
                  (calendar-persian-from-absolute
                   (calendar-absolute-from-gregorian
                    (calendar-current-date)))))))
         (completion-ignore-case t)
         (month (cdr (assoc
                      (completing-read
                       "Persian calendar month name: "
                       (mapcar 'list
                               (append calendar-persian-month-name-array nil))
                       nil t)
                      (calendar-make-alist calendar-persian-month-name-array
                                           1))))
         (last (calendar-persian-last-day-of-month month year))
         (day (calendar-read
               (format "Persian calendar day (1-%d): " last)
               (lambda (x) (and (< 0 x) (<= x last))))))
    (list (list month day year))))

(define-obsolete-function-alias 'persian-prompt-for-date
  'calendar-persian-read-date "23.1")

;;;###cal-autoload
(defun calendar-persian-goto-date (date &optional noecho)
  "Move cursor to Persian date DATE.
Echo Persian date unless NOECHO is non-nil."
  (interactive (calendar-persian-read-date))
  (calendar-goto-date (calendar-gregorian-from-absolute
                       (calendar-persian-to-absolute date)))
  (or noecho (calendar-persian-print-date)))

(define-obsolete-function-alias 'calendar-goto-persian-date
  'calendar-persian-goto-date "23.1")

(defvar date)

;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-persian-date ()
  "Persian calendar equivalent of date diary entry."
  (format "Persian date: %s" (calendar-persian-date-string date)))

(provide 'cal-persia)

;;; cal-persia.el ends here
