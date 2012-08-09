;;; cal-move.el --- calendar functions for movement in the calendar

;; Copyright (C) 1995, 2001-2012  Free Software Foundation, Inc.

;; Author: Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: calendar
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

;; FIXME should calendar just require this?
(require 'calendar)


;; Note that this is not really the "closest" date.
;; In most cases, it just searches forwards for the next day.
;;;###cal-autoload
(defun calendar-cursor-to-nearest-date ()
  "Move the cursor to the closest date.
The position of the cursor is unchanged if it is already on a date.
Returns the list (month day year) giving the cursor position."
  (or (calendar-cursor-to-date)
      (let* ((col (current-column))
             (edges (cdr (assoc (calendar-column-to-segment)
                                calendar-month-edges)))
             (last (nth 2 edges))
             (right (nth 3 edges)))
        (when (< (count-lines (point-min) (point)) calendar-first-date-row)
          (goto-char (point-min))
          (forward-line (1- calendar-first-date-row))
          (move-to-column col))
        ;; The date positions are fixed and computable, but searching
        ;; is probably more flexible.  Need to consider blank days at
        ;; start and end of month if computing positions.
        ;; 'date text-property is used to exclude intermonth text.
        (unless (and (looking-at "[0-9]")
                     (get-text-property (point) 'date))
          ;; We search forwards for a number, except close to the RH
          ;; margin of a month, where we search backwards.
          ;; Note that the searches can go to other lines.
          (if (or (looking-at " *$")
                  (and (> col last) (< col right)))
              (while (and (re-search-backward "[0-9]" nil t)
                          (not (get-text-property (point) 'date))))
            (while (and (re-search-forward "[0-9]" nil t)
                        (not (get-text-property (1- (point)) 'date))))
            (backward-char 1)))
        (calendar-cursor-to-date))))

(defvar displayed-month)                ; from calendar-generate
(defvar displayed-year)

;;;###cal-autoload
(defun calendar-cursor-to-visible-date (date)
  "Move the cursor to DATE that is on the screen."
  (let ((month (calendar-extract-month date))
        (day (calendar-extract-day date))
        (year (calendar-extract-year date)))
    (goto-char (point-min))
    (forward-line (+ calendar-first-date-row -1
                     (/ (+ day -1
                           (mod
                            (- (calendar-day-of-week (list month 1 year))
                               calendar-week-start-day)
                            7))
                        7)))
    (move-to-column (+ calendar-left-margin (1- calendar-day-digit-width)
                       (* calendar-month-width
                          (1+ (calendar-interval
                               displayed-month displayed-year month year)))
                       (* calendar-column-width
                          (mod
                           (- (calendar-day-of-week date)
                              calendar-week-start-day)
                           7))))))

;;;###cal-autoload
(defun calendar-goto-today ()
  "Reposition the calendar window so the current date is visible."
  (interactive)
  (let ((today (calendar-current-date))) ; the date might have changed
    (if (not (calendar-date-is-visible-p today))
        (calendar-generate-window)
      (calendar-update-mode-line)
      (calendar-cursor-to-visible-date today)))
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-forward-month (arg)
  "Move the cursor forward ARG months.
Movement is backward if ARG is negative."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((cursor-date (calendar-cursor-to-date t))
         (month (calendar-extract-month cursor-date))
         (day (calendar-extract-day cursor-date))
         (year (calendar-extract-year cursor-date))
         (last (progn
                 (calendar-increment-month month year arg)
                 (calendar-last-day-of-month month year)))
         (day (min last day))
         ;; Put the new month on the screen, if needed, and go to the new date.
         (new-cursor-date (list month day year)))
    (if (not (calendar-date-is-visible-p new-cursor-date))
        (calendar-other-month month year))
    (calendar-cursor-to-visible-date new-cursor-date))
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-forward-year (arg)
  "Move the cursor forward by ARG years.
Movement is backward if ARG is negative."
  (interactive "p")
  (calendar-forward-month (* 12 arg)))

;;;###cal-autoload
(defun calendar-backward-month (arg)
  "Move the cursor backward by ARG months.
Movement is forward if ARG is negative."
  (interactive "p")
  (calendar-forward-month (- arg)))

;;;###cal-autoload
(defun calendar-backward-year (arg)
  "Move the cursor backward ARG years.
Movement is forward is ARG is negative."
  (interactive "p")
  (calendar-forward-month (* -12 arg)))

;;;###cal-autoload
(defun calendar-scroll-left (&optional arg event)
  "Scroll the displayed calendar left by ARG months.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 1))
  (save-selected-window
    ;; Nil if called from menu-bar.
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (calendar-cursor-to-nearest-date)
    (unless (zerop arg)
      (let ((old-date (calendar-cursor-to-date))
            (today (calendar-current-date))
            (month displayed-month)
            (year displayed-year))
        (calendar-increment-month month year arg)
        (calendar-generate-window month year)
        (calendar-cursor-to-visible-date
         (cond
          ((calendar-date-is-visible-p old-date) old-date)
          ((calendar-date-is-visible-p today) today)
          (t (list month 1 year))))))
    (run-hooks 'calendar-move-hook)))

(define-obsolete-function-alias
  'scroll-calendar-left 'calendar-scroll-left "23.1")

;;;###cal-autoload
(defun calendar-scroll-right (&optional arg event)
  "Scroll the displayed calendar window right by ARG months.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (calendar-scroll-left (- (or arg 1)) event))

(define-obsolete-function-alias
  'scroll-calendar-right 'calendar-scroll-right "23.1")

;;;###cal-autoload
(defun calendar-scroll-left-three-months (arg &optional event)
  "Scroll the displayed calendar window left by 3*ARG months.
If ARG is negative the calendar is scrolled right.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (calendar-scroll-left (* 3 arg) event))

(define-obsolete-function-alias 'scroll-calendar-left-three-months
  'calendar-scroll-left-three-months "23.1")

;; cf scroll-bar-toolkit-scroll
;;;###cal-autoload
(defun calendar-scroll-toolkit-scroll (event)
  "Function to scroll the calendar after a toolkit scroll-bar click."
  (interactive "e")
  (let ((part (nth 4 (event-end event))))
    ;; Not bothering with drag events (handle, end-scroll).
    (cond ((memq part '(above-handle up top))
           (calendar-scroll-right nil event))
          ((memq part '(below-handle down bottom))
           (calendar-scroll-left nil event)))))

;;;###cal-autoload
(defun calendar-scroll-right-three-months (arg &optional event)
  "Scroll the displayed calendar window right by 3*ARG months.
If ARG is negative the calendar is scrolled left.  Maintains the relative
position of the cursor with respect to the calendar as well as possible.
EVENT is an event like `last-nonmenu-event'."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (calendar-scroll-left (* -3 arg) event))

(define-obsolete-function-alias 'scroll-calendar-right-three-months
  'calendar-scroll-right-three-months "23.1")

;;;###cal-autoload
(defun calendar-forward-day (arg)
  "Move the cursor forward ARG days.
Moves backward if ARG is negative."
  (interactive "p")
  (unless (zerop arg)
    (let* ((cursor-date (or (calendar-cursor-to-date)
                            (progn
                              (if (> arg 0) (setq arg (1- arg)))
                              (calendar-cursor-to-nearest-date))))
           (new-cursor-date
            (calendar-gregorian-from-absolute
             (+ (calendar-absolute-from-gregorian cursor-date) arg)))
           (new-display-month (calendar-extract-month new-cursor-date))
           (new-display-year (calendar-extract-year new-cursor-date)))
      ;; Put the new month on the screen, if needed.
      (unless (calendar-date-is-visible-p new-cursor-date)
        ;; The next line gives smoother scrolling IMO (one month at a
        ;; time rather than two).
        (calendar-increment-month new-display-month new-display-year
                                  (if (< arg 0) 1 -1))
        (calendar-other-month new-display-month new-display-year))
      ;; Go to the new date.
      (calendar-cursor-to-visible-date new-cursor-date)))
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-backward-day (arg)
  "Move the cursor back ARG days.
Moves forward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (- arg)))

;;;###cal-autoload
(defun calendar-forward-week (arg)
  "Move the cursor forward ARG weeks.
Moves backward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (* arg 7)))

;;;###cal-autoload
(defun calendar-backward-week (arg)
  "Move the cursor back ARG weeks.
Moves forward if ARG is negative."
  (interactive "p")
  (calendar-forward-day (* arg -7)))

;;;###cal-autoload
(defun calendar-beginning-of-week (arg)
  "Move the cursor back ARG calendar-week-start-day's."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let ((day (calendar-day-of-week (calendar-cursor-to-date))))
    (calendar-backward-day
     (if (= day calendar-week-start-day)
         (* 7 arg)
       (+ (mod (- day calendar-week-start-day) 7)
          (* 7 (1- arg)))))))

;;;###cal-autoload
(defun calendar-end-of-week (arg)
  "Move the cursor forward ARG calendar-week-start-day+6's."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let ((day (calendar-day-of-week (calendar-cursor-to-date))))
    (calendar-forward-day
     (if (= day (mod (1- calendar-week-start-day) 7))
         (* 7 arg)
       (+ (- 6 (mod (- day calendar-week-start-day) 7))
          (* 7 (1- arg)))))))

;;;###cal-autoload
(defun calendar-beginning-of-month (arg)
  "Move the cursor backward ARG month beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date)))
    (if (= day 1)
        (calendar-backward-month arg)
      (calendar-cursor-to-visible-date (list month 1 year))
      (calendar-backward-month (1- arg)))))

;;;###cal-autoload
(defun calendar-end-of-month (arg)
  "Move the cursor forward ARG month ends."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (last-day (calendar-last-day-of-month month year))
         (last-day (progn
                     (unless (= day last-day)
                       (calendar-cursor-to-visible-date
                        (list month last-day year))
                       (setq arg (1- arg)))
                     (calendar-increment-month month year arg)
                     (list month
                           (calendar-last-day-of-month month year)
                           year))))
    (if (not (calendar-date-is-visible-p last-day))
        (calendar-other-month month year)
      (calendar-cursor-to-visible-date last-day)))
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-beginning-of-year (arg)
  "Move the cursor backward ARG year beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (jan-first (list 1 1 year))
         (calendar-move-hook nil))
    (if (and (= day 1) (= 1 month))
        (calendar-backward-month (* 12 arg))
      (if (and (= arg 1)
               (calendar-date-is-visible-p jan-first))
          (calendar-cursor-to-visible-date jan-first)
        (calendar-other-month 1 (- year (1- arg)))
        (calendar-cursor-to-visible-date (list 1 1 displayed-year)))))
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-end-of-year (arg)
  "Move the cursor forward ARG year beginnings."
  (interactive "p")
  (calendar-cursor-to-nearest-date)
  (let* ((date (calendar-cursor-to-date))
         (month (calendar-extract-month date))
         (day (calendar-extract-day date))
         (year (calendar-extract-year date))
         (dec-31 (list 12 31 year))
         (calendar-move-hook nil))
    (if (and (= day 31) (= 12 month))
        (calendar-forward-month (* 12 arg))
      (if (and (= arg 1)
               (calendar-date-is-visible-p dec-31))
          (calendar-cursor-to-visible-date dec-31)
        (calendar-other-month 12 (+ year (1- arg)))
        (calendar-cursor-to-visible-date (list 12 31 displayed-year)))))
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-goto-date (date)
  "Move cursor to DATE."
  (interactive (list (calendar-read-date)))
  (let ((month (calendar-extract-month date))
        (year (calendar-extract-year date)))
    (if (not (calendar-date-is-visible-p date))
        (calendar-other-month
         (if (and (= month 1) (= year 1))
             2
           month)
         year)))
  (calendar-cursor-to-visible-date date)
  (run-hooks 'calendar-move-hook))

;;;###cal-autoload
(defun calendar-goto-day-of-year (year day &optional noecho)
  "Move cursor to YEAR, DAY number; echo DAY/YEAR unless NOECHO is non-nil.
Negative DAY counts backward from end of year."
  (interactive
   (let* ((year (calendar-read
                 "Year (>0): "
                 (lambda (x) (> x 0))
                 (number-to-string (calendar-extract-year
                                 (calendar-current-date)))))
          (last (if (calendar-leap-year-p year) 366 365))
          (day (calendar-read
                (format "Day number (+/- 1-%d): " last)
                (lambda (x) (and (<= 1 (abs x)) (<= (abs x) last))))))
     (list year day)))
  (calendar-goto-date
   (calendar-gregorian-from-absolute
    (if (< 0 day)
        (+ -1 day (calendar-absolute-from-gregorian (list 1 1 year)))
      (+ 1 day (calendar-absolute-from-gregorian (list 12 31 year))))))
  (or noecho (calendar-print-day-of-year)))

(provide 'cal-move)

;;; cal-move.el ends here
