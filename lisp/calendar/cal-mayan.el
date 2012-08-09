;;; cal-mayan.el --- calendar functions for the Mayan calendars

;; Copyright (C) 1992-1993, 1995, 1997, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Stewart M. Clamen <clamen@cs.cmu.edu>
;;         Edward M. Reingold <reingold@cs.uiuc.edu>
;; Maintainer: Glenn Morris <rgm@gnu.org>
;; Keywords: calendar
;; Human-Keywords: Mayan calendar, Maya, calendar, diary
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

(defconst calendar-mayan-days-before-absolute-zero 1137142
  "Number of days of the Mayan calendar epoch before absolute day 0.
This is the Goodman-Martinez-Thompson correlation used by almost all experts,
but some use 1137140.  Using 1232041 gives you Spinden's correlation; using
1142840 gives you Hochleitner's correlation.")

(defconst calendar-mayan-haab-at-epoch '(8 . 18)
  "Mayan haab date at the epoch.")

(defconst calendar-mayan-haab-month-name-array
  ["Pop" "Uo" "Zip" "Zotz" "Tzec" "Xul" "Yaxkin" "Mol" "Chen" "Yax"
   "Zac" "Ceh" "Mac" "Kankin" "Muan" "Pax" "Kayab" "Cumku"]
  "Names of the Mayan haab months.")

(defconst calendar-mayan-tzolkin-at-epoch '(4 . 20)
  "Mayan tzolkin date at the epoch.")

(defconst calendar-mayan-tzolkin-names-array
  ["Imix" "Ik" "Akbal" "Kan" "Chicchan" "Cimi" "Manik" "Lamat" "Muluc" "Oc"
   "Chuen" "Eb" "Ben" "Ix" "Men" "Cib" "Caban" "Etznab" "Cauac" "Ahau"]
  "Names of the Mayan tzolkin months.")

(defun calendar-mayan-long-count-from-absolute (date)
  "Compute the Mayan long count corresponding to the absolute DATE."
  (let* ((long-count (+ date calendar-mayan-days-before-absolute-zero))
         (baktun (/ long-count 144000))
         (remainder (% long-count 144000))
         (katun (/ remainder 7200))
         (remainder (% remainder 7200))
         (tun (/ remainder 360))
         (remainder (% remainder 360))
         (uinal (/ remainder 20))
         (kin (% remainder 20)))
    (list baktun katun tun uinal kin)))

(defun calendar-mayan-long-count-to-string (mayan-long-count)
  "Convert MAYAN-LONG-COUNT into traditional written form."
  (apply 'format (cons "%s.%s.%s.%s.%s" mayan-long-count)))

(defun calendar-mayan-string-from-long-count (str)
  "Given STR, a string of format \"%d.%d.%d.%d.%d\", return list of numbers."
  (let ((end 0)
        rlc)
    (condition-case nil
        (progn
          ;; cf split-string.
          (while (string-match "[0-9]+" str end)
            (setq rlc (cons (string-to-number (match-string 0 str)) rlc)
                  end (match-end 0)))
          (unless (= (length rlc) 5) (signal 'invalid-read-syntax nil)))
      (invalid-read-syntax nil))
    (nreverse rlc)))

(defun calendar-mayan-haab-from-absolute (date)
  "Convert absolute DATE into a Mayan haab date (a pair)."
  (let* ((long-count (+ date calendar-mayan-days-before-absolute-zero))
         (day-of-haab
          (% (+ long-count
                (car calendar-mayan-haab-at-epoch)
                (* 20 (1- (cdr calendar-mayan-haab-at-epoch))))
             365))
         (day (% day-of-haab 20))
         (month (1+ (/ day-of-haab 20))))
    (cons day month)))

(defun calendar-mayan-haab-difference (date1 date2)
  "Number of days from Mayan haab DATE1 to next occurrence of haab date DATE2."
  (mod (+ (* 20 (- (cdr date2) (cdr date1)))
          (- (car date2) (car date1)))
       365))

(defun calendar-mayan-haab-on-or-before (haab-date date)
  "Absolute date of latest HAAB-DATE on or before absolute DATE."
  (- date
     (% (- date
           (calendar-mayan-haab-difference
            (calendar-mayan-haab-from-absolute 0) haab-date))
        365)))

;;;###cal-autoload
(defun calendar-mayan-date-string (&optional date)
  "String of Mayan date of Gregorian DATE; default today."
  (let* ((d (calendar-absolute-from-gregorian
             (or date (calendar-current-date))))
         (tzolkin (calendar-mayan-tzolkin-from-absolute d))
         (haab (calendar-mayan-haab-from-absolute d))
         (long-count (calendar-mayan-long-count-from-absolute d)))
    (format "Long count = %s; tzolkin = %s; haab = %s"
            (calendar-mayan-long-count-to-string long-count)
            (calendar-mayan-tzolkin-to-string tzolkin)
            (calendar-mayan-haab-to-string haab))))

;;;###cal-autoload
(defun calendar-mayan-print-date ()
  "Show the Mayan long count, tzolkin, and haab equivalents of date."
  (interactive)
  (message "Mayan date: %s"
           (calendar-mayan-date-string (calendar-cursor-to-date t))))

(define-obsolete-function-alias 'calendar-print-mayan-date
  'calendar-mayan-print-date "23.1")

(defun calendar-mayan-read-haab-date ()
  "Prompt for a Mayan haab date."
  (let* ((completion-ignore-case t)
         (haab-day (calendar-read
                    "Haab kin (0-19): "
                    (lambda (x) (and (>= x 0) (< x 20)))))
         (haab-month-list (append calendar-mayan-haab-month-name-array
                                  (and (< haab-day 5) '("Uayeb"))))
         (haab-month (cdr
                      (assoc-string
                       (completing-read "Haab uinal: "
                                        (mapcar 'list haab-month-list)
                                        nil t)
                       (calendar-make-alist haab-month-list 1) t))))
    (cons haab-day haab-month)))

(defun calendar-mayan-read-tzolkin-date ()
  "Prompt for a Mayan tzolkin date."
  (let* ((completion-ignore-case t)
         (tzolkin-count (calendar-read
                         "Tzolkin kin (1-13): "
                         (lambda (x) (and (> x 0) (< x 14)))))
         (tzolkin-name-list (append calendar-mayan-tzolkin-names-array nil))
         (tzolkin-name (cdr
                        (assoc-string
                         (completing-read "Tzolkin uinal: "
                                          (mapcar 'list tzolkin-name-list)
                                          nil t)
                         (calendar-make-alist tzolkin-name-list 1) t))))
    (cons tzolkin-count tzolkin-name)))

;;;###cal-autoload
(defun calendar-mayan-next-haab-date (haab-date &optional noecho)
  "Move cursor to next instance of Mayan HAAB-DATE.
Echo Mayan date unless NOECHO is non-nil."
  (interactive (list (calendar-mayan-read-haab-date)))
  (calendar-goto-date
   (calendar-gregorian-from-absolute
    (calendar-mayan-haab-on-or-before
     haab-date
     (+ 365
        (calendar-absolute-from-gregorian (calendar-cursor-to-date))))))
  (or noecho (calendar-mayan-print-date)))

(define-obsolete-function-alias 'calendar-next-haab-date
  'calendar-mayan-next-haab-date "23.1")

;;;###cal-autoload
(defun calendar-mayan-previous-haab-date (haab-date &optional noecho)
  "Move cursor to previous instance of Mayan HAAB-DATE.
Echo Mayan date unless NOECHO is non-nil."
  (interactive (list (calendar-mayan-read-haab-date)))
  (calendar-goto-date
   (calendar-gregorian-from-absolute
    (calendar-mayan-haab-on-or-before
     haab-date
     (1- (calendar-absolute-from-gregorian (calendar-cursor-to-date))))))
  (or noecho (calendar-mayan-print-date)))

(define-obsolete-function-alias 'calendar-previous-haab-date
  'calendar-mayan-previous-haab-date "23.1")

(defun calendar-mayan-haab-to-string (haab)
  "Convert Mayan HAAB date (a pair) into its traditional written form."
  (let ((month (cdr haab)))
    (format "%d %s" (car haab)          ; day
            ;; 19th month consists of 5 special days
            (if (= month 19) "Uayeb"
              (aref calendar-mayan-haab-month-name-array (1- month))))))

(defun calendar-mayan-tzolkin-from-absolute (date)
  "Convert absolute DATE into a Mayan tzolkin date (a pair)."
  (let* ((long-count (+ date calendar-mayan-days-before-absolute-zero))
         ;; Remainder on division by 13,20 with 13,20 instead of zero.
         (day (1+ (mod
                   (1- (+ long-count (car calendar-mayan-tzolkin-at-epoch)))
                   13)))
         (name (1+ (mod
                    (1- (+ long-count (cdr calendar-mayan-tzolkin-at-epoch)))
                    20))))
    (cons day name)))

(defun calendar-mayan-tzolkin-difference (date1 date2)
  "Number of days from Mayan tzolkin DATE1 to next occurrence of tzolkin DATE2."
  (let ((number-difference (- (car date2) (car date1)))
        (name-difference (- (cdr date2) (cdr date1))))
    (mod (+ number-difference
            (* 13 (mod (* 3 (- number-difference name-difference))
                       20)))
         260)))

(defun calendar-mayan-tzolkin-on-or-before (tzolkin-date date)
  "Absolute date of latest TZOLKIN-DATE on or before absolute DATE."
  (- date
     (% (- date (calendar-mayan-tzolkin-difference
                 (calendar-mayan-tzolkin-from-absolute 0)
                 tzolkin-date))
        260)))

;;;###cal-autoload
(defun calendar-mayan-next-tzolkin-date (tzolkin-date &optional noecho)
  "Move cursor to next instance of Mayan TZOLKIN-DATE.
Echo Mayan date unless NOECHO is non-nil."
  (interactive (list (calendar-mayan-read-tzolkin-date)))
  (calendar-goto-date
   (calendar-gregorian-from-absolute
    (calendar-mayan-tzolkin-on-or-before
     tzolkin-date
     (+ 260
        (calendar-absolute-from-gregorian (calendar-cursor-to-date))))))
  (or noecho (calendar-mayan-print-date)))

(define-obsolete-function-alias 'calendar-next-tzolkin-date
  'calendar-mayan-next-tzolkin-date "23.1")

;;;###cal-autoload
(defun calendar-mayan-previous-tzolkin-date (tzolkin-date &optional noecho)
  "Move cursor to previous instance of Mayan TZOLKIN-DATE.
Echo Mayan date unless NOECHO is non-nil."
  (interactive (list (calendar-mayan-read-tzolkin-date)))
  (calendar-goto-date
   (calendar-gregorian-from-absolute
    (calendar-mayan-tzolkin-on-or-before
     tzolkin-date
     (1- (calendar-absolute-from-gregorian (calendar-cursor-to-date))))))
  (or noecho (calendar-mayan-print-date)))

(define-obsolete-function-alias 'calendar-previous-tzolkin-date
  'calendar-mayan-previous-tzolkin-date "23.1")

(defun calendar-mayan-tzolkin-to-string (tzolkin)
  "Convert Mayan TZOLKIN date (a pair) into its traditional written form."
  (format "%d %s"
          (car tzolkin)
          (aref calendar-mayan-tzolkin-names-array (1- (cdr tzolkin)))))

(defun calendar-mayan-tzolkin-haab-on-or-before (tzolkin-date haab-date date)
  "Absolute date that is Mayan TZOLKIN-DATE and HAAB-DATE.
Latest such date on or before DATE.
Returns nil if such a tzolkin-haab combination is impossible."
  (let* ((haab-difference
          (calendar-mayan-haab-difference
           (calendar-mayan-haab-from-absolute 0)
           haab-date))
         (tzolkin-difference
          (calendar-mayan-tzolkin-difference
           (calendar-mayan-tzolkin-from-absolute 0)
           tzolkin-date))
         (difference (- tzolkin-difference haab-difference)))
    (if (zerop (% difference 5))
        (- date
           (mod (- date
                   (+ haab-difference (* 365 difference)))
                18980))
      nil)))

;;;###cal-autoload
(defun calendar-mayan-next-round-date (tzolkin-date haab-date
                                                       &optional noecho)
  "Move cursor to next instance of Mayan TZOLKIN-DATE HAAB-DATE combination.
Echo Mayan date unless NOECHO is non-nil."
  (interactive (list (calendar-mayan-read-tzolkin-date)
                     (calendar-mayan-read-haab-date)))
  (let ((date (calendar-mayan-tzolkin-haab-on-or-before
               tzolkin-date haab-date
               (+ 18980 (calendar-absolute-from-gregorian
                         (calendar-cursor-to-date))))))
    (if (not date)
        (error "%s, %s does not exist in the Mayan calendar round"
               (calendar-mayan-tzolkin-to-string tzolkin-date)
               (calendar-mayan-haab-to-string haab-date))
      (calendar-goto-date (calendar-gregorian-from-absolute date))
      (or noecho (calendar-mayan-print-date)))))

(define-obsolete-function-alias 'calendar-next-calendar-round-date
  'calendar-mayan-next-round-date "23.1")

;;;###cal-autoload
(defun calendar-mayan-previous-round-date
  (tzolkin-date haab-date &optional noecho)
  "Move to previous instance of Mayan TZOLKIN-DATE HAAB-DATE combination.
Echo Mayan date unless NOECHO is non-nil."
  (interactive (list (calendar-mayan-read-tzolkin-date)
                     (calendar-mayan-read-haab-date)))
  (let ((date (calendar-mayan-tzolkin-haab-on-or-before
               tzolkin-date haab-date
               (1- (calendar-absolute-from-gregorian
                    (calendar-cursor-to-date))))))
    (if (not date)
        (error "%s, %s does not exist in the Mayan calendar round"
               (calendar-mayan-tzolkin-to-string tzolkin-date)
               (calendar-mayan-haab-to-string haab-date))
      (calendar-goto-date (calendar-gregorian-from-absolute date))
      (or noecho (calendar-mayan-print-date)))))

(define-obsolete-function-alias 'calendar-previous-calendar-round-date
  'calendar-mayan-previous-round-date "23.1")

(defun calendar-mayan-long-count-to-absolute (c)
  "Compute the absolute date corresponding to the Mayan Long Count C.
Long count is a list (baktun katun tun uinal kin)"
  (+ (* (nth 0 c) 144000)               ; baktun
     (* (nth 1 c) 7200)                 ; katun
     (* (nth 2 c) 360)                  ; tun
     (* (nth 3 c) 20)                   ; uinal
     (nth 4 c)                          ; kin (days)
     ;; Days before absolute date 0.
     (- calendar-mayan-days-before-absolute-zero)))

(define-obsolete-function-alias 'calendar-absolute-from-mayan-long-count
  'calendar-mayan-long-count-to-absolute "23.1")

(defun calendar-mayan-long-count-common-era (lc)
  "Return non-nil if long count LC represents a date in the Common Era."
  (let ((base (calendar-mayan-long-count-from-absolute 1)))
    (while (and base (= (car lc) (car base)))
      (setq lc (cdr lc)
            base (cdr base)))
    (or (null lc) (> (car lc) (car base)))))

;;;###cal-autoload
(defun calendar-mayan-goto-long-count-date (date &optional noecho)
  "Move cursor to Mayan long count DATE.
Echo Mayan date unless NOECHO is non-nil."
  (interactive
   (let (datum)
     (while (not (setq datum
                       (calendar-mayan-string-from-long-count
                        (read-string
                         "Mayan long count (baktun.katun.tun.uinal.kin): "
                         (calendar-mayan-long-count-to-string
                          (calendar-mayan-long-count-from-absolute
                           (calendar-absolute-from-gregorian
                            (calendar-current-date))))))
                       datum (if (calendar-mayan-long-count-common-era datum)
                                 (list datum)))))
     datum))
  (calendar-goto-date
   (calendar-gregorian-from-absolute
    (calendar-mayan-long-count-to-absolute date)))
  (or noecho (calendar-mayan-print-date)))

(define-obsolete-function-alias 'calendar-goto-mayan-long-count-date
  'calendar-mayan-goto-long-count-date "23.1")

(defvar date)

;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-mayan-date ()
  "Show the Mayan long count, haab, and tzolkin dates as a diary entry."
  (format "Mayan date: %s" (calendar-mayan-date-string date)))

(provide 'cal-mayan)

;;; cal-mayan.el ends here
