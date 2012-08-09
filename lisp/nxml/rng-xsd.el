;;; rng-xsd.el --- W3C XML Schema datatypes library for RELAX NG

;; Copyright (C) 2003, 2007-2012 Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML, RelaxNG

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

;; The main entry point is `rng-xsd-compile'. The validator
;; knows to use this for the datatype library with URI
;; http://www.w3.org/2001/XMLSchema-datatypes because it
;; is the value of the rng-dt-compile property on that URI
;; as a symbol.
;;
;; W3C XML Schema Datatypes are specified by
;;   http://www.w3.org/TR/xmlschema-2/
;; Guidelines for using them with RELAX NG are described in
;;   http://relaxng.org/xsd.html

;;; Code:

(require 'rng-dt)
(require 'rng-util)
(require 'xsd-regexp)

;;;###autoload
(put 'http://www.w3.org/2001/XMLSchema-datatypes
     'rng-dt-compile
     'rng-xsd-compile)

;;;###autoload
(defun rng-xsd-compile (name params)
  "Provides W3C XML Schema as a RELAX NG datatypes library.
NAME is a symbol giving the local name of the datatype.  PARAMS is a
list of pairs (PARAM-NAME . PARAM-VALUE) where PARAM-NAME is a symbol
giving the name of the parameter and PARAM-VALUE is a string giving
its value.  If NAME or PARAMS are invalid, it calls rng-dt-error
passing it arguments in the same style as format; the value from
rng-dt-error will be returned.  Otherwise, it returns a list.  The
first member of the list is t if any string is a legal value for the
datatype and nil otherwise.  The second argument is a symbol; this
symbol will be called as a function passing it a string followed by
the remaining members of the list.  The function must return an object
representing the value of the datatype that was represented by the
string, or nil if the string is not a representation of any value.
The object returned can be any convenient non-nil value, provided
that, if two strings represent the same value, the returned objects
must be equal."
  (let ((convert (get name 'rng-xsd-convert)))
    (if (not convert)
	(rng-dt-error "There is no XSD datatype named %s" name)
      (rng-xsd-compile1 name params convert))))

;;; Parameters

(defun rng-xsd-compile1 (name params convert)
  (if (null params)
      (cons (equal convert '(identity))
	    (cond ((eq name 'string) convert)
		  ((eq name 'normalizedString)
		   (cons 'rng-xsd-replace-space convert))
		  ((and (not (eq name 'string))
			(or (memq 'identity convert)
			    (memq 'rng-xsd-convert-any-uri convert)
			    (memq 'rng-xsd-check-pattern convert)))
		   (cons 'rng-xsd-collapse-space convert))
		  (t convert)))
    (let* ((param (car params))
	   (param-name (car param))
	   (param-value (cdr param)))
      (cond ((memq param-name
		   '(minExclusive maxExclusive minInclusive maxInclusive))
	     (let ((limit (apply (car convert)
				 (cons param-value
				       (cdr convert))))
		   (less-than-fun (get name 'rng-xsd-less-than)))
	       (cond ((not limit)
		      (rng-dt-error "Minimum value %s is not valid"
				    param-value))
		     ((not less-than-fun)
		      (rng-dt-error "Values of type %s are not ordered"
				    param-name))
		     (t
		      (rng-xsd-compile1 name
					(cdr params)
					(cons (get param-name
						   'rng-xsd-check)
					      (cons less-than-fun
						    (cons limit convert))))))))
	    ((memq param-name '(length minLength maxLength))
	     (let ((limit (rng-xsd-string-to-non-negative-integer param-value))
		   (length-fun (get name 'rng-xsd-length)))
	       (cond ((not limit)
		      (rng-dt-error "Length %s is not valid" param-value))
		     ((not length-fun)
		      (rng-dt-error "Values of type %s do not have a length"
				    param-name))
		     (t
		      (rng-xsd-compile1 name
					(cdr params)
					(cons (get param-name
						   'rng-xsd-check)
					      (cons length-fun
						    (cons limit convert))))))))
	    ((memq param-name '(fractionDigits totalDigits))
	     (let ((n (rng-xsd-string-to-non-negative-integer param-value)))
	       (cond ((not n)
		      (rng-dt-error "Number of digits %s is not valid"
				    param-value))
		     (t
		      (rng-xsd-compile1 name
					(cdr params)
					(cons (get param-name
						   'rng-xsd-check)
					      (cons n convert)))))))
	    ((eq param-name 'pattern)
	     (condition-case err
		 (rng-xsd-compile1 name
				   (cdr params)
				   (cons 'rng-xsd-check-pattern
					 (cons (concat
						"\\`"
						(xsdre-translate param-value)
						"\\'")
					       convert)))
	       (xsdre-invalid-regexp
		(rng-dt-error "Invalid regular expression (%s)"
			      (nth 1 err)))))
	    ((memq param-name '(enumeration whiteSpace))
	     (rng-dt-error "Facet %s cannot be used in RELAX NG" param-name))
	    (t (rng-dt-error "Unknown facet %s" param-name))))))

(defun rng-xsd-string-to-non-negative-integer (str)
  (and (rng-xsd-convert-integer str)
       (let ((n (string-to-number str)))
	 (and (integerp n)
	      (>= n 0)
	      n))))

(defun rng-xsd-collapse-space (str convert &rest args)
  (apply convert (cons (mapconcat 'identity (split-string str "[ \t\n\r]+")
				  " ")
		       args)))

(defun rng-xsd-replace-space (str convert &rest args)
  (apply convert
	 (cons (let ((i 0)
		     copied)
		 (while (and (setq i (string-match "[\r\n\t]" str i))
			     (or copied (setq copied (copy-sequence str)))
			     (aset copied i 32)
			     (setq i (1+ i))))
		 (or copied str))
	       args)))

(put 'minExclusive 'rng-xsd-check 'rng-xsd-check-min-exclusive)
(put 'minInclusive 'rng-xsd-check 'rng-xsd-check-min-inclusive)
(put 'maxExclusive 'rng-xsd-check 'rng-xsd-check-max-exclusive)
(put 'maxInclusive 'rng-xsd-check 'rng-xsd-check-max-inclusive)
(put 'length 'rng-xsd-check 'rng-xsd-check-length)
(put 'minLength 'rng-xsd-check 'rng-xsd-check-min-length)
(put 'maxLength 'rng-xsd-check 'rng-xsd-check-max-length)
(put 'fractionDigits 'rng-xsd-check 'rng-xsd-check-fraction-digits)
(put 'totalDigits 'rng-xsd-check 'rng-xsd-check-total-digits)

(defun rng-xsd-check-min-exclusive (str less-than-fun limit convert &rest args)
  (let ((obj (apply convert (cons str args))))
    (and obj
	 (funcall less-than-fun limit obj)
	 obj)))

(defun rng-xsd-check-min-inclusive (str less-than-fun limit convert &rest args)
  (let ((obj (apply convert (cons str args))))
    (and obj
	 (or (funcall less-than-fun limit obj)
	     (equal limit obj))
	 obj)))

(defun rng-xsd-check-max-exclusive (str less-than-fun limit convert &rest args)
  (let ((obj (apply convert (cons str args))))
    (and obj
	 (funcall less-than-fun obj limit)
	 obj)))

(defun rng-xsd-check-max-inclusive (str less-than-fun limit convert &rest args)
  (let ((obj (apply convert (cons str args))))
    (and obj
	 (or (funcall less-than-fun obj limit)
	     (equal obj limit))
	 obj)))

(defun rng-xsd-check-min-length (str length-fun limit convert &rest args)
  (let ((obj (apply convert (cons str args))))
    (and obj
	 (>= (funcall length-fun obj) limit)
	 obj)))

(defun rng-xsd-check-max-length (str length-fun limit convert &rest args)
  (let ((obj (apply convert (cons str args))))
    (and obj
	 (<= (funcall length-fun obj) limit)
	 obj)))

(defun rng-xsd-check-length (str length-fun len convert &rest args)
  (let ((obj (apply convert (cons str args))))
    (and obj
	 (= (funcall length-fun obj) len)
	 obj)))

(defun rng-xsd-check-fraction-digits (str n convert &rest args)
  (let ((obj (apply convert (cons str args))))
    (and obj
	 (<= (length (aref obj 2)) n)
	 obj)))

(defun rng-xsd-check-total-digits (str n convert &rest args)
  (let ((obj (apply convert (cons str args))))
    (and obj
	 (<= (+ (length (aref obj 1))
		(length (aref obj 2)))
	     n)
	 obj)))

(defun rng-xsd-check-pattern (str regexp convert &rest args)
  (and (let ((case-fold-search nil)) (string-match regexp str))
       (apply convert (cons str args))))


(defun rng-xsd-convert-boolean (string)
  (and (string-match "\\`[ \t\n\r]*\\(?:\\(true\\|1\\)\\|false\\|0\\)[ \t\n\r]*\\'" string)
       (if (match-beginning 1) 'true 'false)))

(defun rng-xsd-convert-decimal (string)
  "Convert a string representing a decimal to an object representing it values.
A decimal value is represented by a vector [SIGN INTEGER-DIGITS
FRACTION-DIGITS] where SIGN is 1 or -1, INTEGER-DIGITS is a string
containing zero or more digits, with no leading zero, and
FRACTION-DIGITS is a string containing zero or more digits with no
trailing digits.  For example, -0021.0430 would be represented by [-1
\"21\" \"043\"]."
  (and (string-match "\\`[ \t\n\r]*\\([-+]\\)?\\(0*\\([1-9][0-9]*\\)?\\(\\.\\([0-9]*[1-9]\\)?0*\\)?\\)[ \t\n\r]*\\'" string)
       (let ((digits (match-string 2 string)))
	 (and (not (string= digits "."))
	      (not (string= digits ""))))
       (let ((integer-digits (match-string 3 string)))
	 (vector (if (and (equal (match-string 1 string) "-")
			  ;; Normalize -0 to 0
			  integer-digits)
		     -1
		   1)
		 (or integer-digits "")
		 (or (match-string 5 string) "")))))

(defun rng-xsd-convert-integer (string)
  (and (string-match "\\`[ \t\n\r]*\\([-+]\\)?\\(?:0*\\([1-9][0-9]*\\)\\|0+\\)[ \t\n\r]*\\'" string)
       (let ((integer-digits (match-string 2 string)))
	 (vector (if (and (equal (match-string 1 string) "-")
			  ;; Normalize -0 to 0
			  integer-digits)
		     -1
		   1)
		 (or integer-digits "")
		 ""))))

(defun rng-xsd-decimal< (n1 n2)
  (< (rng-xsd-compare-decimal n1 n2) 0))

(defun rng-xsd-compare-decimal (n1 n2)
  "Return a < 0, 0, > 0 according as n1 < n2, n1 = n2 or n1 > n2."
  (let* ((sign1 (aref n1 0))
	 (sign2 (aref n2 0))
	 (sign (- sign1 sign2)))
    (if (= sign 0)
	(* sign1
	   (let* ((int1 (aref n1 1))
		  (int2 (aref n2 1))
		  (len1 (length int1))
		  (len2 (length int2))
		  (lencmp (- len1 len2)))
	     (if (eq lencmp 0)
		 (if (string= int1 int2)
		     (rng-xsd-strcmp (aref n1 2) (aref n2 2))
		   (rng-xsd-strcmp int1 int2))
	       lencmp)))
      sign)))

(defconst rng-xsd-float-regexp
  (concat "\\`[ \r\n\t]*\\(?:"
	  "\\("
	  "[-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)"
	  "\\(?:[eE][-+]?[0-9]+\\)?"
	  "\\)"
	  "\\|\\(INF\\)"
	  "\\|\\(-INF\\)"
	  "\\|\\(NaN\\)"
	  "\\)[ \r\n\t]*\\'"))

(defun rng-xsd-convert-float (string)
  (cond ((not (string-match rng-xsd-float-regexp string)) nil)
	((match-beginning 1)
	 (float (string-to-number (match-string 1 string))))
	((match-beginning 2) 1.0e+INF)
	((match-beginning 3) -1.0e+INF)
	;; Don't use a NaN float because we want NaN to be equal to NaN
	((match-beginning 4) 'NaN)))

(defun rng-xsd-float< (f1 f2)
  (and (not (eq f1 'NaN))
       (not (eq f2 'NaN))
       (< f1 f2)))

(defun rng-xsd-convert-token (string regexp)
  (and (string-match regexp string)
       (match-string 1 string)))

(defun rng-xsd-convert-hex-binary (string)
  (and (string-match "\\`[ \r\n\t]*\\(\\(?:[0-9A-Fa-f][0-9A-Fa-f]\\)*\\)[ \r\n\t]*\\'"
		     string)
       (downcase (match-string 1 string))))

(defun rng-xsd-hex-binary-length (obj)
  (/ (length obj) 2))

(defconst rng-xsd-base64-binary-regexp
  (let ((S "[ \t\r\n]*")
	(B04 "[AQgw]")
	(B16 "[AEIMQUYcgkosw048]")
	(B64 "[A-Za-z0-9+/]"))
    (concat "\\`" S "\\(?:\\(?:" B64 S "\\)\\{4\\}\\)*"
	    "\\(?:" B64 S B64 S B16 S "=" S
	    "\\|" B64 S B04 S "=" S "=" S "\\)?\\'")))

(defun rng-xsd-convert-base64-binary (string)
  (and (string-match rng-xsd-base64-binary-regexp string)
       (replace-regexp-in-string "[ \t\r\n]+" "" string t t)))

(defun rng-xsd-base64-binary-length (obj)
  (let ((n (* (/ (length obj) 4) 3)))
    (if (and (> n 0)
	     (string= (substring obj -1) "="))
	(- n (if (string= (substring obj -2) "==")
		 2
	       1))
      n)))

(defun rng-xsd-convert-any-uri (string)
  (and (string-match "\\`\\(?:[^%]\\|%[0-9a-fA-F][0-9a-fA-F]\\)?*\\'" string)
       (string-match "\\`[^#]*\\(?:#[^#]*\\)?\\'" string)
       (string-match "\\`\\(?:[a-zA-Z][-+.A-Za-z0-9]*:.+\\|[^:]*\\(?:[#/?].*\\)?\\)\\'" string)
       string))

(defun rng-xsd-make-date-time-regexp (template)
  "Returns a regular expression matching a ISO 8601 date/time.
The template is a string with Y standing for years field, M standing
for months, D standing for day of month, T standing for a literal T, t
standing for time and - standing for a literal hyphen.  A time zone is
always allowed at the end.  Regardless of the fields appearing in the
template, the regular expression will have twelve groups matching the
year sign, year, month, day of month, hours, minutes, integer seconds,
fractional seconds (including leading period), time zone, time zone
sign, time zone hours, time zone minutes."
  (let ((i 0)
	(len (length template))
	(parts nil)
	first last c)
    (while (< i len)
      (setq c (aref template i))
      (setq parts
	    (cons (cond ((eq c ?Y)
			 (setq first 0)
			 (setq last 1)
			 "\\(-\\)?\\(\\(?:[1-9][0-9]*\\)?[0-9]\\{4\\}\\)")
			((eq c ?M)
			 (or first
			     (setq first 2))
			 (setq last 2)
			 "\\([0-9][0-9]\\)")
			((eq c ?D)
			 (or first
			     (setq first 3))
			 (setq last 3)
			 "\\([0-9][0-9]\\)")
			((eq c ?t)
			 (or first
			     (setq first 4))
			 (setq last 7)
			 "\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\(\\.[0-9]*\\)?")
			(t (string c)))
		  parts))
      (setq i (1+ i)))
    (while (< last 7)
      (setq last (1+ last))
      ;; Add dummy fields that can never much but keep the group
      ;; numbers uniform.
      (setq parts (cons "\\(\\'X\\)?" parts)))
    (setq parts (cons "\\(Z\\|\\([-+]\\)\\([0-9][0-9]\\):\\([0-5][0-9]\\)\\)?[ \t\n\r]*\\'"
		      parts))
    (setq parts (cons "\\`[ \t\n\r]*" (nreverse parts)))
    (while (> first 0)
      (setq first (1- first))
      (setq parts (cons "\\(X\\)?" parts)))
    (apply 'concat parts)))

(defconst rng-xsd-seconds-per-day (* 24 60 60))
(defconst rng-xsd-days-in-month [31 28 31 30 31 30 31 31 30 31 30 31])

(defun rng-xsd-days-in-month (year month)
  (if (and (= month 2) (rng-xsd-leap-year-p year))
      29
    (aref rng-xsd-days-in-month (1- month))))

(defconst rng-xsd-months-to-days
  (let ((v (make-vector 12 nil))
	(total 0)
	(i 0))
    (while (< i 12)
      (setq total (+ total (aref rng-xsd-days-in-month i)))
      (aset v i total)
      (setq i (1+ i)))
    v))

(defun rng-xsd-convert-date-time (string regexp)
  "Converts an XML Schema date/time to a list.
Returns nil if invalid.  REGEXP is a regexp for parsing the date time
as returned by `rng-xsd-make-date-time-regexp'.  The list has 4 members
\(HAS-TIME-ZONE DAY SECOND SECOND-FRACTION), where HAS-TIME-ZONE is t
or nil depending on whether a time zone was specified, DAY is an
integer giving a day number (with Jan 1 1AD being day 1), SECOND is the
second within that day, and SECOND-FRACTION is a float giving the
fractional part of the second."
  (and (string-match regexp string)
       (let ((year-sign (match-string 1 string))
	     (year (match-string 2 string))
	     (month (match-string 3 string))
	     (day (match-string 4 string))
	     (hour (match-string 5 string))
	     (minute (match-string 6 string))
	     (second (match-string 7 string))
	     (second-fraction (match-string 8 string))
	     (has-time-zone (match-string 9 string))
	     (time-zone-sign (match-string 10 string))
	     (time-zone-hour (match-string 11 string))
	     (time-zone-minute (match-string 12 string)))
	 (setq year-sign (if year-sign -1 1))
	 (setq year
	       (if year
		   (* year-sign
		      (string-to-number year))
		 2000))
	 (setq month
	       (if month (string-to-number month) 1))
	 (setq day
	       (if day (string-to-number day) 1))
	 (setq hour
	       (if hour (string-to-number hour) 0))
	 (setq minute
	       (if minute (string-to-number minute) 0))
	 (setq second
	       (if second (string-to-number second) 0))
	 (setq second-fraction
	       (if second-fraction
		   (float (string-to-number second-fraction))
		 0.0))
	 (setq has-time-zone (and has-time-zone t))
	 (setq time-zone-sign
	       (if (equal time-zone-sign "-") -1 1))
	 (setq time-zone-hour
	       (if time-zone-hour (string-to-number time-zone-hour) 0))
	 (setq time-zone-minute
	       (if time-zone-minute (string-to-number time-zone-minute) 0))
	 (and (>= month 1)
	      (<= month 12)
	      (>= day 1)
	      (<= day (rng-xsd-days-in-month year month))
	      (<= hour 23)
	      (<= minute 59)
	      (<= second 60)		; leap second
	      (<= time-zone-hour 23)
	      (<= time-zone-minute 59)
	      (cons has-time-zone
		    (rng-xsd-add-seconds
		     (list (rng-xsd-date-to-days year month day)
			   (rng-xsd-time-to-seconds hour minute second)
			   second-fraction)
		     (* (rng-xsd-time-to-seconds time-zone-hour
						 time-zone-minute
						 0)
			(- time-zone-sign))))))))

(defun rng-xsd-leap-year-p (year)
  (and (= (% year 4) 0)
       (or (/= (% year 100) 0)
	   (= (% year 400) 0))))

(defun rng-xsd-time-to-seconds (hour minute second)
  (+ (* (+ (* hour 60)
	   minute)
	60)
     second))

(defconst rng-xsd-max-tz (rng-xsd-time-to-seconds 14 0 0))

(defun rng-xsd-date-time< (dt1 dt2)
  (cond ((eq (car dt1) (car dt2))
	 (rng-xsd-number-list< (cdr dt1) (cdr dt2)))
	((car dt1)
	 (rng-xsd-number-list< (cdr dt1)
			       (rng-xsd-add-seconds (cdr dt2)
						    (- rng-xsd-max-tz))))
	(t
	 (rng-xsd-number-list< (rng-xsd-add-seconds (cdr dt1)
						    rng-xsd-max-tz)
			       (cdr dt2)))))

(defun rng-xsd-add-seconds (date offset)
  (let ((day (nth 0 date))
	(second (+ (nth 1 date) offset))
	(fraction (nth 2 date)))
    (cond ((< second 0)
	   (list (1- day)
		 (+ second rng-xsd-seconds-per-day)
		 fraction))
	  ((>= second rng-xsd-seconds-per-day)
	   (list (1+ day)
		 (- second rng-xsd-seconds-per-day)
		 fraction))
	  (t (list day second fraction)))))

(defun rng-xsd-number-list< (numbers1 numbers2)
  (while (and numbers1 (= (car numbers1) (car numbers2)))
    (setq numbers1 (cdr numbers1))
    (setq numbers2 (cdr numbers2)))
  (and numbers1
       (< (car numbers1) (car numbers2))))

(defun rng-xsd-date-to-days (year month day)
  "Return a unique day number where Jan 1 1 AD is day 1"
  (if (> year 0)			; AD
      (+ (rng-xsd-days-in-years (- year 1))
	 (rng-xsd-day-number-in-year year month day))
    (- (+ (- (rng-xsd-days-in-years (- 3 year))
	     (rng-xsd-days-in-years 3))
	  (- (if (rng-xsd-leap-year-p year) 366 365)
	     (rng-xsd-day-number-in-year year month day))))))

(defun rng-xsd-days-in-years (years)
  "The number of days in YEARS years where the first year is 1AD."
  (+ (* 365 years)
     (/ years 4)
     (- (/ years 100))
     (/ years 400)))

(defun rng-xsd-day-number-in-year (year month day)
  (+ (if (= month 1)
	 0
	 (aref rng-xsd-months-to-days (- month 2)))
     day
     (if (and (> month 2)
	      (rng-xsd-leap-year-p year))
	 1
       0)))

(defconst rng-xsd-duration-regexp
    "\\`[ \t\r\n]*\\(-\\)?P\
\\([0-9]+Y\\)?\\([0-9]+M\\)?\\([0-9]+D\\)?\
\\(?:T\\([0-9]+H\\)?\\([0-9]+M\\)?\
\\(\\([0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)S\\)?\\)?\
[ \t\r\n]*\\'")


(defun rng-xsd-convert-duration (string)
  (and (string-match rng-xsd-duration-regexp string)
       (let ((last (substring string -1)))
	 (not (or (string= last "P")
		  (string= last "T"))))
       ;; years months days hours minutes seconds
       (let ((v (make-vector 6 0))
	     (sign (if (match-beginning 1) -1 1))
	     (i 0))
	 (while (< i 6)
	   (let ((start (match-beginning (+ i 2))))
	     (when start
	       (aset v i (* sign
			    (string-to-number
			     (substring string
					start
					(1- (match-end (+ i 2)))))))))
	   (setq i (1+ i)))
	 ;; Force seconds to be float so that equal works properly.
	 (aset v 5 (float (aref v 5)))
	 v)))

(defconst rng-xsd-min-seconds-per-month (* 28 rng-xsd-seconds-per-day))

(defun rng-xsd-duration< (d1 d2)
  (let* ((months1 (rng-xsd-duration-months d1))
	 (months2 (rng-xsd-duration-months d2))
	 (seconds1 (rng-xsd-duration-seconds d1))
	 (seconds2 (rng-xsd-duration-seconds d2)))
    (cond ((< months1 months2)
	   (if (< (- seconds1 seconds2) rng-xsd-min-seconds-per-month)
	       t
	     (rng-xsd-months-seconds< months1 seconds1 months2 seconds2)))
	  ((> months1 months2)
	   (if (< (- seconds2 seconds1) rng-xsd-min-seconds-per-month)
	       nil
	     (rng-xsd-months-seconds< months1 seconds1 months2 seconds2)))
	  (t (< seconds1 seconds2)))))

(defconst xsd-duration-reference-dates
  '((1696 . 9) (1697 . 2) (1903 . 3) (1903 . 7)))

(defun rng-xsd-months-seconds< (months1 seconds1 months2 seconds2)
  (let ((ret t)
	(ref-dates xsd-duration-reference-dates))
    (while (let* ((ref-date (car ref-dates))
		  (ref-year (car ref-date))
		  (ref-month (cdr ref-date)))
	     (unless (< (+ (rng-xsd-month-seconds months1
						  ref-year
						  ref-month)
			   seconds1)
			(+ (rng-xsd-month-seconds months2
						  ref-year
						  ref-month)
			   seconds2))
		 (setq ret nil))
	     (and ret
		  (setq ref-dates (cdr ref-dates)))))
    ret))


(defun rng-xsd-month-seconds (months ref-year ref-month)
  "Return the seconds in a number of months starting on a reference date.
Returns a floating point number."
  (* (rng-xsd-month-days (abs months) ref-year ref-month)
     (float rng-xsd-seconds-per-day)
     (if (< months 0) -1.0 1.0)))

(defconst rng-xsd-years-per-gregorian-cycle 400)
(defconst rng-xsd-months-per-gregorian-cycle
  (* rng-xsd-years-per-gregorian-cycle 12))
(defconst rng-xsd-leap-years-per-gregorian-cycle (- 100 (- 4 1)))
(defconst rng-xsd-days-per-gregorian-cycle
  (+ (* 365 rng-xsd-years-per-gregorian-cycle)
     rng-xsd-leap-years-per-gregorian-cycle))

(defun rng-xsd-month-days (months ref-year ref-month)
  "Return the days in a number of months starting on a reference date.
MONTHS must be an integer >= 0."
  (let ((days 0))
    (setq months (mod months rng-xsd-months-per-gregorian-cycle))
    ;; This may be rather slow, but it is highly unlikely
    ;; ever to be used in real life.
    (while (> months 0)
      (setq days
	    (+ (rng-xsd-days-in-month ref-year ref-month)
	       days))
      (setq ref-month
	    (if (eq ref-month 12)
		(progn
		  (setq ref-year (1+ ref-year))
		  1)
	      (1+ ref-month)))
      (setq months (1- months)))
    (+ (* (/ months rng-xsd-months-per-gregorian-cycle)
	  rng-xsd-days-per-gregorian-cycle)
       days)))

(defun rng-xsd-duration-months (d)
  (+ (* (aref d 0) 12)
     (aref d 1)))

(defun rng-xsd-duration-seconds (d)
  (+ (* (+ (* (+ (* (aref d 2)
		    24.0)
		 (aref d 3))
	      60.0)
	   (aref d 4))
	60.0)
     (aref d 5)))

(defun rng-xsd-convert-qname (string)
  (and (string-match "\\`[ \r\n\t]*\\([_[:alpha:]][-._[:alnum:]]*\\(:[_[:alpha:]][-._[:alnum:]]*\\)?\\)[ \r\n\t]*\\'" string)
       (let ((colon (match-beginning 2))
	     (context (apply (car rng-dt-namespace-context-getter)
			     (cdr rng-dt-namespace-context-getter))))
	 (if colon
	     (let* ((prefix (substring string
				       (match-beginning 1)
				       colon))
		    (binding (assoc prefix (cdr context))))
	       (and binding
		    (cons (cdr binding)
			  (substring string
				     (1+ colon)
				     (match-end 1)))))
	   (cons (car context)
		 (match-string 1 string))))))

(defun rng-xsd-convert-list (string convert &rest args)
  (let* ((tokens (split-string string "[ \t\n\r]+"))
	 (tem tokens))
    (while tem
      (let ((obj (apply convert
			(cons (car tem) args))))
	(cond (obj
	       (setcar tem obj)
	       (setq tem (cdr tem)))
	      (t
	       (setq tokens nil)
	       (setq tem nil)))))
    ;; Fortuitously this returns nil if the list is empty
    ;; which is what we want since the list types
    ;; have to have one or more members.
    tokens))

(defun rng-xsd-strcmp (s1 s2)
  (cond ((string= s1 s2) 0)
	((string< s1 s2) -1)
	(t 1)))

(put 'string 'rng-xsd-convert '(identity))
(put 'string 'rng-xsd-length 'length)
(put 'string 'rng-xsd-matches-anything t)

(put 'normalizedString 'rng-xsd-convert '(identity))
(put 'normalizedString 'rng-xsd-length 'length)
(put 'normalizedString 'rng-xsd-matches-anything t)

(put 'token 'rng-xsd-convert '(identity))
(put 'token 'rng-xsd-length 'length)
(put 'token 'rng-xsd-matches-anything t)

(put 'hexBinary 'rng-xsd-convert '(rng-xsd-convert-hex-binary))
(put 'hexBinary 'rng-xsd-length 'rng-xsd-hex-binary-length)

(put 'base64Binary 'rng-xsd-convert '(rng-xsd-convert-base64-binary))
(put 'base64Binary 'rng-xsd-length 'rng-xsd-base64-binary-length)

(put 'boolean 'rng-xsd-convert '(rng-xsd-convert-boolean))

(put 'float 'rng-xsd-convert '(rng-xsd-convert-float))
(put 'float 'rng-xsd-less-than 'rng-xsd-float<)

(put 'double 'rng-xsd-convert '(rng-xsd-convert-float))
(put 'double 'rng-xsd-less-than 'rng-xsd-float<)

(put 'decimal 'rng-xsd-convert '(rng-xsd-convert-decimal))
(put 'decimal 'rng-xsd-less-than 'rng-xsd-decimal<)

(put 'integer 'rng-xsd-convert '(rng-xsd-convert-integer))
(put 'integer 'rng-xsd-less-than 'rng-xsd-decimal<)

(defun rng-xsd-def-integer-type (name min max)
  (put name 'rng-xsd-less-than 'rng-xsd-decimal<)
  (put name
       'rng-xsd-convert
       (cdr (rng-xsd-compile 'integer
			     (append (and min `((minInclusive . ,min)))
				     (and max `((maxInclusive . ,max))))))))

(defun rng-xsd-def-token-type (name regexp)
  (put name 'rng-xsd-convert (list 'rng-xsd-convert-token
				   (concat "\\`[\r\n\t ]*\\("
					   regexp
					   "\\)[\r\n\t ]*\\'")))
  (put name 'rng-xsd-length 'length))

(rng-xsd-def-token-type 'NMTOKEN "[-.:_[:alnum:]]+")
(rng-xsd-def-token-type 'Name "[:_[:alpha:]][-.:_[:alnum:]]*")
(rng-xsd-def-token-type 'NCName "[_[:alpha:]][-._[:alnum:]]*")
(rng-xsd-def-token-type 'language
			"[a-zA-Z]\\{1,8\\}\\(?:-[a-zA-Z0-9]\\{1,8\\}\\)*")

(put 'ENTITY 'rng-xsd-convert (get 'NCName 'rng-xsd-convert))
(put 'ENTITY 'rng-xsd-length 'length)
(put 'ID 'rng-xsd-convert (get 'NCName 'rng-xsd-convert))
(put 'ID 'rng-xsd-length 'length)
(put 'IDREF 'rng-xsd-convert (get 'NCName 'rng-xsd-convert))
(put 'IDREF 'rng-xsd-length 'length)

(defun rng-xsd-def-list-type (name member-name)
  (put name 'rng-xsd-convert (cons 'rng-xsd-convert-list
				   (get member-name 'rng-xsd-convert)))
  (put name 'rng-xsd-length 'length))

(rng-xsd-def-list-type 'NMTOKENS 'NMTOKEN)
(rng-xsd-def-list-type 'IDREFS 'IDREF)
(rng-xsd-def-list-type 'ENTITIES 'ENTITY)

(put 'anyURI 'rng-xsd-convert '(rng-xsd-convert-any-uri))
(put 'anyURI 'rng-xsd-length 'length)

(put 'QName 'rng-xsd-convert '(rng-xsd-convert-qname))
(put 'NOTATION 'rng-xsd-convert '(rng-xsd-convert-qname))

(defconst rng-xsd-long-max "9223372036854775807")
(defconst rng-xsd-long-min "-9223372036854775808")
(defconst rng-xsd-int-max "2147483647")
(defconst rng-xsd-int-min "-2147483648")
(defconst rng-xsd-short-max "32767")
(defconst rng-xsd-short-min "-32768")
(defconst rng-xsd-byte-max "127")
(defconst rng-xsd-byte-min "-128")
(defconst rng-xsd-unsigned-long-max "18446744073709551615")
(defconst rng-xsd-unsigned-int-max "4294967295")
(defconst rng-xsd-unsigned-short-max "65535")
(defconst rng-xsd-unsigned-byte-max "255")

(rng-xsd-def-integer-type 'nonNegativeInteger "0" nil)
(rng-xsd-def-integer-type 'positiveInteger "1" nil)
(rng-xsd-def-integer-type 'nonPositiveInteger nil "0")
(rng-xsd-def-integer-type 'negativeInteger nil "-1")
(rng-xsd-def-integer-type 'long rng-xsd-long-min rng-xsd-long-max)
(rng-xsd-def-integer-type 'int rng-xsd-int-min rng-xsd-int-max)
(rng-xsd-def-integer-type 'short rng-xsd-short-min rng-xsd-short-max)
(rng-xsd-def-integer-type 'byte rng-xsd-byte-min rng-xsd-byte-max)
(rng-xsd-def-integer-type 'unsignedLong "0" rng-xsd-unsigned-long-max)
(rng-xsd-def-integer-type 'unsignedInt "0" rng-xsd-unsigned-int-max)
(rng-xsd-def-integer-type 'unsignedShort "0" rng-xsd-unsigned-short-max)
(rng-xsd-def-integer-type 'unsignedByte "0" rng-xsd-unsigned-byte-max)

(defun rng-xsd-def-date-time-type (name template)
  (put name 'rng-xsd-convert (list 'rng-xsd-convert-date-time
				   (rng-xsd-make-date-time-regexp template)))
  (put name 'rng-xsd-less-than 'rng-xsd-date-time<))

(rng-xsd-def-date-time-type 'dateTime "Y-M-DTt")
(rng-xsd-def-date-time-type 'time "t")
(rng-xsd-def-date-time-type 'date "Y-M-D")
(rng-xsd-def-date-time-type 'gYearMonth "Y-M")
(rng-xsd-def-date-time-type 'gYear "Y")
(rng-xsd-def-date-time-type 'gMonthDay "--M-D")
(rng-xsd-def-date-time-type 'gDay "---D")
(rng-xsd-def-date-time-type 'gMonth "--M")

(put 'duration 'rng-xsd-convert '(rng-xsd-convert-duration))
(put 'duration 'rng-xsd-less-than 'rng-xsd-duration<)

(provide 'rng-xsd)

;;; rng-xsd.el ends here
