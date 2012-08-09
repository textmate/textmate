;;; calc-frac.el --- fraction functions for Calc

;; Copyright (C) 1990-1993, 2001-2012 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger <jay.p.belanger@gmail.com>

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

;;; Code:

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

(defun calc-fdiv (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op ":" 'calcFunc-fdiv arg 1)))


(defun calc-fraction (arg)
  (interactive "P")
  (calc-slow-wrapper
   (let ((func (if (calc-is-hyperbolic) 'calcFunc-frac 'calcFunc-pfrac)))
     (if (eq arg 0)
	 (calc-enter-result 2 "frac" (list func
					   (calc-top-n 2)
					   (calc-top-n 1)))
       (calc-enter-result 1 "frac" (list func
					 (calc-top-n 1)
					 (prefix-numeric-value (or arg 0))))))))


(defun calc-over-notation (fmt)
  (interactive "sFraction separator: ")
  (calc-wrapper
   (if (string-match "\\`\\([^ 0-9][^ 0-9]?\\)[0-9]*\\'" fmt)
       (let ((n nil))
	 (if (/= (match-end 0) (match-end 1))
	     (setq n (string-to-number (substring fmt (match-end 1)))
		   fmt (math-match-substring fmt 1)))
	 (if (eq n 0) (error "Bad denominator"))
	 (calc-change-mode 'calc-frac-format (list fmt n) t))
     (error "Bad fraction separator format"))))

(defun calc-slash-notation (n)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-frac-format (if n '("//" nil) '("/" nil)) t)))


(defun calc-frac-mode (n)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-prefer-frac n nil t)
   (message (if calc-prefer-frac
		"Integer division will now generate fractions"
	      "Integer division will now generate floating-point results"))))


;;;; Fractions.

;;; Build a normalized fraction.  [R I I]
;;; (This could probably be implemented more efficiently than using
;;;  the plain gcd algorithm.)
(defun math-make-frac (num den)
  (if (Math-integer-negp den)
      (setq num (math-neg num)
	    den (math-neg den)))
  (let ((gcd (math-gcd num den)))
    (if (eq gcd 1)
	(if (eq den 1)
	    num
	  (list 'frac num den))
      (if (equal gcd den)
	  (math-quotient num gcd)
	(list 'frac (math-quotient num gcd) (math-quotient den gcd))))))

(defun calc-add-fractions (a b)
  (if (eq (car-safe a) 'frac)
      (if (eq (car-safe b) 'frac)
	  (math-make-frac (math-add (math-mul (nth 1 a) (nth 2 b))
				    (math-mul (nth 2 a) (nth 1 b)))
			  (math-mul (nth 2 a) (nth 2 b)))
	(math-make-frac (math-add (nth 1 a)
				  (math-mul (nth 2 a) b))
			(nth 2 a)))
    (math-make-frac (math-add (math-mul a (nth 2 b))
			      (nth 1 b))
		    (nth 2 b))))

(defun calc-mul-fractions (a b)
  (if (eq (car-safe a) 'frac)
      (if (eq (car-safe b) 'frac)
	  (math-make-frac (math-mul (nth 1 a) (nth 1 b))
			  (math-mul (nth 2 a) (nth 2 b)))
	(math-make-frac (math-mul (nth 1 a) b)
			(nth 2 a)))
    (math-make-frac (math-mul a (nth 1 b))
		    (nth 2 b))))

(defun calc-div-fractions (a b)
  (if (eq (car-safe a) 'frac)
      (if (eq (car-safe b) 'frac)
	  (math-make-frac (math-mul (nth 1 a) (nth 2 b))
			  (math-mul (nth 2 a) (nth 1 b)))
	(math-make-frac (nth 1 a)
			(math-mul (nth 2 a) b)))
    (math-make-frac (math-mul a (nth 2 b))
		    (nth 1 b))))


;;; Convert a real value to fractional form.  [T R I; T R F] [Public]
(defun calcFunc-frac (a &optional tol)
  (or tol (setq tol 0))
  (cond ((Math-ratp a)
	 a)
	((memq (car a) '(cplx polar vec hms date sdev intv mod))
	 (cons (car a) (mapcar (function
				(lambda (x)
				  (calcFunc-frac x tol)))
			       (cdr a))))
	((Math-messy-integerp a)
	 (math-trunc a))
	((Math-negp a)
	 (math-neg (calcFunc-frac (math-neg a) tol)))
	((not (eq (car a) 'float))
	 (if (math-infinitep a)
	     a
	   (if (math-provably-integerp a)
	       a
	     (math-reject-arg a 'numberp))))
	((integerp tol)
	 (if (<= tol 0)
	     (setq tol (+ tol calc-internal-prec)))
	 (calcFunc-frac a (list 'float 5
				(- (+ (math-numdigs (nth 1 a))
				      (nth 2 a))
				   (1+ tol)))))
	((not (eq (car tol) 'float))
	 (if (Math-realp tol)
	     (calcFunc-frac a (math-float tol))
	   (math-reject-arg tol 'realp)))
	((Math-negp tol)
	 (calcFunc-frac a (math-neg tol)))
	((Math-zerop tol)
	 (calcFunc-frac a 0))
	((not (math-lessp-float tol '(float 1 0)))
	 (math-trunc a))
	((Math-zerop a)
	 0)
	(t
	 (let ((cfrac (math-continued-fraction a tol))
	       (calc-prefer-frac t))
	   (math-eval-continued-fraction cfrac)))))

(defun math-continued-fraction (a tol)
  (let ((calc-internal-prec (+ calc-internal-prec 2)))
    (let ((cfrac nil)
	  (aa a)
	  (calc-prefer-frac nil)
	  int)
      (while (or (null cfrac)
		 (and (not (Math-zerop aa))
		      (not (math-lessp-float
			    (math-abs
			     (math-sub a
				       (let ((f (math-eval-continued-fraction
						 cfrac)))
					 (math-working "Fractionalize" f)
					 f)))
			    tol))))
	(setq int (math-trunc aa)
	      aa (math-sub aa int)
	      cfrac (cons int cfrac))
	(or (Math-zerop aa)
	    (setq aa (math-div 1 aa))))
      cfrac)))

(defun math-eval-continued-fraction (cf)
  (let ((n (car cf))
	(d 1)
	temp)
    (while (setq cf (cdr cf))
      (setq temp (math-add (math-mul (car cf) n) d)
	    d n
	    n temp))
    (math-div n d)))

(defun calcFunc-fdiv (a b)   ; [R I I] [Public]
  (cond
   ((Math-num-integerp a)
    (cond 
     ((Math-num-integerp b)
      (if (Math-zerop b)
	  (math-reject-arg a "*Division by zero")
	(math-make-frac (math-trunc a) (math-trunc b))))
     ((eq (car-safe b) 'frac)
      (if (Math-zerop (nth 1 b))
	  (math-reject-arg a "*Division by zero")
	(math-make-frac (math-mul (math-trunc a) (nth 2 b)) (nth 1 b))))
     (t (math-reject-arg b 'integerp))))
   ((eq (car-safe a) 'frac)
    (cond 
     ((Math-num-integerp b)
      (if (Math-zerop b)
	  (math-reject-arg a "*Division by zero")
	(math-make-frac (cadr a) (math-mul (nth 2 a) (math-trunc b)))))
     ((eq (car-safe b) 'frac)
      (if (Math-zerop (nth 1 b))
	  (math-reject-arg a "*Division by zero")
	(math-make-frac (math-mul (nth 1 a) (nth 2 b)) (math-mul (nth 2 a) (nth 1 b)))))
     (t (math-reject-arg b 'integerp))))
   (t 
    (math-reject-arg a 'integerp))))

(provide 'calc-frac)

;;; calc-frac.el ends here
