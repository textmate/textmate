;;; calc-math.el --- mathematical functions for Calc

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


;;; Find out how many 9s in 9.9999... will give distinct Emacs floats,
;;; then back off by one.

(defvar math-emacs-precision
  (let* ((n 1)
         (x 9)
         (xx (+ x (* 9 (expt 10 (- n))))))
    (while (/= x xx)
      (progn
        (setq n (1+ n))
        (setq x xx)
        (setq xx (+ x (* 9 (expt 10 (- n)))))))
    (1- n))
  "The number of digits in an Emacs float.")

;;; Find the largest power of 10 which is an Emacs float, 
;;; then back off by one so that any float d.dddd...eN 
;;; is an Emacs float, for acceptable d.dddd....

(defvar math-largest-emacs-expt
  (let ((x 1)
        (pow 1e2))
    ;; The following loop is for efficiency; it should stop when 
    ;; 10^(2x) is too large.  This could be indicated by a range 
    ;; error when computing 10^(2x) or an infinite value for 10^(2x).
    (while (and
            pow
            (< pow 1.0e+INF))
      (setq x (* 2 x))
      (setq pow (condition-case nil
                    (expt 10.0 (* 2 x))
                  (error nil))))
    ;; The following loop should stop when 10^(x+1) is too large.
    (setq pow (condition-case nil
                    (expt 10.0 (1+ x))
                  (error nil)))
    (while (and
            pow
            (< pow 1.0e+INF))
      (setq x (1+ x))
      (setq pow (condition-case nil
                    (expt 10.0 (1+ x))
                  (error nil))))
    (1- x))
  "The largest exponent which Calc will convert to an Emacs float.")

(defvar math-smallest-emacs-expt
  (let ((x -1))
    (while (condition-case nil
               (> (expt 10.0 x) 0.0)
             (error nil))
      (setq x (* 2 x)))
    (setq x (/ x 2))
    (while (condition-case nil
               (> (expt 10.0 x) 0.0)
             (error nil))
      (setq x (1- x)))
    (+ x 2))
    "The smallest exponent which Calc will convert to an Emacs float.")

(defun math-use-emacs-fn (fn x)
  "Use the native Emacs function FN to evaluate the Calc number X.
If this can't be done, return NIL."
  (and
   (<= calc-internal-prec math-emacs-precision)
   (math-realp x)
   (let* ((fx (math-float x))
          (xpon (+ (nth 2 x) (1- (math-numdigs (nth 1 x))))))
     (and (<= math-smallest-emacs-expt xpon)
          (<= xpon math-largest-emacs-expt)
          (condition-case nil
              (math-read-number
               (number-to-string
                (funcall fn 
			 (string-to-number 
			  (let 
                              ((calc-number-radix 10)
                               (calc-twos-complement-mode nil)
                               (calc-float-format (list 'float calc-internal-prec))
                               (calc-group-digits nil)
                               (calc-point-char "."))
			    (math-format-number (math-float x)))))))
            (error nil))))))

(defun calc-sqrt (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-unary-op "^2" 'calcFunc-sqr arg)
     (calc-unary-op "sqrt" 'calcFunc-sqrt arg))))

(defun calc-isqrt (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-unary-op "^2" 'calcFunc-sqr arg)
     (calc-unary-op "isqt" 'calcFunc-isqrt arg))))


(defun calc-hypot (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "hypt" 'calcFunc-hypot arg)))

(defun calc-ln (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-exp arg))

(defun calc-log10 (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-ln arg))

(defun calc-log (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-binary-op "alog" 'calcFunc-alog arg)
     (calc-binary-op "log" 'calcFunc-log arg))))

(defun calc-ilog (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-binary-op "alog" 'calcFunc-alog arg)
     (calc-binary-op "ilog" 'calcFunc-ilog arg))))

(defun calc-lnp1 (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-expm1 arg))

(defun calc-exp (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (if (calc-is-inverse)
	   (calc-unary-op "lg10" 'calcFunc-log10 arg)
	 (calc-unary-op "10^" 'calcFunc-exp10 arg))
     (if (calc-is-inverse)
	 (calc-unary-op "ln" 'calcFunc-ln arg)
       (calc-unary-op "exp" 'calcFunc-exp arg)))))

(defun calc-expm1 (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-unary-op "ln+1" 'calcFunc-lnp1 arg)
     (calc-unary-op "ex-1" 'calcFunc-expm1 arg))))

(defun calc-pi ()
  (interactive)
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (if (calc-is-hyperbolic)
	   (if calc-symbolic-mode
	       (calc-pop-push-record 0 "phi" '(var phi var-phi))
	     (calc-pop-push-record 0 "phi" (math-phi)))
	 (if calc-symbolic-mode
	     (calc-pop-push-record 0 "gmma" '(var gamma var-gamma))
	   (calc-pop-push-record 0 "gmma" (math-gamma-const))))
     (if (calc-is-hyperbolic)
	 (if calc-symbolic-mode
	     (calc-pop-push-record 0 "e" '(var e var-e))
	   (calc-pop-push-record 0 "e" (math-e)))
       (if calc-symbolic-mode
	   (calc-pop-push-record 0 "pi" '(var pi var-pi))
	 (calc-pop-push-record 0 "pi" (math-pi)))))))

(defun calc-sin (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (if (calc-is-inverse)
	   (calc-unary-op "asnh" 'calcFunc-arcsinh arg)
	 (calc-unary-op "sinh" 'calcFunc-sinh arg))
     (if (calc-is-inverse)
	 (calc-unary-op "asin" 'calcFunc-arcsin arg)
       (calc-unary-op "sin" 'calcFunc-sin arg)))))

(defun calc-arcsin (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-sin arg))

(defun calc-sinh (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-sin arg))

(defun calc-arcsinh (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-hyperbolic-func)
  (calc-sin arg))

(defun calc-sec (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-unary-op "sech" 'calcFunc-sech arg)
     (calc-unary-op "sec" 'calcFunc-sec arg))))

(defun calc-sech (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-sec arg))

(defun calc-cos (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (if (calc-is-inverse)
	   (calc-unary-op "acsh" 'calcFunc-arccosh arg)
	 (calc-unary-op "cosh" 'calcFunc-cosh arg))
     (if (calc-is-inverse)
	 (calc-unary-op "acos" 'calcFunc-arccos arg)
       (calc-unary-op "cos" 'calcFunc-cos arg)))))

(defun calc-arccos (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-cos arg))

(defun calc-cosh (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-cos arg))

(defun calc-arccosh (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-hyperbolic-func)
  (calc-cos arg))

(defun calc-csc (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-unary-op "csch" 'calcFunc-csch arg)
     (calc-unary-op "csc" 'calcFunc-csc arg))))

(defun calc-csch (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-csc arg))

(defun calc-sincos ()
  (interactive)
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-enter-result 1 "asnc" (list 'calcFunc-arcsincos (calc-top-n 1)))
     (calc-enter-result 1 "sncs" (list 'calcFunc-sincos (calc-top-n 1))))))

(defun calc-tan (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (if (calc-is-inverse)
	   (calc-unary-op "atnh" 'calcFunc-arctanh arg)
	 (calc-unary-op "tanh" 'calcFunc-tanh arg))
     (if (calc-is-inverse)
	 (calc-unary-op "atan" 'calcFunc-arctan arg)
       (calc-unary-op "tan" 'calcFunc-tan arg)))))

(defun calc-arctan (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-tan arg))

(defun calc-tanh (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-tan arg))

(defun calc-arctanh (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-hyperbolic-func)
  (calc-tan arg))

(defun calc-cot (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-unary-op "coth" 'calcFunc-coth arg)
     (calc-unary-op "cot" 'calcFunc-cot arg))))

(defun calc-coth (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-cot arg))

(defun calc-arctan2 ()
  (interactive)
  (calc-slow-wrapper
   (calc-enter-result 2 "atn2" (cons 'calcFunc-arctan2 (calc-top-list-n 2)))))

(defun calc-conj (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "conj" 'calcFunc-conj arg)))

(defun calc-imaginary ()
  (interactive)
  (calc-slow-wrapper
   (calc-pop-push-record 1 "i*" (math-imaginary (calc-top-n 1)))))

(defun calc-to-degrees (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op ">deg" 'calcFunc-deg arg)))

(defun calc-to-radians (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op ">rad" 'calcFunc-rad arg)))


(defun calc-degrees-mode (arg)
  (interactive "p")
  (cond ((= arg 1)
	 (calc-wrapper
	  (calc-change-mode 'calc-angle-mode 'deg)
	  (message "Angles measured in degrees")))
	((= arg 2) (calc-radians-mode))
	((= arg 3) (calc-hms-mode))
	(t (error "Prefix argument out of range"))))

(defun calc-radians-mode ()
  (interactive)
  (calc-wrapper
   (calc-change-mode 'calc-angle-mode 'rad)
   (message "Angles measured in radians")))


;;; Compute the integer square-root floor(sqrt(A)).  A > 0.  [I I] [Public]
;;; This method takes advantage of the fact that Newton's method starting
;;; with an overestimate always works, even using truncating integer division!
(defun math-isqrt (a)
  (cond ((Math-zerop a) a)
	((not (math-natnump a))
	 (math-reject-arg a 'natnump))
	((integerp a)
	 (math-isqrt-small a))
	(t
	 (math-normalize (cons 'bigpos (cdr (math-isqrt-bignum (cdr a))))))))

(defun calcFunc-isqrt (a)
  (if (math-realp a)
      (math-isqrt (math-floor a))
    (math-floor (math-sqrt a))))


;;; This returns (flag . result) where the flag is t if A is a perfect square.
(defun math-isqrt-bignum (a)   ; [P.l L]
  (let ((len (length a)))
    (if (= (% len 2) 0)
	(let* ((top (nthcdr (- len 2) a)))
	  (math-isqrt-bignum-iter
	   a
	   (math-scale-bignum-digit-size
	    (math-bignum-big
	     (1+ (math-isqrt-small
		  (+ (* (nth 1 top) math-bignum-digit-size) (car top)))))
	    (1- (/ len 2)))))
      (let* ((top (nth (1- len) a)))
	(math-isqrt-bignum-iter
	 a
	 (math-scale-bignum-digit-size
	  (list (1+ (math-isqrt-small top)))
	  (/ len 2)))))))

(defun math-isqrt-bignum-iter (a guess)   ; [l L l]
  (math-working "isqrt" (cons 'bigpos guess))
  (let* ((q (math-div-bignum a guess))
	 (s (math-add-bignum (car q) guess))
	 (g2 (math-div2-bignum s))
	 (comp (math-compare-bignum g2 guess)))
    (if (< comp 0)
	(math-isqrt-bignum-iter a g2)
      (cons (and (= comp 0)
		 (math-zerop-bignum (cdr q))
		 (= (% (car s) 2) 0))
	    guess))))

(defun math-zerop-bignum (a)
  (and (eq (car a) 0)
       (progn
	 (while (eq (car (setq a (cdr a))) 0))
	 (null a))))

(defun math-scale-bignum-digit-size (a n)   ; [L L S]
  (while (> n 0)
    (setq a (cons 0 a)
	  n (1- n)))
  a)

(defun math-isqrt-small (a)   ; A > 0.  [S S]
  (let ((g (cond ((>= a 1000000) 10000)
                 ((>= a 10000) 1000)
		 ((>= a 100) 100)
		 (t 10)))
	g2)
    (while (< (setq g2 (/ (+ g (/ a g)) 2)) g)
      (setq g g2))
    g))




;;; Compute the square root of a number.
;;; [T N] if possible, else [F N] if possible, else [C N].  [Public]
(defun math-sqrt (a)
  (or
   (and (Math-zerop a) a)
   (and (math-known-nonposp a)
	(math-imaginary (math-sqrt (math-neg a))))
   (and (integerp a)
	(let ((sqrt (math-isqrt-small a)))
	  (if (= (* sqrt sqrt) a)
	      sqrt
	    (if calc-symbolic-mode
		(list 'calcFunc-sqrt a)
	      (math-sqrt-float (math-float a) (math-float sqrt))))))
   (and (eq (car-safe a) 'bigpos)
	(let* ((res (math-isqrt-bignum (cdr a)))
	       (sqrt (math-normalize (cons 'bigpos (cdr res)))))
	  (if (car res)
	      sqrt
	    (if calc-symbolic-mode
		(list 'calcFunc-sqrt a)
	      (math-sqrt-float (math-float a) (math-float sqrt))))))
   (and (eq (car-safe a) 'frac)
	(let* ((num-res (math-isqrt-bignum (cdr (Math-bignum-test (nth 1 a)))))
	       (num-sqrt (math-normalize (cons 'bigpos (cdr num-res))))
	       (den-res (math-isqrt-bignum (cdr (Math-bignum-test (nth 2 a)))))
	       (den-sqrt (math-normalize (cons 'bigpos (cdr den-res)))))
	  (if (and (car num-res) (car den-res))
	      (list 'frac num-sqrt den-sqrt)
	    (if calc-symbolic-mode
		(if (or (car num-res) (car den-res))
		    (math-div (if (car num-res)
				  num-sqrt (list 'calcFunc-sqrt (nth 1 a)))
			      (if (car den-res)
				  den-sqrt (list 'calcFunc-sqrt (nth 2 a))))
		  (list 'calcFunc-sqrt a))
	      (math-sqrt-float (math-float a)
			       (math-div (math-float num-sqrt) den-sqrt))))))
   (and (eq (car-safe a) 'float)
	(if calc-symbolic-mode
	    (if (= (% (nth 2 a) 2) 0)
		(let ((res (math-isqrt-bignum
			    (cdr (Math-bignum-test (nth 1 a))))))
		  (if (car res)
		      (math-make-float (math-normalize
					(cons 'bigpos (cdr res)))
				       (/ (nth 2 a) 2))
		    (signal 'inexact-result nil)))
	      (signal 'inexact-result nil))
	  (math-sqrt-float a)))
   (and (eq (car-safe a) 'cplx)
	(math-with-extra-prec 2
	  (let* ((d (math-abs a))
		 (imag (math-sqrt (math-mul (math-sub d (nth 1 a))
					    '(float 5 -1)))))
	    (list 'cplx
		  (math-sqrt (math-mul (math-add d (nth 1 a)) '(float 5 -1)))
		  (if (math-negp (nth 2 a)) (math-neg imag) imag)))))
   (and (eq (car-safe a) 'polar)
	(list 'polar
	      (math-sqrt (nth 1 a))
	      (math-mul (nth 2 a) '(float 5 -1))))
   (and (eq (car-safe a) 'sdev)
	(let ((sqrt (math-sqrt (nth 1 a))))
	  (math-make-sdev sqrt
			  (math-div (nth 2 a) (math-mul sqrt 2)))))
   (and (eq (car-safe a) 'intv)
	(not (math-negp (nth 2 a)))
	(math-make-intv (nth 1 a) (math-sqrt (nth 2 a)) (math-sqrt (nth 3 a))))
   (and (eq (car-safe a) '*)
	(or (math-known-nonnegp (nth 1 a))
	    (math-known-nonnegp (nth 2 a)))
	(math-mul (math-sqrt (nth 1 a)) (math-sqrt (nth 2 a))))
   (and (eq (car-safe a) '/)
	(or (and (math-known-nonnegp (nth 2 a))
		 (math-div (math-sqrt (nth 1 a)) (math-sqrt (nth 2 a))))
	    (and (math-known-nonnegp (nth 1 a))
		 (not (math-equal-int (nth 1 a) 1))
		 (math-mul (math-sqrt (nth 1 a))
			   (math-sqrt (math-div 1 (nth 2 a)))))))
   (and (eq (car-safe a) '^)
	(math-known-evenp (nth 2 a))
	(math-known-realp (nth 1 a))
	(math-abs (math-pow (nth 1 a) (math-div (nth 2 a) 2))))
   (let ((inf (math-infinitep a)))
     (and inf
	  (math-mul (math-sqrt (math-infinite-dir a inf)) inf)))
   (progn
     (calc-record-why 'numberp a)
     (list 'calcFunc-sqrt a))))
(defalias 'calcFunc-sqrt 'math-sqrt)

(defun math-infinite-dir (a &optional inf)
  (or inf (setq inf (math-infinitep a)))
  (math-normalize (math-expr-subst a inf 1)))

(defun math-sqrt-float (a &optional guess)   ; [F F F]
  (if calc-symbolic-mode
      (signal 'inexact-result nil)
    (math-with-extra-prec 1 (math-sqrt-raw a guess))))

(defun math-sqrt-raw (a &optional guess)   ; [F F F]
  (if (not (Math-posp a))
      (math-sqrt a)
    (cond
     ((math-use-emacs-fn 'sqrt a))
     (t
      (if (null guess)
          (let ((ldiff (- (math-numdigs (nth 1 a)) 6)))
            (or (= (% (+ (nth 2 a) ldiff) 2) 0) (setq ldiff (1+ ldiff)))
            (setq guess (math-make-float (math-isqrt-small
                                          (math-scale-int (nth 1 a) (- ldiff)))
                                         (/ (+ (nth 2 a) ldiff) 2)))))
      (math-sqrt-float-iter a guess)))))

(defun math-sqrt-float-iter (a guess)   ; [F F F]
  (math-working "sqrt" guess)
  (let ((g2 (math-mul-float (math-add-float guess (math-div-float a guess))
			    '(float 5 -1))))
     (if (math-nearly-equal-float g2 guess)
	 g2
       (math-sqrt-float-iter a g2))))

;;; True if A and B differ only in the last digit of precision.  [P F F]
(defun math-nearly-equal-float (a b)
  (let ((ediff (- (nth 2 a) (nth 2 b))))
    (cond ((= ediff 0)   ;; Expanded out for speed
	   (setq ediff (math-add (Math-integer-neg (nth 1 a)) (nth 1 b)))
	   (or (eq ediff 0)
	       (and (not (consp ediff))
		    (< ediff 10)
		    (> ediff -10)
		    (= (math-numdigs (nth 1 a)) calc-internal-prec))))
	  ((= ediff 1)
	   (setq ediff (math-add (Math-integer-neg (nth 1 b))
				 (math-scale-int (nth 1 a) 1)))
	   (and (not (consp ediff))
		(< ediff 10)
		(> ediff -10)
		(= (math-numdigs (nth 1 b)) calc-internal-prec)))
	  ((= ediff -1)
	   (setq ediff (math-add (Math-integer-neg (nth 1 a))
				 (math-scale-int (nth 1 b) 1)))
	   (and (not (consp ediff))
		(< ediff 10)
		(> ediff -10)
		(= (math-numdigs (nth 1 a)) calc-internal-prec))))))

(defun math-nearly-equal (a b)   ;  [P N N] [Public]
  (setq a (math-float a))
  (setq b (math-float b))
  (if (eq (car a) 'polar) (setq a (math-complex a)))
  (if (eq (car b) 'polar) (setq b (math-complex b)))
  (if (eq (car a) 'cplx)
      (if (eq (car b) 'cplx)
	  (and (or (math-nearly-equal-float (nth 1 a) (nth 1 b))
		   (and (math-nearly-zerop-float (nth 1 a) (nth 2 a))
			(math-nearly-zerop-float (nth 1 b) (nth 2 b))))
	       (or (math-nearly-equal-float (nth 2 a) (nth 2 b))
		   (and (math-nearly-zerop-float (nth 2 a) (nth 1 a))
			(math-nearly-zerop-float (nth 2 b) (nth 1 b)))))
	(and (math-nearly-equal-float (nth 1 a) b)
	     (math-nearly-zerop-float (nth 2 a) b)))
      (if (eq (car b) 'cplx)
	  (and (math-nearly-equal-float a (nth 1 b))
	       (math-nearly-zerop-float a (nth 2 b)))
	(math-nearly-equal-float a b))))

;;; True if A is nearly zero compared to B.  [P F F]
(defun math-nearly-zerop-float (a b)
  (or (eq (nth 1 a) 0)
      (<= (+ (math-numdigs (nth 1 a)) (nth 2 a))
	  (1+ (- (+ (math-numdigs (nth 1 b)) (nth 2 b)) calc-internal-prec)))))

(defun math-nearly-zerop (a b)   ; [P N R] [Public]
  (setq a (math-float a))
  (setq b (math-float b))
  (if (eq (car a) 'cplx)
      (and (math-nearly-zerop-float (nth 1 a) b)
	   (math-nearly-zerop-float (nth 2 a) b))
    (if (eq (car a) 'polar)
	(math-nearly-zerop-float (nth 1 a) b)
      (math-nearly-zerop-float a b))))

;;; This implementation could be improved, accuracy-wise.
(defun math-hypot (a b)
  (cond ((Math-zerop a) (math-abs b))
	((Math-zerop b) (math-abs a))
	((not (Math-scalarp a))
	 (if (math-infinitep a)
	     (if (math-infinitep b)
		 (if (equal a b)
		     a
		   '(var nan var-nan))
	       a)
	   (calc-record-why 'scalarp a)
	   (list 'calcFunc-hypot a b)))
	((not (Math-scalarp b))
	 (if (math-infinitep b)
	     b
	   (calc-record-why 'scalarp b)
	   (list 'calcFunc-hypot a b)))
	((and (Math-numberp a) (Math-numberp b))
	 (math-with-extra-prec 1
	   (math-sqrt (math-add (calcFunc-abssqr a) (calcFunc-abssqr b)))))
	((eq (car-safe a) 'hms)
	 (if (eq (car-safe b) 'hms)   ; this helps sdev's of hms forms
	     (math-to-hms (math-hypot (math-from-hms a 'deg)
				      (math-from-hms b 'deg)))
	   (math-to-hms (math-hypot (math-from-hms a 'deg) b))))
	((eq (car-safe b) 'hms)
	 (math-to-hms (math-hypot a (math-from-hms b 'deg))))
	(t nil)))
(defalias 'calcFunc-hypot 'math-hypot)

(defun calcFunc-sqr (x)
  (math-pow x 2))



(defun math-nth-root (a n)
  (cond ((= n 2) (math-sqrt a))
	((Math-zerop a) a)
	((Math-negp a) nil)
	((Math-integerp a)
	 (let ((root (math-nth-root-integer a n)))
	   (if (car root)
	       (cdr root)
	     (and (not calc-symbolic-mode)
		  (math-nth-root-float (math-float a) n
				       (math-float (cdr root)))))))
	((eq (car-safe a) 'frac)
	 (let* ((num-root (math-nth-root-integer (nth 1 a) n))
		(den-root (math-nth-root-integer (nth 2 a) n)))
	   (if (and (car num-root) (car den-root))
	       (list 'frac (cdr num-root) (cdr den-root))
	     (and (not calc-symbolic-mode)
		  (math-nth-root-float
		   (math-float a) n
		   (math-div-float (math-float (cdr num-root))
				   (math-float (cdr den-root))))))))
	((eq (car-safe a) 'float)
	 (and (not calc-symbolic-mode)
	      (math-nth-root-float a n)))
	((eq (car-safe a) 'polar)
	 (let ((root (math-nth-root (nth 1 a) n)))
	   (and root (list 'polar root (math-div (nth 2 a) n)))))
	(t nil)))

;; The variables math-nrf-n, math-nrf-nf and math-nrf-nfm1 are local
;; to math-nth-root-float, but are used by math-nth-root-float-iter,
;; which is called by math-nth-root-float.
(defvar math-nrf-n)
(defvar math-nrf-nf)
(defvar math-nrf-nfm1)

(defun math-nth-root-float (a math-nrf-n &optional guess)
  (math-inexact-result)
  (math-with-extra-prec 1
    (let ((math-nrf-nf (math-float math-nrf-n))
	  (math-nrf-nfm1 (math-float (1- math-nrf-n))))
      (math-nth-root-float-iter a (or guess
				      (math-make-float
				       1 (/ (+ (math-numdigs (nth 1 a))
					       (nth 2 a)
					       (/ math-nrf-n 2))
					    math-nrf-n)))))))

(defun math-nth-root-float-iter (a guess)
  (math-working "root" guess)
  (let ((g2 (math-div-float (math-add-float (math-mul math-nrf-nfm1 guess)
					    (math-div-float
					     a (math-ipow guess (1- math-nrf-n))))
			    math-nrf-nf)))
    (if (math-nearly-equal-float g2 guess)
	g2
      (math-nth-root-float-iter a g2))))

;; The variable math-nri-n is local to math-nth-root-integer, but
;; is used by math-nth-root-int-iter, which is called by
;; math-nth-root-int.
(defvar math-nri-n)

(defun math-nth-root-integer (a math-nri-n &optional guess)   ; [I I S]
  (math-nth-root-int-iter a (or guess
				(math-scale-int 1 (/ (+ (math-numdigs a)
							(1- math-nri-n))
						     math-nri-n)))))

(defun math-nth-root-int-iter (a guess)
  (math-working "root" guess)
  (let* ((q (math-idivmod a (math-ipow guess (1- math-nri-n))))
	 (s (math-add (car q) (math-mul (1- math-nri-n) guess)))
	 (g2 (math-idivmod s math-nri-n)))
    (if (Math-natnum-lessp (car g2) guess)
	(math-nth-root-int-iter a (car g2))
      (cons (and (equal (car g2) guess)
		 (eq (cdr q) 0)
		 (eq (cdr g2) 0))
	    guess))))

(defun calcFunc-nroot (x n)
  (calcFunc-pow x (if (integerp n)
		      (math-make-frac 1 n)
		    (math-div 1 n))))




;;;; Transcendental functions.

;;; All of these functions are defined on the complex plane.
;;; (Branch cuts, etc. follow Steele's Common Lisp book.)

;;; Most functions increase calc-internal-prec by 2 digits, then round
;;; down afterward.  "-raw" functions use the current precision, require
;;; their arguments to be in float (or complex float) format, and always
;;; work in radians (where applicable).

(defun math-to-radians (a)   ; [N N]
  (cond ((eq (car-safe a) 'hms)
	 (math-from-hms a 'rad))
	((memq calc-angle-mode '(deg hms))
	 (math-mul a (math-pi-over-180)))
	(t a)))

(defun math-from-radians (a)   ; [N N]
  (cond ((eq calc-angle-mode 'deg)
	 (if (math-constp a)
	     (math-div a (math-pi-over-180))
	   (list 'calcFunc-deg a)))
	((eq calc-angle-mode 'hms)
	 (math-to-hms a 'rad))
	(t a)))

(defun math-to-radians-2 (a)   ; [N N]
  (cond ((eq (car-safe a) 'hms)
	 (math-from-hms a 'rad))
	((memq calc-angle-mode '(deg hms))
	 (if calc-symbolic-mode
	     (math-div (math-mul a '(var pi var-pi)) 180)
	   (math-mul a (math-pi-over-180))))
	(t a)))

(defun math-from-radians-2 (a)   ; [N N]
  (cond ((memq calc-angle-mode '(deg hms))
	 (if calc-symbolic-mode
	     (math-div (math-mul 180 a) '(var pi var-pi))
	   (math-div a (math-pi-over-180))))
	(t a)))



;;; Sine, cosine, and tangent.

(defun calcFunc-sin (x)   ; [N N] [Public]
  (cond ((and (integerp x)
	      (if (eq calc-angle-mode 'deg)
		  (= (% x 90) 0)
		(= x 0)))
	 (aref [0 1 0 -1] (math-mod (/ x 90) 4)))
	((Math-scalarp x)
	 (math-with-extra-prec 2
	   (math-sin-raw (math-to-radians (math-float x)))))
	((eq (car x) 'sdev)
	 (if (math-constp x)
	     (math-with-extra-prec 2
	       (let* ((xx (math-to-radians (math-float (nth 1 x))))
		      (xs (math-to-radians (math-float (nth 2 x))))
		      (sc (math-sin-cos-raw xx)))
		 (math-make-sdev (car sc) (math-mul xs (cdr sc)))))
	   (math-make-sdev (calcFunc-sin (nth 1 x))
			   (math-mul (nth 2 x) (calcFunc-cos (nth 1 x))))))
	((and (eq (car x) 'intv) (math-intv-constp x))
	 (calcFunc-cos (math-sub x (math-quarter-circle nil))))
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'scalarp x)
	   (list 'calcFunc-sin x))))

(defun calcFunc-cos (x)   ; [N N] [Public]
  (cond ((and (integerp x)
	      (if (eq calc-angle-mode 'deg)
		  (= (% x 90) 0)
		(= x 0)))
	 (aref [1 0 -1 0] (math-mod (/ x 90) 4)))
	((Math-scalarp x)
	 (math-with-extra-prec 2
	   (math-cos-raw (math-to-radians (math-float x)))))
	((eq (car x) 'sdev)
	 (if (math-constp x)
	     (math-with-extra-prec 2
	       (let* ((xx (math-to-radians (math-float (nth 1 x))))
		      (xs (math-to-radians (math-float (nth 2 x))))
		      (sc (math-sin-cos-raw xx)))
		 (math-make-sdev (cdr sc) (math-mul xs (car sc)))))
	   (math-make-sdev (calcFunc-cos (nth 1 x))
			   (math-mul (nth 2 x) (calcFunc-sin (nth 1 x))))))
	((and (eq (car x) 'intv) (math-intv-constp x))
	 (math-with-extra-prec 2
	   (let* ((xx (math-to-radians (math-float x)))
		  (na (math-floor (math-div (nth 2 xx) (math-pi))))
		  (nb (math-floor (math-div (nth 3 xx) (math-pi))))
		  (span (math-sub nb na)))
	     (if (memq span '(0 1))
		 (let ((int (math-sort-intv (nth 1 x)
					    (math-cos-raw (nth 2 xx))
					    (math-cos-raw (nth 3 xx)))))
		   (if (eq span 1)
		       (if (math-evenp na)
			   (math-make-intv (logior (nth 1 x) 2)
					   -1
					   (nth 3 int))
			 (math-make-intv (logior (nth 1 x) 1)
					 (nth 2 int)
					 1))
		     int))
	       (list 'intv 3 -1 1)))))
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'scalarp x)
	   (list 'calcFunc-cos x))))

(defun calcFunc-sincos (x)   ; [V N] [Public]
  (if (Math-scalarp x)
      (math-with-extra-prec 2
	(let ((sc (math-sin-cos-raw (math-to-radians (math-float x)))))
	  (list 'vec (cdr sc) (car sc))))    ; the vector [cos, sin]
    (list 'vec (calcFunc-sin x) (calcFunc-cos x))))

(defun calcFunc-tan (x)   ; [N N] [Public]
  (cond ((and (integerp x)
	      (if (eq calc-angle-mode 'deg)
		  (= (% x 180) 0)
		(= x 0)))
	 0)
	((Math-scalarp x)
	 (math-with-extra-prec 2
	   (math-tan-raw (math-to-radians (math-float x)))))
	((eq (car x) 'sdev)
	 (if (math-constp x)
	     (math-with-extra-prec 2
	       (let* ((xx (math-to-radians (math-float (nth 1 x))))
		      (xs (math-to-radians (math-float (nth 2 x))))
		      (sc (math-sin-cos-raw xx)))
		 (if (and (math-zerop (cdr sc)) (not calc-infinite-mode))
		     (progn
		       (calc-record-why "*Division by zero")
		       (list 'calcFunc-tan x))
		   (math-make-sdev (math-div-float (car sc) (cdr sc))
				   (math-div-float xs (math-sqr (cdr sc)))))))
	   (math-make-sdev (calcFunc-tan (nth 1 x))
			   (math-div (nth 2 x)
				     (math-sqr (calcFunc-cos (nth 1 x)))))))
	((and (eq (car x) 'intv) (math-intv-constp x))
	 (or (math-with-extra-prec 2
	       (let* ((xx (math-to-radians (math-float x)))
		      (na (math-floor (math-div (math-sub (nth 2 xx)
							  (math-pi-over-2))
						(math-pi))))
		      (nb (math-floor (math-div (math-sub (nth 3 xx)
							  (math-pi-over-2))
						(math-pi)))))
		 (and (equal na nb)
		      (math-sort-intv (nth 1 x)
				      (math-tan-raw (nth 2 xx))
				      (math-tan-raw (nth 3 xx))))))
	     '(intv 3 (neg (var inf var-inf)) (var inf var-inf))))
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'scalarp x)
	   (list 'calcFunc-tan x))))

(defun calcFunc-sec (x)
  (cond ((and (integerp x)
              (eq calc-angle-mode 'deg)
              (= (% x 180) 0))
         (if (= (% x 360) 0)
             1
           -1))
        ((and (integerp x)
              (eq calc-angle-mode 'rad)
              (= x 0))
         1)
        ((Math-scalarp x)
         (math-with-extra-prec 2
           (math-sec-raw (math-to-radians (math-float x)))))
        ((eq (car x) 'sdev)
         (if (math-constp x)
             (math-with-extra-prec 2
               (let* ((xx (math-to-radians (math-float (nth 1 x))))
                      (xs (math-to-radians (math-float (nth 2 x))))
                      (sc (math-sin-cos-raw xx)))
                 (if (and (math-zerop (cdr sc))
                          (not calc-infinite-mode))
                     (progn
                       (calc-record-why "*Division by zero")
                       (list 'calcFunc-sec x))
                   (math-make-sdev (math-div-float '(float 1 0) (cdr sc))
                                   (math-div-float
                                    (math-mul xs (car sc))
                                    (math-sqr (cdr sc)))))))
           (math-make-sdev (calcFunc-sec (nth 1 x))
                           (math-div 
                            (math-mul (nth 2 x)
                                      (calcFunc-sin (nth 1 x)))
                            (math-sqr (calcFunc-cos (nth 1 x)))))))
        ((and (eq (car x) 'intv)
              (math-intv-constp x))
         (math-with-extra-prec 2
           (let* ((xx (math-to-radians (math-float x)))
                  (na (math-floor (math-div (math-sub (nth 2 xx)
                                                      (math-pi-over-2))
                                            (math-pi))))
                  (nb (math-floor (math-div (math-sub (nth 3 xx)
                                                      (math-pi-over-2))
                                            (math-pi))))
                  (naa (math-floor (math-div (nth 2 xx) (math-pi-over-2))))
                  (nbb (math-floor (math-div (nth 3 xx) (math-pi-over-2))))
                  (span (math-sub nbb naa)))
             (if (not (equal na nb))
                 '(intv 3 (neg (var inf var-inf)) (var inf var-inf))
               (let ((int (math-sort-intv (nth 1 x)
                                          (math-sec-raw (nth 2 xx))
                                          (math-sec-raw (nth 3 xx)))))
                 (if (eq span 1)
                     (if (math-evenp (math-div (math-add naa 1) 2))
                         (math-make-intv (logior (nth 1 int) 2)
                                         1
                                         (nth 3 int))
                       (math-make-intv (logior (nth 1 int) 1)
                                       (nth 2 int)
                                       -1))
                   int))))))
        ((equal x '(var nan var-nan))
         x)
        (t (calc-record-why 'scalarp x)
           (list 'calcFunc-sec x))))

(defun calcFunc-csc (x)
  (cond ((and (integerp x)
              (eq calc-angle-mode 'deg)
              (= (% (- x 90) 180) 0))
         (if (= (% (- x 90) 360) 0)
             1
           -1))
        ((Math-scalarp x)
         (math-with-extra-prec 2
           (math-csc-raw (math-to-radians (math-float x)))))
        ((eq (car x) 'sdev)
         (if (math-constp x)
             (math-with-extra-prec 2
               (let* ((xx (math-to-radians (math-float (nth 1 x))))
                      (xs (math-to-radians (math-float (nth 2 x))))
                      (sc (math-sin-cos-raw xx)))
                 (if (and (math-zerop (car sc))
                          (not calc-infinite-mode))
                     (progn
                       (calc-record-why "*Division by zero")
                       (list 'calcFunc-csc x))
                   (math-make-sdev (math-div-float '(float 1 0) (car sc))
                                   (math-div-float
                                    (math-mul xs (cdr sc))
                                    (math-sqr (car sc)))))))
           (math-make-sdev (calcFunc-csc (nth 1 x))
                           (math-div 
                            (math-mul (nth 2 x)
                                      (calcFunc-cos (nth 1 x)))
                            (math-sqr (calcFunc-sin (nth 1 x)))))))
        ((and (eq (car x) 'intv)
              (math-intv-constp x))
         (math-with-extra-prec 2
           (let* ((xx (math-to-radians (math-float x)))
                  (na (math-floor (math-div (nth 2 xx) (math-pi))))
                  (nb (math-floor (math-div (nth 3 xx) (math-pi))))
                  (naa (math-floor (math-div (nth 2 xx) (math-pi-over-2))))
                  (nbb (math-floor (math-div (nth 3 xx) (math-pi-over-2))))
                  (span (math-sub nbb naa)))
             (if (not (equal na nb))
                 '(intv 3 (neg (var inf var-inf)) (var inf var-inf))
               (let ((int (math-sort-intv (nth 1 x)
                                          (math-csc-raw (nth 2 xx))
                                          (math-csc-raw (nth 3 xx)))))
                 (if (eq span 1)
                     (if (math-evenp (math-div naa 2))
                         (math-make-intv (logior (nth 1 int) 2)
                                         1
                                         (nth 3 int))
                       (math-make-intv (logior (nth 1 int) 1)
                                       (nth 2 int)
                                       -1))
                   int))))))
        ((equal x '(var nan var-nan))
         x)
        (t (calc-record-why 'scalarp x)
           (list 'calcFunc-csc x))))

(defun calcFunc-cot (x)   ; [N N] [Public]
  (cond ((and (integerp x)
	      (if (eq calc-angle-mode 'deg)
		  (= (% (- x 90) 180) 0)
		(= x 0)))
	 0)
	((Math-scalarp x)
	 (math-with-extra-prec 2
	   (math-cot-raw (math-to-radians (math-float x)))))
	((eq (car x) 'sdev)
	 (if (math-constp x)
	     (math-with-extra-prec 2
	       (let* ((xx (math-to-radians (math-float (nth 1 x))))
		      (xs (math-to-radians (math-float (nth 2 x))))
		      (sc (math-sin-cos-raw xx)))
		 (if (and (math-zerop (car sc)) (not calc-infinite-mode))
		     (progn
		       (calc-record-why "*Division by zero")
		       (list 'calcFunc-cot x))
		   (math-make-sdev (math-div-float (cdr sc) (car sc))
				   (math-div-float xs (math-sqr (car sc)))))))
	   (math-make-sdev (calcFunc-cot (nth 1 x))
			   (math-div (nth 2 x)
				     (math-sqr (calcFunc-sin (nth 1 x)))))))
	((and (eq (car x) 'intv) (math-intv-constp x))
	 (or (math-with-extra-prec 2
	       (let* ((xx (math-to-radians (math-float x)))
		      (na (math-floor (math-div (nth 2 xx) (math-pi))))
		      (nb (math-floor (math-div (nth 3 xx) (math-pi)))))
		 (and (equal na nb)
		      (math-sort-intv (nth 1 x)
				      (math-cot-raw (nth 2 xx))
				      (math-cot-raw (nth 3 xx))))))
	     '(intv 3 (neg (var inf var-inf)) (var inf var-inf))))
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'scalarp x)
	   (list 'calcFunc-cot x))))

(defun math-sin-raw (x &optional orgx)   ; [N N]
  (cond ((eq (car x) 'cplx)
	 (let* ((expx (math-exp-raw (nth 2 x)))
		(expmx (math-div-float '(float 1 0) expx))
		(sc (math-sin-cos-raw (nth 1 x))))
	   (list 'cplx
		 (math-mul-float (car sc)
				 (math-mul-float (math-add-float expx expmx)
						 '(float 5 -1)))
		 (math-mul-float (cdr sc)
				 (math-mul-float (math-sub-float expx expmx)
						 '(float 5 -1))))))
	((eq (car x) 'polar)
	 (math-polar (math-sin-raw (math-complex x))))
	((Math-integer-negp (nth 1 x))
	 (math-neg-float (math-sin-raw (math-neg-float x) (if orgx orgx x))))
	((math-lessp-float '(float 7 0) x)  ; avoid inf loops due to roundoff
	 (math-sin-raw (math-mod x (math-two-pi)) (if orgx orgx x)))
	(t (math-sin-raw-2 x (if orgx orgx x)))))

(defun math-cos-raw (x)   ; [N N]
  (if (eq (car-safe x) 'polar)
      (math-polar (math-cos-raw (math-complex x)))
    (math-sin-raw (math-sub (math-pi-over-2) x) x)))

(defun math-sec-raw (x)   ; [N N]
  (cond ((eq (car x) 'cplx)
	 (let* ((x (math-mul x '(float 1 0)))
                (expx (math-exp-raw (nth 2 x)))
		(expmx (math-div-float '(float 1 0) expx))
                (sh (math-mul-float (math-sub-float expx expmx) '(float 5 -1)))
                (ch (math-mul-float (math-add-float expx expmx) '(float 5 -1)))
		(sc (math-sin-cos-raw (nth 1 x)))
		(d (math-add-float 
                    (math-mul-float (math-sqr (car sc))
                                    (math-sqr sh))
                    (math-mul-float (math-sqr (cdr sc))
                                    (math-sqr ch)))))
	   (and (not (eq (nth 1 d) 0))
		(list 'cplx
		      (math-div-float (math-mul-float (cdr sc) ch) d)
		      (math-div-float (math-mul-float (car sc) sh) d)))))
	((eq (car x) 'polar)
	 (math-polar (math-sec-raw (math-complex x))))
	(t
	 (let ((cs (math-cos-raw x)))
           (if (eq cs 0)
               (math-div 1 0)
	     (math-div-float '(float 1 0) cs))))))

(defun math-csc-raw (x)   ; [N N]
  (cond ((eq (car x) 'cplx)
	 (let* ((x (math-mul x '(float 1 0)))
                (expx (math-exp-raw (nth 2 x)))
		(expmx (math-div-float '(float 1 0) expx))
                (sh (math-mul-float (math-sub-float expx expmx) '(float 5 -1)))
                (ch (math-mul-float (math-add-float expx expmx) '(float 5 -1)))
		(sc (math-sin-cos-raw (nth 1 x)))
		(d (math-add-float 
                    (math-mul-float (math-sqr (car sc))
                                    (math-sqr ch))
                    (math-mul-float (math-sqr (cdr sc))
                                    (math-sqr sh)))))
	   (and (not (eq (nth 1 d) 0))
		(list 'cplx
		      (math-div-float (math-mul-float (car sc) ch) d)
		      (math-div-float (math-mul-float (cdr sc) sh) d)))))
	((eq (car x) 'polar)
	 (math-polar (math-csc-raw (math-complex x))))
	(t
	 (let ((sn (math-sin-raw x)))
           (if (eq sn 0)
               (math-div 1 0)
	     (math-div-float '(float 1 0) sn))))))

(defun math-cot-raw (x)   ; [N N]
  (cond ((eq (car x) 'cplx)
	 (let* ((x (math-mul x '(float 1 0)))
                (expx (math-exp-raw (nth 2 x)))
		(expmx (math-div-float '(float 1 0) expx))
                (sh (math-mul-float (math-sub-float expx expmx) '(float 5 -1)))
                (ch (math-mul-float (math-add-float expx expmx) '(float 5 -1)))
		(sc (math-sin-cos-raw (nth 1 x)))
		(d (math-add-float 
                    (math-sqr (car sc))
                    (math-sqr sh))))
	   (and (not (eq (nth 1 d) 0))
		(list 'cplx
		      (math-div-float 
                       (math-mul-float (car sc) (cdr sc))
                       d)
                      (math-neg
                       (math-div-float 
                        (math-mul-float sh ch) 
                        d))))))
	((eq (car x) 'polar)
	 (math-polar (math-cot-raw (math-complex x))))
	(t
	 (let ((sc (math-sin-cos-raw x)))
	   (if (eq (nth 1 (car sc)) 0)
	       (math-div (cdr sc) 0)
	     (math-div-float (cdr sc) (car sc)))))))


;;; This could use a smarter method:  Reduce x as in math-sin-raw, then
;;;   compute either sin(x) or cos(x), whichever is smaller, and compute
;;;   the other using the identity sin(x)^2 + cos(x)^2 = 1.
(defun math-sin-cos-raw (x)   ; [F.F F]  (result is (sin x . cos x))
  (cons (math-sin-raw x) (math-cos-raw x)))

(defun math-tan-raw (x)   ; [N N]
  (cond ((eq (car x) 'cplx)
	 (let* ((x (math-mul x '(float 2 0)))
		(expx (math-exp-raw (nth 2 x)))
		(expmx (math-div-float '(float 1 0) expx))
		(sc (math-sin-cos-raw (nth 1 x)))
		(d (math-add-float (cdr sc)
				   (math-mul-float (math-add-float expx expmx)
						   '(float 5 -1)))))
	   (and (not (eq (nth 1 d) 0))
		(list 'cplx
		      (math-div-float (car sc) d)
		      (math-div-float (math-mul-float (math-sub-float expx
								      expmx)
						      '(float 5 -1)) d)))))
	((eq (car x) 'polar)
	 (math-polar (math-tan-raw (math-complex x))))
	(t
	 (let ((sc (math-sin-cos-raw x)))
	   (if (eq (nth 1 (cdr sc)) 0)
	       (math-div (car sc) 0)
	     (math-div-float (car sc) (cdr sc)))))))

(defun math-sin-raw-2 (x orgx)   ; This avoids poss of inf recursion.  [F F]
  (let ((xmpo2 (math-sub-float (math-pi-over-2) x)))
    (cond ((Math-integer-negp (nth 1 xmpo2))
	   (math-neg-float (math-sin-raw-2 (math-sub-float x (math-pi))
					   orgx)))
	  ((math-lessp-float (math-pi-over-4) x)
	   (math-cos-raw-2 xmpo2 orgx))
	  ((math-lessp-float x (math-neg (math-pi-over-4)))
	   (math-neg (math-cos-raw-2 (math-add (math-pi-over-2) x) orgx)))
	  ((math-with-extra-prec -1 (math-nearly-zerop-float x orgx)) 
           '(float 0 0))
          ((math-use-emacs-fn 'sin x))
	  (calc-symbolic-mode (signal 'inexact-result nil))
	  (t (math-sin-series x 6 4 x (math-neg-float (math-sqr-float x)))))))

(defun math-cos-raw-2 (x orgx)   ; [F F]
  (cond ((math-with-extra-prec -1 (math-nearly-zerop-float x orgx))
         '(float 1 0))
        ((math-use-emacs-fn 'cos x))
	(calc-symbolic-mode (signal 'inexact-result nil))
	(t (let ((xnegsqr (math-neg-float (math-sqr-float x))))
	     (math-sin-series
	      (math-add-float '(float 1 0)
			      (math-mul-float xnegsqr '(float 5 -1)))
	      24 5 xnegsqr xnegsqr)))))

(defun math-sin-series (sum nfac n x xnegsqr)
  (math-working "sin" sum)
  (let* ((nextx (math-mul-float x xnegsqr))
	 (nextsum (math-add-float sum (math-div-float nextx
						      (math-float nfac)))))
    (if (math-nearly-equal-float sum nextsum)
	sum
      (math-sin-series nextsum (math-mul nfac (* n (1+ n)))
		       (+ n 2) nextx xnegsqr))))


;;; Inverse sine, cosine, tangent.

(defun calcFunc-arcsin (x)   ; [N N] [Public]
  (cond ((eq x 0) 0)
	((and (eq x 1) (eq calc-angle-mode 'deg)) 90)
	((and (eq x -1) (eq calc-angle-mode 'deg)) -90)
	(calc-symbolic-mode (signal 'inexact-result nil))
	((Math-numberp x)
	 (math-with-extra-prec 2
	   (math-from-radians (math-arcsin-raw (math-float x)))))
	((eq (car x) 'sdev)
	 (math-make-sdev (calcFunc-arcsin (nth 1 x))
			 (math-from-radians
			  (math-div (nth 2 x)
				    (math-sqrt
				     (math-sub 1 (math-sqr (nth 1 x))))))))
	((eq (car x) 'intv)
	 (math-sort-intv (nth 1 x)
			 (calcFunc-arcsin (nth 2 x))
			 (calcFunc-arcsin (nth 3 x))))
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-arcsin x))))

(defun calcFunc-arccos (x)   ; [N N] [Public]
  (cond ((eq x 1) 0)
	((and (eq x 0) (eq calc-angle-mode 'deg)) 90)
	((and (eq x -1) (eq calc-angle-mode 'deg)) 180)
	(calc-symbolic-mode (signal 'inexact-result nil))
	((Math-numberp x)
	 (math-with-extra-prec 2
	   (math-from-radians (math-arccos-raw (math-float x)))))
	((eq (car x) 'sdev)
	 (math-make-sdev (calcFunc-arccos (nth 1 x))
			 (math-from-radians
			  (math-div (nth 2 x)
				    (math-sqrt
				     (math-sub 1 (math-sqr (nth 1 x))))))))
	((eq (car x) 'intv)
	 (math-sort-intv (nth 1 x)
			 (calcFunc-arccos (nth 2 x))
			 (calcFunc-arccos (nth 3 x))))
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-arccos x))))

(defun calcFunc-arctan (x)   ; [N N] [Public]
  (cond ((eq x 0) 0)
	((and (eq x 1) (eq calc-angle-mode 'deg)) 45)
	((and (eq x -1) (eq calc-angle-mode 'deg)) -45)
	((Math-numberp x)
	 (math-with-extra-prec 2
	   (math-from-radians (math-arctan-raw (math-float x)))))
	((eq (car x) 'sdev)
	 (math-make-sdev (calcFunc-arctan (nth 1 x))
			 (math-from-radians
			  (math-div (nth 2 x)
				    (math-add 1 (math-sqr (nth 1 x)))))))
	((eq (car x) 'intv)
	 (math-sort-intv (nth 1 x)
			 (calcFunc-arctan (nth 2 x))
			 (calcFunc-arctan (nth 3 x))))
	((equal x '(var inf var-inf))
	 (math-quarter-circle t))
	((equal x '(neg (var inf var-inf)))
	 (math-neg (math-quarter-circle t)))
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-arctan x))))

(defun math-arcsin-raw (x)   ; [N N]
  (let ((a (math-sqrt-raw (math-sub '(float 1 0) (math-sqr x)))))
    (if (or (memq (car x) '(cplx polar))
	    (memq (car a) '(cplx polar)))
	(math-with-extra-prec 2   ; use extra precision for difficult case
	  (math-mul '(cplx 0 -1)
		    (math-ln-raw (math-add (math-mul '(cplx 0 1) x) a))))
      (math-arctan2-raw x a))))

(defun math-arccos-raw (x)   ; [N N]
  (math-sub (math-pi-over-2) (math-arcsin-raw x)))

(defun math-arctan-raw (x)   ; [N N]
  (cond ((memq (car x) '(cplx polar))
	 (math-with-extra-prec 2   ; extra-extra
	   (math-div (math-sub
		      (math-ln-raw (math-add 1 (math-mul '(cplx 0 1) x)))
		      (math-ln-raw (math-add 1 (math-mul '(cplx 0 -1) x))))
		     '(cplx 0 2))))
	((Math-integer-negp (nth 1 x))
	 (math-neg-float (math-arctan-raw (math-neg-float x))))
	((math-zerop x) x)
        ((math-use-emacs-fn 'atan x))
	(calc-symbolic-mode (signal 'inexact-result nil))
	((math-equal-int x 1) (math-pi-over-4))
	((math-equal-int x -1) (math-neg (math-pi-over-4)))
	((math-lessp-float '(float 414214 -6) x)  ; if x > sqrt(2) - 1, reduce
	 (if (math-lessp-float '(float 1 0) x)
	     (math-sub-float (math-mul-float (math-pi) '(float 5 -1))
			     (math-arctan-raw (math-div-float '(float 1 0) x)))
	   (math-sub-float (math-mul-float (math-pi) '(float 25 -2))
			   (math-arctan-raw (math-div-float
					     (math-sub-float '(float 1 0) x)
					     (math-add-float '(float 1 0)
							     x))))))
	(t (math-arctan-series x 3 x (math-neg-float (math-sqr-float x))))))

(defun math-arctan-series (sum n x xnegsqr)
  (math-working "arctan" sum)
  (let* ((nextx (math-mul-float x xnegsqr))
	 (nextsum (math-add-float sum (math-div-float nextx (math-float n)))))
    (if (math-nearly-equal-float sum nextsum)
	sum
      (math-arctan-series nextsum (+ n 2) nextx xnegsqr))))

(defun calcFunc-arctan2 (y x)   ; [F R R] [Public]
  (if (Math-anglep y)
      (if (Math-anglep x)
	  (math-with-extra-prec 2
	    (math-from-radians (math-arctan2-raw (math-float y)
						 (math-float x))))
	(calc-record-why 'anglep x)
	(list 'calcFunc-arctan2 y x))
    (if (and (or (math-infinitep x) (math-anglep x))
	     (or (math-infinitep y) (math-anglep y)))
	(progn
	  (if (math-posp x)
	      (setq x 1)
	    (if (math-negp x)
		(setq x -1)
	      (or (math-zerop x)
		  (setq x nil))))
	  (if (math-posp y)
	      (setq y 1)
	    (if (math-negp y)
		(setq y -1)
	      (or (math-zerop y)
		  (setq y nil))))
	  (if (and y x)
	      (calcFunc-arctan2 y x)
	    '(var nan var-nan)))
      (calc-record-why 'anglep y)
      (list 'calcFunc-arctan2 y x))))

(defun math-arctan2-raw (y x)   ; [F R R]
  (cond ((math-zerop y)
	 (if (math-negp x) (math-pi)
	   (if (or (math-floatp x) (math-floatp y)) '(float 0 0) 0)))
	((math-zerop x)
	 (if (math-posp y)
	     (math-pi-over-2)
	   (math-neg (math-pi-over-2))))
	((math-posp x)
	 (math-arctan-raw (math-div-float y x)))
	((math-posp y)
	 (math-add-float (math-arctan-raw (math-div-float y x))
			 (math-pi)))
	(t
	 (math-sub-float (math-arctan-raw (math-div-float y x))
			 (math-pi)))))

(defun calcFunc-arcsincos (x)   ; [V N] [Public]
  (if (and (Math-vectorp x)
	   (= (length x) 3))
      (calcFunc-arctan2 (nth 2 x) (nth 1 x))
    (math-reject-arg x "*Two-element vector expected")))



;;; Exponential function.

(defun calcFunc-exp (x)   ; [N N] [Public]
  (cond ((eq x 0) 1)
	((and (memq x '(1 -1)) calc-symbolic-mode)
	 (if (eq x 1) '(var e var-e) (math-div 1 '(var e var-e))))
	((Math-numberp x)
	 (math-with-extra-prec 2 (math-exp-raw (math-float x))))
	((eq (car-safe x) 'sdev)
	 (let ((ex (calcFunc-exp (nth 1 x))))
	   (math-make-sdev ex (math-mul (nth 2 x) ex))))
	((eq (car-safe x) 'intv)
	 (math-make-intv (nth 1 x) (calcFunc-exp (nth 2 x))
			 (calcFunc-exp (nth 3 x))))
	((equal x '(var inf var-inf))
	 x)
	((equal x '(neg (var inf var-inf)))
	 0)
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-exp x))))

(defun calcFunc-expm1 (x)   ; [N N] [Public]
  (cond ((eq x 0) 0)
	((math-zerop x) '(float 0 0))
	(calc-symbolic-mode (signal 'inexact-result nil))
	((Math-numberp x)
	 (math-with-extra-prec 2
	   (let ((x (math-float x)))
	     (if (and (eq (car x) 'float)
		      (math-lessp-float x '(float 1 0))
		      (math-lessp-float '(float -1 0) x))
		 (math-exp-minus-1-raw x)
	       (math-add (math-exp-raw x) -1)))))
	((eq (car-safe x) 'sdev)
	 (if (math-constp x)
	     (let ((ex (calcFunc-expm1 (nth 1 x))))
	       (math-make-sdev ex (math-mul (nth 2 x) (math-add ex 1))))
	   (math-make-sdev (calcFunc-expm1 (nth 1 x))
			   (math-mul (nth 2 x) (calcFunc-exp (nth 1 x))))))
	((eq (car-safe x) 'intv)
	 (math-make-intv (nth 1 x)
			 (calcFunc-expm1 (nth 2 x))
			 (calcFunc-expm1 (nth 3 x))))
	((equal x '(var inf var-inf))
	 x)
	((equal x '(neg (var inf var-inf)))
	 -1)
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-expm1 x))))

(defun calcFunc-exp10 (x)   ; [N N] [Public]
  (if (eq x 0)
      1
    (math-pow '(float 1 1) x)))

(defun math-exp-raw (x)   ; [N N]
  (cond ((math-zerop x) '(float 1 0))
	(calc-symbolic-mode (signal 'inexact-result nil))
	((eq (car x) 'cplx)
	 (let ((expx (math-exp-raw (nth 1 x)))
	       (sc (math-sin-cos-raw (nth 2 x))))
	   (list 'cplx
		 (math-mul-float expx (cdr sc))
		 (math-mul-float expx (car sc)))))
	((eq (car x) 'polar)
	 (let ((xc (math-complex x)))
	   (list 'polar
		 (math-exp-raw (nth 1 xc))
		 (math-from-radians (nth 2 xc)))))
        ((math-use-emacs-fn 'exp x))
	((or (math-lessp-float '(float 5 -1) x)
	     (math-lessp-float x '(float -5 -1)))
	 (if (math-lessp-float '(float 921035 1) x)
	     (math-overflow)
	   (if (math-lessp-float x '(float -921035 1))
	       (math-underflow)))
	 (let* ((two-x (math-mul-float x '(float 2 0)))
		(hint (math-scale-int (nth 1 two-x) (nth 2 two-x)))
		(hfrac (math-sub-float x (math-mul-float (math-float hint)
							 '(float 5 -1)))))
	   (math-mul-float (math-ipow (math-sqrt-e) hint)
			   (math-add-float '(float 1 0)
					   (math-exp-minus-1-raw hfrac)))))
	(t (math-add-float '(float 1 0) (math-exp-minus-1-raw x)))))

(defun math-exp-minus-1-raw (x)   ; [F F]
  (math-exp-series x 2 3 x x))

(defun math-exp-series (sum nfac n xpow x)
  (math-working "exp" sum)
  (let* ((nextx (math-mul-float xpow x))
	 (nextsum (math-add-float sum (math-div-float nextx
						      (math-float nfac)))))
    (if (math-nearly-equal-float sum nextsum)
	sum
      (math-exp-series nextsum (math-mul nfac n) (1+ n) nextx x))))



;;; Logarithms.

(defun calcFunc-ln (x)   ; [N N] [Public]
  (cond ((math-zerop x)
	 (if calc-infinite-mode
	     '(neg (var inf var-inf))
	   (math-reject-arg x "*Logarithm of zero")))
	((eq x 1) 0)
	((Math-numberp x)
	 (math-with-extra-prec 2 (math-ln-raw (math-float x))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-ln (nth 1 x))
			 (math-div (nth 2 x) (nth 1 x))))
	((and (eq (car-safe x) 'intv) (or (Math-posp (nth 2 x))
					  (Math-zerop (nth 2 x))
					  (not (math-intv-constp x))))
	 (let ((calc-infinite-mode t))
	   (math-make-intv (nth 1 x) (calcFunc-ln (nth 2 x))
			   (calcFunc-ln (nth 3 x)))))
	((equal x '(var e var-e))
	 1)
	((and (eq (car-safe x) '^)
	      (equal (nth 1 x) '(var e var-e))
	      (math-known-realp (nth 2 x)))
	 (nth 2 x))
	((math-infinitep x)
	 (if (equal x '(var nan var-nan))
	     x
	   '(var inf var-inf)))
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-ln x))))

(defun calcFunc-log10 (x)   ; [N N] [Public]
  (cond ((math-equal-int x 1)
	 (if (math-floatp x) '(float 0 0) 0))
	((and (Math-integerp x)
	      (math-posp x)
	      (let ((res (math-integer-log x 10)))
		(and (car res)
		     (setq x (cdr res)))))
	 x)
	((and (eq (car-safe x) 'frac)
	      (eq (nth 1 x) 1)
	      (let ((res (math-integer-log (nth 2 x) 10)))
		(and (car res)
		     (setq x (- (cdr res))))))
	 x)
	((math-zerop x)
	 (if calc-infinite-mode
	     '(neg (var inf var-inf))
	   (math-reject-arg x "*Logarithm of zero")))
        (calc-symbolic-mode (signal 'inexact-result nil))
	((Math-numberp x)
	 (math-with-extra-prec 2
	   (let ((xf (math-float x)))
	     (if (eq (nth 1 xf) 0)
		 (math-reject-arg x "*Logarithm of zero"))
	     (if (Math-integer-posp (nth 1 xf))
		 (if (eq (nth 1 xf) 1)    ; log10(1*10^n) = n
		     (math-float (nth 2 xf))
		   (let ((xdigs (1- (math-numdigs (nth 1 xf)))))
		     (math-add-float
		      (math-div-float (math-ln-raw-2
				       (list 'float (nth 1 xf) (- xdigs)))
				      (math-ln-10))
		      (math-float (+ (nth 2 xf) xdigs)))))
	       (math-div (calcFunc-ln xf) (math-ln-10))))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-log10 (nth 1 x))
			 (math-div (nth 2 x)
				   (math-mul (nth 1 x) (math-ln-10)))))
	((and (eq (car-safe x) 'intv) (or (Math-posp (nth 2 x))
					  (not (math-intv-constp x))))
	 (math-make-intv (nth 1 x)
			 (calcFunc-log10 (nth 2 x))
			 (calcFunc-log10 (nth 3 x))))
	((math-infinitep x)
	 (if (equal x '(var nan var-nan))
	     x
	   '(var inf var-inf)))
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-log10 x))))

(defun calcFunc-log (x &optional b)   ; [N N N] [Public]
  (cond ((or (null b) (equal b '(var e var-e)))
	 (math-normalize (list 'calcFunc-ln x)))
	((or (eq b 10) (equal b '(float 1 1)))
	 (math-normalize (list 'calcFunc-log10 x)))
	((math-zerop x)
	 (if calc-infinite-mode
	     (math-div (calcFunc-ln x) (calcFunc-ln b))
	   (math-reject-arg x "*Logarithm of zero")))
	((math-zerop b)
	 (if calc-infinite-mode
	     (math-div (calcFunc-ln x) (calcFunc-ln b))
	   (math-reject-arg b "*Logarithm of zero")))
	((math-equal-int b 1)
	 (if calc-infinite-mode
	     (math-div (calcFunc-ln x) 0)
	   (math-reject-arg b "*Logarithm base one")))
	((math-equal-int x 1)
	 (if (math-floatp b) '(float 0 0) 0))
	((and (Math-ratp x) (Math-ratp b)
	      (math-posp x) (math-posp b)
	      (let* ((sign 1) (inv nil)
		     (xx (if (Math-lessp 1 x)
			     x
			   (setq sign -1)
			   (math-div 1 x)))
		     (bb (if (Math-lessp 1 b)
			     b
			   (setq sign (- sign))
			   (math-div 1 b)))
		     (res (if (Math-lessp xx bb)
			      (setq inv (math-integer-log bb xx))
			    (math-integer-log xx bb))))
		(and (car res)
		     (setq x (if inv
				 (math-div 1 (* sign (cdr res)))
			       (* sign (cdr res)))))))
	 x)
	(calc-symbolic-mode (signal 'inexact-result nil))
	((and (Math-numberp x) (Math-numberp b))
	 (math-with-extra-prec 2
	   (math-div (math-ln-raw (math-float x))
		     (math-log-base-raw b))))
	((and (eq (car-safe x) 'sdev)
	      (Math-numberp b))
	 (math-make-sdev (calcFunc-log (nth 1 x) b)
			 (math-div (nth 2 x)
				   (math-mul (nth 1 x)
					     (math-log-base-raw b)))))
	((and (eq (car-safe x) 'intv) (or (Math-posp (nth 2 x))
					  (not (math-intv-constp x)))
	      (math-realp b))
	 (math-make-intv (nth 1 x)
			 (calcFunc-log (nth 2 x) b)
			 (calcFunc-log (nth 3 x) b)))
	((or (eq (car-safe x) 'intv) (eq (car-safe b) 'intv))
	 (math-div (calcFunc-ln x) (calcFunc-ln b)))
	((or (math-infinitep x)
	     (math-infinitep b))
	 (math-div (calcFunc-ln x) (calcFunc-ln b)))
	(t (if (Math-numberp b)
	       (calc-record-why 'numberp x)
	     (calc-record-why 'numberp b))
	   (list 'calcFunc-log x b))))

(defun calcFunc-alog (x &optional b)
  (cond ((or (null b) (equal b '(var e var-e)))
	 (math-normalize (list 'calcFunc-exp x)))
	(t (math-pow b x))))

(defun calcFunc-ilog (x b)
  (if (and (math-natnump x) (not (eq x 0))
	   (math-natnump b) (not (eq b 0)))
      (if (eq b 1)
	  (math-reject-arg x "*Logarithm base one")
	(if (Math-natnum-lessp x b)
	    0
	  (cdr (math-integer-log x b))))
    (math-floor (calcFunc-log x b))))

(defun math-integer-log (x b)
  (let ((pows (list b))
	(pow (math-sqr b))
	next
	sum n)
    (while (not (Math-lessp x pow))
      (setq pows (cons pow pows)
	    pow (math-sqr pow)))
    (setq n (lsh 1 (1- (length pows)))
	  sum n
	  pow (car pows))
    (while (and (setq pows (cdr pows))
		(Math-lessp pow x))
      (setq n (/ n 2)
	    next (math-mul pow (car pows)))
      (or (Math-lessp x next)
	  (setq pow next
		sum (+ sum n))))
    (cons (equal pow x) sum)))


(defvar math-log-base-cache nil)
(defun math-log-base-raw (b)   ; [N N]
  (if (not (and (equal (car math-log-base-cache) b)
		(eq (nth 1 math-log-base-cache) calc-internal-prec)))
      (setq math-log-base-cache (list b calc-internal-prec
				      (math-ln-raw (math-float b)))))
  (nth 2 math-log-base-cache))

(defun calcFunc-lnp1 (x)   ; [N N] [Public]
  (cond ((Math-equal-int x -1)
	 (if calc-infinite-mode
	     '(neg (var inf var-inf))
	   (math-reject-arg x "*Logarithm of zero")))
	((eq x 0) 0)
	((math-zerop x) '(float 0 0))
	(calc-symbolic-mode (signal 'inexact-result nil))
	((Math-numberp x)
	 (math-with-extra-prec 2
	   (let ((x (math-float x)))
	     (if (and (eq (car x) 'float)
		      (math-lessp-float x '(float 5 -1))
		      (math-lessp-float '(float -5 -1) x))
		 (math-ln-plus-1-raw x)
	       (math-ln-raw (math-add-float x '(float 1 0)))))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-lnp1 (nth 1 x))
			 (math-div (nth 2 x) (math-add (nth 1 x) 1))))
	((and (eq (car-safe x) 'intv) (or (Math-posp (nth 2 x))
					  (not (math-intv-constp x))))
	 (math-make-intv (nth 1 x)
			 (calcFunc-lnp1 (nth 2 x))
			 (calcFunc-lnp1 (nth 3 x))))
	((math-infinitep x)
	 (if (equal x '(var nan var-nan))
	     x
	   '(var inf var-inf)))
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-lnp1 x))))

(defun math-ln-raw (x)    ; [N N] --- must be float format!
  (cond ((eq (car-safe x) 'cplx)
	 (list 'cplx
	       (math-mul-float (math-ln-raw
				(math-add-float (math-sqr-float (nth 1 x))
						(math-sqr-float (nth 2 x))))
			       '(float 5 -1))
	       (math-arctan2-raw (nth 2 x) (nth 1 x))))
	((eq (car x) 'polar)
	 (math-polar (list 'cplx
			   (math-ln-raw (nth 1 x))
			   (math-to-radians (nth 2 x)))))
	((Math-equal-int x 1)
	 '(float 0 0))
	(calc-symbolic-mode (signal 'inexact-result nil))
	((math-posp (nth 1 x))    ; positive and real
         (cond 
          ((math-use-emacs-fn 'log x))
          (t
           (let ((xdigs (1- (math-numdigs (nth 1 x)))))
             (math-add-float (math-ln-raw-2 (list 'float (nth 1 x) (- xdigs)))
                             (math-mul-float (math-float (+ (nth 2 x) xdigs))
                                             (math-ln-10)))))))
	((math-zerop x)
	 (math-reject-arg x "*Logarithm of zero"))
	((eq calc-complex-mode 'polar)    ; negative and real
	 (math-polar
	  (list 'cplx   ; negative and real
		(math-ln-raw (math-neg-float x))
		(math-pi))))
	(t (list 'cplx   ; negative and real
		 (math-ln-raw (math-neg-float x))
		 (math-pi)))))

(defun math-ln-raw-2 (x)    ; [F F]
  (cond ((math-lessp-float '(float 14 -1) x)
	 (math-add-float (math-ln-raw-2 (math-mul-float x '(float 5 -1)))
			 (math-ln-2)))
	(t    ; now .7 < x <= 1.4
	 (math-ln-raw-3 (math-div-float (math-sub-float x '(float 1 0))
					(math-add-float x '(float 1 0)))))))

(defun math-ln-raw-3 (x)   ; [F F]
  (math-mul-float (math-ln-raw-series x 3 x (math-sqr-float x))
		  '(float 2 0)))

;;; Compute ln((1+x)/(1-x))
(defun math-ln-raw-series (sum n x xsqr)
  (math-working "log" sum)
  (let* ((nextx (math-mul-float x xsqr))
	 (nextsum (math-add-float sum (math-div-float nextx (math-float n)))))
    (if (math-nearly-equal-float sum nextsum)
	sum
      (math-ln-raw-series nextsum (+ n 2) nextx xsqr))))

(defun math-ln-plus-1-raw (x)
  (math-lnp1-series x 2 x (math-neg x)))

(defun math-lnp1-series (sum n xpow x)
  (math-working "lnp1" sum)
  (let* ((nextx (math-mul-float xpow x))
	 (nextsum (math-add-float sum (math-div-float nextx (math-float n)))))
    (if (math-nearly-equal-float sum nextsum)
	sum
      (math-lnp1-series nextsum (1+ n) nextx x))))

(defconst math-approx-ln-10
  (math-read-number-simple "2.302585092994045684018")
  "An approximation for ln(10).")
     
(math-defcache math-ln-10 math-approx-ln-10
  (math-ln-raw-2 '(float 1 1)))

(defconst math-approx-ln-2
  (math-read-number-simple "0.693147180559945309417")
  "An approximation for ln(2).")

(math-defcache math-ln-2 math-approx-ln-2
  (math-ln-raw-3 (math-float '(frac 1 3))))



;;; Hyperbolic functions.

(defun calcFunc-sinh (x)   ; [N N] [Public]
  (cond ((eq x 0) 0)
	(math-expand-formulas
	 (math-normalize
	  (list '/ (list '- (list 'calcFunc-exp x)
			 (list 'calcFunc-exp (list 'neg x))) 2)))
	((Math-numberp x)
	 (if calc-symbolic-mode (signal 'inexact-result nil))
	 (math-with-extra-prec 2
	   (let ((expx (math-exp-raw (math-float x))))
	     (math-mul (math-add expx (math-div -1 expx)) '(float 5 -1)))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-sinh (nth 1 x))
			 (math-mul (nth 2 x) (calcFunc-cosh (nth 1 x)))))
	((eq (car x) 'intv)
	 (math-sort-intv (nth 1 x)
			 (calcFunc-sinh (nth 2 x))
			 (calcFunc-sinh (nth 3 x))))
	((or (equal x '(var inf var-inf))
	     (equal x '(neg (var inf var-inf)))
	     (equal x '(var nan var-nan)))
	 x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-sinh x))))
(put 'calcFunc-sinh 'math-expandable t)

(defun calcFunc-cosh (x)   ; [N N] [Public]
  (cond ((eq x 0) 1)
	(math-expand-formulas
	 (math-normalize
	  (list '/ (list '+ (list 'calcFunc-exp x)
			 (list 'calcFunc-exp (list 'neg x))) 2)))
	((Math-numberp x)
	 (if calc-symbolic-mode (signal 'inexact-result nil))
	 (math-with-extra-prec 2
	   (let ((expx (math-exp-raw (math-float x))))
	     (math-mul (math-add expx (math-div 1 expx)) '(float 5 -1)))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-cosh (nth 1 x))
			 (math-mul (nth 2 x)
				   (calcFunc-sinh (nth 1 x)))))
	((and (eq (car x) 'intv) (math-intv-constp x))
	 (setq x (math-abs x))
	 (math-sort-intv (nth 1 x)
			 (calcFunc-cosh (nth 2 x))
			 (calcFunc-cosh (nth 3 x))))
	((or (equal x '(var inf var-inf))
	     (equal x '(neg (var inf var-inf)))
	     (equal x '(var nan var-nan)))
	 (math-abs x))
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-cosh x))))
(put 'calcFunc-cosh 'math-expandable t)

(defun calcFunc-tanh (x)   ; [N N] [Public]
  (cond ((eq x 0) 0)
	(math-expand-formulas
	 (math-normalize
	  (let ((expx (list 'calcFunc-exp x))
		(expmx (list 'calcFunc-exp (list 'neg x))))
	    (math-normalize
	     (list '/ (list '- expx expmx) (list '+ expx expmx))))))
	((Math-numberp x)
	 (if calc-symbolic-mode (signal 'inexact-result nil))
	 (math-with-extra-prec 2
	   (let* ((expx (calcFunc-exp (math-float x)))
		  (expmx (math-div 1 expx)))
	     (math-div (math-sub expx expmx)
		       (math-add expx expmx)))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-tanh (nth 1 x))
			 (math-div (nth 2 x)
				   (math-sqr (calcFunc-cosh (nth 1 x))))))
	((eq (car x) 'intv)
	 (math-sort-intv (nth 1 x)
			 (calcFunc-tanh (nth 2 x))
			 (calcFunc-tanh (nth 3 x))))
	((equal x '(var inf var-inf))
	 1)
	((equal x '(neg (var inf var-inf)))
	 -1)
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-tanh x))))
(put 'calcFunc-tanh 'math-expandable t)

(defun calcFunc-sech (x)   ; [N N] [Public]
  (cond ((eq x 0) 1)
	(math-expand-formulas
	 (math-normalize
	  (list '/ 2 (list '+ (list 'calcFunc-exp x)
                           (list 'calcFunc-exp (list 'neg x))))))
	((Math-numberp x)
	 (if calc-symbolic-mode (signal 'inexact-result nil))
	 (math-with-extra-prec 2
	   (let ((expx (math-exp-raw (math-float x))))
	     (math-div '(float 2 0) (math-add expx (math-div 1 expx))))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-sech (nth 1 x))
			 (math-mul (nth 2 x)
                                   (math-mul (calcFunc-sech (nth 1 x))
                                             (calcFunc-tanh (nth 1 x))))))
	((and (eq (car x) 'intv) (math-intv-constp x))
	 (setq x (math-abs x))
	 (math-sort-intv (nth 1 x)
			 (calcFunc-sech (nth 2 x))
			 (calcFunc-sech (nth 3 x))))
	((or (equal x '(var inf var-inf))
	     (equal x '(neg (var inf var-inf))))
         0)
        ((equal x '(var nan var-nan))
         x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-sech x))))
(put 'calcFunc-sech 'math-expandable t)

(defun calcFunc-csch (x)   ; [N N] [Public]
  (cond ((eq x 0) (math-div 1 0))
	(math-expand-formulas
	 (math-normalize
	  (list '/ 2 (list '- (list 'calcFunc-exp x)
                           (list 'calcFunc-exp (list 'neg x))))))
	((Math-numberp x)
	 (if calc-symbolic-mode (signal 'inexact-result nil))
	 (math-with-extra-prec 2
	   (let ((expx (math-exp-raw (math-float x))))
	     (math-div '(float 2 0) (math-add expx (math-div -1 expx))))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-csch (nth 1 x))
			 (math-mul (nth 2 x) 
                                   (math-mul (calcFunc-csch (nth 1 x))
                                             (calcFunc-coth (nth 1 x))))))
	((eq (car x) 'intv)
         (if (and (Math-negp (nth 2 x))
                  (Math-posp (nth 3 x)))
             '(intv 3 (neg (var inf var-inf)) (var inf var-inf))
           (math-sort-intv (nth 1 x)
                           (calcFunc-csch (nth 2 x))
                           (calcFunc-csch (nth 3 x)))))
	((or (equal x '(var inf var-inf))
	     (equal x '(neg (var inf var-inf))))
         0)
        ((equal x '(var nan var-nan))
         x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-csch x))))
(put 'calcFunc-csch 'math-expandable t)

(defun calcFunc-coth (x)   ; [N N] [Public]
  (cond ((eq x 0) (math-div 1 0))
	(math-expand-formulas
	 (math-normalize
	  (let ((expx (list 'calcFunc-exp x))
		(expmx (list 'calcFunc-exp (list 'neg x))))
	    (math-normalize
	     (list '/ (list '+ expx expmx) (list '- expx expmx))))))
	((Math-numberp x)
	 (if calc-symbolic-mode (signal 'inexact-result nil))
	 (math-with-extra-prec 2
	   (let* ((expx (calcFunc-exp (math-float x)))
		  (expmx (math-div 1 expx)))
	     (math-div (math-add expx expmx)
		       (math-sub expx expmx)))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-coth (nth 1 x))
			 (math-div (nth 2 x)
				   (math-sqr (calcFunc-sinh (nth 1 x))))))
	((eq (car x) 'intv)
         (if (and (Math-negp (nth 2 x))
                  (Math-posp (nth 3 x)))
             '(intv 3 (neg (var inf var-inf)) (var inf var-inf))
           (math-sort-intv (nth 1 x)
                           (calcFunc-coth (nth 2 x))
                           (calcFunc-coth (nth 3 x)))))
	((equal x '(var inf var-inf))
	 1)
	((equal x '(neg (var inf var-inf)))
	 -1)
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-coth x))))
(put 'calcFunc-coth 'math-expandable t)

(defun calcFunc-arcsinh (x)   ; [N N] [Public]
  (cond ((eq x 0) 0)
	(math-expand-formulas
	 (math-normalize
	  (list 'calcFunc-ln (list '+ x (list 'calcFunc-sqrt
					      (list '+ (list '^ x 2) 1))))))
	((Math-numberp x)
	 (if calc-symbolic-mode (signal 'inexact-result nil))
	 (math-with-extra-prec 2
	   (math-ln-raw (math-add x (math-sqrt-raw (math-add (math-sqr x)
							     '(float 1 0)))))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-arcsinh (nth 1 x))
			 (math-div (nth 2 x)
				   (math-sqrt
				    (math-add (math-sqr (nth 1 x)) 1)))))
	((eq (car x) 'intv)
	 (math-sort-intv (nth 1 x)
			 (calcFunc-arcsinh (nth 2 x))
			 (calcFunc-arcsinh (nth 3 x))))
	((or (equal x '(var inf var-inf))
	     (equal x '(neg (var inf var-inf)))
	     (equal x '(var nan var-nan)))
	 x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-arcsinh x))))
(put 'calcFunc-arcsinh 'math-expandable t)

(defun calcFunc-arccosh (x)   ; [N N] [Public]
  (cond ((eq x 1) 0)
	((and (eq x -1) calc-symbolic-mode)
	 '(var pi var-pi))
	((and (eq x 0) calc-symbolic-mode)
	 (math-div (math-mul '(var pi var-pi) '(var i var-i)) 2))
	(math-expand-formulas
	 (math-normalize
	  (list 'calcFunc-ln (list '+ x (list 'calcFunc-sqrt
					      (list '- (list '^ x 2) 1))))))
	((Math-numberp x)
	 (if calc-symbolic-mode (signal 'inexact-result nil))
	 (if (Math-equal-int x -1)
	     (math-imaginary (math-pi))
	   (math-with-extra-prec 2
	     (if (or t    ; need to do this even in the real case!
		     (memq (car-safe x) '(cplx polar)))
		 (let ((xp1 (math-add 1 x)))  ; this gets the branch cuts right
		   (math-ln-raw
		    (math-add x (math-mul xp1
					  (math-sqrt-raw
					   (math-div (math-sub
						      x
						      '(float 1 0))
						     xp1))))))
	       (math-ln-raw
		(math-add x (math-sqrt-raw (math-add (math-sqr x)
						     '(float -1 0)))))))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-arccosh (nth 1 x))
			 (math-div (nth 2 x)
				   (math-sqrt
				    (math-add (math-sqr (nth 1 x)) -1)))))
	((eq (car x) 'intv)
	 (math-sort-intv (nth 1 x)
			 (calcFunc-arccosh (nth 2 x))
			 (calcFunc-arccosh (nth 3 x))))
	((or (equal x '(var inf var-inf))
	     (equal x '(neg (var inf var-inf)))
	     (equal x '(var nan var-nan)))
	 x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-arccosh x))))
(put 'calcFunc-arccosh 'math-expandable t)

(defun calcFunc-arctanh (x)   ; [N N] [Public]
  (cond ((eq x 0) 0)
	((and (Math-equal-int x 1) calc-infinite-mode)
	 '(var inf var-inf))
	((and (Math-equal-int x -1) calc-infinite-mode)
	 '(neg (var inf var-inf)))
	(math-expand-formulas
	 (list '/ (list '-
			(list 'calcFunc-ln (list '+ 1 x))
			(list 'calcFunc-ln (list '- 1 x))) 2))
	((Math-numberp x)
	 (if calc-symbolic-mode (signal 'inexact-result nil))
	 (math-with-extra-prec 2
	   (if (or (memq (car-safe x) '(cplx polar))
		   (Math-lessp 1 x))
	       (math-mul (math-sub (math-ln-raw (math-add '(float 1 0) x))
				   (math-ln-raw (math-sub '(float 1 0) x)))
			 '(float 5 -1))
	     (if (and (math-equal-int x 1) calc-infinite-mode)
		 '(var inf var-inf)
	       (if (and (math-equal-int x -1) calc-infinite-mode)
		   '(neg (var inf var-inf))
		 (math-mul (math-ln-raw (math-div (math-add '(float 1 0) x)
						  (math-sub 1 x)))
			   '(float 5 -1)))))))
	((eq (car-safe x) 'sdev)
	 (math-make-sdev (calcFunc-arctanh (nth 1 x))
			 (math-div (nth 2 x)
				   (math-sub 1 (math-sqr (nth 1 x))))))
	((eq (car x) 'intv)
	 (math-sort-intv (nth 1 x)
			 (calcFunc-arctanh (nth 2 x))
			 (calcFunc-arctanh (nth 3 x))))
	((equal x '(var nan var-nan))
	 x)
	(t (calc-record-why 'numberp x)
	   (list 'calcFunc-arctanh x))))
(put 'calcFunc-arctanh 'math-expandable t)


;;; Convert A from HMS or degrees to radians.
(defun calcFunc-rad (a)   ; [R R] [Public]
  (cond ((or (Math-numberp a)
	     (eq (car a) 'intv))
	 (math-with-extra-prec 2
	   (math-mul a (math-pi-over-180))))
	((eq (car a) 'hms)
	 (math-from-hms a 'rad))
	((eq (car a) 'sdev)
	 (math-make-sdev (calcFunc-rad (nth 1 a))
			 (calcFunc-rad (nth 2 a))))
	(math-expand-formulas
	 (math-div (math-mul a '(var pi var-pi)) 180))
	((math-infinitep a) a)
	(t (list 'calcFunc-rad a))))
(put 'calcFunc-rad 'math-expandable t)

;;; Convert A from HMS or radians to degrees.
(defun calcFunc-deg (a)   ; [R R] [Public]
  (cond ((or (Math-numberp a)
	     (eq (car a) 'intv))
	 (math-with-extra-prec 2
	   (math-div a (math-pi-over-180))))
	((eq (car a) 'hms)
	 (math-from-hms a 'deg))
	((eq (car a) 'sdev)
	 (math-make-sdev (calcFunc-deg (nth 1 a))
			 (calcFunc-deg (nth 2 a))))
	(math-expand-formulas
	 (math-div (math-mul 180 a) '(var pi var-pi)))
	((math-infinitep a) a)
	(t (list 'calcFunc-deg a))))
(put 'calcFunc-deg 'math-expandable t)

(provide 'calc-math)

;;; calc-math.el ends here
