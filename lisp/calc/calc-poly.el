;;; calc-poly.el --- polynomial functions for Calc

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

(defun calcFunc-pcont (expr &optional var)
  (cond ((Math-primp expr)
	 (cond ((Math-zerop expr) 1)
	       ((Math-messy-integerp expr) (math-trunc expr))
	       ((Math-objectp expr) expr)
	       ((or (equal expr var) (not var)) 1)
	       (t expr)))
	((eq (car expr) '*)
	 (math-mul (calcFunc-pcont (nth 1 expr) var)
		   (calcFunc-pcont (nth 2 expr) var)))
	((eq (car expr) '/)
	 (math-div (calcFunc-pcont (nth 1 expr) var)
		   (calcFunc-pcont (nth 2 expr) var)))
	((and (eq (car expr) '^) (Math-natnump (nth 2 expr)))
	 (math-pow (calcFunc-pcont (nth 1 expr) var) (nth 2 expr)))
	((memq (car expr) '(neg polar))
	 (calcFunc-pcont (nth 1 expr) var))
	((consp var)
	 (let ((p (math-is-polynomial expr var)))
	   (if p
	       (let ((lead (nth (1- (length p)) p))
		     (cont (math-poly-gcd-list p)))
		 (if (math-guess-if-neg lead)
		     (math-neg cont)
		   cont))
	     1)))
	((memq (car expr) '(+ - cplx sdev))
	 (let ((cont (calcFunc-pcont (nth 1 expr) var)))
	   (if (eq cont 1)
	       1
	     (let ((c2 (calcFunc-pcont (nth 2 expr) var)))
	       (if (and (math-negp cont)
			(if (eq (car expr) '-) (math-posp c2) (math-negp c2)))
		   (math-neg (math-poly-gcd cont c2))
		 (math-poly-gcd cont c2))))))
	(var expr)
	(t 1)))

(defun calcFunc-pprim (expr &optional var)
  (let ((cont (calcFunc-pcont expr var)))
    (if (math-equal-int cont 1)
	expr
      (math-poly-div-exact expr cont var))))

(defun math-div-poly-const (expr c)
  (cond ((memq (car-safe expr) '(+ -))
	 (list (car expr)
	       (math-div-poly-const (nth 1 expr) c)
	       (math-div-poly-const (nth 2 expr) c)))
	(t (math-div expr c))))

(defun calcFunc-pdeg (expr &optional var)
  (if (Math-zerop expr)
      '(neg (var inf var-inf))
    (if var
	(or (math-polynomial-p expr var)
	    (math-reject-arg expr "Expected a polynomial"))
      (math-poly-degree expr))))

(defun math-poly-degree (expr)
  (cond ((Math-primp expr)
	 (if (eq (car-safe expr) 'var) 1 0))
	((eq (car expr) 'neg)
	 (math-poly-degree (nth 1 expr)))
	((eq (car expr) '*)
	 (+ (math-poly-degree (nth 1 expr))
	    (math-poly-degree (nth 2 expr))))
	((eq (car expr) '/)
	 (- (math-poly-degree (nth 1 expr))
	    (math-poly-degree (nth 2 expr))))
	((and (eq (car expr) '^) (natnump (nth 2 expr)))
	 (* (math-poly-degree (nth 1 expr)) (nth 2 expr)))
	((memq (car expr) '(+ -))
	 (max (math-poly-degree (nth 1 expr))
	      (math-poly-degree (nth 2 expr))))
	(t 1)))

(defun calcFunc-plead (expr var)
  (cond ((eq (car-safe expr) '*)
	 (math-mul (calcFunc-plead (nth 1 expr) var)
		   (calcFunc-plead (nth 2 expr) var)))
	((eq (car-safe expr) '/)
	 (math-div (calcFunc-plead (nth 1 expr) var)
		   (calcFunc-plead (nth 2 expr) var)))
	((and (eq (car-safe expr) '^) (math-natnump (nth 2 expr)))
	 (math-pow (calcFunc-plead (nth 1 expr) var) (nth 2 expr)))
	((Math-primp expr)
	 (if (equal expr var)
	     1
	   expr))
	(t
	 (let ((p (math-is-polynomial expr var)))
	   (if (cdr p)
	       (nth (1- (length p)) p)
	     1)))))





;;; Polynomial quotient, remainder, and GCD.
;;; Originally by Ove Ewerlid (ewerlid@mizar.DoCS.UU.SE).
;;; Modifications and simplifications by daveg.

(defvar math-poly-modulus 1)

;;; Return gcd of two polynomials
(defun calcFunc-pgcd (pn pd)
  (if (math-any-floats pn)
      (math-reject-arg pn "Coefficients must be rational"))
  (if (math-any-floats pd)
      (math-reject-arg pd "Coefficients must be rational"))
  (let ((calc-prefer-frac t)
	(math-poly-modulus (math-poly-modulus pn pd)))
    (math-poly-gcd pn pd)))

;;; Return only quotient to top of stack (nil if zero)

;; calc-poly-div-remainder is a local variable for
;; calc-poly-div (in calc-alg.el), but is used by
;; calcFunc-pdiv, which is called by calc-poly-div.
(defvar calc-poly-div-remainder)

(defun calcFunc-pdiv (pn pd &optional base)
  (let* ((calc-prefer-frac t)
	 (math-poly-modulus (math-poly-modulus pn pd))
	 (res (math-poly-div pn pd base)))
    (setq calc-poly-div-remainder (cdr res))
    (car res)))

;;; Return only remainder to top of stack
(defun calcFunc-prem (pn pd &optional base)
  (let ((calc-prefer-frac t)
	(math-poly-modulus (math-poly-modulus pn pd)))
    (cdr (math-poly-div pn pd base))))

(defun calcFunc-pdivrem (pn pd &optional base)
  (let* ((calc-prefer-frac t)
	 (math-poly-modulus (math-poly-modulus pn pd))
	 (res (math-poly-div pn pd base)))
    (list 'vec (car res) (cdr res))))

(defun calcFunc-pdivide (pn pd &optional base)
  (let* ((calc-prefer-frac t)
	 (math-poly-modulus (math-poly-modulus pn pd))
	 (res (math-poly-div pn pd base)))
    (math-add (car res) (math-div (cdr res) pd))))


;;; Multiply two terms, expanding out products of sums.
(defun math-mul-thru (lhs rhs)
  (if (memq (car-safe lhs) '(+ -))
      (list (car lhs)
	    (math-mul-thru (nth 1 lhs) rhs)
	    (math-mul-thru (nth 2 lhs) rhs))
    (if (memq (car-safe rhs) '(+ -))
	(list (car rhs)
	      (math-mul-thru lhs (nth 1 rhs))
	      (math-mul-thru lhs (nth 2 rhs)))
      (math-mul lhs rhs))))

(defun math-div-thru (num den)
  (if (memq (car-safe num) '(+ -))
      (list (car num)
	    (math-div-thru (nth 1 num) den)
	    (math-div-thru (nth 2 num) den))
    (math-div num den)))


;;; Sort the terms of a sum into canonical order.
(defun math-sort-terms (expr)
  (if (memq (car-safe expr) '(+ -))
      (math-list-to-sum
       (sort (math-sum-to-list expr)
	     (function (lambda (a b) (math-beforep (car a) (car b))))))
    expr))

(defun math-list-to-sum (lst)
  (if (cdr lst)
      (list (if (cdr (car lst)) '- '+)
	    (math-list-to-sum (cdr lst))
	    (car (car lst)))
    (if (cdr (car lst))
	(math-neg (car (car lst)))
      (car (car lst)))))

(defun math-sum-to-list (tree &optional neg)
  (cond ((eq (car-safe tree) '+)
	 (nconc (math-sum-to-list (nth 1 tree) neg)
		(math-sum-to-list (nth 2 tree) neg)))
	((eq (car-safe tree) '-)
	 (nconc (math-sum-to-list (nth 1 tree) neg)
		(math-sum-to-list (nth 2 tree) (not neg))))
	(t (list (cons tree neg)))))

;;; Check if the polynomial coefficients are modulo forms.
(defun math-poly-modulus (expr &optional expr2)
  (or (math-poly-modulus-rec expr)
      (and expr2 (math-poly-modulus-rec expr2))
      1))

(defun math-poly-modulus-rec (expr)
  (if (and (eq (car-safe expr) 'mod) (Math-natnump (nth 2 expr)))
      (list 'mod 1 (nth 2 expr))
    (and (memq (car-safe expr) '(+ - * /))
	 (or (math-poly-modulus-rec (nth 1 expr))
	     (math-poly-modulus-rec (nth 2 expr))))))


;;; Divide two polynomials.  Return (quotient . remainder).
(defvar math-poly-div-base nil)
(defun math-poly-div (u v &optional math-poly-div-base)
  (if math-poly-div-base
      (math-do-poly-div u v)
    (math-do-poly-div (calcFunc-expand u) (calcFunc-expand v))))

(defun math-poly-div-exact (u v &optional base)
  (let ((res (math-poly-div u v base)))
    (if (eq (cdr res) 0)
	(car res)
      (math-reject-arg (list 'vec u v) "Argument is not a polynomial"))))

(defun math-do-poly-div (u v)
  (cond ((math-constp u)
	 (if (math-constp v)
	     (cons (math-div u v) 0)
	   (cons 0 u)))
	((math-constp v)
	 (cons (if (eq v 1)
		   u
		 (if (memq (car-safe u) '(+ -))
		     (math-add-or-sub (math-poly-div-exact (nth 1 u) v)
				      (math-poly-div-exact (nth 2 u) v)
				      nil (eq (car u) '-))
		   (math-div u v)))
	       0))
	((Math-equal u v)
	 (cons math-poly-modulus 0))
	((and (math-atomic-factorp u) (math-atomic-factorp v))
	 (cons (math-simplify (math-div u v)) 0))
	(t
	 (let ((base (or math-poly-div-base
			 (math-poly-div-base u v)))
	       vp up res)
	   (if (or (null base)
		   (null (setq vp (math-is-polynomial v base nil 'gen))))
	       (cons 0 u)
	     (setq up (math-is-polynomial u base nil 'gen)
		   res (math-poly-div-coefs up vp))
	     (cons (math-build-polynomial-expr (car res) base)
		   (math-build-polynomial-expr (cdr res) base)))))))

(defun math-poly-div-rec (u v)
  (cond ((math-constp u)
	 (math-div u v))
	((math-constp v)
	 (if (eq v 1)
	     u
	   (if (memq (car-safe u) '(+ -))
	       (math-add-or-sub (math-poly-div-rec (nth 1 u) v)
				(math-poly-div-rec (nth 2 u) v)
				nil (eq (car u) '-))
	     (math-div u v))))
	((Math-equal u v) math-poly-modulus)
	((and (math-atomic-factorp u) (math-atomic-factorp v))
	 (math-simplify (math-div u v)))
	(math-poly-div-base
	 (math-div u v))
	(t
	 (let ((base (math-poly-div-base u v))
	       vp up res)
	   (if (or (null base)
		   (null (setq vp (math-is-polynomial v base nil 'gen))))
	       (math-div u v)
	     (setq up (math-is-polynomial u base nil 'gen)
		   res (math-poly-div-coefs up vp))
	     (math-add (math-build-polynomial-expr (car res) base)
		       (math-div (math-build-polynomial-expr (cdr res) base)
				 v)))))))

;;; Divide two polynomials in coefficient-list form.  Return (quot . rem).
(defun math-poly-div-coefs (u v)
  (cond ((null v) (math-reject-arg nil "Division by zero"))
	((< (length u) (length v)) (cons nil u))
	((cdr u)
	 (let ((q nil)
	       (urev (reverse u))
	       (vrev (reverse v)))
	   (while
	       (let ((qk (math-poly-div-rec (math-simplify (car urev))
					    (car vrev)))
		     (up urev)
		     (vp vrev))
		 (if (or q (not (math-zerop qk)))
		     (setq q (cons qk q)))
		 (while (setq up (cdr up) vp (cdr vp))
		   (setcar up (math-sub (car up) (math-mul-thru qk (car vp)))))
		 (setq urev (cdr urev))
		 up))
	   (while (and urev (Math-zerop (car urev)))
	     (setq urev (cdr urev)))
	   (cons q (nreverse (mapcar 'math-simplify urev)))))
	(t
	 (cons (list (math-poly-div-rec (car u) (car v)))
	       nil))))

;;; Perform a pseudo-division of polynomials.  (See Knuth section 4.6.1.)
;;; This returns only the remainder from the pseudo-division.
(defun math-poly-pseudo-div (u v)
  (cond ((null v) nil)
	((< (length u) (length v)) u)
	((or (cdr u) (cdr v))
	 (let ((urev (reverse u))
	       (vrev (reverse v))
	       up)
	   (while
	       (let ((vp vrev))
		 (setq up urev)
		 (while (setq up (cdr up) vp (cdr vp))
		   (setcar up (math-sub (math-mul-thru (car vrev) (car up))
					(math-mul-thru (car urev) (car vp)))))
		 (setq urev (cdr urev))
		 up)
	     (while up
	       (setcar up (math-mul-thru (car vrev) (car up)))
	       (setq up (cdr up))))
	   (while (and urev (Math-zerop (car urev)))
	     (setq urev (cdr urev)))
	   (nreverse (mapcar 'math-simplify urev))))
	(t nil)))

;;; Compute the GCD of two multivariate polynomials.
(defun math-poly-gcd (u v)
  (cond ((Math-equal u v) u)
	((math-constp u)
	 (if (Math-zerop u)
	     v
	   (calcFunc-gcd u (calcFunc-pcont v))))
	((math-constp v)
	 (if (Math-zerop v)
	     v
	   (calcFunc-gcd v (calcFunc-pcont u))))
	(t
	 (let ((base (math-poly-gcd-base u v)))
	   (if base
	       (math-simplify
		(calcFunc-expand
		 (math-build-polynomial-expr
		  (math-poly-gcd-coefs (math-is-polynomial u base nil 'gen)
				       (math-is-polynomial v base nil 'gen))
		  base)))
	     (calcFunc-gcd (calcFunc-pcont u) (calcFunc-pcont u)))))))

(defun math-poly-div-list (lst a)
  (if (eq a 1)
      lst
    (if (eq a -1)
	(math-mul-list lst a)
      (mapcar (function (lambda (x) (math-poly-div-exact x a))) lst))))

(defun math-mul-list (lst a)
  (if (eq a 1)
      lst
    (if (eq a -1)
	(mapcar 'math-neg lst)
      (and (not (eq a 0))
	   (mapcar (function (lambda (x) (math-mul x a))) lst)))))

;;; Run GCD on all elements in a list.
(defun math-poly-gcd-list (lst)
  (if (or (memq 1 lst) (memq -1 lst))
      (math-poly-gcd-frac-list lst)
    (let ((gcd (car lst)))
      (while (and (setq lst (cdr lst)) (not (eq gcd 1)))
	(or (eq (car lst) 0)
	    (setq gcd (math-poly-gcd gcd (car lst)))))
      (if lst (setq lst (math-poly-gcd-frac-list lst)))
      gcd)))

(defun math-poly-gcd-frac-list (lst)
  (while (and lst (not (eq (car-safe (car lst)) 'frac)))
    (setq lst (cdr lst)))
  (if lst
      (let ((denom (nth 2 (car lst))))
	(while (setq lst (cdr lst))
	  (if (eq (car-safe (car lst)) 'frac)
	      (setq denom (calcFunc-lcm denom (nth 2 (car lst))))))
	(list 'frac 1 denom))
    1))

;;; Compute the GCD of two univariate polynomial lists.
;;; Knuth section 4.6.1, algorithm C.
(defun math-poly-gcd-coefs (u v)
  (let ((d (math-poly-gcd (math-poly-gcd-list u)
			  (math-poly-gcd-list v)))
	(g 1) (h 1) (z 0) hh r delta ghd)
    (while (and u v (Math-zerop (car u)) (Math-zerop (car v)))
      (setq u (cdr u) v (cdr v) z (1+ z)))
    (or (eq d 1)
	(setq u (math-poly-div-list u d)
	      v (math-poly-div-list v d)))
    (while (progn
	     (setq delta (- (length u) (length v)))
	     (if (< delta 0)
		 (setq r u u v v r delta (- delta)))
	     (setq r (math-poly-pseudo-div u v))
	     (cdr r))
      (setq u v
	    v (math-poly-div-list r (math-mul g (math-pow h delta)))
	    g (nth (1- (length u)) u)
	    h (if (<= delta 1)
		  (math-mul (math-pow g delta) (math-pow h (- 1 delta)))
		(math-poly-div-exact (math-pow g delta)
				     (math-pow h (1- delta))))))
    (setq v (if r
		(list d)
	      (math-mul-list (math-poly-div-list v (math-poly-gcd-list v)) d)))
    (if (math-guess-if-neg (nth (1- (length v)) v))
	(setq v (math-mul-list v -1)))
    (while (>= (setq z (1- z)) 0)
      (setq v (cons 0 v)))
    v))


;;; Return true if is a factor containing no sums or quotients.
(defun math-atomic-factorp (expr)
  (cond ((eq (car-safe expr) '*)
	 (and (math-atomic-factorp (nth 1 expr))
	      (math-atomic-factorp (nth 2 expr))))
	((memq (car-safe expr) '(+ - /))
	 nil)
	((memq (car-safe expr) '(^ neg))
	 (math-atomic-factorp (nth 1 expr)))
	(t t)))

;;; Find a suitable base for dividing a by b.
;;; The base must exist in both expressions.
;;; The degree in the numerator must be higher or equal than the
;;; degree in the denominator.
;;; If the above conditions are not met the quotient is just a remainder.
;;; Return nil if this is the case.

(defun math-poly-div-base (a b)
  (let (a-base b-base)
    (and (setq a-base (math-total-polynomial-base a))
	 (setq b-base (math-total-polynomial-base b))
	 (catch 'return
	   (while a-base
	     (let ((maybe (assoc (car (car a-base)) b-base)))
	       (if maybe
		   (if (>= (nth 1 (car a-base)) (nth 1 maybe))
		       (throw 'return (car (car a-base))))))
	     (setq a-base (cdr a-base)))))))

;;; Same as above but for gcd algorithm.
;;; Here there is no requirement that degree(a) > degree(b).
;;; Take the base that has the highest degree considering both a and b.
;;; ("a^20+b^21+x^3+a+b", "a+b^2+x^5+a^22+b^10") --> (a 22)

(defun math-poly-gcd-base (a b)
  (let (a-base b-base)
    (and (setq a-base (math-total-polynomial-base a))
	 (setq b-base (math-total-polynomial-base b))
	 (catch 'return
	   (while (and a-base b-base)
	     (if (> (nth 1 (car a-base)) (nth 1 (car b-base)))
		 (if (assoc (car (car a-base)) b-base)
		     (throw 'return (car (car a-base)))
		   (setq a-base (cdr a-base)))
	       (if (assoc (car (car b-base)) a-base)
		   (throw 'return (car (car b-base)))
		 (setq b-base (cdr b-base)))))))))

;;; Sort a list of polynomial bases.
(defun math-sort-poly-base-list (lst)
  (sort lst (function (lambda (a b)
			(or (> (nth 1 a) (nth 1 b))
			    (and (= (nth 1 a) (nth 1 b))
				 (math-beforep (car a) (car b))))))))

;;; Given an expression find all variables that are polynomial bases.
;;; Return list in the form '( (var1 degree1) (var2 degree2) ... ).

;; The variable math-poly-base-total-base is local to
;; math-total-polynomial-base, but is used by math-polynomial-p1,
;; which is called by math-total-polynomial-base.
(defvar math-poly-base-total-base)

(defun math-total-polynomial-base (expr)
  (let ((math-poly-base-total-base nil))
    (math-polynomial-base expr 'math-polynomial-p1)
    (math-sort-poly-base-list math-poly-base-total-base)))

;; The variable math-poly-base-top-expr is local to math-polynomial-base
;; in calc-alg.el, but is used by math-polynomial-p1 which is called
;; by math-polynomial-base.
(defvar math-poly-base-top-expr)

(defun math-polynomial-p1 (subexpr)
  (or (assoc subexpr math-poly-base-total-base)
      (memq (car subexpr) '(+ - * / neg))
      (and (eq (car subexpr) '^) (natnump (nth 2 subexpr)))
      (let* ((math-poly-base-variable subexpr)
	     (exponent (math-polynomial-p math-poly-base-top-expr subexpr)))
	(if exponent
	    (setq math-poly-base-total-base (cons (list subexpr exponent)
				       math-poly-base-total-base)))))
  nil)

;; The variable math-factored-vars is local to calcFunc-factors and
;; calcFunc-factor, but is used by math-factor-expr and
;; math-factor-expr-part, which are called (directly and indirectly) by
;; calcFunc-factor and calcFunc-factors.
(defvar math-factored-vars)

;; The variable math-fact-expr is local to calcFunc-factors,
;; calcFunc-factor and math-factor-expr, but is used by math-factor-expr-try
;; and math-factor-expr-part, which are called (directly and indirectly) by
;; calcFunc-factor, calcFunc-factors and math-factor-expr.
(defvar math-fact-expr)

;; The variable math-to-list is local to calcFunc-factors and
;; calcFunc-factor, but is used by math-accum-factors, which is
;; called (indirectly) by calcFunc-factors and calcFunc-factor.
(defvar math-to-list)

(defun calcFunc-factors (math-fact-expr &optional var)
  (let ((math-factored-vars (if var t nil))
	(math-to-list t)
	(calc-prefer-frac t))
    (or var
	(setq var (math-polynomial-base math-fact-expr)))
    (let ((res (math-factor-finish
		(or (catch 'factor (math-factor-expr-try var))
		    math-fact-expr))))
      (math-simplify (if (math-vectorp res)
			 res
		       (list 'vec (list 'vec res 1)))))))

(defun calcFunc-factor (math-fact-expr &optional var)
  (let ((math-factored-vars nil)
	(math-to-list nil)
	(calc-prefer-frac t))
    (math-simplify (math-factor-finish
		    (if var
			(let ((math-factored-vars t))
			  (or (catch 'factor (math-factor-expr-try var)) math-fact-expr))
		      (math-factor-expr math-fact-expr))))))

(defun math-factor-finish (x)
  (if (Math-primp x)
      x
    (if (eq (car x) 'calcFunc-Fac-Prot)
	(math-factor-finish (nth 1 x))
      (cons (car x) (mapcar 'math-factor-finish (cdr x))))))

(defun math-factor-protect (x)
  (if (memq (car-safe x) '(+ -))
      (list 'calcFunc-Fac-Prot x)
    x))

(defun math-factor-expr (math-fact-expr)
  (cond ((eq math-factored-vars t) math-fact-expr)
	((or (memq (car-safe math-fact-expr) '(* / ^ neg))
	     (assq (car-safe math-fact-expr) calc-tweak-eqn-table))
	 (cons (car math-fact-expr) (mapcar 'math-factor-expr (cdr math-fact-expr))))
	((memq (car-safe math-fact-expr) '(+ -))
	 (let* ((math-factored-vars math-factored-vars)
		(y (catch 'factor (math-factor-expr-part math-fact-expr))))
	   (if y
	       (math-factor-expr y)
	     math-fact-expr)))
	(t math-fact-expr)))

(defun math-factor-expr-part (x)    ; uses "expr"
  (if (memq (car-safe x) '(+ - * / ^ neg))
      (while (setq x (cdr x))
	(math-factor-expr-part (car x)))
    (and (not (Math-objvecp x))
	 (not (assoc x math-factored-vars))
	 (> (math-factor-contains math-fact-expr x) 1)
	 (setq math-factored-vars (cons (list x) math-factored-vars))
	 (math-factor-expr-try x))))

;; The variable math-fet-x is local to math-factor-expr-try, but is
;; used by math-factor-poly-coefs, which is called by math-factor-expr-try.
(defvar math-fet-x)

(defun math-factor-expr-try (math-fet-x)
  (if (eq (car-safe math-fact-expr) '*)
      (let ((res1 (catch 'factor (let ((math-fact-expr (nth 1 math-fact-expr)))
				   (math-factor-expr-try math-fet-x))))
	    (res2 (catch 'factor (let ((math-fact-expr (nth 2 math-fact-expr)))
				   (math-factor-expr-try math-fet-x)))))
	(and (or res1 res2)
	     (throw 'factor (math-accum-factors (or res1 (nth 1 math-fact-expr)) 1
						(or res2 (nth 2 math-fact-expr))))))
    (let* ((p (math-is-polynomial math-fact-expr math-fet-x 30 'gen))
	   (math-poly-modulus (math-poly-modulus math-fact-expr))
	   res)
      (and (cdr p)
	   (setq res (math-factor-poly-coefs p))
	   (throw 'factor res)))))

(defun math-accum-factors (fac pow facs)
  (if math-to-list
      (if (math-vectorp fac)
	  (progn
	    (while (setq fac (cdr fac))
	      (setq facs (math-accum-factors (nth 1 (car fac))
					     (* pow (nth 2 (car fac)))
					     facs)))
	    facs)
	(if (and (eq (car-safe fac) '^) (natnump (nth 2 fac)))
	    (setq pow (* pow (nth 2 fac))
		  fac (nth 1 fac)))
	(if (eq fac 1)
	    facs
	  (or (math-vectorp facs)
	      (setq facs (if (eq facs 1) '(vec)
			   (list 'vec (list 'vec facs 1)))))
	  (let ((found facs))
	    (while (and (setq found (cdr found))
			(not (equal fac (nth 1 (car found))))))
	    (if found
		(progn
		  (setcar (cdr (cdr (car found))) (+ pow (nth 2 (car found))))
		  facs)
	      ;; Put constant term first.
	      (if (and (cdr facs) (Math-ratp (nth 1 (nth 1 facs))))
		  (cons 'vec (cons (nth 1 facs) (cons (list 'vec fac pow)
						      (cdr (cdr facs)))))
		(cons 'vec (cons (list 'vec fac pow) (cdr facs))))))))
    (math-mul (math-pow fac pow) (math-factor-protect facs))))

(defun math-factor-poly-coefs (p &optional square-free)    ; uses "x"
  (let (t1 t2 temp)
    (cond ((not (cdr p))
	   (or (car p) 0))

	  ;; Strip off multiples of math-fet-x.
	  ((Math-zerop (car p))
	   (let ((z 0))
	     (while (and p (Math-zerop (car p)))
	       (setq z (1+ z) p (cdr p)))
	     (if (cdr p)
		 (setq p (math-factor-poly-coefs p square-free))
	       (setq p (math-sort-terms (math-factor-expr (car p)))))
	     (math-accum-factors math-fet-x z (math-factor-protect p))))

	  ;; Factor out content.
	  ((and (not square-free)
		(not (eq 1 (setq t1 (math-mul (math-poly-gcd-list p)
					      (if (math-guess-if-neg
						   (nth (1- (length p)) p))
						  -1 1))))))
	   (math-accum-factors t1 1 (math-factor-poly-coefs
				     (math-poly-div-list p t1) 'cont)))

	  ;; Check if linear in math-fet-x.
	  ((not (cdr (cdr p)))
           (math-sort-terms
            (math-add (math-factor-protect
                       (math-sort-terms
                        (math-factor-expr (car p))))
                      (math-mul math-fet-x (math-factor-protect
                                            (math-sort-terms
                                             (math-factor-expr (nth 1 p))))))))

	  ;; If symbolic coefficients, use FactorRules.
	  ((let ((pp p))
	     (while (and pp (or (Math-ratp (car pp))
				(and (eq (car (car pp)) 'mod)
				     (Math-integerp (nth 1 (car pp)))
				     (Math-integerp (nth 2 (car pp))))))
	       (setq pp (cdr pp)))
	     pp)
	   (let ((res (math-rewrite
		       (list 'calcFunc-thecoefs math-fet-x (cons 'vec p))
		       '(var FactorRules var-FactorRules))))
	     (or (and (eq (car-safe res) 'calcFunc-thefactors)
		      (= (length res) 3)
		      (math-vectorp (nth 2 res))
		      (let ((facs 1)
			    (vec (nth 2 res)))
			(while (setq vec (cdr vec))
			  (setq facs (math-accum-factors (car vec) 1 facs)))
			facs))
		 (math-build-polynomial-expr p math-fet-x))))

	  ;; Check if rational coefficients (i.e., not modulo a prime).
	  ((eq math-poly-modulus 1)

	   ;; Check if there are any squared terms, or a content not = 1.
	   (if (or (eq square-free t)
		   (equal (setq t1 (math-poly-gcd-coefs
				    p (setq t2 (math-poly-deriv-coefs p))))
			  '(1)))

	       ;; We now have a square-free polynomial with integer coefs.
	       ;; For now, we use a kludgy method that finds linear and
	       ;; quadratic terms using floating-point root-finding.
	       (if (setq t1 (let ((calc-symbolic-mode nil))
			      (math-poly-all-roots nil p t)))
		   (let ((roots (car t1))
			 (csign (if (math-negp (nth (1- (length p)) p)) -1 1))
			 (expr 1)
			 (unfac (nth 1 t1))
			 (scale (nth 2 t1)))
		     (while roots
		       (let ((coef0 (car (car roots)))
			     (coef1 (cdr (car roots))))
			 (setq expr (math-accum-factors
				     (if coef1
					 (let ((den (math-lcm-denoms
						     coef0 coef1)))
					   (setq scale (math-div scale den))
					   (math-add
					    (math-add
					     (math-mul den (math-pow math-fet-x 2))
					     (math-mul (math-mul coef1 den)
                                                       math-fet-x))
					    (math-mul coef0 den)))
				       (let ((den (math-lcm-denoms coef0)))
					 (setq scale (math-div scale den))
					 (math-add (math-mul den math-fet-x)
						   (math-mul coef0 den))))
				     1 expr)
			       roots (cdr roots))))
		     (setq expr (math-accum-factors
				 expr 1
				 (math-mul csign
					   (math-build-polynomial-expr
					    (math-mul-list (nth 1 t1) scale)
					    math-fet-x)))))
		 (math-build-polynomial-expr p math-fet-x))   ; can't factor it.

	     ;; Separate out the squared terms (Knuth exercise 4.6.2-34).
	     ;; This step also divides out the content of the polynomial.
	     (let* ((cabs (math-poly-gcd-list p))
		    (csign (if (math-negp (nth (1- (length p)) p)) -1 1))
		    (t1s (math-mul-list t1 csign))
		    (uu nil)
		    (v (car (math-poly-div-coefs p t1s)))
		    (w (car (math-poly-div-coefs t2 t1s))))
	       (while
		   (not (math-poly-zerop
			 (setq t2 (math-poly-simplify
				   (math-poly-mix
				    w 1 (math-poly-deriv-coefs v) -1)))))
		 (setq t1 (math-poly-gcd-coefs v t2)
		       uu (cons t1 uu)
		       v (car (math-poly-div-coefs v t1))
		       w (car (math-poly-div-coefs t2 t1))))
	       (setq t1 (length uu)
		     t2 (math-accum-factors (math-factor-poly-coefs v t)
					    (1+ t1) 1))
	       (while uu
		 (setq t2 (math-accum-factors (math-factor-poly-coefs
					       (car uu) t)
					      t1 t2)
		       t1 (1- t1)
		       uu (cdr uu)))
	       (math-accum-factors (math-mul cabs csign) 1 t2))))

	  ;; Factoring modulo a prime.
	  ((and (= (length (setq temp (math-poly-gcd-coefs
				       p (math-poly-deriv-coefs p))))
		   (length p)))
	   (setq p (car temp))
	   (while (cdr temp)
	     (setq temp (nthcdr (nth 2 math-poly-modulus) temp)
		   p (cons (car temp) p)))
	   (and (setq temp (math-factor-poly-coefs p))
		(math-pow temp (nth 2 math-poly-modulus))))
	  (t
	   (math-reject-arg nil "*Modulo factorization not yet implemented")))))

(defun math-poly-deriv-coefs (p)
  (let ((n 1)
	(dp nil))
    (while (setq p (cdr p))
      (setq dp (cons (math-mul (car p) n) dp)
	    n (1+ n)))
    (nreverse dp)))

(defun math-factor-contains (x a)
  (if (equal x a)
      1
    (if (memq (car-safe x) '(+ - * / neg))
	(let ((sum 0))
	  (while (setq x (cdr x))
	    (setq sum (+ sum (math-factor-contains (car x) a))))
	  sum)
      (if (and (eq (car-safe x) '^)
	       (natnump (nth 2 x)))
	  (* (math-factor-contains (nth 1 x) a) (nth 2 x))
	0))))





;;; Merge all quotients and expand/simplify the numerator
(defun calcFunc-nrat (expr)
  (if (math-any-floats expr)
      (setq expr (calcFunc-pfrac expr)))
  (if (or (math-vectorp expr)
	  (assq (car-safe expr) calc-tweak-eqn-table))
      (cons (car expr) (mapcar 'calcFunc-nrat (cdr expr)))
    (let* ((calc-prefer-frac t)
	   (res (math-to-ratpoly expr))
	   (num (math-simplify (math-sort-terms (calcFunc-expand (car res)))))
	   (den (math-simplify (math-sort-terms (calcFunc-expand (cdr res)))))
	   (g (math-poly-gcd num den)))
      (or (eq g 1)
	  (let ((num2 (math-poly-div num g))
		(den2 (math-poly-div den g)))
	    (and (eq (cdr num2) 0) (eq (cdr den2) 0)
		 (setq num (car num2) den (car den2)))))
      (math-simplify (math-div num den)))))

;;; Returns expressions (num . denom).
(defun math-to-ratpoly (expr)
  (let ((res (math-to-ratpoly-rec expr)))
    (cons (math-simplify (car res)) (math-simplify (cdr res)))))

(defun math-to-ratpoly-rec (expr)
  (cond ((Math-primp expr)
	 (cons expr 1))
	((memq (car expr) '(+ -))
	 (let ((r1 (math-to-ratpoly-rec (nth 1 expr)))
	       (r2 (math-to-ratpoly-rec (nth 2 expr))))
	   (if (equal (cdr r1) (cdr r2))
	       (cons (list (car expr) (car r1) (car r2)) (cdr r1))
	     (if (eq (cdr r1) 1)
		 (cons (list (car expr)
			     (math-mul (car r1) (cdr r2))
			     (car r2))
		       (cdr r2))
	       (if (eq (cdr r2) 1)
		   (cons (list (car expr)
			       (car r1)
			       (math-mul (car r2) (cdr r1)))
			 (cdr r1))
		 (let ((g (math-poly-gcd (cdr r1) (cdr r2))))
		   (let ((d1 (and (not (eq g 1)) (math-poly-div (cdr r1) g)))
			 (d2 (and (not (eq g 1)) (math-poly-div
						  (math-mul (car r1) (cdr r2))
						  g))))
		     (if (and (eq (cdr d1) 0) (eq (cdr d2) 0))
			 (cons (list (car expr) (car d2)
				     (math-mul (car r2) (car d1)))
			       (math-mul (car d1) (cdr r2)))
		       (cons (list (car expr)
				   (math-mul (car r1) (cdr r2))
				   (math-mul (car r2) (cdr r1)))
			     (math-mul (cdr r1) (cdr r2)))))))))))
	((eq (car expr) '*)
	 (let* ((r1 (math-to-ratpoly-rec (nth 1 expr)))
		(r2 (math-to-ratpoly-rec (nth 2 expr)))
		(g (math-mul (math-poly-gcd (car r1) (cdr r2))
			     (math-poly-gcd (cdr r1) (car r2)))))
	   (if (eq g 1)
	       (cons (math-mul (car r1) (car r2))
		     (math-mul (cdr r1) (cdr r2)))
	     (cons (math-poly-div-exact (math-mul (car r1) (car r2)) g)
		   (math-poly-div-exact (math-mul (cdr r1) (cdr r2)) g)))))
	((eq (car expr) '/)
	 (let* ((r1 (math-to-ratpoly-rec (nth 1 expr)))
		(r2 (math-to-ratpoly-rec (nth 2 expr))))
	   (if (and (eq (cdr r1) 1) (eq (cdr r2) 1))
	       (cons (car r1) (car r2))
	     (let ((g (math-mul (math-poly-gcd (car r1) (car r2))
				(math-poly-gcd (cdr r1) (cdr r2)))))
	       (if (eq g 1)
		   (cons (math-mul (car r1) (cdr r2))
			 (math-mul (cdr r1) (car r2)))
		 (cons (math-poly-div-exact (math-mul (car r1) (cdr r2)) g)
		       (math-poly-div-exact (math-mul (cdr r1) (car r2))
					    g)))))))
	((and (eq (car expr) '^) (integerp (nth 2 expr)))
	 (let ((r1 (math-to-ratpoly-rec (nth 1 expr))))
	   (if (> (nth 2 expr) 0)
	       (cons (math-pow (car r1) (nth 2 expr))
		     (math-pow (cdr r1) (nth 2 expr)))
	     (cons (math-pow (cdr r1) (- (nth 2 expr)))
		   (math-pow (car r1) (- (nth 2 expr)))))))
	((eq (car expr) 'neg)
	 (let ((r1 (math-to-ratpoly-rec (nth 1 expr))))
	   (cons (math-neg (car r1)) (cdr r1))))
	(t (cons expr 1))))


(defun math-ratpoly-p (expr &optional var)
  (cond ((equal expr var) 1)
	((Math-primp expr) 0)
	((memq (car expr) '(+ -))
	 (let ((p1 (math-ratpoly-p (nth 1 expr) var))
	       p2)
	   (and p1 (setq p2 (math-ratpoly-p (nth 2 expr) var))
		(max p1 p2))))
	((eq (car expr) '*)
	 (let ((p1 (math-ratpoly-p (nth 1 expr) var))
	       p2)
	   (and p1 (setq p2 (math-ratpoly-p (nth 2 expr) var))
		(+ p1 p2))))
	((eq (car expr) 'neg)
	 (math-ratpoly-p (nth 1 expr) var))
	((eq (car expr) '/)
	 (let ((p1 (math-ratpoly-p (nth 1 expr) var))
	       p2)
	   (and p1 (setq p2 (math-ratpoly-p (nth 2 expr) var))
		(- p1 p2))))
	((and (eq (car expr) '^)
	      (integerp (nth 2 expr)))
	 (let ((p1 (math-ratpoly-p (nth 1 expr) var)))
	   (and p1 (* p1 (nth 2 expr)))))
	((not var) 1)
	((math-poly-depends expr var) nil)
	(t 0)))


(defun calcFunc-apart (expr &optional var)
  (cond ((Math-primp expr) expr)
	((eq (car expr) '+)
	 (math-add (calcFunc-apart (nth 1 expr) var)
		   (calcFunc-apart (nth 2 expr) var)))
	((eq (car expr) '-)
	 (math-sub (calcFunc-apart (nth 1 expr) var)
		   (calcFunc-apart (nth 2 expr) var)))
        ((and var (not (math-ratpoly-p expr var)))
         (math-reject-arg expr "Expected a rational function"))
	(t
         (let* ((calc-prefer-frac t)
                (rat (math-to-ratpoly expr))
                (num (car rat))
                (den (cdr rat)))
           (or var
               (setq var (math-polynomial-base den)))
           (if (not (math-ratpoly-p expr var))
               (math-reject-arg expr "Expected a rational function")
             (let* ((qr (math-poly-div num den))
                    (q (car qr))
                    (r (cdr qr)))
               (math-add q (or (and var
                                    (math-expr-contains den var)
                                    (math-partial-fractions r den var))
                               (math-div r den)))))))))


(defun math-padded-polynomial (expr var deg)
  "Return a polynomial as list of coefficients.
If EXPR is of the form \"a + bx + cx^2 + ...\" in the variable VAR, return
the list (a b c ...) with at least DEG elements, else return NIL."
  (let ((p (math-is-polynomial expr var deg)))
    (append p (make-list (- deg (length p)) 0))))

(defun math-partial-fractions (r den var)
  "Return R divided by DEN expressed in partial fractions of VAR.
All whole factors of DEN have already been split off from R.
If no partial fraction representation can be found, return nil."
  (let* ((fden (calcFunc-factors den var))
	 (tdeg (math-polynomial-p den var))
	 (fp fden)
	 (dlist nil)
	 (eqns 0)
	 (lz nil)
	 (tz (make-list (1- tdeg) 0))
	 (calc-matrix-mode 'scalar))
    (and (not (and (= (length fden) 2) (eq (nth 2 (nth 1 fden)) 1)))
	 (progn
	   (while (setq fp (cdr fp))
	     (let ((rpt (nth 2 (car fp)))
		   (deg (math-polynomial-p (nth 1 (car fp)) var))
		   dnum dvar deg2)
	       (while (> rpt 0)
		 (setq deg2 deg
		       dnum 0)
		 (while (> deg2 0)
		   (setq dvar (append '(vec) lz '(1) tz)
			 lz (cons 0 lz)
			 tz (cdr tz)
			 deg2 (1- deg2)
			 dnum (math-add dnum (math-mul dvar
						       (math-pow var deg2)))
			 dlist (cons (and (= deg2 (1- deg))
					  (math-pow (nth 1 (car fp)) rpt))
				     dlist)))
		 (let ((fpp fden)
		       (mult 1))
		   (while (setq fpp (cdr fpp))
		     (or (eq fpp fp)
			 (setq mult (math-mul mult
					      (math-pow (nth 1 (car fpp))
							(nth 2 (car fpp)))))))
		   (setq dnum (math-mul dnum mult)))
		 (setq eqns (math-add eqns (math-mul dnum
						     (math-pow
						      (nth 1 (car fp))
						      (- (nth 2 (car fp))
							 rpt))))
		       rpt (1- rpt)))))
	   (setq eqns (math-div (cons 'vec (math-padded-polynomial r var tdeg))
				(math-transpose
				 (cons 'vec
				       (mapcar
					(function
					 (lambda (x)
					   (cons 'vec (math-padded-polynomial
						       x var tdeg))))
					(cdr eqns))))))
	   (and (math-vectorp eqns)
		(let ((res 0)
		      (num nil))
		  (setq eqns (nreverse eqns))
		  (while eqns
		    (setq num (cons (car eqns) num)
			  eqns (cdr eqns))
		    (if (car dlist)
			(setq num (math-build-polynomial-expr
				   (nreverse num) var)
			      res (math-add res (math-div num (car dlist)))
			      num nil))
		    (setq dlist (cdr dlist)))
		  (math-normalize res)))))))



(defun math-expand-term (expr)
  (cond ((and (eq (car-safe expr) '*)
	      (memq (car-safe (nth 1 expr)) '(+ -)))
	 (math-add-or-sub (list '* (nth 1 (nth 1 expr)) (nth 2 expr))
			  (list '* (nth 2 (nth 1 expr)) (nth 2 expr))
			  nil (eq (car (nth 1 expr)) '-)))
	((and (eq (car-safe expr) '*)
	      (memq (car-safe (nth 2 expr)) '(+ -)))
	 (math-add-or-sub (list '* (nth 1 expr) (nth 1 (nth 2 expr)))
			  (list '* (nth 1 expr) (nth 2 (nth 2 expr)))
			  nil (eq (car (nth 2 expr)) '-)))
	((and (eq (car-safe expr) '/)
	      (memq (car-safe (nth 1 expr)) '(+ -)))
	 (math-add-or-sub (list '/ (nth 1 (nth 1 expr)) (nth 2 expr))
			  (list '/ (nth 2 (nth 1 expr)) (nth 2 expr))
			  nil (eq (car (nth 1 expr)) '-)))
	((and (eq (car-safe expr) '^)
	      (memq (car-safe (nth 1 expr)) '(+ -))
	      (integerp (nth 2 expr))
              (if (and
                   (or (math-known-matrixp (nth 1 (nth 1 expr)))
                       (math-known-matrixp (nth 2 (nth 1 expr)))
                       (and
                        calc-matrix-mode
                        (not (eq calc-matrix-mode 'scalar))
                        (not (and (math-known-scalarp (nth 1 (nth 1 expr)))
                                  (math-known-scalarp (nth 2 (nth 1 expr)))))))
                   (> (nth 2 expr) 1))
                  (if (= (nth 2 expr) 2)
                      (math-add-or-sub (list '* (nth 1 (nth 1 expr)) (nth 1 expr))
                                       (list '* (nth 2 (nth 1 expr)) (nth 1 expr))
                                       nil (eq (car (nth 1 expr)) '-))
                    (math-add-or-sub (list '* (nth 1 (nth 1 expr))
                                           (list '^ (nth 1 expr)
                                                 (1- (nth 2 expr))))
                                     (list '* (nth 2 (nth 1 expr))
                                           (list '^ (nth 1 expr)
                                                 (1- (nth 2 expr))))
                                     nil (eq (car (nth 1 expr)) '-)))
                (if (> (nth 2 expr) 0)
                    (or (and (or (> math-mt-many 500000) (< math-mt-many -500000))
                             (math-expand-power (nth 1 expr) (nth 2 expr)
                                                nil t))
                        (list '*
                              (nth 1 expr)
                              (list '^ (nth 1 expr) (1- (nth 2 expr)))))
                  (if (< (nth 2 expr) 0)
                      (list '/ 1 (list '^ (nth 1 expr) (- (nth 2 expr)))))))))
	(t expr)))

(defun calcFunc-expand (expr &optional many)
  (math-normalize (math-map-tree 'math-expand-term expr many)))

(defun math-expand-power (x n &optional var else-nil)
  (or (and (natnump n)
	   (memq (car-safe x) '(+ -))
	   (let ((terms nil)
		 (cterms nil))
	     (while (memq (car-safe x) '(+ -))
	       (setq terms (cons (if (eq (car x) '-)
				     (math-neg (nth 2 x))
				   (nth 2 x))
				 terms)
		     x (nth 1 x)))
	     (setq terms (cons x terms))
	     (if var
		 (let ((p terms))
		   (while p
		     (or (math-expr-contains (car p) var)
			 (setq terms (delq (car p) terms)
			       cterms (cons (car p) cterms)))
		     (setq p (cdr p)))
		   (if cterms
		       (setq terms (cons (apply 'calcFunc-add cterms)
					 terms)))))
	     (if (= (length terms) 2)
		 (let ((i 0)
		       (accum 0))
		   (while (<= i n)
		     (setq accum (list '+ accum
				       (list '* (calcFunc-choose n i)
					     (list '*
						   (list '^ (nth 1 terms) i)
						   (list '^ (car terms)
							 (- n i)))))
			   i (1+ i)))
		   accum)
	       (if (= n 2)
		   (let ((accum 0)
			 (p1 terms)
			 p2)
		     (while p1
		       (setq accum (list '+ accum
					 (list '^ (car p1) 2))
			     p2 p1)
		       (while (setq p2 (cdr p2))
			 (setq accum (list '+ accum
					   (list '* 2 (list '*
							    (car p1)
							    (car p2))))))
		       (setq p1 (cdr p1)))
		     accum)
		 (if (= n 3)
		     (let ((accum 0)
			   (p1 terms)
			   p2 p3)
		       (while p1
			 (setq accum (list '+ accum (list '^ (car p1) 3))
			       p2 p1)
			 (while (setq p2 (cdr p2))
			   (setq accum (list '+
					     (list '+
						   accum
						   (list '* 3
							 (list
							  '*
							  (list '^ (car p1) 2)
							  (car p2))))
					     (list '* 3
						   (list
						    '* (car p1)
						    (list '^ (car p2) 2))))
				 p3 p2)
			   (while (setq p3 (cdr p3))
			     (setq accum (list '+ accum
					       (list '* 6
						     (list '*
							   (car p1)
							   (list
							    '* (car p2)
							    (car p3))))))))
			 (setq p1 (cdr p1)))
		       accum))))))
      (and (not else-nil)
	   (list '^ x n))))

(defun calcFunc-expandpow (x n)
  (math-normalize (math-expand-power x n)))

(provide 'calc-poly)

;;; calc-poly.el ends here
