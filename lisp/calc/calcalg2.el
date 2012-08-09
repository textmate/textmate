;;; calcalg2.el --- more algebraic functions for Calc

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

(defun calc-derivative (var num)
  (interactive "sDifferentiate with respect to: \np")
  (calc-slow-wrapper
   (when (< num 0)
     (error "Order of derivative must be positive"))
   (let ((func (if (calc-is-hyperbolic) 'calcFunc-tderiv 'calcFunc-deriv))
	 n expr)
     (if (or (equal var "") (equal var "$"))
	 (setq n 2
	       expr (calc-top-n 2)
	       var (calc-top-n 1))
       (setq var (math-read-expr var))
       (when (eq (car-safe var) 'error)
	 (error "Bad format in expression: %s" (nth 1 var)))
       (setq n 1
	     expr (calc-top-n 1)))
     (while (>= (setq num (1- num)) 0)
       (setq expr (list func expr var)))
     (calc-enter-result n "derv" expr))))

(defun calc-integral (var &optional arg)
  (interactive "sIntegration variable: \nP")
  (if arg
      (calc-tabular-command 'calcFunc-integ "Integration" "intg" nil var nil nil)
    (calc-slow-wrapper
     (if (or (equal var "") (equal var "$"))
         (calc-enter-result 2 "intg" (list 'calcFunc-integ
                                           (calc-top-n 2)
                                           (calc-top-n 1)))
       (let ((var (math-read-expr var)))
         (if (eq (car-safe var) 'error)
             (error "Bad format in expression: %s" (nth 1 var)))
         (calc-enter-result 1 "intg" (list 'calcFunc-integ
                                           (calc-top-n 1)
                                           var)))))))

(defun calc-num-integral (&optional varname lowname highname)
  (interactive "sIntegration variable: ")
  (calc-tabular-command 'calcFunc-ninteg "Integration" "nint"
			nil varname lowname highname))

(defun calc-summation (arg &optional varname lowname highname)
  (interactive "P\nsSummation variable: ")
  (calc-tabular-command 'calcFunc-sum "Summation" "sum"
			arg varname lowname highname))

(defun calc-alt-summation (arg &optional varname lowname highname)
  (interactive "P\nsSummation variable: ")
  (calc-tabular-command 'calcFunc-asum "Summation" "asum"
			arg varname lowname highname))

(defun calc-product (arg &optional varname lowname highname)
  (interactive "P\nsIndex variable: ")
  (calc-tabular-command 'calcFunc-prod "Index" "prod"
			arg varname lowname highname))

(defun calc-tabulate (arg &optional varname lowname highname)
  (interactive "P\nsIndex variable: ")
  (calc-tabular-command 'calcFunc-table "Index" "tabl"
			arg varname lowname highname))

(defun calc-tabular-command (func prompt prefix arg varname lowname highname)
  (calc-slow-wrapper
   (let (var (low nil) (high nil) (step nil) stepname stepnum (num 1) expr)
     (if (consp arg)
	 (setq stepnum 1)
       (setq stepnum 0))
     (if (or (equal varname "") (equal varname "$") (null varname))
	 (setq high (calc-top-n (+ stepnum 1))
	       low (calc-top-n (+ stepnum 2))
	       var (calc-top-n (+ stepnum 3))
	       num (+ stepnum 4))
       (setq var (if (stringp varname) (math-read-expr varname) varname))
       (if (eq (car-safe var) 'error)
	   (error "Bad format in expression: %s" (nth 1 var)))
       (or lowname
	   (setq lowname (read-string (concat prompt " variable: " varname
					      ", from: "))))
       (if (or (equal lowname "") (equal lowname "$"))
	   (setq high (calc-top-n (+ stepnum 1))
		 low (calc-top-n (+ stepnum 2))
		 num (+ stepnum 3))
	 (setq low (if (stringp lowname) (math-read-expr lowname) lowname))
	 (if (eq (car-safe low) 'error)
	     (error "Bad format in expression: %s" (nth 1 low)))
	 (or highname
	     (setq highname (read-string (concat prompt " variable: " varname
						 ", from: " lowname
						 ", to: "))))
	 (if (or (equal highname "") (equal highname "$"))
	     (setq high (calc-top-n (+ stepnum 1))
		   num (+ stepnum 2))
	   (setq high (if (stringp highname) (math-read-expr highname)
			highname))
	   (if (eq (car-safe high) 'error)
	       (error "Bad format in expression: %s" (nth 1 high)))
	   (if (consp arg)
	       (progn
		 (setq stepname (read-string (concat prompt " variable: "
						     varname
						     ", from: " lowname
						     ", to: " highname
						     ", step: ")))
		 (if (or (equal stepname "") (equal stepname "$"))
		     (setq step (calc-top-n 1)
			   num 2)
		   (setq step (math-read-expr stepname))
		   (if (eq (car-safe step) 'error)
		       (error "Bad format in expression: %s"
			      (nth 1 step)))))))))
     (or step
	 (if (consp arg)
	     (setq step (calc-top-n 1))
	   (if arg
	       (setq step (prefix-numeric-value arg)))))
     (setq expr (calc-top-n num))
     (calc-enter-result num prefix (append (list func expr var low high)
					   (and step (list step)))))))

(defun calc-solve-for (var)
  (interactive "sVariable(s) to solve for: ")
  (calc-slow-wrapper
   (let ((func (if (calc-is-inverse)
		   (if (calc-is-hyperbolic) 'calcFunc-ffinv 'calcFunc-finv)
		 (if (calc-is-hyperbolic) 'calcFunc-fsolve 'calcFunc-solve))))
     (if (or (equal var "") (equal var "$"))
	 (calc-enter-result 2 "solv" (list func
					   (calc-top-n 2)
					   (calc-top-n 1)))
       (let ((var (if (and (string-match ",\\|[^ ] +[^ ]" var)
			   (not (string-match "\\[" var)))
		      (math-read-expr (concat "[" var "]"))
		    (math-read-expr var))))
	 (if (eq (car-safe var) 'error)
	     (error "Bad format in expression: %s" (nth 1 var)))
	 (calc-enter-result 1 "solv" (list func
					   (calc-top-n 1)
					   var)))))))

(defun calc-poly-roots (var)
  (interactive "sVariable to solve for: ")
  (calc-slow-wrapper
   (if (or (equal var "") (equal var "$"))
       (calc-enter-result 2 "prts" (list 'calcFunc-roots
					 (calc-top-n 2)
					 (calc-top-n 1)))
     (let ((var (if (and (string-match ",\\|[^ ] +[^ ]" var)
			 (not (string-match "\\[" var)))
		    (math-read-expr (concat "[" var "]"))
		  (math-read-expr var))))
       (if (eq (car-safe var) 'error)
	   (error "Bad format in expression: %s" (nth 1 var)))
       (calc-enter-result 1 "prts" (list 'calcFunc-roots
					 (calc-top-n 1)
					 var))))))

(defun calc-taylor (var nterms)
  (interactive "sTaylor expansion variable: \nNNumber of terms: ")
  (calc-slow-wrapper
   (let ((var (math-read-expr var)))
     (if (eq (car-safe var) 'error)
	 (error "Bad format in expression: %s" (nth 1 var)))
     (calc-enter-result 1 "tylr" (list 'calcFunc-taylor
				       (calc-top-n 1)
				       var
				       (prefix-numeric-value nterms))))))


;; The following are global variables used by math-derivative and some
;; related functions
(defvar math-deriv-var)
(defvar math-deriv-total)
(defvar math-deriv-symb)
(defvar math-decls-cache)
(defvar math-decls-all)

(defun math-derivative (expr)
  (cond ((equal expr math-deriv-var)
	 1)
	((or (Math-scalarp expr)
	     (eq (car expr) 'sdev)
	     (and (eq (car expr) 'var)
		  (or (not math-deriv-total)
		      (math-const-var expr)
		      (progn
			(math-setup-declarations)
			(memq 'const (nth 1 (or (assq (nth 2 expr)
						      math-decls-cache)
						math-decls-all)))))))
	 0)
	((eq (car expr) '+)
	 (math-add (math-derivative (nth 1 expr))
		   (math-derivative (nth 2 expr))))
	((eq (car expr) '-)
	 (math-sub (math-derivative (nth 1 expr))
		   (math-derivative (nth 2 expr))))
	((memq (car expr) '(calcFunc-eq calcFunc-neq calcFunc-lt
					calcFunc-gt calcFunc-leq calcFunc-geq))
	 (list (car expr)
	       (math-derivative (nth 1 expr))
	       (math-derivative (nth 2 expr))))
	((eq (car expr) 'neg)
	 (math-neg (math-derivative (nth 1 expr))))
	((eq (car expr) '*)
	 (math-add (math-mul (nth 2 expr)
			     (math-derivative (nth 1 expr)))
		   (math-mul (nth 1 expr)
			     (math-derivative (nth 2 expr)))))
	((eq (car expr) '/)
	 (math-sub (math-div (math-derivative (nth 1 expr))
			     (nth 2 expr))
		   (math-div (math-mul (nth 1 expr)
				       (math-derivative (nth 2 expr)))
			     (math-sqr (nth 2 expr)))))
	((eq (car expr) '^)
	 (let ((du (math-derivative (nth 1 expr)))
	       (dv (math-derivative (nth 2 expr))))
	   (or (Math-zerop du)
	       (setq du (math-mul (nth 2 expr)
				  (math-mul (math-normalize
					     (list '^
						   (nth 1 expr)
						   (math-add (nth 2 expr) -1)))
					    du))))
	   (or (Math-zerop dv)
	       (setq dv (math-mul (math-normalize
				   (list 'calcFunc-ln (nth 1 expr)))
				  (math-mul expr dv))))
	   (math-add du dv)))
	((eq (car expr) '%)
	 (math-derivative (nth 1 expr)))   ; a reasonable definition
	((eq (car expr) 'vec)
	 (math-map-vec 'math-derivative expr))
	((and (memq (car expr) '(calcFunc-conj calcFunc-re calcFunc-im))
	      (= (length expr) 2))
	 (list (car expr) (math-derivative (nth 1 expr))))
	((and (memq (car expr) '(calcFunc-subscr calcFunc-mrow calcFunc-mcol))
	      (= (length expr) 3))
	 (let ((d (math-derivative (nth 1 expr))))
	   (if (math-numberp d)
	       0    ; assume x and x_1 are independent vars
	     (list (car expr) d (nth 2 expr)))))
	(t (or (and (symbolp (car expr))
		    (if (= (length expr) 2)
			(let ((handler (get (car expr) 'math-derivative)))
			  (and handler
			       (let ((deriv (math-derivative (nth 1 expr))))
				 (if (Math-zerop deriv)
				     deriv
				   (math-mul (funcall handler (nth 1 expr))
					     deriv)))))
		      (let ((handler (get (car expr) 'math-derivative-n)))
			(and handler
			     (funcall handler expr)))))
	       (and (not (eq math-deriv-symb 'pre-expand))
		    (let ((exp (math-expand-formula expr)))
		      (and exp
			   (or (let ((math-deriv-symb 'pre-expand))
				 (catch 'math-deriv (math-derivative expr)))
			       (math-derivative exp)))))
	       (if (or (Math-objvecp expr)
		       (eq (car expr) 'var)
		       (not (symbolp (car expr))))
		   (if math-deriv-symb
		       (throw 'math-deriv nil)
		     (list (if math-deriv-total 'calcFunc-tderiv 'calcFunc-deriv)
			   expr
			   math-deriv-var))
		 (let ((accum 0)
		       (arg expr)
		       (n 1)
		       derv)
		   (while (setq arg (cdr arg))
		     (or (Math-zerop (setq derv (math-derivative (car arg))))
			 (let ((func (intern (concat (symbol-name (car expr))
						     "'"
						     (if (> n 1)
							 (int-to-string n)
						       ""))))
			       (prop (cond ((= (length expr) 2)
					    'math-derivative-1)
					   ((= (length expr) 3)
					    'math-derivative-2)
					   ((= (length expr) 4)
					    'math-derivative-3)
					   ((= (length expr) 5)
					    'math-derivative-4)
					   ((= (length expr) 6)
					    'math-derivative-5))))
			   (setq accum
				 (math-add
				  accum
				  (math-mul
				   derv
				   (let ((handler (get func prop)))
				     (or (and prop handler
					      (apply handler (cdr expr)))
					 (if (and math-deriv-symb
						  (not (get func
							    'calc-user-defn)))
					     (throw 'math-deriv nil)
					   (cons func (cdr expr))))))))))
		     (setq n (1+ n)))
		   accum))))))

(defun calcFunc-deriv (expr math-deriv-var &optional deriv-value math-deriv-symb)
  (let* ((math-deriv-total nil)
	 (res (catch 'math-deriv (math-derivative expr))))
    (or (eq (car-safe res) 'calcFunc-deriv)
	(null res)
	(setq res (math-normalize res)))
    (and res
	 (if deriv-value
	     (math-expr-subst res math-deriv-var deriv-value)
	   res))))

(defun calcFunc-tderiv (expr math-deriv-var &optional deriv-value math-deriv-symb)
  (math-setup-declarations)
  (let* ((math-deriv-total t)
	 (res (catch 'math-deriv (math-derivative expr))))
    (or (eq (car-safe res) 'calcFunc-tderiv)
	(null res)
	(setq res (math-normalize res)))
    (and res
	 (if deriv-value
	     (math-expr-subst res math-deriv-var deriv-value)
	   res))))

(put 'calcFunc-inv\' 'math-derivative-1
     (function (lambda (u) (math-neg (math-div 1 (math-sqr u))))))

(put 'calcFunc-sqrt\' 'math-derivative-1
     (function (lambda (u) (math-div 1 (math-mul 2 (list 'calcFunc-sqrt u))))))

(put 'calcFunc-deg\' 'math-derivative-1
     (function (lambda (u) (math-div-float '(float 18 1) (math-pi)))))

(put 'calcFunc-rad\' 'math-derivative-1
     (function (lambda (u) (math-pi-over-180))))

(put 'calcFunc-ln\' 'math-derivative-1
     (function (lambda (u) (math-div 1 u))))

(put 'calcFunc-log10\' 'math-derivative-1
     (function (lambda (u)
		 (math-div (math-div 1 (math-normalize '(calcFunc-ln 10)))
			   u))))

(put 'calcFunc-lnp1\' 'math-derivative-1
     (function (lambda (u) (math-div 1 (math-add u 1)))))

(put 'calcFunc-log\' 'math-derivative-2
     (function (lambda (x b)
		 (and (not (Math-zerop b))
		      (let ((lnv (math-normalize
				  (list 'calcFunc-ln b))))
			(math-div 1 (math-mul lnv x)))))))

(put 'calcFunc-log\'2 'math-derivative-2
     (function (lambda (x b)
		 (let ((lnv (list 'calcFunc-ln b)))
		   (math-neg (math-div (list 'calcFunc-log x b)
				       (math-mul lnv b)))))))

(put 'calcFunc-exp\' 'math-derivative-1
     (function (lambda (u) (math-normalize (list 'calcFunc-exp u)))))

(put 'calcFunc-expm1\' 'math-derivative-1
     (function (lambda (u) (math-normalize (list 'calcFunc-expm1 u)))))

(put 'calcFunc-sin\' 'math-derivative-1
     (function (lambda (u) (math-to-radians-2 (math-normalize
					       (list 'calcFunc-cos u))))))

(put 'calcFunc-cos\' 'math-derivative-1
     (function (lambda (u) (math-neg (math-to-radians-2
				      (math-normalize
				       (list 'calcFunc-sin u)))))))

(put 'calcFunc-tan\' 'math-derivative-1
     (function (lambda (u) (math-to-radians-2
			    (math-sqr
                             (math-normalize
                              (list 'calcFunc-sec u)))))))

(put 'calcFunc-sec\' 'math-derivative-1
     (function (lambda (u) (math-to-radians-2
                            (math-mul
                             (math-normalize
                              (list 'calcFunc-sec u))
                             (math-normalize
                              (list 'calcFunc-tan u)))))))

(put 'calcFunc-csc\' 'math-derivative-1
     (function (lambda (u) (math-neg
                            (math-to-radians-2
                             (math-mul
                              (math-normalize
                               (list 'calcFunc-csc u))
                              (math-normalize
                               (list 'calcFunc-cot u))))))))

(put 'calcFunc-cot\' 'math-derivative-1
     (function (lambda (u) (math-neg
                            (math-to-radians-2
                             (math-sqr
                              (math-normalize
                               (list 'calcFunc-csc u))))))))

(put 'calcFunc-arcsin\' 'math-derivative-1
     (function (lambda (u)
		 (math-from-radians-2
		  (math-div 1 (math-normalize
			       (list 'calcFunc-sqrt
				     (math-sub 1 (math-sqr u)))))))))

(put 'calcFunc-arccos\' 'math-derivative-1
     (function (lambda (u)
		 (math-from-radians-2
		  (math-div -1 (math-normalize
				(list 'calcFunc-sqrt
				      (math-sub 1 (math-sqr u)))))))))

(put 'calcFunc-arctan\' 'math-derivative-1
     (function (lambda (u) (math-from-radians-2
			    (math-div 1 (math-add 1 (math-sqr u)))))))

(put 'calcFunc-sinh\' 'math-derivative-1
     (function (lambda (u) (math-normalize (list 'calcFunc-cosh u)))))

(put 'calcFunc-cosh\' 'math-derivative-1
     (function (lambda (u) (math-normalize (list 'calcFunc-sinh u)))))

(put 'calcFunc-tanh\' 'math-derivative-1
     (function (lambda (u) (math-sqr
                            (math-normalize
                             (list 'calcFunc-sech u))))))

(put 'calcFunc-sech\' 'math-derivative-1
     (function (lambda (u) (math-neg
                            (math-mul
                             (math-normalize (list 'calcFunc-sech u))
                             (math-normalize (list 'calcFunc-tanh u)))))))

(put 'calcFunc-csch\' 'math-derivative-1
     (function (lambda (u) (math-neg
                            (math-mul
                             (math-normalize (list 'calcFunc-csch u))
                             (math-normalize (list 'calcFunc-coth u)))))))

(put 'calcFunc-coth\' 'math-derivative-1
     (function (lambda (u) (math-neg
                            (math-sqr
                             (math-normalize
                              (list 'calcFunc-csch u)))))))

(put 'calcFunc-arcsinh\' 'math-derivative-1
     (function (lambda (u)
		 (math-div 1 (math-normalize
			      (list 'calcFunc-sqrt
				    (math-add (math-sqr u) 1)))))))

(put 'calcFunc-arccosh\' 'math-derivative-1
     (function (lambda (u)
		  (math-div 1 (math-normalize
			       (list 'calcFunc-sqrt
				     (math-add (math-sqr u) -1)))))))

(put 'calcFunc-arctanh\' 'math-derivative-1
     (function (lambda (u) (math-div 1 (math-sub 1 (math-sqr u))))))

(put 'calcFunc-bern\'2 'math-derivative-2
     (function (lambda (n x)
		 (math-mul n (list 'calcFunc-bern (math-add n -1) x)))))

(put 'calcFunc-euler\'2 'math-derivative-2
     (function (lambda (n x)
		 (math-mul n (list 'calcFunc-euler (math-add n -1) x)))))

(put 'calcFunc-gammag\'2 'math-derivative-2
     (function (lambda (a x) (math-deriv-gamma a x 1))))

(put 'calcFunc-gammaG\'2 'math-derivative-2
     (function (lambda (a x) (math-deriv-gamma a x -1))))

(put 'calcFunc-gammaP\'2 'math-derivative-2
     (function (lambda (a x) (math-deriv-gamma a x
					       (math-div
						1 (math-normalize
						   (list 'calcFunc-gamma
							 a)))))))

(put 'calcFunc-gammaQ\'2 'math-derivative-2
     (function (lambda (a x) (math-deriv-gamma a x
					       (math-div
						-1 (math-normalize
						    (list 'calcFunc-gamma
							  a)))))))

(defun math-deriv-gamma (a x scale)
  (math-mul scale
	    (math-mul (math-pow x (math-add a -1))
		      (list 'calcFunc-exp (math-neg x)))))

(put 'calcFunc-betaB\' 'math-derivative-3
     (function (lambda (x a b) (math-deriv-beta x a b 1))))

(put 'calcFunc-betaI\' 'math-derivative-3
     (function (lambda (x a b) (math-deriv-beta x a b
						(math-div
						 1 (list 'calcFunc-beta
							 a b))))))

(defun math-deriv-beta (x a b scale)
  (math-mul (math-mul (math-pow x (math-add a -1))
		      (math-pow (math-sub 1 x) (math-add b -1)))
	    scale))

(put 'calcFunc-erf\' 'math-derivative-1
     (function (lambda (x) (math-div 2
				     (math-mul (list 'calcFunc-exp
						     (math-sqr x))
					       (if calc-symbolic-mode
						   '(calcFunc-sqrt
						     (var pi var-pi))
						 (math-sqrt-pi)))))))

(put 'calcFunc-erfc\' 'math-derivative-1
     (function (lambda (x) (math-div -2
				     (math-mul (list 'calcFunc-exp
						     (math-sqr x))
					       (if calc-symbolic-mode
						   '(calcFunc-sqrt
						     (var pi var-pi))
						 (math-sqrt-pi)))))))

(put 'calcFunc-besJ\'2 'math-derivative-2
     (function (lambda (v z) (math-div (math-sub (list 'calcFunc-besJ
						       (math-add v -1)
						       z)
						 (list 'calcFunc-besJ
						       (math-add v 1)
						       z))
				       2))))

(put 'calcFunc-besY\'2 'math-derivative-2
     (function (lambda (v z) (math-div (math-sub (list 'calcFunc-besY
						       (math-add v -1)
						       z)
						 (list 'calcFunc-besY
						       (math-add v 1)
						       z))
				       2))))

(put 'calcFunc-sum 'math-derivative-n
     (function
      (lambda (expr)
	(if (math-expr-contains (cons 'vec (cdr (cdr expr))) math-deriv-var)
	    (throw 'math-deriv nil)
	  (cons 'calcFunc-sum
		(cons (math-derivative (nth 1 expr))
		      (cdr (cdr expr))))))))

(put 'calcFunc-prod 'math-derivative-n
     (function
      (lambda (expr)
	(if (math-expr-contains (cons 'vec (cdr (cdr expr))) math-deriv-var)
	    (throw 'math-deriv nil)
	  (math-mul expr
		    (cons 'calcFunc-sum
			  (cons (math-div (math-derivative (nth 1 expr))
					  (nth 1 expr))
				(cdr (cdr expr)))))))))

(put 'calcFunc-integ 'math-derivative-n
     (function
      (lambda (expr)
	(if (= (length expr) 3)
	    (if (equal (nth 2 expr) math-deriv-var)
		(nth 1 expr)
	      (math-normalize
	       (list 'calcFunc-integ
		     (math-derivative (nth 1 expr))
		     (nth 2 expr))))
	  (if (= (length expr) 5)
	      (let ((lower (math-expr-subst (nth 1 expr) (nth 2 expr)
					    (nth 3 expr)))
		    (upper (math-expr-subst (nth 1 expr) (nth 2 expr)
					    (nth 4 expr))))
		(math-add (math-sub (math-mul upper
					      (math-derivative (nth 4 expr)))
				    (math-mul lower
					      (math-derivative (nth 3 expr))))
			  (if (equal (nth 2 expr) math-deriv-var)
			      0
			    (math-normalize
			     (list 'calcFunc-integ
				   (math-derivative (nth 1 expr)) (nth 2 expr)
				   (nth 3 expr) (nth 4 expr)))))))))))

(put 'calcFunc-if 'math-derivative-n
     (function
      (lambda (expr)
	(and (= (length expr) 4)
	     (list 'calcFunc-if (nth 1 expr)
		   (math-derivative (nth 2 expr))
		   (math-derivative (nth 3 expr)))))))

(put 'calcFunc-subscr 'math-derivative-n
     (function
      (lambda (expr)
	(and (= (length expr) 3)
	     (list 'calcFunc-subscr (nth 1 expr)
		   (math-derivative (nth 2 expr)))))))


(defvar math-integ-var '(var X ---))
(defvar math-integ-var-2 '(var Y ---))
(defvar math-integ-vars (list 'f math-integ-var math-integ-var-2))
(defvar math-integ-var-list (list math-integ-var))
(defvar math-integ-var-list-list (list math-integ-var-list))

;; math-integ-depth is a local variable for math-try-integral, but is used
;; by math-integral and math-tracing-integral
;; which are called (directly or indirectly) by math-try-integral.
(defvar math-integ-depth)
;; math-integ-level is a local variable for math-try-integral, but is used
;; by math-integral, math-do-integral, math-tracing-integral,
;; math-sub-integration, math-integrate-by-parts and
;; math-integrate-by-substitution, which are called (directly or
;; indirectly) by math-try-integral.
(defvar math-integ-level)
;; math-integral-limit is a local variable for calcFunc-integ, but is
;; used by math-tracing-integral, math-sub-integration and
;; math-try-integration.
(defvar math-integral-limit)

(defmacro math-tracing-integral (&rest parts)
  (list 'and
	'trace-buffer
	(list 'with-current-buffer
	      'trace-buffer
	      '(goto-char (point-max))
	      (list 'and
		    '(bolp)
		    '(insert (make-string (- math-integral-limit
					     math-integ-level) 32)
			     (format "%2d " math-integ-depth)
			     (make-string math-integ-level 32)))
	      ;;(list 'condition-case 'err
		    (cons 'insert parts)
		;;    '(error (insert (prin1-to-string err))))
	      '(sit-for 0))))

;;; The following wrapper caches results and avoids infinite recursion.
;;; Each cache entry is: ( A B )          Integral of A is B;
;;;			 ( A N )          Integral of A failed at level N;
;;;			 ( A busy )	  Currently working on integral of A;
;;;			 ( A parts )	  Currently working, integ-by-parts;
;;;			 ( A parts2 )	  Currently working, integ-by-parts;
;;;			 ( A cancelled )  Ignore this cache entry;
;;;			 ( A [B] )        Same result as for math-cur-record = B.

;; math-cur-record is a local variable for math-try-integral, but is used
;; by math-integral, math-replace-integral-parts and math-integrate-by-parts
;; which are called (directly or indirectly) by math-try-integral, as well as
;; by calc-dump-integral-cache
(defvar math-cur-record)
;; math-enable-subst and math-any-substs are local variables for
;; calcFunc-integ, but are used by math-integral and math-try-integral.
(defvar math-enable-subst)
(defvar math-any-substs)

;; math-integ-msg is a local variable for math-try-integral, but is
;; used (both locally and non-locally) by math-integral.
(defvar math-integ-msg)

(defvar math-integral-cache nil)
(defvar math-integral-cache-state nil)

(defun math-integral (expr &optional simplify same-as-above)
  (let* ((simp math-cur-record)
	 (math-cur-record (assoc expr math-integral-cache))
	 (math-integ-depth (1+ math-integ-depth))
	 (val 'cancelled))
    (math-tracing-integral "Integrating "
			   (math-format-value expr 1000)
			   "...\n")
    (and math-cur-record
	 (progn
	   (math-tracing-integral "Found "
				  (math-format-value (nth 1 math-cur-record) 1000))
	   (and (consp (nth 1 math-cur-record))
		(math-replace-integral-parts math-cur-record))
	   (math-tracing-integral " => "
				  (math-format-value (nth 1 math-cur-record) 1000)
				  "\n")))
    (or (and math-cur-record
	     (not (eq (nth 1 math-cur-record) 'cancelled))
	     (or (not (integerp (nth 1 math-cur-record)))
		 (>= (nth 1 math-cur-record) math-integ-level)))
	(and (math-integral-contains-parts expr)
	     (progn
	       (setq val nil)
	       t))
	(unwind-protect
	    (progn
	      (let (math-integ-msg)
		(if (eq calc-display-working-message 'lots)
		    (progn
		      (calc-set-command-flag 'clear-message)
		      (setq math-integ-msg (format
					    "Working... Integrating %s"
					    (math-format-flat-expr expr 0)))
		      (message "%s" math-integ-msg)))
		(if math-cur-record
		    (setcar (cdr math-cur-record)
			    (if same-as-above (vector simp) 'busy))
		  (setq math-cur-record
			(list expr (if same-as-above (vector simp) 'busy))
			math-integral-cache (cons math-cur-record
						  math-integral-cache)))
		(if (eq simplify 'yes)
		    (progn
		      (math-tracing-integral "Simplifying...")
		      (setq simp (math-simplify expr))
		      (setq val (if (equal simp expr)
				    (progn
				      (math-tracing-integral " no change\n")
				      (math-do-integral expr))
				  (math-tracing-integral " simplified\n")
				  (math-integral simp 'no t))))
		  (or (setq val (math-do-integral expr))
		      (eq simplify 'no)
		      (let ((simp (math-simplify expr)))
			(or (equal simp expr)
			    (progn
			      (math-tracing-integral "Trying again after "
						     "simplification...\n")
			      (setq val (math-integral simp 'no t))))))))
	      (if (eq calc-display-working-message 'lots)
		  (message "%s" math-integ-msg)))
	  (setcar (cdr math-cur-record) (or val
				       (if (or math-enable-subst
					       (not math-any-substs))
					   math-integ-level
					 'cancelled)))))
    (setq val math-cur-record)
    (while (vectorp (nth 1 val))
      (setq val (aref (nth 1 val) 0)))
    (setq val (if (memq (nth 1 val) '(parts parts2))
		  (progn
		    (setcar (cdr val) 'parts2)
		    (list 'var 'PARTS val))
		(and (consp (nth 1 val))
		     (nth 1 val))))
    (math-tracing-integral "Integral of "
			   (math-format-value expr 1000)
			   "  is  "
			   (math-format-value val 1000)
			   "\n")
    val))

(defun math-integral-contains-parts (expr)
  (if (Math-primp expr)
      (and (eq (car-safe expr) 'var)
	   (eq (nth 1 expr) 'PARTS)
	   (listp (nth 2 expr)))
    (while (and (setq expr (cdr expr))
		(not (math-integral-contains-parts (car expr)))))
    expr))

(defun math-replace-integral-parts (expr)
  (or (Math-primp expr)
      (while (setq expr (cdr expr))
	(and (consp (car expr))
	     (if (eq (car (car expr)) 'var)
		 (and (eq (nth 1 (car expr)) 'PARTS)
		      (consp (nth 2 (car expr)))
		      (if (listp (nth 1 (nth 2 (car expr))))
			  (progn
			    (setcar expr (nth 1 (nth 2 (car expr))))
			    (math-replace-integral-parts (cons 'foo expr)))
			(setcar (cdr math-cur-record) 'cancelled)))
	       (math-replace-integral-parts (car expr)))))))

(defvar math-linear-subst-tried t
  "Non-nil means that a linear substitution has been tried.")

;; The variable math-has-rules is a local variable for math-try-integral,
;; but is used by math-do-integral, which is called (non-directly) by
;; math-try-integral.
(defvar math-has-rules)

;; math-old-integ is a local variable for math-do-integral, but is
;; used by math-sub-integration.
(defvar math-old-integ)

;; The variables math-t1, math-t2 and math-t3 are local to
;; math-do-integral, math-try-solve-for and math-decompose-poly, but
;; are used by functions they call (directly or indirectly);
;; math-do-integral calls math-do-integral-methods;
;; math-try-solve-for calls math-try-solve-prod,
;; math-solve-find-root-term and math-solve-find-root-in-prod;
;; math-decompose-poly calls math-solve-poly-funny-powers and
;; math-solve-crunch-poly.
(defvar math-t1)
(defvar math-t2)
(defvar math-t3)

(defun math-do-integral (expr)
  (let ((math-linear-subst-tried nil)
        math-t1 math-t2)
    (or (cond ((not (math-expr-contains expr math-integ-var))
	       (math-mul expr math-integ-var))
	      ((equal expr math-integ-var)
	       (math-div (math-sqr expr) 2))
	      ((eq (car expr) '+)
	       (and (setq math-t1 (math-integral (nth 1 expr)))
		    (setq math-t2 (math-integral (nth 2 expr)))
		    (math-add math-t1 math-t2)))
	      ((eq (car expr) '-)
	       (and (setq math-t1 (math-integral (nth 1 expr)))
		    (setq math-t2 (math-integral (nth 2 expr)))
		    (math-sub math-t1 math-t2)))
	      ((eq (car expr) 'neg)
	       (and (setq math-t1 (math-integral (nth 1 expr)))
		    (math-neg math-t1)))
	      ((eq (car expr) '*)
	       (cond ((not (math-expr-contains (nth 1 expr) math-integ-var))
		      (and (setq math-t1 (math-integral (nth 2 expr)))
			   (math-mul (nth 1 expr) math-t1)))
		     ((not (math-expr-contains (nth 2 expr) math-integ-var))
		      (and (setq math-t1 (math-integral (nth 1 expr)))
			   (math-mul math-t1 (nth 2 expr))))
		     ((memq (car-safe (nth 1 expr)) '(+ -))
		      (math-integral (list (car (nth 1 expr))
					   (math-mul (nth 1 (nth 1 expr))
						     (nth 2 expr))
					   (math-mul (nth 2 (nth 1 expr))
						     (nth 2 expr)))
				     'yes t))
		     ((memq (car-safe (nth 2 expr)) '(+ -))
		      (math-integral (list (car (nth 2 expr))
					   (math-mul (nth 1 (nth 2 expr))
						     (nth 1 expr))
					   (math-mul (nth 2 (nth 2 expr))
						     (nth 1 expr)))
				     'yes t))))
	      ((eq (car expr) '/)
	       (cond ((and (not (math-expr-contains (nth 1 expr)
						    math-integ-var))
			   (not (math-equal-int (nth 1 expr) 1)))
		      (and (setq math-t1 (math-integral (math-div 1 (nth 2 expr))))
			   (math-mul (nth 1 expr) math-t1)))
		     ((not (math-expr-contains (nth 2 expr) math-integ-var))
		      (and (setq math-t1 (math-integral (nth 1 expr)))
			   (math-div math-t1 (nth 2 expr))))
		     ((and (eq (car-safe (nth 1 expr)) '*)
			   (not (math-expr-contains (nth 1 (nth 1 expr))
						    math-integ-var)))
		      (and (setq math-t1 (math-integral
				     (math-div (nth 2 (nth 1 expr))
					       (nth 2 expr))))
			   (math-mul math-t1 (nth 1 (nth 1 expr)))))
		     ((and (eq (car-safe (nth 1 expr)) '*)
			   (not (math-expr-contains (nth 2 (nth 1 expr))
						    math-integ-var)))
		      (and (setq math-t1 (math-integral
				     (math-div (nth 1 (nth 1 expr))
					       (nth 2 expr))))
			   (math-mul math-t1 (nth 2 (nth 1 expr)))))
		     ((and (eq (car-safe (nth 2 expr)) '*)
			   (not (math-expr-contains (nth 1 (nth 2 expr))
						    math-integ-var)))
		      (and (setq math-t1 (math-integral
				     (math-div (nth 1 expr)
					       (nth 2 (nth 2 expr)))))
			   (math-div math-t1 (nth 1 (nth 2 expr)))))
		     ((and (eq (car-safe (nth 2 expr)) '*)
			   (not (math-expr-contains (nth 2 (nth 2 expr))
						    math-integ-var)))
		      (and (setq math-t1 (math-integral
				     (math-div (nth 1 expr)
					       (nth 1 (nth 2 expr)))))
			   (math-div math-t1 (nth 2 (nth 2 expr)))))
		     ((eq (car-safe (nth 2 expr)) 'calcFunc-exp)
		      (math-integral
		       (math-mul (nth 1 expr)
				 (list 'calcFunc-exp
				       (math-neg (nth 1 (nth 2 expr)))))))))
	      ((eq (car expr) '^)
	       (cond ((not (math-expr-contains (nth 1 expr) math-integ-var))
		      (or (and (setq math-t1 (math-is-polynomial (nth 2 expr)
							    math-integ-var 1))
			       (math-div expr
					 (math-mul (nth 1 math-t1)
						   (math-normalize
						    (list 'calcFunc-ln
							  (nth 1 expr))))))
			  (math-integral
			   (list 'calcFunc-exp
				 (math-mul (nth 2 expr)
					   (math-normalize
					    (list 'calcFunc-ln
						  (nth 1 expr)))))
			   'yes t)))
		     ((not (math-expr-contains (nth 2 expr) math-integ-var))
		      (if (and (integerp (nth 2 expr)) (< (nth 2 expr) 0))
			  (math-integral
			   (list '/ 1 (math-pow (nth 1 expr) (- (nth 2 expr))))
			   nil t)
			(or (and (setq math-t1 (math-is-polynomial (nth 1 expr)
							      math-integ-var
							      1))
				 (setq math-t2 (math-add (nth 2 expr) 1))
				 (math-div (math-pow (nth 1 expr) math-t2)
					   (math-mul math-t2 (nth 1 math-t1))))
			    (and (Math-negp (nth 2 expr))
				 (math-integral
				  (math-div 1
					    (math-pow (nth 1 expr)
						      (math-neg
						       (nth 2 expr))))
				  nil t))
			    nil))))))

	;; Integral of a polynomial.
	(and (setq math-t1 (math-is-polynomial expr math-integ-var 20))
	     (let ((accum 0)
		   (n 1))
	       (while math-t1
		 (if (setq accum (math-add accum
					   (math-div (math-mul (car math-t1)
							       (math-pow
								math-integ-var
								n))
						     n))
			   math-t1 (cdr math-t1))
		     (setq n (1+ n))))
	       accum))

	;; Try looking it up!
	(cond ((= (length expr) 2)
	       (and (symbolp (car expr))
		    (setq math-t1 (get (car expr) 'math-integral))
		    (progn
		      (while (and math-t1
				  (not (setq math-t2 (funcall (car math-t1)
							 (nth 1 expr)))))
			(setq math-t1 (cdr math-t1)))
		      (and math-t2 (math-normalize math-t2)))))
	      ((= (length expr) 3)
	       (and (symbolp (car expr))
		    (setq math-t1 (get (car expr) 'math-integral-2))
		    (progn
		      (while (and math-t1
				  (not (setq math-t2 (funcall (car math-t1)
							 (nth 1 expr)
							 (nth 2 expr)))))
			(setq math-t1 (cdr math-t1)))
		      (and math-t2 (math-normalize math-t2))))))

	;; Integral of a rational function.
	(and (math-ratpoly-p expr math-integ-var)
	     (setq math-t1 (calcFunc-apart expr math-integ-var))
	     (not (equal math-t1 expr))
	     (math-integral math-t1))

	;; Try user-defined integration rules.
	(and math-has-rules
	     (let ((math-old-integ (symbol-function 'calcFunc-integ))
		   (input (list 'calcFunc-integtry expr math-integ-var))
		   res part)
	       (unwind-protect
		   (progn
		     (fset 'calcFunc-integ 'math-sub-integration)
		     (setq res (math-rewrite input
					     '(var IntegRules var-IntegRules)
					     1))
		     (fset 'calcFunc-integ math-old-integ)
		     (and (not (equal res input))
			  (if (setq part (math-expr-calls
					  res '(calcFunc-integsubst)))
			      (and (memq (length part) '(3 4 5))
				   (let ((parts (mapcar
						 (function
						  (lambda (x)
						    (math-expr-subst
						     x (nth 2 part)
						     math-integ-var)))
						 (cdr part))))
				     (math-integrate-by-substitution
				      expr (car parts) t
				      (or (nth 2 parts)
					  (list 'calcFunc-integfailed
						math-integ-var))
				      (nth 3 parts))))
			    (if (not (math-expr-calls res
						      '(calcFunc-integtry
							calcFunc-integfailed)))
				res))))
		 (fset 'calcFunc-integ math-old-integ))))

	;; See if the function is a symbolic derivative.
	(and (string-match "'" (symbol-name (car expr)))
	     (let ((name (symbol-name (car expr)))
		   (p expr) (n 0) (which nil) (bad nil))
	       (while (setq n (1+ n) p (cdr p))
		 (if (equal (car p) math-integ-var)
		     (if which (setq bad t) (setq which n))
		   (if (math-expr-contains (car p) math-integ-var)
		       (setq bad t))))
	       (and which (not bad)
		    (let ((prime (if (= which 1) "'" (format "'%d" which))))
		      (and (string-match (concat prime "\\('['0-9]*\\|$\\)")
					 name)
			   (cons (intern
				  (concat
				   (substring name 0 (match-beginning 0))
				   (substring name (+ (match-beginning 0)
						      (length prime)))))
				 (cdr expr)))))))

	;; Try transformation methods (parts, substitutions).
	(and (> math-integ-level 0)
	     (math-do-integral-methods expr))

	;; Try expanding the function's definition.
	(let ((res (math-expand-formula expr)))
	  (and res
	       (math-integral res))))))

(defun math-sub-integration (expr &rest rest)
  (or (if (or (not rest)
	      (and (< math-integ-level math-integral-limit)
		   (eq (car rest) math-integ-var)))
	  (math-integral expr)
	(let ((res (apply math-old-integ expr rest)))
	  (and (or (= math-integ-level math-integral-limit)
		   (not (math-expr-calls res 'calcFunc-integ)))
	       res)))
      (list 'calcFunc-integfailed expr)))

;; math-so-far is a local variable for math-do-integral-methods, but
;; is used by math-integ-try-linear-substitutions and
;; math-integ-try-substitutions.
(defvar math-so-far)

;; math-integ-expr is a local variable for math-do-integral-methods,
;; but is used by math-integ-try-linear-substitutions and
;; math-integ-try-substitutions.
(defvar math-integ-expr)

(defun math-do-integral-methods (math-integ-expr)
  (let ((math-so-far math-integ-var-list-list)
	rat-in)

    ;; Integration by substitution, for various likely sub-expressions.
    ;; (In first pass, we look only for sub-exprs that are linear in X.)
    (or (math-integ-try-linear-substitutions math-integ-expr)
        (math-integ-try-substitutions math-integ-expr)

	;; If function has sines and cosines, try tan(x/2) substitution.
	(and (let ((p (setq rat-in (math-expr-rational-in math-integ-expr))))
	       (while (and p
			   (memq (car (car p)) '(calcFunc-sin
						 calcFunc-cos
						 calcFunc-tan
                                                 calcFunc-sec
                                                 calcFunc-csc
                                                 calcFunc-cot))
			   (equal (nth 1 (car p)) math-integ-var))
		 (setq p (cdr p)))
	       (null p))
	     (or (and (math-integ-parts-easy math-integ-expr)
		      (math-integ-try-parts math-integ-expr t))
		 (math-integrate-by-good-substitution
		  math-integ-expr (list 'calcFunc-tan (math-div math-integ-var 2)))))

	;; If function has sinh and cosh, try tanh(x/2) substitution.
	(and (let ((p rat-in))
	       (while (and p
			   (memq (car (car p)) '(calcFunc-sinh
						 calcFunc-cosh
						 calcFunc-tanh
                                                 calcFunc-sech
                                                 calcFunc-csch
                                                 calcFunc-coth
						 calcFunc-exp))
			   (equal (nth 1 (car p)) math-integ-var))
		 (setq p (cdr p)))
	       (null p))
	     (or (and (math-integ-parts-easy math-integ-expr)
		      (math-integ-try-parts math-integ-expr t))
		 (math-integrate-by-good-substitution
		  math-integ-expr (list 'calcFunc-tanh (math-div math-integ-var 2)))))

	;; If function has square roots, try sin, tan, or sec substitution.
	(and (let ((p rat-in))
	       (setq math-t1 nil)
	       (while (and p
			   (or (equal (car p) math-integ-var)
			       (and (eq (car (car p)) 'calcFunc-sqrt)
				    (setq math-t1 (math-is-polynomial
					      (nth 1 (setq math-t2 (car p)))
					      math-integ-var 2)))))
		 (setq p (cdr p)))
	       (and (null p) math-t1))
	     (if (cdr (cdr math-t1))
		 (if (math-guess-if-neg (nth 2 math-t1))
		     (let* ((c (math-sqrt (math-neg (nth 2 math-t1))))
			    (d (math-div (nth 1 math-t1) (math-mul -2 c)))
			    (a (math-sqrt (math-add (car math-t1) (math-sqr d)))))
		       (math-integrate-by-good-substitution
			math-integ-expr (list 'calcFunc-arcsin
				   (math-div-thru
				    (math-add (math-mul c math-integ-var) d)
				    a))))
		   (let* ((c (math-sqrt (nth 2 math-t1)))
			  (d (math-div (nth 1 math-t1) (math-mul 2 c)))
			  (aa (math-sub (car math-t1) (math-sqr d))))
		     (if (and nil (not (and (eq d 0) (eq c 1))))
			 (math-integrate-by-good-substitution
			  math-integ-expr (math-add (math-mul c math-integ-var) d))
		       (if (math-guess-if-neg aa)
			   (math-integrate-by-good-substitution
			    math-integ-expr (list 'calcFunc-arccosh
				       (math-div-thru
					(math-add (math-mul c math-integ-var)
						  d)
					(math-sqrt (math-neg aa)))))
			 (math-integrate-by-good-substitution
			  math-integ-expr (list 'calcFunc-arcsinh
				     (math-div-thru
				      (math-add (math-mul c math-integ-var)
						d)
				      (math-sqrt aa))))))))
	       (math-integrate-by-good-substitution math-integ-expr math-t2)) )

	;; Try integration by parts.
	(math-integ-try-parts math-integ-expr)

	;; Give up.
	nil)))

(defun math-integ-parts-easy (expr)
  (cond ((Math-primp expr) t)
	((memq (car expr) '(+ - *))
	 (and (math-integ-parts-easy (nth 1 expr))
	      (math-integ-parts-easy (nth 2 expr))))
	((eq (car expr) '/)
	 (and (math-integ-parts-easy (nth 1 expr))
	      (math-atomic-factorp (nth 2 expr))))
	((eq (car expr) '^)
	 (and (natnump (nth 2 expr))
	      (math-integ-parts-easy (nth 1 expr))))
	((eq (car expr) 'neg)
	 (math-integ-parts-easy (nth 1 expr)))
	(t t)))

;; math-prev-parts-v is local to calcFunc-integ (as well as
;; math-integrate-by-parts), but is used by math-integ-try-parts.
(defvar math-prev-parts-v)

;; math-good-parts is local to calcFunc-integ (as well as
;; math-integ-try-parts), but is used by math-integrate-by-parts.
(defvar math-good-parts)


(defun math-integ-try-parts (expr &optional math-good-parts)
  ;; Integration by parts:
  ;;   integ(f(x) g(x),x) = f(x) h(x) - integ(h(x) f'(x),x)
  ;;     where h(x) = integ(g(x),x).
  (or (let ((exp (calcFunc-expand expr)))
	(and (not (equal exp expr))
	     (math-integral exp)))
      (and (eq (car expr) '*)
	   (let ((first-bad (or (math-polynomial-p (nth 1 expr)
						   math-integ-var)
				(equal (nth 2 expr) math-prev-parts-v))))
	     (or (and first-bad   ; so try this one first
		      (math-integrate-by-parts (nth 1 expr) (nth 2 expr)))
		 (math-integrate-by-parts (nth 2 expr) (nth 1 expr))
		 (and (not first-bad)
		      (math-integrate-by-parts (nth 1 expr) (nth 2 expr))))))
      (and (eq (car expr) '/)
	   (math-expr-contains (nth 1 expr) math-integ-var)
	   (let ((recip (math-div 1 (nth 2 expr))))
	     (or (math-integrate-by-parts (nth 1 expr) recip)
		 (math-integrate-by-parts recip (nth 1 expr)))))
      (and (eq (car expr) '^)
	   (math-integrate-by-parts (math-pow (nth 1 expr)
					      (math-sub (nth 2 expr) 1))
				    (nth 1 expr)))))

(defun math-integrate-by-parts (u vprime)
  (let ((math-integ-level (if (or math-good-parts
				  (math-polynomial-p u math-integ-var))
			      math-integ-level
			    (1- math-integ-level)))
	(math-doing-parts t)
	v temp)
    (and (>= math-integ-level 0)
	 (unwind-protect
	     (progn
	       (setcar (cdr math-cur-record) 'parts)
	       (math-tracing-integral "Integrating by parts, u = "
				      (math-format-value u 1000)
				      ", v' = "
				      (math-format-value vprime 1000)
				      "\n")
	       (and (setq v (math-integral vprime))
		    (setq temp (calcFunc-deriv u math-integ-var nil t))
		    (setq temp (let ((math-prev-parts-v v))
				 (math-integral (math-mul v temp) 'yes)))
		    (setq temp (math-sub (math-mul u v) temp))
		    (if (eq (nth 1 math-cur-record) 'parts)
			(calcFunc-expand temp)
		      (setq v (list 'var 'PARTS math-cur-record)
			    temp (let (calc-next-why)
                                   (math-simplify-extended
                                    (math-solve-for (math-sub v temp) 0 v nil)))
                            temp (if (and (eq (car-safe temp) '/)
                                          (math-zerop (nth 2 temp)))
                                     nil temp)))))
	   (setcar (cdr math-cur-record) 'busy)))))

;;; This tries two different formulations, hoping the algebraic simplifier
;;; will be strong enough to handle at least one.
(defun math-integrate-by-substitution (expr u &optional user uinv uinvprime)
  (and (> math-integ-level 0)
       (let ((math-integ-level (max (- math-integ-level 2) 0)))
	 (math-integrate-by-good-substitution expr u user uinv uinvprime))))

(defun math-integrate-by-good-substitution (expr u &optional user
						 uinv uinvprime)
  (let ((math-living-dangerously t)
	deriv temp)
    (and (setq uinv (if uinv
			(math-expr-subst uinv math-integ-var
					 math-integ-var-2)
		      (let (calc-next-why)
			(math-solve-for u
					math-integ-var-2
					math-integ-var nil))))
	 (progn
	   (math-tracing-integral "Integrating by substitution, u = "
				  (math-format-value u 1000)
				  "\n")
	   (or (and (setq deriv (calcFunc-deriv u
						math-integ-var nil
						(not user)))
		    (setq temp (math-integral (math-expr-subst
					       (math-expr-subst
						(math-expr-subst
						 (math-div expr deriv)
						 u
						 math-integ-var-2)
						math-integ-var
						uinv)
					       math-integ-var-2
					       math-integ-var)
					      'yes)))
	       (and (setq deriv (or uinvprime
				    (calcFunc-deriv uinv
						    math-integ-var-2
						    math-integ-var
						    (not user))))
		    (setq temp (math-integral (math-mul
					       (math-expr-subst
						(math-expr-subst
						 (math-expr-subst
						  expr
						  u
						  math-integ-var-2)
						 math-integ-var
						 uinv)
						math-integ-var-2
						math-integ-var)
					       deriv)
					      'yes)))))
	 (math-simplify-extended
	  (math-expr-subst temp math-integ-var u)))))

;;; Look for substitutions of the form u = a x + b.
(defun math-integ-try-linear-substitutions (sub-expr)
  (setq math-linear-subst-tried t)
  (and (not (Math-primp sub-expr))
       (or (and (not (memq (car sub-expr) '(+ - * / neg)))
		(not (and (eq (car sub-expr) '^)
			  (integerp (nth 2 sub-expr))))
		(math-expr-contains sub-expr math-integ-var)
		(let ((res nil))
		  (while (and (setq sub-expr (cdr sub-expr))
			      (or (not (math-linear-in (car sub-expr)
						       math-integ-var))
				  (assoc (car sub-expr) math-so-far)
				  (progn
				    (setq math-so-far (cons (list (car sub-expr))
						       math-so-far))
				    (not (setq res
					       (math-integrate-by-substitution
						math-integ-expr (car sub-expr))))))))
		  res))
	   (let ((res nil))
	     (while (and (setq sub-expr (cdr sub-expr))
			 (not (setq res (math-integ-try-linear-substitutions
					 (car sub-expr))))))
	     res))))

;;; Recursively try different substitutions based on various sub-expressions.
(defun math-integ-try-substitutions (sub-expr &optional allow-rat)
  (and (not (Math-primp sub-expr))
       (not (assoc sub-expr math-so-far))
       (math-expr-contains sub-expr math-integ-var)
       (or (and (if (and (not (memq (car sub-expr) '(+ - * / neg)))
			 (not (and (eq (car sub-expr) '^)
				   (integerp (nth 2 sub-expr)))))
		    (setq allow-rat t)
		  (prog1 allow-rat (setq allow-rat nil)))
		(not (eq sub-expr math-integ-expr))
		(or (math-integrate-by-substitution math-integ-expr sub-expr)
		    (and (eq (car sub-expr) '^)
			 (integerp (nth 2 sub-expr))
			 (< (nth 2 sub-expr) 0)
			 (math-integ-try-substitutions
			  (math-pow (nth 1 sub-expr) (- (nth 2 sub-expr)))
			  t))))
	   (let ((res nil))
	     (setq math-so-far (cons (list sub-expr) math-so-far))
	     (while (and (setq sub-expr (cdr sub-expr))
			 (not (setq res (math-integ-try-substitutions
					 (car sub-expr) allow-rat)))))
	     res))))

;; The variable math-expr-parts is local to math-expr-rational-in,
;; but is used by math-expr-rational-in-rec
(defvar math-expr-parts)

(defun math-expr-rational-in (expr)
  (let ((math-expr-parts nil))
    (math-expr-rational-in-rec expr)
    (mapcar 'car math-expr-parts)))

(defun math-expr-rational-in-rec (expr)
  (cond ((Math-primp expr)
	 (and (equal expr math-integ-var)
	      (not (assoc expr math-expr-parts))
	      (setq math-expr-parts (cons (list expr) math-expr-parts))))
	((or (memq (car expr) '(+ - * / neg))
	     (and (eq (car expr) '^) (integerp (nth 2 expr))))
	 (math-expr-rational-in-rec (nth 1 expr))
	 (and (nth 2 expr) (math-expr-rational-in-rec (nth 2 expr))))
	((and (eq (car expr) '^)
	      (eq (math-quarter-integer (nth 2 expr)) 2))
	 (math-expr-rational-in-rec (list 'calcFunc-sqrt (nth 1 expr))))
	(t
	 (and (not (assoc expr math-expr-parts))
	      (math-expr-contains expr math-integ-var)
	      (setq math-expr-parts (cons (list expr) math-expr-parts))))))

(defun math-expr-calls (expr funcs &optional arg-contains)
  (if (consp expr)
      (if (or (memq (car expr) funcs)
	      (and (eq (car expr) '^) (eq (car funcs) 'calcFunc-sqrt)
		   (eq (math-quarter-integer (nth 2 expr)) 2)))
	  (and (or (not arg-contains)
		   (math-expr-contains expr arg-contains))
	       expr)
	(and (not (Math-primp expr))
	     (let ((res nil))
	       (while (and (setq expr (cdr expr))
			   (not (setq res (math-expr-calls
					   (car expr) funcs arg-contains)))))
	       res)))))

(defun math-fix-const-terms (expr except-vars)
  (cond ((not (math-expr-depends expr except-vars)) 0)
	((Math-primp expr) expr)
	((eq (car expr) '+)
	 (math-add (math-fix-const-terms (nth 1 expr) except-vars)
		   (math-fix-const-terms (nth 2 expr) except-vars)))
	((eq (car expr) '-)
	 (math-sub (math-fix-const-terms (nth 1 expr) except-vars)
		   (math-fix-const-terms (nth 2 expr) except-vars)))
	(t expr)))

;; Command for debugging the Calculator's symbolic integrator.
(defun calc-dump-integral-cache (&optional arg)
  (interactive "P")
  (let ((buf (current-buffer)))
    (unwind-protect
	(let ((p math-integral-cache)
	      math-cur-record)
	  (display-buffer (get-buffer-create "*Integral Cache*"))
	  (set-buffer (get-buffer "*Integral Cache*"))
	  (erase-buffer)
	  (while p
	    (setq math-cur-record (car p))
	    (or arg (math-replace-integral-parts math-cur-record))
	    (insert (math-format-flat-expr (car math-cur-record) 0)
		    " --> "
		    (if (symbolp (nth 1 math-cur-record))
			(concat "(" (symbol-name (nth 1 math-cur-record)) ")")
		      (math-format-flat-expr (nth 1 math-cur-record) 0))
		    "\n")
	    (setq p (cdr p)))
	  (goto-char (point-min)))
      (set-buffer buf))))

;; The variable math-max-integral-limit is local to calcFunc-integ,
;; but is used by math-try-integral.
(defvar math-max-integral-limit)

(defun math-try-integral (expr)
  (let ((math-integ-level math-integral-limit)
	(math-integ-depth 0)
	(math-integ-msg "Working...done")
	(math-cur-record nil)   ; a technicality
	(math-integrating t)
	(calc-prefer-frac t)
	(calc-symbolic-mode t)
	(math-has-rules (calc-has-rules 'var-IntegRules)))
    (or (math-integral expr 'yes)
	(and math-any-substs
	     (setq math-enable-subst t)
	     (math-integral expr 'yes))
	(and (> math-max-integral-limit math-integral-limit)
	     (setq math-integral-limit math-max-integral-limit
		   math-integ-level math-integral-limit)
	     (math-integral expr 'yes)))))

(defvar var-IntegLimit nil)

(defun calcFunc-integ (expr var &optional low high)
  (cond
   ;; Do these even if the parts turn out not to be integrable.
   ((eq (car-safe expr) '+)
    (math-add (calcFunc-integ (nth 1 expr) var low high)
	      (calcFunc-integ (nth 2 expr) var low high)))
   ((eq (car-safe expr) '-)
    (math-sub (calcFunc-integ (nth 1 expr) var low high)
	      (calcFunc-integ (nth 2 expr) var low high)))
   ((eq (car-safe expr) 'neg)
    (math-neg (calcFunc-integ (nth 1 expr) var low high)))
   ((and (eq (car-safe expr) '*)
	 (not (math-expr-contains (nth 1 expr) var)))
    (math-mul (nth 1 expr) (calcFunc-integ (nth 2 expr) var low high)))
   ((and (eq (car-safe expr) '*)
	 (not (math-expr-contains (nth 2 expr) var)))
    (math-mul (calcFunc-integ (nth 1 expr) var low high) (nth 2 expr)))
   ((and (eq (car-safe expr) '/)
	 (not (math-expr-contains (nth 1 expr) var))
	 (not (math-equal-int (nth 1 expr) 1)))
    (math-mul (nth 1 expr)
	      (calcFunc-integ (math-div 1 (nth 2 expr)) var low high)))
   ((and (eq (car-safe expr) '/)
	 (not (math-expr-contains (nth 2 expr) var)))
    (math-div (calcFunc-integ (nth 1 expr) var low high) (nth 2 expr)))
   ((and (eq (car-safe expr) '/)
	 (eq (car-safe (nth 1 expr)) '*)
	 (not (math-expr-contains (nth 1 (nth 1 expr)) var)))
    (math-mul (nth 1 (nth 1 expr))
	      (calcFunc-integ (math-div (nth 2 (nth 1 expr)) (nth 2 expr))
			      var low high)))
   ((and (eq (car-safe expr) '/)
	 (eq (car-safe (nth 1 expr)) '*)
	 (not (math-expr-contains (nth 2 (nth 1 expr)) var)))
    (math-mul (nth 2 (nth 1 expr))
	      (calcFunc-integ (math-div (nth 1 (nth 1 expr)) (nth 2 expr))
			      var low high)))
   ((and (eq (car-safe expr) '/)
	 (eq (car-safe (nth 2 expr)) '*)
	 (not (math-expr-contains (nth 1 (nth 2 expr)) var)))
    (math-div (calcFunc-integ (math-div (nth 1 expr) (nth 2 (nth 2 expr)))
			      var low high)
	      (nth 1 (nth 2 expr))))
   ((and (eq (car-safe expr) '/)
	 (eq (car-safe (nth 2 expr)) '*)
	 (not (math-expr-contains (nth 2 (nth 2 expr)) var)))
    (math-div (calcFunc-integ (math-div (nth 1 expr) (nth 1 (nth 2 expr)))
			      var low high)
	      (nth 2 (nth 2 expr))))
   ((eq (car-safe expr) 'vec)
    (cons 'vec (mapcar (function (lambda (x) (calcFunc-integ x var low high)))
		       (cdr expr))))
   (t
    (let ((state (list calc-angle-mode
		       ;;calc-symbolic-mode
		       ;;calc-prefer-frac
		       calc-internal-prec
		       (calc-var-value 'var-IntegRules)
		       (calc-var-value 'var-IntegSimpRules))))
      (or (equal state math-integral-cache-state)
	  (setq math-integral-cache-state state
		math-integral-cache nil)))
    (let* ((math-max-integral-limit (or (and (natnump var-IntegLimit)
					     var-IntegLimit)
					3))
	   (math-integral-limit 1)
	   (sexpr (math-expr-subst expr var math-integ-var))
	   (trace-buffer (get-buffer "*Trace*"))
	   (calc-language (if (eq calc-language 'big) nil calc-language))
	   (math-any-substs t)
	   (math-enable-subst nil)
	   (math-prev-parts-v nil)
	   (math-doing-parts nil)
	   (math-good-parts nil)
	   (res
	    (if trace-buffer
		(let ((calcbuf (current-buffer))
		      (calcwin (selected-window)))
		  (unwind-protect
		      (progn
			(if (get-buffer-window trace-buffer)
			    (select-window (get-buffer-window trace-buffer)))
			(set-buffer trace-buffer)
			(goto-char (point-max))
			(or (assq 'scroll-stop (buffer-local-variables))
			    (progn
			      (make-local-variable 'scroll-step)
			      (setq scroll-step 3)))
			(insert "\n\n\n")
			(set-buffer calcbuf)
			(math-try-integral sexpr))
		    (select-window calcwin)
		      (set-buffer calcbuf)))
	      (math-try-integral sexpr))))
      (if res
	  (progn
	    (if (calc-has-rules 'var-IntegAfterRules)
		(setq res (math-rewrite res '(var IntegAfterRules
						  var-IntegAfterRules))))
	    (math-simplify
	     (if (and low high)
		 (math-sub (math-expr-subst res math-integ-var high)
			   (math-expr-subst res math-integ-var low))
	       (setq res (math-fix-const-terms res math-integ-vars))
	       (if low
		   (math-expr-subst res math-integ-var low)
		 (math-expr-subst res math-integ-var var)))))
	(append (list 'calcFunc-integ expr var)
		(and low (list low))
		(and high (list high))))))))


(math-defintegral calcFunc-inv
  (math-integral (math-div 1 u)))

(math-defintegral calcFunc-conj
  (let ((int (math-integral u)))
    (and int
	 (list 'calcFunc-conj int))))

(math-defintegral calcFunc-deg
  (let ((int (math-integral u)))
    (and int
	 (list 'calcFunc-deg int))))

(math-defintegral calcFunc-rad
  (let ((int (math-integral u)))
    (and int
	 (list 'calcFunc-rad int))))

(math-defintegral calcFunc-re
  (let ((int (math-integral u)))
    (and int
	 (list 'calcFunc-re int))))

(math-defintegral calcFunc-im
  (let ((int (math-integral u)))
    (and int
	 (list 'calcFunc-im int))))

(math-defintegral calcFunc-sqrt
  (and (equal u math-integ-var)
       (math-mul '(frac 2 3)
		 (list 'calcFunc-sqrt (math-pow u 3)))))

(math-defintegral calcFunc-exp
  (or (and (equal u math-integ-var)
	   (list 'calcFunc-exp u))
      (let ((p (math-is-polynomial u math-integ-var 2)))
	(and (nth 2 p)
	     (let ((sqa (math-sqrt (math-neg (nth 2 p)))))
	       (math-div
		(math-mul
		 (math-mul (math-div (list 'calcFunc-sqrt '(var pi var-pi))
				     sqa)
			   (math-normalize
			    (list 'calcFunc-exp
				  (math-div (math-sub (math-mul (car p)
								(nth 2 p))
						      (math-div
						       (math-sqr (nth 1 p))
						       4))
					    (nth 2 p)))))
		 (list 'calcFunc-erf
		       (math-sub (math-mul sqa math-integ-var)
				 (math-div (nth 1 p) (math-mul 2 sqa)))))
		2))))))

(math-defintegral calcFunc-ln
  (or (and (equal u math-integ-var)
	   (math-sub (math-mul u (list 'calcFunc-ln u)) u))
      (and (eq (car u) '*)
	   (math-integral (math-add (list 'calcFunc-ln (nth 1 u))
				    (list 'calcFunc-ln (nth 2 u)))))
      (and (eq (car u) '/)
	   (math-integral (math-sub (list 'calcFunc-ln (nth 1 u))
				    (list 'calcFunc-ln (nth 2 u)))))
      (and (eq (car u) '^)
	   (math-integral (math-mul (nth 2 u)
				    (list 'calcFunc-ln (nth 1 u)))))))

(math-defintegral calcFunc-log10
  (and (equal u math-integ-var)
       (math-sub (math-mul u (list 'calcFunc-ln u))
		 (math-div u (list 'calcFunc-ln 10)))))

(math-defintegral-2 calcFunc-log
  (math-integral (math-div (list 'calcFunc-ln u)
			   (list 'calcFunc-ln v))))

(math-defintegral calcFunc-sin
  (or (and (equal u math-integ-var)
	   (math-neg (math-from-radians-2 (list 'calcFunc-cos u))))
      (and (nth 2 (math-is-polynomial u math-integ-var 2))
	   (math-integral (math-to-exponentials (list 'calcFunc-sin u))))))

(math-defintegral calcFunc-cos
  (or (and (equal u math-integ-var)
	   (math-from-radians-2 (list 'calcFunc-sin u)))
      (and (nth 2 (math-is-polynomial u math-integ-var 2))
	   (math-integral (math-to-exponentials (list 'calcFunc-cos u))))))

(math-defintegral calcFunc-tan
  (and (equal u math-integ-var)
       (math-from-radians-2
        (list 'calcFunc-ln (list 'calcFunc-sec u)))))

(math-defintegral calcFunc-sec
  (and (equal u math-integ-var)
       (math-from-radians-2
        (list 'calcFunc-ln
              (math-add
               (list 'calcFunc-sec u)
               (list 'calcFunc-tan u))))))

(math-defintegral calcFunc-csc
  (and (equal u math-integ-var)
       (math-from-radians-2
        (list 'calcFunc-ln
              (math-sub
               (list 'calcFunc-csc u)
               (list 'calcFunc-cot u))))))

(math-defintegral calcFunc-cot
  (and (equal u math-integ-var)
       (math-from-radians-2
        (list 'calcFunc-ln (list 'calcFunc-sin u)))))

(math-defintegral calcFunc-arcsin
  (and (equal u math-integ-var)
       (math-add (math-mul u (list 'calcFunc-arcsin u))
		 (math-from-radians-2
		  (list 'calcFunc-sqrt (math-sub 1 (math-sqr u)))))))

(math-defintegral calcFunc-arccos
  (and (equal u math-integ-var)
       (math-sub (math-mul u (list 'calcFunc-arccos u))
		 (math-from-radians-2
		  (list 'calcFunc-sqrt (math-sub 1 (math-sqr u)))))))

(math-defintegral calcFunc-arctan
  (and (equal u math-integ-var)
       (math-sub (math-mul u (list 'calcFunc-arctan u))
		 (math-from-radians-2
		  (math-div (list 'calcFunc-ln (math-add 1 (math-sqr u)))
			    2)))))

(math-defintegral calcFunc-sinh
  (and (equal u math-integ-var)
       (list 'calcFunc-cosh u)))

(math-defintegral calcFunc-cosh
  (and (equal u math-integ-var)
       (list 'calcFunc-sinh u)))

(math-defintegral calcFunc-tanh
  (and (equal u math-integ-var)
       (list 'calcFunc-ln (list 'calcFunc-cosh u))))

(math-defintegral calcFunc-sech
  (and (equal u math-integ-var)
       (list 'calcFunc-arctan (list 'calcFunc-sinh u))))

(math-defintegral calcFunc-csch
  (and (equal u math-integ-var)
       (list 'calcFunc-ln (list 'calcFunc-tanh (math-div u 2)))))

(math-defintegral calcFunc-coth
  (and (equal u math-integ-var)
       (list 'calcFunc-ln (list 'calcFunc-sinh u))))

(math-defintegral calcFunc-arcsinh
  (and (equal u math-integ-var)
       (math-sub (math-mul u (list 'calcFunc-arcsinh u))
		 (list 'calcFunc-sqrt (math-add (math-sqr u) 1)))))

(math-defintegral calcFunc-arccosh
  (and (equal u math-integ-var)
       (math-sub (math-mul u (list 'calcFunc-arccosh u))
		 (list 'calcFunc-sqrt (math-sub 1 (math-sqr u))))))

(math-defintegral calcFunc-arctanh
  (and (equal u math-integ-var)
       (math-sub (math-mul u (list 'calcFunc-arctan u))
		 (math-div (list 'calcFunc-ln
				 (math-add 1 (math-sqr u)))
			   2))))

;;; (Ax + B) / (ax^2 + bx + c)^n forms.
(math-defintegral-2 /
  (math-integral-rational-funcs u v))

(defun math-integral-rational-funcs (u v)
  (let ((pu (math-is-polynomial u math-integ-var 1))
	(vpow 1) pv)
    (and pu
	 (catch 'int-rat
	   (if (and (eq (car-safe v) '^) (natnump (nth 2 v)))
	       (setq vpow (nth 2 v)
		     v (nth 1 v)))
	   (and (setq pv (math-is-polynomial v math-integ-var 2))
		(let ((int (math-mul-thru
			    (car pu)
			    (math-integral-q02 (car pv) (nth 1 pv)
					       (nth 2 pv) v vpow))))
		  (if (cdr pu)
		      (setq int (math-add int
					  (math-mul-thru
					   (nth 1 pu)
					   (math-integral-q12
					    (car pv) (nth 1 pv)
					    (nth 2 pv) v vpow)))))
		  int))))))

(defun math-integral-q12 (a b c v vpow)
  (let (q)
    (cond ((not c)
	   (cond ((= vpow 1)
		  (math-sub (math-div math-integ-var b)
			    (math-mul (math-div a (math-sqr b))
				      (list 'calcFunc-ln v))))
		 ((= vpow 2)
		  (math-div (math-add (list 'calcFunc-ln v)
				      (math-div a v))
			    (math-sqr b)))
		 (t
		  (let ((nm1 (math-sub vpow 1))
			(nm2 (math-sub vpow 2)))
		    (math-div (math-sub
			       (math-div a (math-mul nm1 (math-pow v nm1)))
			       (math-div 1 (math-mul nm2 (math-pow v nm2))))
			      (math-sqr b))))))
	  ((math-zerop
	    (setq q (math-sub (math-mul 4 (math-mul a c)) (math-sqr b))))
	   (let ((part (math-div b (math-mul 2 c))))
	     (math-mul-thru (math-pow c vpow)
			    (math-integral-q12 part 1 nil
					       (math-add math-integ-var part)
					       (* vpow 2)))))
	  ((= vpow 1)
	   (and (math-ratp q) (math-negp q)
		(let ((calc-symbolic-mode t))
		  (math-ratp (math-sqrt (math-neg q))))
		(throw 'int-rat nil))  ; should have used calcFunc-apart first
	   (math-sub (math-div (list 'calcFunc-ln v) (math-mul 2 c))
		     (math-mul-thru (math-div b (math-mul 2 c))
				    (math-integral-q02 a b c v 1))))
	  (t
	   (let ((n (1- vpow)))
	     (math-sub (math-neg (math-div
				  (math-add (math-mul b math-integ-var)
					    (math-mul 2 a))
				  (math-mul n (math-mul q (math-pow v n)))))
		       (math-mul-thru (math-div (math-mul b (1- (* 2 n)))
						(math-mul n q))
				      (math-integral-q02 a b c v n))))))))

(defun math-integral-q02 (a b c v vpow)
  (let (q rq part)
    (cond ((not c)
	   (cond ((= vpow 1)
		  (math-div (list 'calcFunc-ln v) b))
		 (t
		  (math-div (math-pow v (- 1 vpow))
			    (math-mul (- 1 vpow) b)))))
	  ((math-zerop
	    (setq q (math-sub (math-mul 4 (math-mul a c)) (math-sqr b))))
	   (let ((part (math-div b (math-mul 2 c))))
	     (math-mul-thru (math-pow c vpow)
			    (math-integral-q02 part 1 nil
					       (math-add math-integ-var part)
					       (* vpow 2)))))
	  ((progn
	     (setq part (math-add (math-mul 2 (math-mul c math-integ-var)) b))
	     (> vpow 1))
	   (let ((n (1- vpow)))
	     (math-add (math-div part (math-mul n (math-mul q (math-pow v n))))
		       (math-mul-thru (math-div (math-mul (- (* 4 n) 2) c)
						(math-mul n q))
				      (math-integral-q02 a b c v n)))))
	  ((math-guess-if-neg q)
	   (setq rq (list 'calcFunc-sqrt (math-neg q)))
	   ;;(math-div-thru (list 'calcFunc-ln
	   ;;			(math-div (math-sub part rq)
	   ;;				  (math-add part rq)))
	   ;;		  rq)
	   (math-div (math-mul -2 (list 'calcFunc-arctanh
					(math-div part rq)))
		     rq))
	  (t
	   (setq rq (list 'calcFunc-sqrt q))
	   (math-div (math-mul 2 (math-to-radians-2
				  (list 'calcFunc-arctan
					(math-div part rq))))
		     rq)))))


(math-defintegral calcFunc-erf
  (and (equal u math-integ-var)
       (math-add (math-mul u (list 'calcFunc-erf u))
		 (math-div 1 (math-mul (list 'calcFunc-exp (math-sqr u))
				       (list 'calcFunc-sqrt
					     '(var pi var-pi)))))))

(math-defintegral calcFunc-erfc
  (and (equal u math-integ-var)
       (math-sub (math-mul u (list 'calcFunc-erfc u))
		 (math-div 1 (math-mul (list 'calcFunc-exp (math-sqr u))
				       (list 'calcFunc-sqrt
					     '(var pi var-pi)))))))




(defvar math-tabulate-initial nil)
(defvar math-tabulate-function nil)

;; These variables are local to calcFunc-table, but are used by
;; math-scan-for-limits.
(defvar calc-low)
(defvar calc-high)
(defvar math-var)

(defun calcFunc-table (expr math-var &optional calc-low calc-high step)
  (or calc-low
      (setq calc-low '(neg (var inf var-inf)) calc-high '(var inf var-inf)))
  (or calc-high (setq calc-high calc-low calc-low 1))
  (and (or (math-infinitep calc-low) (math-infinitep calc-high))
       (not step)
       (math-scan-for-limits expr))
  (and step (math-zerop step) (math-reject-arg step 'nonzerop))
  (let ((known (+ (if (Math-objectp calc-low) 1 0)
		  (if (Math-objectp calc-high) 1 0)
		  (if (or (null step) (Math-objectp step)) 1 0)))
	(count '(var inf var-inf))
	vec)
    (or (= known 2)   ; handy optimization
	(equal calc-high '(var inf var-inf))
	(progn
	  (setq count (math-div (math-sub calc-high calc-low) (or step 1)))
	  (or (Math-objectp count)
	      (setq count (math-simplify count)))
	  (if (Math-messy-integerp count)
	      (setq count (math-trunc count)))))
    (if (Math-negp count)
	(setq count -1))
    (if (integerp count)
	(let ((var-DUMMY nil)
	      (vec math-tabulate-initial)
	      (math-working-step-2 (1+ count))
	      (math-working-step 0))
	  (setq expr (math-evaluate-expr
		      (math-expr-subst expr math-var '(var DUMMY var-DUMMY))))
	  (while (>= count 0)
	    (setq math-working-step (1+ math-working-step)
		  var-DUMMY calc-low
		  vec (cond ((eq math-tabulate-function 'calcFunc-sum)
			     (math-add vec (math-evaluate-expr expr)))
			    ((eq math-tabulate-function 'calcFunc-prod)
			     (math-mul vec (math-evaluate-expr expr)))
			    (t
			     (cons (math-evaluate-expr expr) vec)))
		  calc-low (math-add calc-low (or step 1))
		  count (1- count)))
	  (if math-tabulate-function
	      vec
	    (cons 'vec (nreverse vec))))
      (if (Math-integerp count)
	  (calc-record-why 'fixnump calc-high)
	(if (Math-num-integerp calc-low)
	    (if (Math-num-integerp calc-high)
		(calc-record-why 'integerp step)
	      (calc-record-why 'integerp calc-high))
	  (calc-record-why 'integerp calc-low)))
      (append (list (or math-tabulate-function 'calcFunc-table)
		    expr math-var)
	      (and (not (and (equal calc-low '(neg (var inf var-inf)))
			     (equal calc-high '(var inf var-inf))))
		   (list calc-low calc-high))
	      (and step (list step))))))

(defun math-scan-for-limits (x)
  (cond ((Math-primp x))
	((and (eq (car x) 'calcFunc-subscr)
	      (Math-vectorp (nth 1 x))
	      (math-expr-contains (nth 2 x) math-var))
	 (let* ((calc-next-why nil)
		(low-val (math-solve-for (nth 2 x) 1 math-var nil))
		(high-val (math-solve-for (nth 2 x) (1- (length (nth 1 x)))
					  math-var nil))
		temp)
	   (and low-val (math-realp low-val)
		high-val (math-realp high-val))
	   (and (Math-lessp high-val low-val)
		(setq temp low-val low-val high-val high-val temp))
	   (setq calc-low (math-max calc-low (math-ceiling low-val))
		 calc-high (math-min calc-high (math-floor high-val)))))
	(t
	 (while (setq x (cdr x))
	   (math-scan-for-limits (car x))))))


(defvar math-disable-sums nil)
(defun calcFunc-sum (expr var &optional low high step)
  (if math-disable-sums (math-reject-arg))
  (let* ((res (let* ((calc-internal-prec (+ calc-internal-prec 2)))
		(math-sum-rec expr var low high step)))
	 (math-disable-sums t))
    (math-normalize res)))

(defun math-sum-rec (expr var &optional low high step)
  (or low (setq low '(neg (var inf var-inf)) high '(var inf var-inf)))
  (and low (not high) (setq high low low 1))
  (let (t1 t2 val)
    (setq val
	  (cond
	   ((not (math-expr-contains expr var))
	    (math-mul expr (math-add (math-div (math-sub high low) (or step 1))
				     1)))
	   ((and step (not (math-equal-int step 1)))
	    (if (math-negp step)
		(math-sum-rec expr var high low (math-neg step))
	      (let ((lo (math-simplify (math-div low step))))
		(if (math-known-num-integerp lo)
		    (math-sum-rec (math-normalize
				   (math-expr-subst expr var
						    (math-mul step var)))
				  var lo (math-simplify (math-div high step)))
		  (math-sum-rec (math-normalize
				 (math-expr-subst expr var
						  (math-add (math-mul step var)
							    low)))
				var 0
				(math-simplify (math-div (math-sub high low)
							 step)))))))
	   ((memq (setq t1 (math-compare low high)) '(0 1))
	    (if (eq t1 0)
		(math-expr-subst expr var low)
	      0))
	   ((setq t1 (math-is-polynomial expr var 20))
	    (let ((poly nil)
		  (n 0))
	      (while t1
		(setq poly (math-poly-mix poly 1
					  (math-sum-integer-power n) (car t1))
		      n (1+ n)
		      t1 (cdr t1)))
	      (setq n (math-build-polynomial-expr poly high))
	      (if (= low 1)
		  n
		(math-sub n (math-build-polynomial-expr poly
							(math-sub low 1))))))
	   ((and (memq (car expr) '(+ -))
		 (setq t1 (math-sum-rec (nth 1 expr) var low high)
		       t2 (math-sum-rec (nth 2 expr) var low high))
		 (not (and (math-expr-calls t1 '(calcFunc-sum))
			   (math-expr-calls t2 '(calcFunc-sum)))))
	    (list (car expr) t1 t2))
	   ((and (eq (car expr) '*)
		 (setq t1 (math-sum-const-factors expr var)))
	    (math-mul (car t1) (math-sum-rec (cdr t1) var low high)))
	   ((and (eq (car expr) '*) (memq (car-safe (nth 1 expr)) '(+ -)))
	    (math-sum-rec (math-add-or-sub (math-mul (nth 1 (nth 1 expr))
						     (nth 2 expr))
					   (math-mul (nth 2 (nth 1 expr))
						     (nth 2 expr))
					   nil (eq (car (nth 1 expr)) '-))
			  var low high))
	   ((and (eq (car expr) '*) (memq (car-safe (nth 2 expr)) '(+ -)))
	    (math-sum-rec (math-add-or-sub (math-mul (nth 1 expr)
						     (nth 1 (nth 2 expr)))
					   (math-mul (nth 1 expr)
						     (nth 2 (nth 2 expr)))
					   nil (eq (car (nth 2 expr)) '-))
			  var low high))
	   ((and (eq (car expr) '/)
		 (not (math-primp (nth 1 expr)))
		 (setq t1 (math-sum-const-factors (nth 1 expr) var)))
	    (math-mul (car t1)
		      (math-sum-rec (math-div (cdr t1) (nth 2 expr))
				    var low high)))
	   ((and (eq (car expr) '/)
		 (setq t1 (math-sum-const-factors (nth 2 expr) var)))
	    (math-div (math-sum-rec (math-div (nth 1 expr) (cdr t1))
				    var low high)
		      (car t1)))
	   ((eq (car expr) 'neg)
	    (math-neg (math-sum-rec (nth 1 expr) var low high)))
	   ((and (eq (car expr) '^)
		 (not (math-expr-contains (nth 1 expr) var))
		 (setq t1 (math-is-polynomial (nth 2 expr) var 1)))
	    (let ((x (math-pow (nth 1 expr) (nth 1 t1))))
	      (math-div (math-mul (math-sub (math-pow x (math-add 1 high))
					    (math-pow x low))
				  (math-pow (nth 1 expr) (car t1)))
			(math-sub x 1))))
	   ((and (setq t1 (math-to-exponentials expr))
		 (setq t1 (math-sum-rec t1 var low high))
		 (not (math-expr-calls t1 '(calcFunc-sum))))
	    (math-to-exps t1))
	   ((memq (car expr) '(calcFunc-ln calcFunc-log10))
	    (list (car expr) (calcFunc-prod (nth 1 expr) var low high)))
	   ((and (eq (car expr) 'calcFunc-log)
		 (= (length expr) 3)
		 (not (math-expr-contains (nth 2 expr) var)))
	    (list 'calcFunc-log
		  (calcFunc-prod (nth 1 expr) var low high)
		  (nth 2 expr)))))
    (if (equal val '(var nan var-nan)) (setq val nil))
    (or val
	(let* ((math-tabulate-initial 0)
	       (math-tabulate-function 'calcFunc-sum))
	  (calcFunc-table expr var low high)))))

(defun calcFunc-asum (expr var low &optional high step no-mul-flag)
  (or high (setq high low low 1))
  (if (and step (not (math-equal-int step 1)))
      (if (math-negp step)
	  (math-mul (math-pow -1 low)
		    (calcFunc-asum expr var high low (math-neg step) t))
	(let ((lo (math-simplify (math-div low step))))
	  (if (math-num-integerp lo)
	      (calcFunc-asum (math-normalize
			      (math-expr-subst expr var
					       (math-mul step var)))
			     var lo (math-simplify (math-div high step)))
	    (calcFunc-asum (math-normalize
			    (math-expr-subst expr var
					     (math-add (math-mul step var)
						       low)))
			   var 0
			   (math-simplify (math-div (math-sub high low)
						    step))))))
    (math-mul (if no-mul-flag 1 (math-pow -1 low))
	      (calcFunc-sum (math-mul (math-pow -1 var) expr) var low high))))

(defun math-sum-const-factors (expr var)
  (let ((const nil)
	(not-const nil)
	(p expr))
    (while (eq (car-safe p) '*)
      (if (math-expr-contains (nth 1 p) var)
	  (setq not-const (cons (nth 1 p) not-const))
	(setq const (cons (nth 1 p) const)))
      (setq p (nth 2 p)))
    (if (math-expr-contains p var)
	(setq not-const (cons p not-const))
      (setq const (cons p const)))
    (and const
	 (cons (let ((temp (car const)))
		 (while (setq const (cdr const))
		   (setq temp (list '* (car const) temp)))
		 temp)
	       (let ((temp (or (car not-const) 1)))
		 (while (setq not-const (cdr not-const))
		   (setq temp (list '* (car not-const) temp)))
		 temp)))))

(defvar math-sum-int-pow-cache (list '(0 1)))
;; Following is from CRC Math Tables, 27th ed, pp. 52-53.
(defun math-sum-integer-power (pow)
  (let ((calc-prefer-frac t)
	(n (length math-sum-int-pow-cache)))
    (while (<= n pow)
      (let* ((new (list 0 0))
	     (lin new)
	     (pp (cdr (nth (1- n) math-sum-int-pow-cache)))
	     (p 2)
	     (sum 0)
	     q)
	(while pp
	  (setq q (math-div (car pp) p)
		new (cons (math-mul q n) new)
		sum (math-add sum q)
		p (1+ p)
		pp (cdr pp)))
	(setcar lin (math-sub 1 (math-mul n sum)))
	(setq math-sum-int-pow-cache
	      (nconc math-sum-int-pow-cache (list (nreverse new)))
	      n (1+ n))))
    (nth pow math-sum-int-pow-cache)))

(defun math-to-exponentials (expr)
  (and (consp expr)
       (= (length expr) 2)
       (let ((x (nth 1 expr))
	     (pi (if calc-symbolic-mode '(var pi var-pi) (math-pi)))
	     (i (if calc-symbolic-mode '(var i var-i) '(cplx 0 1))))
	 (cond ((eq (car expr) 'calcFunc-exp)
		(list '^ '(var e var-e) x))
	       ((eq (car expr) 'calcFunc-sin)
		(or (eq calc-angle-mode 'rad)
		    (setq x (list '/ (list '* x pi) 180)))
		(list '/ (list '-
			       (list '^ '(var e var-e) (list '* x i))
			       (list '^ '(var e var-e)
				     (list 'neg (list '* x i))))
		      (list '* 2 i)))
	       ((eq (car expr) 'calcFunc-cos)
		(or (eq calc-angle-mode 'rad)
		    (setq x (list '/ (list '* x pi) 180)))
		(list '/ (list '+
			       (list '^ '(var e var-e)
				     (list '* x i))
			       (list '^ '(var e var-e)
				     (list 'neg (list '* x i))))
		      2))
	       ((eq (car expr) 'calcFunc-sinh)
		(list '/ (list '-
			       (list '^ '(var e var-e) x)
			       (list '^ '(var e var-e) (list 'neg x)))
		      2))
	       ((eq (car expr) 'calcFunc-cosh)
		(list '/ (list '+
			       (list '^ '(var e var-e) x)
			       (list '^ '(var e var-e) (list 'neg x)))
		      2))
	       (t nil)))))

(defun math-to-exps (expr)
  (cond (calc-symbolic-mode expr)
	((Math-primp expr)
	 (if (equal expr '(var e var-e)) (math-e) expr))
	((and (eq (car expr) '^)
	      (equal (nth 1 expr) '(var e var-e)))
	 (list 'calcFunc-exp (nth 2 expr)))
	(t
	 (cons (car expr) (mapcar 'math-to-exps (cdr expr))))))


(defvar math-disable-prods nil)
(defun calcFunc-prod (expr var &optional low high step)
  (if math-disable-prods (math-reject-arg))
  (let* ((res (let* ((calc-internal-prec (+ calc-internal-prec 2)))
		(math-prod-rec expr var low high step)))
	 (math-disable-prods t))
    (math-normalize res)))

(defun math-prod-rec (expr var &optional low high step)
  (or low (setq low '(neg (var inf var-inf)) high '(var inf var-inf)))
  (and low (not high) (setq high '(var inf var-inf)))
  (let (t1 t2 t3 val)
    (setq val
	  (cond
	   ((not (math-expr-contains expr var))
	    (math-pow expr (math-add (math-div (math-sub high low) (or step 1))
				     1)))
	   ((and step (not (math-equal-int step 1)))
	    (if (math-negp step)
		(math-prod-rec expr var high low (math-neg step))
	      (let ((lo (math-simplify (math-div low step))))
		(if (math-known-num-integerp lo)
		    (math-prod-rec (math-normalize
				    (math-expr-subst expr var
						     (math-mul step var)))
				   var lo (math-simplify (math-div high step)))
		  (math-prod-rec (math-normalize
				  (math-expr-subst expr var
						   (math-add (math-mul step
								       var)
							     low)))
				 var 0
				 (math-simplify (math-div (math-sub high low)
							  step)))))))
	   ((and (memq (car expr) '(* /))
		 (setq t1 (math-prod-rec (nth 1 expr) var low high)
		       t2 (math-prod-rec (nth 2 expr) var low high))
		 (not (and (math-expr-calls t1 '(calcFunc-prod))
			   (math-expr-calls t2 '(calcFunc-prod)))))
	    (list (car expr) t1 t2))
	   ((and (eq (car expr) '^)
		 (not (math-expr-contains (nth 2 expr) var)))
	    (math-pow (math-prod-rec (nth 1 expr) var low high)
		      (nth 2 expr)))
	   ((and (eq (car expr) '^)
		 (not (math-expr-contains (nth 1 expr) var)))
	    (math-pow (nth 1 expr)
		      (calcFunc-sum (nth 2 expr) var low high)))
	   ((eq (car expr) 'sqrt)
	    (math-normalize (list 'calcFunc-sqrt
				  (list 'calcFunc-prod (nth 1 expr)
					var low high))))
	   ((eq (car expr) 'neg)
	    (math-mul (math-pow -1 (math-add (math-sub high low) 1))
		      (math-prod-rec (nth 1 expr) var low high)))
	   ((eq (car expr) 'calcFunc-exp)
	    (list 'calcFunc-exp (calcFunc-sum (nth 1 expr) var low high)))
	   ((and (setq t1 (math-is-polynomial expr var 1))
		 (setq t2
		       (cond
			((or (and (math-equal-int (nth 1 t1) 1)
				  (setq low (math-simplify
					     (math-add low (car t1)))
					high (math-simplify
					      (math-add high (car t1)))))
			     (and (math-equal-int (nth 1 t1) -1)
				  (setq t2 low
					low (math-simplify
					     (math-sub (car t1) high))
					high (math-simplify
					      (math-sub (car t1) t2)))))
			 (if (or (math-zerop low) (math-zerop high))
			     0
			   (if (and (or (math-negp low) (math-negp high))
				    (or (math-num-integerp low)
					(math-num-integerp high)))
			       (if (math-posp high)
				   0
				 (math-mul (math-pow -1
						     (math-add
						      (math-add low high) 1))
					   (list '/
						 (list 'calcFunc-fact
						       (math-neg low))
						 (list 'calcFunc-fact
						       (math-sub -1 high)))))
			     (list '/
				   (list 'calcFunc-fact high)
				   (list 'calcFunc-fact (math-sub low 1))))))
			((and (or (and (math-equal-int (nth 1 t1) 2)
				       (setq t2 (math-simplify
						 (math-add (math-mul low 2)
							   (car t1)))
					     t3 (math-simplify
						 (math-add (math-mul high 2)
							   (car t1)))))
				  (and (math-equal-int (nth 1 t1) -2)
				       (setq t2 (math-simplify
						 (math-sub (car t1)
							   (math-mul high 2)))
					     t3 (math-simplify
						 (math-sub (car t1)
							   (math-mul low
								     2))))))
			      (or (math-integerp t2)
				  (and (math-messy-integerp t2)
				       (setq t2 (math-trunc t2)))
				  (math-integerp t3)
				  (and (math-messy-integerp t3)
				       (setq t3 (math-trunc t3)))))
			 (if (or (math-zerop t2) (math-zerop t3))
			     0
			   (if (or (math-evenp t2) (math-evenp t3))
			       (if (or (math-negp t2) (math-negp t3))
				   (if (math-posp high)
				       0
				     (list '/
					   (list 'calcFunc-dfact
						 (math-neg t2))
					   (list 'calcFunc-dfact
						 (math-sub -2 t3))))
				 (list '/
				       (list 'calcFunc-dfact t3)
				       (list 'calcFunc-dfact
					     (math-sub t2 2))))
			     (if (math-negp t3)
				 (list '*
				       (list '^ -1
					     (list '/ (list '- (list '- t2 t3)
							    2)
						   2))
				       (list '/
					     (list 'calcFunc-dfact
						   (math-neg t2))
					     (list 'calcFunc-dfact
						   (math-sub -2 t3))))
			       (if (math-posp t2)
				   (list '/
					 (list 'calcFunc-dfact t3)
					 (list 'calcFunc-dfact
					       (math-sub t2 2)))
				 nil))))))))
	    t2)))
    (if (equal val '(var nan var-nan)) (setq val nil))
    (or val
	(let* ((math-tabulate-initial 1)
	       (math-tabulate-function 'calcFunc-prod))
	  (calcFunc-table expr var low high)))))




(defvar math-solve-ranges nil)
(defvar math-solve-sign)
;;; Attempt to reduce math-solve-lhs = math-solve-rhs to
;;; math-solve-var = math-solve-rhs', where math-solve-var appears
;;; in math-solve-lhs but not in math-solve-rhs or math-solve-rhs';
;;; return math-solve-rhs'.
;;; Uses global values: math-solve-var, math-solve-full.
(defvar math-solve-var)
(defvar math-solve-full)

;; The variables math-solve-lhs, math-solve-rhs and math-try-solve-sign
;; are local to math-try-solve-for,  but are used by math-try-solve-prod.
;; (math-solve-lhs and math-solve-rhs are is also local to
;; math-decompose-poly, but used by math-solve-poly-funny-powers.)
(defvar math-solve-lhs)
(defvar math-solve-rhs)
(defvar math-try-solve-sign)

(defun math-try-solve-for
  (math-solve-lhs math-solve-rhs &optional math-try-solve-sign no-poly)
  (let (math-t1 math-t2 math-t3)
    (cond ((equal math-solve-lhs math-solve-var)
	   (setq math-solve-sign math-try-solve-sign)
	   (if (eq math-solve-full 'all)
	       (let ((vec (list 'vec (math-evaluate-expr math-solve-rhs)))
		     newvec var p)
		 (while math-solve-ranges
		   (setq p (car math-solve-ranges)
			 var (car p)
			 newvec (list 'vec))
		   (while (setq p (cdr p))
		     (setq newvec (nconc newvec
					 (cdr (math-expr-subst
					       vec var (car p))))))
		   (setq vec newvec
			 math-solve-ranges (cdr math-solve-ranges)))
		 (math-normalize vec))
	     math-solve-rhs))
	  ((Math-primp math-solve-lhs)
	   nil)
	  ((and (eq (car math-solve-lhs) '-)
		(eq (car-safe (nth 1 math-solve-lhs)) (car-safe (nth 2 math-solve-lhs)))
		(Math-zerop math-solve-rhs)
		(= (length (nth 1 math-solve-lhs)) 2)
		(= (length (nth 2 math-solve-lhs)) 2)
		(setq math-t1 (get (car (nth 1 math-solve-lhs)) 'math-inverse))
		(setq math-t2 (funcall math-t1 '(var SOLVEDUM SOLVEDUM)))
		(eq (math-expr-contains-count math-t2 '(var SOLVEDUM SOLVEDUM)) 1)
		(setq math-t3 (math-solve-above-dummy math-t2))
		(setq math-t1 (math-try-solve-for
                               (math-sub (nth 1 (nth 1 math-solve-lhs))
                                         (math-expr-subst
                                          math-t2 math-t3
                                          (nth 1 (nth 2 math-solve-lhs))))
                               0)))
	   math-t1)
	  ((eq (car math-solve-lhs) 'neg)
	   (math-try-solve-for (nth 1 math-solve-lhs) (math-neg math-solve-rhs)
			       (and math-try-solve-sign (- math-try-solve-sign))))
	  ((and (not (eq math-solve-full 't)) (math-try-solve-prod)))
	  ((and (not no-poly)
		(setq math-t2
                      (math-decompose-poly math-solve-lhs
                                           math-solve-var 15 math-solve-rhs)))
	   (setq math-t1 (cdr (nth 1 math-t2))
		 math-t1 (let ((math-solve-ranges math-solve-ranges))
		      (cond ((= (length math-t1) 5)
			     (apply 'math-solve-quartic (car math-t2) math-t1))
			    ((= (length math-t1) 4)
			     (apply 'math-solve-cubic (car math-t2) math-t1))
			    ((= (length math-t1) 3)
			     (apply 'math-solve-quadratic (car math-t2) math-t1))
			    ((= (length math-t1) 2)
			     (apply 'math-solve-linear
                                    (car math-t2) math-try-solve-sign math-t1))
			    (math-solve-full
			     (math-poly-all-roots (car math-t2) math-t1))
			    (calc-symbolic-mode nil)
			    (t
			     (math-try-solve-for
			      (car math-t2)
			      (math-poly-any-root (reverse math-t1) 0 t)
			      nil t)))))
	   (if math-t1
	       (if (eq (nth 2 math-t2) 1)
		   math-t1
		 (math-solve-prod math-t1 (math-try-solve-for (nth 2 math-t2) 0 nil t)))
	     (calc-record-why "*Unable to find a symbolic solution")
	     nil))
	  ((and (math-solve-find-root-term math-solve-lhs nil)
		(eq (math-expr-contains-count math-solve-lhs math-t1) 1))   ; just in case
	   (math-try-solve-for (math-simplify
				(math-sub (if (or math-t3 (math-evenp math-t2))
					      (math-pow math-t1 math-t2)
					    (math-neg (math-pow math-t1 math-t2)))
					  (math-expand-power
					   (math-sub (math-normalize
						      (math-expr-subst
						       math-solve-lhs math-t1 0))
						     math-solve-rhs)
					   math-t2 math-solve-var)))
			       0))
	  ((eq (car math-solve-lhs) '+)
	   (cond ((not (math-expr-contains (nth 1 math-solve-lhs) math-solve-var))
		  (math-try-solve-for (nth 2 math-solve-lhs)
				      (math-sub math-solve-rhs (nth 1 math-solve-lhs))
				      math-try-solve-sign))
		 ((not (math-expr-contains (nth 2 math-solve-lhs) math-solve-var))
		  (math-try-solve-for (nth 1 math-solve-lhs)
				      (math-sub math-solve-rhs (nth 2 math-solve-lhs))
				      math-try-solve-sign))))
	  ((eq (car math-solve-lhs) 'calcFunc-eq)
	   (math-try-solve-for (math-sub (nth 1 math-solve-lhs) (nth 2 math-solve-lhs))
			       math-solve-rhs math-try-solve-sign no-poly))
	  ((eq (car math-solve-lhs) '-)
	   (cond ((or (and (eq (car-safe (nth 1 math-solve-lhs)) 'calcFunc-sin)
			   (eq (car-safe (nth 2 math-solve-lhs)) 'calcFunc-cos))
		      (and (eq (car-safe (nth 1 math-solve-lhs)) 'calcFunc-cos)
			   (eq (car-safe (nth 2 math-solve-lhs)) 'calcFunc-sin)))
		  (math-try-solve-for (math-sub (nth 1 math-solve-lhs)
						(list (car (nth 1 math-solve-lhs))
						      (math-sub
						       (math-quarter-circle t)
						       (nth 1 (nth 2 math-solve-lhs)))))
				      math-solve-rhs))
		 ((not (math-expr-contains (nth 1 math-solve-lhs) math-solve-var))
		  (math-try-solve-for (nth 2 math-solve-lhs)
				      (math-sub (nth 1 math-solve-lhs) math-solve-rhs)
				      (and math-try-solve-sign
                                           (- math-try-solve-sign))))
		 ((not (math-expr-contains (nth 2 math-solve-lhs) math-solve-var))
		  (math-try-solve-for (nth 1 math-solve-lhs)
				      (math-add math-solve-rhs (nth 2 math-solve-lhs))
				      math-try-solve-sign))))
	  ((and (eq math-solve-full 't) (math-try-solve-prod)))
	  ((and (eq (car math-solve-lhs) '%)
		(not (math-expr-contains (nth 2 math-solve-lhs) math-solve-var)))
	   (math-try-solve-for (nth 1 math-solve-lhs) (math-add math-solve-rhs
						     (math-solve-get-int
						      (nth 2 math-solve-lhs)))))
	  ((eq (car math-solve-lhs) 'calcFunc-log)
	   (cond ((not (math-expr-contains (nth 2 math-solve-lhs) math-solve-var))
		  (math-try-solve-for (nth 1 math-solve-lhs)
                                      (math-pow (nth 2 math-solve-lhs) math-solve-rhs)))
		 ((not (math-expr-contains (nth 1 math-solve-lhs) math-solve-var))
		  (math-try-solve-for (nth 2 math-solve-lhs) (math-pow
						   (nth 1 math-solve-lhs)
						   (math-div 1 math-solve-rhs))))))
	  ((and (= (length math-solve-lhs) 2)
		(symbolp (car math-solve-lhs))
		(setq math-t1 (get (car math-solve-lhs) 'math-inverse))
		(setq math-t2 (funcall math-t1 math-solve-rhs)))
	   (setq math-t1 (get (car math-solve-lhs) 'math-inverse-sign))
	   (math-try-solve-for (nth 1 math-solve-lhs) (math-normalize math-t2)
			       (and math-try-solve-sign math-t1
				    (if (integerp math-t1)
					(* math-t1 math-try-solve-sign)
				      (funcall math-t1 math-solve-lhs
                                               math-try-solve-sign)))))
	  ((and (symbolp (car math-solve-lhs))
		(setq math-t1 (get (car math-solve-lhs) 'math-inverse-n))
		(setq math-t2 (funcall math-t1 math-solve-lhs math-solve-rhs)))
	   math-t2)
	  ((setq math-t1 (math-expand-formula math-solve-lhs))
	   (math-try-solve-for math-t1 math-solve-rhs math-try-solve-sign))
	  (t
	   (calc-record-why "*No inverse known" math-solve-lhs)
	   nil))))


(defun math-try-solve-prod ()
  (cond ((eq (car math-solve-lhs) '*)
	 (cond ((not (math-expr-contains (nth 1 math-solve-lhs) math-solve-var))
		(math-try-solve-for (nth 2 math-solve-lhs)
				    (math-div math-solve-rhs (nth 1 math-solve-lhs))
				    (math-solve-sign math-try-solve-sign
                                                     (nth 1 math-solve-lhs))))
	       ((not (math-expr-contains (nth 2 math-solve-lhs) math-solve-var))
		(math-try-solve-for (nth 1 math-solve-lhs)
				    (math-div math-solve-rhs (nth 2 math-solve-lhs))
				    (math-solve-sign math-try-solve-sign
                                                     (nth 2 math-solve-lhs))))
	       ((Math-zerop math-solve-rhs)
		(math-solve-prod (let ((math-solve-ranges math-solve-ranges))
				   (math-try-solve-for (nth 2 math-solve-lhs) 0))
				 (math-try-solve-for (nth 1 math-solve-lhs) 0)))))
	((eq (car math-solve-lhs) '/)
	 (cond ((not (math-expr-contains (nth 1 math-solve-lhs) math-solve-var))
		(math-try-solve-for (nth 2 math-solve-lhs)
				    (math-div (nth 1 math-solve-lhs) math-solve-rhs)
				    (math-solve-sign math-try-solve-sign
                                                     (nth 1 math-solve-lhs))))
	       ((not (math-expr-contains (nth 2 math-solve-lhs) math-solve-var))
		(math-try-solve-for (nth 1 math-solve-lhs)
				    (math-mul math-solve-rhs (nth 2 math-solve-lhs))
				    (math-solve-sign math-try-solve-sign
                                                     (nth 2 math-solve-lhs))))
	       ((setq math-t1 (math-try-solve-for (math-sub (nth 1 math-solve-lhs)
						       (math-mul (nth 2 math-solve-lhs)
								 math-solve-rhs))
					     0))
		math-t1)))
	((eq (car math-solve-lhs) '^)
	 (cond ((not (math-expr-contains (nth 1 math-solve-lhs) math-solve-var))
		(math-try-solve-for
		 (nth 2 math-solve-lhs)
		 (math-add (math-normalize
			    (list 'calcFunc-log math-solve-rhs (nth 1 math-solve-lhs)))
			   (math-div
			    (math-mul 2
				      (math-mul '(var pi var-pi)
						(math-solve-get-int
						 '(var i var-i))))
			    (math-normalize
			     (list 'calcFunc-ln (nth 1 math-solve-lhs)))))))
	       ((not (math-expr-contains (nth 2 math-solve-lhs) math-solve-var))
		(cond ((and (integerp (nth 2 math-solve-lhs))
			    (>= (nth 2 math-solve-lhs) 2)
			    (setq math-t1 (math-integer-log2 (nth 2 math-solve-lhs))))
		       (setq math-t2 math-solve-rhs)
		       (if (and (eq math-solve-full t)
				(math-known-realp (nth 1 math-solve-lhs)))
			   (progn
			     (while (>= (setq math-t1 (1- math-t1)) 0)
			       (setq math-t2 (list 'calcFunc-sqrt math-t2)))
			     (setq math-t2 (math-solve-get-sign math-t2)))
			 (while (>= (setq math-t1 (1- math-t1)) 0)
			   (setq math-t2 (math-solve-get-sign
				     (math-normalize
				      (list 'calcFunc-sqrt math-t2))))))
		       (math-try-solve-for
			(nth 1 math-solve-lhs)
			(math-normalize math-t2)))
		      ((math-looks-negp (nth 2 math-solve-lhs))
		       (math-try-solve-for
			(list '^ (nth 1 math-solve-lhs)
                              (math-neg (nth 2 math-solve-lhs)))
			(math-div 1 math-solve-rhs)))
		      ((and (eq math-solve-full t)
			    (Math-integerp (nth 2 math-solve-lhs))
			    (math-known-realp (nth 1 math-solve-lhs)))
		       (setq math-t1 (math-normalize
				 (list 'calcFunc-nroot math-solve-rhs
                                       (nth 2 math-solve-lhs))))
		       (if (math-evenp (nth 2 math-solve-lhs))
			   (setq math-t1 (math-solve-get-sign math-t1)))
		       (math-try-solve-for
			(nth 1 math-solve-lhs) math-t1
			(and math-try-solve-sign
			     (math-oddp (nth 2 math-solve-lhs))
			     (math-solve-sign math-try-solve-sign
                                              (nth 2 math-solve-lhs)))))
		      (t (math-try-solve-for
			  (nth 1 math-solve-lhs)
			  (math-mul
			   (math-normalize
			    (list 'calcFunc-exp
				  (if (Math-realp (nth 2 math-solve-lhs))
				      (math-div (math-mul
						 '(var pi var-pi)
						 (math-solve-get-int
						  '(var i var-i)
						  (and (integerp (nth 2 math-solve-lhs))
						       (math-abs
							(nth 2 math-solve-lhs)))))
						(math-div (nth 2 math-solve-lhs) 2))
				    (math-div (math-mul
					       2
					       (math-mul
						'(var pi var-pi)
						(math-solve-get-int
						 '(var i var-i)
						 (and (integerp (nth 2 math-solve-lhs))
						      (math-abs
						       (nth 2 math-solve-lhs))))))
					      (nth 2 math-solve-lhs)))))
			   (math-normalize
			    (list 'calcFunc-nroot
				  math-solve-rhs
				  (nth 2 math-solve-lhs))))
			  (and math-try-solve-sign
			       (math-oddp (nth 2 math-solve-lhs))
			       (math-solve-sign math-try-solve-sign
                                                (nth 2 math-solve-lhs)))))))))
	(t nil)))

(defun math-solve-prod (lsoln rsoln)
  (cond ((null lsoln)
	 rsoln)
	((null rsoln)
	 lsoln)
	((eq math-solve-full 'all)
	 (cons 'vec (append (cdr lsoln) (cdr rsoln))))
	(math-solve-full
	 (list 'calcFunc-if
	       (list 'calcFunc-gt (math-solve-get-sign 1) 0)
	       lsoln
	       rsoln))
	(t lsoln)))

;;; This deals with negative, fractional, and symbolic powers of "x".
;; The variable math-solve-b is local to math-decompose-poly,
;; but is used by math-solve-poly-funny-powers.
(defvar math-solve-b)

(defun math-solve-poly-funny-powers (sub-rhs)    ; uses "t1", "t2"
  (setq math-t1 math-solve-lhs)
  (let ((pp math-poly-neg-powers)
	fac)
    (while pp
      (setq fac (math-pow (car pp) (or math-poly-mult-powers 1))
	    math-t1 (math-mul math-t1 fac)
	    math-solve-rhs (math-mul math-solve-rhs fac)
	    pp (cdr pp))))
  (if sub-rhs (setq math-t1 (math-sub math-t1 math-solve-rhs)))
  (let ((math-poly-neg-powers nil))
    (setq math-t2 (math-mul (or math-poly-mult-powers 1)
		       (let ((calc-prefer-frac t))
			 (math-div 1 math-poly-frac-powers)))
	  math-t1 (math-is-polynomial
                   (math-simplify (calcFunc-expand math-t1)) math-solve-b 50))))

;;; This converts "a x^8 + b x^5 + c x^2" to "(a (x^3)^2 + b (x^3) + c) * x^2".
(defun math-solve-crunch-poly (max-degree)   ; uses "t1", "t3"
  (let ((count 0))
    (while (and math-t1 (Math-zerop (car math-t1)))
      (setq math-t1 (cdr math-t1)
	    count (1+ count)))
    (and math-t1
	 (let* ((degree (1- (length math-t1)))
		(scale degree))
	   (while (and (> scale 1) (= (car math-t3) 1))
	     (and (= (% degree scale) 0)
		  (let ((p math-t1)
			(n 0)
			(new-t1 nil)
			(okay t))
		    (while (and p okay)
		      (if (= (% n scale) 0)
			  (setq new-t1 (nconc new-t1 (list (car p))))
			(or (Math-zerop (car p))
			    (setq okay nil)))
		      (setq p (cdr p)
			    n (1+ n)))
		    (if okay
			(setq math-t3 (cons scale (cdr math-t3))
			      math-t1 new-t1))))
	     (setq scale (1- scale)))
	   (setq math-t3 (list (math-mul (car math-t3) math-t2)
                               (math-mul count math-t2)))
	   (<= (1- (length math-t1)) max-degree)))))

(defun calcFunc-poly (expr var &optional degree)
  (if degree
      (or (natnump degree) (math-reject-arg degree 'fixnatnump))
    (setq degree 50))
  (let ((p (math-is-polynomial expr var degree 'gen)))
    (if p
	(if (equal p '(0))
	    (list 'vec)
	  (cons 'vec p))
      (math-reject-arg expr "Expected a polynomial"))))

(defun calcFunc-gpoly (expr var &optional degree)
  (if degree
      (or (natnump degree) (math-reject-arg degree 'fixnatnump))
    (setq degree 50))
  (let* ((math-poly-base-variable var)
	 (d (math-decompose-poly expr var degree nil)))
    (if d
	(cons 'vec d)
      (math-reject-arg expr "Expected a polynomial"))))

(defun math-decompose-poly (math-solve-lhs math-solve-var degree sub-rhs)
  (let ((math-solve-rhs (or sub-rhs 1))
	math-t1 math-t2 math-t3)
    (setq math-t2 (math-polynomial-base
	      math-solve-lhs
	      (function
	       (lambda (math-solve-b)
		 (let ((math-poly-neg-powers '(1))
		       (math-poly-mult-powers nil)
		       (math-poly-frac-powers 1)
		       (math-poly-exp-base t))
		   (and (not (equal math-solve-b math-solve-lhs))
			(or (not (memq (car-safe math-solve-b) '(+ -))) sub-rhs)
			(setq math-t3 '(1 0) math-t2 1
			      math-t1 (math-is-polynomial math-solve-lhs
                                                          math-solve-b 50))
			(if (and (equal math-poly-neg-powers '(1))
				 (memq math-poly-mult-powers '(nil 1))
				 (eq math-poly-frac-powers 1)
				 sub-rhs)
			    (setq math-t1 (cons (math-sub (car math-t1) math-solve-rhs)
					   (cdr math-t1)))
			  (math-solve-poly-funny-powers sub-rhs))
			(math-solve-crunch-poly degree)
			(or (math-expr-contains math-solve-b math-solve-var)
			    (math-expr-contains (car math-t3) math-solve-var))))))))
    (if math-t2
	(list (math-pow math-t2 (car math-t3))
	      (cons 'vec math-t1)
	      (if sub-rhs
		  (math-pow math-t2 (nth 1 math-t3))
		(math-div (math-pow math-t2 (nth 1 math-t3)) math-solve-rhs))))))

(defun math-solve-linear (var sign b a)
  (math-try-solve-for var
		      (math-div (math-neg b) a)
		      (math-solve-sign sign a)
		      t))

(defun math-solve-quadratic (var c b a)
  (math-try-solve-for
   var
   (if (math-looks-evenp b)
       (let ((halfb (math-div b 2)))
	 (math-div
	  (math-add
	   (math-neg halfb)
	   (math-solve-get-sign
	    (math-normalize
	     (list 'calcFunc-sqrt
		   (math-add (math-sqr halfb)
			     (math-mul (math-neg c) a))))))
	  a))
     (math-div
      (math-add
       (math-neg b)
       (math-solve-get-sign
	(math-normalize
	 (list 'calcFunc-sqrt
	       (math-add (math-sqr b)
			 (math-mul 4 (math-mul (math-neg c) a)))))))
      (math-mul 2 a)))
   nil t))

(defun math-solve-cubic (var d c b a)
  (let* ((p (math-div b a))
	 (q (math-div c a))
	 (r (math-div d a))
	 (psqr (math-sqr p))
	 (aa (math-sub q (math-div psqr 3)))
	 (bb (math-add r
		       (math-div (math-sub (math-mul 2 (math-mul psqr p))
					   (math-mul 9 (math-mul p q)))
				 27)))
	 m)
    (if (Math-zerop aa)
	(math-try-solve-for (math-pow (math-add var (math-div p 3)) 3)
			    (math-neg bb) nil t)
      (if (Math-zerop bb)
	  (math-try-solve-for
	   (math-mul (math-add var (math-div p 3))
		     (math-add (math-sqr (math-add var (math-div p 3)))
			       aa))
	   0 nil t)
	(setq m (math-mul 2 (list 'calcFunc-sqrt (math-div aa -3))))
	(math-try-solve-for
	 var
	 (math-sub
	  (math-normalize
	   (math-mul
	    m
	    (list 'calcFunc-cos
		  (math-div
		   (math-sub (list 'calcFunc-arccos
				   (math-div (math-mul 3 bb)
					     (math-mul aa m)))
			     (math-mul 2
				       (math-mul
					(math-add 1 (math-solve-get-int
						     1 3))
					(math-half-circle
					 calc-symbolic-mode))))
		   3))))
	  (math-div p 3))
	 nil t)))))

(defun math-solve-quartic (var d c b a aa)
  (setq a (math-div a aa))
  (setq b (math-div b aa))
  (setq c (math-div c aa))
  (setq d (math-div d aa))
  (math-try-solve-for
   var
   (let* ((asqr (math-sqr a))
	  (asqr4 (math-div asqr 4))
	  (y (let ((math-solve-full nil)
		   calc-next-why)
	       (math-solve-cubic math-solve-var
				 (math-sub (math-sub
					    (math-mul 4 (math-mul b d))
					    (math-mul asqr d))
					   (math-sqr c))
				 (math-sub (math-mul a c)
					   (math-mul 4 d))
				 (math-neg b)
				 1)))
	  (rsqr (math-add (math-sub asqr4 b) y))
	  (r (list 'calcFunc-sqrt rsqr))
	  (sign1 (math-solve-get-sign 1))
	  (de (list 'calcFunc-sqrt
		    (math-add
		     (math-sub (math-mul 3 asqr4)
			       (math-mul 2 b))
		     (if (Math-zerop rsqr)
			 (math-mul
			  2
			  (math-mul sign1
				    (list 'calcFunc-sqrt
					  (math-sub (math-sqr y)
						    (math-mul 4 d)))))
		       (math-sub
			(math-mul sign1
				  (math-div
				   (math-sub (math-sub
					      (math-mul 4 (math-mul a b))
					      (math-mul 8 c))
					     (math-mul asqr a))
				   (math-mul 4 r)))
			rsqr))))))
     (math-normalize
      (math-sub (math-add (math-mul sign1 (math-div r 2))
			  (math-solve-get-sign (math-div de 2)))
		(math-div a 4))))
   nil t))

(defvar math-symbolic-solve nil)
(defvar math-int-coefs nil)

;; The variable math-int-threshold is local to math-poly-all-roots,
;; but is used by math-poly-newton-root.
(defvar math-int-threshold)
;; The variables math-int-scale, math-int-factors and math-double-roots
;; are local to math-poly-all-roots, but are used by math-poly-integer-root.
(defvar math-int-scale)
(defvar math-int-factors)
(defvar math-double-roots)

(defun math-poly-all-roots (var p &optional math-factoring)
  (catch 'ouch
    (let* ((math-symbolic-solve calc-symbolic-mode)
	   (roots nil)
	   (deg (1- (length p)))
	   (orig-p (reverse p))
	   (math-int-coefs nil)
	   (math-int-scale nil)
	   (math-double-roots nil)
	   (math-int-factors nil)
	   (math-int-threshold nil)
	   (pp p))
      ;; If rational coefficients, look for exact rational factors.
      (while (and pp (Math-ratp (car pp)))
	(setq pp (cdr pp)))
      (if pp
	  (if (or math-factoring math-symbolic-solve)
	      (throw 'ouch nil))
	(let ((lead (car orig-p))
	      (calc-prefer-frac t)
	      (scale (apply 'math-lcm-denoms p)))
	  (setq math-int-scale (math-abs (math-mul scale lead))
		math-int-threshold (math-div '(float 5 -2) math-int-scale)
		math-int-coefs (cdr (math-div (cons 'vec orig-p) lead)))))
      (if (> deg 4)
	  (let ((calc-prefer-frac nil)
		(calc-symbolic-mode nil)
		(pp p)
		(def-p (copy-sequence orig-p)))
	    (while pp
	      (if (Math-numberp (car pp))
		  (setq pp (cdr pp))
		(throw 'ouch nil)))
	    (while (> deg (if math-symbolic-solve 2 4))
	      (let* ((x (math-poly-any-root def-p '(float 0 0) nil))
		     b c pp)
		(if (and (eq (car-safe x) 'cplx)
			 (math-nearly-zerop (nth 2 x) (nth 1 x)))
		    (setq x (calcFunc-re x)))
		(or math-factoring
		    (setq roots (cons x roots)))
		(or (math-numberp x)
		    (setq x (math-evaluate-expr x)))
		(setq pp def-p
		      b (car def-p))
		(while (setq pp (cdr pp))
		  (setq c (car pp))
		  (setcar pp b)
		  (setq b (math-add (math-mul x b) c)))
		(setq def-p (cdr def-p)
		      deg (1- deg))))
	    (setq p (reverse def-p))))
      (if (> deg 1)
	  (let ((math-solve-var '(var DUMMY var-DUMMY))
		(math-solve-sign nil)
		(math-solve-ranges nil)
		(math-solve-full 'all))
	    (if (= (length p) (length math-int-coefs))
		(setq p (reverse math-int-coefs)))
	    (setq roots (append (cdr (apply (cond ((= deg 2)
						   'math-solve-quadratic)
						  ((= deg 3)
						   'math-solve-cubic)
						  (t
						   'math-solve-quartic))
					    math-solve-var p))
				roots)))
	(if (> deg 0)
	    (setq roots (cons (math-div (math-neg (car p)) (nth 1 p))
			      roots))))
      (if math-factoring
	  (progn
	    (while roots
	      (math-poly-integer-root (car roots))
	      (setq roots (cdr roots)))
	    (list math-int-factors (nreverse math-int-coefs) math-int-scale))
	(let ((vec nil) res)
	  (while roots
	    (let ((root (car roots))
		  (math-solve-full (and math-solve-full 'all)))
	      (if (math-floatp root)
		  (setq root (math-poly-any-root orig-p root t)))
	      (setq vec (append vec
				(cdr (or (math-try-solve-for var root nil t)
					 (throw 'ouch nil))))))
	    (setq roots (cdr roots)))
	  (setq vec (cons 'vec (nreverse vec)))
	  (if math-symbolic-solve
	      (setq vec (math-normalize vec)))
	  (if (eq math-solve-full t)
	      (list 'calcFunc-subscr
		    vec
		    (math-solve-get-int 1 (1- (length orig-p)) 1))
	    vec))))))

(defun math-lcm-denoms (&rest fracs)
  (let ((den 1))
    (while fracs
      (if (eq (car-safe (car fracs)) 'frac)
	  (setq den (calcFunc-lcm den (nth 2 (car fracs)))))
      (setq fracs (cdr fracs)))
    den))

(defun math-poly-any-root (p x polish)    ; p is a reverse poly coeff list
  (let* ((newt (if (math-zerop x)
		   (math-poly-newton-root
		    p '(cplx (float 123 -6) (float 1 -4)) 4)
		 (math-poly-newton-root p x 4)))
	 (res (if (math-zerop (cdr newt))
		  (car newt)
		(if (and (math-lessp (cdr newt) '(float 1 -3)) (not polish))
		    (setq newt (math-poly-newton-root p (car newt) 30)))
		(if (math-zerop (cdr newt))
		    (car newt)
		  (math-poly-laguerre-root p x polish)))))
    (and math-symbolic-solve (math-floatp res)
	 (throw 'ouch nil))
    res))

(defun math-poly-newton-root (p x iters)
  (let* ((calc-prefer-frac nil)
	 (calc-symbolic-mode nil)
	 (try-integer math-int-coefs)
	 (dx x) b d)
    (while (and (> (setq iters (1- iters)) 0)
		(let ((pp p))
		  (math-working "newton" x)
		  (setq b (car p)
			d 0)
		  (while (setq pp (cdr pp))
		    (setq d (math-add (math-mul x d) b)
			  b (math-add (math-mul x b) (car pp))))
		  (not (math-zerop d)))
		(progn
		  (setq dx (math-div b d)
			x (math-sub x dx))
		  (if try-integer
		      (let ((adx (math-abs-approx dx)))
			(and (math-lessp adx math-int-threshold)
			     (let ((iroot (math-poly-integer-root x)))
			       (if iroot
				   (setq x iroot dx 0)
				 (setq try-integer nil))))))
		  (or (not (or (eq dx 0)
			       (math-nearly-zerop dx (math-abs-approx x))))
		      (progn (setq dx 0) nil)))))
    (cons x (if (math-zerop x)
		1 (math-div (math-abs-approx dx) (math-abs-approx x))))))

(defun math-poly-integer-root (x)
  (and (math-lessp (calcFunc-xpon (math-abs-approx x)) calc-internal-prec)
       math-int-coefs
       (let* ((calc-prefer-frac t)
	      (xre (calcFunc-re x))
	      (xim (calcFunc-im x))
	      (xresq (math-sqr xre))
	      (ximsq (math-sqr xim)))
	 (if (math-lessp ximsq (calcFunc-scf xresq -1))
	     ;; Look for linear factor
	     (let* ((rnd (math-div (math-round (math-mul xre math-int-scale))
				   math-int-scale))
		    (icp math-int-coefs)
		    (rem (car icp))
		    (newcoef nil))
	       (while (setq icp (cdr icp))
		 (setq newcoef (cons rem newcoef)
		       rem (math-add (car icp)
				     (math-mul rem rnd))))
	       (and (math-zerop rem)
		    (progn
		      (setq math-int-coefs (nreverse newcoef)
			    math-int-factors (cons (list (math-neg rnd))
						   math-int-factors))
		      rnd)))
	   ;; Look for irreducible quadratic factor
	   (let* ((rnd1 (math-div (math-round
				   (math-mul xre (math-mul -2 math-int-scale)))
				  math-int-scale))
		  (sqscale (math-sqr math-int-scale))
		  (rnd0 (math-div (math-round (math-mul (math-add xresq ximsq)
							sqscale))
				  sqscale))
		  (rem1 (car math-int-coefs))
		  (icp (cdr math-int-coefs))
		  (rem0 (car icp))
		  (newcoef nil)
		  (found (assoc (list rnd0 rnd1 (math-posp xim))
				math-double-roots))
		  this)
	     (if found
		 (setq math-double-roots (delq found math-double-roots)
		       rem0 0 rem1 0)
	       (while (setq icp (cdr icp))
		 (setq this rem1
		       newcoef (cons rem1 newcoef)
		       rem1 (math-sub rem0 (math-mul this rnd1))
		       rem0 (math-sub (car icp) (math-mul this rnd0)))))
	     (and (math-zerop rem0)
		  (math-zerop rem1)
		  (let ((aa (math-div rnd1 -2)))
		    (or found (setq math-int-coefs (reverse newcoef)
				    math-double-roots (cons (list
							     (list
							      rnd0 rnd1
							      (math-negp xim)))
							    math-double-roots)
				    math-int-factors (cons (cons rnd0 rnd1)
							   math-int-factors)))
		    (math-add aa
			      (let ((calc-symbolic-mode math-symbolic-solve))
				(math-mul (math-sqrt (math-sub (math-sqr aa)
							       rnd0))
					  (if (math-negp xim) -1 1)))))))))))

;;; The following routine is from Numerical Recipes, section 9.5.
(defun math-poly-laguerre-root (p x polish)
  (let* ((calc-prefer-frac nil)
	 (calc-symbolic-mode nil)
	 (iters 0)
	 (m (1- (length p)))
	 (try-newt (not polish))
	 (tried-newt nil)
	 b d f x1 dx dxold)
    (while
	(and (or (< (setq iters (1+ iters)) 50)
		 (math-reject-arg x "*Laguerre's method failed to converge"))
	     (let ((err (math-abs-approx (car p)))
		   (abx (math-abs-approx x))
		   (pp p))
	       (setq b (car p)
		     d 0 f 0)
	       (while (setq pp (cdr pp))
		 (setq f (math-add (math-mul x f) d)
		       d (math-add (math-mul x d) b)
		       b (math-add (math-mul x b) (car pp))
		       err (math-add (math-abs-approx b) (math-mul abx err))))
	       (math-lessp (calcFunc-scf err (- -2 calc-internal-prec))
			   (math-abs-approx b)))
	     (or (not (math-zerop d))
		 (not (math-zerop f))
		 (progn
		   (setq x (math-pow (math-neg b) (list 'frac 1 m)))
		   nil))
	     (let* ((g (math-div d b))
		    (g2 (math-sqr g))
		    (h (math-sub g2 (math-mul 2 (math-div f b))))
		    (sq (math-sqrt
			 (math-mul (1- m) (math-sub (math-mul m h) g2))))
		    (gp (math-add g sq))
		    (gm (math-sub g sq)))
	       (if (math-lessp (calcFunc-abssqr gp) (calcFunc-abssqr gm))
		   (setq gp gm))
	       (setq dx (math-div m gp)
		     x1 (math-sub x dx))
	       (if (and try-newt
			(math-lessp (math-abs-approx dx)
				    (calcFunc-scf (math-abs-approx x) -3)))
		   (let ((newt (math-poly-newton-root p x1 7)))
		     (setq tried-newt t
			   try-newt nil)
		     (if (math-zerop (cdr newt))
			 (setq x (car newt) x1 x)
		       (if (math-lessp (cdr newt) '(float 1 -6))
			   (let ((newt2 (math-poly-newton-root
					 p (car newt) 20)))
			     (if (math-zerop (cdr newt2))
				 (setq x (car newt2) x1 x)
			       (setq x (car newt))))))))
	       (not (or (eq x x1)
			(math-nearly-equal x x1))))
	     (let ((cdx (math-abs-approx dx)))
	       (setq x x1
		     tried-newt nil)
	       (prog1
		   (or (<= iters 6)
		       (math-lessp cdx dxold)
		       (progn
			 (if polish
			     (let ((digs (calcFunc-xpon
					  (math-div (math-abs-approx x) cdx))))
			       (calc-record-why
				"*Could not attain full precision")
			       (if (natnump digs)
				   (let ((calc-internal-prec (max 3 digs)))
				     (setq x (math-normalize x))))))
			 nil))
		 (setq dxold cdx)))
	     (or polish
		 (math-lessp (calcFunc-scf (math-abs-approx x)
					   (- calc-internal-prec))
			     dxold))))
    (or (and (math-floatp x)
	     (math-poly-integer-root x))
	x)))

(defun math-solve-above-dummy (x)
  (and (not (Math-primp x))
       (if (and (equal (nth 1 x) '(var SOLVEDUM SOLVEDUM))
		(= (length x) 2))
	   x
	 (let ((res nil))
	   (while (and (setq x (cdr x))
		       (not (setq res (math-solve-above-dummy (car x))))))
	   res))))

(defun math-solve-find-root-term (x neg)    ; sets "t2", "t3"
  (if (math-solve-find-root-in-prod x)
      (setq math-t3 neg
	    math-t1 x)
    (and (memq (car-safe x) '(+ -))
	 (or (math-solve-find-root-term (nth 1 x) neg)
	     (math-solve-find-root-term (nth 2 x)
					(if (eq (car x) '-) (not neg) neg))))))

(defun math-solve-find-root-in-prod (x)
  (and (consp x)
       (math-expr-contains x math-solve-var)
       (or (and (eq (car x) 'calcFunc-sqrt)
		(setq math-t2 2))
	   (and (eq (car x) '^)
		(or (and (memq (math-quarter-integer (nth 2 x)) '(1 2 3))
			 (setq math-t2 2))
		    (and (eq (car-safe (nth 2 x)) 'frac)
			 (eq (nth 2 (nth 2 x)) 3)
			 (setq math-t2 3))))
	   (and (memq (car x) '(* /))
		(or (and (not (math-expr-contains (nth 1 x) math-solve-var))
			 (math-solve-find-root-in-prod (nth 2 x)))
		    (and (not (math-expr-contains (nth 2 x) math-solve-var))
			 (math-solve-find-root-in-prod (nth 1 x))))))))

;; The variable math-solve-vars is local to math-solve-system,
;; but is used by math-solve-system-rec.
(defvar math-solve-vars)

;; The variable math-solve-simplifying is local to math-solve-system
;; and math-solve-system-rec, but is used by math-solve-system-subst.
(defvar math-solve-simplifying)

(defun math-solve-system (exprs math-solve-vars math-solve-full)
  (setq exprs (mapcar 'list (if (Math-vectorp exprs)
				(cdr exprs)
			      (list exprs)))
	math-solve-vars (if (Math-vectorp math-solve-vars)
		       (cdr math-solve-vars)
		     (list math-solve-vars)))
  (or (let ((math-solve-simplifying nil))
	(math-solve-system-rec exprs math-solve-vars nil))
      (let ((math-solve-simplifying t))
	(math-solve-system-rec exprs math-solve-vars nil))))

;;; The following backtracking solver works by choosing a variable
;;; and equation, and trying to solve the equation for the variable.
;;; If it succeeds it calls itself recursively with that variable and
;;; equation removed from their respective lists, and with the solution
;;; added to solns as well as being substituted into all existing
;;; equations.  The algorithm terminates when any solution path
;;; manages to remove all the variables from var-list.

;;; To support calcFunc-roots, entries in eqn-list and solns are
;;; actually lists of equations.

;; The variables math-solve-system-res and math-solve-system-vv are
;; local to math-solve-system-rec, but are used by math-solve-system-subst.
(defvar math-solve-system-vv)
(defvar math-solve-system-res)


(defun math-solve-system-rec (eqn-list var-list solns)
  (if var-list
      (let ((v var-list)
	    (math-solve-system-res nil))

	;; Try each variable in turn.
	(while
	    (and
	     v
	     (let* ((math-solve-system-vv (car v))
		    (e eqn-list)
		    (elim (eq (car-safe math-solve-system-vv) 'calcFunc-elim)))
	       (if elim
		   (setq math-solve-system-vv (nth 1 math-solve-system-vv)))

	       ;; Try each equation in turn.
	       (while
		   (and
		    e
		    (let ((e2 (car e))
			  (eprev nil)
			  res2)
		      (setq math-solve-system-res nil)

		      ;; Try to solve for math-solve-system-vv the list of equations e2.
		      (while (and e2
				  (setq res2 (or (and (eq (car e2) eprev)
						      res2)
						 (math-solve-for (car e2) 0
                                                                 math-solve-system-vv
								 math-solve-full))))
			(setq eprev (car e2)
			      math-solve-system-res (cons (if (eq math-solve-full 'all)
					    (cdr res2)
					  (list res2))
					math-solve-system-res)
			      e2 (cdr e2)))
		      (if e2
			  (setq math-solve-system-res nil)

			;; Found a solution.  Now try other variables.
			(setq math-solve-system-res (nreverse math-solve-system-res)
			      math-solve-system-res (math-solve-system-rec
				   (mapcar
				    'math-solve-system-subst
				    (delq (car e)
					  (copy-sequence eqn-list)))
				   (delq (car v) (copy-sequence var-list))
				   (let ((math-solve-simplifying nil)
					 (s (mapcar
					     (function
					      (lambda (x)
						(cons
						 (car x)
						 (math-solve-system-subst
						  (cdr x)))))
					     solns)))
				     (if elim
					 s
				       (cons (cons
                                              math-solve-system-vv
                                              (apply 'append math-solve-system-res))
					     s)))))
			(not math-solve-system-res))))
		 (setq e (cdr e)))
	       (not math-solve-system-res)))
	  (setq v (cdr v)))
	math-solve-system-res)

    ;; Eliminated all variables, so now put solution into the proper format.
    (setq solns (sort solns
		      (function
		       (lambda (x y)
			 (not (memq (car x) (memq (car y) math-solve-vars)))))))
    (if (eq math-solve-full 'all)
	(math-transpose
	 (math-normalize
	  (cons 'vec
		(if solns
		    (mapcar (function (lambda (x) (cons 'vec (cdr x)))) solns)
		  (mapcar (function (lambda (x) (cons 'vec x))) eqn-list)))))
      (math-normalize
       (cons 'vec
	     (if solns
		 (mapcar (function (lambda (x) (cons 'calcFunc-eq x))) solns)
	       (mapcar 'car eqn-list)))))))

(defun math-solve-system-subst (x)    ; uses "res" and "v"
  (let ((accum nil)
	(res2 math-solve-system-res))
    (while x
      (setq accum (nconc accum
			 (mapcar (function
				  (lambda (r)
				    (if math-solve-simplifying
					(math-simplify
					 (math-expr-subst
                                          (car x) math-solve-system-vv r))
				      (math-expr-subst
                                       (car x) math-solve-system-vv r))))
				 (car res2)))
	    x (cdr x)
	    res2 (cdr res2)))
    accum))


;; calc-command-flags is declared in calc.el
(defvar calc-command-flags)

(defun math-get-from-counter (name)
  (let ((ctr (assq name calc-command-flags)))
    (if ctr
	(setcdr ctr (1+ (cdr ctr)))
      (setq ctr (cons name 1)
	    calc-command-flags (cons ctr calc-command-flags)))
    (cdr ctr)))

(defvar var-GenCount)

(defun math-solve-get-sign (val)
  (setq val (math-simplify val))
  (if (and (eq (car-safe val) '*)
	   (Math-numberp (nth 1 val)))
      (list '* (nth 1 val) (math-solve-get-sign (nth 2 val)))
    (and (eq (car-safe val) 'calcFunc-sqrt)
	 (eq (car-safe (nth 1 val)) '^)
	 (setq val (math-normalize (list '^
					 (nth 1 (nth 1 val))
					 (math-div (nth 2 (nth 1 val)) 2)))))
    (if math-solve-full
	(if (and (calc-var-value 'var-GenCount)
		 (Math-natnump var-GenCount)
		 (not (eq math-solve-full 'all)))
	    (prog1
		(math-mul (list 'calcFunc-as var-GenCount) val)
	      (setq var-GenCount (math-add var-GenCount 1))
	      (calc-refresh-evaltos 'var-GenCount))
	  (let* ((var (concat "s" (int-to-string (math-get-from-counter 'solve-sign))))
		 (var2 (list 'var (intern var) (intern (concat "var-" var)))))
	    (if (eq math-solve-full 'all)
		(setq math-solve-ranges (cons (list var2 1 -1)
					      math-solve-ranges)))
	    (math-mul var2 val)))
      (calc-record-why "*Choosing positive solution")
      val)))

(defun math-solve-get-int (val &optional range first)
  (if math-solve-full
      (if (and (calc-var-value 'var-GenCount)
	       (Math-natnump var-GenCount)
	       (not (eq math-solve-full 'all)))
	  (prog1
	      (math-mul val (list 'calcFunc-an var-GenCount))
	    (setq var-GenCount (math-add var-GenCount 1))
	    (calc-refresh-evaltos 'var-GenCount))
	(let* ((var (concat "n" (int-to-string
				 (math-get-from-counter 'solve-int))))
	       (var2 (list 'var (intern var) (intern (concat "var-" var)))))
	  (if (and range (eq math-solve-full 'all))
	      (setq math-solve-ranges (cons (cons var2
						  (cdr (calcFunc-index
							range (or first 0))))
					    math-solve-ranges)))
	  (math-mul val var2)))
    (calc-record-why "*Choosing 0 for arbitrary integer in solution")
    0))

(defun math-solve-sign (sign expr)
  (and sign
       (let ((s1 (math-possible-signs expr)))
	 (cond ((memq s1 '(4 6))
		sign)
	       ((memq s1 '(1 3))
		(- sign))))))

(defun math-looks-evenp (expr)
  (if (Math-integerp expr)
      (math-evenp expr)
    (if (memq (car expr) '(* /))
	(math-looks-evenp (nth 1 expr)))))

(defun math-solve-for (lhs rhs math-solve-var math-solve-full &optional sign)
  (if (math-expr-contains rhs math-solve-var)
      (math-solve-for (math-sub lhs rhs) 0 math-solve-var math-solve-full)
    (and (math-expr-contains lhs math-solve-var)
	 (math-with-extra-prec 1
	   (let* ((math-poly-base-variable math-solve-var)
		  (res (math-try-solve-for lhs rhs sign)))
	     (if (and (eq math-solve-full 'all)
		      (math-known-realp math-solve-var))
		 (let ((old-len (length res))
		       new-len)
		   (setq res (delq nil
				   (mapcar (function
					    (lambda (x)
					      (and (not (memq (car-safe x)
							      '(cplx polar)))
						   x)))
					   res))
			 new-len (length res))
		   (if (< new-len old-len)
		       (calc-record-why (if (= new-len 1)
					    "*All solutions were complex"
					  (format
					   "*Omitted %d complex solutions"
					   (- old-len new-len)))))))
	     res)))))

(defun math-solve-eqn (expr var full)
  (if (memq (car-safe expr) '(calcFunc-neq calcFunc-lt calcFunc-gt
					   calcFunc-leq calcFunc-geq))
      (let ((res (math-solve-for (cons '- (cdr expr))
				 0 var full
				 (if (eq (car expr) 'calcFunc-neq) nil 1))))
	(and res
	     (if (eq math-solve-sign 1)
		 (list (car expr) var res)
	       (if (eq math-solve-sign -1)
		   (list (car expr) res var)
		 (or (eq (car expr) 'calcFunc-neq)
		     (calc-record-why
		      "*Can't determine direction of inequality"))
		 (and (memq (car expr) '(calcFunc-neq calcFunc-lt calcFunc-gt))
		      (list 'calcFunc-neq var res))))))
    (let ((res (math-solve-for expr 0 var full)))
      (and res
	   (list 'calcFunc-eq var res)))))

(defun math-reject-solution (expr var func)
  (if (math-expr-contains expr var)
      (or (equal (car calc-next-why) '(* "Unable to find a symbolic solution"))
	  (calc-record-why "*Unable to find a solution")))
  (list func expr var))

(defun calcFunc-solve (expr var)
  (or (if (or (Math-vectorp expr) (Math-vectorp var))
	  (math-solve-system expr var nil)
	(math-solve-eqn expr var nil))
      (math-reject-solution expr var 'calcFunc-solve)))

(defun calcFunc-fsolve (expr var)
  (or (if (or (Math-vectorp expr) (Math-vectorp var))
	  (math-solve-system expr var t)
	(math-solve-eqn expr var t))
      (math-reject-solution expr var 'calcFunc-fsolve)))

(defun calcFunc-roots (expr var)
  (let ((math-solve-ranges nil))
    (or (if (or (Math-vectorp expr) (Math-vectorp var))
	    (math-solve-system expr var 'all)
	  (math-solve-for expr 0 var 'all))
      (math-reject-solution expr var 'calcFunc-roots))))

(defun calcFunc-finv (expr var)
  (let ((res (math-solve-for expr math-integ-var var nil)))
    (if res
	(math-normalize (math-expr-subst res math-integ-var var))
      (math-reject-solution expr var 'calcFunc-finv))))

(defun calcFunc-ffinv (expr var)
  (let ((res (math-solve-for expr math-integ-var var t)))
    (if res
	(math-normalize (math-expr-subst res math-integ-var var))
      (math-reject-solution expr var 'calcFunc-finv))))


(put 'calcFunc-inv 'math-inverse
     (function (lambda (x) (math-div 1 x))))
(put 'calcFunc-inv 'math-inverse-sign -1)

(put 'calcFunc-sqrt 'math-inverse
     (function (lambda (x) (math-sqr x))))

(put 'calcFunc-conj 'math-inverse
     (function (lambda (x) (list 'calcFunc-conj x))))

(put 'calcFunc-abs 'math-inverse
     (function (lambda (x) (math-solve-get-sign x))))

(put 'calcFunc-deg 'math-inverse
     (function (lambda (x) (list 'calcFunc-rad x))))
(put 'calcFunc-deg 'math-inverse-sign 1)

(put 'calcFunc-rad 'math-inverse
     (function (lambda (x) (list 'calcFunc-deg x))))
(put 'calcFunc-rad 'math-inverse-sign 1)

(put 'calcFunc-ln 'math-inverse
     (function (lambda (x) (list 'calcFunc-exp x))))
(put 'calcFunc-ln 'math-inverse-sign 1)

(put 'calcFunc-log10 'math-inverse
     (function (lambda (x) (list 'calcFunc-exp10 x))))
(put 'calcFunc-log10 'math-inverse-sign 1)

(put 'calcFunc-lnp1 'math-inverse
     (function (lambda (x) (list 'calcFunc-expm1 x))))
(put 'calcFunc-lnp1 'math-inverse-sign 1)

(put 'calcFunc-exp 'math-inverse
     (function (lambda (x) (math-add (math-normalize (list 'calcFunc-ln x))
				     (math-mul 2
					       (math-mul '(var pi var-pi)
							 (math-solve-get-int
							  '(var i var-i))))))))
(put 'calcFunc-exp 'math-inverse-sign 1)

(put 'calcFunc-expm1 'math-inverse
     (function (lambda (x) (math-add (math-normalize (list 'calcFunc-lnp1 x))
				     (math-mul 2
					       (math-mul '(var pi var-pi)
							 (math-solve-get-int
							  '(var i var-i))))))))
(put 'calcFunc-expm1 'math-inverse-sign 1)

(put 'calcFunc-sin 'math-inverse
     (function (lambda (x) (let ((n (math-solve-get-int 1)))
			     (math-add (math-mul (math-normalize
						  (list 'calcFunc-arcsin x))
						 (math-pow -1 n))
				       (math-mul (math-half-circle t)
						 n))))))

(put 'calcFunc-cos 'math-inverse
     (function (lambda (x) (math-add (math-solve-get-sign
				      (math-normalize
				       (list 'calcFunc-arccos x)))
				     (math-solve-get-int
				      (math-full-circle t))))))

(put 'calcFunc-tan 'math-inverse
     (function (lambda (x) (math-add (math-normalize (list 'calcFunc-arctan x))
				     (math-solve-get-int
				      (math-half-circle t))))))

(put 'calcFunc-arcsin 'math-inverse
     (function (lambda (x) (math-normalize (list 'calcFunc-sin x)))))

(put 'calcFunc-arccos 'math-inverse
     (function (lambda (x) (math-normalize (list 'calcFunc-cos x)))))

(put 'calcFunc-arctan 'math-inverse
     (function (lambda (x) (math-normalize (list 'calcFunc-tan x)))))

(put 'calcFunc-sinh 'math-inverse
     (function (lambda (x) (let ((n (math-solve-get-int 1)))
			     (math-add (math-mul (math-normalize
						  (list 'calcFunc-arcsinh x))
						 (math-pow -1 n))
				       (math-mul (math-half-circle t)
						 (math-mul
						  '(var i var-i)
						  n)))))))
(put 'calcFunc-sinh 'math-inverse-sign 1)

(put 'calcFunc-cosh 'math-inverse
     (function (lambda (x) (math-add (math-solve-get-sign
				      (math-normalize
				       (list 'calcFunc-arccosh x)))
				     (math-mul (math-full-circle t)
					       (math-solve-get-int
						'(var i var-i)))))))

(put 'calcFunc-tanh 'math-inverse
     (function (lambda (x) (math-add (math-normalize
				      (list 'calcFunc-arctanh x))
				     (math-mul (math-half-circle t)
					       (math-solve-get-int
						'(var i var-i)))))))
(put 'calcFunc-tanh 'math-inverse-sign 1)

(put 'calcFunc-arcsinh 'math-inverse
     (function (lambda (x) (math-normalize (list 'calcFunc-sinh x)))))
(put 'calcFunc-arcsinh 'math-inverse-sign 1)

(put 'calcFunc-arccosh 'math-inverse
     (function (lambda (x) (math-normalize (list 'calcFunc-cosh x)))))

(put 'calcFunc-arctanh 'math-inverse
     (function (lambda (x) (math-normalize (list 'calcFunc-tanh x)))))
(put 'calcFunc-arctanh 'math-inverse-sign 1)



(defun calcFunc-taylor (expr var num)
  (let ((x0 0) (v var))
    (if (memq (car-safe var) '(+ - calcFunc-eq))
	(setq x0 (if (eq (car var) '+) (math-neg (nth 2 var)) (nth 2 var))
	      v (nth 1 var)))
    (or (and (eq (car-safe v) 'var)
	     (math-expr-contains expr v)
	     (natnump num)
	     (let ((accum (math-expr-subst expr v x0))
		   (var2 (if (eq (car var) 'calcFunc-eq)
			     (cons '- (cdr var))
			   var))
		   (n 0)
		   (nfac 1)
		   (fprime expr))
	       (while (and (<= (setq n (1+ n)) num)
			   (setq fprime (calcFunc-deriv fprime v nil t)))
		 (setq fprime (math-simplify fprime)
		       nfac (math-mul nfac n)
		       accum (math-add accum
				       (math-div (math-mul (math-pow var2 n)
							   (math-expr-subst
							    fprime v x0))
						 nfac))))
	       (and fprime
		    (math-normalize accum))))
	(list 'calcFunc-taylor expr var num))))

(provide 'calcalg2)

;;; calcalg2.el ends here
