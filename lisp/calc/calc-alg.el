;;; calc-alg.el --- algebraic functions for Calc

;; Copyright (C) 1990-1993, 2001-2012 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger  <jay.p.belanger@gmail.com>

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

;;; Algebra commands.

(defun calc-alg-evaluate (arg)
  (interactive "p")
  (calc-slow-wrapper
   (calc-with-default-simplification
    (let ((math-simplify-only nil))
      (calc-modify-simplify-mode arg)
      (calc-enter-result 1 "dsmp" (calc-top 1))))))

(defun calc-modify-simplify-mode (arg)
  (if (= (math-abs arg) 2)
      (setq calc-simplify-mode 'alg)
    (if (>= (math-abs arg) 3)
	(setq calc-simplify-mode 'ext)))
  (if (< arg 0)
      (setq calc-simplify-mode (list calc-simplify-mode))))

(defun calc-simplify ()
  (interactive)
  (calc-slow-wrapper
   (let ((top (calc-top-n 1)))
     (if (calc-is-inverse)
         (setq top
               (let ((calc-simplify-mode nil))
                 (math-normalize (math-trig-rewrite top)))))
     (if (calc-is-hyperbolic)
         (setq top
               (let ((calc-simplify-mode nil))
                 (math-normalize (math-hyperbolic-trig-rewrite top)))))
     (calc-with-default-simplification
      (calc-enter-result 1 "simp" (math-simplify top))))))

(defun calc-simplify-extended ()
  (interactive)
  (calc-slow-wrapper
   (calc-with-default-simplification
    (calc-enter-result 1 "esmp" (math-simplify-extended (calc-top-n 1))))))

(defun calc-expand-formula (arg)
  (interactive "p")
  (calc-slow-wrapper
   (calc-with-default-simplification
    (let ((math-simplify-only nil))
      (calc-modify-simplify-mode arg)
      (calc-enter-result 1 "expf"
			 (if (> arg 0)
			     (let ((math-expand-formulas t))
			       (calc-top-n 1))
			   (let ((top (calc-top-n 1)))
			     (or (math-expand-formula top)
				 top))))))))

(defun calc-factor (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "fctr" (if (calc-is-hyperbolic)
			     'calcFunc-factors 'calcFunc-factor)
		  arg)))

(defun calc-expand (n)
  (interactive "P")
  (calc-slow-wrapper
   (calc-enter-result 1 "expa"
		      (append (list 'calcFunc-expand
				    (calc-top-n 1))
			      (and n (list (prefix-numeric-value n)))))))

;;; Write out powers (a*b*...)^n as a*b*...*a*b*...
(defun calcFunc-powerexpand (expr)
  (math-normalize (math-map-tree 'math-powerexpand expr)))

(defun math-powerexpand (expr)
  (if (eq (car-safe expr) '^)
      (let ((n (nth 2 expr)))
        (cond ((and (integerp n)
                    (> n 0))
               (let ((i 1)
                     (a (nth 1 expr))
                     (prod (nth 1 expr)))
                 (while (< i n)
                   (setq prod (math-mul prod a))
                   (setq i (1+ i)))
                 prod))
              ((and (integerp n)
                    (< n 0))
               (let ((i -1)
                     (a (math-pow (nth 1 expr) -1))
                     (prod (math-pow (nth 1 expr) -1)))
                 (while (> i n)
                   (setq prod (math-mul a prod))
                   (setq i (1- i)))
                 prod))
              (t
               expr)))
    expr))

(defun calc-powerexpand ()
  (interactive)
  (calc-slow-wrapper
   (calc-enter-result 1 "pexp"
		      (calcFunc-powerexpand (calc-top-n 1)))))

(defun calc-collect (&optional var)
  (interactive "sCollect terms involving: ")
  (calc-slow-wrapper
   (if (or (equal var "") (equal var "$") (null var))
       (calc-enter-result 2 "clct" (cons 'calcFunc-collect
					 (calc-top-list-n 2)))
     (let ((var (math-read-expr var)))
       (if (eq (car-safe var) 'error)
	   (error "Bad format in expression: %s" (nth 1 var)))
       (calc-enter-result 1 "clct" (list 'calcFunc-collect
					 (calc-top-n 1)
					 var))))))

(defun calc-apart (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "aprt" 'calcFunc-apart arg)))

(defun calc-normalize-rat (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "nrat" 'calcFunc-nrat arg)))

(defun calc-poly-gcd (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "pgcd" 'calcFunc-pgcd arg)))


(defun calc-poly-div (arg)
  (interactive "P")
  (calc-slow-wrapper
   (let ((calc-poly-div-remainder nil))
     (calc-binary-op "pdiv" 'calcFunc-pdiv arg)
     (if (and calc-poly-div-remainder (null arg))
         (progn
           (calc-clear-command-flag 'clear-message)
           (calc-record calc-poly-div-remainder "prem")
           (if (not (Math-zerop calc-poly-div-remainder))
               (message "(Remainder was %s)"
                        (math-format-flat-expr calc-poly-div-remainder 0))
             (message "(No remainder)")))))))

(defun calc-poly-rem (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "prem" 'calcFunc-prem arg)))

(defun calc-poly-div-rem (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-binary-op "pdvr" 'calcFunc-pdivide arg)
     (calc-binary-op "pdvr" 'calcFunc-pdivrem arg))))

(defun calc-substitute (&optional oldname newname)
  (interactive "sSubstitute old: ")
  (calc-slow-wrapper
   (let (old new (num 1) expr)
     (if (or (equal oldname "") (equal oldname "$") (null oldname))
	 (setq new (calc-top-n 1)
	       old (calc-top-n 2)
	       expr (calc-top-n 3)
	       num 3)
       (or newname
	   (progn (calc-unread-command ?\C-a)
		  (setq newname (read-string (concat "Substitute old: "
						     oldname
						     ", new: ")
					     oldname))))
       (if (or (equal newname "") (equal newname "$") (null newname))
	   (setq new (calc-top-n 1)
		 expr (calc-top-n 2)
		 num 2)
	 (setq new (if (stringp newname) (math-read-expr newname) newname))
	 (if (eq (car-safe new) 'error)
	     (error "Bad format in expression: %s" (nth 1 new)))
	 (setq expr (calc-top-n 1)))
       (setq old (if (stringp oldname) (math-read-expr oldname) oldname))
       (if (eq (car-safe old) 'error)
	   (error "Bad format in expression: %s" (nth 1 old)))
       (or (math-expr-contains expr old)
	   (error "No occurrences found")))
     (calc-enter-result num "sbst" (math-expr-subst expr old new)))))


(defun calc-has-rules (name)
  (setq name (calc-var-value name))
  (and (consp name)
       (memq (car name) '(vec calcFunc-assign calcFunc-condition))
       name))

;; math-eval-rules-cache and math-eval-rules-cache-other are
;; declared in calc.el, but are used here by math-recompile-eval-rules.
(defvar math-eval-rules-cache)
(defvar math-eval-rules-cache-other)

(defun math-recompile-eval-rules ()
  (setq math-eval-rules-cache (and (calc-has-rules 'var-EvalRules)
				   (math-compile-rewrites
				    '(var EvalRules var-EvalRules)))
	math-eval-rules-cache-other (assq nil math-eval-rules-cache)
	math-eval-rules-cache-tag (calc-var-value 'var-EvalRules)))


;;; Try to expand a formula according to its definition.
(defun math-expand-formula (expr)
  (and (consp expr)
       (symbolp (car expr))
       (or (get (car expr) 'calc-user-defn)
	   (get (car expr) 'math-expandable))
       (let ((res (let ((math-expand-formulas t))
		    (apply (car expr) (cdr expr)))))
	 (and (not (eq (car-safe res) (car expr)))
	      res))))




;;; True if A comes before B in a canonical ordering of expressions.  [P X X]
(defun math-beforep (a b)   ; [Public]
  (cond ((and (Math-realp a) (Math-realp b))
	 (let ((comp (math-compare a b)))
	   (or (eq comp -1)
	       (and (eq comp 0)
		    (not (equal a b))
		    (> (length (memq (car-safe a)
				     '(bigneg nil bigpos frac float)))
		       (length (memq (car-safe b)
				     '(bigneg nil bigpos frac float))))))))
	((equal b '(neg (var inf var-inf))) nil)
	((equal a '(neg (var inf var-inf))) t)
	((equal a '(var inf var-inf)) nil)
	((equal b '(var inf var-inf)) t)
	((Math-realp a)
	 (if (and (eq (car-safe b) 'intv) (math-intv-constp b))
	     (if (or (math-beforep a (nth 2 b)) (Math-equal a (nth 2 b)))
		 t
	       nil)
	   t))
	((Math-realp b)
	 (if (and (eq (car-safe a) 'intv) (math-intv-constp a))
	     (if (math-beforep (nth 2 a) b)
		 t
	       nil)
	   nil))
	((and (eq (car a) 'intv) (eq (car b) 'intv)
	      (math-intv-constp a) (math-intv-constp b))
	 (let ((comp (math-compare (nth 2 a) (nth 2 b))))
	   (cond ((eq comp -1) t)
		 ((eq comp 1) nil)
		 ((and (memq (nth 1 a) '(2 3)) (memq (nth 1 b) '(0 1))) t)
		 ((and (memq (nth 1 a) '(0 1)) (memq (nth 1 b) '(2 3))) nil)
		 ((eq (setq comp (math-compare (nth 3 a) (nth 3 b))) -1) t)
		 ((eq comp 1) nil)
		 ((and (memq (nth 1 a) '(0 2)) (memq (nth 1 b) '(1 3))) t)
		 (t nil))))
	((not (eq (not (Math-objectp a)) (not (Math-objectp b))))
	 (Math-objectp a))
	((eq (car a) 'var)
	 (if (eq (car b) 'var)
	     (string-lessp (symbol-name (nth 1 a)) (symbol-name (nth 1 b)))
	   (not (Math-numberp b))))
	((eq (car b) 'var) (Math-numberp a))
	((eq (car a) (car b))
	 (while (and (setq a (cdr a) b (cdr b)) a
		     (equal (car a) (car b))))
	 (and b
	      (or (null a)
		  (math-beforep (car a) (car b)))))
	(t (string-lessp (symbol-name (car a)) (symbol-name (car b))))))


(defsubst math-simplify-extended (a)
  (let ((math-living-dangerously t))
    (math-simplify a)))

(defalias 'calcFunc-esimplify 'math-simplify-extended)

;;; Rewrite the trig functions in a form easier to simplify.
(defun math-trig-rewrite (fn)
  "Rewrite trigonometric functions in terms of sines and cosines."
  (cond
   ((not (consp fn))
    fn)
   ((eq (car-safe fn) 'calcFunc-sec)
    (list '/ 1 (cons 'calcFunc-cos (math-trig-rewrite (cdr fn)))))
   ((eq (car-safe fn) 'calcFunc-csc)
    (list '/ 1 (cons 'calcFunc-sin (math-trig-rewrite (cdr fn)))))
   ((eq (car-safe fn) 'calcFunc-tan)
    (let ((newfn (math-trig-rewrite (cdr fn))))
      (list '/ (cons 'calcFunc-sin newfn)
            (cons 'calcFunc-cos newfn))))
   ((eq (car-safe fn) 'calcFunc-cot)
    (let ((newfn (math-trig-rewrite (cdr fn))))
      (list '/ (cons 'calcFunc-cos newfn)
            (cons 'calcFunc-sin newfn))))
   (t
    (mapcar 'math-trig-rewrite fn))))

(defun math-hyperbolic-trig-rewrite (fn)
  "Rewrite hyperbolic functions in terms of sinhs and coshs."
  (cond
   ((not (consp fn))
    fn)
   ((eq (car-safe fn) 'calcFunc-sech)
    (list '/ 1 (cons 'calcFunc-cosh (math-hyperbolic-trig-rewrite (cdr fn)))))
   ((eq (car-safe fn) 'calcFunc-csch)
    (list '/ 1 (cons 'calcFunc-sinh (math-hyperbolic-trig-rewrite (cdr fn)))))
   ((eq (car-safe fn) 'calcFunc-tanh)
    (let ((newfn (math-hyperbolic-trig-rewrite (cdr fn))))
      (list '/ (cons 'calcFunc-sinh newfn)
            (cons 'calcFunc-cosh newfn))))
   ((eq (car-safe fn) 'calcFunc-coth)
    (let ((newfn (math-hyperbolic-trig-rewrite (cdr fn))))
      (list '/ (cons 'calcFunc-cosh newfn)
            (cons 'calcFunc-sinh newfn))))
   (t
    (mapcar 'math-hyperbolic-trig-rewrite fn))))

;; math-top-only is local to math-simplify, but is used by
;; math-simplify-step, which is called by math-simplify.
(defvar math-top-only)

(defun math-simplify (top-expr)
  (let ((math-simplifying t)
	(math-top-only (consp calc-simplify-mode))
	(simp-rules (append (and (calc-has-rules 'var-AlgSimpRules)
				 '((var AlgSimpRules var-AlgSimpRules)))
			    (and math-living-dangerously
				 (calc-has-rules 'var-ExtSimpRules)
				 '((var ExtSimpRules var-ExtSimpRules)))
			    (and math-simplifying-units
				 (calc-has-rules 'var-UnitSimpRules)
				 '((var UnitSimpRules var-UnitSimpRules)))
			    (and math-integrating
				 (calc-has-rules 'var-IntegSimpRules)
				 '((var IntegSimpRules var-IntegSimpRules)))))
	res)
    (if math-top-only
	(let ((r simp-rules))
	  (setq res (math-simplify-step (math-normalize top-expr))
		calc-simplify-mode '(nil)
		top-expr (math-normalize res))
	  (while r
	    (setq top-expr (math-rewrite top-expr (car r)
					 '(neg (var inf var-inf)))
		  r (cdr r))))
      (calc-with-default-simplification
       (while (let ((r simp-rules))
		(setq res (math-normalize top-expr))
		(while r
		  (setq res (math-rewrite res (car r))
			r (cdr r)))
		(not (equal top-expr (setq res (math-simplify-step res)))))
	 (setq top-expr res)))))
  top-expr)

(defalias 'calcFunc-simplify 'math-simplify)

;;; The following has a "bug" in that if any recursive simplifications
;;; occur only the first handler will be tried; this doesn't really
;;; matter, since math-simplify-step is iterated to a fixed point anyway.
(defun math-simplify-step (a)
  (if (Math-primp a)
      a
    (let ((aa (if (or math-top-only
		      (memq (car a) '(calcFunc-quote calcFunc-condition
						     calcFunc-evalto)))
		  a
		(cons (car a) (mapcar 'math-simplify-step (cdr a))))))
      (and (symbolp (car aa))
	   (let ((handler (get (car aa) 'math-simplify)))
	     (and handler
		  (while (and handler
			      (equal (setq aa (or (funcall (car handler) aa)
						  aa))
				     a))
		    (setq handler (cdr handler))))))
      aa)))


(defmacro math-defsimplify (funcs &rest code)
  (cons 'progn
        (mapcar #'(lambda (func)
                    `(put ',func 'math-simplify
                          (nconc
                           (get ',func 'math-simplify)
                           (list
                            #'(lambda (math-simplify-expr) ,@code)))))
                (if (symbolp funcs) (list funcs) funcs))))
(put 'math-defsimplify 'lisp-indent-hook 1)

;; The function created by math-defsimplify uses the variable
;; math-simplify-expr, and so is used by functions in math-defsimplify
(defvar math-simplify-expr)

(math-defsimplify (+ -)
  (math-simplify-plus))

(defun math-simplify-plus ()
  (cond ((and (memq (car-safe (nth 1 math-simplify-expr)) '(+ -))
	      (Math-numberp (nth 2 (nth 1 math-simplify-expr)))
	      (not (Math-numberp (nth 2 math-simplify-expr))))
	 (let ((x (nth 2 math-simplify-expr))
	       (op (car math-simplify-expr)))
	   (setcar (cdr (cdr math-simplify-expr)) (nth 2 (nth 1 math-simplify-expr)))
	   (setcar math-simplify-expr (car (nth 1 math-simplify-expr)))
	   (setcar (cdr (cdr (nth 1 math-simplify-expr))) x)
	   (setcar (nth 1 math-simplify-expr) op)))
	((and (eq (car math-simplify-expr) '+)
	      (Math-numberp (nth 1 math-simplify-expr))
	      (not (Math-numberp (nth 2 math-simplify-expr))))
	 (let ((x (nth 2 math-simplify-expr)))
	   (setcar (cdr (cdr math-simplify-expr)) (nth 1 math-simplify-expr))
	   (setcar (cdr math-simplify-expr) x))))
  (let ((aa math-simplify-expr)
	aaa temp)
    (while (memq (car-safe (setq aaa (nth 1 aa))) '(+ -))
      (if (setq temp (math-combine-sum (nth 2 aaa) (nth 2 math-simplify-expr)
				       (eq (car aaa) '-)
                                       (eq (car math-simplify-expr) '-) t))
	  (progn
	    (setcar (cdr (cdr math-simplify-expr)) temp)
	    (setcar math-simplify-expr '+)
	    (setcar (cdr (cdr aaa)) 0)))
      (setq aa (nth 1 aa)))
    (if (setq temp (math-combine-sum aaa (nth 2 math-simplify-expr)
				     nil (eq (car math-simplify-expr) '-) t))
	(progn
	  (setcar (cdr (cdr math-simplify-expr)) temp)
	  (setcar math-simplify-expr '+)
	  (setcar (cdr aa) 0)))
    math-simplify-expr))

(math-defsimplify *
  (math-simplify-times))

(defun math-simplify-times ()
  (if (eq (car-safe (nth 2 math-simplify-expr)) '*)
      (and (math-beforep (nth 1 (nth 2 math-simplify-expr)) (nth 1 math-simplify-expr))
	   (or (math-known-scalarp (nth 1 math-simplify-expr) t)
	       (math-known-scalarp (nth 1 (nth 2 math-simplify-expr)) t))
	   (let ((x (nth 1 math-simplify-expr)))
	     (setcar (cdr math-simplify-expr) (nth 1 (nth 2 math-simplify-expr)))
	     (setcar (cdr (nth 2 math-simplify-expr)) x)))
    (and (math-beforep (nth 2 math-simplify-expr) (nth 1 math-simplify-expr))
	 (or (math-known-scalarp (nth 1 math-simplify-expr) t)
	     (math-known-scalarp (nth 2 math-simplify-expr) t))
	 (let ((x (nth 2 math-simplify-expr)))
	   (setcar (cdr (cdr math-simplify-expr)) (nth 1 math-simplify-expr))
	   (setcar (cdr math-simplify-expr) x))))
  (let ((aa math-simplify-expr)
	aaa temp
	(safe t) (scalar (math-known-scalarp (nth 1 math-simplify-expr))))
    (if (and (Math-ratp (nth 1 math-simplify-expr))
	     (setq temp (math-common-constant-factor (nth 2 math-simplify-expr))))
	(progn
	  (setcar (cdr (cdr math-simplify-expr))
		  (math-cancel-common-factor (nth 2 math-simplify-expr) temp))
	  (setcar (cdr math-simplify-expr) (math-mul (nth 1 math-simplify-expr) temp))))
    (while (and (eq (car-safe (setq aaa (nth 2 aa))) '*)
		safe)
      (if (setq temp (math-combine-prod (nth 1 math-simplify-expr)
                                        (nth 1 aaa) nil nil t))
	  (progn
	    (setcar (cdr math-simplify-expr) temp)
	    (setcar (cdr aaa) 1)))
      (setq safe (or scalar (math-known-scalarp (nth 1 aaa) t))
	    aa (nth 2 aa)))
    (if (and (setq temp (math-combine-prod aaa (nth 1 math-simplify-expr) nil nil t))
	     safe)
	(progn
	  (setcar (cdr math-simplify-expr) temp)
	  (setcar (cdr (cdr aa)) 1)))
    (if (and (eq (car-safe (nth 1 math-simplify-expr)) 'frac)
	     (memq (nth 1 (nth 1 math-simplify-expr)) '(1 -1)))
	(math-div (math-mul (nth 2 math-simplify-expr)
                            (nth 1 (nth 1 math-simplify-expr)))
		  (nth 2 (nth 1 math-simplify-expr)))
      math-simplify-expr)))

(math-defsimplify /
  (math-simplify-divide))

(defun math-simplify-divide ()
  (let ((np (cdr math-simplify-expr))
	(nover nil)
	(nn (and (or (eq (car math-simplify-expr) '/)
                     (not (Math-realp (nth 2 math-simplify-expr))))
		 (math-common-constant-factor (nth 2 math-simplify-expr))))
	n op)
    (if nn
	(progn
	  (setq n (and (or (eq (car math-simplify-expr) '/)
                           (not (Math-realp (nth 1 math-simplify-expr))))
		       (math-common-constant-factor (nth 1 math-simplify-expr))))
	  (if (and (eq (car-safe nn) 'frac) (eq (nth 1 nn) 1) (not n))
	      (progn
		(setcar (cdr math-simplify-expr)
                        (math-mul (nth 2 nn) (nth 1 math-simplify-expr)))
		(setcar (cdr (cdr math-simplify-expr))
			(math-cancel-common-factor (nth 2 math-simplify-expr) nn))
		(if (and (math-negp nn)
			 (setq op (assq (car math-simplify-expr) calc-tweak-eqn-table)))
		    (setcar math-simplify-expr (nth 1 op))))
	    (if (and n (not (eq (setq n (math-frac-gcd n nn)) 1)))
		(progn
		  (setcar (cdr math-simplify-expr)
			  (math-cancel-common-factor (nth 1 math-simplify-expr) n))
		  (setcar (cdr (cdr math-simplify-expr))
			  (math-cancel-common-factor (nth 2 math-simplify-expr) n))
		  (if (and (math-negp n)
			   (setq op (assq (car math-simplify-expr)
                                          calc-tweak-eqn-table)))
		      (setcar math-simplify-expr (nth 1 op))))))))
    (if (and (eq (car-safe (car np)) '/)
	     (math-known-scalarp (nth 2 math-simplify-expr) t))
	(progn
	  (setq np (cdr (nth 1 math-simplify-expr)))
	  (while (eq (car-safe (setq n (car np))) '*)
	    (and (math-known-scalarp (nth 2 n) t)
		 (math-simplify-divisor (cdr n) (cdr (cdr math-simplify-expr)) nil t))
	    (setq np (cdr (cdr n))))
	  (math-simplify-divisor np (cdr (cdr math-simplify-expr)) nil t)
	  (setq nover t
		np (cdr (cdr (nth 1 math-simplify-expr))))))
    (while (eq (car-safe (setq n (car np))) '*)
      (and (math-known-scalarp (nth 2 n) t)
	   (math-simplify-divisor (cdr n) (cdr (cdr math-simplify-expr)) nover t))
      (setq np (cdr (cdr n))))
    (math-simplify-divisor np (cdr (cdr math-simplify-expr)) nover t)
    math-simplify-expr))

;; The variables math-simplify-divisor-nover and math-simplify-divisor-dover
;; are local variables for math-simplify-divisor, but are used by
;; math-simplify-one-divisor.
(defvar math-simplify-divisor-nover)
(defvar math-simplify-divisor-dover)

(defun math-simplify-divisor (np dp math-simplify-divisor-nover
                                 math-simplify-divisor-dover)
  (cond ((eq (car-safe (car dp)) '/)
	 (math-simplify-divisor np (cdr (car dp))
                                math-simplify-divisor-nover
                                math-simplify-divisor-dover)
	 (and (math-known-scalarp (nth 1 (car dp)) t)
	      (math-simplify-divisor np (cdr (cdr (car dp)))
				     math-simplify-divisor-nover
                                     (not math-simplify-divisor-dover))))
	((or (or (eq (car math-simplify-expr) '/)
		 (let ((signs (math-possible-signs (car np))))
		   (or (memq signs '(1 4))
		       (and (memq (car math-simplify-expr) '(calcFunc-eq calcFunc-neq))
			    (eq signs 5))
		       math-living-dangerously)))
	     (math-numberp (car np)))
	 (let (d
               (safe t)
               (scalar (math-known-scalarp (car np))))
	   (while (and (eq (car-safe (setq d (car dp))) '*)
		       safe)
	     (math-simplify-one-divisor np (cdr d))
	     (setq safe (or scalar (math-known-scalarp (nth 1 d) t))
		   dp (cdr (cdr d))))
	   (if safe
	       (math-simplify-one-divisor np dp))))))

(defun math-simplify-one-divisor (np dp)
  (let ((temp (math-combine-prod (car np) (car dp) math-simplify-divisor-nover
                                 math-simplify-divisor-dover t))
        op)
    (if temp
        (progn
          (and (not (memq (car math-simplify-expr) '(/ calcFunc-eq calcFunc-neq)))
               (math-known-negp (car dp))
               (setq op (assq (car math-simplify-expr) calc-tweak-eqn-table))
               (setcar math-simplify-expr (nth 1 op)))
          (setcar np (if math-simplify-divisor-nover (math-div 1 temp) temp))
          (setcar dp 1))
      (and math-simplify-divisor-dover (not math-simplify-divisor-nover)
           (eq (car math-simplify-expr) '/)
           (eq (car-safe (car dp)) 'calcFunc-sqrt)
           (Math-integerp (nth 1 (car dp)))
           (progn
             (setcar np (math-mul (car np)
                                  (list 'calcFunc-sqrt (nth 1 (car dp)))))
             (setcar dp (nth 1 (car dp))))))))

(defun math-common-constant-factor (expr)
  (if (Math-realp expr)
      (if (Math-ratp expr)
	  (and (not (memq expr '(0 1 -1)))
	       (math-abs expr))
	(if (math-ratp (setq expr (math-to-simple-fraction expr)))
	    (math-common-constant-factor expr)))
    (if (memq (car expr) '(+ - cplx sdev))
	(let ((f1 (math-common-constant-factor (nth 1 expr)))
	      (f2 (math-common-constant-factor (nth 2 expr))))
	  (and f1 f2
	       (not (eq (setq f1 (math-frac-gcd f1 f2)) 1))
	       f1))
      (if (memq (car expr) '(* polar))
	  (math-common-constant-factor (nth 1 expr))
	(if (eq (car expr) '/)
	    (or (math-common-constant-factor (nth 1 expr))
		(and (Math-integerp (nth 2 expr))
		     (list 'frac 1 (math-abs (nth 2 expr))))))))))

(defun math-cancel-common-factor (expr val)
  (if (memq (car-safe expr) '(+ - cplx sdev))
      (progn
	(setcar (cdr expr) (math-cancel-common-factor (nth 1 expr) val))
	(setcar (cdr (cdr expr)) (math-cancel-common-factor (nth 2 expr) val))
	expr)
    (if (eq (car-safe expr) '*)
	(math-mul (math-cancel-common-factor (nth 1 expr) val) (nth 2 expr))
      (math-div expr val))))

(defun math-frac-gcd (a b)
  (if (Math-zerop a)
      b
    (if (Math-zerop b)
	a
      (if (and (Math-integerp a)
	       (Math-integerp b))
	  (math-gcd a b)
	(and (Math-integerp a) (setq a (list 'frac a 1)))
	(and (Math-integerp b) (setq b (list 'frac b 1)))
	(math-make-frac (math-gcd (nth 1 a) (nth 1 b))
			(math-gcd (nth 2 a) (nth 2 b)))))))

(math-defsimplify %
  (math-simplify-mod))

(defun math-simplify-mod ()
  (and (Math-realp (nth 2 math-simplify-expr))
       (Math-posp (nth 2 math-simplify-expr))
       (let ((lin (math-is-linear (nth 1 math-simplify-expr)))
	     t1 t2 t3)
	 (or (and lin
		  (or (math-negp (car lin))
		      (not (Math-lessp (car lin) (nth 2 math-simplify-expr))))
		  (list '%
			(list '+
			      (math-mul (nth 1 lin) (nth 2 lin))
			      (math-mod (car lin) (nth 2 math-simplify-expr)))
			(nth 2 math-simplify-expr)))
	     (and lin
		  (not (math-equal-int (nth 1 lin) 1))
		  (math-num-integerp (nth 1 lin))
		  (math-num-integerp (nth 2 math-simplify-expr))
		  (setq t1 (calcFunc-gcd (nth 1 lin) (nth 2 math-simplify-expr)))
		  (not (math-equal-int t1 1))
		  (list '*
			t1
			(list '%
			      (list '+
				    (math-mul (math-div (nth 1 lin) t1)
					      (nth 2 lin))
				    (let ((calc-prefer-frac t))
				      (math-div (car lin) t1)))
			      (math-div (nth 2 math-simplify-expr) t1))))
	     (and (math-equal-int (nth 2 math-simplify-expr) 1)
		  (math-known-integerp (if lin
					   (math-mul (nth 1 lin) (nth 2 lin))
					 (nth 1 math-simplify-expr)))
		  (if lin (math-mod (car lin) 1) 0))))))

(math-defsimplify (calcFunc-eq calcFunc-neq calcFunc-lt
			       calcFunc-gt calcFunc-leq calcFunc-geq)
  (if (= (length math-simplify-expr) 3)
      (math-simplify-ineq)))

(defun math-simplify-ineq ()
  (let ((np (cdr math-simplify-expr))
	n)
    (while (memq (car-safe (setq n (car np))) '(+ -))
      (math-simplify-add-term (cdr (cdr n)) (cdr (cdr math-simplify-expr))
			      (eq (car n) '-) nil)
      (setq np (cdr n)))
    (math-simplify-add-term np (cdr (cdr math-simplify-expr)) nil
                            (eq np (cdr math-simplify-expr)))
    (math-simplify-divide)
    (let ((signs (math-possible-signs (cons '- (cdr math-simplify-expr)))))
      (or (cond ((eq (car math-simplify-expr) 'calcFunc-eq)
		 (or (and (eq signs 2) 1)
		     (and (memq signs '(1 4 5)) 0)))
		((eq (car math-simplify-expr) 'calcFunc-neq)
		 (or (and (eq signs 2) 0)
		     (and (memq signs '(1 4 5)) 1)))
		((eq (car math-simplify-expr) 'calcFunc-lt)
		 (or (and (eq signs 1) 1)
		     (and (memq signs '(2 4 6)) 0)))
		((eq (car math-simplify-expr) 'calcFunc-gt)
		 (or (and (eq signs 4) 1)
		     (and (memq signs '(1 2 3)) 0)))
		((eq (car math-simplify-expr) 'calcFunc-leq)
		 (or (and (eq signs 4) 0)
		     (and (memq signs '(1 2 3)) 1)))
		((eq (car math-simplify-expr) 'calcFunc-geq)
		 (or (and (eq signs 1) 0)
		     (and (memq signs '(2 4 6)) 1))))
	  math-simplify-expr))))

(defun math-simplify-add-term (np dp minus lplain)
  (or (math-vectorp (car np))
      (let ((rplain t)
	    n d dd temp)
	(while (memq (car-safe (setq n (car np) d (car dp))) '(+ -))
	  (setq rplain nil)
	  (if (setq temp (math-combine-sum n (nth 2 d)
					   minus (eq (car d) '+) t))
	      (if (or lplain (eq (math-looks-negp temp) minus))
		  (progn
		    (setcar np (setq n (if minus (math-neg temp) temp)))
		    (setcar (cdr (cdr d)) 0))
		(progn
		  (setcar np 0)
		  (setcar (cdr (cdr d)) (setq n (if (eq (car d) '+)
						    (math-neg temp)
						  temp))))))
	  (setq dp (cdr d)))
	(if (setq temp (math-combine-sum n d minus t t))
	    (if (or lplain
		    (and (not rplain)
			 (eq (math-looks-negp temp) minus)))
		(progn
		  (setcar np (setq n (if minus (math-neg temp) temp)))
		  (setcar dp 0))
	      (progn
		(setcar np 0)
		(setcar dp (setq n (math-neg temp)))))))))

(math-defsimplify calcFunc-sin
  (or (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsin)
	   (nth 1 (nth 1 math-simplify-expr)))
      (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-sin (math-neg (nth 1 math-simplify-expr)))))
      (and (eq calc-angle-mode 'rad)
	   (let ((n (math-linear-in (nth 1 math-simplify-expr) '(var pi var-pi))))
	     (and n
		  (math-known-sin (car n) (nth 1 n) 120 0))))
      (and (eq calc-angle-mode 'deg)
	   (let ((n (math-integer-plus (nth 1 math-simplify-expr))))
	     (and n
		  (math-known-sin (car n) (nth 1 n) '(frac 2 3) 0))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccos)
	   (list 'calcFunc-sqrt (math-sub 1 (math-sqr
                                             (nth 1 (nth 1 math-simplify-expr))))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctan)
	   (math-div (nth 1 (nth 1 math-simplify-expr))
		     (list 'calcFunc-sqrt
			   (math-add 1 (math-sqr
                                        (nth 1 (nth 1 math-simplify-expr)))))))
      (let ((m (math-should-expand-trig (nth 1 math-simplify-expr))))
	(and m (integerp (car m))
	     (let ((n (car m)) (a (nth 1 m)))
	       (list '+
		     (list '* (list 'calcFunc-sin (list '* (1- n) a))
			   (list 'calcFunc-cos a))
		     (list '* (list 'calcFunc-cos (list '* (1- n) a))
			   (list 'calcFunc-sin a))))))))

(math-defsimplify calcFunc-cos
  (or (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccos)
	   (nth 1 (nth 1 math-simplify-expr)))
      (and (math-looks-negp (nth 1 math-simplify-expr))
	   (list 'calcFunc-cos (math-neg (nth 1 math-simplify-expr))))
      (and (eq calc-angle-mode 'rad)
	   (let ((n (math-linear-in (nth 1 math-simplify-expr) '(var pi var-pi))))
	     (and n
		  (math-known-sin (car n) (nth 1 n) 120 300))))
      (and (eq calc-angle-mode 'deg)
	   (let ((n (math-integer-plus (nth 1 math-simplify-expr))))
	     (and n
		  (math-known-sin (car n) (nth 1 n) '(frac 2 3) 300))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsin)
	   (list 'calcFunc-sqrt
                 (math-sub 1 (math-sqr (nth 1 (nth 1 math-simplify-expr))))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctan)
	   (math-div 1
		     (list 'calcFunc-sqrt
			   (math-add 1
                                     (math-sqr (nth 1 (nth 1 math-simplify-expr)))))))
      (let ((m (math-should-expand-trig (nth 1 math-simplify-expr))))
	(and m (integerp (car m))
	     (let ((n (car m)) (a (nth 1 m)))
	       (list '-
		     (list '* (list 'calcFunc-cos (list '* (1- n) a))
			   (list 'calcFunc-cos a))
		     (list '* (list 'calcFunc-sin (list '* (1- n) a))
			   (list 'calcFunc-sin a))))))))

(math-defsimplify calcFunc-sec
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (list 'calcFunc-sec (math-neg (nth 1 math-simplify-expr))))
      (and (eq calc-angle-mode 'rad)
	   (let ((n (math-linear-in (nth 1 math-simplify-expr) '(var pi var-pi))))
	     (and n
		  (math-div 1 (math-known-sin (car n) (nth 1 n) 120 300)))))
      (and (eq calc-angle-mode 'deg)
	   (let ((n (math-integer-plus (nth 1 math-simplify-expr))))
	     (and n
                  (math-div 1 (math-known-sin (car n) (nth 1 n) '(frac 2 3) 300)))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsin)
           (math-div
            1
            (list 'calcFunc-sqrt
                  (math-sub 1 (math-sqr (nth 1 (nth 1 math-simplify-expr)))))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccos)
           (math-div
            1
            (nth 1 (nth 1 math-simplify-expr))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctan)
           (list 'calcFunc-sqrt
                 (math-add 1
                           (math-sqr (nth 1 (nth 1 math-simplify-expr))))))))

(math-defsimplify calcFunc-csc
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-csc (math-neg (nth 1 math-simplify-expr)))))
      (and (eq calc-angle-mode 'rad)
	   (let ((n (math-linear-in (nth 1 math-simplify-expr) '(var pi var-pi))))
	     (and n
                  (math-div 1 (math-known-sin (car n) (nth 1 n) 120 0)))))
      (and (eq calc-angle-mode 'deg)
	   (let ((n (math-integer-plus (nth 1 math-simplify-expr))))
	     (and n
                  (math-div 1 (math-known-sin (car n) (nth 1 n) '(frac 2 3) 0)))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsin)
	   (math-div 1 (nth 1 (nth 1 math-simplify-expr))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccos)
           (math-div
            1
            (list 'calcFunc-sqrt (math-sub 1 (math-sqr
                                              (nth 1 (nth 1 math-simplify-expr)))))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctan)
	   (math-div (list 'calcFunc-sqrt
			   (math-add 1 (math-sqr
                                        (nth 1 (nth 1 math-simplify-expr)))))
                     (nth 1 (nth 1 math-simplify-expr))))))

(defun math-should-expand-trig (x &optional hyperbolic)
  (let ((m (math-is-multiple x)))
    (and math-living-dangerously
	 m (or (and (integerp (car m)) (> (car m) 1))
	       (equal (car m) '(frac 1 2)))
	 (or math-integrating
	     (memq (car-safe (nth 1 m))
		   (if hyperbolic
		       '(calcFunc-arcsinh calcFunc-arccosh calcFunc-arctanh)
		     '(calcFunc-arcsin calcFunc-arccos calcFunc-arctan)))
	     (and (eq (car-safe (nth 1 m)) 'calcFunc-ln)
		  (eq hyperbolic 'exp)))
	 m)))

(defun math-known-sin (plus n mul off)
  (setq n (math-mul n mul))
  (and (math-num-integerp n)
       (setq n (math-mod (math-add (math-trunc n) off) 240))
       (if (>= n 120)
	   (and (setq n (math-known-sin plus (- n 120) 1 0))
		(math-neg n))
	 (if (> n 60)
	     (setq n (- 120 n)))
	 (if (math-zerop plus)
	     (and (or calc-symbolic-mode
		      (memq n '(0 20 60)))
		  (cdr (assq n
			     '( (0 . 0)
				(10 . (/ (calcFunc-sqrt
					  (- 2 (calcFunc-sqrt 3))) 2))
				(12 . (/ (- (calcFunc-sqrt 5) 1) 4))
				(15 . (/ (calcFunc-sqrt
					  (- 2 (calcFunc-sqrt 2))) 2))
				(20 . (/ 1 2))
				(24 . (* (^ (/ 1 2) (/ 3 2))
					 (calcFunc-sqrt
					  (- 5 (calcFunc-sqrt 5)))))
				(30 . (/ (calcFunc-sqrt 2) 2))
				(36 . (/ (+ (calcFunc-sqrt 5) 1) 4))
				(40 . (/ (calcFunc-sqrt 3) 2))
				(45 . (/ (calcFunc-sqrt
					  (+ 2 (calcFunc-sqrt 2))) 2))
				(48 . (* (^ (/ 1 2) (/ 3 2))
					 (calcFunc-sqrt
					  (+ 5 (calcFunc-sqrt 5)))))
				(50 . (/ (calcFunc-sqrt
					  (+ 2 (calcFunc-sqrt 3))) 2))
				(60 . 1)))))
	   (cond ((eq n 0) (math-normalize (list 'calcFunc-sin plus)))
		 ((eq n 60) (math-normalize (list 'calcFunc-cos plus)))
		 (t nil))))))

(math-defsimplify calcFunc-tan
  (or (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctan)
	   (nth 1 (nth 1 math-simplify-expr)))
      (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-tan (math-neg (nth 1 math-simplify-expr)))))
      (and (eq calc-angle-mode 'rad)
	   (let ((n (math-linear-in (nth 1 math-simplify-expr) '(var pi var-pi))))
	     (and n
		  (math-known-tan (car n) (nth 1 n) 120))))
      (and (eq calc-angle-mode 'deg)
	   (let ((n (math-integer-plus (nth 1 math-simplify-expr))))
	     (and n
		  (math-known-tan (car n) (nth 1 n) '(frac 2 3)))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsin)
	   (math-div (nth 1 (nth 1 math-simplify-expr))
		     (list 'calcFunc-sqrt
			   (math-sub 1 (math-sqr (nth 1 (nth 1 math-simplify-expr)))))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccos)
	   (math-div (list 'calcFunc-sqrt
			   (math-sub 1 (math-sqr (nth 1 (nth 1 math-simplify-expr)))))
		     (nth 1 (nth 1 math-simplify-expr))))
      (let ((m (math-should-expand-trig (nth 1 math-simplify-expr))))
	(and m
	     (if (equal (car m) '(frac 1 2))
		 (math-div (math-sub 1 (list 'calcFunc-cos (nth 1 m)))
			   (list 'calcFunc-sin (nth 1 m)))
	       (math-div (list 'calcFunc-sin (nth 1 math-simplify-expr))
			 (list 'calcFunc-cos (nth 1 math-simplify-expr))))))))

(math-defsimplify calcFunc-cot
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-cot (math-neg (nth 1 math-simplify-expr)))))
      (and (eq calc-angle-mode 'rad)
	   (let ((n (math-linear-in (nth 1 math-simplify-expr) '(var pi var-pi))))
	     (and n
                  (math-div 1 (math-known-tan (car n) (nth 1 n) 120)))))
      (and (eq calc-angle-mode 'deg)
	   (let ((n (math-integer-plus (nth 1 math-simplify-expr))))
	     (and n
                  (math-div 1 (math-known-tan (car n) (nth 1 n) '(frac 2 3))))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsin)
	   (math-div (list 'calcFunc-sqrt
			   (math-sub 1 (math-sqr (nth 1 (nth 1 math-simplify-expr)))))
                     (nth 1 (nth 1 math-simplify-expr))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccos)
	   (math-div (nth 1 (nth 1 math-simplify-expr))
                     (list 'calcFunc-sqrt
			   (math-sub 1 (math-sqr (nth 1 (nth 1 math-simplify-expr)))))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctan)
	   (math-div 1 (nth 1 (nth 1 math-simplify-expr))))))

(defun math-known-tan (plus n mul)
  (setq n (math-mul n mul))
  (and (math-num-integerp n)
       (setq n (math-mod (math-trunc n) 120))
       (if (> n 60)
	   (and (setq n (math-known-tan plus (- 120 n) 1))
		(math-neg n))
	 (if (math-zerop plus)
	     (and (or calc-symbolic-mode
		      (memq n '(0 30 60)))
		  (cdr (assq n '( (0 . 0)
				  (10 . (- 2 (calcFunc-sqrt 3)))
				  (12 . (calcFunc-sqrt
					 (- 1 (* (/ 2 5) (calcFunc-sqrt 5)))))
				  (15 . (- (calcFunc-sqrt 2) 1))
				  (20 . (/ (calcFunc-sqrt 3) 3))
				  (24 . (calcFunc-sqrt
					 (- 5 (* 2 (calcFunc-sqrt 5)))))
				  (30 . 1)
				  (36 . (calcFunc-sqrt
					 (+ 1 (* (/ 2 5) (calcFunc-sqrt 5)))))
				  (40 . (calcFunc-sqrt 3))
				  (45 . (+ (calcFunc-sqrt 2) 1))
				  (48 . (calcFunc-sqrt
					 (+ 5 (* 2 (calcFunc-sqrt 5)))))
				  (50 . (+ 2 (calcFunc-sqrt 3)))
				  (60 . (var uinf var-uinf))))))
	   (cond ((eq n 0) (math-normalize (list 'calcFunc-tan plus)))
		 ((eq n 60) (math-normalize (list '/ -1
						  (list 'calcFunc-tan plus))))
		 (t nil))))))

(math-defsimplify calcFunc-sinh
  (or (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsinh)
	   (nth 1 (nth 1 math-simplify-expr)))
      (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-sinh (math-neg (nth 1 math-simplify-expr)))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccosh)
	   math-living-dangerously
	   (list 'calcFunc-sqrt
                 (math-sub (math-sqr (nth 1 (nth 1 math-simplify-expr))) 1)))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctanh)
	   math-living-dangerously
	   (math-div (nth 1 (nth 1 math-simplify-expr))
		     (list 'calcFunc-sqrt
			   (math-sub 1 (math-sqr (nth 1 (nth 1 math-simplify-expr)))))))
      (let ((m (math-should-expand-trig (nth 1 math-simplify-expr) t)))
	(and m (integerp (car m))
	     (let ((n (car m)) (a (nth 1 m)))
	       (if (> n 1)
		   (list '+
			 (list '* (list 'calcFunc-sinh (list '* (1- n) a))
			       (list 'calcFunc-cosh a))
			 (list '* (list 'calcFunc-cosh (list '* (1- n) a))
			       (list 'calcFunc-sinh a)))))))))

(math-defsimplify calcFunc-cosh
  (or (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccosh)
	   (nth 1 (nth 1 math-simplify-expr)))
      (and (math-looks-negp (nth 1 math-simplify-expr))
	   (list 'calcFunc-cosh (math-neg (nth 1 math-simplify-expr))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsinh)
	   math-living-dangerously
	   (list 'calcFunc-sqrt
                 (math-add (math-sqr (nth 1 (nth 1 math-simplify-expr))) 1)))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctanh)
	   math-living-dangerously
	   (math-div 1
		     (list 'calcFunc-sqrt
			   (math-sub 1 (math-sqr (nth 1 (nth 1 math-simplify-expr)))))))
      (let ((m (math-should-expand-trig (nth 1 math-simplify-expr) t)))
	(and m (integerp (car m))
	     (let ((n (car m)) (a (nth 1 m)))
	       (if (> n 1)
		   (list '+
			 (list '* (list 'calcFunc-cosh (list '* (1- n) a))
			       (list 'calcFunc-cosh a))
			 (list '* (list 'calcFunc-sinh (list '* (1- n) a))
			       (list 'calcFunc-sinh a)))))))))

(math-defsimplify calcFunc-tanh
  (or (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctanh)
	   (nth 1 (nth 1 math-simplify-expr)))
      (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-tanh (math-neg (nth 1 math-simplify-expr)))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsinh)
	   math-living-dangerously
	   (math-div (nth 1 (nth 1 math-simplify-expr))
		     (list 'calcFunc-sqrt
			   (math-add (math-sqr (nth 1 (nth 1 math-simplify-expr))) 1))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccosh)
	   math-living-dangerously
	   (math-div (list 'calcFunc-sqrt
			   (math-sub (math-sqr (nth 1 (nth 1 math-simplify-expr))) 1))
		     (nth 1 (nth 1 math-simplify-expr))))
      (let ((m (math-should-expand-trig (nth 1 math-simplify-expr) t)))
	(and m
	     (if (equal (car m) '(frac 1 2))
		 (math-div (math-sub (list 'calcFunc-cosh (nth 1 m)) 1)
			   (list 'calcFunc-sinh (nth 1 m)))
	       (math-div (list 'calcFunc-sinh (nth 1 math-simplify-expr))
			 (list 'calcFunc-cosh (nth 1 math-simplify-expr))))))))

(math-defsimplify calcFunc-sech
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (list 'calcFunc-sech (math-neg (nth 1 math-simplify-expr))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsinh)
	   math-living-dangerously
           (math-div
            1
            (list 'calcFunc-sqrt
                  (math-add (math-sqr (nth 1 (nth 1 math-simplify-expr))) 1))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccosh)
	   math-living-dangerously
           (math-div 1 (nth 1 (nth 1 math-simplify-expr))) 1)
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctanh)
	   math-living-dangerously
           (list 'calcFunc-sqrt
                 (math-sub 1 (math-sqr (nth 1 (nth 1 math-simplify-expr))))))))

(math-defsimplify calcFunc-csch
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-csch (math-neg (nth 1 math-simplify-expr)))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsinh)
	   math-living-dangerously
           (math-div 1 (nth 1 (nth 1 math-simplify-expr))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccosh)
	   math-living-dangerously
           (math-div
            1
            (list 'calcFunc-sqrt
                  (math-sub (math-sqr (nth 1 (nth 1 math-simplify-expr))) 1))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctanh)
	   math-living-dangerously
	   (math-div (list 'calcFunc-sqrt
			   (math-sub 1 (math-sqr (nth 1 (nth 1 math-simplify-expr)))))
                     (nth 1 (nth 1 math-simplify-expr))))))

(math-defsimplify calcFunc-coth
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-coth (math-neg (nth 1 math-simplify-expr)))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arcsinh)
	   math-living-dangerously
	   (math-div (list 'calcFunc-sqrt
			   (math-add (math-sqr (nth 1 (nth 1 math-simplify-expr))) 1))
                     (nth 1 (nth 1 math-simplify-expr))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arccosh)
	   math-living-dangerously
	   (math-div (nth 1 (nth 1 math-simplify-expr))
                     (list 'calcFunc-sqrt
			   (math-sub (math-sqr (nth 1 (nth 1 math-simplify-expr))) 1))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-arctanh)
	   math-living-dangerously
	   (math-div 1 (nth 1 (nth 1 math-simplify-expr))))))

(math-defsimplify calcFunc-arcsin
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-arcsin (math-neg (nth 1 math-simplify-expr)))))
      (and (eq (nth 1 math-simplify-expr) 1)
	   (math-quarter-circle t))
      (and (equal (nth 1 math-simplify-expr) '(frac 1 2))
	   (math-div (math-half-circle t) 6))
      (and math-living-dangerously
	   (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-sin)
	   (nth 1 (nth 1 math-simplify-expr)))
      (and math-living-dangerously
	   (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-cos)
	   (math-sub (math-quarter-circle t)
		     (nth 1 (nth 1 math-simplify-expr))))))

(math-defsimplify calcFunc-arccos
  (or (and (eq (nth 1 math-simplify-expr) 0)
	   (math-quarter-circle t))
      (and (eq (nth 1 math-simplify-expr) -1)
	   (math-half-circle t))
      (and (equal (nth 1 math-simplify-expr) '(frac 1 2))
	   (math-div (math-half-circle t) 3))
      (and (equal (nth 1 math-simplify-expr) '(frac -1 2))
	   (math-div (math-mul (math-half-circle t) 2) 3))
      (and math-living-dangerously
	   (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-cos)
	   (nth 1 (nth 1 math-simplify-expr)))
      (and math-living-dangerously
	   (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-sin)
	   (math-sub (math-quarter-circle t)
		     (nth 1 (nth 1 math-simplify-expr))))))

(math-defsimplify calcFunc-arctan
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-arctan (math-neg (nth 1 math-simplify-expr)))))
      (and (eq (nth 1 math-simplify-expr) 1)
	   (math-div (math-half-circle t) 4))
      (and math-living-dangerously
	   (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-tan)
	   (nth 1 (nth 1 math-simplify-expr)))))

(math-defsimplify calcFunc-arcsinh
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-arcsinh (math-neg (nth 1 math-simplify-expr)))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-sinh)
	   (or math-living-dangerously
	       (math-known-realp (nth 1 (nth 1 math-simplify-expr))))
	   (nth 1 (nth 1 math-simplify-expr)))))

(math-defsimplify calcFunc-arccosh
  (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-cosh)
       (or math-living-dangerously
	   (math-known-realp (nth 1 (nth 1 math-simplify-expr))))
       (nth 1 (nth 1 math-simplify-expr))))

(math-defsimplify calcFunc-arctanh
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-arctanh (math-neg (nth 1 math-simplify-expr)))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-tanh)
	   (or math-living-dangerously
	       (math-known-realp (nth 1 (nth 1 math-simplify-expr))))
	   (nth 1 (nth 1 math-simplify-expr)))))

(math-defsimplify calcFunc-sqrt
  (math-simplify-sqrt))

(defun math-simplify-sqrt ()
  (or (and (eq (car-safe (nth 1 math-simplify-expr)) 'frac)
	   (math-div (list 'calcFunc-sqrt
                           (math-mul (nth 1 (nth 1 math-simplify-expr))
                                     (nth 2 (nth 1 math-simplify-expr))))
		     (nth 2 (nth 1 math-simplify-expr))))
      (let ((fac (if (math-objectp (nth 1 math-simplify-expr))
		     (math-squared-factor (nth 1 math-simplify-expr))
		   (math-common-constant-factor (nth 1 math-simplify-expr)))))
	(and fac (not (eq fac 1))
	     (math-mul (math-normalize (list 'calcFunc-sqrt fac))
		       (math-normalize
			(list 'calcFunc-sqrt
			      (math-cancel-common-factor
                               (nth 1 math-simplify-expr) fac))))))
      (and math-living-dangerously
	   (or (and (eq (car-safe (nth 1 math-simplify-expr)) '-)
		    (math-equal-int (nth 1 (nth 1 math-simplify-expr)) 1)
		    (eq (car-safe (nth 2 (nth 1 math-simplify-expr))) '^)
		    (math-equal-int (nth 2 (nth 2 (nth 1 math-simplify-expr))) 2)
		    (or (and (eq (car-safe (nth 1 (nth 2 (nth 1 math-simplify-expr))))
				 'calcFunc-sin)
			     (list 'calcFunc-cos
				   (nth 1 (nth 1 (nth 2 (nth 1 math-simplify-expr))))))
			(and (eq (car-safe (nth 1 (nth 2 (nth 1 math-simplify-expr))))
				 'calcFunc-cos)
			     (list 'calcFunc-sin
				   (nth 1 (nth 1 (nth 2
                                                      (nth 1 math-simplify-expr))))))))
	       (and (eq (car-safe (nth 1 math-simplify-expr)) '-)
		    (math-equal-int (nth 2 (nth 1 math-simplify-expr)) 1)
		    (eq (car-safe (nth 1 (nth 1 math-simplify-expr))) '^)
		    (math-equal-int (nth 2 (nth 1 (nth 1 math-simplify-expr))) 2)
		    (and (eq (car-safe (nth 1 (nth 1 (nth 1 math-simplify-expr))))
			     'calcFunc-cosh)
			 (list 'calcFunc-sinh
			       (nth 1 (nth 1 (nth 1 (nth 1 math-simplify-expr)))))))
	       (and (eq (car-safe (nth 1 math-simplify-expr)) '+)
		    (let ((a (nth 1 (nth 1 math-simplify-expr)))
			  (b (nth 2 (nth 1 math-simplify-expr))))
		      (and (or (and (math-equal-int a 1)
				    (setq a b b (nth 1 (nth 1 math-simplify-expr))))
			       (math-equal-int b 1))
			   (eq (car-safe a) '^)
			   (math-equal-int (nth 2 a) 2)
			   (or (and (eq (car-safe (nth 1 a)) 'calcFunc-sinh)
				    (list 'calcFunc-cosh (nth 1 (nth 1 a))))
                               (and (eq (car-safe (nth 1 a)) 'calcFunc-csch)
				    (list 'calcFunc-coth (nth 1 (nth 1 a))))
			       (and (eq (car-safe (nth 1 a)) 'calcFunc-tan)
				    (list '/ 1 (list 'calcFunc-cos
						     (nth 1 (nth 1 a)))))
			       (and (eq (car-safe (nth 1 a)) 'calcFunc-cot)
				    (list '/ 1 (list 'calcFunc-sin
						     (nth 1 (nth 1 a)))))))))
	       (and (eq (car-safe (nth 1 math-simplify-expr)) '^)
		    (list '^
			  (nth 1 (nth 1 math-simplify-expr))
			  (math-div (nth 2 (nth 1 math-simplify-expr)) 2)))
	       (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-sqrt)
		    (list '^ (nth 1 (nth 1 math-simplify-expr)) (math-div 1 4)))
	       (and (memq (car-safe (nth 1 math-simplify-expr)) '(* /))
		    (list (car (nth 1 math-simplify-expr))
			  (list 'calcFunc-sqrt (nth 1 (nth 1 math-simplify-expr)))
			  (list 'calcFunc-sqrt (nth 2 (nth 1 math-simplify-expr)))))
	       (and (memq (car-safe (nth 1 math-simplify-expr)) '(+ -))
		    (not (math-any-floats (nth 1 math-simplify-expr)))
		    (let ((f (calcFunc-factors (calcFunc-expand
						(nth 1 math-simplify-expr)))))
		      (and (math-vectorp f)
			   (or (> (length f) 2)
			       (> (nth 2 (nth 1 f)) 1))
			   (let ((out 1) (rest 1) (sums 1) fac pow)
			     (while (setq f (cdr f))
			       (setq fac (nth 1 (car f))
				     pow (nth 2 (car f)))
			       (if (> pow 1)
				   (setq out (math-mul out (math-pow
							    fac (/ pow 2)))
					 pow (% pow 2)))
			       (if (> pow 0)
				   (if (memq (car-safe fac) '(+ -))
				       (setq sums (math-mul-thru sums fac))
				     (setq rest (math-mul rest fac)))))
			     (and (not (and (eq out 1) (memq rest '(1 -1))))
				  (math-mul
				   out
				   (list 'calcFunc-sqrt
					 (math-mul sums rest))))))))))))

;;; Rather than factoring x into primes, just check for the first ten primes.
(defun math-squared-factor (x)
  (if (Math-integerp x)
      (let ((prsqr '(4 9 25 49 121 169 289 361 529 841))
	    (fac 1)
	    res)
	(while prsqr
	  (if (eq (cdr (setq res (math-idivmod x (car prsqr)))) 0)
	      (setq x (car res)
		    fac (math-mul fac (car prsqr)))
	    (setq prsqr (cdr prsqr))))
	fac)))

(math-defsimplify calcFunc-exp
  (math-simplify-exp (nth 1 math-simplify-expr)))

(defun math-simplify-exp (x)
  (or (and (eq (car-safe x) 'calcFunc-ln)
	   (nth 1 x))
      (and math-living-dangerously
	   (or (and (eq (car-safe x) 'calcFunc-arcsinh)
		    (math-add (nth 1 x)
			      (list 'calcFunc-sqrt
				    (math-add (math-sqr (nth 1 x)) 1))))
	       (and (eq (car-safe x) 'calcFunc-arccosh)
		    (math-add (nth 1 x)
			      (list 'calcFunc-sqrt
				    (math-sub (math-sqr (nth 1 x)) 1))))
	       (and (eq (car-safe x) 'calcFunc-arctanh)
		    (math-div (list 'calcFunc-sqrt (math-add 1 (nth 1 x)))
			      (list 'calcFunc-sqrt (math-sub 1 (nth 1 x)))))
	       (let ((m (math-should-expand-trig x 'exp)))
		 (and m (integerp (car m))
		      (list '^ (list 'calcFunc-exp (nth 1 m)) (car m))))))
      (and calc-symbolic-mode
	   (math-known-imagp x)
	   (let* ((ip (calcFunc-im x))
		  (n (math-linear-in ip '(var pi var-pi)))
		  s c)
	     (and n
		  (setq s (math-known-sin (car n) (nth 1 n) 120 0))
		  (setq c (math-known-sin (car n) (nth 1 n) 120 300))
		  (list '+ c (list '* s '(var i var-i))))))))

(math-defsimplify calcFunc-ln
  (or (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-exp)
	   (or math-living-dangerously
	       (math-known-realp (nth 1 (nth 1 math-simplify-expr))))
	   (nth 1 (nth 1 math-simplify-expr)))
      (and (eq (car-safe (nth 1 math-simplify-expr)) '^)
	   (equal (nth 1 (nth 1 math-simplify-expr)) '(var e var-e))
	   (or math-living-dangerously
	       (math-known-realp (nth 2 (nth 1 math-simplify-expr))))
	   (nth 2 (nth 1 math-simplify-expr)))
      (and calc-symbolic-mode
	   (math-known-negp (nth 1 math-simplify-expr))
	   (math-add (list 'calcFunc-ln (math-neg (nth 1 math-simplify-expr)))
		     '(* (var pi var-pi) (var i var-i))))
      (and calc-symbolic-mode
	   (math-known-imagp (nth 1 math-simplify-expr))
	   (let* ((ip (calcFunc-im (nth 1 math-simplify-expr)))
		  (ips (math-possible-signs ip)))
	     (or (and (memq ips '(4 6))
		      (math-add (list 'calcFunc-ln ip)
				'(/ (* (var pi var-pi) (var i var-i)) 2)))
		 (and (memq ips '(1 3))
		      (math-sub (list 'calcFunc-ln (math-neg ip))
				'(/ (* (var pi var-pi) (var i var-i)) 2))))))))

(math-defsimplify ^
  (math-simplify-pow))

(defun math-simplify-pow ()
  (or (and math-living-dangerously
	   (or (and (eq (car-safe (nth 1 math-simplify-expr)) '^)
		    (list '^
			  (nth 1 (nth 1 math-simplify-expr))
			  (math-mul (nth 2 math-simplify-expr)
                                    (nth 2 (nth 1 math-simplify-expr)))))
	       (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-sqrt)
		    (list '^
			  (nth 1 (nth 1 math-simplify-expr))
			  (math-div (nth 2 math-simplify-expr) 2)))
	       (and (memq (car-safe (nth 1 math-simplify-expr)) '(* /))
		    (list (car (nth 1 math-simplify-expr))
			  (list '^ (nth 1 (nth 1 math-simplify-expr))
                                (nth 2 math-simplify-expr))
			  (list '^ (nth 2 (nth 1 math-simplify-expr))
                                (nth 2 math-simplify-expr))))))
      (and (math-equal-int (nth 1 math-simplify-expr) 10)
	   (eq (car-safe (nth 2 math-simplify-expr)) 'calcFunc-log10)
	   (nth 1 (nth 2 math-simplify-expr)))
      (and (equal (nth 1 math-simplify-expr) '(var e var-e))
	   (math-simplify-exp (nth 2 math-simplify-expr)))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-exp)
	   (not math-integrating)
	   (list 'calcFunc-exp (math-mul (nth 1 (nth 1 math-simplify-expr))
                                         (nth 2 math-simplify-expr))))
      (and (equal (nth 1 math-simplify-expr) '(var i var-i))
	   (math-imaginary-i)
	   (math-num-integerp (nth 2 math-simplify-expr))
	   (let ((x (math-mod (math-trunc (nth 2 math-simplify-expr)) 4)))
	     (cond ((eq x 0) 1)
		   ((eq x 1) (nth 1 math-simplify-expr))
		   ((eq x 2) -1)
		   ((eq x 3) (math-neg (nth 1 math-simplify-expr))))))
      (and math-integrating
	   (integerp (nth 2 math-simplify-expr))
	   (>= (nth 2 math-simplify-expr) 2)
	   (or (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-cos)
		    (math-mul (math-pow (nth 1 math-simplify-expr)
                                        (- (nth 2 math-simplify-expr) 2))
			      (math-sub 1
					(math-sqr
					 (list 'calcFunc-sin
					       (nth 1 (nth 1 math-simplify-expr)))))))
	       (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-cosh)
		    (math-mul (math-pow (nth 1 math-simplify-expr)
                                        (- (nth 2 math-simplify-expr) 2))
			      (math-add 1
					(math-sqr
					 (list 'calcFunc-sinh
					       (nth 1 (nth 1 math-simplify-expr)))))))))
      (and (eq (car-safe (nth 2 math-simplify-expr)) 'frac)
	   (Math-ratp (nth 1 math-simplify-expr))
	   (Math-posp (nth 1 math-simplify-expr))
	   (if (equal (nth 2 math-simplify-expr) '(frac 1 2))
	       (list 'calcFunc-sqrt (nth 1 math-simplify-expr))
	     (let ((flr (math-floor (nth 2 math-simplify-expr))))
	       (and (not (Math-zerop flr))
		    (list '* (list '^ (nth 1 math-simplify-expr) flr)
			  (list '^ (nth 1 math-simplify-expr)
				(math-sub (nth 2 math-simplify-expr) flr)))))))
      (and (eq (math-quarter-integer (nth 2 math-simplify-expr)) 2)
	   (let ((temp (math-simplify-sqrt)))
	     (and temp
		  (list '^ temp (math-mul (nth 2 math-simplify-expr) 2)))))))

(math-defsimplify calcFunc-log10
  (and (eq (car-safe (nth 1 math-simplify-expr)) '^)
       (math-equal-int (nth 1 (nth 1 math-simplify-expr)) 10)
       (or math-living-dangerously
	   (math-known-realp (nth 2 (nth 1 math-simplify-expr))))
       (nth 2 (nth 1 math-simplify-expr))))


(math-defsimplify calcFunc-erf
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-neg (list 'calcFunc-erf (math-neg (nth 1 math-simplify-expr)))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-conj)
	   (list 'calcFunc-conj
                 (list 'calcFunc-erf (nth 1 (nth 1 math-simplify-expr)))))))

(math-defsimplify calcFunc-erfc
  (or (and (math-looks-negp (nth 1 math-simplify-expr))
	   (math-sub 2 (list 'calcFunc-erfc (math-neg (nth 1 math-simplify-expr)))))
      (and (eq (car-safe (nth 1 math-simplify-expr)) 'calcFunc-conj)
	   (list 'calcFunc-conj
                 (list 'calcFunc-erfc (nth 1 (nth 1 math-simplify-expr)))))))


(defun math-linear-in (expr term &optional always)
  (if (math-expr-contains expr term)
      (let* ((calc-prefer-frac t)
	     (p (math-is-polynomial expr term 1)))
	(and (cdr p)
	     p))
    (and always (list expr 0))))

(defun math-multiple-of (expr term)
  (let ((p (math-linear-in expr term)))
    (and p
	 (math-zerop (car p))
	 (nth 1 p))))

; not perfect, but it'll do
(defun math-integer-plus (expr)
  (cond ((Math-integerp expr)
	 (list 0 expr))
	((and (memq (car expr) '(+ -))
	      (Math-integerp (nth 1 expr)))
	 (list (if (eq (car expr) '+) (nth 2 expr) (math-neg (nth 2 expr)))
	       (nth 1 expr)))
	((and (memq (car expr) '(+ -))
	      (Math-integerp (nth 2 expr)))
	 (list (nth 1 expr)
	       (if (eq (car expr) '+) (nth 2 expr) (math-neg (nth 2 expr)))))
	(t nil)))

(defun math-is-linear (expr &optional always)
  (let ((offset nil)
	(coef nil))
    (if (eq (car-safe expr) '+)
	(if (Math-objectp (nth 1 expr))
	    (setq offset (nth 1 expr)
		  expr (nth 2 expr))
	  (if (Math-objectp (nth 2 expr))
	      (setq offset (nth 2 expr)
		    expr (nth 1 expr))))
      (if (eq (car-safe expr) '-)
	  (if (Math-objectp (nth 1 expr))
	      (setq offset (nth 1 expr)
		    expr (math-neg (nth 2 expr)))
	    (if (Math-objectp (nth 2 expr))
		(setq offset (math-neg (nth 2 expr))
		      expr (nth 1 expr))))))
    (setq coef (math-is-multiple expr always))
    (if offset
	(list offset (or (car coef) 1) (or (nth 1 coef) expr))
      (if coef
	  (cons 0 coef)))))

(defun math-is-multiple (expr &optional always)
  (or (if (eq (car-safe expr) '*)
	  (if (Math-objectp (nth 1 expr))
	      (list (nth 1 expr) (nth 2 expr)))
	(if (eq (car-safe expr) '/)
	    (if (and (Math-objectp (nth 1 expr))
		     (not (math-equal-int (nth 1 expr) 1)))
		(list (nth 1 expr) (math-div 1 (nth 2 expr)))
	      (if (Math-objectp (nth 2 expr))
		  (list (math-div 1 (nth 2 expr)) (nth 1 expr))
		(let ((res (math-is-multiple (nth 1 expr))))
		  (if res
		      (list (car res)
			    (math-div (nth 2 (nth 1 expr)) (nth 2 expr)))
		    (setq res (math-is-multiple (nth 2 expr)))
		    (if res
			(list (math-div 1 (car res))
			      (math-div (nth 1 expr)
					(nth 2 (nth 2 expr)))))))))
	  (if (eq (car-safe expr) 'neg)
	      (list -1 (nth 1 expr)))))
      (if (Math-objvecp expr)
	  (and (eq always 1)
	       (list expr 1))
	(and always
	     (list 1 expr)))))

(defun calcFunc-lin (expr &optional var)
  (if var
      (let ((res (math-linear-in expr var t)))
	(or res (math-reject-arg expr "Linear term expected"))
	(list 'vec (car res) (nth 1 res) var))
    (let ((res (math-is-linear expr t)))
      (or res (math-reject-arg expr "Linear term expected"))
      (cons 'vec res))))

(defun calcFunc-linnt (expr &optional var)
  (if var
      (let ((res (math-linear-in expr var)))
	(or res (math-reject-arg expr "Linear term expected"))
	(list 'vec (car res) (nth 1 res) var))
    (let ((res (math-is-linear expr)))
      (or res (math-reject-arg expr "Linear term expected"))
      (cons 'vec res))))

(defun calcFunc-islin (expr &optional var)
  (if (and (Math-objvecp expr) (not var))
      0
    (calcFunc-lin expr var)
    1))

(defun calcFunc-islinnt (expr &optional var)
  (if (Math-objvecp expr)
      0
    (calcFunc-linnt expr var)
    1))




;;; Simple operations on expressions.

;;; Return number of occurrences of thing in expr, or nil if none.
(defun math-expr-contains-count (expr thing)
  (cond ((equal expr thing) 1)
	((Math-primp expr) nil)
	(t
	 (let ((num 0))
	   (while (setq expr (cdr expr))
	     (setq num (+ num (or (math-expr-contains-count
				   (car expr) thing) 0))))
	   (and (> num 0)
		num)))))

(defun math-expr-contains (expr thing)
  (cond ((equal expr thing) 1)
	((Math-primp expr) nil)
	(t
	 (while (and (setq expr (cdr expr))
		     (not (math-expr-contains (car expr) thing))))
	 expr)))

;;; Return non-nil if any variable of thing occurs in expr.
(defun math-expr-depends (expr thing)
  (if (Math-primp thing)
      (and (eq (car-safe thing) 'var)
	   (math-expr-contains expr thing))
    (while (and (setq thing (cdr thing))
		(not (math-expr-depends expr (car thing)))))
    thing))

;;; Substitute all occurrences of old for new in expr (non-destructive).

;; The variables math-expr-subst-old and math-expr-subst-new are local
;; for math-expr-subst, but used by math-expr-subst-rec.
(defvar math-expr-subst-old)
(defvar math-expr-subst-new)

(defun math-expr-subst (expr math-expr-subst-old math-expr-subst-new)
  (math-expr-subst-rec expr))

(defalias 'calcFunc-subst 'math-expr-subst)

(defun math-expr-subst-rec (expr)
  (cond ((equal expr math-expr-subst-old) math-expr-subst-new)
	((Math-primp expr) expr)
	((memq (car expr) '(calcFunc-deriv
			    calcFunc-tderiv))
	 (if (= (length expr) 2)
	     (if (equal (nth 1 expr) math-expr-subst-old)
		 (append expr (list math-expr-subst-new))
	       expr)
	   (list (car expr) (nth 1 expr)
		 (math-expr-subst-rec (nth 2 expr)))))
	(t
	 (cons (car expr)
	       (mapcar 'math-expr-subst-rec (cdr expr))))))

;;; Various measures of the size of an expression.
(defun math-expr-weight (expr)
  (if (Math-primp expr)
      1
    (let ((w 1))
      (while (setq expr (cdr expr))
	(setq w (+ w (math-expr-weight (car expr)))))
      w)))

(defun math-expr-height (expr)
  (if (Math-primp expr)
      0
    (let ((h 0))
      (while (setq expr (cdr expr))
	(setq h (max h (math-expr-height (car expr)))))
      (1+ h))))




;;; Polynomial operations (to support the integrator and solve-for).

(defun calcFunc-collect (expr base)
  (let ((p (math-is-polynomial expr base 50 t)))
    (if (cdr p)
        (math-build-polynomial-expr (mapcar 'math-normalize p) base)
      (car p))))

;;; If expr is of the form "a + bx + cx^2 + ...", return the list (a b c ...),
;;; else return nil if not in polynomial form.  If "loose" (math-is-poly-loose),
;;; coefficients may contain x, e.g., sin(x) + cos(x) x^2 is a loose polynomial in x.

;; These variables are local to math-is-polynomial, but are used by
;; math-is-poly-rec.
(defvar math-is-poly-degree)
(defvar math-is-poly-loose)
(defvar math-var)

(defun math-is-polynomial (expr math-var &optional math-is-poly-degree math-is-poly-loose)
  (let* ((math-poly-base-variable (if math-is-poly-loose
				      (if (eq math-is-poly-loose 'gen) math-var '(var XXX XXX))
				    math-poly-base-variable))
	 (poly (math-is-poly-rec expr math-poly-neg-powers)))
    (and (or (null math-is-poly-degree)
	     (<= (length poly) (1+ math-is-poly-degree)))
	 poly)))

(defun math-is-poly-rec (expr negpow)
  (math-poly-simplify
   (or (cond ((or (equal expr math-var)
		  (eq (car-safe expr) '^))
	      (let ((pow 1)
		    (expr expr))
		(or (equal expr math-var)
		    (setq pow (nth 2 expr)
			  expr (nth 1 expr)))
		(or (eq math-poly-mult-powers 1)
		    (setq pow (let ((m (math-is-multiple pow 1)))
				(and (eq (car-safe (car m)) 'cplx)
				     (Math-zerop (nth 1 (car m)))
				     (setq m (list (nth 2 (car m))
						   (math-mul (nth 1 m)
							     '(var i var-i)))))
				(and (if math-poly-mult-powers
					 (equal math-poly-mult-powers
						(nth 1 m))
				       (setq math-poly-mult-powers (nth 1 m)))
				     (or (equal expr math-var)
					 (eq math-poly-mult-powers 1))
				     (car m)))))
		(if (consp pow)
		    (progn
		      (setq pow (math-to-simple-fraction pow))
		      (and (eq (car-safe pow) 'frac)
			   math-poly-frac-powers
			   (equal expr math-var)
			   (setq math-poly-frac-powers
				 (calcFunc-lcm math-poly-frac-powers
					       (nth 2 pow))))))
		(or (memq math-poly-frac-powers '(1 nil))
		    (setq pow (math-mul pow math-poly-frac-powers)))
		(if (integerp pow)
		    (if (and (= pow 1)
			     (equal expr math-var))
			(list 0 1)
		      (if (natnump pow)
			  (let ((p1 (if (equal expr math-var)
					(list 0 1)
				      (math-is-poly-rec expr nil)))
				(n pow)
				(accum (list 1)))
			    (and p1
				 (or (null math-is-poly-degree)
				     (<= (* (1- (length p1)) n) math-is-poly-degree))
				 (progn
				   (while (>= n 1)
				     (setq accum (math-poly-mul accum p1)
					   n (1- n)))
				   accum)))
			(and negpow
			     (math-is-poly-rec expr nil)
			     (setq math-poly-neg-powers
				   (cons (math-pow expr (- pow))
					 math-poly-neg-powers))
			     (list (list '^ expr pow))))))))
	     ((Math-objectp expr)
	      (list expr))
	     ((memq (car expr) '(+ -))
	      (let ((p1 (math-is-poly-rec (nth 1 expr) negpow)))
		(and p1
		     (let ((p2 (math-is-poly-rec (nth 2 expr) negpow)))
		       (and p2
			    (math-poly-mix p1 1 p2
					   (if (eq (car expr) '+) 1 -1)))))))
	     ((eq (car expr) 'neg)
	      (mapcar 'math-neg (math-is-poly-rec (nth 1 expr) negpow)))
	     ((eq (car expr) '*)
	      (let ((p1 (math-is-poly-rec (nth 1 expr) negpow)))
		(and p1
		     (let ((p2 (math-is-poly-rec (nth 2 expr) negpow)))
		       (and p2
			    (or (null math-is-poly-degree)
				(<= (- (+ (length p1) (length p2)) 2)
                                    math-is-poly-degree))
			    (math-poly-mul p1 p2))))))
	     ((eq (car expr) '/)
	      (and (or (not (math-poly-depends (nth 2 expr) math-var))
		       (and negpow
			    (math-is-poly-rec (nth 2 expr) nil)
			    (setq math-poly-neg-powers
				  (cons (nth 2 expr) math-poly-neg-powers))))
		   (not (Math-zerop (nth 2 expr)))
		   (let ((p1 (math-is-poly-rec (nth 1 expr) negpow)))
		     (mapcar (function (lambda (x) (math-div x (nth 2 expr))))
			     p1))))
	     ((and (eq (car expr) 'calcFunc-exp)
		   (equal math-var '(var e var-e)))
	      (math-is-poly-rec (list '^ math-var (nth 1 expr)) negpow))
	     ((and (eq (car expr) 'calcFunc-sqrt)
		   math-poly-frac-powers)
	      (math-is-poly-rec (list '^ (nth 1 expr) '(frac 1 2)) negpow))
	     (t nil))
       (and (or (not (math-poly-depends expr math-var))
		math-is-poly-loose)
	    (not (eq (car expr) 'vec))
	    (list expr)))))

;;; Check if expr is a polynomial in var; if so, return its degree.
(defun math-polynomial-p (expr var)
  (cond ((equal expr var) 1)
	((Math-primp expr) 0)
	((memq (car expr) '(+ -))
	 (let ((p1 (math-polynomial-p (nth 1 expr) var))
	       p2)
	   (and p1 (setq p2 (math-polynomial-p (nth 2 expr) var))
		(max p1 p2))))
	((eq (car expr) '*)
	 (let ((p1 (math-polynomial-p (nth 1 expr) var))
	       p2)
	   (and p1 (setq p2 (math-polynomial-p (nth 2 expr) var))
		(+ p1 p2))))
	((eq (car expr) 'neg)
	 (math-polynomial-p (nth 1 expr) var))
	((and (eq (car expr) '/)
	      (not (math-poly-depends (nth 2 expr) var)))
	 (math-polynomial-p (nth 1 expr) var))
	((and (eq (car expr) '^)
	      (natnump (nth 2 expr)))
	 (let ((p1 (math-polynomial-p (nth 1 expr) var)))
	   (and p1 (* p1 (nth 2 expr)))))
	((math-poly-depends expr var) nil)
	(t 0)))

(defun math-poly-depends (expr var)
  (if math-poly-base-variable
      (math-expr-contains expr math-poly-base-variable)
    (math-expr-depends expr var)))

;;; Find the variable (or sub-expression) which is the base of polynomial expr.
;; The variables math-poly-base-const-ok and math-poly-base-pred are
;; local to math-polynomial-base, but are used by math-polynomial-base-rec.
(defvar math-poly-base-const-ok)
(defvar math-poly-base-pred)

;; The variable math-poly-base-top-expr is local to math-polynomial-base,
;; but is used by math-polynomial-p1 in calc-poly.el, which is called
;; by math-polynomial-base.

(defun math-polynomial-base (math-poly-base-top-expr &optional math-poly-base-pred)
  (or math-poly-base-pred
      (setq math-poly-base-pred (function (lambda (base) (math-polynomial-p
					       math-poly-base-top-expr base)))))
  (or (let ((math-poly-base-const-ok nil))
	(math-polynomial-base-rec math-poly-base-top-expr))
      (let ((math-poly-base-const-ok t))
	(math-polynomial-base-rec math-poly-base-top-expr))))

(defun math-polynomial-base-rec (mpb-expr)
  (and (not (Math-objvecp mpb-expr))
       (or (and (memq (car mpb-expr) '(+ - *))
		(or (math-polynomial-base-rec (nth 1 mpb-expr))
		    (math-polynomial-base-rec (nth 2 mpb-expr))))
	   (and (memq (car mpb-expr) '(/ neg))
		(math-polynomial-base-rec (nth 1 mpb-expr)))
	   (and (eq (car mpb-expr) '^)
		(math-polynomial-base-rec (nth 1 mpb-expr)))
	   (and (eq (car mpb-expr) 'calcFunc-exp)
		(math-polynomial-base-rec '(var e var-e)))
	   (and (or math-poly-base-const-ok (math-expr-contains-vars mpb-expr))
		(funcall math-poly-base-pred mpb-expr)
		mpb-expr))))

;;; Return non-nil if expr refers to any variables.
(defun math-expr-contains-vars (expr)
  (or (eq (car-safe expr) 'var)
      (and (not (Math-primp expr))
	   (progn
	     (while (and (setq expr (cdr expr))
			 (not (math-expr-contains-vars (car expr)))))
	     expr))))

;;; Simplify a polynomial in list form by stripping off high-end zeros.
;;; This always leaves the constant part, i.e., nil->nil and non-nil->non-nil.
(defun math-poly-simplify (p)
  (and p
       (if (Math-zerop (nth (1- (length p)) p))
	   (let ((pp (copy-sequence p)))
	     (while (and (cdr pp)
			 (Math-zerop (nth (1- (length pp)) pp)))
	       (setcdr (nthcdr (- (length pp) 2) pp) nil))
	     pp)
	 p)))

;;; Compute ac*a + bc*b for polynomials in list form a, b and
;;; coefficients ac, bc.  Result may be unsimplified.
(defun math-poly-mix (a ac b bc)
  (and (or a b)
       (cons (math-add (math-mul (or (car a) 0) ac)
		       (math-mul (or (car b) 0) bc))
	     (math-poly-mix (cdr a) ac (cdr b) bc))))

(defun math-poly-zerop (a)
  (or (null a)
      (and (null (cdr a)) (Math-zerop (car a)))))

;;; Multiply two polynomials in list form.
(defun math-poly-mul (a b)
  (and a b
       (math-poly-mix b (car a)
		      (math-poly-mul (cdr a) (cons 0 b)) 1)))

;;; Build an expression from a polynomial list.
(defun math-build-polynomial-expr (p var)
  (if p
      (if (Math-numberp var)
	  (math-with-extra-prec 1
	    (let* ((rp (reverse p))
		   (accum (car rp)))
	      (while (setq rp (cdr rp))
		(setq accum (math-add (car rp) (math-mul accum var))))
	      accum))
	(let* ((rp (reverse p))
	       (n (1- (length rp)))
	       (accum (math-mul (car rp) (math-pow var n)))
	       term)
	  (while (setq rp (cdr rp))
	    (setq n (1- n))
	    (or (math-zerop (car rp))
		(setq accum (list (if (math-looks-negp (car rp)) '- '+)
				  accum
				  (math-mul (if (math-looks-negp (car rp))
						(math-neg (car rp))
					      (car rp))
					    (math-pow var n))))))
	  accum))
    0))


(defun math-to-simple-fraction (f)
  (or (and (eq (car-safe f) 'float)
	   (or (and (>= (nth 2 f) 0)
		    (math-scale-int (nth 1 f) (nth 2 f)))
	       (and (integerp (nth 1 f))
		    (> (nth 1 f) -1000)
		    (< (nth 1 f) 1000)
		    (math-make-frac (nth 1 f)
				    (math-scale-int 1 (- (nth 2 f)))))))
      f))

(provide 'calc-alg)

;;; calc-alg.el ends here
