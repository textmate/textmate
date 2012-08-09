;;; calc-funcs.el --- well-known functions for Calc

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

(defun calc-inc-gamma (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (if (calc-is-hyperbolic)
	   (calc-binary-op "gamG" 'calcFunc-gammaG arg)
	 (calc-binary-op "gamQ" 'calcFunc-gammaQ arg))
       (if (calc-is-hyperbolic)
	   (calc-binary-op "gamg" 'calcFunc-gammag arg)
	 (calc-binary-op "gamP" 'calcFunc-gammaP arg)))))

(defun calc-erf (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-unary-op "erfc" 'calcFunc-erfc arg)
     (calc-unary-op "erf" 'calcFunc-erf arg))))

(defun calc-erfc (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-erf arg))

(defun calc-beta (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "beta" 'calcFunc-beta arg)))

(defun calc-inc-beta ()
  (interactive)
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-enter-result 3 "betB" (cons 'calcFunc-betaB (calc-top-list-n 3)))
     (calc-enter-result 3 "betI" (cons 'calcFunc-betaI (calc-top-list-n 3))))))

(defun calc-bessel-J (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "besJ" 'calcFunc-besJ arg)))

(defun calc-bessel-Y (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "besY" 'calcFunc-besY arg)))

(defun calc-bernoulli-number (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-binary-op "bern" 'calcFunc-bern arg)
     (calc-unary-op "bern" 'calcFunc-bern arg))))

(defun calc-euler-number (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-binary-op "eulr" 'calcFunc-euler arg)
     (calc-unary-op "eulr" 'calcFunc-euler arg))))

(defun calc-stirling-number (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-binary-op "str2" 'calcFunc-stir2 arg)
     (calc-binary-op "str1" 'calcFunc-stir1 arg))))

(defun calc-utpb ()
  (interactive)
  (calc-prob-dist "b" 3))

(defun calc-utpc ()
  (interactive)
  (calc-prob-dist "c" 2))

(defun calc-utpf ()
  (interactive)
  (calc-prob-dist "f" 3))

(defun calc-utpn ()
  (interactive)
  (calc-prob-dist "n" 3))

(defun calc-utpp ()
  (interactive)
  (calc-prob-dist "p" 2))

(defun calc-utpt ()
  (interactive)
  (calc-prob-dist "t" 2))

(defun calc-prob-dist (letter nargs)
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-enter-result nargs (concat "ltp" letter)
			  (append (list (intern (concat "calcFunc-ltp" letter))
					(calc-top-n 1))
				  (calc-top-list-n (1- nargs) 2)))
     (calc-enter-result nargs (concat "utp" letter)
			(append (list (intern (concat "calcFunc-utp" letter))
				      (calc-top-n 1))
				(calc-top-list-n (1- nargs) 2))))))




;;; Sources:  Numerical Recipes, Press et al;
;;;           Handbook of Mathematical Functions, Abramowitz & Stegun.


;;; Gamma function.

(defun calcFunc-gamma (x)
  (or (math-numberp x) (math-reject-arg x 'numberp))
  (calcFunc-fact (math-add x -1)))

(defun math-gammap1-raw (x &optional fprec nfprec)
  "Compute gamma(1+X) to the appropriate precision."
  (or fprec
      (setq fprec (math-float calc-internal-prec)
	    nfprec (math-float (- calc-internal-prec))))
  (cond ((math-lessp-float (calcFunc-re x) fprec)
	 (if (math-lessp-float (calcFunc-re x) nfprec)
	     (math-neg (math-div
			(math-pi)
			(math-mul (math-gammap1-raw
				   (math-add (math-neg x)
					     '(float -1 0))
				   fprec nfprec)
				  (math-sin-raw
				   (math-mul (math-pi) x)))))
	   (let ((xplus1 (math-add x '(float 1 0))))
	     (math-div (math-gammap1-raw xplus1 fprec nfprec) xplus1))))
	((and (math-realp x)
	      (math-lessp-float '(float 736276 0) x))
	 (math-overflow))
	(t   ; re(x) now >= 10.0
	 (let ((xinv (math-div 1 x))
	       (lnx (math-ln-raw x)))
	   (math-mul (math-sqrt-two-pi)
		     (math-exp-raw
		      (math-gamma-series
		       (math-sub (math-mul (math-add x '(float 5 -1))
					   lnx)
				 x)
		       xinv
		       (math-sqr xinv)
		       '(float 0 0)
		       2)))))))

(defun math-gamma-series (sum x xinvsqr oterm n)
  (math-working "gamma" sum)
  (let* ((bn (math-bernoulli-number n))
	 (term (math-mul (math-div-float (math-float (nth 1 bn))
					 (math-float (* (nth 2 bn)
							(* n (1- n)))))
			 x))
	 (next (math-add sum term)))
    (if (math-nearly-equal sum next)
	next
      (if (> n (* 2 calc-internal-prec))
	  (progn
	    ;; Need this because series eventually diverges for large enough n.
	    (calc-record-why
	     "*Gamma computation stopped early, not all digits may be valid")
	    next)
	(math-gamma-series next (math-mul x xinvsqr) xinvsqr term (+ n 2))))))


;;; Incomplete gamma function.

(defvar math-current-gamma-value nil)
(defun calcFunc-gammaP (a x)
  (if (equal x '(var inf var-inf))
      '(float 1 0)
    (math-inexact-result)
    (or (Math-numberp a) (math-reject-arg a 'numberp))
    (or (math-numberp x) (math-reject-arg x 'numberp))
    (if (and (math-num-integerp a)
	     (integerp (setq a (math-trunc a)))
	     (> a 0) (< a 20))
	(math-sub 1 (calcFunc-gammaQ a x))
      (let ((math-current-gamma-value (calcFunc-gamma a)))
	(math-div (calcFunc-gammag a x) math-current-gamma-value)))))

(defun calcFunc-gammaQ (a x)
  (if (equal x '(var inf var-inf))
      '(float 0 0)
    (math-inexact-result)
    (or (Math-numberp a) (math-reject-arg a 'numberp))
    (or (math-numberp x) (math-reject-arg x 'numberp))
    (if (and (math-num-integerp a)
	     (integerp (setq a (math-trunc a)))
	     (> a 0) (< a 20))
	(let ((n 0)
	      (sum '(float 1 0))
	      (term '(float 1 0)))
	  (math-with-extra-prec 1
	    (while (< (setq n (1+ n)) a)
	      (setq term (math-div (math-mul term x) n)
		    sum (math-add sum term))
	      (math-working "gamma" sum))
	    (math-mul sum (calcFunc-exp (math-neg x)))))
      (let ((math-current-gamma-value (calcFunc-gamma a)))
	(math-div (calcFunc-gammaG a x) math-current-gamma-value)))))

(defun calcFunc-gammag (a x)
  (if (equal x '(var inf var-inf))
      (calcFunc-gamma a)
    (math-inexact-result)
    (or (Math-numberp a) (math-reject-arg a 'numberp))
    (or (Math-numberp x) (math-reject-arg x 'numberp))
    (math-with-extra-prec 2
      (setq a (math-float a))
      (setq x (math-float x))
      (if (or (math-negp (calcFunc-re a))
	      (math-lessp-float (calcFunc-re x)
				(math-add-float (calcFunc-re a)
						'(float 1 0))))
	  (math-inc-gamma-series a x)
	(math-sub (or math-current-gamma-value (calcFunc-gamma a))
		  (math-inc-gamma-cfrac a x))))))

(defun calcFunc-gammaG (a x)
  (if (equal x '(var inf var-inf))
      '(float 0 0)
    (math-inexact-result)
    (or (Math-numberp a) (math-reject-arg a 'numberp))
    (or (Math-numberp x) (math-reject-arg x 'numberp))
    (math-with-extra-prec 2
      (setq a (math-float a))
      (setq x (math-float x))
      (if (or (math-negp (calcFunc-re a))
	      (math-lessp-float (calcFunc-re x)
				(math-add-float (math-abs-approx a)
						'(float 1 0))))
	  (math-sub (or math-current-gamma-value (calcFunc-gamma a))
		    (math-inc-gamma-series a x))
	(math-inc-gamma-cfrac a x)))))

(defun math-inc-gamma-series (a x)
  (if (Math-zerop x)
      '(float 0 0)
    (math-mul (math-exp-raw (math-sub (math-mul a (math-ln-raw x)) x))
	      (math-with-extra-prec 2
		(let ((start (math-div '(float 1 0) a)))
		  (math-inc-gamma-series-step start start a x))))))

(defun math-inc-gamma-series-step (sum term a x)
  (math-working "gamma" sum)
  (setq a (math-add a '(float 1 0))
	term (math-div (math-mul term x) a))
  (let ((next (math-add sum term)))
    (if (math-nearly-equal sum next)
	next
      (math-inc-gamma-series-step next term a x))))

(defun math-inc-gamma-cfrac (a x)
  (if (Math-zerop x)
      (or math-current-gamma-value (calcFunc-gamma a))
    (math-mul (math-exp-raw (math-sub (math-mul a (math-ln-raw x)) x))
	      (math-inc-gamma-cfrac-step '(float 1 0) x
					 '(float 0 0) '(float 1 0)
					 '(float 1 0) '(float 1 0) '(float 0 0)
					 a x))))

(defun math-inc-gamma-cfrac-step (a0 a1 b0 b1 n fac g a x)
  (let ((ana (math-sub n a))
	(anf (math-mul n fac)))
    (setq n (math-add n '(float 1 0))
	  a0 (math-mul (math-add a1 (math-mul a0 ana)) fac)
	  b0 (math-mul (math-add b1 (math-mul b0 ana)) fac)
	  a1 (math-add (math-mul x a0) (math-mul anf a1))
	  b1 (math-add (math-mul x b0) (math-mul anf b1)))
    (if (math-zerop a1)
	(math-inc-gamma-cfrac-step a0 a1 b0 b1 n fac g a x)
      (setq fac (math-div '(float 1 0) a1))
      (let ((next (math-mul b1 fac)))
	(math-working "gamma" next)
	(if (math-nearly-equal next g)
	    next
	  (math-inc-gamma-cfrac-step a0 a1 b0 b1 n fac next a x))))))


;;; Error function.

(defun calcFunc-erf (x)
  (if (equal x '(var inf var-inf))
      '(float 1 0)
    (if (equal x '(neg (var inf var-inf)))
	'(float -1 0)
      (if (Math-zerop x)
	  x
	(let ((math-current-gamma-value (math-sqrt-pi)))
	  (math-to-same-complex-quad
	   (math-div (calcFunc-gammag '(float 5 -1)
				      (math-sqr (math-to-complex-quad-one x)))
		     math-current-gamma-value)
	   x))))))

(defun calcFunc-erfc (x)
  (if (equal x '(var inf var-inf))
      '(float 0 0)
    (if (math-posp x)
	(let ((math-current-gamma-value (math-sqrt-pi)))
	  (math-div (calcFunc-gammaG '(float 5 -1) (math-sqr x))
		    math-current-gamma-value))
      (math-sub 1 (calcFunc-erf x)))))

(defun math-to-complex-quad-one (x)
  (if (eq (car-safe x) 'polar) (setq x (math-complex x)))
  (if (eq (car-safe x) 'cplx)
      (list 'cplx (math-abs (nth 1 x)) (math-abs (nth 2 x)))
    x))

(defun math-to-same-complex-quad (x y)
  (if (eq (car-safe y) 'cplx)
      (if (eq (car-safe x) 'cplx)
	  (list 'cplx
		(if (math-negp (nth 1 y)) (math-neg (nth 1 x)) (nth 1 x))
		(if (math-negp (nth 2 y)) (math-neg (nth 2 x)) (nth 2 x)))
	(if (math-negp (nth 1 y)) (math-neg x) x))
    (if (math-negp y)
	(if (eq (car-safe x) 'cplx)
	    (list 'cplx (math-neg (nth 1 x)) (nth 2 x))
	  (math-neg x))
      x)))


;;; Beta function.

(defun calcFunc-beta (a b)
  (if (math-num-integerp a)
      (let ((am (math-add a -1)))
	(or (math-numberp b) (math-reject-arg b 'numberp))
	(math-div 1 (math-mul b (calcFunc-choose (math-add b am) am))))
    (if (math-num-integerp b)
	(calcFunc-beta b a)
      (math-div (math-mul (calcFunc-gamma a) (calcFunc-gamma b))
		(calcFunc-gamma (math-add a b))))))


;;; Incomplete beta function.

(defvar math-current-beta-value nil)
(defun calcFunc-betaI (x a b)
  (cond ((math-zerop x)
	 '(float 0 0))
	((math-equal-int x 1)
	 '(float 1 0))
	((or (math-zerop a)
	     (and (math-num-integerp a)
		  (math-negp a)))
	 (if (or (math-zerop b)
		 (and (math-num-integerp b)
		      (math-negp b)))
	     (math-reject-arg b 'range)
	   '(float 1 0)))
	((or (math-zerop b)
	     (and (math-num-integerp b)
		  (math-negp b)))
	 '(float 0 0))
	((not (math-numberp a)) (math-reject-arg a 'numberp))
	((not (math-numberp b)) (math-reject-arg b 'numberp))
	((math-inexact-result))
	(t (let ((math-current-beta-value (calcFunc-beta a b)))
	     (math-div (calcFunc-betaB x a b) math-current-beta-value)))))

(defun calcFunc-betaB (x a b)
  (cond
   ((math-zerop x)
    '(float 0 0))
   ((math-equal-int x 1)
    (calcFunc-beta a b))
   ((not (math-numberp x)) (math-reject-arg x 'numberp))
   ((not (math-numberp a)) (math-reject-arg a 'numberp))
   ((not (math-numberp b)) (math-reject-arg b 'numberp))
   ((math-zerop a) (math-reject-arg a 'nonzerop))
   ((math-zerop b) (math-reject-arg b 'nonzerop))
   ((and (math-num-integerp b)
	 (if (math-negp b)
	     (math-reject-arg b 'range)
	   (Math-natnum-lessp (setq b (math-trunc b)) 20)))
    (and calc-symbolic-mode (or (math-floatp a) (math-floatp b))
	 (math-inexact-result))
    (math-mul
     (math-with-extra-prec 2
       (let* ((i 0)
	      (term 1)
	      (sum (math-div term a)))
	 (while (< (setq i (1+ i)) b)
	   (setq term (math-mul (math-div (math-mul term (- i b)) i) x)
		 sum (math-add sum (math-div term (math-add a i))))
	   (math-working "beta" sum))
	 sum))
     (math-pow x a)))
   ((and (math-num-integerp a)
	 (if (math-negp a)
	     (math-reject-arg a 'range)
	   (Math-natnum-lessp (setq a (math-trunc a)) 20)))
    (math-sub (or math-current-beta-value (calcFunc-beta a b))
	      (calcFunc-betaB (math-sub 1 x) b a)))
   (t
    (math-inexact-result)
    (math-with-extra-prec 2
      (setq x (math-float x))
      (setq a (math-float a))
      (setq b (math-float b))
      (let ((bt (math-exp-raw (math-add (math-mul a (math-ln-raw x))
					(math-mul b (math-ln-raw
						     (math-sub '(float 1 0)
							       x)))))))
	(if (Math-lessp x (math-div (math-add a '(float 1 0))
				    (math-add (math-add a b) '(float 2 0))))
	    (math-div (math-mul bt (math-beta-cfrac a b x)) a)
	  (math-sub (or math-current-beta-value (calcFunc-beta a b))
		    (math-div (math-mul bt
					(math-beta-cfrac b a (math-sub 1 x)))
			      b))))))))

(defun math-beta-cfrac (a b x)
  (let ((qab (math-add a b))
	(qap (math-add a '(float 1 0)))
	(qam (math-add a '(float -1 0))))
    (math-beta-cfrac-step '(float 1 0)
			  (math-sub '(float 1 0)
				    (math-div (math-mul qab x) qap))
			  '(float 1 0) '(float 1 0)
			  '(float 1 0)
			  qab qap qam a b x)))

(defun math-beta-cfrac-step (az bz am bm m qab qap qam a b x)
  (let* ((two-m (math-mul m '(float 2 0)))
	 (d (math-div (math-mul (math-mul (math-sub b m) m) x)
		      (math-mul (math-add qam two-m) (math-add a two-m))))
	 (ap (math-add az (math-mul d am)))
	 (bp (math-add bz (math-mul d bm)))
	 (d2 (math-neg
	      (math-div (math-mul (math-mul (math-add a m) (math-add qab m)) x)
			(math-mul (math-add qap two-m) (math-add a two-m)))))
	 (app (math-add ap (math-mul d2 az)))
	 (bpp (math-add bp (math-mul d2 bz)))
	 (next (math-div app bpp)))
    (math-working "beta" next)
    (if (math-nearly-equal next az)
	next
      (math-beta-cfrac-step next '(float 1 0)
			    (math-div ap bpp) (math-div bp bpp)
			    (math-add m '(float 1 0))
			    qab qap qam a b x))))


;;; Bessel functions.

;;; Should generalize this to handle arbitrary precision!

(defun calcFunc-besJ (v x)
  (or (math-numberp v) (math-reject-arg v 'numberp))
  (or (math-numberp x) (math-reject-arg x 'numberp))
  (let ((calc-internal-prec (min 8 calc-internal-prec)))
    (math-with-extra-prec 3
      (setq x (math-float (math-normalize x)))
      (setq v (math-float (math-normalize v)))
      (cond ((math-zerop x)
	     (if (math-zerop v)
		 '(float 1 0)
	       '(float 0 0)))
	    ((math-inexact-result))
	    ((not (math-num-integerp v))
	     (let ((start (math-div 1 (calcFunc-fact v))))
	       (math-mul (math-besJ-series start start
					   0
					   (math-mul '(float -25 -2)
						     (math-sqr x))
					   v)
			 (math-pow (math-div x 2) v))))
	    ((math-negp (setq v (math-trunc v)))
	     (if (math-oddp v)
		 (math-neg (calcFunc-besJ (math-neg v) x))
	       (calcFunc-besJ (math-neg v) x)))
	    ((eq v 0)
	     (math-besJ0 x))
	    ((eq v 1)
	     (math-besJ1 x))
	    ((Math-lessp v (math-abs-approx x))
	     (let ((j 0)
		   (bjm (math-besJ0 x))
		   (bj (math-besJ1 x))
		   (two-over-x (math-div 2 x))
		   bjp)
	       (while (< (setq j (1+ j)) v)
		 (setq bjp (math-sub (math-mul (math-mul j two-over-x) bj)
				     bjm)
		       bjm bj
		       bj bjp))
	       bj))
	    (t
	     (if (Math-lessp 100 v) (math-reject-arg v 'range))
	     (let* ((j (logior (+ v (math-isqrt-small (* 40 v))) 1))
		    (two-over-x (math-div 2 x))
		    (jsum nil)
		    (bjp '(float 0 0))
		    (sum '(float 0 0))
		    (bj '(float 1 0))
		    bjm ans)
	       (while (> (setq j (1- j)) 0)
		 (setq bjm (math-sub (math-mul (math-mul j two-over-x) bj)
				     bjp)
		       bjp bj
		       bj bjm)
		 (if (> (nth 2 (math-abs-approx bj)) 10)
		     (setq bj (math-mul bj '(float 1 -10))
			   bjp (math-mul bjp '(float 1 -10))
			   ans (and ans (math-mul ans '(float 1 -10)))
			   sum (math-mul sum '(float 1 -10))))
		 (or (setq jsum (not jsum))
		     (setq sum (math-add sum bj)))
		 (if (= j v)
		     (setq ans bjp)))
	       (math-div ans (math-sub (math-mul 2 sum) bj))))))))

(defun math-besJ-series (sum term k zz vk)
  (math-working "besJ" sum)
  (setq k (1+ k)
	vk (math-add 1 vk)
	term (math-div (math-mul term zz) (math-mul k vk)))
  (let ((next (math-add sum term)))
    (if (math-nearly-equal next sum)
	next
      (math-besJ-series next term k zz vk))))

(defun math-besJ0 (x &optional yflag)
  (cond ((and (not yflag) (math-negp (calcFunc-re x)))
	 (math-besJ0 (math-neg x)))
	((Math-lessp '(float 8 0) (math-abs-approx x))
	 (let* ((z (math-div '(float 8 0) x))
		(y (math-sqr z))
		(xx (math-add x 
                              (math-read-number-simple "-0.785398164")))
		(a1 (math-poly-eval y
                          (list
                           (math-read-number-simple "0.0000002093887211")
                           (math-read-number-simple "-0.000002073370639")
                           (math-read-number-simple "0.00002734510407")
                           (math-read-number-simple "-0.001098628627")
                           '(float 1 0))))
		(a2 (math-poly-eval y
                          (list
                           (math-read-number-simple "-0.0000000934935152")
                           (math-read-number-simple "0.0000007621095161")
                           (math-read-number-simple "-0.000006911147651")
                           (math-read-number-simple "0.0001430488765")
                           (math-read-number-simple "-0.01562499995"))))
		(sc (math-sin-cos-raw xx)))
	       (if yflag
		   (setq sc (cons (math-neg (cdr sc)) (car sc))))
	       (math-mul (math-sqrt
			  (math-div (math-read-number-simple "0.636619722")
                                    x))
			 (math-sub (math-mul (cdr sc) a1)
				   (math-mul (car sc) (math-mul z a2))))))
	 (t
	  (let ((y (math-sqr x)))
	    (math-div (math-poly-eval y
                            (list
                             (math-read-number-simple "-184.9052456")
                             (math-read-number-simple "77392.33017")
                             (math-read-number-simple "-11214424.18")
                             (math-read-number-simple "651619640.7")
                             (math-read-number-simple "-13362590354.0")
                             (math-read-number-simple "57568490574.0")))
		      (math-poly-eval y
                            (list
                             '(float 1 0)
                             (math-read-number-simple "267.8532712")
                             (math-read-number-simple "59272.64853")
                             (math-read-number-simple "9494680.718")
                             (math-read-number-simple "1029532985.0")
                             (math-read-number-simple "57568490411.0"))))))))

(defun math-besJ1 (x &optional yflag)
  (cond ((and (math-negp (calcFunc-re x)) (not yflag))
	 (math-neg (math-besJ1 (math-neg x))))
	((Math-lessp '(float 8 0) (math-abs-approx x))
	 (let* ((z (math-div '(float 8 0) x))
		(y (math-sqr z))
		(xx (math-add x (math-read-number-simple "-2.356194491")))
		(a1 (math-poly-eval y
                          (list
                           (math-read-number-simple "-0.000000240337019")
                           (math-read-number-simple "0.000002457520174")
                           (math-read-number-simple "-0.00003516396496")
                           '(float 183105 -8)
                           '(float 1 0))))
		(a2 (math-poly-eval y
                          (list
                           (math-read-number-simple "0.000000105787412")
                           (math-read-number-simple "-0.00000088228987")
                           (math-read-number-simple "0.000008449199096")
                           (math-read-number-simple "-0.0002002690873")
                           (math-read-number-simple "0.04687499995"))))
		(sc (math-sin-cos-raw xx)))
	   (if yflag
	       (setq sc (cons (math-neg (cdr sc)) (car sc)))
	     (if (math-negp x)
		 (setq sc (cons (math-neg (car sc)) (math-neg (cdr sc))))))
	   (math-mul (math-sqrt (math-div 
                                 (math-read-number-simple "0.636619722")
                                 x))
		     (math-sub (math-mul (cdr sc) a1)
			       (math-mul (car sc) (math-mul z a2))))))
	(t
	 (let ((y (math-sqr x)))
	   (math-mul
	    x
	    (math-div (math-poly-eval y
                            (list
                             (math-read-number-simple "-30.16036606")
                             (math-read-number-simple "15704.4826")
                             (math-read-number-simple "-2972611.439")
                             (math-read-number-simple "242396853.1")
                             (math-read-number-simple "-7895059235.0")
                             (math-read-number-simple "72362614232.0")))
		      (math-poly-eval y
                            (list
                             '(float 1 0)
                             (math-read-number-simple "376.9991397")
                             (math-read-number-simple "99447.43394")
                             (math-read-number-simple "18583304.74")
                             (math-read-number-simple "2300535178.0")
                             (math-read-number-simple "144725228442.0")))))))))

(defun calcFunc-besY (v x)
  (math-inexact-result)
  (or (math-numberp v) (math-reject-arg v 'numberp))
  (or (math-numberp x) (math-reject-arg x 'numberp))
  (let ((calc-internal-prec (min 8 calc-internal-prec)))
    (math-with-extra-prec 3
      (setq x (math-float (math-normalize x)))
      (setq v (math-float (math-normalize v)))
      (cond ((not (math-num-integerp v))
	     (let ((sc (math-sin-cos-raw (math-mul v (math-pi)))))
	       (math-div (math-sub (math-mul (calcFunc-besJ v x) (cdr sc))
				   (calcFunc-besJ (math-neg v) x))
			 (car sc))))
	    ((math-negp (setq v (math-trunc v)))
	     (if (math-oddp v)
		 (math-neg (calcFunc-besY (math-neg v) x))
	       (calcFunc-besY (math-neg v) x)))
	    ((eq v 0)
	     (math-besY0 x))
	    ((eq v 1)
	     (math-besY1 x))
	    (t
	     (let ((j 0)
		   (bym (math-besY0 x))
		   (by (math-besY1 x))
		   (two-over-x (math-div 2 x))
		   byp)
	       (while (< (setq j (1+ j)) v)
		 (setq byp (math-sub (math-mul (math-mul j two-over-x) by)
				     bym)
		       bym by
		       by byp))
	       by))))))

(defun math-besY0 (x)
  (cond ((Math-lessp (math-abs-approx x) '(float 8 0))
	 (let ((y (math-sqr x)))
 	   (math-add
	    (math-div (math-poly-eval y
                            (list
                             (math-read-number-simple "228.4622733")
                             (math-read-number-simple "-86327.92757")
                             (math-read-number-simple "10879881.29")
                             (math-read-number-simple "-512359803.6")
                             (math-read-number-simple "7062834065.0")
                             (math-read-number-simple "-2957821389.0")))
		      (math-poly-eval y
                            (list
                             '(float 1 0)
                             (math-read-number-simple "226.1030244")
                             (math-read-number-simple "47447.2647")
                             (math-read-number-simple "7189466.438")
                             (math-read-number-simple "745249964.8")
                             (math-read-number-simple "40076544269.0"))))
	    (math-mul (math-read-number-simple "0.636619772")
		      (math-mul (math-besJ0 x) (math-ln-raw x))))))
	((math-negp (calcFunc-re x))
	 (math-add (math-besJ0 (math-neg x) t)
		   (math-mul '(cplx 0 2)
			     (math-besJ0 (math-neg x)))))
	(t
	 (math-besJ0 x t))))

(defun math-besY1 (x)
  (cond ((Math-lessp (math-abs-approx x) '(float 8 0))
	 (let ((y (math-sqr x)))
	   (math-add
	    (math-mul
	     x
	     (math-div (math-poly-eval y
                             (list
                              (math-read-number-simple "8511.937935")
                              (math-read-number-simple "-4237922.726")
                              (math-read-number-simple "734926455.1")
                              (math-read-number-simple "-51534381390.0")
                              (math-read-number-simple "1275274390000.0")
                              (math-read-number-simple "-4900604943000.0")))
		       (math-poly-eval y
                             (list
                              '(float 1 0)
                              (math-read-number-simple "354.9632885")
                              (math-read-number-simple "102042.605")
                              (math-read-number-simple "22459040.02")
                              (math-read-number-simple "3733650367.0")
                              (math-read-number-simple "424441966400.0")
                              (math-read-number-simple "24995805700000.0")))))
	    (math-mul (math-read-number-simple "0.636619772")
                      (math-sub (math-mul (math-besJ1 x) (math-ln-raw x))
				(math-div 1 x))))))
	((math-negp (calcFunc-re x))
	 (math-neg
	  (math-add (math-besJ1 (math-neg x) t)
		    (math-mul '(cplx 0 2)
			      (math-besJ1 (math-neg x))))))
	(t
	 (math-besJ1 x t))))

(defun math-poly-eval (x coefs)
  (let ((accum (car coefs)))
    (while (setq coefs (cdr coefs))
      (setq accum (math-add (car coefs) (math-mul accum x))))
    accum))


;;;; Bernoulli and Euler polynomials and numbers.

(defun calcFunc-bern (n &optional x)
  (if (and x (not (math-zerop x)))
      (if (and calc-symbolic-mode (math-floatp x))
	  (math-inexact-result)
	(math-build-polynomial-expr (math-bernoulli-coefs n) x))
    (or (math-num-natnump n) (math-reject-arg n 'natnump))
    (if (consp n)
	(progn
	  (math-inexact-result)
	  (math-float (math-bernoulli-number (math-trunc n))))
      (math-bernoulli-number n))))

(defun calcFunc-euler (n &optional x)
  (or (math-num-natnump n) (math-reject-arg n 'natnump))
  (if x
      (let* ((n1 (math-add n 1))
	     (coefs (math-bernoulli-coefs n1))
	     (fac (math-div (math-pow 2 n1) n1))
	     (k -1)
	     (x1 (math-div (math-add x 1) 2))
	     (x2 (math-div x 2)))
	(if (math-numberp x)
	    (if (and calc-symbolic-mode (math-floatp x))
		(math-inexact-result)
	      (math-mul fac
			(math-sub (math-build-polynomial-expr coefs x1)
				  (math-build-polynomial-expr coefs x2))))
	  (calcFunc-collect
	   (math-reduce-vec
	    'math-add
	    (cons 'vec
		  (mapcar (function
			   (lambda (c)
			     (setq k (1+ k))
			     (math-mul (math-mul fac c)
				       (math-sub (math-pow x1 k)
						 (math-pow x2 k)))))
			  coefs)))
	   x)))
    (math-mul (math-pow 2 n)
	      (if (consp n)
		  (progn
		    (math-inexact-result)
		    (calcFunc-euler n '(float 5 -1)))
		(calcFunc-euler n '(frac 1 2))))))

(defvar math-bernoulli-b-cache
  (list
   (list 'frac 
         -174611
         (math-read-number-simple "802857662698291200000"))
   (list 'frac 
         43867 
         (math-read-number-simple "5109094217170944000"))
   (list 'frac 
         -3617 
         (math-read-number-simple "10670622842880000"))
   (list 'frac 
         1 
         (math-read-number-simple "74724249600"))
   (list 'frac 
         -691 
         (math-read-number-simple "1307674368000"))
   (list 'frac 
         1 
         (math-read-number-simple "47900160"))
   (list 'frac 
         -1 
         (math-read-number-simple "1209600"))
   (list 'frac 
         1 
         30240) 
   (list 'frac 
         -1 
         720)
   (list 'frac 
         1 
         12) 
   1 ))

(defvar math-bernoulli-B-cache 
  '((frac -174611 330) (frac 43867 798)
    (frac -3617 510) (frac 7 6) (frac -691 2730)
    (frac 5 66) (frac -1 30) (frac 1 42)
    (frac -1 30) (frac 1 6) 1 ))

(defvar math-bernoulli-cache-size 11)
(defun math-bernoulli-coefs (n)
  (let* ((coefs (list (calcFunc-bern n)))
	 (nn (math-trunc n))
	 (k nn)
	 (term nn)
	 coef
	 (calc-prefer-frac (or (integerp n) calc-prefer-frac)))
    (while (>= (setq k (1- k)) 0)
      (setq term (math-div term (- nn k))
	    coef (math-mul term (math-bernoulli-number k))
	    coefs (cons (if (consp n) (math-float coef) coef) coefs)
	    term (math-mul term k)))
    (nreverse coefs)))

(defun math-bernoulli-number (n)
  (if (= (% n 2) 1)
      (if (= n 1)
	  '(frac -1 2)
	0)
    (setq n (/ n 2))
    (while (>= n math-bernoulli-cache-size)
      (let* ((sum 0)
	     (nk 1)     ; nk = n-k+1
	     (fact 1)   ; fact = (n-k+1)!
	     ofact
	     (p math-bernoulli-b-cache)
	     (calc-prefer-frac t))
	(math-working "bernoulli B" (* 2 math-bernoulli-cache-size))
	(while p
	  (setq nk (+ nk 2)
		ofact fact
		fact (math-mul fact (* nk (1- nk)))
		sum (math-add sum (math-div (car p) fact))
		p (cdr p)))
	(setq ofact (math-mul ofact (1- nk))
	      sum (math-sub (math-div '(frac 1 2) ofact) sum)
	      math-bernoulli-b-cache (cons sum math-bernoulli-b-cache)
	      math-bernoulli-B-cache (cons (math-mul sum ofact)
					   math-bernoulli-B-cache)
	      math-bernoulli-cache-size (1+ math-bernoulli-cache-size))))
    (nth (- math-bernoulli-cache-size n 1) math-bernoulli-B-cache)))

;;;   Bn = n! bn
;;;   bn = - sum_k=0^n-1 bk / (n-k+1)!

;;; A faster method would be to use "tangent numbers", c.f., Concrete
;;; Mathematics pg. 273.


;;; Probability distributions.

;;; Binomial.
(defun calcFunc-utpb (x n p)
  (if math-expand-formulas
      (math-normalize (list 'calcFunc-betaI p x (list '+ (list '- n x) 1)))
    (calcFunc-betaI p x (math-add (math-sub n x) 1))))
(put 'calcFunc-utpb 'math-expandable t)

(defun calcFunc-ltpb (x n p)
  (math-sub 1 (calcFunc-utpb x n p)))
(put 'calcFunc-ltpb 'math-expandable t)

;;; Chi-square.
(defun calcFunc-utpc (chisq v)
  (if math-expand-formulas
      (math-normalize (list 'calcFunc-gammaQ (list '/ v 2) (list '/ chisq 2)))
    (calcFunc-gammaQ (math-div v 2) (math-div chisq 2))))
(put 'calcFunc-utpc 'math-expandable t)

(defun calcFunc-ltpc (chisq v)
  (if math-expand-formulas
      (math-normalize (list 'calcFunc-gammaP (list '/ v 2) (list '/ chisq 2)))
    (calcFunc-gammaP (math-div v 2) (math-div chisq 2))))
(put 'calcFunc-ltpc 'math-expandable t)

;;; F-distribution.
(defun calcFunc-utpf (f v1 v2)
  (if math-expand-formulas
      (math-normalize (list 'calcFunc-betaI
			    (list '/ v2 (list '+ v2 (list '* v1 f)))
			    (list '/ v2 2)
			    (list '/ v1 2)))
    (calcFunc-betaI (math-div v2 (math-add v2 (math-mul v1 f)))
		    (math-div v2 2)
		    (math-div v1 2))))
(put 'calcFunc-utpf 'math-expandable t)

(defun calcFunc-ltpf (f v1 v2)
  (math-sub 1 (calcFunc-utpf f v1 v2)))
(put 'calcFunc-ltpf 'math-expandable t)

;;; Normal.
(defun calcFunc-utpn (x mean sdev)
  (if math-expand-formulas
      (math-normalize
       (list '/
	     (list '+ 1
		   (list 'calcFunc-erf
			 (list '/ (list '- mean x)
			       (list '* sdev (list 'calcFunc-sqrt 2)))))
	     2))
    (math-mul (math-add '(float 1 0)
			(calcFunc-erf
			 (math-div (math-sub mean x)
				   (math-mul sdev (math-sqrt-2)))))
	      '(float 5 -1))))
(put 'calcFunc-utpn 'math-expandable t)

(defun calcFunc-ltpn (x mean sdev)
  (if math-expand-formulas
      (math-normalize
       (list '/
	     (list '+ 1
		   (list 'calcFunc-erf
			 (list '/ (list '- x mean)
			       (list '* sdev (list 'calcFunc-sqrt 2)))))
	     2))
    (math-mul (math-add '(float 1 0)
			(calcFunc-erf
			 (math-div (math-sub x mean)
				   (math-mul sdev (math-sqrt-2)))))
	      '(float 5 -1))))
(put 'calcFunc-ltpn 'math-expandable t)

;;; Poisson.
(defun calcFunc-utpp (n x)
  (if math-expand-formulas
      (math-normalize (list 'calcFunc-gammaP x n))
    (calcFunc-gammaP x n)))
(put 'calcFunc-utpp 'math-expandable t)

(defun calcFunc-ltpp (n x)
  (if math-expand-formulas
      (math-normalize (list 'calcFunc-gammaQ x n))
    (calcFunc-gammaQ x n)))
(put 'calcFunc-ltpp 'math-expandable t)

;;; Student's t.  (As defined in Abramowitz & Stegun and Numerical Recipes.)
(defun calcFunc-utpt (tt v)
  (if math-expand-formulas
      (math-normalize (list 'calcFunc-betaI
			    (list '/ v (list '+ v (list '^ tt 2)))
			    (list '/ v 2)
			    '(float 5 -1)))
    (calcFunc-betaI (math-div v (math-add v (math-sqr tt)))
		    (math-div v 2)
		    '(float 5 -1))))
(put 'calcFunc-utpt 'math-expandable t)

(defun calcFunc-ltpt (tt v)
  (math-sub 1 (calcFunc-utpt tt v)))
(put 'calcFunc-ltpt 'math-expandable t)

(provide 'calc-funcs)

;;; calc-funcs.el ends here
