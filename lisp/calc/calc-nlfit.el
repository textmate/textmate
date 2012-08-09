;;; calc-nlfit.el --- nonlinear curve fitting for Calc

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

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

;; This code uses the Levenberg-Marquardt method, as described in
;; _Numerical Analysis_ by H. R. Schwarz, to fit data to
;; nonlinear curves.  Currently, the only the following curves are
;; supported:
;; The logistic S curve, y=a/(1+exp(b*(t-c)))
;;   Here, y is usually interpreted as the population of some
;;   quantity at time t.  So we will think of the data as consisting
;;   of quantities q0, q1, ..., qn and their respective times
;;   t0, t1, ..., tn.

;; The logistic bell curve, y=A*exp(B*(t-C))/(1+exp(B*(t-C)))^2
;;   Note that this is the derivative of the formula for the S curve.
;;   We get A=-a*b, B=b and C=c.  Here, y is interpreted as the rate
;;   of growth of a population at time t.  So we will think of the
;;   data as consisting of rates p0, p1, ..., pn and their
;;   respective times t0, t1, ..., tn.

;; The Hubbert Linearization, y/x=A*(1-x/B)
;;   Here, y is thought of as the rate of growth of a population
;;   and x represents the actual population.  This is essentially
;;   the differential equation describing the actual population.

;; The Levenberg-Marquardt method is an iterative process: it takes
;; an initial guess for the parameters and refines them.  To get an
;; initial guess for the parameters, we'll use a method described by
;; Luis de Sousa in "Hubbert's Peak Mathematics".  The idea is that
;; given quantities Q and the corresponding rates P, they should
;; satisfy P/Q= mQ+a.  We can use the parameter a for an
;; approximation for the parameter a in the S curve, and
;; approximations for b and c are found using least squares on the
;; linearization log((a/y)-1) = log(bb) + cc*t of
;; y=a/(1+bb*exp(cc*t)), which is equivalent to the above s curve
;; formula, and then translating it to b and c.  From this, we can
;; also get approximations for the bell curve parameters.

;;; Code:

(require 'calc-arith)
(require 'calcalg3)

;; Declare functions which are defined elsewhere.
(declare-function calc-get-fit-variables "calcalg3" (nv nc &optional defv defc with-y homog))
(declare-function math-map-binop "calcalg3" (binop args1 args2))

(defun math-nlfit-least-squares (xdata ydata &optional sdata sigmas)
  "Return the parameters A and B for the best least squares fit y=a+bx."
  (let* ((n (length xdata))
         (s2data (if sdata
                     (mapcar 'calcFunc-sqr sdata)
                  (make-list n 1)))
         (S (if sdata 0 n))
         (Sx 0)
         (Sy 0)
         (Sxx 0)
         (Sxy 0)
         D)
    (while xdata
      (let ((x (car xdata))
            (y (car ydata))
            (s (car s2data)))
        (setq Sx  (math-add Sx (if s (math-div x s) x)))
        (setq Sy  (math-add Sy (if s (math-div y s) y)))
        (setq Sxx (math-add Sxx (if s (math-div (math-mul x x) s)
                                  (math-mul x x))))
        (setq Sxy (math-add Sxy (if s (math-div (math-mul x y) s)
                                  (math-mul x y))))
        (if sdata
            (setq S (math-add S (math-div 1 s)))))
      (setq xdata (cdr xdata))
      (setq ydata (cdr ydata))
      (setq s2data (cdr s2data)))
    (setq D (math-sub (math-mul S Sxx) (math-mul Sx Sx)))
    (let ((A (math-div (math-sub (math-mul Sxx Sy) (math-mul Sx Sxy)) D))
          (B (math-div (math-sub (math-mul S Sxy) (math-mul Sx Sy)) D)))
      (if sigmas
          (let ((C11 (math-div Sxx D))
                (C12 (math-neg (math-div Sx D)))
                (C22 (math-div S D)))
            (list (list 'sdev A (calcFunc-sqrt C11))
                  (list 'sdev B (calcFunc-sqrt C22))
                  (list 'vec
                        (list 'vec C11 C12)
                        (list 'vec C12 C22))))
        (list A B)))))

;;; The methods described by de Sousa require the cumulative data qdata
;;; and the rates pdata.  We will assume that we are given either
;;; qdata and the corresponding times tdata, or pdata and the corresponding
;;; tdata.  The following two functions will find pdata or qdata,
;;; given the other..

;;; First, given two lists; one of values q0, q1, ..., qn and one of
;;; corresponding times t0, t1, ..., tn; return a list
;;; p0, p1, ..., pn of the rates of  change of the qi with respect to t.
;;; p0 is the right hand derivative (q1 - q0)/(t1 - t0).
;;; pn is the left hand derivative (qn - q(n-1))/(tn - t(n-1)).
;;; The other pis are the averages of the two:
;;;      (1/2)((qi - q(i-1))/(ti - t(i-1)) + (q(i+1) - qi)/(t(i+1) - ti)).

(defun math-nlfit-get-rates-from-cumul (tdata qdata)
  (let ((pdata (list
                (math-div
                 (math-sub (nth 1 qdata)
                           (nth 0 qdata))
                 (math-sub (nth 1 tdata)
                           (nth 0 tdata))))))
    (while (> (length qdata) 2)
      (setq pdata
            (cons
             (math-mul
              '(float 5 -1)
              (math-add
               (math-div
                (math-sub (nth 2 qdata)
                          (nth 1 qdata))
                (math-sub (nth 2 tdata)
                          (nth 1 tdata)))
               (math-div
                (math-sub (nth 1 qdata)
                          (nth 0 qdata))
                (math-sub (nth 1 tdata)
                          (nth 0 tdata)))))
             pdata))
      (setq qdata (cdr qdata)))
    (setq pdata
          (cons
           (math-div
            (math-sub (nth 1 qdata)
                      (nth 0 qdata))
            (math-sub (nth 1 tdata)
                      (nth 0 tdata)))
           pdata))
    (reverse pdata)))

;;; Next, given two lists -- one of rates p0, p1, ..., pn and one of
;;; corresponding times t0, t1, ..., tn -- and an initial values q0,
;;;  return a list q0, q1, ..., qn of the cumulative values.
;;; q0 is the initial value given.
;;; For i>0, qi is computed using the trapezoid rule:
;;;     qi = q(i-1) + (1/2)(pi + p(i-1))(ti - t(i-1))

(defun math-nlfit-get-cumul-from-rates (tdata pdata q0)
  (let* ((qdata (list q0)))
    (while (cdr pdata)
      (setq qdata
            (cons
             (math-add (car qdata)
                       (math-mul
                        (math-mul
                         '(float 5 -1)
                         (math-add (nth 1 pdata) (nth 0 pdata)))
                        (math-sub (nth 1 tdata)
                                  (nth 0 tdata))))
             qdata))
      (setq pdata (cdr pdata))
      (setq tdata (cdr tdata)))
    (reverse qdata)))

;;; Given the qdata, pdata and tdata, find the parameters
;;; a, b and c that fit q = a/(1+b*exp(c*t)).
;;; a is found using the method described by de Sousa.
;;; b and c are found using least squares on the linearization
;;; log((a/q)-1) = log(b) + c*t
;;; In some cases (where the logistic curve may well be the wrong
;;; model), the computed a will be less than or equal to the maximum
;;; value of q in qdata; in which case the above linearization won't work.
;;; In this case, a will be replaced by a number slightly above
;;; the maximum value of q.

(defun math-nlfit-find-qmax (qdata pdata tdata)
  (let* ((ratios (math-map-binop 'math-div pdata qdata))
         (lsdata (math-nlfit-least-squares ratios tdata))
         (qmax (math-max-list (car qdata) (cdr qdata)))
         (a (math-neg (math-div (nth 1 lsdata) (nth 0 lsdata)))))
    (if (math-lessp a qmax)
        (math-add '(float 5 -1) qmax)
      a)))

(defun math-nlfit-find-logistic-parameters (qdata pdata tdata)
  (let* ((a (math-nlfit-find-qmax qdata pdata tdata))
         (newqdata
          (mapcar (lambda (q) (calcFunc-ln (math-sub (math-div a q) 1)))
                  qdata))
         (bandc (math-nlfit-least-squares tdata newqdata)))
    (list
     a
     (calcFunc-exp (nth 0 bandc))
     (nth 1 bandc))))

;;; Next, given the pdata and tdata, we can find the qdata if we know q0.
;;; We first try to find q0, using the fact that when p takes on its largest
;;; value, q is half of its maximum value.  So we'll find the maximum value
;;; of q given various q0, and use bisection to approximate the correct q0.

;;; First, given pdata and tdata, find what half of qmax would be if q0=0.

(defun math-nlfit-find-qmaxhalf (pdata tdata)
  (let ((pmax (math-max-list (car pdata) (cdr pdata)))
        (qmh 0))
    (while (math-lessp (car pdata) pmax)
      (setq qmh
            (math-add qmh
                      (math-mul
                       (math-mul
                        '(float 5 -1)
                        (math-add (nth 1 pdata) (nth 0 pdata)))
                       (math-sub (nth 1 tdata)
                                 (nth 0 tdata)))))
      (setq pdata (cdr pdata))
      (setq tdata (cdr tdata)))
    qmh))

;;; Next, given pdata and tdata, approximate q0.

(defun math-nlfit-find-q0 (pdata tdata)
  (let* ((qhalf (math-nlfit-find-qmaxhalf pdata tdata))
         (q0 (math-mul 2 qhalf))
         (qdata (math-nlfit-get-cumul-from-rates tdata pdata q0)))
    (while (math-lessp (math-nlfit-find-qmax
                        (mapcar
                         (lambda (q) (math-add q0 q))
                         qdata)
                        pdata tdata)
                       (math-mul
                        '(float 5 -1)
                        (math-add
                         q0
                         qhalf)))
      (setq q0 (math-add q0 qhalf)))
    (let* ((qmin (math-sub q0 qhalf))
           (qmax q0)
           (qt (math-nlfit-find-qmax
                (mapcar
                 (lambda (q) (math-add q0 q))
                 qdata)
                pdata tdata))
           (i 0))
      (while (< i 10)
        (setq q0 (math-mul '(float 5 -1) (math-add qmin qmax)))
        (if (math-lessp
             (math-nlfit-find-qmax
              (mapcar
               (lambda (q) (math-add q0 q))
               qdata)
              pdata tdata)
             (math-mul '(float 5 -1) (math-add qhalf q0)))
            (setq qmin q0)
          (setq qmax q0))
        (setq i (1+ i)))
      (math-mul '(float 5 -1) (math-add qmin qmax)))))

;;; To improve the approximations to the parameters, we can use
;;; Marquardt method as described in Schwarz's book.

;;; Small numbers used in the Givens algorithm
(defvar math-nlfit-delta '(float 1 -8))

(defvar math-nlfit-epsilon '(float 1 -5))

;;; Maximum number of iterations
(defvar math-nlfit-max-its 100)

;;; Next, we need some functions for dealing with vectors and
;;; matrices.  For convenience, we'll work with Emacs lists
;;; as vectors, rather than Calc's vectors.

(defun math-nlfit-set-elt (vec i x)
  (setcar (nthcdr (1- i) vec) x))

(defun math-nlfit-get-elt (vec i)
  (nth (1- i) vec))

(defun math-nlfit-make-matrix (i j)
  (let ((row (make-list j 0))
        (mat nil)
        (k 0))
    (while (< k i)
      (setq mat (cons (copy-sequence row) mat))
      (setq k (1+ k)))
    mat))

(defun math-nlfit-set-matx-elt (mat i j x)
  (setcar (nthcdr (1- j) (nth (1- i) mat)) x))

(defun math-nlfit-get-matx-elt (mat i j)
  (nth (1- j) (nth (1- i) mat)))

;;; For solving the linearized system.
;;; (The Givens method, from Schwarz.)

(defun math-nlfit-givens (C d)
  (let* ((C (copy-tree C))
         (d (copy-tree d))
         (n (length (car C)))
         (N (length C))
         (j 1)
         (r (make-list N 0))
         (x (make-list N 0))
         w
         gamma
         sigma
         rho)
    (while (<= j n)
      (let ((i (1+ j)))
        (while (<= i N)
          (let ((cij (math-nlfit-get-matx-elt C i j))
                (cjj (math-nlfit-get-matx-elt C j j)))
            (when (not (math-equal 0 cij))
                (if (math-lessp (calcFunc-abs cjj)
                                (math-mul math-nlfit-delta (calcFunc-abs cij)))
                    (setq w (math-neg cij)
                          gamma 0
                          sigma 1
                          rho 1)
                  (setq w (math-mul
                           (calcFunc-sign cjj)
                           (calcFunc-sqrt
                            (math-add
                             (math-mul cjj cjj)
                             (math-mul cij cij))))
                        gamma (math-div cjj w)
                        sigma (math-neg (math-div cij w)))
                  (if (math-lessp (calcFunc-abs sigma) gamma)
                      (setq rho sigma)
                    (setq rho (math-div (calcFunc-sign sigma) gamma))))
              (setq cjj w
                    cij rho)
              (math-nlfit-set-matx-elt C j j w)
              (math-nlfit-set-matx-elt C i j rho)
              (let ((k (1+ j)))
                (while (<= k n)
                  (let* ((cjk (math-nlfit-get-matx-elt C j k))
                         (cik (math-nlfit-get-matx-elt C i k))
                         (h (math-sub
                             (math-mul gamma cjk) (math-mul sigma cik))))
                    (setq cik (math-add
                               (math-mul sigma cjk)
                               (math-mul gamma cik)))
                    (setq cjk h)
                    (math-nlfit-set-matx-elt C i k cik)
                    (math-nlfit-set-matx-elt C j k cjk)
                    (setq k (1+ k)))))
              (let* ((di (math-nlfit-get-elt d i))
                     (dj (math-nlfit-get-elt d j))
                     (h (math-sub
                         (math-mul gamma dj)
                         (math-mul sigma di))))
                (setq di (math-add
                          (math-mul sigma dj)
                          (math-mul gamma di)))
                (setq dj h)
                (math-nlfit-set-elt d i di)
                (math-nlfit-set-elt d j dj))))
          (setq i (1+ i))))
      (setq j (1+ j)))
    (let ((i n)
          s)
      (while (>= i 1)
        (math-nlfit-set-elt r i 0)
        (setq s (math-nlfit-get-elt d i))
        (let ((k (1+ i)))
          (while (<= k n)
            (setq s (math-add s (math-mul (math-nlfit-get-matx-elt C i k)
                            (math-nlfit-get-elt x k))))
            (setq k (1+ k))))
        (math-nlfit-set-elt x i
                            (math-neg
                             (math-div s
                                       (math-nlfit-get-matx-elt C i i))))
        (setq i (1- i))))
    (let ((i (1+ n)))
      (while (<= i N)
        (math-nlfit-set-elt r i (math-nlfit-get-elt d i))
        (setq i (1+ i))))
    (let ((j n))
      (while (>= j 1)
        (let ((i N))
          (while (>= i (1+ j))
            (setq rho (math-nlfit-get-matx-elt C i j))
            (if (math-equal rho 1)
                (setq gamma 0
                      sigma 1)
              (if (math-lessp (calcFunc-abs rho) 1)
                  (setq sigma rho
                        gamma (calcFunc-sqrt
                               (math-sub 1 (math-mul sigma sigma))))
                (setq gamma (math-div 1 (calcFunc-abs rho))
                      sigma (math-mul (calcFunc-sign rho)
                                       (calcFunc-sqrt
                                        (math-sub 1 (math-mul gamma gamma)))))))
            (let ((ri (math-nlfit-get-elt r i))
                  (rj (math-nlfit-get-elt r j))
                  h)
              (setq h (math-add (math-mul gamma rj)
                         (math-mul sigma ri)))
              (setq ri (math-sub
                        (math-mul gamma ri)
                        (math-mul sigma rj)))
              (setq rj h)
              (math-nlfit-set-elt r i ri)
              (math-nlfit-set-elt r j rj))
            (setq i (1- i))))
        (setq j (1- j))))

    x))

(defun math-nlfit-jacobian (grad xlist parms &optional slist)
  (let ((j nil))
    (while xlist
      (let ((row (apply grad (car xlist) parms)))
        (setq j
              (cons
               (if slist
                   (mapcar (lambda (x) (math-div x (car slist))) row)
                 row)
               j)))
      (setq slist (cdr slist))
      (setq xlist (cdr xlist)))
    (reverse j)))

(defun math-nlfit-make-ident (l n)
  (let ((m (math-nlfit-make-matrix n n))
        (i 1))
    (while (<= i n)
      (math-nlfit-set-matx-elt m i i l)
      (setq i (1+ i)))
    m))

(defun math-nlfit-chi-sq (xlist ylist parms fn &optional slist)
  (let ((cs 0))
    (while xlist
      (let ((c
             (math-sub
              (apply fn (car xlist) parms)
              (car ylist))))
        (if slist
            (setq c (math-div c (car slist))))
        (setq cs
              (math-add cs
                 (math-mul c c))))
      (setq xlist (cdr xlist))
      (setq ylist (cdr ylist))
      (setq slist (cdr slist)))
    cs))

(defun math-nlfit-init-lambda (C)
  (let ((l 0)
        (n (length (car C)))
        (N (length C)))
    (while C
      (let ((row (car C)))
        (while row
          (setq l (math-add l (math-mul (car row) (car row))))
          (setq row (cdr row))))
      (setq C (cdr C)))
    (calcFunc-sqrt (math-div l (math-mul n N)))))

(defun math-nlfit-make-Ctilda (C l)
  (let* ((n (length (car C)))
         (bot (math-nlfit-make-ident l n)))
    (append C bot)))

(defun math-nlfit-make-d (fn xdata ydata parms &optional sdata)
  (let ((d nil))
    (while xdata
      (setq d (cons
               (let ((dd (math-sub (apply fn (car xdata) parms)
                                   (car ydata))))
                 (if sdata (math-div dd (car sdata)) dd))
               d))
      (setq xdata (cdr xdata))
      (setq ydata (cdr ydata))
      (setq sdata (cdr sdata)))
    (reverse d)))

(defun math-nlfit-make-dtilda (d n)
  (append d (make-list n 0)))

(defun math-nlfit-fit (xlist ylist parms fn grad &optional slist)
  (let*
      ((C (math-nlfit-jacobian grad xlist parms slist))
       (d (math-nlfit-make-d fn xlist ylist parms slist))
       (chisq (math-nlfit-chi-sq xlist ylist parms fn slist))
       (lambda (math-nlfit-init-lambda C))
       (really-done nil)
       (iters 0))
    (while (and
            (not really-done)
            (< iters math-nlfit-max-its))
      (setq iters (1+ iters))
      (let ((done nil))
        (while (not done)
          (let* ((Ctilda (math-nlfit-make-Ctilda C lambda))
                 (dtilda (math-nlfit-make-dtilda d (length (car C))))
                 (zeta (math-nlfit-givens Ctilda dtilda))
                 (newparms (math-map-binop 'math-add (copy-tree parms) zeta))
                 (newchisq (math-nlfit-chi-sq xlist ylist newparms fn slist)))
            (if (math-lessp newchisq chisq)
                (progn
                  (if (math-lessp
                       (math-div
                        (math-sub chisq newchisq) newchisq) math-nlfit-epsilon)
                      (setq really-done t))
                  (setq lambda (math-div lambda 10))
                  (setq chisq newchisq)
                  (setq parms newparms)
                  (setq done t))
              (setq lambda (math-mul lambda 10)))))
        (setq C (math-nlfit-jacobian grad xlist parms slist))
        (setq d (math-nlfit-make-d fn xlist ylist parms slist))))
    (list chisq parms)))

;;; The functions that describe our models, and their gradients.

(defun math-nlfit-s-logistic-fn (x a b c)
  (math-div a (math-add 1 (math-mul b (calcFunc-exp (math-mul c x))))))

(defun math-nlfit-s-logistic-grad (x a b c)
  (let* ((ep (calcFunc-exp (math-mul c x)))
         (d (math-add 1 (math-mul b ep)))
         (d2 (math-mul d d)))
    (list
     (math-div 1 d)
     (math-neg (math-div (math-mul a ep) d2))
     (math-neg (math-div (math-mul a (math-mul b (math-mul x ep))) d2)))))

(defun math-nlfit-b-logistic-fn (x a c d)
  (let ((ex (calcFunc-exp (math-mul c (math-sub x d)))))
    (math-div
     (math-mul a ex)
     (math-sqr
      (math-add
       1 ex)))))

(defun math-nlfit-b-logistic-grad (x a c d)
  (let* ((ex (calcFunc-exp (math-mul c (math-sub x d))))
        (ex1 (math-add 1 ex))
        (xd (math-sub x d)))
    (list
     (math-div
      ex
      (math-sqr ex1))
     (math-sub
      (math-div
       (math-mul a (math-mul xd ex))
       (math-sqr ex1))
      (math-div
       (math-mul 2 (math-mul a (math-mul xd (math-sqr ex))))
       (math-pow ex1 3)))
     (math-sub
      (math-div
       (math-mul 2 (math-mul a (math-mul c (math-sqr ex))))
       (math-pow ex1 3))
      (math-div
       (math-mul a (math-mul c ex))
       (math-sqr ex1))))))

;;; Functions to get the final covariance matrix and the sdevs

(defun math-nlfit-find-covar (grad xlist pparms)
  (let ((j nil))
    (while xlist
      (setq j (cons (cons 'vec (apply grad (car xlist) pparms)) j))
      (setq xlist (cdr xlist)))
    (setq j (cons 'vec (reverse j)))
    (setq j
          (math-mul
           (calcFunc-trn j) j))
    (calcFunc-inv j)))

(defun math-nlfit-get-sigmas (grad xlist pparms chisq)
  (let* ((sgs nil)
         (covar (math-nlfit-find-covar grad xlist pparms))
         (n (1- (length covar)))
         (N (length xlist))
         (i 1))
    (when (> N n)
      (while (<= i n)
        (setq sgs (cons (calcFunc-sqrt (nth i (nth i covar))) sgs))
        (setq i (1+ i)))
      (setq sgs (reverse sgs)))
    (list sgs covar)))

;;; Now the Calc functions

(defun math-nlfit-s-logistic-params (xdata ydata)
  (let ((pdata (math-nlfit-get-rates-from-cumul xdata ydata)))
    (math-nlfit-find-logistic-parameters ydata pdata xdata)))

(defun math-nlfit-b-logistic-params (xdata ydata)
  (let* ((q0 (math-nlfit-find-q0 ydata xdata))
         (qdata (math-nlfit-get-cumul-from-rates xdata ydata q0))
         (abc (math-nlfit-find-logistic-parameters qdata ydata xdata))
         (B (nth 1 abc))
         (C (nth 2 abc))
         (A (math-neg
             (math-mul
              (nth 0 abc)
              (math-mul B C))))
         (D (math-neg (math-div (calcFunc-ln B) C)))
         (A (math-div A B)))
    (list A C D)))

;;; Some functions to turn the parameter lists and variables
;;; into the appropriate functions.

(defun math-nlfit-s-logistic-solnexpr (pms var)
  (let ((a (nth 0 pms))
        (b (nth 1 pms))
        (c (nth 2 pms)))
    (list '/ a
            (list '+
                  1
             (list '*
                   b
                   (calcFunc-exp
                    (list '*
                          c
                          var)))))))

(defun math-nlfit-b-logistic-solnexpr (pms var)
  (let ((a (nth 0 pms))
        (c (nth 1 pms))
        (d (nth 2 pms)))
    (list '/
          (list '*
                a
                (calcFunc-exp
                 (list '*
                       c
                       (list '- var d))))
          (list '^
                (list '+
                      1
                      (calcFunc-exp
                       (list '*
                             c
                             (list '- var d))))
                2))))

(defun math-nlfit-enter-result (n prefix vals)
  (setq calc-aborted-prefix prefix)
  (calc-pop-push-record-list n prefix vals)
  (calc-handle-whys))

(defun math-nlfit-fit-curve (fn grad solnexpr initparms &optional sdv)
  (calc-slow-wrapper
   (let* ((sdevv (or (eq sdv 'calcFunc-efit) (eq sdv 'calcFunc-xfit)))
          (calc-display-working-message nil)
          (data (calc-top 1))
          (xdata (cdr (car (cdr data))))
          (ydata (cdr (car (cdr (cdr data)))))
          (sdata (if (math-contains-sdev-p ydata)
                     (mapcar (lambda (x) (math-get-sdev x t)) ydata)
                   nil))
          (ydata (mapcar (lambda (x) (math-get-value x)) ydata))
          (calc-curve-varnames nil)
          (calc-curve-coefnames nil)
          (calc-curve-nvars 1)
          (fitvars (calc-get-fit-variables 1 3))
          (var (nth 1 calc-curve-varnames))
          (parms (cdr calc-curve-coefnames))
          (parmguess
           (funcall initparms xdata ydata))
          (fit (math-nlfit-fit xdata ydata parmguess fn grad sdata))
          (finalparms (nth 1 fit))
          (sigmacovar
           (if sdevv
               (math-nlfit-get-sigmas grad xdata finalparms (nth 0 fit))))
          (sigmas
           (if sdevv
               (nth 0 sigmacovar)))
          (finalparms
           (if sigmas
               (math-map-binop
                (lambda (x y) (list 'sdev x y)) finalparms sigmas)
             finalparms))
          (soln (funcall solnexpr finalparms var)))
     (let ((calc-fit-to-trail t)
           (traillist nil))
       (while parms
         (setq traillist (cons (list 'calcFunc-eq (car parms) (car finalparms))
                               traillist))
         (setq finalparms (cdr finalparms))
         (setq parms (cdr parms)))
       (setq traillist (calc-normalize (cons 'vec (nreverse traillist))))
       (cond ((eq sdv 'calcFunc-efit)
              (math-nlfit-enter-result 1 "efit" soln))
             ((eq sdv 'calcFunc-xfit)
              (let (sln)
                (setq sln
                      (list 'vec
                            soln
                            traillist
                            (nth 1 sigmacovar)
                            '(vec)
                            (nth 0 fit)
                            (let ((n (length xdata))
                                  (m (length finalparms)))
                              (if (and sdata (> n m))
                                  (calcFunc-utpc (nth 0 fit)
                                                 (- n m))
                                '(var nan var-nan)))))
                (math-nlfit-enter-result 1 "xfit" sln)))
             (t
              (math-nlfit-enter-result 1 "fit" soln)))
       (calc-record traillist "parm")))))

(defun calc-fit-s-shaped-logistic-curve (arg)
  (interactive "P")
  (math-nlfit-fit-curve 'math-nlfit-s-logistic-fn
                        'math-nlfit-s-logistic-grad
                        'math-nlfit-s-logistic-solnexpr
                        'math-nlfit-s-logistic-params
                        arg))

(defun calc-fit-bell-shaped-logistic-curve (arg)
  (interactive "P")
  (math-nlfit-fit-curve 'math-nlfit-b-logistic-fn
                        'math-nlfit-b-logistic-grad
                        'math-nlfit-b-logistic-solnexpr
                        'math-nlfit-b-logistic-params
                        arg))

(defun calc-fit-hubbert-linear-curve (&optional sdv)
  (calc-slow-wrapper
   (let* ((sdevv (or (eq sdv 'calcFunc-efit) (eq sdv 'calcFunc-xfit)))
          (calc-display-working-message nil)
          (data (calc-top 1))
          (qdata (cdr (car (cdr data))))
          (pdata (cdr (car (cdr (cdr data)))))
          (sdata (if (math-contains-sdev-p pdata)
                     (mapcar (lambda (x) (math-get-sdev x t)) pdata)
                   nil))
          (pdata (mapcar (lambda (x) (math-get-value x)) pdata))
          (poverqdata (math-map-binop 'math-div pdata qdata))
          (parmvals (math-nlfit-least-squares qdata poverqdata sdata sdevv))
          (finalparms (list (nth 0 parmvals)
                            (math-neg
                             (math-div (nth 0 parmvals)
                                       (nth 1 parmvals)))))
          (calc-curve-varnames nil)
          (calc-curve-coefnames nil)
          (calc-curve-nvars 1)
          (fitvars (calc-get-fit-variables 1 2))
          (var (nth 1 calc-curve-varnames))
          (parms (cdr calc-curve-coefnames))
          (soln (list '* (nth 0 finalparms)
                      (list '- 1
                            (list '/ var (nth 1 finalparms))))))
     (let ((calc-fit-to-trail t)
           (traillist nil))
       (setq traillist
             (list 'vec
                   (list 'calcFunc-eq (nth 0 parms) (nth 0 finalparms))
                   (list 'calcFunc-eq (nth 1 parms) (nth 1 finalparms))))
       (cond ((eq sdv 'calcFunc-efit)
              (math-nlfit-enter-result 1 "efit" soln))
             ((eq sdv 'calcFunc-xfit)
              (let (sln
                    (chisq
                     (math-nlfit-chi-sq
                      qdata poverqdata
                      (list (nth 1 (nth 0 finalparms))
                            (nth 1 (nth 1 finalparms)))
                      (lambda (x a b)
                        (math-mul a
                                  (math-sub
                                   1
                                   (math-div x b))))
                      sdata)))
                (setq sln
                      (list 'vec
                            soln
                            traillist
                            (nth 2 parmvals)
                            (list
                             'vec
                             '(calcFunc-fitdummy 1)
                             (list 'calcFunc-neg
                                   (list '/
                                         '(calcFunc-fitdummy 1)
                                         '(calcFunc-fitdummy 2))))
                            chisq
                            (let ((n (length qdata)))
                              (if (and sdata (> n 2))
                                  (calcFunc-utpc
                                   chisq
                                   (- n 2))
                                '(var nan var-nan)))))
                (math-nlfit-enter-result 1 "xfit" sln)))
             (t
              (math-nlfit-enter-result 1 "fit" soln)))
       (calc-record traillist "parm")))))

(provide 'calc-nlfit)
