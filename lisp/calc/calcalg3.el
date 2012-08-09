;;; calcalg3.el --- more algebraic functions for Calc

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

;; Declare functions which are defined elsewhere.
(declare-function calc-fit-s-shaped-logistic-curve "calc-nlfit" (arg))
(declare-function calc-fit-bell-shaped-logistic-curve "calc-nlfit" (arg))
(declare-function calc-fit-hubbert-linear-curve "calc-nlfit" (&optional sdv))
(declare-function calc-graph-add-curve "calc-graph" (xdata ydata &optional zdata))
(declare-function calc-graph-lookup "calc-graph" (thing))
(declare-function calc-graph-set-styles "calc-graph" (lines points &optional yerr))
(declare-function math-min-list "calc-arith" (a b))
(declare-function math-max-list "calc-arith" (a b))


(defun math-map-binop (binop args1 args2)
  "Apply BINOP to the elements of the lists ARGS1 and ARGS2"
  (if args1
      (cons
       (funcall binop (car args1) (car args2))
       (funcall 'math-map-binop binop (cdr args1) (cdr args2)))))

(defun calc-find-root (var)
  (interactive "sVariable(s) to solve for: ")
  (calc-slow-wrapper
   (let ((func (if (calc-is-hyperbolic) 'calcFunc-wroot 'calcFunc-root)))
     (if (or (equal var "") (equal var "$"))
	 (calc-enter-result 2 "root" (list func
					   (calc-top-n 3)
					   (calc-top-n 1)
					   (calc-top-n 2)))
       (let ((var (if (and (string-match ",\\|[^ ] +[^ ]" var)
			   (not (string-match "\\[" var)))
		      (math-read-expr (concat "[" var "]"))
		    (math-read-expr var))))
	 (if (eq (car-safe var) 'error)
	     (error "Bad format in expression: %s" (nth 1 var)))
	 (calc-enter-result 1 "root" (list func
					   (calc-top-n 2)
					   var
					   (calc-top-n 1))))))))

(defun calc-find-minimum (var)
  (interactive "sVariable(s) to minimize over: ")
  (calc-slow-wrapper
   (let ((func (if (calc-is-inverse)
		   (if (calc-is-hyperbolic)
		       'calcFunc-wmaximize 'calcFunc-maximize)
		 (if (calc-is-hyperbolic)
		     'calcFunc-wminimize 'calcFunc-minimize)))
	 (tag (if (calc-is-inverse) "max" "min")))
     (if (or (equal var "") (equal var "$"))
	 (calc-enter-result 2 tag (list func
					(calc-top-n 3)
					(calc-top-n 1)
					(calc-top-n 2)))
       (let ((var (if (and (string-match ",\\|[^ ] +[^ ]" var)
			   (not (string-match "\\[" var)))
		      (math-read-expr (concat "[" var "]"))
		    (math-read-expr var))))
	 (if (eq (car-safe var) 'error)
	     (error "Bad format in expression: %s" (nth 1 var)))
	 (calc-enter-result 1 tag (list func
					(calc-top-n 2)
					var
					(calc-top-n 1))))))))

(defun calc-find-maximum (var)
  (interactive "sVariable to maximize over: ")
  (calc-invert-func)
  (calc-find-minimum var))


(defun calc-poly-interp (arg)
  (interactive "P")
  (calc-slow-wrapper
   (let ((data (calc-top 2)))
     (if (or (consp arg) (eq arg 0) (eq arg 2))
	 (setq data (cons 'vec (calc-top-list 2 2)))
       (or (null arg)
	   (error "Bad prefix argument")))
     (if (calc-is-hyperbolic)
	 (calc-enter-result 1 "rati" (list 'calcFunc-ratint data (calc-top 1)))
       (calc-enter-result 1 "poli" (list 'calcFunc-polint data
					 (calc-top 1)))))))

;; The variables calc-curve-nvars, calc-curve-varnames, calc-curve-model and calc-curve-coefnames are local to calc-curve-fit, but are
;; used by calc-get-fit-variables which is called by calc-curve-fit.
(defvar calc-curve-nvars)
(defvar calc-curve-varnames)
(defvar calc-curve-model)
(defvar calc-curve-coefnames)

(defvar calc-curve-fit-history nil
  "History for calc-curve-fit.")

(defun calc-curve-fit (arg &optional calc-curve-model 
                           calc-curve-coefnames calc-curve-varnames)
  (interactive "P")
  (calc-slow-wrapper
   (setq calc-aborted-prefix nil)
   (let ((func (if (calc-is-inverse) 'calcFunc-xfit
		 (if (calc-is-hyperbolic) 'calcFunc-efit
		   'calcFunc-fit)))
	 key (which 0)
         (nonlinear nil)
         (plot nil)
	 n calc-curve-nvars temp data
	 (homog nil)
	 (msgs '( "(Press ? for help)"
		  "1 = linear or multilinear"
		  "2-9 = polynomial fits; i = interpolating polynomial"
		  "p = a x^b, ^ = a b^x"
		  "e = a exp(b x), x = exp(a + b x), l = a + b ln(x)"
		  "E = a 10^(b x), X = 10^(a + b x), L = a + b log10(x)"
		  "q = a + b (x-c)^2"
		  "g = (a/b sqrt(2 pi)) exp(-0.5*((x-c)/b)^2)"
                  "s = a/(1 + exp(b (x - c)))"
                  "b = a exp(b (x - c))/(1 + exp(b (x - c)))^2"
                  "o = (y/x) = a (1 - x/b)"
		  "h prefix = homogeneous model (no constant term)"
                  "P prefix = plot result"
		  "' = alg entry, $ = stack, u = Model1, U = Model2")))
     (while (not calc-curve-model)
       (message 
        "Fit to model: %s:%s%s"
        (nth which msgs)
        (if plot "P" " ")
        (if homog "h" ""))
       (setq key (read-char))
       (cond ((= key ?\C-g)
	      (keyboard-quit))
	     ((= key ??)
	      (setq which (% (1+ which) (length msgs))))
	     ((memq key '(?h ?H))
	      (setq homog (not homog)))
             ((= key ?P)
              (if plot
                  (setq plot nil)
                (let ((data (calc-top 1)))
                  (if (or
                       (calc-is-hyperbolic)
                       (calc-is-inverse)
                       (not (= (length data) 3)))
                      (setq plot "Can't plot")
                    (setq plot data)))))
	     ((progn
		(if (eq key ?\$)
		    (setq n 1)
		  (setq n 0))
		(cond ((null arg)
		       (setq n (1+ n)
			     data (calc-top n)))
		      ((or (consp arg) (eq arg 0))
		       (setq n (+ n 2)
			     data (calc-top n)
			     data (if (math-matrixp data)
				      (append data (list (calc-top (1- n))))
				    (list 'vec data (calc-top (1- n))))))
		      ((> (setq arg (prefix-numeric-value arg)) 0)
		       (setq data (cons 'vec (calc-top-list arg (1+ n)))
			     n (+ n arg)))
		      (t (error "Bad prefix argument")))
		(or (math-matrixp data) (not (cdr (cdr data)))
		    (error "Data matrix is not a matrix!"))
		(setq calc-curve-nvars (- (length data) 2)
		      calc-curve-coefnames nil
		      calc-curve-varnames nil)
		nil))
	     ((= key ?1)  ; linear or multilinear
	      (calc-get-fit-variables calc-curve-nvars 
                                      (1+ calc-curve-nvars) (and homog 0))
	      (setq calc-curve-model 
                    (math-mul calc-curve-coefnames
                              (cons 'vec (cons 1 (cdr calc-curve-varnames))))))
	     ((and (>= key ?2) (<= key ?9))   ; polynomial
	      (calc-get-fit-variables 1 (- key ?0 -1) (and homog 0))
	      (setq calc-curve-model 
                    (math-build-polynomial-expr (cdr calc-curve-coefnames)
                                                (nth 1 calc-curve-varnames))))
	     ((= key ?i)  ; exact polynomial
	      (calc-get-fit-variables 1 (1- (length (nth 1 data)))
				      (and homog 0))
	      (setq calc-curve-model 
                    (math-build-polynomial-expr (cdr calc-curve-coefnames)
                                                (nth 1 calc-curve-varnames))))
	     ((= key ?p)  ; power law
	      (calc-get-fit-variables calc-curve-nvars 
                                      (1+ calc-curve-nvars) (and homog 1))
	      (setq calc-curve-model 
                    (math-mul 
                     (nth 1 calc-curve-coefnames)
                     (calcFunc-reduce
                      '(var mul var-mul)
                      (calcFunc-map
                       '(var pow var-pow)
                       calc-curve-varnames
                       (cons 'vec (cdr (cdr calc-curve-coefnames))))))))
	     ((= key ?^)  ; exponential law
	      (calc-get-fit-variables calc-curve-nvars 
                                      (1+ calc-curve-nvars) (and homog 1))
	      (setq calc-curve-model 
                    (math-mul (nth 1 calc-curve-coefnames)
                              (calcFunc-reduce
                               '(var mul var-mul)
                               (calcFunc-map
                                '(var pow var-pow)
                                (cons 'vec (cdr (cdr calc-curve-coefnames)))
                                calc-curve-varnames)))))
             ((= key ?s)
              (setq nonlinear t)
              (setq calc-curve-model t)
              (require 'calc-nlfit)
              (calc-fit-s-shaped-logistic-curve func))
             ((= key ?b)
              (setq nonlinear t)
              (setq calc-curve-model t)
              (require 'calc-nlfit)
              (calc-fit-bell-shaped-logistic-curve func))
             ((= key ?o)
              (setq nonlinear t)
              (setq calc-curve-model t)
              (require 'calc-nlfit)
              (if (and plot (not (stringp plot)))
                  (setq plot
                        (list 'vec
                              (nth 1 plot)
                              (cons
                               'vec
                               (math-map-binop 'calcFunc-div
                                               (cdr (nth 2 plot))
                                               (cdr (nth 1 plot)))))))
              (calc-fit-hubbert-linear-curve func))
	     ((memq key '(?e ?E))
	      (calc-get-fit-variables calc-curve-nvars 
                                      (1+ calc-curve-nvars) (and homog 1))
	      (setq calc-curve-model 
                    (math-mul (nth 1 calc-curve-coefnames)
                              (calcFunc-reduce
                               '(var mul var-mul)
                               (calcFunc-map
                                (if (eq key ?e)
                                    '(var exp var-exp)
                                  '(calcFunc-lambda
                                    (var a var-a)
                                    (^ 10 (var a var-a))))
                                (calcFunc-map
                                 '(var mul var-mul)
                                 (cons 'vec (cdr (cdr calc-curve-coefnames)))
                                 calc-curve-varnames))))))
	     ((memq key '(?x ?X))
	      (calc-get-fit-variables calc-curve-nvars 
                                      (1+ calc-curve-nvars) (and homog 0))
	      (setq calc-curve-model 
                    (math-mul calc-curve-coefnames
                              (cons 'vec (cons 1 (cdr calc-curve-varnames)))))
	      (setq calc-curve-model (if (eq key ?x)
			      (list 'calcFunc-exp calc-curve-model)
			    (list '^ 10 calc-curve-model))))
	     ((memq key '(?l ?L))
	      (calc-get-fit-variables calc-curve-nvars 
                                      (1+ calc-curve-nvars) (and homog 0))
	      (setq calc-curve-model 
                    (math-mul calc-curve-coefnames
                              (cons 'vec
                                    (cons 1 (cdr (calcFunc-map
                                                  (if (eq key ?l)
                                                      '(var ln var-ln)
                                                    '(var log10
                                                          var-log10))
                                                  calc-curve-varnames)))))))
	     ((= key ?q)
	      (calc-get-fit-variables calc-curve-nvars 
                                      (1+ (* 2 calc-curve-nvars)) (and homog 0))
	      (let ((c calc-curve-coefnames)
		    (v calc-curve-varnames))
		(setq calc-curve-model (nth 1 c))
		(while (setq v (cdr v) c (cdr (cdr c)))
		  (setq calc-curve-model (math-add
			       calc-curve-model
			       (list '*
				     (car c)
				     (list '^
					   (list '- (car v) (nth 1 c))
					   2)))))))
	     ((= key ?g)
	      (setq 
               calc-curve-model 
               (math-read-expr 
                "(AFit / BFit sqrt(2 pi)) exp(-0.5 * ((XFit - CFit) / BFit)^2)")
               calc-curve-varnames '(vec (var XFit var-XFit))
               calc-curve-coefnames '(vec (var AFit var-AFit)
                                          (var BFit var-BFit)
                                          (var CFit var-CFit)))
	      (calc-get-fit-variables 1 (1- (length calc-curve-coefnames)) 
                                      (and homog 1)))
	     ((memq key '(?\$ ?\' ?u ?U))
	      (let* ((defvars nil)
		     (record-entry nil))
		(if (eq key ?\')
		    (let* ((calc-dollar-values calc-arg-values)
			   (calc-dollar-used 0)
			   (calc-hashes-used 0))
		      (setq calc-curve-model 
                            (calc-do-alg-entry "" "Model formula: "
                                               nil 'calc-curve-fit-history))
		      (if (/= (length calc-curve-model) 1)
			  (error "Bad format"))
		      (setq calc-curve-model (car calc-curve-model)
			    record-entry t)
		      (if (> calc-dollar-used 0)
			  (setq calc-curve-coefnames
				(cons 'vec
				      (nthcdr (- (length calc-arg-values)
						 calc-dollar-used)
					      (reverse calc-arg-values))))
			(if (> calc-hashes-used 0)
			    (setq calc-curve-coefnames
				  (cons 'vec (calc-invent-args
					      calc-hashes-used))))))
		  (progn
		    (setq calc-curve-model (cond ((eq key ?u)
				       (calc-var-value 'var-Model1))
				      ((eq key ?U)
				       (calc-var-value 'var-Model2))
				      (t (calc-top 1))))
		    (or calc-curve-model (error "User model not yet defined"))
		    (if (math-vectorp calc-curve-model)
			(if (and (memq (length calc-curve-model) '(3 4))
				 (not (math-objvecp (nth 1 calc-curve-model)))
				 (math-vectorp (nth 2 calc-curve-model))
				 (or (null (nth 3 calc-curve-model))
				     (math-vectorp (nth 3 calc-curve-model))))
			    (setq calc-curve-varnames (nth 2 calc-curve-model)
				  calc-curve-coefnames 
                                  (or (nth 3 calc-curve-model)
                                      (cons 'vec
                                            (math-all-vars-but
                                             calc-curve-model 
                                             calc-curve-varnames)))
				  calc-curve-model (nth 1 calc-curve-model))
			  (error "Incorrect model specifier")))))
		(or calc-curve-varnames
		    (let ((with-y 
                           (eq (car-safe calc-curve-model) 'calcFunc-eq)))
		      (if calc-curve-coefnames
			  (calc-get-fit-variables 
                           (if with-y (1+ calc-curve-nvars) calc-curve-nvars)
                           (1- (length calc-curve-coefnames))
                           (math-all-vars-but
                            calc-curve-model calc-curve-coefnames)
                           nil with-y)
			(let* ((coefs (math-all-vars-but calc-curve-model nil))
			       (vars nil)
			       (n (- 
                                   (length coefs) 
                                   calc-curve-nvars 
                                   (if with-y 2 1)))
			       p)
			  (if (< n 0)
			      (error "Not enough variables in model"))
			  (setq p (nthcdr n coefs))
			  (setq vars (cdr p))
			  (setcdr p nil)
			  (calc-get-fit-variables 
                           (if with-y (1+ calc-curve-nvars) calc-curve-nvars)
                           (length coefs)
                           vars coefs with-y)))))
		(if record-entry
		    (calc-record (list 'vec calc-curve-model 
                                       calc-curve-varnames calc-curve-coefnames)
				 "modl"))))
	     (t (beep))))
     (unless nonlinear
       (let ((calc-fit-to-trail t))
         (calc-enter-result n (substring (symbol-name func) 9)
                            (list func calc-curve-model
                                  (if (= (length calc-curve-varnames) 2)
                                      (nth 1 calc-curve-varnames)
                                    calc-curve-varnames)
                                  (if (= (length calc-curve-coefnames) 2)
                                      (nth 1 calc-curve-coefnames)
                                    calc-curve-coefnames)
                                  data))
         (if (consp calc-fit-to-trail)
             (calc-record (calc-normalize calc-fit-to-trail) "parm"))))
  (when plot
    (if (stringp plot)
        (message "%s" plot)
      (let ((calc-graph-no-auto-view t))
        (calc-graph-delete t)
        (calc-graph-add-curve
         (calc-graph-lookup (nth 1 plot))
         (calc-graph-lookup (nth 2 plot)))
        (unless (math-contains-sdev-p (nth 2 data))
          (calc-graph-set-styles nil nil)
          (calc-graph-point-style nil))
        (setq plot (cdr (nth 1 plot)))
        (setq plot 
              (list 'intv
                    3
                    (math-sub
                     (math-min-list (car plot) (cdr plot))
                     '(float 5 -1))
                    (math-add
                     '(float 5 -1)
                     (math-max-list (car plot) (cdr plot)))))
        (calc-graph-add-curve (calc-graph-lookup plot)
                              (calc-graph-lookup (calc-top-n 1)))
        (calc-graph-plot nil)))))))

(defun calc-invent-independent-variables (n &optional but)
  (calc-invent-variables n but '(x y z t) "x"))

(defun calc-invent-parameter-variables (n &optional but)
  (calc-invent-variables n but '(a b c d) "a"))

(defun calc-invent-variables (num but names base)
  (let ((vars nil)
	(n num) (nn 0)
	var)
    (while (and (> n 0) names)
      (setq var (math-build-var-name (if (consp names)
					 (car names)
				       (concat base (int-to-string
						     (setq nn (1+ nn)))))))
      (or (math-expr-contains (cons 'vec but) var)
	  (setq vars (cons var vars)
		n (1- n)))
      (or (symbolp names) (setq names (cdr names))))
    (if (= n 0)
	(nreverse vars)
      (calc-invent-variables num but t base))))

(defun calc-get-fit-variables (nv nc &optional defv defc with-y homog)
  (or (= nv (if with-y (1+ calc-curve-nvars) calc-curve-nvars))
      (error "Wrong number of data vectors for this type of model"))
  (if (integerp defv)
      (setq homog defv
	    defv nil))
  (if homog
      (setq nc (1- nc)))
  (or defv
      (setq defv (calc-invent-independent-variables nv)))
  (or defc
      (setq defc (calc-invent-parameter-variables nc defv)))
  (let ((vars (read-string (format "Fitting variables (default %s; %s): "
				   (mapconcat 'symbol-name
					      (mapcar (function (lambda (v)
								  (nth 1 v)))
						      defv)
					      ",")
				   (mapconcat 'symbol-name
					      (mapcar (function (lambda (v)
								  (nth 1 v)))
						      defc)
					      ","))))
	(coefs nil))
    (setq vars (if (string-match "\\[" vars)
		   (math-read-expr vars)
		 (math-read-expr (concat "[" vars "]"))))
    (if (eq (car-safe vars) 'error)
	(error "Bad format in expression: %s" (nth 2 vars)))
    (or (math-vectorp vars)
	(error "Expected a variable or vector of variables"))
    (if (equal vars '(vec))
	(setq vars (cons 'vec defv)
	      coefs (cons 'vec defc))
      (if (math-vectorp (nth 1 vars))
	  (if (and (= (length vars) 3)
		   (math-vectorp (nth 2 vars)))
	      (setq coefs (nth 2 vars)
		    vars (nth 1 vars))
	    (error
	     "Expected independent variables vector, then parameters vector"))
	(setq coefs (cons 'vec defc))))
    (or (= nv (1- (length vars)))
	(and (not with-y) (= (1+ nv) (1- (length vars))))
	(error "Expected %d independent variable%s" nv (if (= nv 1) "" "s")))
    (or (= nc (1- (length coefs)))
	(error "Expected %d parameter variable%s" nc (if (= nc 1) "" "s")))
    (if homog
	(setq coefs (cons 'vec (cons homog (cdr coefs)))))
    (if calc-curve-varnames
	(setq calc-curve-model (math-multi-subst calc-curve-model (cdr calc-curve-varnames) (cdr vars))))
    (if calc-curve-coefnames
	(setq calc-curve-model (math-multi-subst calc-curve-model (cdr calc-curve-coefnames) (cdr coefs))))
    (setq calc-curve-varnames vars
	  calc-curve-coefnames coefs)))




;;; The following algorithms are from Numerical Recipes chapter 9.

;;; "rtnewt" with safety kludges

(defvar var-DUMMY)

(defun math-newton-root (expr deriv guess orig-guess limit)
  (math-working "newton" guess)
  (let* ((var-DUMMY guess)
	 next dval)
    (setq next (math-evaluate-expr expr)
	  dval (math-evaluate-expr deriv))
    (if (and (Math-numberp next)
	     (Math-numberp dval)
	     (not (Math-zerop dval)))
	(progn
	  (setq next (math-sub guess (math-div next dval)))
	  (if (math-nearly-equal guess (setq next (math-float next)))
	      (progn
		(setq var-DUMMY next)
		(list 'vec next (math-evaluate-expr expr)))
	    (if (Math-lessp (math-abs-approx (math-sub next orig-guess))
			    limit)
		(math-newton-root expr deriv next orig-guess limit)
	      (math-reject-arg next "*Newton's method failed to converge"))))
      (math-reject-arg next "*Newton's method encountered a singularity"))))

;;; Inspired by "rtsafe"
(defun math-newton-search-root (expr deriv guess vguess ostep oostep
				     low vlow high vhigh)
  (let ((var-DUMMY guess)
	(better t)
	pos step next vnext)
    (if guess
	(math-working "newton" (list 'intv 0 low high))
      (math-working "bisect" (list 'intv 0 low high))
      (setq ostep (math-mul-float (math-sub-float high low)
				  '(float 5 -1))
	    guess (math-add-float low ostep)
	    var-DUMMY guess
	    vguess (math-evaluate-expr expr))
      (or (Math-realp vguess)
	  (progn
	    (setq ostep (math-mul-float ostep '(float 6 -1))
		  guess (math-add-float low ostep)
		  var-DUMMY guess
		  vguess (math-evaluate-expr expr))
	    (or (math-realp vguess)
		(progn
		  (setq ostep (math-mul-float ostep '(float 123456 -5))
			guess (math-add-float low ostep)
			var-DUMMY guess
			vguess nil))))))
    (or vguess
	(setq vguess (math-evaluate-expr expr)))
    (or (Math-realp vguess)
	(math-reject-arg guess "*Newton's method encountered a singularity"))
    (setq vguess (math-float vguess))
    (if (eq (Math-negp vlow) (setq pos (Math-posp vguess)))
	(setq high guess
	      vhigh vguess)
      (if (eq (Math-negp vhigh) pos)
	  (setq low guess
		vlow vguess)
	(setq better nil)))
    (if (or (Math-zerop vguess)
	    (math-nearly-equal low high))
	(list 'vec guess vguess)
      (setq step (math-evaluate-expr deriv))
      (if (and (Math-realp step)
	       (not (Math-zerop step))
	       (setq step (math-div-float vguess (math-float step))
		     next (math-sub-float guess step))
	       (not (math-lessp-float high next))
	       (not (math-lessp-float next low)))
	  (progn
	    (setq var-DUMMY next
		  vnext (math-evaluate-expr expr))
	    (if (or (Math-zerop vnext)
		    (math-nearly-equal next guess))
		(list 'vec next vnext)
	      (if (and better
		       (math-lessp-float (math-abs (or oostep
						       (math-sub-float
							high low)))
					 (math-abs
					  (math-mul-float '(float 2 0)
							  step))))
		  (math-newton-search-root expr deriv nil nil nil ostep
					   low vlow high vhigh)
		(math-newton-search-root expr deriv next vnext step ostep
					 low vlow high vhigh))))
	(if (or (and (Math-posp vlow) (Math-posp vhigh))
		(and (Math-negp vlow) (Math-negp vhigh)))
	    (math-search-root expr deriv low vlow high vhigh)
	  (math-newton-search-root expr deriv nil nil nil ostep
				   low vlow high vhigh))))))

;;; Search for a root in an interval with no overt zero crossing.

;; The variable math-root-widen is local to math-find-root, but
;; is used by math-search-root, which is called (directly and
;; indirectly) by math-find-root.
(defvar math-root-widen)

(defun math-search-root (expr deriv low vlow high vhigh)
  (let (found)
    (if math-root-widen
	(let ((iters 0)
	      (iterlim (if (eq math-root-widen 'point)
			   (+ calc-internal-prec 10)
			 20))
	      (factor (if (eq math-root-widen 'point)
			  '(float 9 0)
			'(float 16 -1)))
	      (prev nil) vprev waslow
	      diff)
	  (while (or (and (math-posp vlow) (math-posp vhigh))
		     (and (math-negp vlow) (math-negp vhigh)))
	    (math-working "widen" (list 'intv 0 low high))
	    (if (> (setq iters (1+ iters)) iterlim)
		(math-reject-arg (list 'intv 0 low high)
				 "*Unable to bracket root"))
	    (if (= iters calc-internal-prec)
		(setq factor '(float 16 -1)))
	    (setq diff (math-mul-float (math-sub-float high low) factor))
	    (if (Math-zerop diff)
		(setq high (calcFunc-incr high 10))
	      (if (math-lessp-float (math-abs vlow) (math-abs vhigh))
		  (setq waslow t
			prev low
			low (math-sub low diff)
			var-DUMMY low
			vprev vlow
			vlow (math-evaluate-expr expr))
		(setq waslow nil
		      prev high
		      high (math-add high diff)
		      var-DUMMY high
		      vprev vhigh
		      vhigh (math-evaluate-expr expr)))))
	  (if prev
	      (if waslow
		  (setq high prev vhigh vprev)
		(setq low prev vlow vprev)))
	  (setq found t))
      (or (Math-realp vlow)
	  (math-reject-arg vlow 'realp))
      (or (Math-realp vhigh)
	  (math-reject-arg vhigh 'realp))
      (let ((xvals (list low high))
	    (yvals (list vlow vhigh))
	    (pos (Math-posp vlow))
	    (levels 0)
	    (step (math-sub-float high low))
	    xp yp var-DUMMY)
	(while (and (<= (setq levels (1+ levels)) 5)
		    (not found))
	  (setq xp xvals
		yp yvals
		step (math-mul-float step '(float 497 -3)))
	  (while (and (cdr xp) (not found))
	    (if (Math-realp (car yp))
		(setq low (car xp)
		      vlow (car yp)))
	    (setq high (math-add-float (car xp) step)
		  var-DUMMY high
		  vhigh (math-evaluate-expr expr))
	    (math-working "search" high)
	    (if (and (Math-realp vhigh)
		     (eq (math-negp vhigh) pos))
		(setq found t)
	      (setcdr xp (cons high (cdr xp)))
	      (setcdr yp (cons vhigh (cdr yp)))
	      (setq xp (cdr (cdr xp))
		    yp (cdr (cdr yp))))))))
    (if found
	(if (Math-zerop vhigh)
	    (list 'vec high vhigh)
	  (if (Math-zerop vlow)
	      (list 'vec low vlow)
	    (if deriv
		(math-newton-search-root expr deriv nil nil nil nil
					 low vlow high vhigh)
	      (math-bisect-root expr low vlow high vhigh))))
      (math-reject-arg (list 'intv 3 low high)
		       "*Unable to find a sign change in this interval"))))

;;; "rtbis"  (but we should be using Brent's method)
(defun math-bisect-root (expr low vlow high vhigh)
  (let ((step (math-sub-float high low))
	(pos (Math-posp vhigh))
	var-DUMMY
	mid vmid)
    (while (not (or (math-nearly-equal low
				       (setq step (math-mul-float
						   step '(float 5 -1))
					     mid (math-add-float low step)))
		    (progn
		      (setq var-DUMMY mid
			    vmid (math-evaluate-expr expr))
		      (Math-zerop vmid))))
      (math-working "bisect" mid)
      (if (eq (Math-posp vmid) pos)
	  (setq high mid
		vhigh vmid)
	(setq low mid
	      vlow vmid)))
    (list 'vec mid vmid)))

;;; "mnewt"

(defvar math-root-vars [(var DUMMY var-DUMMY)])

(defun math-newton-multi (expr jacob n guess orig-guess limit)
  (let ((m -1)
	(p guess)
	p2 expr-val jacob-val next)
    (while (< (setq p (cdr p) m (1+ m)) n)
      (set (nth 2 (aref math-root-vars m)) (car p)))
    (setq expr-val (math-evaluate-expr expr)
	  jacob-val (math-evaluate-expr jacob))
    (unless (and (math-constp expr-val)
		 (math-constp jacob-val))
      (math-reject-arg guess "*Newton's method encountered a singularity"))
    (setq next (math-add guess (math-div (math-float (math-neg expr-val))
					 (math-float jacob-val)))
	  p guess p2 next)
    (math-working "newton" next)
    (while (and (setq p (cdr p) p2 (cdr p2))
		(math-nearly-equal (car p) (car p2))))
    (if p
	(if (Math-lessp (math-abs-approx (math-sub next orig-guess))
			limit)
	    (math-newton-multi expr jacob n next orig-guess limit)
	  (math-reject-arg nil "*Newton's method failed to converge"))
      (list 'vec next expr-val))))


(defun math-find-root (expr var guess math-root-widen)
  (if (eq (car-safe expr) 'vec)
      (let ((n (1- (length expr)))
	    (calc-symbolic-mode nil)
	    (var-DUMMY nil)
	    (jacob (list 'vec))
	    p p2 m row)
	(unless (eq (car-safe var) 'vec)
	  (math-reject-arg var 'vectorp))
	(unless (= (length var) (1+ n))
	  (math-dimension-error))
	(setq expr (copy-sequence expr))
	(while (>= n (length math-root-vars))
	  (let ((symb (intern (concat "math-root-v"
				      (int-to-string
				       (length math-root-vars))))))
	    (setq math-root-vars (vconcat math-root-vars
					  (vector (list 'var symb symb))))))
	(setq m -1)
	(while (< (setq m (1+ m)) n)
	  (set (nth 2 (aref math-root-vars m)) nil))
	(setq m -1 p var)
	(while (setq m (1+ m) p (cdr p))
	  (or (eq (car-safe (car p)) 'var)
	      (math-reject-arg var "*Expected a variable"))
	  (setq p2 expr)
	  (while (setq p2 (cdr p2))
	    (setcar p2 (math-expr-subst (car p2) (car p)
					(aref math-root-vars m)))))
	(unless (eq (car-safe guess) 'vec)
	  (math-reject-arg guess 'vectorp))
	(unless (= (length guess) (1+ n))
	  (math-dimension-error))
	(setq guess (copy-sequence guess)
	      p guess)
	(while (setq p (cdr p))
	  (or (Math-numberp (car guess))
	      (math-reject-arg guess 'numberp))
	  (setcar p (math-float (car p))))
	(setq p expr)
	(while (setq p (cdr p))
	  (if (assq (car-safe (car p)) calc-tweak-eqn-table)
	      (setcar p (math-sub (nth 1 (car p)) (nth 2 (car p)))))
	  (setcar p (math-evaluate-expr (car p)))
	  (setq row (list 'vec)
		m -1)
	  (while (< (setq m (1+ m)) n)
	    (nconc row (list (math-evaluate-expr
			      (or (calcFunc-deriv (car p)
						  (aref math-root-vars m)
						  nil t)
				  (math-reject-arg
				   expr
				   "*Formulas must be differentiable"))))))
	  (nconc jacob (list row)))
	(setq m (math-abs-approx guess))
	(math-newton-multi expr jacob n guess guess
			   (if (math-zerop m) '(float 1 3) (math-mul m 10))))
    (unless (eq (car-safe var) 'var)
      (math-reject-arg var "*Expected a variable"))
    (unless (math-expr-contains expr var)
      (math-reject-arg expr "*Formula does not contain specified variable"))
    (if (assq (car expr) calc-tweak-eqn-table)
	(setq expr (math-sub (nth 1 expr) (nth 2 expr))))
    (math-with-extra-prec 2
      (setq expr (math-expr-subst expr var '(var DUMMY var-DUMMY)))
      (let* ((calc-symbolic-mode nil)
	     (var-DUMMY nil)
	     (expr (math-evaluate-expr expr))
	     (deriv (calcFunc-deriv expr '(var DUMMY var-DUMMY) nil t))
	     low high vlow vhigh)
	(and deriv (setq deriv (math-evaluate-expr deriv)))
	(setq guess (math-float guess))
	(if (and (math-numberp guess)
		 deriv)
	    (math-newton-root expr deriv guess guess
			      (if (math-zerop guess) '(float 1 6)
				(math-mul (math-abs-approx guess) 100)))
	  (if (Math-realp guess)
	      (setq low guess
		    high guess
		    var-DUMMY guess
		    vlow (math-evaluate-expr expr)
		    vhigh vlow
		    math-root-widen 'point)
	    (if (eq (car guess) 'intv)
		(progn
		  (or (math-constp guess) (math-reject-arg guess 'constp))
		  (setq low (nth 2 guess)
			high (nth 3 guess))
		  (if (memq (nth 1 guess) '(0 1))
		      (setq low (calcFunc-incr low 1 high)))
		  (if (memq (nth 1 guess) '(0 2))
		      (setq high (calcFunc-incr high -1 low)))
		  (setq var-DUMMY low
			vlow (math-evaluate-expr expr)
			var-DUMMY high
			vhigh (math-evaluate-expr expr)))
	      (if (math-complexp guess)
		  (math-reject-arg "*Complex root finder must have derivative")
		(math-reject-arg guess 'realp))))
	  (if (Math-zerop vlow)
	      (list 'vec low vlow)
	    (if (Math-zerop vhigh)
		(list 'vec high vhigh)
	      (if (and deriv (Math-numberp vlow) (Math-numberp vhigh))
		  (math-newton-search-root expr deriv nil nil nil nil
					   low vlow high vhigh)
		(if (or (and (Math-posp vlow) (Math-posp vhigh))
			(and (Math-negp vlow) (Math-negp vhigh))
			(not (Math-numberp vlow))
			(not (Math-numberp vhigh)))
		    (math-search-root expr deriv low vlow high vhigh)
		  (math-bisect-root expr low vlow high vhigh))))))))))

(defun calcFunc-root (expr var guess)
  (math-find-root expr var guess nil))

(defun calcFunc-wroot (expr var guess)
  (math-find-root expr var guess t))




;;; The following algorithms come from Numerical Recipes, chapter 10.

(defvar math-min-vars [(var DUMMY var-DUMMY)])

(defun math-min-eval (expr a)
  (if (Math-vectorp a)
      (let ((m -1))
	(while (setq m (1+ m) a (cdr a))
	  (set (nth 2 (aref math-min-vars m)) (car a))))
    (setq var-DUMMY a))
  (setq a (math-evaluate-expr expr))
  (if (Math-ratp a)
      (math-float a)
    (if (eq (car a) 'float)
	a
      (math-reject-arg a 'realp))))

(defvar math-min-or-max "minimum")

;;; A bracket for a minimum is a < b < c where f(b) < f(a) and f(b) < f(c).

;;; "mnbrak"
(defun math-widen-min (expr a b)
  (let ((done nil)
	(iters 30)
	incr c va vb vc u vu r q ulim bc ba qr)
    (or b (setq b (math-mul a '(float 101 -2))))
    (setq va (math-min-eval expr a)
	  vb (math-min-eval expr b))
    (if (math-lessp-float va vb)
	(setq u a a b b u
	      vu va va vb vb vu))
    (setq c (math-add-float b (math-mul-float '(float 161803 -5)
					      (math-sub-float b a)))
	  vc (math-min-eval expr c))
    (while (and (not done) (math-lessp-float vc vb))
      (math-working "widen" (list 'intv 0 a c))
      (if (= (setq iters (1- iters)) 0)
	  (math-reject-arg nil (format "*Unable to find a %s near the interval"
				       math-min-or-max)))
      (setq bc (math-sub-float b c)
	    ba (math-sub-float b a)
	    r (math-mul-float ba (math-sub-float vb vc))
	    q (math-mul-float bc (math-sub-float vb va))
	    qr (math-sub-float q r))
      (if (math-lessp-float (math-abs qr) '(float 1 -20))
	  (setq qr (if (math-negp qr) '(float -1 -20) '(float 1 -20))))
      (setq u (math-sub-float
	       b
	       (math-div-float (math-sub-float (math-mul-float bc q)
					       (math-mul-float ba r))
			       (math-mul-float '(float 2 0) qr)))
	    ulim (math-add-float b (math-mul-float '(float -1 2) bc))
	    incr (math-negp bc))
      (if (if incr (math-lessp-float b u) (math-lessp-float u b))
	  (if (if incr (math-lessp-float u c) (math-lessp-float c u))
	      (if (math-lessp-float (setq vu (math-min-eval expr u)) vc)
		  (setq a b  va vb
			b u  vb vu
			done t)
		(if (math-lessp-float vb vu)
		    (setq c u  vc vu
			  done t)
		  (setq u (math-add-float c (math-mul-float '(float -161803 -5)
							    bc))
			vu (math-min-eval expr u))))
	    (if (if incr (math-lessp-float u ulim) (math-lessp-float ulim u))
		(if (math-lessp-float (setq vu (math-min-eval expr u)) vc)
		    (setq b c  vb vc
			  c u  vc vu
			  u (math-add-float c (math-mul-float
					       '(float -161803 -5)
					       (math-sub-float b c)))
			  vu (math-min-eval expr u)))
	      (setq u ulim
		    vu (math-min-eval expr u))))
	(setq u (math-add-float c (math-mul-float '(float -161803 -5)
						  bc))
	      vu (math-min-eval expr u)))
      (setq a b  va vb
	    b c  vb vc
	    c u  vc vu))
    (if (math-lessp-float a c)
	(list a va b vb c vc)
      (list c vc b vb a va))))

(defun math-narrow-min (expr a c intv)
  (let ((xvals (list a c))
	(yvals (list (math-min-eval expr a)
		     (math-min-eval expr c)))
	(levels 0)
	(step (math-sub-float c a))
	(found nil)
	xp yp b)
    (while (and (<= (setq levels (1+ levels)) 5)
		(not found))
      (setq xp xvals
	    yp yvals
	    step (math-mul-float step '(float 497 -3)))
      (while (and (cdr xp) (not found))
	(setq b (math-add-float (car xp) step))
	(math-working "search" b)
	(setcdr xp (cons b (cdr xp)))
	(setcdr yp (cons (math-min-eval expr b) (cdr yp)))
	(if (and (math-lessp-float (nth 1 yp) (car yp))
		 (math-lessp-float (nth 1 yp) (nth 2 yp)))
	    (setq found t)
	  (setq xp (cdr xp)
		yp (cdr yp))
	  (if (and (cdr (cdr yp))
		   (math-lessp-float (nth 1 yp) (car yp))
		   (math-lessp-float (nth 1 yp) (nth 2 yp)))
	      (setq found t)
	    (setq xp (cdr xp)
		  yp (cdr yp))))))
    (if found
	(list (car xp) (car yp)
	      (nth 1 xp) (nth 1 yp)
	      (nth 2 xp) (nth 2 yp))
      (or (if (math-lessp-float (car yvals) (nth 1 yvals))
	      (and (memq (nth 1 intv) '(2 3))
		   (let ((min (car yvals)))
		     (while (and (setq yvals (cdr yvals))
				 (math-lessp-float min (car yvals))))
		     (and (not yvals)
			  (list (nth 2 intv) min))))
	    (and (memq (nth 1 intv) '(1 3))
		 (setq yvals (nreverse yvals))
		 (let ((min (car yvals)))
		   (while (and (setq yvals (cdr yvals))
			       (math-lessp-float min (car yvals))))
		   (and (not yvals)
			(list (nth 3 intv) min)))))
	  (math-reject-arg nil (format "*Unable to find a %s in the interval"
				       math-min-or-max))))))

;;; "brent"
(defun math-brent-min (expr prec a va x vx b vb)
  (let ((iters (+ 20 (* 5 prec)))
	(w x)
	(vw vx)
	(v x)
	(vv vx)
	(tol (list 'float 1 (- -1 prec)))
	(zeps (list 'float 1 (- -5 prec)))
	(e '(float 0 0))
	d u vu xm tol1 tol2 etemp p q r xv xw)
    (while (progn
	     (setq xm (math-mul-float '(float 5 -1)
				      (math-add-float a b))
		   tol1 (math-add-float
			 zeps
			 (math-mul-float tol (math-abs x)))
		   tol2 (math-mul-float tol1 '(float 2 0)))
	     (math-lessp-float (math-sub-float tol2
					       (math-mul-float
						'(float 5 -1)
						(math-sub-float b a)))
			       (math-abs (math-sub-float x xm))))
      (if (= (setq iters (1- iters)) 0)
	  (math-reject-arg nil (format "*Unable to converge on a %s"
				       math-min-or-max)))
      (math-working "brent" x)
      (if (math-lessp-float (math-abs e) tol1)
	  (setq e (if (math-lessp-float x xm)
		      (math-sub-float b x)
		    (math-sub-float a x))
		d (math-mul-float '(float 381966 -6) e))
	(setq xw (math-sub-float x w)
	      r (math-mul-float xw (math-sub-float vx vv))
	      xv (math-sub-float x v)
	      q (math-mul-float xv (math-sub-float vx vw))
	      p (math-sub-float (math-mul-float xv q)
				(math-mul-float xw r))
	      q (math-mul-float '(float 2 0) (math-sub-float q r)))
	(if (math-posp q)
	    (setq p (math-neg-float p))
	  (setq q (math-neg-float q)))
	(setq etemp e
	      e d)
	(if (and (math-lessp-float (math-abs p)
				   (math-abs (math-mul-float
					      '(float 5 -1)
					      (math-mul-float q etemp))))
		 (math-lessp-float (math-mul-float
				    q (math-sub-float a x)) p)
		 (math-lessp-float p (math-mul-float
				      q (math-sub-float b x))))
	    (progn
	      (setq d (math-div-float p q)
		    u (math-add-float x d))
	      (if (or (math-lessp-float (math-sub-float u a) tol2)
		      (math-lessp-float (math-sub-float b u) tol2))
		  (setq d (if (math-lessp-float xm x)
			      (math-neg-float tol1)
			    tol1))))
	  (setq e (if (math-lessp-float x xm)
		      (math-sub-float b x)
		    (math-sub-float a x))
		d (math-mul-float '(float 381966 -6) e))))
      (setq u (math-add-float x
			      (if (math-lessp-float (math-abs d) tol1)
				  (if (math-negp d)
				      (math-neg-float tol1)
				    tol1)
				d))
	    vu (math-min-eval expr u))
      (if (math-lessp-float vx vu)
	  (progn
	    (if (math-lessp-float u x)
		(setq a u)
	      (setq b u))
	    (if (or (equal w x)
		    (not (math-lessp-float vw vu)))
		(setq v w  vv vw
		      w u  vw vu)
	      (if (or (equal v x)
		      (equal v w)
		      (not (math-lessp-float vv vu)))
		  (setq v u  vv vu))))
	(if (math-lessp-float u x)
	    (setq b x)
	  (setq a x))
	(setq v w  vv vw
	      w x  vw vx
	      x u  vx vu)))
    (list 'vec x vx)))

;;; "powell"
(defun math-powell-min (expr n guesses prec)
  (let* ((f1dim (math-line-min-func expr n))
	 (xi (calcFunc-idn 1 n))
	 (p (cons 'vec (mapcar 'car guesses)))
	 (pt p)
	 (ftol (list 'float 1 (- prec)))
	 (fret (math-min-eval expr p))
	 fp ptt fptt xit i ibig del diff res)
    (while (progn
	     (setq fp fret
		   ibig 0
		   del '(float 0 0)
		   i 0)
	     (while (<= (setq i (1+ i)) n)
	       (setq fptt fret
		     res (math-line-min f1dim p
					(math-mat-col xi i)
					n prec)
		     p (let ((calc-internal-prec prec))
			 (math-normalize (car res)))
		     fret (nth 2 res)
		     diff (math-abs (math-sub-float fptt fret)))
	       (if (math-lessp-float del diff)
		   (setq del diff
			 ibig i)))
	     (math-lessp-float
	      (math-mul-float ftol
			      (math-add-float (math-abs fp)
					      (math-abs fret)))
	      (math-mul-float '(float 2 0)
			      (math-abs (math-sub-float fp
							fret)))))
      (setq ptt (math-sub (math-mul '(float 2 0) p) pt)
	    xit (math-sub p pt)
	    pt p
	    fptt (math-min-eval expr ptt))
      (if (and (math-lessp-float fptt fp)
	       (math-lessp-float
		(math-mul-float
		 (math-mul-float '(float 2 0)
				 (math-add-float
				  (math-sub-float fp
						  (math-mul-float '(float 2 0)
								  fret))
				  fptt))
		 (math-sqr-float (math-sub-float
				  (math-sub-float fp fret) del)))
		(math-mul-float del
				(math-sqr-float (math-sub-float fp fptt)))))
	  (progn
	    (setq res (math-line-min f1dim p xit n prec)
		  p (car res)
		  fret (nth 2 res)
		  i 0)
	    (while (<= (setq i (1+ i)) n)
	      (setcar (nthcdr ibig (nth i xi))
		      (nth i (nth 1 res)))))))
    (list 'vec p fret)))

(defun math-line-min-func (expr n)
  (let ((m -1))
    (while (< (setq m (1+ m)) n)
      (set (nth 2 (aref math-min-vars m))
	   (list '+
		 (list '*
		       '(var DUMMY var-DUMMY)
		       (list 'calcFunc-mrow '(var line-xi line-xi) (1+ m)))
		 (list 'calcFunc-mrow '(var line-p line-p) (1+ m)))))
    (math-evaluate-expr expr)))

(defun math-line-min (f1dim line-p line-xi n prec)
  (let* ((var-DUMMY nil)
	 (expr (math-evaluate-expr f1dim))
	 (params (math-widen-min expr '(float 0 0) '(float 1 0)))
	 (res (apply 'math-brent-min expr prec params))
	 (xi (math-mul (nth 1 res) line-xi)))
    (list (math-add line-p xi) xi (nth 2 res))))


(defun math-find-minimum (expr var guess min-widen)
  (let* ((calc-symbolic-mode nil)
	 (n 0)
	 (var-DUMMY nil)
	 (isvec (math-vectorp var))
	 g guesses)
    (or (math-vectorp var)
	(setq var (list 'vec var)))
    (or (math-vectorp guess)
	(setq guess (list 'vec guess)))
    (or (= (length var) (length guess))
	(math-dimension-error))
    (while (setq var (cdr var) guess (cdr guess))
      (or (eq (car-safe (car var)) 'var)
	  (math-reject-arg (car var) "*Expected a variable"))
      (or (math-expr-contains expr (car var))
	  (math-reject-arg (car var)
			   "*Formula does not contain specified variable"))
      (while (>= (1+ n) (length math-min-vars))
	(let ((symb (intern (concat "math-min-v"
				    (int-to-string
				     (length math-min-vars))))))
	  (setq math-min-vars (vconcat math-min-vars
				       (vector (list 'var symb symb))))))
      (set (nth 2 (aref math-min-vars n)) nil)
      (set (nth 2 (aref math-min-vars (1+ n))) nil)
      (if (math-complexp (car guess))
	  (setq expr (math-expr-subst expr
				      (car var)
				      (list '+ (aref math-min-vars n)
					    (list '*
						  (aref math-min-vars (1+ n))
						  '(cplx 0 1))))
		guesses (let ((g (math-float (math-complex (car guess)))))
			  (cons (list (nth 2 g) nil nil)
				(cons (list (nth 1 g) nil nil t)
				      guesses)))
		n (+ n 2))
	(setq expr (math-expr-subst expr
				    (car var)
				    (aref math-min-vars n))
	      guesses (cons (if (math-realp (car guess))
				(list (math-float (car guess)) nil nil)
			      (if (and (eq (car-safe (car guess)) 'intv)
				       (math-constp (car guess)))
				  (list (math-mul
					 (math-add (nth 2 (car guess))
						   (nth 3 (car guess)))
					 '(float 5 -1))
					(math-float (nth 2 (car guess)))
					(math-float (nth 3 (car guess)))
					(car guess))
				(math-reject-arg (car guess) 'realp)))
			    guesses)
	      n (1+ n))))
    (setq guesses (nreverse guesses)
	  expr (math-evaluate-expr expr))
    (if (= n 1)
	(let* ((params (if (nth 1 (car guesses))
			   (if min-widen
			       (math-widen-min expr
					       (nth 1 (car guesses))
					       (nth 2 (car guesses)))
			     (math-narrow-min expr
					      (nth 1 (car guesses))
					      (nth 2 (car guesses))
					      (nth 3 (car guesses))))
			 (math-widen-min expr
					 (car (car guesses))
					 nil)))
	       (prec calc-internal-prec)
	       (res (if (cdr (cdr params))
			(math-with-extra-prec (+ calc-internal-prec 2)
			  (apply 'math-brent-min expr prec params))
		      (cons 'vec params))))
	  (if isvec
	      (list 'vec (list 'vec (nth 1 res)) (nth 2 res))
	    res))
      (let* ((prec calc-internal-prec)
	     (res (math-with-extra-prec (+ calc-internal-prec 2)
		    (math-powell-min expr n guesses prec)))
	     (p (nth 1 res))
	     (vec (list 'vec)))
	(while (setq p (cdr p))
	  (if (nth 3 (car guesses))
	      (progn
		(nconc vec (list (math-normalize
				  (list 'cplx (car p) (nth 1 p)))))
		(setq p (cdr p)
		      guesses (cdr guesses)))
	    (nconc vec (list (car p))))
	  (setq guesses (cdr guesses)))
	(if isvec
	    (list 'vec vec (nth 2 res))
	  (list 'vec (nth 1 vec) (nth 2 res)))))))

(defun calcFunc-minimize (expr var guess)
  (let ((calc-internal-prec (max (/ calc-internal-prec 2) 3))
	(math-min-or-max "minimum"))
    (math-find-minimum (math-normalize expr)
		       (math-normalize var)
		       (math-normalize guess) nil)))

(defun calcFunc-wminimize (expr var guess)
  (let ((calc-internal-prec (max (/ calc-internal-prec 2) 3))
	(math-min-or-max "minimum"))
    (math-find-minimum (math-normalize expr)
		       (math-normalize var)
		       (math-normalize guess) t)))

(defun calcFunc-maximize (expr var guess)
  (let* ((calc-internal-prec (max (/ calc-internal-prec 2) 3))
	 (math-min-or-max "maximum")
	 (res (math-find-minimum (math-normalize (math-neg expr))
				 (math-normalize var)
				 (math-normalize guess) nil)))
    (list 'vec (nth 1 res) (math-neg (nth 2 res)))))

(defun calcFunc-wmaximize (expr var guess)
  (let* ((calc-internal-prec (max (/ calc-internal-prec 2) 3))
	 (math-min-or-max "maximum")
	 (res (math-find-minimum (math-normalize (math-neg expr))
				 (math-normalize var)
				 (math-normalize guess) t)))
    (list 'vec (nth 1 res) (math-neg (nth 2 res)))))




;;; The following algorithms come from Numerical Recipes, chapter 3.

(defun calcFunc-polint (data x)
  (or (math-matrixp data) (math-reject-arg data 'matrixp))
  (or (= (length data) 3)
      (math-reject-arg data "*Wrong number of data rows"))
  (or (> (length (nth 1 data)) 2)
      (math-reject-arg data "*Too few data points"))
  (if (and (math-vectorp x) (or (math-constp x) math-expand-formulas))
      (cons 'vec (mapcar (function (lambda (x) (calcFunc-polint data x)))
			 (cdr x)))
    (or (math-objectp x) math-expand-formulas (math-reject-arg x 'objectp))
    (math-with-extra-prec 2
      (cons 'vec (math-poly-interp (cdr (nth 1 data)) (cdr (nth 2 data)) x
				   nil)))))
(put 'calcFunc-polint 'math-expandable t)


(defun calcFunc-ratint (data x)
  (or (math-matrixp data) (math-reject-arg data 'matrixp))
  (or (= (length data) 3)
      (math-reject-arg data "*Wrong number of data rows"))
  (or (> (length (nth 1 data)) 2)
      (math-reject-arg data "*Too few data points"))
  (if (and (math-vectorp x) (or (math-constp x) math-expand-formulas))
      (cons 'vec (mapcar (function (lambda (x) (calcFunc-ratint data x)))
			 (cdr x)))
    (or (math-objectp x) math-expand-formulas (math-reject-arg x 'objectp))
    (math-with-extra-prec 2
      (cons 'vec (math-poly-interp (cdr (nth 1 data)) (cdr (nth 2 data)) x
				   (cdr (cdr (cdr (nth 1 data)))))))))
(put 'calcFunc-ratint 'math-expandable t)


(defun math-poly-interp (xa ya x ratp)
  (let ((n (length xa))
	(dif nil)
	(ns nil)
	(xax nil)
	(c (copy-sequence ya))
	(d (copy-sequence ya))
	(i 0)
	(m 0)
	y dy (xp xa) xpm cp dp temp)
    (while (<= (setq i (1+ i)) n)
      (setq xax (cons (math-sub (car xp) x) xax)
	    xp (cdr xp)
	    temp (math-abs (car xax)))
      (if (or (null dif) (math-lessp temp dif))
	  (setq dif temp
		ns i)))
    (setq xax (nreverse xax)
	  ns (1- ns)
	  y (nth ns ya))
    (if (math-zerop dif)
	(list y 0)
      (while (< (setq m (1+ m)) n)
	(setq i 0
	      xp xax
	      xpm (nthcdr m xax)
	      cp c
	      dp d)
	(while (<= (setq i (1+ i)) (- n m))
	  (if ratp
	      (let ((t2 (math-div (math-mul (car xp) (car dp)) (car xpm))))
		(setq temp (math-div (math-sub (nth 1 cp) (car dp))
				     (math-sub t2 (nth 1 cp))))
		(setcar dp (math-mul (nth 1 cp) temp))
		(setcar cp (math-mul t2 temp)))
	    (if (math-equal (car xp) (car xpm))
		(math-reject-arg (cons 'vec xa) "*Duplicate X values"))
	    (setq temp (math-div (math-sub (nth 1 cp) (car dp))
				 (math-sub (car xp) (car xpm))))
	    (setcar dp (math-mul (car xpm) temp))
	    (setcar cp (math-mul (car xp) temp)))
	  (setq cp (cdr cp)
		dp (cdr dp)
		xp (cdr xp)
		xpm (cdr xpm)))
	(if (< (+ ns ns) (- n m))
	    (setq dy (nth ns c))
	  (setq ns (1- ns)
		dy (nth ns d)))
	(setq y (math-add y dy)))
      (list y dy))))



;;; The following algorithms come from Numerical Recipes, chapter 4.

(defun calcFunc-ninteg (expr var lo hi)
  (setq lo (math-evaluate-expr lo)
	hi (math-evaluate-expr hi))
  (or (math-numberp lo) (math-infinitep lo) (math-reject-arg lo 'numberp))
  (or (math-numberp hi) (math-infinitep hi) (math-reject-arg hi 'numberp))
  (if (math-lessp hi lo)
      (math-neg (calcFunc-ninteg expr var hi lo))
    (setq expr (math-expr-subst expr var '(var DUMMY var-DUMMY)))
    (let ((var-DUMMY nil)
	  (calc-symbolic-mode nil)
	  (calc-prefer-frac nil)
	  (sum 0))
      (setq expr (math-evaluate-expr expr))
      (if (equal lo '(neg (var inf var-inf)))
	  (let ((thi (if (math-lessp hi '(float -2 0))
			 hi '(float -2 0))))
	    (setq sum (math-ninteg-romberg
		       'math-ninteg-midpoint expr
			 (math-float lo) (math-float thi) 'inf)
		  lo thi)))
      (if (equal hi '(var inf var-inf))
	  (let ((tlo (if (math-lessp '(float 2 0) lo)
			 lo '(float 2 0))))
	    (setq sum (math-add sum
				(math-ninteg-romberg
				 'math-ninteg-midpoint expr
				 (math-float tlo) (math-float hi) 'inf))
		  hi tlo)))
      (or (math-equal lo hi)
	  (setq sum (math-add sum
			      (math-ninteg-romberg
			       'math-ninteg-midpoint expr
			       (math-float lo) (math-float hi) nil))))
      sum)))


;;; Open Romberg method; "qromo" in section 4.4.

;; The variable math-ninteg-temp is local to math-ninteg-romberg,
;; but is used by math-ninteg-midpoint, which is used by 
;; math-ninteg-romberg.
(defvar math-ninteg-temp)

(defun math-ninteg-romberg (func expr lo hi mode)
  (let ((curh '(float 1 0))
	(h nil)
	(s nil)
	(j 0)
	(ss nil)
	(prec calc-internal-prec)
	(math-ninteg-temp nil))
    (math-with-extra-prec 2
      ;; Limit on "j" loop must be 14 or less to keep "it" from overflowing.
      (or (while (and (null ss) (<= (setq j (1+ j)) 8))
	    (setq s (nconc s (list (funcall func expr lo hi mode)))
		  h (nconc h (list curh)))
	    (if (>= j 3)
		(let ((res (math-poly-interp h s '(float 0 0) nil)))
		  (if (math-lessp (math-abs (nth 1 res))
				  (calcFunc-scf (math-abs (car res))
						(- prec)))
		      (setq ss (car res)))))
	    (if (>= j 5)
		(setq s (cdr s)
		      h (cdr h)))
	    (setq curh (math-div-float curh '(float 9 0))))
	  ss
	  (math-reject-arg nil (format "*Integral failed to converge"))))))


(defun math-ninteg-evaluate (expr x mode)
  (if (eq mode 'inf)
      (setq x (math-div '(float 1 0) x)))
  (let* ((var-DUMMY x)
	 (res (math-evaluate-expr expr)))
    (or (Math-numberp res)
	(math-reject-arg res "*Integrand does not evaluate to a number"))
    (if (eq mode 'inf)
	(setq res (math-mul res (math-sqr x))))
    res))


(defun math-ninteg-midpoint (expr lo hi mode)    ; uses "math-ninteg-temp"
  (if (eq mode 'inf)
      (let ((math-infinite-mode t) temp)
	(setq temp (math-div 1 lo)
	      lo (math-div 1 hi)
	      hi temp)))
  (if math-ninteg-temp
      (let* ((it3 (* 3 (car math-ninteg-temp)))
	     (math-working-step-2 (* 2 (car math-ninteg-temp)))
	     (math-working-step 0)
	     (range (math-sub hi lo))
	     (del (math-div range (math-float it3)))
	     (del2 (math-add del del))
	     (del3 (math-add del del2))
	     (x (math-add lo (math-mul '(float 5 -1) del)))
	     (sum '(float 0 0))
	     (j 0) temp)
	(while (<= (setq j (1+ j)) (car math-ninteg-temp))
	  (setq math-working-step (1+ math-working-step)
		temp (math-ninteg-evaluate expr x mode)
		math-working-step (1+ math-working-step)
		sum (math-add sum (math-add temp (math-ninteg-evaluate
						  expr (math-add x del2)
						  mode)))
		x (math-add x del3)))
	(setq math-ninteg-temp (list it3
                                     (math-add (math-div (nth 1 math-ninteg-temp)
                                                         '(float 3 0))
                                               (math-mul sum del)))))
    (setq math-ninteg-temp (list 1 (math-mul
                                    (math-sub hi lo)
                                    (math-ninteg-evaluate
                                     expr
                                     (math-mul (math-add lo hi) '(float 5 -1))
                                     mode)))))
  (nth 1 math-ninteg-temp))





;;; The following algorithms come from Numerical Recipes, chapter 14.

(defvar math-dummy-vars [(var DUMMY var-DUMMY)])
(defvar math-dummy-counter 0)
(defun math-dummy-variable ()
  (if (= math-dummy-counter (length math-dummy-vars))
      (let ((symb (intern (format "math-dummy-%d" math-dummy-counter))))
	(setq math-dummy-vars (vconcat math-dummy-vars
				       (vector (list 'var symb symb))))))
  (set (nth 2 (aref math-dummy-vars math-dummy-counter)) nil)
  (prog1
      (aref math-dummy-vars math-dummy-counter)
    (setq math-dummy-counter (1+ math-dummy-counter))))

(defvar math-in-fit 0)
(defvar calc-fit-to-trail nil)

(defun calcFunc-fit (expr vars &optional coefs data)
  (let ((math-in-fit 10))
    (math-with-extra-prec 2
      (math-general-fit expr vars coefs data nil))))

(defun calcFunc-efit (expr vars &optional coefs data)
  (let ((math-in-fit 10))
    (math-with-extra-prec 2
      (math-general-fit expr vars coefs data 'sdev))))

(defun calcFunc-xfit (expr vars &optional coefs data)
  (let ((math-in-fit 10))
    (math-with-extra-prec 2
      (math-general-fit expr vars coefs data 'full))))

;; The variables math-fit-first-var, math-fit-first-coef and
;; math-fit-new-coefs are local to math-general-fit, but are used by
;; calcFunc-fitvar, calcFunc-fitparam and calcFunc-fitdummy 
;; (respectively), which are used by math-general-fit.
(defvar math-fit-first-var)
(defvar math-fit-first-coef)
(defvar math-fit-new-coefs)

(defun math-general-fit (expr vars coefs data mode)
  (let ((calc-simplify-mode nil)
	(math-dummy-counter math-dummy-counter)
	(math-in-fit 1)
	(extended (eq mode 'full))
	(math-fit-first-coef math-dummy-counter)
	math-fit-first-var
	(plain-expr expr)
	orig-expr
	have-sdevs need-chisq chisq
	(x-funcs nil)
	(y-filter nil)
	y-dummy
	(coef-filters nil)
	math-fit-new-coefs
	(xy-values nil)
	(weights nil)
	(var-YVAL nil) (var-YVALX nil)
	covar beta
	n nn m mm v dummy p)

    ;; Validate and parse arguments.
    (or data
	(if coefs
	    (setq data coefs
		  coefs nil)
	  (if (math-vectorp expr)
	      (if (memq (length expr) '(3 4))
		  (setq data vars
			vars (nth 2 expr)
			coefs (nth 3 expr)
			expr (nth 1 expr))
		(math-dimension-error))
	    (setq data vars
		  vars nil
		  coefs nil))))
    (or (math-matrixp data) (math-reject-arg data 'matrixp))
    (setq v (1- (length data))
	  n (1- (length (nth 1 data))))
    (or (math-vectorp vars) (null vars)
	(setq vars (list 'vec vars)))
    (or (math-vectorp coefs) (null coefs)
	(setq coefs (list 'vec coefs)))
    (or coefs
	(setq coefs (cons 'vec (math-all-vars-but expr vars))))
    (or vars
	(if (<= (1- (length coefs)) v)
	    (math-reject-arg coefs "*Not enough variables in model")
	  (setq coefs (copy-sequence coefs))
	  (let ((p (nthcdr (- (length coefs) v
			      (if (eq (car-safe expr) 'calcFunc-eq) 1 0))
			   coefs)))
	    (setq vars (cons 'vec (cdr p)))
	    (setcdr p nil))))
    (or (= (1- (length vars)) v)
	(= (length vars) v)
	(math-reject-arg vars "*Number of variables does not match data"))
    (setq m (1- (length coefs)))
    (if (< m 1)
	(math-reject-arg coefs "*Need at least one parameter"))

    ;; Rewrite expr in terms of fitparam and fitvar, make into an equation.
    (setq p coefs)
    (while (setq p (cdr p))
      (or (eq (car-safe (car p)) 'var)
	  (math-reject-arg (car p) "*Expected a variable"))
      (setq dummy (math-dummy-variable)
	    expr (math-expr-subst expr (car p)
				  (list 'calcFunc-fitparam
					(- math-dummy-counter math-fit-first-coef)))))
    (setq math-fit-first-var math-dummy-counter
	  p vars)
    (while (setq p (cdr p))
      (or (eq (car-safe (car p)) 'var)
	  (math-reject-arg (car p) "*Expected a variable"))
      (setq dummy (math-dummy-variable)
	    expr (math-expr-subst expr (car p)
				  (list 'calcFunc-fitvar
					(- math-dummy-counter math-fit-first-var)))))
    (if (< math-dummy-counter (+ math-fit-first-var v))
	(setq dummy (math-dummy-variable))) ; dependent variable may be unnamed
    (setq y-dummy dummy
	  orig-expr expr)
    (or (eq (car-safe expr) 'calcFunc-eq)
	(setq expr (list 'calcFunc-eq (list 'calcFunc-fitvar v) expr)))

    (let ((calc-symbolic-mode nil))

      ;; Apply rewrites to put expr into a linear-like form.
      (setq expr (math-evaluate-expr expr)
	    expr (math-rewrite (list 'calcFunc-fitmodel expr)
			       '(var FitRules var-FitRules))
	    math-in-fit 2
	    expr (math-evaluate-expr expr))
      (or (and (eq (car-safe expr) 'calcFunc-fitsystem)
	       (= (length expr) 4)
	       (math-vectorp (nth 2 expr))
	       (math-vectorp (nth 3 expr))
	       (> (length (nth 2 expr)) 1)
	       (= (length (nth 3 expr)) (1+ m)))
	  (math-reject-arg plain-expr "*Model expression is too complex"))
      (setq y-filter (nth 1 expr)
	    x-funcs (vconcat (cdr (nth 2 expr)))
	    coef-filters (nth 3 expr)
	    mm (length x-funcs))
      (if (equal y-filter y-dummy)
	  (setq y-filter nil))

      ;; Build the (square) system of linear equations to be solved.
      (setq beta (cons 'vec (make-list mm 0))
	    covar (cons 'vec (mapcar 'copy-sequence (make-list mm beta))))
      (let* ((ptrs (vconcat (cdr data)))
	     (isigsq 1)
	     (xvals (make-vector mm 0))
	     (i 0)
	     j k xval yval sigmasqr wt covj covjk covk betaj lud)
	(while (<= (setq i (1+ i)) n)

	  ;; Assign various independent variables for this data point.
	  (setq j 0
		sigmasqr nil)
	  (while (< j v)
	    (aset ptrs j (cdr (aref ptrs j)))
	    (setq xval (car (aref ptrs j)))
	    (if (= j (1- v))
		(if sigmasqr
		    (progn
		      (if (eq (car-safe xval) 'sdev)
			  (setq sigmasqr (math-add (math-sqr (nth 2 xval))
						   sigmasqr)
				xval (nth 1 xval)))
		      (if y-filter
			  (setq xval (math-make-sdev xval
						     (math-sqrt sigmasqr))))))
	      (if (eq (car-safe xval) 'sdev)
		  (setq sigmasqr (math-add (math-sqr (nth 2 xval))
					   (or sigmasqr 0))
			xval (nth 1 xval))))
	    (set (nth 2 (aref math-dummy-vars (+ math-fit-first-var j))) xval)
	    (setq j (1+ j)))

	  ;; Compute Y value for this data point.
	  (if y-filter
	      (setq yval (math-evaluate-expr y-filter))
	    (setq yval (symbol-value (nth 2 y-dummy))))
	  (if (eq (car-safe yval) 'sdev)
	      (setq sigmasqr (math-sqr (nth 2 yval))
		    yval (nth 1 yval)))
	  (if (= i 1)
	      (setq have-sdevs sigmasqr
		    need-chisq (or extended
				   (and (eq mode 'sdev) (not have-sdevs)))))
	  (if have-sdevs
	      (if sigmasqr
		  (progn
		    (setq isigsq (math-div 1 sigmasqr))
		    (if need-chisq
			(setq weights (cons isigsq weights))))
		(math-reject-arg yval "*Mixed error forms and plain numbers"))
	    (if sigmasqr
		(math-reject-arg yval "*Mixed error forms and plain numbers")))

	  ;; Compute X values for this data point and update covar and beta.
	  (if (eq (car-safe xval) 'sdev)
	      (set (nth 2 y-dummy) (nth 1 xval)))
	  (setq j 0
		covj covar
		betaj beta)
	  (while (< j mm)
	    (setq wt (math-evaluate-expr (aref x-funcs j)))
	    (aset xvals j wt)
	    (setq wt (math-mul wt isigsq)
		  betaj (cdr betaj)
		  covjk (car (setq covj (cdr covj)))
		  k 0)
	    (while (<= k j)
	      (setq covjk (cdr covjk))
	      (setcar covjk (math-add (car covjk)
				      (math-mul wt (aref xvals k))))
	      (setq k (1+ k)))
	    (setcar betaj (math-add (car betaj) (math-mul wt yval)))
	    (setq j (1+ j)))
	  (if need-chisq
	      (setq xy-values (cons (append xvals (list yval)) xy-values))))

	;; Fill in symmetric half of covar matrix.
	(setq j 0
	      covj covar)
	(while (< j (1- mm))
	  (setq k j
		j (1+ j)
		covjk (nthcdr j (car (setq covj (cdr covj))))
		covk (nthcdr j covar))
	  (while (< (setq k (1+ k)) mm)
	    (setq covjk (cdr covjk)
		  covk (cdr covk))
	    (setcar covjk (nth j (car covk))))))

      ;; Solve the linear system.
      (if mode
	  (progn
	    (setq covar (math-matrix-inv-raw covar))
	    (if covar
		(setq beta (math-mul covar beta))
	      (if (math-zerop (math-abs beta))
		  (setq covar (calcFunc-diag 0 (1- (length beta))))
		(math-reject-arg orig-expr "*Singular matrix")))
	    (or (math-vectorp covar)
		(setq covar (list 'vec (list 'vec covar)))))
	(setq beta (math-div beta covar)))

      ;; Compute chi-square statistic if necessary.
      (if need-chisq
	  (let (bp xp sum)
	    (setq chisq 0)
	    (while xy-values
	      (setq bp beta
		    xp (car xy-values)
		    sum 0)
	      (while (setq bp (cdr bp))
		(setq sum (math-add sum (math-mul (car bp) (car xp)))
		      xp (cdr xp)))
	      (setq sum (math-sqr (math-sub (car xp) sum)))
	      (if weights (setq sum (math-mul sum (car weights))))
	      (setq chisq (math-add chisq sum)
		    weights (cdr weights)
		    xy-values (cdr xy-values)))))

      ;; Convert coefficients back into original terms.
      (setq math-fit-new-coefs (copy-sequence beta))
      (let* ((bp math-fit-new-coefs)
	     (cp covar)
	     (sigdat 1)
	     (math-in-fit 3)
	     (j 0))
	(and mode (not have-sdevs)
	     (setq sigdat (if (<= n mm)
			      0
			    (math-div chisq (- n mm)))))
	(if mode
	    (while (setq bp (cdr bp))
	      (setcar bp (math-make-sdev
			  (car bp)
			  (math-sqrt (math-mul (nth (setq j (1+ j))
						    (car (setq cp (cdr cp))))
					       sigdat))))))
	(setq math-fit-new-coefs (math-evaluate-expr coef-filters))
	(if calc-fit-to-trail
	    (let ((bp math-fit-new-coefs)
		  (cp coefs)
		  (vec nil))
	      (while (setq bp (cdr bp) cp (cdr cp))
		(setq vec (cons (list 'calcFunc-eq (car cp) (car bp)) vec)))
	      (setq calc-fit-to-trail (cons 'vec (nreverse vec)))))))

    ;; Substitute best-fit coefficients back into original formula.
    (setq expr (math-multi-subst
		orig-expr
		(let ((n v)
		      (vec nil))
		  (while (>= n 1)
		    (setq vec (cons (list 'calcFunc-fitvar n) vec)
			  n (1- n)))
		  (setq n m)
		  (while (>= n 1)
		    (setq vec (cons (list 'calcFunc-fitparam n) vec)
			  n (1- n)))
		  vec)
		(append (cdr math-fit-new-coefs) (cdr vars))))

    ;; Package the result.
    (math-normalize
     (if extended
	 (list 'vec expr beta covar
	       (let ((p coef-filters)
		     (n 0))
		 (while (and (setq n (1+ n) p (cdr p))
			     (eq (car-safe (car p)) 'calcFunc-fitdummy)
			     (eq (nth 1 (car p)) n)))
		 (if p
		     coef-filters
		   (list 'vec)))
	       chisq
	       (if (and have-sdevs (> n mm))
		   (list 'calcFunc-utpc chisq (- n mm))
		 '(var nan var-nan)))
       expr))))


(defun calcFunc-fitvar (x)
  (if (>= math-in-fit 2)
      (progn
	(setq x (aref math-dummy-vars (+ math-fit-first-var x -1)))
	(or (calc-var-value (nth 2 x)) x))
    (math-reject-arg x)))

(defun calcFunc-fitparam (x)
  (if (>= math-in-fit 2)
      (progn
	(setq x (aref math-dummy-vars (+ math-fit-first-coef x -1)))
	(or (calc-var-value (nth 2 x)) x))
    (math-reject-arg x)))

(defun calcFunc-fitdummy (x)
  (if (= math-in-fit 3)
      (nth x math-fit-new-coefs)
    (math-reject-arg x)))

(defun calcFunc-hasfitvars (expr)
  (if (Math-primp expr)
      0
    (if (eq (car expr) 'calcFunc-fitvar)
	(nth 1 expr)
      (apply 'max (mapcar 'calcFunc-hasfitvars (cdr expr))))))

(defun calcFunc-hasfitparams (expr)
  (if (Math-primp expr)
      0
    (if (eq (car expr) 'calcFunc-fitparam)
	(nth 1 expr)
      (apply 'max (mapcar 'calcFunc-hasfitparams (cdr expr))))))


(defun math-all-vars-but (expr but)
  (let* ((vars (math-all-vars-in expr))
	 (p but))
    (while p
      (setq vars (delq (assoc (car-safe p) vars) vars)
	    p (cdr p)))
    (sort (mapcar 'car vars)
	  (function (lambda (x y) (string< (nth 1 x) (nth 1 y)))))))

;; The variables math-all-vars-vars (the vars for math-all-vars) and
;; math-all-vars-found are local to math-all-vars-in, but are used by 
;; math-all-vars-rec which is called by math-all-vars-in.
(defvar math-all-vars-vars)
(defvar math-all-vars-found)

(defun math-all-vars-in (expr)
  (let ((math-all-vars-vars nil)
	math-all-vars-found)
    (math-all-vars-rec expr)
    math-all-vars-vars))

(defun math-all-vars-rec (expr)
  (if (Math-primp expr)
      (if (eq (car-safe expr) 'var)
	  (or (math-const-var expr)
	      (if (setq math-all-vars-found (assoc expr math-all-vars-vars))
		  (setcdr math-all-vars-found (1+ (cdr math-all-vars-found)))
		(setq math-all-vars-vars (cons (cons expr 1) math-all-vars-vars)))))
    (while (setq expr (cdr expr))
      (math-all-vars-rec (car expr)))))

(provide 'calcalg3)

;;; calcalg3.el ends here
