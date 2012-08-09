;;; calc-stat.el --- statistical functions for Calc

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

;;; Statistical operations on vectors.

(defun calc-vector-count (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-vector-op "coun" 'calcFunc-vcount arg)))

(defun calc-vector-sum (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-vector-op "vprd" 'calcFunc-vprod arg)
     (calc-vector-op "vsum" 'calcFunc-vsum arg))))

(defun calc-vector-product (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-vector-sum arg))

(defun calc-vector-max (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-vector-op "vmin" 'calcFunc-vmin arg)
     (calc-vector-op "vmax" 'calcFunc-vmax arg))))

(defun calc-vector-min (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-vector-max arg))

(defun calc-vector-mean (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (if (calc-is-inverse)
	   (calc-vector-op "harm" 'calcFunc-vhmean arg)
	 (calc-vector-op "medn" 'calcFunc-vmedian arg))
     (if (calc-is-inverse)
	 (calc-vector-op "meae" 'calcFunc-vmeane arg)
       (calc-vector-op "mean" 'calcFunc-vmean arg)))))

(defun calc-vector-mean-error (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-vector-mean arg))

(defun calc-vector-median (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-vector-mean arg))

(defun calc-vector-harmonic-mean (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-hyperbolic-func)
  (calc-vector-mean arg))

(defun calc-vector-geometric-mean (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-binary-op "geom" 'calcFunc-agmean arg)
     (calc-vector-op "geom" 'calcFunc-vgmean arg))))

(defun calc-vector-sdev (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (if (calc-is-inverse)
	   (calc-vector-op "pvar" 'calcFunc-vpvar arg)
	 (calc-vector-op "var" 'calcFunc-vvar arg))
     (if (calc-is-inverse)
	 (calc-vector-op "psdv" 'calcFunc-vpsdev arg)
       (calc-vector-op "sdev" 'calcFunc-vsdev arg)))))

(defun calc-vector-pop-sdev (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-vector-sdev arg))

(defun calc-vector-variance (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-vector-sdev arg))

(defun calc-vector-pop-variance (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-hyperbolic-func)
  (calc-vector-sdev arg))

(defun calc-vector-covariance (arg)
  (interactive "P")
  (calc-slow-wrapper
   (let ((n (if (eq arg 1) 1 2)))
     (if (calc-is-hyperbolic)
	 (calc-enter-result n "corr" (cons 'calcFunc-vcorr
					   (calc-top-list-n n)))
       (if (calc-is-inverse)
	   (calc-enter-result n "pcov" (cons 'calcFunc-vpcov
					     (calc-top-list-n n)))
	 (calc-enter-result n "cov" (cons 'calcFunc-vcov
					  (calc-top-list-n n))))))))

(defun calc-vector-pop-covariance (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-vector-covariance arg))

(defun calc-vector-correlation (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-vector-covariance arg))

(defun calc-vector-op (name func arg)
  (setq calc-aborted-prefix name
	arg (prefix-numeric-value arg))
  (if (< arg 0)
      (error "Negative arguments not allowed"))
  (calc-enter-result arg name (cons func (calc-top-list-n arg))))




;;; Useful statistical functions

;;; Sum, product, etc., of one or more values or vectors.
;;; Each argument must be either a number or a vector.  Vectors
;;; are flattened, but variables inside are assumed to represent
;;; non-vectors.

(defun calcFunc-vsum (&rest vecs)
  (math-reduce-many-vecs 'calcFunc-add 'calcFunc-vsum vecs 0))

(defun calcFunc-vprod (&rest vecs)
  (math-reduce-many-vecs 'calcFunc-mul 'calcFunc-vprod vecs 1))

(defun calcFunc-vmax (&rest vecs)
  (if (eq (car-safe (car vecs)) 'sdev)
      '(var inf var-inf)
    (if (eq (car-safe (car vecs)) 'intv)
	(nth 3 (math-fix-int-intv (car vecs)))
      (math-reduce-many-vecs 'calcFunc-max 'calcFunc-vmax vecs
			     '(neg (var inf var-inf))))))

(defun calcFunc-vmin (&rest vecs)
  (if (eq (car-safe (car vecs)) 'sdev)
      '(neg (var inf var-inf))
    (if (eq (car-safe (car vecs)) 'intv)
	(nth 2 (math-fix-int-intv (car vecs)))
      (math-reduce-many-vecs 'calcFunc-min 'calcFunc-vmin vecs
			     '(var inf var-inf)))))

(defun math-reduce-many-vecs (func whole-func vecs ident)
  (let ((const-part nil)
	(symb-part nil)
	val vec)
    (let ((calc-internal-prec (+ calc-internal-prec 2)))
      (while vecs
	(setq val (car vecs))
	(and (eq (car-safe val) 'var)
	     (eq (car-safe (calc-var-value (nth 2 val))) 'vec)
	     (setq val (symbol-value (nth 2 val))))
	(cond ((Math-vectorp val)
	       (setq vec (append (and const-part (list const-part))
				 (math-flatten-vector val)))
	       (setq const-part (if vec
				    (calcFunc-reducer
				     (math-calcFunc-to-var func)
				     (cons 'vec vec))
				  ident)))
	      ((or (Math-objectp val) (math-infinitep val))
	       (setq const-part (if const-part
				    (funcall func const-part val)
				  val)))
	      (t
	       (setq symb-part (nconc symb-part (list val)))))
	(setq vecs (cdr vecs))))
    (if const-part
	(progn
	  (setq const-part (math-normalize const-part))
	  (if symb-part
	      (funcall func const-part (cons whole-func symb-part))
	    const-part))
      (if symb-part (cons whole-func symb-part) ident))))


;;; Return the number of data elements among the arguments.
(defun calcFunc-vcount (&rest vecs)
  (let ((count 0))
    (while vecs
      (setq count (if (Math-vectorp (car vecs))
		      (+ count (math-count-elements (car vecs)))
		    (if (Math-objectp (car vecs))
			(1+ count)
		      (if (and (eq (car-safe (car vecs)) 'var)
			       (eq (car-safe (calc-var-value
					      (nth 2 (car vecs))))
				   'vec))
			  (+ count (math-count-elements
				    (symbol-value (nth 2 (car vecs)))))
			(math-reject-arg (car vecs) 'numvecp))))
	    vecs (cdr vecs)))
    count))

(defun math-count-elements (vec)
  (let ((count 0))
    (while (setq vec (cdr vec))
      (setq count (if (Math-vectorp (car vec))
		      (+ count (math-count-elements (car vec)))
		    (1+ count))))
    count))


(defun math-flatten-many-vecs (vecs)
  (let ((p vecs)
	(vec (list 'vec)))
    (while p
      (setq vec (nconc vec
		       (if (Math-vectorp (car p))
			   (math-flatten-vector (car p))
			 (if (Math-objectp (car p))
			     (list (car p))
			   (if (and (eq (car-safe (car p)) 'var)
				    (eq (car-safe (calc-var-value
						   (nth 2 (car p)))) 'vec))
			       (math-flatten-vector (symbol-value
						     (nth 2 (car p))))
			     (math-reject-arg (car p) 'numvecp)))))
	    p (cdr p)))
    vec))

(defun calcFunc-vflat (&rest vecs)
  (math-flatten-many-vecs vecs))

(defun math-split-sdev-vec (vec zero-ok)
  (let ((means (list 'vec))
	(wts (list 'vec))
	(exact nil)
	(p vec))
    (while (and (setq p (cdr p))
		(not (and (consp (car p))
			  (eq (car (car p)) 'sdev)))))
    (if (null p)
	(list vec nil)
      (while (setq vec (cdr vec))
	(if (and (consp (setq p (car vec)))
		 (eq (car p) 'sdev))
	    (or exact
		(setq means (cons (nth 1 p) means)
		      wts (cons (nth 2 p) wts)))
	  (if zero-ok
	      (setq means (cons (nth 1 p) means)
		    wts (cons 0 wts))
	    (or exact
		(setq means (list 'vec)
		      wts nil
		      exact t))
	    (setq means (cons p means)))))
      (list (nreverse means)
	    (and wts (nreverse wts))))))


;;; Return the arithmetic mean of the argument numbers or vectors.
;;; (If numbers are error forms, computes the weighted mean.)
(defun calcFunc-vmean (&rest vecs)
  (let* ((split (math-split-sdev-vec (math-flatten-many-vecs vecs) nil))
	 (means (car split))
	 (wts (nth 1 split))
	 (len (1- (length means))))
    (if (= len 0)
	(math-reject-arg nil "*Must be at least 1 argument")
      (if (and (= len 1) (eq (car-safe (nth 1 means)) 'intv))
	  (let ((x (math-fix-int-intv (nth 1 means))))
	    (calcFunc-vmean (nth 2 x) (nth 3 x)))
	(math-with-extra-prec 2
	  (if (and wts (> len 1))
	      (let* ((sqrwts (calcFunc-map '(var mul var-mul) wts wts))
		     (suminvsqrwts (calcFunc-reduce
				    '(var add var-add)
				    (calcFunc-map '(var div var-div)
						  1 sqrwts))))
		(math-div (calcFunc-reduce '(var add var-add)
					   (calcFunc-map '(var div var-div)
							 means sqrwts))
			  suminvsqrwts))
	    (math-div (calcFunc-reduce '(var add var-add) means) len)))))))

(defun math-fix-int-intv (x)
  (if (math-floatp x)
      x
    (list 'intv 3
	  (if (memq (nth 1 x) '(2 3)) (nth 2 x) (math-add (nth 2 x) 1))
	  (if (memq (nth 1 x) '(1 3)) (nth 3 x) (math-sub (nth 3 x) 1)))))

;;; Compute the mean with an error estimate.
(defun calcFunc-vmeane (&rest vecs)
  (let* ((split (math-split-sdev-vec (math-flatten-many-vecs vecs) nil))
	 (means (car split))
	 (wts (nth 1 split))
	 (len (1- (length means))))
    (if (= len 0)
	(math-reject-arg nil "*Must be at least 1 argument")
      (math-with-extra-prec 2
	(if wts
	    (let* ((sqrwts (calcFunc-map '(var mul var-mul) wts wts))
		   (suminvsqrwts (calcFunc-reduce
				  '(var add var-add)
				  (calcFunc-map '(var div var-div)
						1 sqrwts))))
	      (math-make-sdev
	       (math-div (calcFunc-reduce '(var add var-add)
					  (calcFunc-map '(var div var-div)
							means sqrwts))
			 suminvsqrwts)
	       (list 'calcFunc-sqrt (math-div 1 suminvsqrwts))))
	  (let ((mean (math-div (calcFunc-reduce '(var add var-add) means)
				len)))
	    (math-make-sdev
	     mean
	     (list 'calcFunc-sqrt
		   (math-div (calcFunc-reducer
			      '(var add var-add)
			      (calcFunc-map '(var pow var-pow)
					    (calcFunc-map '(var abs var-abs)
							  (calcFunc-map
							   '(var add var-add)
							   means
							   (math-neg mean)))
					    2))
			     (math-mul len (1- len)))))))))))


;;; Compute the median of a list of values.
(defun calcFunc-vmedian (&rest vecs)
  (let* ((flat (copy-sequence (cdr (math-flatten-many-vecs vecs))))
	 (p flat)
	 (len (length flat))
	 (hlen (/ len 2)))
    (if (= len 0)
	(math-reject-arg nil "*Must be at least 1 argument")
      (if (and (= len 1) (memq (car-safe (car flat)) '(sdev intv)))
	  (calcFunc-vmean (car flat))
	(while p
	  (if (eq (car-safe (car p)) 'sdev)
	      (setcar p (nth 1 (car p))))
	  (or (Math-anglep (car p))
	      (math-reject-arg (car p) 'anglep))
	  (setq p (cdr p)))
	(setq flat (sort flat 'math-lessp))
	(if (= (% len 2) 0)
	    (math-div (math-add (nth (1- hlen) flat) (nth hlen flat)) 2)
	  (nth hlen flat))))))


(defun calcFunc-vgmean (&rest vecs)
  (let* ((flat (math-flatten-many-vecs vecs))
	 (len (1- (length flat))))
    (if (= len 0)
	(math-reject-arg nil "*Must be at least 1 argument")
      (math-with-extra-prec 2
	(let ((x (calcFunc-reduce '(var mul math-mul) flat)))
	  (if (= len 2)
	      (math-sqrt x)
	    (math-pow x (list 'frac 1 len))))))))


(defun calcFunc-agmean (a b)
  (cond ((Math-equal a b) a)
	((math-zerop a) a)
	((math-zerop b) b)
	(calc-symbolic-mode (math-inexact-result))
	((not (Math-realp a)) (math-reject-arg a 'realp))
	((not (Math-realp b)) (math-reject-arg b 'realp))
	(t
	 (math-with-extra-prec 2
	   (setq a (math-float (math-abs a))
		 b (math-float (math-abs b)))
	   (let (mean)
	     (while (not (math-nearly-equal-float a b))
	       (setq mean (math-mul-float (math-add-float a b) '(float 5 -1))
		     b (math-sqrt-float (math-mul-float a b))
		     a mean))
	     a)))))


(defun calcFunc-vhmean (&rest vecs)
  (let* ((flat (math-flatten-many-vecs vecs))
	 (len (1- (length flat))))
    (if (= len 0)
	(math-reject-arg nil "*Must be at least 1 argument")
      (math-with-extra-prec 2
	(math-div len
		  (calcFunc-reduce '(var add math-add)
				   (calcFunc-map '(var inv var-inv) flat)))))))



;;; Compute the sample variance or standard deviation of numbers or vectors.
;;; (If the numbers are error forms, only the mean part of them is used.)
(defun calcFunc-vvar (&rest vecs)
  (if (and (= (length vecs) 1)
	   (memq (car-safe (car vecs)) '(sdev intv)))
      (if (eq (car-safe (car vecs)) 'intv)
	  (math-intv-variance (car vecs) nil)
	(math-sqr (nth 2 (car vecs))))
    (math-covariance vecs nil nil 0)))

(defun calcFunc-vsdev (&rest vecs)
  (if (and (= (length vecs) 1)
	   (memq (car-safe (car vecs)) '(sdev intv)))
      (if (eq (car-safe (car vecs)) 'intv)
	  (if (math-floatp (car vecs))
	      (math-div (math-sub (nth 3 (car vecs)) (nth 2 (car vecs)))
			(math-sqrt-12))
	    (math-sqrt (calcFunc-vvar (car vecs))))
	(nth 2 (car vecs)))
    (math-sqrt (math-covariance vecs nil nil 0))))

;;; Compute the population variance or std deviation of numbers or vectors.
(defun calcFunc-vpvar (&rest vecs)
  (if (and (= (length vecs) 1)
	   (memq (car-safe (car vecs)) '(sdev intv)))
      (if (eq (car-safe (car vecs)) 'intv)
	  (math-intv-variance (car vecs) t)
	(math-sqr (nth 2 (car vecs))))
    (math-covariance vecs nil t 0)))

(defun calcFunc-vpsdev (&rest vecs)
  (if (and (= (length vecs) 1)
	   (memq (car-safe (car vecs)) '(sdev intv)))
      (if (eq (car-safe (car vecs)) 'intv)
	  (if (math-floatp (car vecs))
	      (math-div (math-sub (nth 3 (car vecs)) (nth 2 (car vecs)))
			(math-sqrt-12))
	    (math-sqrt (calcFunc-vpvar (car vecs))))
	(nth 2 (car vecs)))
    (math-sqrt (math-covariance vecs nil t 0))))

(defun math-intv-variance (x pop)
  (or (math-constp x) (math-reject-arg x 'constp))
  (if (math-floatp x)
      (math-div (math-sqr (math-sub (nth 3 x) (nth 2 x))) 12)
    (let* ((x (math-fix-int-intv x))
	   (len (math-sub (nth 3 x) (nth 2 x)))
	   (hlen (math-quotient len 2)))
      (math-div (if (math-evenp len)
		    (calcFunc-sum '(^ (var X var-X) 2) '(var X var-X)
				  (math-neg hlen) hlen)
		  (calcFunc-sum '(^ (- (var X var-X) (/ 1 2)) 2)
				'(var X var-X)
				(math-neg hlen) (math-add hlen 1)))
		(if pop (math-add len 1) len)))))

;;; Compute the covariance and linear correlation coefficient.
(defun calcFunc-vcov (vec1 &optional vec2)
  (math-covariance (list vec1) (list vec2) nil 1))

(defun calcFunc-vpcov (vec1 &optional vec2)
  (math-covariance (list vec1) (list vec2) t 1))

(defun calcFunc-vcorr (vec1 &optional vec2)
  (math-covariance (list vec1) (list vec2) nil 2))


(defun math-covariance (vec1 vec2 pop mode)
  (or (car vec2) (= mode 0)
      (progn
	(if (and (eq (car-safe (car vec1)) 'var)
		 (eq (car-safe (calc-var-value (nth 2 (car vec1)))) 'vec))
	    (setq vec1 (symbol-value (nth 2 (car vec1))))
	  (setq vec1 (car vec1)))
	(or (math-matrixp vec1) (math-dimension-error))
	(or (= (length (nth 1 vec1)) 3) (math-dimension-error))
	(setq vec2 (list (math-mat-col vec1 2))
	      vec1 (list (math-mat-col vec1 1)))))
  (math-with-extra-prec 2
    (let* ((split1 (math-split-sdev-vec (math-flatten-many-vecs vec1) nil))
	   (means1 (car split1))
	   (wts1 (nth 1 split1))
	   split2 means2 (wts2 nil)
	   (sqrwts nil)
	   suminvsqrwts
	   (len (1- (length means1))))
      (if (< len (if pop 1 2))
	  (math-reject-arg nil (if pop
				   "*Must be at least 1 argument"
				 "*Must be at least 2 arguments")))
      (if (or wts1 wts2)
	  (setq sqrwts (math-add
			(if wts1
			    (calcFunc-map '(var mul var-mul) wts1 wts1)
			  0)
			(if wts2
			    (calcFunc-map '(var mul var-mul) wts2 wts2)
			  0))
		suminvsqrwts (calcFunc-reduce
			      '(var add var-add)
			      (calcFunc-map '(var div var-div) 1 sqrwts))))
      (or (= mode 0)
	  (progn
	    (setq split2 (math-split-sdev-vec (math-flatten-many-vecs vec2)
					      nil)
		  means2 (car split2)
		  wts2 (nth 2 split1))
	    (or (= len (1- (length means2))) (math-dimension-error))))
      (let* ((diff1 (calcFunc-map
		     '(var add var-add)
		     means1
		     (if sqrwts
			 (math-div (calcFunc-reduce
				    '(var add var-add)
				    (calcFunc-map '(var div var-div)
						  means1 sqrwts))
				   (math-neg suminvsqrwts))
		       (math-div (calcFunc-reducer '(var add var-add) means1)
				 (- len)))))
	     (diff2 (if (= mode 0)
			diff1
		      (calcFunc-map
		       '(var add var-add)
		       means2
		       (if sqrwts
			   (math-div (calcFunc-reduce
				      '(var add var-add)
				      (calcFunc-map '(var div var-div)
						    means2 sqrwts))
				     (math-neg suminvsqrwts))
			 (math-div (calcFunc-reducer '(var add var-add) means2)
				   (- len))))))
	     (covar (calcFunc-map '(var mul var-mul) diff1 diff2)))
	(if sqrwts
	    (setq covar (calcFunc-map '(var div var-div) covar sqrwts)))
	(math-div
	 (calcFunc-reducer '(var add var-add) covar)
	 (if (= mode 2)
	     (let ((var1 (calcFunc-map '(var mul var-mul) diff1 diff1))
		   (var2 (calcFunc-map '(var mul var-mul) diff2 diff2)))
	       (if sqrwts
		   (setq var1 (calcFunc-map '(var div var-div) var1 sqrwts)
			 var2 (calcFunc-map '(var div var-div) var2 sqrwts)))
	       (math-sqrt
		(math-mul (calcFunc-reducer '(var add var-add) var1)
			  (calcFunc-reducer '(var add var-add) var2))))
	   (if sqrwts
	       (if pop
		   suminvsqrwts
		 (math-div (math-mul suminvsqrwts (1- len)) len))
	     (if pop len (1- len)))))))))

(provide 'calc-stat)

;;; calc-stat.el ends here
