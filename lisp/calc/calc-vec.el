;;; calc-vec.el --- vector functions for Calc

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
(declare-function math-read-expr-level "calc-aent" (exp-prec &optional exp-term))


(defun calc-display-strings (n)
  (interactive "P")
  (calc-wrapper
   (message (if (calc-change-mode 'calc-display-strings n t t)
		"Displaying vectors of integers as quoted strings"
	      "Displaying vectors of integers normally"))))


(defun calc-pack (n)
  (interactive "P")
  (calc-wrapper
   (let* ((nn (if n 1 2))
	  (mode (if n (prefix-numeric-value n) (calc-top-n 1)))
	  (mode (if (and (Math-vectorp mode) (cdr mode)) (cdr mode)
		  (if (integerp mode) mode
		    (error "Packing mode must be an integer or vector of integers"))))
	  (num (calc-pack-size mode))
	  (items (calc-top-list num nn)))
     (calc-enter-result (+ nn num -1) "pack" (calc-pack-items mode items)))))

(defun calc-pack-size (mode)
  (cond ((consp mode)
	 (let ((size 1))
	   (while mode
	     (or (integerp (car mode)) (error "Vector of integers expected"))
	     (setq size (* size (calc-pack-size (car mode)))
		   mode (cdr mode)))
	   (if (= size 0)
	       (error "Zero dimensions not allowed")
	     size)))
	((>= mode 0) mode)
	(t (or (cdr (assq mode '((-3 . 3) (-13 . 1) (-14 . 3) (-15 . 6))))
	       2))))

(defun calc-pack-items (mode items)
  (cond ((consp mode)
	 (if (cdr mode)
	     (let* ((size (calc-pack-size (cdr mode)))
		    (len (length items))
		    (new nil)
		    p row)
	       (while (> len 0)
		 (setq p (nthcdr (1- size) items)
		       row items
		       items (cdr p)
		       len (- len size))
		 (setcdr p nil)
		 (setq new (cons (calc-pack-items (cdr mode) row) new)))
	       (calc-pack-items (car mode) (nreverse new)))
	   (calc-pack-items (car mode) items)))
	((>= mode 0)
	 (cons 'vec items))
	((= mode -3)
	 (if (and (math-objvecp (car items))
		  (math-objvecp (nth 1 items))
		  (math-objvecp (nth 2 items)))
	     (if (and (math-num-integerp (car items))
		      (math-num-integerp (nth 1 items)))
		 (if (math-realp (nth 2 items))
		     (cons 'hms items)
		   (error "Seconds must be real"))
	       (error "Hours and minutes must be integers"))
	   (math-normalize (list '+
				 (list '+
				       (if (eq calc-angle-mode 'rad)
					   (list '* (car items)
						 '(hms 1 0 0))
					 (car items))
				       (list '* (nth 1 items) '(hms 0 1 0)))
				 (list '* (nth 2 items) '(hms 0 0 1))))))
	((= mode -13)
	 (if (math-realp (car items))
	     (cons 'date items)
	   (if (eq (car-safe (car items)) 'date)
	       (car items)
	     (if (math-objvecp (car items))
		 (error "Date value must be real")
	       (cons 'calcFunc-date items)))))
	((memq mode '(-14 -15))
	 (let ((p items))
	   (while (and p (math-objvecp (car p)))
	     (or (math-integerp (car p))
		 (error "Components must be integers"))
	     (setq p (cdr p)))
	   (if p
	       (cons 'calcFunc-date items)
	     (list 'date (math-dt-to-date items)))))
	((or (eq (car-safe (car items)) 'vec)
	     (eq (car-safe (nth 1 items)) 'vec))
	 (let* ((x (car items))
		(vx (eq (car-safe x) 'vec))
		(y (nth 1 items))
		(vy (eq (car-safe y) 'vec))
		(z nil)
		(n (1- (length (if vx x y)))))
	   (and vx vy
		(/= n (1- (length y)))
		(error "Vectors must be the same length"))
	   (while (>= (setq n (1- n)) 0)
	     (setq z (cons (calc-pack-items
			    mode
			    (list (if vx (car (setq x (cdr x))) x)
				  (if vy (car (setq y (cdr y))) y)))
			   z)))
	   (cons 'vec (nreverse z))))
	((= mode -1)
	 (if (and (math-realp (car items)) (math-realp (nth 1 items)))
	     (cons 'cplx items)
	   (if (and (math-objectp (car items)) (math-objectp (nth 1 items)))
	       (error "Components must be real"))
	   (math-normalize (list '+ (car items)
				 (list '* (nth 1 items) '(cplx 0 1))))))
	((= mode -2)
	 (if (and (math-realp (car items)) (math-anglep (nth 1 items)))
	     (cons 'polar items)
	   (if (and (math-objectp (car items)) (math-objectp (nth 1 items)))
	       (error "Components must be real"))
	   (math-normalize (list '* (car items)
				 (if (math-anglep (nth 1 items))
				     (list 'polar 1 (nth 1 items))
				   (list 'calcFunc-exp
					 (list '*
					       (math-to-radians-2
						(nth 1 items))
					       (list 'polar
						     1
						     (math-quarter-circle
						      nil)))))))))
	((= mode -4)
	 (let ((x (car items))
	       (sigma (nth 1 items)))
	   (if (or (math-scalarp x) (not (math-objvecp x)))
	       (if (or (math-anglep sigma) (not (math-objvecp sigma)))
		   (math-make-sdev x sigma)
		 (error "Error component must be real"))
	     (error "Mean component must be real or complex"))))
	((= mode -5)
	 (let ((a (car items))
	       (m (nth 1 items)))
	   (if (and (math-anglep a) (math-anglep m))
	       (if (math-posp m)
		   (math-make-mod a m)
		 (error "Modulus must be positive"))
	     (if (and (math-objectp a) (math-objectp m))
		 (error "Components must be real"))
	     (list 'calcFunc-makemod a m))))
	((memq mode '(-6 -7 -8 -9))
	 (let ((lo (car items))
	       (hi (nth 1 items)))
	   (if (and (or (math-anglep lo) (eq (car lo) 'date)
			(not (math-objvecp lo)))
		    (or (math-anglep hi) (eq (car hi) 'date)
			(not (math-objvecp hi))))
	       (math-make-intv (+ mode 9) lo hi)
	     (error "Components must be real"))))
	((eq mode -10)
	 (if (math-zerop (nth 1 items))
	     (error "Denominator must not be zero")
	   (if (and (math-integerp (car items)) (math-integerp (nth 1 items)))
	       (math-normalize (cons 'frac items))
	     (if (and (math-objectp (car items)) (math-objectp (nth 1 items)))
		 (error "Components must be integers"))
	     (cons 'calcFunc-fdiv items))))
	((memq mode '(-11 -12))
	 (if (and (math-realp (car items)) (math-integerp (nth 1 items)))
	     (calcFunc-scf (math-float (car items)) (nth 1 items))
	   (if (and (math-objectp (car items)) (math-objectp (nth 1 items)))
	       (error "Components must be integers"))
	   (math-normalize
	    (list 'calcFunc-scf
		  (list 'calcFunc-float (car items))
		  (nth 1 items)))))
	(t
	 (error "Invalid packing mode: %d" mode))))

(defvar calc-unpack-with-type nil)
(defun calc-unpack (mode)
  (interactive "P")
  (calc-wrapper
   (let ((calc-unpack-with-type t))
     (calc-pop-push-record-list 1 "unpk" (calc-unpack-item
					  (and mode
					       (prefix-numeric-value mode))
					  (calc-top))))))

(defun calc-unpack-type (item)
  (cond ((eq (car-safe item) 'vec)
	 (1- (length item)))
	((eq (car-safe item) 'intv)
	 (- (nth 1 item) 9))
	(t
	 (or (cdr (assq (car-safe item) '( (cplx . -1) (polar . -2)
					   (hms . -3) (sdev . -4) (mod . -5)
					   (frac . -10) (float . -11)
					   (date . -13) )))
	     (error "Argument must be a composite object")))))

(defun calc-unpack-item (mode item)
  (cond ((not mode)
	 (if (or (and (not (memq (car-safe item) '(frac float cplx polar vec
							hms date sdev mod
							intv)))
		      (math-objvecp item))
		 (eq (car-safe item) 'var))
	     (error "Argument must be a composite object or function call"))
	 (if (eq (car item) 'intv)
	     (cdr (cdr item))
	   (cdr item)))
	((> mode 0)
	 (let ((dims nil)
	       type new row)
	   (setq item (list item))
	   (while (> mode 0)
	     (setq type (calc-unpack-type (car item))
		   dims (cons type dims)
		   new (calc-unpack-item nil (car item)))
	     (while (setq item (cdr item))
	       (or (= (calc-unpack-type (car item)) type)
		   (error "Inconsistent types or dimensions in vector elements"))
	       (setq new (append new (calc-unpack-item nil (car item)))))
	     (setq item new
		   mode (1- mode)))
	   (if (cdr dims) (setq dims (list (cons 'vec (nreverse dims)))))
	   (cond ((eq calc-unpack-with-type 'pair)
		  (list (car dims) (cons 'vec item)))
		 (calc-unpack-with-type
		  (append item dims))
		 (t item))))
	((eq calc-unpack-with-type 'pair)
	 (let ((calc-unpack-with-type nil))
	   (list mode (cons 'vec (calc-unpack-item mode item)))))
	((= mode -3)
	 (if (eq (car-safe item) 'hms)
	     (cdr item)
	   (error "Argument must be an HMS form")))
	((= mode -13)
	 (if (eq (car-safe item) 'date)
	     (cdr item)
	   (error "Argument must be a date form")))
	((= mode -14)
	 (if (eq (car-safe item) 'date)
	     (math-date-to-dt (math-floor (nth 1 item)))
	   (error "Argument must be a date form")))
	((= mode -15)
	 (if (eq (car-safe item) 'date)
	     (append (math-date-to-dt (nth 1 item))
		     (and (not (math-integerp (nth 1 item)))
			  (list 0 0 0)))
	   (error "Argument must be a date form")))
	((eq (car-safe item) 'vec)
	 (let ((x nil)
	       (y nil)
	       res)
	   (while (setq item (cdr item))
	     (setq res (calc-unpack-item mode (car item))
		   x (cons (car res) x)
		   y (cons (nth 1 res) y)))
	   (list (cons 'vec (nreverse x))
		 (cons 'vec (nreverse y)))))
	((= mode -1)
	 (if (eq (car-safe item) 'cplx)
	     (cdr item)
	   (if (eq (car-safe item) 'polar)
	       (cdr (math-complex item))
	     (if (Math-realp item)
		 (list item 0)
	       (error "Argument must be a complex number")))))
	((= mode -2)
	 (if (or (memq (car-safe item) '(cplx polar))
		 (Math-realp item))
	     (cdr (math-polar item))
	   (error "Argument must be a complex number")))
	((= mode -4)
	 (if (eq (car-safe item) 'sdev)
	     (cdr item)
	   (list item 0)))
	((= mode -5)
	 (if (eq (car-safe item) 'mod)
	     (cdr item)
	   (error "Argument must be a modulo form")))
	((memq mode '(-6 -7 -8 -9))
	 (if (eq (car-safe item) 'intv)
	     (cdr (cdr item))
	   (list item item)))
	((= mode -10)
	 (if (eq (car-safe item) 'frac)
	     (cdr item)
	   (if (Math-integerp item)
	       (list item 1)
	     (error "Argument must be a rational number"))))
	((= mode -11)
	 (if (eq (car-safe item) 'float)
	     (list (nth 1 item) (math-normalize (nth 2 item)))
	   (error "Expected a floating-point number")))
	((= mode -12)
	 (if (eq (car-safe item) 'float)
	     (list (calcFunc-mant item) (calcFunc-xpon item))
	   (error "Expected a floating-point number")))
	(t
	 (error "Invalid unpacking mode: %d" mode))))

(defun calc-diag (n)
  (interactive "P")
  (calc-wrapper
   (calc-enter-result 1 "diag" (if n
				   (list 'calcFunc-diag (calc-top-n 1)
					 (prefix-numeric-value n))
				 (list 'calcFunc-diag (calc-top-n 1))))))

(defun calc-ident (n)
  (interactive "NDimension of identity matrix = ")
  (calc-wrapper
   (calc-enter-result 0 "idn" (if (eq n 0)
				  '(calcFunc-idn 1)
				(list 'calcFunc-idn 1
				      (prefix-numeric-value n))))))

(defun calc-index (n &optional stack)
  (interactive "NSize of vector = \nP")
  (calc-wrapper
   (if (consp stack)
       (calc-enter-result 3 "indx" (cons 'calcFunc-index (calc-top-list-n 3)))
     (calc-enter-result 0 "indx" (list 'calcFunc-index
				       (prefix-numeric-value n))))))

(defun calc-build-vector (n)
  (interactive "NSize of vector = ")
  (calc-wrapper
   (calc-enter-result 1 "bldv" (list 'calcFunc-cvec
				     (calc-top-n 1)
				     (prefix-numeric-value n)))))

(defun calc-cons (arg)
  (interactive "P")
  (calc-wrapper
   (if (calc-is-hyperbolic)
       (calc-binary-op "rcns" 'calcFunc-rcons arg)
     (calc-binary-op "cons" 'calcFunc-cons arg))))


(defun calc-head (arg)
  (interactive "P")
  (calc-wrapper
   (if (calc-is-inverse)
       (if (calc-is-hyperbolic)
	   (calc-unary-op "rtai" 'calcFunc-rtail arg)
	 (calc-unary-op "tail" 'calcFunc-tail arg))
     (if (calc-is-hyperbolic)
	 (calc-unary-op "rhed" 'calcFunc-rhead arg)
       (calc-unary-op "head" 'calcFunc-head arg)))))

(defun calc-tail (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-head arg))

(defun calc-vlength (arg)
  (interactive "P")
  (calc-wrapper
   (if (calc-is-hyperbolic)
       (calc-unary-op "dims" 'calcFunc-mdims arg)
     (calc-unary-op "len" 'calcFunc-vlen arg))))

(defun calc-arrange-vector (n)
  (interactive "NNumber of columns = ")
  (calc-wrapper
   (calc-enter-result 1 "arng" (list 'calcFunc-arrange (calc-top-n 1)
				     (prefix-numeric-value n)))))

(defun calc-vector-find (arg)
  (interactive "P")
  (calc-wrapper
   (let ((func (cons 'calcFunc-find (calc-top-list-n 2))))
     (calc-enter-result
      2 "find"
      (if arg (append func (list (prefix-numeric-value arg))) func)))))

(defun calc-subvector ()
  (interactive)
  (calc-wrapper
   (if (calc-is-inverse)
       (calc-enter-result 3 "rsvc" (cons 'calcFunc-rsubvec
					 (calc-top-list-n 3)))
     (calc-enter-result 3 "svec" (cons 'calcFunc-subvec (calc-top-list-n 3))))))

(defun calc-reverse-vector (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "rev" 'calcFunc-rev arg)))

(defun calc-mask-vector (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "vmsk" 'calcFunc-vmask arg)))

(defun calc-expand-vector (arg)
  (interactive "P")
  (calc-wrapper
   (if (calc-is-hyperbolic)
       (calc-enter-result 3 "vexp" (cons 'calcFunc-vexp (calc-top-list-n 3)))
     (calc-binary-op "vexp" 'calcFunc-vexp arg))))

(defun calc-sort ()
  (interactive)
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-enter-result 1 "rsrt" (list 'calcFunc-rsort (calc-top-n 1)))
     (calc-enter-result 1 "sort" (list 'calcFunc-sort (calc-top-n 1))))))

(defun calc-grade ()
  (interactive)
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-enter-result 1 "rgrd" (list 'calcFunc-rgrade (calc-top-n 1)))
     (calc-enter-result 1 "grad" (list 'calcFunc-grade (calc-top-n 1))))))

(defun calc-histogram (n)
  (interactive "P")
  (unless (natnump n)
    (setq n (math-read-expr (read-string "Centers of bins: "))))
  (calc-slow-wrapper
   (if calc-hyperbolic-flag
       (calc-enter-result 2 "hist" (list 'calcFunc-histogram
					 (calc-top-n 2)
					 (calc-top-n 1)
					 n))
     (calc-enter-result 1 "hist" (list 'calcFunc-histogram
				       (calc-top-n 1)
                                       n)))))

(defun calc-transpose (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "trn" 'calcFunc-trn arg)))

(defun calc-conj-transpose (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "ctrn" 'calcFunc-ctrn arg)))

(defun calc-cross (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "cros" 'calcFunc-cross arg)))

(defun calc-kron (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "kron" 'calcFunc-kron arg)))

(defun calc-remove-duplicates (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "rdup" 'calcFunc-rdup arg)))

(defun calc-set-union (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "unio" 'calcFunc-vunion arg '(vec) 'calcFunc-rdup)))

(defun calc-set-intersect (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "intr" 'calcFunc-vint arg '(vec) 'calcFunc-rdup)))

(defun calc-set-difference (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "diff" 'calcFunc-vdiff arg '(vec) 'calcFunc-rdup)))

(defun calc-set-xor (arg)
  (interactive "P")
  (calc-wrapper
   (calc-binary-op "xor" 'calcFunc-vxor arg '(vec) 'calcFunc-rdup)))

(defun calc-set-complement (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "cmpl" 'calcFunc-vcompl arg)))

(defun calc-set-floor (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "vflr" 'calcFunc-vfloor arg)))

(defun calc-set-enumerate (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "enum" 'calcFunc-venum arg)))

(defun calc-set-span (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "span" 'calcFunc-vspan arg)))

(defun calc-set-cardinality (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "card" 'calcFunc-vcard arg)))

(defun calc-unpack-bits (arg)
  (interactive "P")
  (calc-wrapper
   (if (calc-is-inverse)
       (calc-unary-op "bpck" 'calcFunc-vpack arg)
     (calc-unary-op "bupk" 'calcFunc-vunpack arg))))

(defun calc-pack-bits (arg)
  (interactive "P")
  (calc-invert-func)
  (calc-unpack-bits arg))


(defun calc-rnorm (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "rnrm" 'calcFunc-rnorm arg)))

(defun calc-cnorm (arg)
  (interactive "P")
  (calc-wrapper
   (calc-unary-op "cnrm" 'calcFunc-cnorm arg)))

(defun calc-mrow (n &optional nn)
  (interactive "NRow number: \nP")
  (calc-wrapper
   (if (consp nn)
       (calc-enter-result 2 "mrow" (cons 'calcFunc-mrow (calc-top-list-n 2)))
     (setq n (prefix-numeric-value n))
     (if (= n 0)
	 (calc-enter-result 1 "getd" (list 'calcFunc-getdiag (calc-top-n 1)))
       (if (< n 0)
	   (calc-enter-result 1 "rrow" (list 'calcFunc-mrrow
					     (calc-top-n 1) (- n)))
	 (calc-enter-result 1 "mrow" (list 'calcFunc-mrow
					   (calc-top-n 1) n)))))))

(defun calc-mcol (n &optional nn)
  (interactive "NColumn number: \nP")
  (calc-wrapper
   (if (consp nn)
       (calc-enter-result 2 "mcol" (cons 'calcFunc-mcol (calc-top-list-n 2)))
     (setq n (prefix-numeric-value n))
     (if (= n 0)
	 (calc-enter-result 1 "getd" (list 'calcFunc-getdiag (calc-top-n 1)))
       (if (< n 0)
	   (calc-enter-result 1 "rcol" (list 'calcFunc-mrcol
					     (calc-top-n 1) (- n)))
	 (calc-enter-result 1 "mcol" (list 'calcFunc-mcol
					   (calc-top-n 1) n)))))))


;;;; Vectors.

(defun calcFunc-mdims (m)
  (or (math-vectorp m)
      (math-reject-arg m 'vectorp))
  (cons 'vec (math-mat-dimens m)))


;;; Apply a function elementwise to vector A.  [V X V; N X N] [Public]
(defun math-map-vec (f a)
  (if (math-vectorp a)
      (cons 'vec (mapcar f (cdr a)))
    (funcall f a)))

(defun math-dimension-error ()
  (calc-record-why "*Dimension error")
  (signal 'wrong-type-argument nil))


;;; Build a vector out of a list of objects.  [Public]
(defun calcFunc-vec (&rest objs)
  (cons 'vec objs))


;;; Build a constant vector or matrix.  [Public]
(defun calcFunc-cvec (obj &rest dims)
  (math-make-vec-dimen obj dims))

(defun math-make-vec-dimen (obj dims)
  (if dims
      (if (natnump (car dims))
	  (if (or (cdr dims)
		  (not (math-numberp obj)))
	      (cons 'vec (copy-sequence
			  (make-list (car dims)
				     (math-make-vec-dimen obj (cdr dims)))))
	    (cons 'vec (make-list (car dims) obj)))
	(math-reject-arg (car dims) 'fixnatnump))
    obj))

(defun calcFunc-head (vec)
  (if (and (Math-vectorp vec)
	   (cdr vec))
      (nth 1 vec)
    (calc-record-why 'vectorp vec)
    (list 'calcFunc-head vec)))

(defun calcFunc-tail (vec)
  (if (and (Math-vectorp vec)
	   (cdr vec))
      (cons 'vec (cdr (cdr vec)))
    (calc-record-why 'vectorp vec)
    (list 'calcFunc-tail vec)))

(defun calcFunc-cons (head tail)
  (if (Math-vectorp tail)
      (cons 'vec (cons head (cdr tail)))
    (calc-record-why 'vectorp tail)
    (list 'calcFunc-cons head tail)))

(defun calcFunc-rhead (vec)
  (if (and (Math-vectorp vec)
	   (cdr vec))
      (let ((vec (copy-sequence vec)))
	(setcdr (nthcdr (- (length vec) 2) vec) nil)
	vec)
    (calc-record-why 'vectorp vec)
    (list 'calcFunc-rhead vec)))

(defun calcFunc-rtail (vec)
  (if (and (Math-vectorp vec)
	   (cdr vec))
      (nth (1- (length vec)) vec)
    (calc-record-why 'vectorp vec)
    (list 'calcFunc-rtail vec)))

(defun calcFunc-rcons (head tail)
  (if (Math-vectorp head)
      (append head (list tail))
    (calc-record-why 'vectorp head)
    (list 'calcFunc-rcons head tail)))



;;; Apply a function elementwise to vectors A and B.  [O X O O] [Public]
(defun math-map-vec-2 (f a b)
  (if (math-vectorp a)
      (if (math-vectorp b)
	  (let ((v nil))
	    (while (setq a (cdr a))
	      (or (setq b (cdr b))
		  (math-dimension-error))
	      (setq v (cons (funcall f (car a) (car b)) v)))
	    (if a (math-dimension-error))
	    (cons 'vec (nreverse v)))
	(let ((v nil))
	  (while (setq a (cdr a))
	    (setq v (cons (funcall f (car a) b) v)))
	  (cons 'vec (nreverse v))))
    (if (math-vectorp b)
	(let ((v nil))
	  (while (setq b (cdr b))
	    (setq v (cons (funcall f a (car b)) v)))
	  (cons 'vec (nreverse v)))
      (funcall f a b))))



;;; "Reduce" a function over a vector (left-associatively).  [O X V] [Public]
(defun math-reduce-vec (f a)
  (if (math-vectorp a)
      (if (cdr a)
	  (let ((accum (car (setq a (cdr a)))))
	    (while (setq a (cdr a))
	      (setq accum (funcall f accum (car a))))
	    accum)
	0)
    a))

;;; Reduce a function over the columns of matrix A.  [V X V] [Public]
(defun math-reduce-cols (f a)
  (if (math-matrixp a)
      (cons 'vec (math-reduce-cols-col-step f (cdr a) 1 (length (nth 1 a))))
    a))

(defun math-reduce-cols-col-step (f a col cols)
  (and (< col cols)
       (cons (math-reduce-cols-row-step f (nth col (car a)) col (cdr a))
	     (math-reduce-cols-col-step f a (1+ col) cols))))

(defun math-reduce-cols-row-step (f tot col a)
  (if a
      (math-reduce-cols-row-step f
				 (funcall f tot (nth col (car a)))
				 col
				 (cdr a))
    tot))



(defun math-dot-product (a b)
  (if (setq a (cdr a) b (cdr b))
      (let ((accum (math-mul (car a) (car b))))
	(while (setq a (cdr a) b (cdr b))
	  (setq accum (math-add accum (math-mul (car a) (car b)))))
	accum)
    0))


;;; Return the number of elements in vector V.  [Public]
(defun calcFunc-vlen (v)
  (if (math-vectorp v)
      (1- (length v))
    (if (math-objectp v)
	0
      (list 'calcFunc-vlen v))))

;;; Get the Nth row of a matrix.
(defun calcFunc-mrow (mat n)   ; [Public]
  (if (Math-vectorp n)
      (math-map-vec (function (lambda (x) (calcFunc-mrow mat x))) n)
    (if (and (eq (car-safe n) 'intv) (math-constp n))
	(calcFunc-subvec mat
			 (math-add (nth 2 n) (if (memq (nth 1 n) '(2 3)) 0 1))
			 (math-add (nth 3 n) (if (memq (nth 1 n) '(1 3)) 1 0)))
      (or (and (integerp (setq n (math-check-integer n)))
	       (> n 0))
	  (math-reject-arg n 'fixposintp))
      (or (Math-vectorp mat)
	  (math-reject-arg mat 'vectorp))
      (or (nth n mat)
	  (math-reject-arg n "*Index out of range")))))

(defun calcFunc-subscr (mat n &optional m)
  (if (eq (car-safe mat) 'var) nil
    (setq mat (calcFunc-mrow mat n))
    (if m
        (if (math-num-integerp n)
            (calcFunc-mrow mat m)
          (calcFunc-mcol mat m))
      mat)))

;;; Get the Nth column of a matrix.
(defun math-mat-col (mat n)
  (cons 'vec (mapcar (function (lambda (x) (elt x n))) (cdr mat))))

(defun calcFunc-mcol (mat n)   ; [Public]
  (if (Math-vectorp n)
      (calcFunc-trn
       (math-map-vec (function (lambda (x) (calcFunc-mcol mat x))) n))
    (if (and (eq (car-safe n) 'intv) (math-constp n))
	(if (math-matrixp mat)
	    (math-map-vec (function (lambda (x) (calcFunc-mrow x n))) mat)
	  (calcFunc-mrow mat n))
      (or (and (integerp (setq n (math-check-integer n)))
	       (> n 0))
	  (math-reject-arg n 'fixposintp))
      (or (Math-vectorp mat)
	  (math-reject-arg mat 'vectorp))
      (or (if (math-matrixp mat)
	      (and (< n (length (nth 1 mat)))
		   (math-mat-col mat n))
	    (nth n mat))
	  (math-reject-arg n "*Index out of range")))))

;;; Remove the Nth row from a matrix.
(defun math-mat-less-row (mat n)
  (if (<= n 0)
      (cdr mat)
    (cons (car mat)
	  (math-mat-less-row (cdr mat) (1- n)))))

(defun calcFunc-mrrow (mat n)   ; [Public]
  (and (integerp (setq n (math-check-integer n)))
       (> n 0)
       (< n (length mat))
       (math-mat-less-row mat n)))

;;; Remove the Nth column from a matrix.
(defun math-mat-less-col (mat n)
  (cons 'vec (mapcar (function (lambda (x) (math-mat-less-row x n)))
		     (cdr mat))))

(defun calcFunc-mrcol (mat n)   ; [Public]
  (and (integerp (setq n (math-check-integer n)))
       (> n 0)
       (if (math-matrixp mat)
	   (and (< n (length (nth 1 mat)))
		(math-mat-less-col mat n))
	 (math-mat-less-row mat n))))

(defun calcFunc-getdiag (mat)   ; [Public]
  (if (math-square-matrixp mat)
      (cons 'vec (math-get-diag-step (cdr mat) 1))
    (calc-record-why 'square-matrixp mat)
    (list 'calcFunc-getdiag mat)))

(defun math-get-diag-step (row n)
  (and row
       (cons (nth n (car row))
	     (math-get-diag-step (cdr row) (1+ n)))))

(defun math-transpose (mat)   ; [Public]
  (let ((m nil)
	(col (length (nth 1 mat))))
    (while (> (setq col (1- col)) 0)
      (setq m (cons (math-mat-col mat col) m)))
    (cons 'vec m)))

(defun calcFunc-trn (mat)
  (if (math-vectorp mat)
      (if (math-matrixp mat)
	  (math-transpose mat)
	(math-col-matrix mat))
    (if (math-numberp mat)
	mat
      (math-reject-arg mat 'matrixp))))

(defun calcFunc-ctrn (mat)
  (calcFunc-conj (calcFunc-trn mat)))

(defun calcFunc-pack (mode els)
  (or (Math-vectorp els) (math-reject-arg els 'vectorp))
  (if (and (Math-vectorp mode) (cdr mode))
      (setq mode (cdr mode))
    (or (integerp mode) (math-reject-arg mode 'fixnump)))
  (condition-case err
      (if (= (calc-pack-size mode) (1- (length els)))
	  (calc-pack-items mode (cdr els))
	(math-reject-arg els "*Wrong number of elements"))
    (error (math-reject-arg els (nth 1 err)))))

(defun calcFunc-unpack (mode thing)
  (or (integerp mode) (math-reject-arg mode 'fixnump))
  (condition-case err
      (cons 'vec (calc-unpack-item mode thing))
    (error (math-reject-arg thing (nth 1 err)))))

(defun calcFunc-unpackt (mode thing)
  (let ((calc-unpack-with-type 'pair))
    (calcFunc-unpack mode thing)))

(defun calcFunc-arrange (vec cols)   ; [Public]
  (setq cols (math-check-fixnum cols t))
  (if (math-vectorp vec)
      (let* ((flat (math-flatten-vector vec))
	     (mat (list 'vec))
	     next)
	(if (<= cols 0)
	    (nconc mat flat)
	  (while (>= (length flat) cols)
	    (setq next (nthcdr cols flat))
	    (setcdr (nthcdr (1- cols) flat) nil)
	    (setq mat (nconc mat (list (cons 'vec flat)))
		  flat next))
	  (if flat
	      (setq mat (nconc mat (list (cons 'vec flat)))))
	  mat))))

(defun math-flatten-vector (vec)   ; [L V]
  (if (math-vectorp vec)
      (apply 'append (mapcar 'math-flatten-vector (cdr vec)))
    (list vec)))

(defun calcFunc-vconcat (a b)
  (math-normalize (list '| a b)))

(defun calcFunc-vconcatrev (a b)
  (math-normalize (list '| b a)))

(defun calcFunc-append (v1 v2)
  (if (and (math-vectorp v1) (math-vectorp v2))
      (append v1 (cdr v2))
    (list 'calcFunc-append v1 v2)))

(defun calcFunc-appendrev (v1 v2)
  (calcFunc-append v2 v1))


;;; Copy a matrix.  [Public]
(defun math-copy-matrix (m)
  (if (math-vectorp (nth 1 m))
      (cons 'vec (mapcar 'copy-sequence (cdr m)))
    (copy-sequence m)))

;;; Convert a scalar or vector into an NxN diagonal matrix.  [Public]
(defun calcFunc-diag (a &optional n)
  (and n (not (integerp n))
       (setq n (math-check-fixnum n)))
  (if (math-vectorp a)
      (if (and n (/= (length a) (1+ n)))
	  (list 'calcFunc-diag a n)
	(if (math-matrixp a)
	    (if (and n (/= (length (elt a 1)) (1+ n)))
		(list 'calcFunc-diag a n)
	      a)
	  (cons 'vec (math-diag-step (cdr a) 0 (1- (length a))))))
    (if n
	(cons 'vec (math-diag-step (make-list n a) 0 n))
      (list 'calcFunc-diag a))))

(defun calcFunc-idn (a &optional n)
  (if n
      (if (math-vectorp a)
	  (math-reject-arg a 'numberp)
	(calcFunc-diag a n))
    (if (integerp calc-matrix-mode)
	(calcFunc-idn a calc-matrix-mode)
      (list 'calcFunc-idn a))))

(defun math-mimic-ident (a m)
  (if (math-square-matrixp m)
      (calcFunc-idn a (1- (length m)))
    (if (math-vectorp m)
	(if (math-zerop a)
	    (cons 'vec (mapcar (function (lambda (x)
					   (if (math-vectorp x)
					       (math-mimic-ident a x)
					     a)))
			       (cdr m)))
	  (math-dimension-error))
      (calcFunc-idn a))))

(defun math-diag-step (a n m)
  (if (< n m)
      (cons (cons 'vec
		  (nconc (make-list n 0)
			 (cons (car a)
			       (make-list (1- (- m n)) 0))))
	    (math-diag-step (cdr a) (1+ n) m))
    nil))

;;; Create a vector of consecutive integers. [Public]
(defun calcFunc-index (n &optional start incr)
  (if (math-messy-integerp n)
      (math-float (calcFunc-index (math-trunc n) start incr))
    (and (not (integerp n))
	 (setq n (math-check-fixnum n)))
    (let ((vec nil))
      (if start
	  (progn
	    (if (>= n 0)
		(while (>= (setq n (1- n)) 0)
		  (setq vec (cons start vec)
			start (math-add start (or incr 1))))
	      (while (<= (setq n (1+ n)) 0)
		(setq vec (cons start vec)
		      start (math-mul start (or incr 2)))))
	    (setq vec (nreverse vec)))
	(if (>= n 0)
	    (while (> n 0)
	      (setq vec (cons n vec)
		    n (1- n)))
	  (let ((i -1))
	    (while (>= i n)
	      (setq vec (cons i vec)
		    i (1- i))))))
      (cons 'vec vec))))

;;; Find an element in a vector.
(defun calcFunc-find (vec x &optional start)
  (setq start (if start (math-check-fixnum start t) 1))
  (if (< start 1) (math-reject-arg start 'posp))
  (setq vec (nthcdr start vec))
  (let ((n start))
    (while (and vec (not (Math-equal x (car vec))))
      (setq n (1+ n)
	    vec (cdr vec)))
    (if vec n 0)))

;;; Return a subvector of a vector.
(defun calcFunc-subvec (vec start &optional end)
  (setq start (math-check-fixnum start t)
	end (math-check-fixnum (or end 0) t))
  (or (math-vectorp vec) (math-reject-arg vec 'vectorp))
  (let ((len (1- (length vec))))
    (if (<= start 0)
	(setq start (+ len start 1)))
    (if (<= end 0)
	(setq end (+ len end 1)))
    (if (or (> start len)
	    (<= end start))
	'(vec)
      (setq vec (nthcdr start vec))
      (if (<= end len)
	  (let ((chop (nthcdr (- end start 1) (setq vec (copy-sequence vec)))))
	    (setcdr chop nil)))
      (cons 'vec vec))))

;;; Remove a subvector from a vector.
(defun calcFunc-rsubvec (vec start &optional end)
  (setq start (math-check-fixnum start t)
	end (math-check-fixnum (or end 0) t))
  (or (math-vectorp vec) (math-reject-arg vec 'vectorp))
  (let ((len (1- (length vec))))
    (if (<= start 0)
	(setq start (+ len start 1)))
    (if (<= end 0)
	(setq end (+ len end 1)))
    (if (or (> start len)
	    (<= end start))
	vec
      (let ((tail (nthcdr end vec))
	    (chop (nthcdr (1- start) (setq vec (copy-sequence vec)))))
	(setcdr chop nil)
	(append vec tail)))))

;;; Reverse the order of the elements of a vector.
(defun calcFunc-rev (vec)
  (if (math-vectorp vec)
      (cons 'vec (reverse (cdr vec)))
    (math-reject-arg vec 'vectorp)))

;;; Compress a vector according to a mask vector.
(defun calcFunc-vmask (mask vec)
  (if (math-numberp mask)
      (if (math-zerop mask)
	  '(vec)
	vec)
    (or (math-vectorp mask) (math-reject-arg mask 'vectorp))
    (or (math-constp mask) (math-reject-arg mask 'constp))
    (or (math-vectorp vec) (math-reject-arg vec 'vectorp))
    (or (= (length mask) (length vec)) (math-dimension-error))
    (let ((new nil))
      (while (setq mask (cdr mask) vec (cdr vec))
	(or (math-zerop (car mask))
	    (setq new (cons (car vec) new))))
      (cons 'vec (nreverse new)))))

;;; Expand a vector according to a mask vector.
(defun calcFunc-vexp (mask vec &optional filler)
  (or (math-vectorp mask) (math-reject-arg mask 'vectorp))
  (or (math-constp mask) (math-reject-arg mask 'constp))
  (or (math-vectorp vec) (math-reject-arg vec 'vectorp))
  (let ((new nil)
	(fvec (and filler (math-vectorp filler))))
    (while (setq mask (cdr mask))
      (if (math-zerop (car mask))
	  (setq new (cons (or (if fvec
				  (car (setq filler (cdr filler)))
				filler)
			      (car mask)) new))
	(setq vec (cdr vec)
	      new (cons (or (car vec) (car mask)) new))))
    (cons 'vec (nreverse new))))


;;; Compute the row and column norms of a vector or matrix.  [Public]
(defun calcFunc-rnorm (a)
  (if (and (Math-vectorp a)
	   (math-constp a))
      (if (math-matrixp a)
	  (math-reduce-vec 'math-max (math-map-vec 'calcFunc-cnorm a))
	(math-reduce-vec 'math-max (math-map-vec 'math-abs a)))
    (calc-record-why 'vectorp a)
    (list 'calcFunc-rnorm a)))

(defun calcFunc-cnorm (a)
  (if (and (Math-vectorp a)
	   (math-constp a))
      (if (math-matrixp a)
	  (math-reduce-vec 'math-max
			   (math-reduce-cols 'math-add-abs a))
	(math-reduce-vec 'math-add-abs a))
    (calc-record-why 'vectorp a)
    (list 'calcFunc-cnorm a)))

(defun math-add-abs (a b)
  (math-add (math-abs a) (math-abs b)))


;;; Sort the elements of a vector into increasing order.
(defun calcFunc-sort (vec)   ; [Public]
  (if (math-vectorp vec)
      (cons 'vec (sort (copy-sequence (cdr vec)) 'math-beforep))
    (math-reject-arg vec 'vectorp)))

(defun calcFunc-rsort (vec)   ; [Public]
  (if (math-vectorp vec)
      (cons 'vec (nreverse (sort (copy-sequence (cdr vec)) 'math-beforep)))
    (math-reject-arg vec 'vectorp)))

;; The variable math-grade-vec is local to calcFunc-grade and 
;; calcFunc-rgrade, but is used by math-grade-beforep, which is called
;; by calcFunc-grade and calcFunc-rgrade.
(defvar math-grade-vec)

(defun calcFunc-grade (math-grade-vec)
  (if (math-vectorp math-grade-vec)
      (let* ((len (1- (length math-grade-vec))))
	(cons 'vec (sort (cdr (calcFunc-index len)) 'math-grade-beforep)))
    (math-reject-arg math-grade-vec 'vectorp)))

(defun calcFunc-rgrade (math-grade-vec)
  (if (math-vectorp math-grade-vec)
      (let* ((len (1- (length math-grade-vec))))
	(cons 'vec (nreverse (sort (cdr (calcFunc-index len))
				   'math-grade-beforep))))
    (math-reject-arg math-grade-vec 'vectorp)))

(defun math-grade-beforep (i j)
  (math-beforep (nth i math-grade-vec) (nth j math-grade-vec)))


;;; Compile a histogram of data from a vector.
(defun calcFunc-histogram (vec wts &optional n)
  (or n (setq n wts wts 1))
  (or (Math-vectorp vec)
      (math-reject-arg vec 'vectorp))
  (if (Math-vectorp wts)
      (or (= (length vec) (length wts))
	  (math-dimension-error)))
  (cond ((natnump n)
         (let ((res (make-vector n 0))
               (vp vec)
               (wvec (Math-vectorp wts))
               (wp wts)
               bin)
           (while (setq vp (cdr vp))
             (setq bin (car vp))
             (or (natnump bin)
                 (setq bin (math-floor bin)))
            (and (natnump bin)
                 (< bin n)
                 (aset res bin 
                       (math-add (aref res bin)
                                 (if wvec (car (setq wp (cdr wp))) wts)))))
           (cons 'vec (append res nil))))
        ((Math-vectorp n) ;; n is a vector of midpoints
         (let* ((bds (math-vector-avg n))
                (res (make-vector (1- (length n)) 0))
                (vp (cdr vec))
                (wvec (Math-vectorp wts))
                (wp wts)
                num)
           (while vp
             (setq num (car vp))
             (let ((tbds (cdr bds))
                   (i 0))
               (while (and tbds (Math-lessp (car tbds) num))
                 (setq i (1+ i))
                 (setq tbds (cdr tbds)))
               (aset res i 
                     (math-add (aref res i)
                               (if wvec (car (setq wp (cdr wp))) wts))))
             (setq vp (cdr vp)))
           (cons 'vec (append res nil))))
        (t
         (math-reject-arg n "*Expecting an integer or vector"))))

;;; Replace a vector [a b c ...] with a vector of averages
;;; [(a+b)/2 (b+c)/2 ...]
(defun math-vector-avg (vec)
  (let ((vp (sort (copy-sequence (cdr vec)) 'math-beforep))
        (res nil))
    (while (and vp (cdr vp))
      (setq res (cons (math-div (math-add (car vp) (cadr vp)) 2) res)
            vp (cdr vp)))
    (cons 'vec (reverse res))))


;;; Set operations.

(defun calcFunc-vunion (a b)
  (if (Math-objectp a)
      (setq a (list 'vec a))
    (or (math-vectorp a) (math-reject-arg a 'vectorp)))
  (if (Math-objectp b)
      (setq b (list b))
    (or (math-vectorp b) (math-reject-arg b 'vectorp))
    (setq b (cdr b)))
  (calcFunc-rdup (append a b)))

(defun calcFunc-vint (a b)
  (if (and (math-simple-set a) (math-simple-set b))
      (progn
	(setq a (cdr (calcFunc-rdup a)))
	(setq b (cdr (calcFunc-rdup b)))
	(let ((vec (list 'vec)))
	  (while (and a b)
	    (if (math-beforep (car a) (car b))
		(setq a (cdr a))
	      (if (Math-equal (car a) (car b))
		  (setq vec (cons (car a) vec)
			a (cdr a)))
	      (setq b (cdr b))))
	  (nreverse vec)))
    (calcFunc-vcompl (calcFunc-vunion (calcFunc-vcompl a)
				      (calcFunc-vcompl b)))))

(defun calcFunc-vdiff (a b)
  (if (and (math-simple-set a) (math-simple-set b))
      (progn
	(setq a (cdr (calcFunc-rdup a)))
	(setq b (cdr (calcFunc-rdup b)))
	(let ((vec (list 'vec)))
	  (while a
	    (while (and b (math-beforep (car b) (car a)))
	      (setq b (cdr b)))
	    (if (and b (Math-equal (car a) (car b)))
		(setq a (cdr a)
		      b (cdr b))
	      (setq vec (cons (car a) vec)
		    a (cdr a))))
	  (nreverse vec)))
    (calcFunc-vcompl (calcFunc-vunion (calcFunc-vcompl a) b))))

(defun calcFunc-vxor (a b)
  (if (and (math-simple-set a) (math-simple-set b))
      (progn
	(setq a (cdr (calcFunc-rdup a)))
	(setq b (cdr (calcFunc-rdup b)))
	(let ((vec (list 'vec)))
	  (while (or a b)
	    (if (and a
		     (or (not b)
			 (math-beforep (car a) (car b))))
		(setq vec (cons (car a) vec)
		      a (cdr a))
	      (if (and a (Math-equal (car a) (car b)))
		  (setq a (cdr a))
		(setq vec (cons (car b) vec)))
	      (setq b (cdr b))))
	  (nreverse vec)))
    (let ((ca (calcFunc-vcompl a))
	  (cb (calcFunc-vcompl b)))
      (calcFunc-vunion (calcFunc-vcompl (calcFunc-vunion ca b))
		       (calcFunc-vcompl (calcFunc-vunion a cb))))))

(defun calcFunc-vcompl (a)
  (setq a (math-prepare-set a))
  (let ((vec (list 'vec))
	(prev '(neg (var inf var-inf)))
	(closed 2))
    (while (setq a (cdr a))
      (or (and (equal (nth 2 (car a)) '(neg (var inf var-inf)))
	       (memq (nth 1 (car a)) '(2 3)))
	  (setq vec (cons (list 'intv
				(+ closed
				   (if (memq (nth 1 (car a)) '(0 1)) 1 0))
				prev
				(nth 2 (car a)))
			  vec)))
      (setq prev (nth 3 (car a))
	    closed (if (memq (nth 1 (car a)) '(0 2)) 2 0)))
    (or (and (equal prev '(var inf var-inf))
	     (= closed 0))
	(setq vec (cons (list 'intv (+ closed 1)
			      prev '(var inf var-inf))
			vec)))
    (math-clean-set (nreverse vec))))

(defun calcFunc-vspan (a)
  (setq a (math-prepare-set a))
  (if (cdr a)
      (let ((last (nth (1- (length a)) a)))
	(math-make-intv (+ (logand (nth 1 (nth 1 a)) 2)
			   (logand (nth 1 last) 1))
			(nth 2 (nth 1 a))
			(nth 3 last)))
    '(intv 2 0 0)))

(defun calcFunc-vfloor (a &optional always-vec)
  (setq a (math-prepare-set a))
  (let ((vec (list 'vec)) (p a) (prev nil) b mask)
    (while (setq p (cdr p))
      (setq mask (nth 1 (car p))
	    a (nth 2 (car p))
	    b (nth 3 (car p)))
      (and (memq mask '(0 1))
	   (not (math-infinitep a))
	   (setq mask (logior mask 2))
	   (math-num-integerp a)
	   (setq a (math-add a 1)))
      (setq a (math-ceiling a))
      (and (memq mask '(0 2))
	   (not (math-infinitep b))
	   (setq mask (logior mask 1))
	   (math-num-integerp b)
	   (setq b (math-sub b 1)))
      (setq b (math-floor b))
      (if (and prev (Math-equal (math-sub a 1) (nth 3 prev)))
	  (setcar (nthcdr 3 prev) b)
	(or (Math-lessp b a)
	    (setq vec (cons (setq prev (list 'intv mask a b)) vec)))))
    (setq vec (nreverse vec))
    (math-clean-set vec always-vec)))

(defun calcFunc-vcard (a)
  (setq a (calcFunc-vfloor a t))
  (or (math-constp a) (math-reject-arg a "*Set must be finite"))
  (let ((count 0))
    (while (setq a (cdr a))
      (if (eq (car-safe (car a)) 'intv)
	  (setq count (math-add count (math-sub (nth 3 (car a))
						(nth 2 (car a))))))
      (setq count (math-add count 1)))
    count))

(defun calcFunc-venum (a)
  (setq a (calcFunc-vfloor a t))
  (or (math-constp a) (math-reject-arg a "*Set must be finite"))
  (let* ((prev a) (this (cdr prev)) this-val next this-last)
    (while this
      (setq next (cdr this)
			this-val (car this))
      (if (eq (car-safe this-val) 'intv)
		  (progn
			(setq this (cdr (calcFunc-index (math-add
											 (math-sub (nth 3 this-val)
													   (nth 2 this-val))
											 1)
											(nth 2 this-val))))
			(setq this-last (last this))
			(setcdr this-last next)
			(setcdr prev this)
			(setq prev this-last))
		(setq prev this))
	  (setq this next)))
  a)

(defun calcFunc-vpack (a)
  (setq a (calcFunc-vfloor a t))
  (if (and (cdr a)
	   (math-negp (if (eq (car-safe (nth 1 a)) 'intv)
			  (nth 2 (nth 1 a))
			(nth 1 a))))
      (math-reject-arg (nth 1 a) 'posp))
  (let ((accum 0))
    (while (setq a (cdr a))
      (if (eq (car-safe (car a)) 'intv)
	  (if (equal (nth 3 (car a)) '(var inf var-inf))
	      (setq accum (math-sub accum
				    (math-power-of-2 (nth 2 (car a)))))
	    (setq accum (math-add accum
				  (math-sub
				   (math-power-of-2 (1+ (nth 3 (car a))))
				   (math-power-of-2 (nth 2 (car a)))))))
	(setq accum (math-add accum (math-power-of-2 (car a))))))
    accum))

(defun calcFunc-vunpack (a &optional w)
  (or (math-num-integerp a) (math-reject-arg a 'integerp))
  (if w (setq a (math-clip a w)))
  (if (math-messy-integerp a) (setq a (math-trunc a)))
  (let* ((calc-number-radix 2)
         (calc-twos-complement-mode nil)
	 (neg (math-negp a))
	 (aa (if neg (math-sub -1 a) a))
	 (str (if (eq aa 0)
		  ""
		(if (consp aa)
		    (math-format-bignum-binary (cdr aa))
		  (math-format-binary aa))))
	 (zero (if neg ?1 ?0))
	 (one (if neg ?0 ?1))
	 (len (length str))
	 (vec (list 'vec))
	 (pos (1- len)) pos2)
    (while (>= pos 0)
      (if (eq (aref str pos) zero)
	  (setq pos (1- pos))
	(setq pos2 pos)
	(while (and (>= pos 0) (eq (aref str pos) one))
	  (setq pos (1- pos)))
	(setq vec (cons (if (= pos (1- pos2))
			    (- len pos2 1)
			  (list 'intv 3 (- len pos2 1) (- len pos 2)))
			vec))))
    (if neg
	(setq vec (cons (list 'intv 2 len '(var inf var-inf)) vec)))
    (math-clean-set (nreverse vec))))

(defun calcFunc-rdup (a)
  (if (math-simple-set a)
      (progn
	(and (Math-objectp a) (setq a (list 'vec a)))
	(or (math-vectorp a) (math-reject-arg a 'vectorp))
	(setq a (sort (copy-sequence (cdr a)) 'math-beforep))
	(let ((p a))
	  (while (cdr p)
	    (if (Math-equal (car p) (nth 1 p))
		(setcdr p (cdr (cdr p)))
	      (setq p (cdr p)))))
	(cons 'vec a))
    (math-clean-set (math-prepare-set a))))

(defun math-prepare-set (a)
  (if (Math-objectp a)
      (setq a (list 'vec a))
    (or (math-vectorp a) (math-reject-arg a 'vectorp))
    (setq a (cons 'vec (sort (copy-sequence (cdr a)) 'math-beforep))))
  (let ((p a) res)

    ;; Convert all elements to non-empty intervals.
    (while (cdr p)
      (if (eq (car-safe (nth 1 p)) 'intv)
	  (if (math-intv-constp (nth 1 p))
	      (if (and (memq (nth 1 (nth 1 p)) '(0 1 2))
		       (Math-equal (nth 2 (nth 1 p)) (nth 3 (nth 1 p))))
		  (setcdr p (cdr (cdr p)))
		(setq p (cdr p)))
	    (math-reject-arg (nth 1 p) 'constp))
	(or (Math-anglep (nth 1 p))
	    (eq (car (nth 1 p)) 'date)
	    (equal (nth 1 p) '(var inf var-inf))
	    (equal (nth 1 p) '(neg (var inf var-inf)))
	    (math-reject-arg (nth 1 p) 'realp))
	(setcar (cdr p) (list 'intv 3 (nth 1 p) (nth 1 p)))
	(setq p (cdr p))))

    ;; Combine redundant intervals.
    (setq p a)
    (while (cdr (cdr p))
      (if (or (memq (setq res (math-compare (nth 3 (nth 1 p))
					    (nth 2 (nth 2 p))))
		    '(-1 2))
	      (and (eq res 0)
		   (memq (nth 1 (nth 1 p)) '(0 2))
		   (memq (nth 1 (nth 2 p)) '(0 1))))
	  (setq p (cdr p))
	(setq res (math-compare (nth 3 (nth 1 p)) (nth 3 (nth 2 p))))
	(setcdr p (cons (list 'intv
			      (+ (logand (logior (nth 1 (nth 1 p))
						 (if (Math-equal
						      (nth 2 (nth 1 p))
						      (nth 2 (nth 2 p)))
						     (nth 1 (nth 2 p))
						   0))
					 2)
				 (logand (logior (if (memq res '(1 0 2))
						     (nth 1 (nth 1 p)) 0)
						 (if (memq res '(-1 0 2))
						     (nth 1 (nth 2 p)) 0))
					 1))
			      (nth 2 (nth 1 p))
			      (if (eq res 1)
				  (nth 3 (nth 1 p))
				(nth 3 (nth 2 p))))
			(cdr (cdr (cdr p))))))))
  a)

(defun math-clean-set (a &optional always-vec)
  (let ((p a) res)
    (while (cdr p)
      (if (and (eq (car-safe (nth 1 p)) 'intv)
	       (Math-equal (nth 2 (nth 1 p)) (nth 3 (nth 1 p))))
	  (setcar (cdr p) (nth 2 (nth 1 p))))
      (setq p (cdr p)))
    (if (and (not (cdr (cdr a)))
	     (eq (car-safe (nth 1 a)) 'intv)
	     (not always-vec))
	(nth 1 a)
      a)))

(defun math-simple-set (a)
  (or (and (Math-objectp a)
	   (not (eq (car-safe a) 'intv)))
      (and (Math-vectorp a)
	   (progn
	     (while (and (setq a (cdr a))
			 (not (eq (car-safe (car a)) 'intv))))
	     (null a)))))




;;; Compute a right-handed vector cross product.  [O O O] [Public]
(defun calcFunc-cross (a b)
  (if (and (eq (car-safe a) 'vec)
	   (= (length a) 4))
      (if (and (eq (car-safe b) 'vec)
	       (= (length b) 4))
	  (list 'vec
		(math-sub (math-mul (nth 2 a) (nth 3 b))
			  (math-mul (nth 3 a) (nth 2 b)))
		(math-sub (math-mul (nth 3 a) (nth 1 b))
			  (math-mul (nth 1 a) (nth 3 b)))
		(math-sub (math-mul (nth 1 a) (nth 2 b))
			  (math-mul (nth 2 a) (nth 1 b))))
	(math-reject-arg b "*Three-vector expected"))
    (math-reject-arg a "*Three-vector expected")))


;;; Compute a Kronecker product
(defun calcFunc-kron (x y &optional nocheck)
  "The Kronecker product of objects X and Y.
The objects X and Y may be scalars, vectors or matrices.
The type of the result depends on the types of the operands;
the product of two scalars is a scalar,
of one scalar and a vector is a vector,
of two vectors is a vector.
of one vector and a matrix is a matrix,
of two matrices is a matrix."
  (unless nocheck
    (cond ((or (math-matrixp x)
               (math-matrixp y))
           (unless (math-matrixp x)
             (setq x (if (math-vectorp x)
                         (list 'vec x)
                       (list 'vec (list 'vec x)))))
           (unless (math-matrixp y)
             (setq y (if (math-vectorp y)
                         (list 'vec y)
                       (list 'vec (list 'vec y))))))
          ((or (math-vectorp x)
               (math-vectorp y))
           (unless (math-vectorp x)
             (setq x (list 'vec x)))
           (unless (math-vectorp y)
             (setq y (list 'vec y))))))
  (if (math-vectorp x)
      (let (ret)
        (dolist (v (cdr x))
          (dolist (w (cdr y))
            (setq ret (cons (calcFunc-kron v w t) ret))))
        (cons 'vec (nreverse ret)))
    (math-mul x y)))


;; The variable math-rb-close is local to math-read-brackets, but
;; is used by math-read-vector, which is called (directly and
;; indirectly) by math-read-brackets.
(defvar math-rb-close)

;; The next few variables are local to math-read-exprs in calc-aent.el 
;; and math-read-expr in calc-ext.el, but are set in functions they call.
(defvar math-exp-pos)
(defvar math-exp-str)
(defvar math-exp-old-pos)
(defvar math-exp-token)
(defvar math-exp-keep-spaces)
(defvar math-expr-data)

(defun math-read-brackets (space-sep math-rb-close)
  (and space-sep (setq space-sep (not (math-check-for-commas))))
  (math-read-token)
  (while (eq math-exp-token 'space)
    (math-read-token))
  (if (or (equal math-expr-data math-rb-close)
	  (eq math-exp-token 'end))
      (progn
	(math-read-token)
	'(vec))
    (let ((save-exp-pos math-exp-pos)
	  (save-exp-old-pos math-exp-old-pos)
	  (save-exp-token math-exp-token)
	  (save-exp-data math-expr-data)
	  (vals (let ((math-exp-keep-spaces space-sep))
		  (if (or (equal math-expr-data "\\dots")
			  (equal math-expr-data "\\ldots"))
		      '(vec (neg (var inf var-inf)))
		    (catch 'syntax (math-read-vector))))))
      (if (stringp vals)
	  (if space-sep
	      (let ((error-exp-pos math-exp-pos)
		    (error-exp-old-pos math-exp-old-pos)
		    vals2)
		(setq math-exp-pos save-exp-pos
		      math-exp-old-pos save-exp-old-pos
		      math-exp-token save-exp-token
		      math-expr-data save-exp-data)
		(let ((math-exp-keep-spaces nil))
		  (setq vals2 (catch 'syntax (math-read-vector))))
		(if (and (not (stringp vals2))
			 (or (assoc math-expr-data '(("\\ldots") ("\\dots") (";")))
			     (equal math-expr-data math-rb-close)
			     (eq math-exp-token 'end)))
		    (setq space-sep nil
			  vals vals2)
		  (setq math-exp-pos error-exp-pos
			math-exp-old-pos error-exp-old-pos)
		  (throw 'syntax vals)))
	    (throw 'syntax vals)))
      (if (or (equal math-expr-data "\\dots")
	      (equal math-expr-data "\\ldots"))
	  (progn
	    (math-read-token)
	    (setq vals (if (> (length vals) 2)
			   (cons 'calcFunc-mul (cdr vals)) (nth 1 vals)))
	    (let ((exp2 (if (or (equal math-expr-data math-rb-close)
				(equal math-expr-data ")")
				(eq math-exp-token 'end))
			    '(var inf var-inf)
			  (math-read-expr-level 0))))
	      (setq vals
		    (list 'intv
			  (if (equal math-expr-data ")") 2 3)
			  vals
			  exp2)))
	    (if (not (or (equal math-expr-data math-rb-close)
			 (equal math-expr-data ")")
			 (eq math-exp-token 'end)))
		(throw 'syntax "Expected `]'")))
	(if (equal math-expr-data ";")
	    (let ((math-exp-keep-spaces space-sep))
	      (setq vals (cons 'vec (math-read-matrix (list vals))))))
	(if (not (or (equal math-expr-data math-rb-close)
		     (eq math-exp-token 'end)))
	    (throw 'syntax "Expected `]'")))
      (or (eq math-exp-token 'end)
	  (math-read-token))
      vals)))

(defun math-check-for-commas (&optional balancing)
  (let ((count 0)
	(pos (1- math-exp-pos)))
    (while (and (>= count 0)
		(setq pos (string-match
			   (if balancing "[],[{}()<>]" "[],[{}()]")
			   math-exp-str (1+ pos)))
		(or (/= (aref math-exp-str pos) ?,) (> count 0) balancing))
      (cond ((memq (aref math-exp-str pos) '(?\[ ?\{ ?\( ?\<))
	     (setq count (1+ count)))
	    ((memq (aref math-exp-str pos) '(?\] ?\} ?\) ?\>))
	     (setq count (1- count)))))
    (if balancing
	pos
      (and pos (= (aref math-exp-str pos) ?,)))))

(defun math-read-vector ()
  (let* ((val (list (math-read-expr-level 0)))
	 (last val))
    (while (progn
	     (while (eq math-exp-token 'space)
	       (math-read-token))
	     (and (not (eq math-exp-token 'end))
		  (not (equal math-expr-data ";"))
		  (not (equal math-expr-data math-rb-close))
		  (not (equal math-expr-data "\\dots"))
		  (not (equal math-expr-data "\\ldots"))))
      (if (equal math-expr-data ",")
	  (math-read-token))
      (while (eq math-exp-token 'space)
	(math-read-token))
      (let ((rest (list (math-read-expr-level 0))))
	(setcdr last rest)
	(setq last rest)))
    (cons 'vec val)))

(defun math-read-matrix (mat)
  (while (equal math-expr-data ";")
    (math-read-token)
    (while (eq math-exp-token 'space)
      (math-read-token))
    (setq mat (nconc mat (list (math-read-vector)))))
  mat)

(provide 'calc-vec)

;;; calc-vec.el ends here
