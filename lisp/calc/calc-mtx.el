;;; calc-mtx.el --- matrix functions for Calc

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

(defun calc-mdet (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "mdet" 'calcFunc-det arg)))

(defun calc-mtrace (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "mtr" 'calcFunc-tr arg)))

(defun calc-mlud (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "mlud" 'calcFunc-lud arg)))


;;; Coerce row vector A to be a matrix.  [V V]
(defun math-row-matrix (a)
  (if (and (Math-vectorp a)
	   (not (math-matrixp a)))
      (list 'vec a)
    a))

;;; Coerce column vector A to be a matrix.  [V V]
(defun math-col-matrix (a)
  (if (and (Math-vectorp a)
	   (not (math-matrixp a)))
      (cons 'vec (mapcar (function (lambda (x) (list 'vec x))) (cdr a)))
    a))



;;; Multiply matrices A and B.  [V V V]
(defun math-mul-mats (a b)
  (let ((mat nil)
	(cols (length (nth 1 b)))
	row col ap bp accum)
    (while (setq a (cdr a))
      (setq col cols
	    row nil)
      (while (> (setq col (1- col)) 0)
	(setq ap (cdr (car a))
	      bp (cdr b)
	      accum (math-mul (car ap) (nth col (car bp))))
	(while (setq ap (cdr ap) bp (cdr bp))
	  (setq accum (math-add accum (math-mul (car ap) (nth col (car bp))))))
	(setq row (cons accum row)))
      (setq mat (cons (cons 'vec row) mat)))
    (cons 'vec (nreverse mat))))

(defun math-mul-mat-vec (a b)
  (cons 'vec (mapcar (function (lambda (row)
				 (math-dot-product row b)))
		     (cdr a))))



(defun calcFunc-tr (mat)   ; [Public]
  (if (math-square-matrixp mat)
      (math-matrix-trace-step 2 (1- (length mat)) mat (nth 1 (nth 1 mat)))
    (math-reject-arg mat 'square-matrixp)))

(defun math-matrix-trace-step (n size mat sum)
  (if (<= n size)
      (math-matrix-trace-step (1+ n) size mat
			      (math-add sum (nth n (nth n mat))))
    sum))


;;; Matrix inverse and determinant.
(defun math-matrix-inv-raw (m)
  (let ((n (1- (length m))))
    (if (<= n 3)
	(let ((det (math-det-raw m)))
	  (and (not (math-zerop det))
	       (math-div
		(cond ((= n 1) 1)
		      ((= n 2)
		       (list 'vec
			     (list 'vec
				   (nth 2 (nth 2 m))
				   (math-neg (nth 2 (nth 1 m))))
			     (list 'vec
				   (math-neg (nth 1 (nth 2 m)))
				   (nth 1 (nth 1 m)))))
		      ((= n 3)
		       (list 'vec
			     (list 'vec
				   (math-sub (math-mul (nth 3 (nth 3 m))
						       (nth 2 (nth 2 m)))
					     (math-mul (nth 3 (nth 2 m))
						       (nth 2 (nth 3 m))))
				   (math-sub (math-mul (nth 3 (nth 1 m))
						       (nth 2 (nth 3 m)))
					     (math-mul (nth 3 (nth 3 m))
						       (nth 2 (nth 1 m))))
				   (math-sub (math-mul (nth 3 (nth 2 m))
						       (nth 2 (nth 1 m)))
					     (math-mul (nth 3 (nth 1 m))
						       (nth 2 (nth 2 m)))))
			     (list 'vec
				   (math-sub (math-mul (nth 3 (nth 2 m))
						       (nth 1 (nth 3 m)))
					     (math-mul (nth 3 (nth 3 m))
						       (nth 1 (nth 2 m))))
				   (math-sub (math-mul (nth 3 (nth 3 m))
						       (nth 1 (nth 1 m)))
					     (math-mul (nth 3 (nth 1 m))
						       (nth 1 (nth 3 m))))
				   (math-sub (math-mul (nth 3 (nth 1 m))
						       (nth 1 (nth 2 m)))
					     (math-mul (nth 3 (nth 2 m))
						       (nth 1 (nth 1 m)))))
			     (list 'vec
				   (math-sub (math-mul (nth 2 (nth 3 m))
						       (nth 1 (nth 2 m)))
					     (math-mul (nth 2 (nth 2 m))
						       (nth 1 (nth 3 m))))
				   (math-sub (math-mul (nth 2 (nth 1 m))
						       (nth 1 (nth 3 m)))
					     (math-mul (nth 2 (nth 3 m))
						       (nth 1 (nth 1 m))))
				   (math-sub (math-mul (nth 2 (nth 2 m))
						       (nth 1 (nth 1 m)))
					     (math-mul (nth 2 (nth 1 m))
						       (nth 1 (nth 2 m))))))))
		det)))
      (let ((lud (math-matrix-lud m)))
	(and lud
	     (math-lud-solve lud (calcFunc-idn 1 n)))))))

(defun calcFunc-det (m)
  (if (math-square-matrixp m)
      (math-with-extra-prec 2 (math-det-raw m))
    (if (and (eq (car-safe m) 'calcFunc-idn)
	     (or (math-zerop (nth 1 m))
		 (math-equal-int (nth 1 m) 1)))
	(nth 1 m)
      (math-reject-arg m 'square-matrixp))))

;; The variable math-det-lu is local to math-det-raw, but is
;; used by math-det-step, which is called by math-det-raw.
(defvar math-det-lu)

(defun math-det-raw (m)
  (let ((n (1- (length m))))
    (cond ((= n 1)
	   (nth 1 (nth 1 m)))
	  ((= n 2)
	   (math-sub (math-mul (nth 1 (nth 1 m))
			       (nth 2 (nth 2 m)))
		     (math-mul (nth 2 (nth 1 m))
			       (nth 1 (nth 2 m)))))
	  ((= n 3)
	   (math-sub
	    (math-sub
	     (math-sub
	      (math-add
	       (math-add
		(math-mul (nth 1 (nth 1 m))
			  (math-mul (nth 2 (nth 2 m))
				    (nth 3 (nth 3 m))))
		(math-mul (nth 2 (nth 1 m))
			  (math-mul (nth 3 (nth 2 m))
				    (nth 1 (nth 3 m)))))
	       (math-mul (nth 3 (nth 1 m))
			 (math-mul (nth 1 (nth 2 m))
				   (nth 2 (nth 3 m)))))
	      (math-mul (nth 3 (nth 1 m))
			(math-mul (nth 2 (nth 2 m))
				  (nth 1 (nth 3 m)))))
	     (math-mul (nth 1 (nth 1 m))
		       (math-mul (nth 3 (nth 2 m))
				 (nth 2 (nth 3 m)))))
	    (math-mul (nth 2 (nth 1 m))
		      (math-mul (nth 1 (nth 2 m))
				(nth 3 (nth 3 m))))))
	  (t (let ((lud (math-matrix-lud m)))
	       (if lud
		   (let ((math-det-lu (car lud)))
		     (math-det-step n (nth 2 lud)))
		 0))))))

(defun math-det-step (n prod)
  (if (> n 0)
      (math-det-step (1- n) (math-mul prod (nth n (nth n math-det-lu))))
    prod))

;;; This returns a list (LU index d), or nil if not possible.
;;; Argument M must be a square matrix.
(defvar math-lud-cache nil)
(defun math-matrix-lud (m)
  (let ((old (assoc m math-lud-cache))
	(context (list calc-internal-prec calc-prefer-frac)))
    (if (and old (equal (nth 1 old) context))
	(cdr (cdr old))
      (let* ((lud (catch 'singular (math-do-matrix-lud m)))
	     (entry (cons context lud)))
	(if old
	    (setcdr old entry)
	  (setq math-lud-cache (cons (cons m entry) math-lud-cache)))
	lud))))


(defun math-lud-pivot-check (a)
  "Determine a useful value for checking the size of potential pivots
in LUD decomposition."
  (cond ((eq (car-safe a) 'mod)
         (if (and (math-integerp (nth 1 a))
                  (math-integerp (nth 2 a))
                  (eq (math-gcd (nth 1 a) (nth 2 a)) 1))
             1
           0))
        (t
         (math-abs-approx a))))


;;; Numerical Recipes section 2.3; implicit pivoting omitted.
(defun math-do-matrix-lud (m)
  (let* ((lu (math-copy-matrix m))
	 (n (1- (length lu)))
	 i (j 1) k imax sum big
	 (d 1) (index nil))
    (while (<= j n)
      (setq i 1
	    big 0
	    imax j)
      (while (< i j)
	(math-working "LUD step" (format "%d/%d" j i))
	(setq sum (nth j (nth i lu))
	      k 1)
	(while (< k i)
	  (setq sum (math-sub sum (math-mul (nth k (nth i lu))
					    (nth j (nth k lu))))
		k (1+ k)))
	(setcar (nthcdr j (nth i lu)) sum)
	(setq i (1+ i)))
      (while (<= i n)
	(math-working "LUD step" (format "%d/%d" j i))
	(setq sum (nth j (nth i lu))
	      k 1)
	(while (< k j)
	  (setq sum (math-sub sum (math-mul (nth k (nth i lu))
					    (nth j (nth k lu))))
		k (1+ k)))
	(setcar (nthcdr j (nth i lu)) sum)
	(let ((dum (math-lud-pivot-check sum)))
	  (if (Math-lessp big dum)
	      (setq big dum
		    imax i)))
	(setq i (1+ i)))
      (if (> imax j)
	  (setq lu (math-swap-rows lu j imax)
		d (- d)))
      (setq index (cons imax index))
      (let ((pivot (nth j (nth j lu))))
	(if (math-zerop pivot)
	    (throw 'singular nil)
	  (setq i j)
	  (while (<= (setq i (1+ i)) n)
	    (setcar (nthcdr j (nth i lu))
		    (math-div (nth j (nth i lu)) pivot)))))
      (setq j (1+ j)))
    (list lu (nreverse index) d)))

(defun math-swap-rows (m r1 r2)
  (or (= r1 r2)
      (let* ((r1prev (nthcdr (1- r1) m))
	     (row1 (cdr r1prev))
	     (r2prev (nthcdr (1- r2) m))
	     (row2 (cdr r2prev))
	     (r2next (cdr row2)))
	(setcdr r2prev row1)
	(setcdr r1prev row2)
	(setcdr row2 (cdr row1))
	(setcdr row1 r2next)))
  m)


(defun math-lud-solve (lud b &optional need)
  (if lud
      (let* ((x (math-copy-matrix b))
	     (n (1- (length x)))
	     (m (1- (length (nth 1 x))))
	     (lu (car lud))
	     (col 1)
	     i j ip ii index sum)
	(while (<= col m)
	  (math-working "LUD solver step" col)
	  (setq i 1
		ii nil
		index (nth 1 lud))
	  (while (<= i n)
	    (setq ip (car index)
		  index (cdr index)
		  sum (nth col (nth ip x)))
	    (setcar (nthcdr col (nth ip x)) (nth col (nth i x)))
	    (if (null ii)
		(or (math-zerop sum)
		    (setq ii i))
	      (setq j ii)
	      (while (< j i)
		(setq sum (math-sub sum (math-mul (nth j (nth i lu))
						  (nth col (nth j x))))
		      j (1+ j))))
	    (setcar (nthcdr col (nth i x)) sum)
	    (setq i (1+ i)))
	  (while (>= (setq i (1- i)) 1)
	    (setq sum (nth col (nth i x))
		  j i)
	    (while (<= (setq j (1+ j)) n)
	      (setq sum (math-sub sum (math-mul (nth j (nth i lu))
						(nth col (nth j x))))))
	    (setcar (nthcdr col (nth i x))
		    (math-div sum (nth i (nth i lu)))))
	  (setq col (1+ col)))
	x)
    (and need
	 (math-reject-arg need "*Singular matrix"))))

(defun calcFunc-lud (m)
  (if (math-square-matrixp m)
      (or (math-with-extra-prec 2
	    (let ((lud (math-matrix-lud m)))
	      (and lud
		   (let* ((lmat (math-copy-matrix (car lud)))
			  (umat (math-copy-matrix (car lud)))
			  (n (1- (length (car lud))))
			  (perm (calcFunc-idn 1 n))
			  i (j 1))
		     (while (<= j n)
		       (setq i 1)
		       (while (< i j)
			 (setcar (nthcdr j (nth i lmat)) 0)
			 (setq i (1+ i)))
		       (setcar (nthcdr j (nth j lmat)) 1)
		       (while (<= (setq i (1+ i)) n)
			 (setcar (nthcdr j (nth i umat)) 0))
		       (setq j (1+ j)))
		     (while (>= (setq j (1- j)) 1)
		       (let ((pos (nth (1- j) (nth 1 lud))))
			 (or (= pos j)
			     (setq perm (math-swap-rows perm j pos)))))
		     (list 'vec perm lmat umat)))))
	  (math-reject-arg m "*Singular matrix"))
    (math-reject-arg m 'square-matrixp)))

(provide 'calc-mtx)

;;; calc-mtx.el ends here
