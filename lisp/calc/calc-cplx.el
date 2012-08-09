;;; calc-cplx.el --- Complex number functions for Calc

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

(defun calc-argument (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "arg" 'calcFunc-arg arg)))

(defun calc-re (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "re" 'calcFunc-re arg)))

(defun calc-im (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "im" 'calcFunc-im arg)))


(defun calc-polar ()
  (interactive)
  (calc-slow-wrapper
   (let ((arg (calc-top-n 1)))
     (if (or (calc-is-inverse)
	     (eq (car-safe arg) 'polar))
	 (calc-enter-result 1 "p-r" (list 'calcFunc-rect arg))
       (calc-enter-result 1 "r-p" (list 'calcFunc-polar arg))))))




(defun calc-complex-notation ()
  (interactive)
  (calc-wrapper
   (calc-change-mode 'calc-complex-format nil t)
   (message "Displaying complex numbers in (X,Y) format")))

(defun calc-i-notation ()
  (interactive)
  (calc-wrapper
   (calc-change-mode 'calc-complex-format 'i t)
   (message "Displaying complex numbers in X+Yi format")))

(defun calc-j-notation ()
  (interactive)
  (calc-wrapper
   (calc-change-mode 'calc-complex-format 'j t)
   (message "Displaying complex numbers in X+Yj format")))


(defun calc-polar-mode (n)
  (interactive "P")
  (calc-wrapper
   (if (if n
	   (> (prefix-numeric-value n) 0)
	 (eq calc-complex-mode 'cplx))
       (progn
	 (calc-change-mode 'calc-complex-mode 'polar)
	 (message "Preferred complex form is polar"))
     (calc-change-mode 'calc-complex-mode 'cplx)
     (message "Preferred complex form is rectangular"))))


;;;; Complex numbers.

(defun math-normalize-polar (a)
  (let ((r (math-normalize (nth 1 a)))
	(th (math-normalize (nth 2 a))))
    (cond ((math-zerop r)
	   '(polar 0 0))
	  ((or (math-zerop th))
	   r)
	  ((and (not (eq calc-angle-mode 'rad))
		(or (equal th '(float 18 1))
		    (equal th 180)))
	   (math-neg r))
	  ((math-negp r)
	   (math-neg (list 'polar (math-neg r) th)))
	  (t
	   (list 'polar r th)))))


;;; Coerce A to be complex (rectangular form).  [c N]
(defun math-complex (a)
  (cond ((eq (car-safe a) 'cplx) a)
	((eq (car-safe a) 'polar)
	 (if (math-zerop (nth 1 a))
	     (nth 1 a)
	   (let ((sc (calcFunc-sincos (nth 2 a))))
	     (list 'cplx
		   (math-mul (nth 1 a) (nth 1 sc))
		   (math-mul (nth 1 a) (nth 2 sc))))))
	(t (list 'cplx a 0))))

;;; Coerce A to be complex (polar form).  [c N]
(defun math-polar (a)
  (cond ((eq (car-safe a) 'polar) a)
	((math-zerop a) '(polar 0 0))
	(t
	 (list 'polar
	       (math-abs a)
	       (calcFunc-arg a)))))

;;; Multiply A by the imaginary constant i.  [N N] [Public]
(defun math-imaginary (a)
  (if (and (or (Math-objvecp a) (math-infinitep a))
	   (not calc-symbolic-mode))
      (math-mul a
		(if (or (eq (car-safe a) 'polar)
			(and (not (eq (car-safe a) 'cplx))
			     (eq calc-complex-mode 'polar)))
		    (list 'polar 1 (math-quarter-circle nil))
		  '(cplx 0 1)))
    (math-mul a '(var i var-i))))




(defun math-want-polar (a b)
  (cond ((eq (car-safe a) 'polar)
	 (if (eq (car-safe b) 'cplx)
	     (eq calc-complex-mode 'polar)
	   t))
	((eq (car-safe a) 'cplx)
	 (if (eq (car-safe b) 'polar)
	     (eq calc-complex-mode 'polar)
	   nil))
	((eq (car-safe b) 'polar)
	 t)
	((eq (car-safe b) 'cplx)
	 nil)
	(t (eq calc-complex-mode 'polar))))

;;; Force A to be in the (-pi,pi] or (-180,180] range.
(defun math-fix-circular (a &optional dir)   ; [R R]
  (cond ((eq (car-safe a) 'hms)
	 (cond ((and (Math-lessp 180 (nth 1 a)) (not (eq dir 1)))
		(math-fix-circular (math-add a '(float -36 1)) -1))
	       ((or (Math-lessp -180 (nth 1 a)) (eq dir -1))
		a)
	       (t
		(math-fix-circular (math-add a '(float 36 1)) 1))))
	((eq calc-angle-mode 'rad)
	 (cond ((and (Math-lessp (math-pi) a) (not (eq dir 1)))
		(math-fix-circular (math-sub a (math-two-pi)) -1))
	       ((or (Math-lessp (math-neg (math-pi)) a) (eq dir -1))
		a)
	       (t
		(math-fix-circular (math-add a (math-two-pi)) 1))))
	(t
	 (cond ((and (Math-lessp '(float 18 1) a) (not (eq dir 1)))
		(math-fix-circular (math-add a '(float -36 1)) -1))
	       ((or (Math-lessp '(float -18 1) a) (eq dir -1))
		a)
	       (t
		(math-fix-circular (math-add a '(float 36 1)) 1))))))


;;;; Complex numbers.

(defun calcFunc-polar (a)   ; [C N] [Public]
  (cond ((Math-vectorp a)
	 (math-map-vec 'calcFunc-polar a))
	((Math-realp a) a)
	((Math-numberp a)
	 (math-normalize (math-polar a)))
	(t (list 'calcFunc-polar a))))

(defun calcFunc-rect (a)   ; [N N] [Public]
  (cond ((Math-vectorp a)
	 (math-map-vec 'calcFunc-rect a))
	((Math-realp a) a)
	((Math-numberp a)
	 (math-normalize (math-complex a)))
	(t (list 'calcFunc-rect a))))

;;; Compute the complex conjugate of A.  [O O] [Public]
(defun calcFunc-conj (a)
  (let (aa bb)
    (cond ((Math-realp a)
	   a)
	  ((eq (car a) 'cplx)
	   (list 'cplx (nth 1 a) (math-neg (nth 2 a))))
	  ((eq (car a) 'polar)
	   (list 'polar (nth 1 a) (math-neg (nth 2 a))))
	  ((eq (car a) 'vec)
	   (math-map-vec 'calcFunc-conj a))
	  ((eq (car a) 'calcFunc-conj)
	   (nth 1 a))
	  ((math-known-realp a)
	   a)
	  ((and (equal a '(var i var-i))
		(math-imaginary-i))
	   (math-neg a))
	  ((and (memq (car a) '(+ - * /))
		(progn
		  (setq aa (calcFunc-conj (nth 1 a))
			bb (calcFunc-conj (nth 2 a)))
		  (or (not (eq (car-safe aa) 'calcFunc-conj))
		      (not (eq (car-safe bb) 'calcFunc-conj)))))
	   (if (eq (car a) '+)
	       (math-add aa bb)
	     (if (eq (car a) '-)
		 (math-sub aa bb)
	       (if (eq (car a) '*)
		   (math-mul aa bb)
		 (math-div aa bb)))))
	  ((eq (car a) 'neg)
	   (math-neg (calcFunc-conj (nth 1 a))))
	  ((let ((inf (math-infinitep a)))
	     (and inf
		  (math-mul (calcFunc-conj (math-infinite-dir a inf)) inf))))
	  (t (calc-record-why 'numberp a)
	     (list 'calcFunc-conj a)))))


;;; Compute the complex argument of A.  [F N] [Public]
(defun calcFunc-arg (a)
  (cond ((Math-anglep a)
	 (if (math-negp a) (math-half-circle nil) 0))
	((eq (car-safe a) 'cplx)
	 (calcFunc-arctan2 (nth 2 a) (nth 1 a)))
	((eq (car-safe a) 'polar)
	 (nth 2 a))
	((eq (car a) 'vec)
	 (math-map-vec 'calcFunc-arg a))
	((and (equal a '(var i var-i))
	      (math-imaginary-i))
	 (math-quarter-circle t))
	((and (equal a '(neg (var i var-i)))
	      (math-imaginary-i))
	 (math-neg (math-quarter-circle t)))
	((let ((signs (math-possible-signs a)))
	   (or (and (memq signs '(2 4 6)) 0)
	       (and (eq signs 1) (math-half-circle nil)))))
	((math-infinitep a)
	 (if (or (equal a '(var uinf var-uinf))
		 (equal a '(var nan var-nan)))
	     '(var nan var-nan)
	   (calcFunc-arg (math-infinite-dir a))))
	(t (calc-record-why 'numvecp a)
	   (list 'calcFunc-arg a))))

(defun math-imaginary-i ()
  (let ((val (calc-var-value 'var-i)))
    (or (eq (car-safe val) 'special-const)
	(equal val '(cplx 0 1))
	(and (eq (car-safe val) 'polar)
	     (eq (nth 1 val) 0)
	     (Math-equal (nth 1 val) (math-quarter-circle nil))))))

;;; Extract the real or complex part of a complex number.  [R N] [Public]
;;; Also extracts the real part of a modulo form.
(defun calcFunc-re (a)
  (let (aa bb)
    (cond ((Math-realp a) a)
	  ((memq (car a) '(mod cplx))
	   (nth 1 a))
	  ((eq (car a) 'polar)
	   (math-mul (nth 1 a) (calcFunc-cos (nth 2 a))))
	  ((eq (car a) 'vec)
	   (math-map-vec 'calcFunc-re a))
	  ((math-known-realp a) a)
	  ((eq (car a) 'calcFunc-conj)
	   (calcFunc-re (nth 1 a)))
	  ((and (equal a '(var i var-i))
		(math-imaginary-i))
	   0)
	  ((and (memq (car a) '(+ - *))
		(progn
		  (setq aa (calcFunc-re (nth 1 a))
			bb (calcFunc-re (nth 2 a)))
		  (or (not (eq (car-safe aa) 'calcFunc-re))
		      (not (eq (car-safe bb) 'calcFunc-re)))))
	   (if (eq (car a) '+)
	       (math-add aa bb)
	     (if (eq (car a) '-)
		 (math-sub aa bb)
	       (math-sub (math-mul aa bb)
			 (math-mul (calcFunc-im (nth 1 a))
				   (calcFunc-im (nth 2 a)))))))
	  ((and (eq (car a) '/)
		(math-known-realp (nth 2 a)))
	   (math-div (calcFunc-re (nth 1 a)) (nth 2 a)))
	  ((eq (car a) 'neg)
	   (math-neg (calcFunc-re (nth 1 a))))
	  (t (calc-record-why 'numberp a)
	     (list 'calcFunc-re a)))))

(defun calcFunc-im (a)
  (let (aa bb)
    (cond ((Math-realp a)
	   (if (math-floatp a) '(float 0 0) 0))
	  ((eq (car a) 'cplx)
	   (nth 2 a))
	  ((eq (car a) 'polar)
	   (math-mul (nth 1 a) (calcFunc-sin (nth 2 a))))
	  ((eq (car a) 'vec)
	   (math-map-vec 'calcFunc-im a))
	  ((math-known-realp a)
	   0)
	  ((eq (car a) 'calcFunc-conj)
	   (math-neg (calcFunc-im (nth 1 a))))
	  ((and (equal a '(var i var-i))
		(math-imaginary-i))
	   1)
	  ((and (memq (car a) '(+ - *))
		(progn
		  (setq aa (calcFunc-im (nth 1 a))
			bb (calcFunc-im (nth 2 a)))
		  (or (not (eq (car-safe aa) 'calcFunc-im))
		      (not (eq (car-safe bb) 'calcFunc-im)))))
	   (if (eq (car a) '+)
	       (math-add aa bb)
	     (if (eq (car a) '-)
		 (math-sub aa bb)
	       (math-add (math-mul (calcFunc-re (nth 1 a)) bb)
			 (math-mul aa (calcFunc-re (nth 2 a)))))))
	  ((and (eq (car a) '/)
		(math-known-realp (nth 2 a)))
	   (math-div (calcFunc-im (nth 1 a)) (nth 2 a)))
	  ((eq (car a) 'neg)
	   (math-neg (calcFunc-im (nth 1 a))))
	  (t (calc-record-why 'numberp a)
	     (list 'calcFunc-im a)))))

(provide 'calc-cplx)

;;; calc-cplx.el ends here
