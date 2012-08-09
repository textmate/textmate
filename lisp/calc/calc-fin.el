;;; calc-fin.el --- financial functions for Calc

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

;;; Financial functions.

(defun calc-fin-pv ()
  (interactive)
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-enter-result 3 "pvl" (cons 'calcFunc-pvl (calc-top-list-n 3)))
     (if (calc-is-inverse)
	 (calc-enter-result 3 "pvb" (cons 'calcFunc-pvb (calc-top-list-n 3)))
       (calc-enter-result 3 "pv" (cons 'calcFunc-pv (calc-top-list-n 3)))))))

(defun calc-fin-npv (arg)
  (interactive "p")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-vector-op "npvb" 'calcFunc-npvb (1+ arg))
     (calc-vector-op "npv" 'calcFunc-npv (1+ arg)))))

(defun calc-fin-fv ()
  (interactive)
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-enter-result 3 "fvl" (cons 'calcFunc-fvl (calc-top-list-n 3)))
     (if (calc-is-inverse)
	 (calc-enter-result 3 "fvb" (cons 'calcFunc-fvb (calc-top-list-n 3)))
       (calc-enter-result 3 "fv" (cons 'calcFunc-fv (calc-top-list-n 3)))))))

(defun calc-fin-pmt ()
  (interactive)
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-enter-result 3 "fvl" (cons 'calcFunc-fvl (calc-top-list-n 3)))
     (if (calc-is-inverse)
	 (calc-enter-result 3 "pmtb" (cons 'calcFunc-pmtb (calc-top-list-n 3)))
       (calc-enter-result 3 "pmt" (cons 'calcFunc-pmt (calc-top-list-n 3)))))))

(defun calc-fin-nper ()
  (interactive)
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-enter-result 3 "nprl" (cons 'calcFunc-nperl (calc-top-list-n 3)))
     (if (calc-is-inverse)
	 (calc-enter-result 3 "nprb" (cons 'calcFunc-nperb
					   (calc-top-list-n 3)))
       (calc-enter-result 3 "nper" (cons 'calcFunc-nper
					 (calc-top-list-n 3)))))))

(defun calc-fin-rate ()
  (interactive)
  (calc-slow-wrapper
   (calc-pop-push-record 3
			 (if (calc-is-hyperbolic) "ratl"
			   (if (calc-is-inverse) "ratb" "rate"))
			 (calc-to-percentage
			  (calc-normalize
			   (cons (if (calc-is-hyperbolic) 'calcFunc-ratel
				   (if (calc-is-hyperbolic) 'calcFunc-rateb
				     'calcFunc-rate))
				 (calc-top-list-n 3)))))))

(defun calc-fin-irr (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (calc-vector-op "irrb" 'calcFunc-irrb arg)
     (calc-vector-op "irr" 'calcFunc-irr arg))))

(defun calc-fin-sln ()
  (interactive)
  (calc-slow-wrapper
   (calc-enter-result 3 "sln" (cons 'calcFunc-sln (calc-top-list-n 3)))))

(defun calc-fin-syd ()
  (interactive)
  (calc-slow-wrapper
   (calc-enter-result 4 "syd" (cons 'calcFunc-syd (calc-top-list-n 4)))))

(defun calc-fin-ddb ()
  (interactive)
  (calc-slow-wrapper
   (calc-enter-result 4 "ddb" (cons 'calcFunc-ddb (calc-top-list-n 4)))))


(defun calc-to-percentage (x)
  (cond ((Math-objectp x)
	 (setq x (math-mul x 100))
	 (if (Math-num-integerp x)
	     (setq x (math-trunc x)))
	 (list 'calcFunc-percent x))
	((Math-vectorp x)
	 (cons 'vec (mapcar 'calc-to-percentage (cdr x))))
	(t x)))

(defun calc-convert-percent ()
  (interactive)
  (calc-slow-wrapper
   (calc-pop-push-record 1 "c%" (calc-to-percentage (calc-top-n 1)))))

(defun calc-percent-change ()
  (interactive)
  (calc-slow-wrapper
   (let ((res (calc-normalize (cons 'calcFunc-relch (calc-top-list 2)))))
     (calc-pop-push-record 2 "%ch" (calc-to-percentage res)))))


;;; Financial functions.

(defun calcFunc-pv (rate num amount &optional lump)
  (math-check-financial rate num)
  (math-with-extra-prec 2
    (let ((p (math-pow (math-add 1 rate) num)))
      (math-add (math-mul amount
			  (math-div (math-sub 1 (math-div 1 p))
				    rate))
		(math-div (or lump 0) p)))))
(put 'calcFunc-pv 'math-expandable t)

(defun calcFunc-pvl (rate num amount)
  (calcFunc-pv rate num 0 amount))
(put 'calcFunc-pvl 'math-expandable t)

(defun calcFunc-pvb (rate num amount &optional lump)
  (math-check-financial rate num)
  (math-with-extra-prec 2
    (let* ((p (math-pow (math-add 1 rate) num)))
      (math-add (math-mul amount
			  (math-div (math-mul (math-sub 1 (math-div 1 p))
					      (math-add 1 rate))
				    rate))
		(math-div (or lump 0) p)))))
(put 'calcFunc-pvb 'math-expandable t)

(defun calcFunc-npv (rate &rest flows)
  (math-check-financial rate 1)
  (math-with-extra-prec 2
    (let* ((flat (math-flatten-many-vecs flows))
	   (pp (math-add 1 rate))
	   (p pp)
	   (accum 0))
      (while (setq flat (cdr flat))
	(setq accum (math-add accum (math-div (car flat) p))
	      p (math-mul p pp)))
      accum)))
(put 'calcFunc-npv 'math-expandable t)

(defun calcFunc-npvb (rate &rest flows)
  (math-check-financial rate 1)
  (math-with-extra-prec 2
    (let* ((flat (math-flatten-many-vecs flows))
	   (pp (math-add 1 rate))
	   (p 1)
	   (accum 0))
      (while (setq flat (cdr flat))
	(setq accum (math-add accum (math-div (car flat) p))
	      p (math-mul p pp)))
      accum)))
(put 'calcFunc-npvb 'math-expandable t)

(defun calcFunc-fv (rate num amount &optional initial)
  (math-check-financial rate num)
  (math-with-extra-prec 2
    (let ((p (math-pow (math-add 1 rate) num)))
      (math-add (math-mul amount
			  (math-div (math-sub p 1)
				    rate))
		(math-mul (or initial 0) p)))))
(put 'calcFunc-fv 'math-expandable t)

(defun calcFunc-fvl (rate num amount)
  (calcFunc-fv rate num 0 amount))
(put 'calcFunc-fvl 'math-expandable t)

(defun calcFunc-fvb (rate num amount &optional initial)
  (math-check-financial rate num)
  (math-with-extra-prec 2
    (let ((p (math-pow (math-add 1 rate) num)))
      (math-add (math-mul amount
			  (math-div (math-mul (math-sub p 1)
					      (math-add 1 rate))
				    rate))
		(math-mul (or initial 0) p)))))
(put 'calcFunc-fvb 'math-expandable t)

(defun calcFunc-pmt (rate num amount &optional lump)
  (math-check-financial rate num)
  (math-with-extra-prec 2
    (let ((p (math-pow (math-add 1 rate) num)))
      (math-div (math-mul (math-sub amount
				    (math-div (or lump 0) p))
			  rate)
		(math-sub 1 (math-div 1 p))))))
(put 'calcFunc-pmt 'math-expandable t)

(defun calcFunc-pmtb (rate num amount &optional lump)
  (math-check-financial rate num)
  (math-with-extra-prec 2
    (let ((p (math-pow (math-add 1 rate) num)))
      (math-div (math-mul (math-sub amount (math-div (or lump 0) p)) rate)
		(math-mul (math-sub 1 (math-div 1 p))
			  (math-add 1 rate))))))
(put 'calcFunc-pmtb 'math-expandable t)

(defun calcFunc-nper (rate pmt amount &optional lump)
  (math-compute-nper rate pmt amount lump nil))
(put 'calcFunc-nper 'math-expandable t)

(defun calcFunc-nperb (rate pmt amount &optional lump)
  (math-compute-nper rate pmt amount lump 'b))
(put 'calcFunc-nperb 'math-expandable t)

(defun calcFunc-nperl (rate pmt amount)
  (math-compute-nper rate pmt amount nil 'l))
(put 'calcFunc-nperl 'math-expandable t)

(defun math-compute-nper (rate pmt amount lump bflag)
  (and lump (math-zerop lump)
       (setq lump nil))
  (and lump (math-zerop pmt)
       (setq amount lump
	     lump nil
	     bflag 'l))
  (or (math-objectp rate) (and math-expand-formulas (null lump))
      (math-reject-arg rate 'numberp))
  (and (math-zerop rate)
       (math-reject-arg rate 'nonzerop))
  (or (math-objectp pmt) (and math-expand-formulas (null lump))
      (math-reject-arg pmt 'numberp))
  (or (math-objectp amount) (and math-expand-formulas (null lump))
      (math-reject-arg amount 'numberp))
  (if lump
      (progn
	(or (math-objectp lump)
	    (math-reject-arg lump 'numberp))
	(let ((root (math-find-root (list 'calcFunc-eq
					  (list (if bflag
						    'calcFunc-pvb
						  'calcFunc-pv)
						rate
						'(var DUMMY var-DUMMY)
						pmt
						lump)
					  amount)
				    '(var DUMMY var-DUMMY)
				    '(intv 3 0 100)
				    t)))
	  (if (math-vectorp root)
	      (nth 1 root)
	    root)))
    (math-with-extra-prec 2
      (let ((temp (if (eq bflag 'l)
		      (math-div amount pmt)
		    (math-sub 1 (math-div (math-mul amount rate)
					  (if bflag
					      (math-mul pmt (math-add 1 rate))
					    pmt))))))
	(if (or (math-posp temp) math-expand-formulas)
	    (math-neg (calcFunc-log temp (math-add 1 rate)))
	  (math-reject-arg pmt "*Payment too small to cover interest rate"))))))

(defun calcFunc-rate (num pmt amount &optional lump)
  (math-compute-rate num pmt amount lump 'calcFunc-pv))

(defun calcFunc-rateb (num pmt amount &optional lump)
  (math-compute-rate num pmt amount lump 'calcFunc-pvb))

(defun math-compute-rate (num pmt amount lump func)
  (or (math-objectp num)
      (math-reject-arg num 'numberp))
  (or (math-objectp pmt)
      (math-reject-arg pmt 'numberp))
  (or (math-objectp amount)
      (math-reject-arg amount 'numberp))
  (or (null lump)
      (math-objectp lump)
      (math-reject-arg lump 'numberp))
  (let ((root (math-find-root (list 'calcFunc-eq
				    (list func
					  '(var DUMMY var-DUMMY)
					  num
					  pmt
					  (or lump 0))
				    amount)
			      '(var DUMMY var-DUMMY)
			      '(intv 3 (float 1 -4) 1)
			      t)))
    (if (math-vectorp root)
	(nth 1 root)
      root)))

(defun calcFunc-ratel (num pmt amount)
  (or (math-objectp num) math-expand-formulas
      (math-reject-arg num 'numberp))
  (or (math-objectp pmt) math-expand-formulas
      (math-reject-arg pmt 'numberp))
  (or (math-objectp amount) math-expand-formulas
      (math-reject-arg amount 'numberp))
  (math-with-extra-prec 2
    (math-sub (math-pow (math-div pmt amount) (math-div 1 num)) 1)))

(defun calcFunc-irr (&rest vecs)
  (math-compute-irr vecs 'calcFunc-npv))

(defun calcFunc-irrb (&rest vecs)
  (math-compute-irr vecs 'calcFunc-npvb))

(defun math-compute-irr (vecs func)
  (let* ((flat (math-flatten-many-vecs vecs))
	 (root (math-find-root (list func
				     '(var DUMMY var-DUMMY)
				     flat)
			       '(var DUMMY var-DUMMY)
			       '(intv 3 (float 1 -4) 1)
			       t)))
    (if (math-vectorp root)
	(nth 1 root)
      root)))

(defun math-check-financial (rate num)
  (or (math-objectp rate) math-expand-formulas
      (math-reject-arg rate 'numberp))
  (and (math-zerop rate)
       (math-reject-arg rate 'nonzerop))
  (or (math-objectp num) math-expand-formulas
      (math-reject-arg num 'numberp)))


(defun calcFunc-sln (cost salvage life &optional period)
  (or (math-realp cost) math-expand-formulas
      (math-reject-arg cost 'realp))
  (or (math-realp salvage) math-expand-formulas
      (math-reject-arg salvage 'realp))
  (or (math-realp life) math-expand-formulas
      (math-reject-arg life 'realp))
  (if (math-zerop life) (math-reject-arg life 'nonzerop))
  (if (and period
	   (if (math-num-integerp period)
	       (or (Math-lessp life period) (not (math-posp period)))
	     (math-reject-arg period 'integerp)))
      0
    (math-div (math-sub cost salvage) life)))
(put 'calcFunc-sln 'math-expandable t)

(defun calcFunc-syd (cost salvage life period)
  (or (math-realp cost) math-expand-formulas
      (math-reject-arg cost 'realp))
  (or (math-realp salvage) math-expand-formulas
      (math-reject-arg salvage 'realp))
  (or (math-realp life) math-expand-formulas
      (math-reject-arg life 'realp))
  (if (math-zerop life) (math-reject-arg life 'nonzerop))
  (or (math-realp period) math-expand-formulas
      (math-reject-arg period 'realp))
  (if (or (Math-lessp life period) (not (math-posp period)))
      0
    (math-div (math-mul (math-sub cost salvage)
			(math-add (math-sub life period) 1))
	      (math-div (math-mul life (math-add life 1)) 2))))
(put 'calcFunc-syd 'math-expandable t)

(defun calcFunc-ddb (cost salvage life period)
  (if (math-messy-integerp period) (setq period (math-trunc period)))
  (or (integerp period) (math-reject-arg period 'fixnump))
  (or (math-realp cost) (math-reject-arg cost 'realp))
  (or (math-realp salvage) (math-reject-arg salvage 'realp))
  (or (math-realp life) (math-reject-arg life 'realp))
  (if (math-zerop life) (math-reject-arg life 'nonzerop))
  (if (or (Math-lessp life period) (<= period 0))
      0
    (let ((book cost)
	  (res 0))
      (while (>= (setq period (1- period)) 0)
	(setq res (math-div (math-mul book 2) life)
	      book (math-sub book res))
	(if (Math-lessp book salvage)
	    (setq res (math-add res (math-sub book salvage))
		  book salvage)))
      res)))

(provide 'calc-fin)

;;; calc-fin.el ends here
