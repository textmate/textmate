;;; calc-comb.el --- combinatoric functions for Calc

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

(defconst math-primes-table
  [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89
     97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181
     191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277
     281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383
     389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487
     491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601
     607 613 617 619 631 641 643 647 653 659 661 673 677 683 691 701 709
     719 727 733 739 743 751 757 761 769 773 787 797 809 811 821 823 827
     829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947
     953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033 1039 1049
     1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151
     1153 1163 1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237 1249
     1259 1277 1279 1283 1289 1291 1297 1301 1303 1307 1319 1321 1327 1361
     1367 1373 1381 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453 1459
     1471 1481 1483 1487 1489 1493 1499 1511 1523 1531 1543 1549 1553 1559
     1567 1571 1579 1583 1597 1601 1607 1609 1613 1619 1621 1627 1637 1657
     1663 1667 1669 1693 1697 1699 1709 1721 1723 1733 1741 1747 1753 1759
     1777 1783 1787 1789 1801 1811 1823 1831 1847 1861 1867 1871 1873 1877
     1879 1889 1901 1907 1913 1931 1933 1949 1951 1973 1979 1987 1993 1997
     1999 2003 2011 2017 2027 2029 2039 2053 2063 2069 2081 2083 2087 2089
     2099 2111 2113 2129 2131 2137 2141 2143 2153 2161 2179 2203 2207 2213
     2221 2237 2239 2243 2251 2267 2269 2273 2281 2287 2293 2297 2309 2311
     2333 2339 2341 2347 2351 2357 2371 2377 2381 2383 2389 2393 2399 2411
     2417 2423 2437 2441 2447 2459 2467 2473 2477 2503 2521 2531 2539 2543
     2549 2551 2557 2579 2591 2593 2609 2617 2621 2633 2647 2657 2659 2663
     2671 2677 2683 2687 2689 2693 2699 2707 2711 2713 2719 2729 2731 2741
     2749 2753 2767 2777 2789 2791 2797 2801 2803 2819 2833 2837 2843 2851
     2857 2861 2879 2887 2897 2903 2909 2917 2927 2939 2953 2957 2963 2969
     2971 2999 3001 3011 3019 3023 3037 3041 3049 3061 3067 3079 3083 3089
     3109 3119 3121 3137 3163 3167 3169 3181 3187 3191 3203 3209 3217 3221
     3229 3251 3253 3257 3259 3271 3299 3301 3307 3313 3319 3323 3329 3331
     3343 3347 3359 3361 3371 3373 3389 3391 3407 3413 3433 3449 3457 3461
     3463 3467 3469 3491 3499 3511 3517 3527 3529 3533 3539 3541 3547 3557
     3559 3571 3581 3583 3593 3607 3613 3617 3623 3631 3637 3643 3659 3671
     3673 3677 3691 3697 3701 3709 3719 3727 3733 3739 3761 3767 3769 3779
     3793 3797 3803 3821 3823 3833 3847 3851 3853 3863 3877 3881 3889 3907
     3911 3917 3919 3923 3929 3931 3943 3947 3967 3989 4001 4003 4007 4013
     4019 4021 4027 4049 4051 4057 4073 4079 4091 4093 4099 4111 4127 4129
     4133 4139 4153 4157 4159 4177 4201 4211 4217 4219 4229 4231 4241 4243
     4253 4259 4261 4271 4273 4283 4289 4297 4327 4337 4339 4349 4357 4363
     4373 4391 4397 4409 4421 4423 4441 4447 4451 4457 4463 4481 4483 4493
     4507 4513 4517 4519 4523 4547 4549 4561 4567 4583 4591 4597 4603 4621
     4637 4639 4643 4649 4651 4657 4663 4673 4679 4691 4703 4721 4723 4729
     4733 4751 4759 4783 4787 4789 4793 4799 4801 4813 4817 4831 4861 4871
     4877 4889 4903 4909 4919 4931 4933 4937 4943 4951 4957 4967 4969 4973
     4987 4993 4999 5003])

;; The variable math-prime-factors-finished is set by calcFunc-prfac to 
;; indicate whether factoring is complete, and used by calcFunc-factors,
;; calcFunc-totient and calcFunc-moebius.
(defvar math-prime-factors-finished)

;;; Combinatorics

(defun calc-gcd (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "gcd" 'calcFunc-gcd arg)))

(defun calc-lcm (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "lcm" 'calcFunc-lcm arg)))

(defun calc-extended-gcd ()
  (interactive)
  (calc-slow-wrapper
   (calc-enter-result 2 "egcd" (cons 'calcFunc-egcd (calc-top-list-n 2)))))

(defun calc-factorial (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "fact" 'calcFunc-fact arg)))

(defun calc-gamma (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "gmma" 'calcFunc-gamma arg)))

(defun calc-double-factorial (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "dfac" 'calcFunc-dfact arg)))

(defun calc-choose (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (calc-binary-op "perm" 'calcFunc-perm arg)
     (calc-binary-op "chos" 'calcFunc-choose arg))))

(defun calc-perm (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-choose arg))

(defvar calc-last-random-limit '(float 1 0))
(defun calc-random (n)
  (interactive "P")
  (calc-slow-wrapper
   (if n
       (calc-enter-result 0 "rand" (list 'calcFunc-random
					 (calc-get-random-limit
					  (prefix-numeric-value n))))
     (calc-enter-result 1 "rand" (list 'calcFunc-random
				       (calc-get-random-limit
					(calc-top-n 1)))))))

(defun calc-get-random-limit (val)
  (if (eq val 0)
      calc-last-random-limit
    (setq calc-last-random-limit val)))

(defun calc-rrandom ()
  (interactive)
  (calc-slow-wrapper
   (setq calc-last-random-limit '(float 1 0))
   (calc-enter-result 0 "rand" (list 'calcFunc-random '(float 1 0)))))

(defun calc-random-again (arg)
  (interactive "p")
  (calc-slow-wrapper
   (while (>= (setq arg (1- arg)) 0)
     (calc-enter-result 0 "rand" (list 'calcFunc-random
				       calc-last-random-limit)))))

(defun calc-shuffle (n)
  (interactive "P")
  (calc-slow-wrapper
   (if n
       (calc-enter-result 1 "shuf" (list 'calcFunc-shuffle
					 (prefix-numeric-value n)
					 (calc-get-random-limit
					  (calc-top-n 1))))
     (calc-enter-result 2 "shuf" (list 'calcFunc-shuffle
				       (calc-top-n 1)
				       (calc-get-random-limit
					(calc-top-n 2)))))))

(defun calc-report-prime-test (res)
  (cond ((eq (car res) t)
	 (calc-record-message "prim" "Prime (guaranteed)"))
	((eq (car res) nil)
	 (if (cdr res)
	     (if (eq (nth 1 res) 'unknown)
		 (calc-record-message
		  "prim" "Non-prime (factors unknown)")
	       (calc-record-message
		"prim" "Non-prime (%s is a factor)"
		(math-format-number (nth 1 res))))
	   (calc-record-message "prim" "Non-prime")))
	(t
	 (calc-record-message
	  "prim" "Probably prime (%d iters; %s%% chance of error)"
	  (nth 1 res)
	  (let ((calc-float-format '(fix 2)))
	    (math-format-number (nth 2 res)))))))

(defun calc-prime-test (iters)
  (interactive "p")
  (calc-slow-wrapper
   (let* ((n (calc-top-n 1))
	  (res (math-prime-test n iters)))
     (calc-report-prime-test res))))

(defvar calc-verbose-nextprime nil)

(defun calc-next-prime (iters)
  (interactive "p")
  (calc-slow-wrapper
   (let ((calc-verbose-nextprime t))
     (if (calc-is-inverse)
	 (calc-enter-result 1 "prvp" (list 'calcFunc-prevprime
					   (calc-top-n 1) (math-abs iters)))
       (calc-enter-result 1 "nxtp" (list 'calcFunc-nextprime
					 (calc-top-n 1) (math-abs iters)))))))

(defun calc-prev-prime (iters)
  (interactive "p")
  (calc-invert-func)
  (calc-next-prime iters))

(defun calc-prime-factors (iters)
  (interactive "p")
  (calc-slow-wrapper
   (let ((res (calcFunc-prfac (calc-top-n 1))))
     (if (not math-prime-factors-finished)
	 (calc-record-message "pfac" "Warning:  May not be fully factored"))
     (calc-enter-result 1 "pfac" res))))

(defun calc-totient (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "phi" 'calcFunc-totient arg)))

(defun calc-moebius (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "mu" 'calcFunc-moebius arg)))


(defun calcFunc-gcd (a b)
  (if (Math-messy-integerp a)
      (setq a (math-trunc a)))
  (if (Math-messy-integerp b)
      (setq b (math-trunc b)))
  (cond ((and (Math-integerp a) (Math-integerp b))
	 (math-gcd a b))
	((Math-looks-negp a)
	 (calcFunc-gcd (math-neg a) b))
	((Math-looks-negp b)
	 (calcFunc-gcd a (math-neg b)))
	((Math-zerop a) b)
	((Math-zerop b) a)
	((and (Math-ratp a)
	      (Math-ratp b))
	 (math-make-frac (math-gcd (if (eq (car-safe a) 'frac) (nth 1 a) a)
				   (if (eq (car-safe b) 'frac) (nth 1 b) b))
			 (calcFunc-lcm
			  (if (eq (car-safe a) 'frac) (nth 2 a) 1)
			  (if (eq (car-safe b) 'frac) (nth 2 b) 1))))
	((not (Math-integerp a))
	 (calc-record-why 'integerp a)
	 (list 'calcFunc-gcd a b))
	(t
	 (calc-record-why 'integerp b)
	 (list 'calcFunc-gcd a b))))

(defun calcFunc-lcm (a b)
  (let ((g (calcFunc-gcd a b)))
    (if (Math-numberp g)
	(math-div (math-mul a b) g)
      (list 'calcFunc-lcm a b))))

(defun calcFunc-egcd (a b)   ; Knuth section 4.5.2
  (cond
   ((not (Math-integerp a))
    (if (Math-messy-integerp a)
	(calcFunc-egcd (math-trunc a) b)
      (calc-record-why 'integerp a)
      (list 'calcFunc-egcd a b)))
   ((not (Math-integerp b))
    (if (Math-messy-integerp b)
	(calcFunc-egcd a (math-trunc b))
      (calc-record-why 'integerp b)
      (list 'calcFunc-egcd a b)))
   (t
    (let ((u1 1) (u2 0) (u3 a)
	  (v1 0) (v2 1) (v3 b)
	  t1 t2 q)
      (while (not (eq v3 0))
	(setq q (math-idivmod u3 v3)
	      t1 (math-sub u1 (math-mul v1 (car q)))
	      t2 (math-sub u2 (math-mul v2 (car q)))
	      u1 v1  u2 v2  u3 v3
	      v1 t1  v2 t2  v3 (cdr q)))
      (list 'vec u3 u1 u2)))))


;;; Factorial and related functions.

(defconst math-small-factorial-table
  (vector 1 1 2 6 24 120 720 5040 40320 362880 3628800 39916800
          (math-read-number-simple "479001600")
          (math-read-number-simple "6227020800")
          (math-read-number-simple "87178291200")
          (math-read-number-simple "1307674368000")
          (math-read-number-simple "20922789888000")
          (math-read-number-simple "355687428096000")
          (math-read-number-simple "6402373705728000")
          (math-read-number-simple "121645100408832000")
          (math-read-number-simple "2432902008176640000")))

(defun calcFunc-fact (n)   ; [I I] [F F] [Public]
  (let (temp)
    (cond ((Math-integer-negp n)
	   (if calc-infinite-mode
	       '(var uinf var-uinf)
	     (math-reject-arg n 'range)))
	  ((integerp n)
	   (if (<= n 20)
	       (aref math-small-factorial-table n)
	     (math-factorial-iter (1- n) 2 1)))
	  ((and (math-messy-integerp n)
		(Math-lessp n 100))
	   (math-inexact-result)
	   (setq temp (math-trunc n))
	   (if (>= temp 0)
	       (if (<= temp 20)
		   (math-float (calcFunc-fact temp))
		 (math-with-extra-prec 1
		   (math-factorial-iter (1- temp) 2 '(float 1 0))))
	     (math-reject-arg n 'range)))
	  ((math-numberp n)
	   (let* ((q (math-quarter-integer n))
		  (tn (and q (Math-lessp n 1000) (Math-lessp -1000 n)
			   (1+ (math-floor n)))))
	     (cond ((and tn (= q 2)
			 (or calc-symbolic-mode (< (math-abs tn) 20)))
		    (let ((q (if (< tn 0)
				 (math-div
				  (math-pow -2 (- tn))
				  (math-double-factorial-iter (* -2 tn) 3 1 2))
			       (math-div
				(math-double-factorial-iter (* 2 tn) 3 1 2)
				(math-pow 2 tn)))))
		      (math-mul q (if calc-symbolic-mode
				      (list 'calcFunc-sqrt '(var pi var-pi))
				    (math-sqrt-pi)))))
		   ((and tn (>= tn 0) (< tn 20)
			 (memq q '(1 3)))
		    (math-inexact-result)
		    (math-div
		     (math-mul (math-double-factorial-iter (* 4 tn) q 1 4)
			       (if (= q 1) (math-gamma-1q) (math-gamma-3q)))
		     (math-pow 4 tn)))
		   (t
		    (math-inexact-result)
		    (math-with-extra-prec 3
		      (math-gammap1-raw (math-float n)))))))
	  ((equal n '(var inf var-inf)) n)
	  (t (calc-record-why 'numberp n)
	     (list 'calcFunc-fact n)))))

(math-defcache math-gamma-1q nil
  (math-with-extra-prec 3
    (math-gammap1-raw '(float -75 -2))))

(math-defcache math-gamma-3q nil
  (math-with-extra-prec 3
    (math-gammap1-raw '(float -25 -2))))

(defun math-factorial-iter (count n f)
  (if (= (% n 5) 1)
      (math-working (format "factorial(%d)" (1- n)) f))
  (if (> count 0)
      (math-factorial-iter (1- count) (1+ n) (math-mul n f))
    f))

(defun calcFunc-dfact (n)   ; [I I] [F F] [Public]
  (cond ((Math-integer-negp n)
	 (if (math-oddp n)
	     (if (eq n -1)
		 1
	       (math-div (if (eq (math-mod n 4) 3) 1 -1)
			 (calcFunc-dfact (math-sub -2 n))))
	   (list 'calcFunc-dfact n)))
	((Math-zerop n) 1)
	((integerp n) (math-double-factorial-iter n (+ 2 (% n 2)) 1 2))
	((math-messy-integerp n)
	 (let ((temp (math-trunc n)))
	   (math-inexact-result)
	   (if (natnump temp)
	       (if (Math-lessp temp 200)
		   (math-with-extra-prec 1
		     (math-double-factorial-iter temp (+ 2 (% temp 2))
						 '(float 1 0) 2))
		 (let* ((half (math-div2 temp))
			(even (math-mul (math-pow 2 half)
					(calcFunc-fact (math-float half)))))
		   (if (math-evenp temp)
		       even
		     (math-div (calcFunc-fact n) even))))
	     (list 'calcFunc-dfact n))))
	((equal n '(var inf var-inf)) n)
	(t (calc-record-why 'natnump n)
	   (list 'calcFunc-dfact n))))

(defun math-double-factorial-iter (max n f step)
  (if (< (% n 12) step)
      (math-working (format "dfact(%d)" (- n step)) f))
  (if (<= n max)
      (math-double-factorial-iter max (+ n step) (math-mul n f) step)
    f))

(defun calcFunc-perm (n m)   ; [I I I] [F F F] [Public]
  (cond ((and (integerp n) (integerp m) (<= m n) (>= m 0))
	 (math-factorial-iter m (1+ (- n m)) 1))
	((or (not (math-num-integerp n))
	     (and (math-messy-integerp n) (Math-lessp 100 n))
	     (not (math-num-integerp m))
	     (and (math-messy-integerp m) (Math-lessp 100 m)))
	 (or (math-realp n) (equal n '(var inf var-inf))
	     (math-reject-arg n 'realp))
	 (or (math-realp m) (equal m '(var inf var-inf))
	     (math-reject-arg m 'realp))
	 (and (math-num-integerp n) (math-negp n) (math-reject-arg n 'range))
	 (and (math-num-integerp m) (math-negp m) (math-reject-arg m 'range))
	 (math-div (calcFunc-fact n) (calcFunc-fact (math-sub n m))))
	(t
	 (let ((tn (math-trunc n))
	       (tm (math-trunc m)))
	   (math-inexact-result)
	   (or (integerp tn) (math-reject-arg tn 'fixnump))
	   (or (integerp tm) (math-reject-arg tm 'fixnump))
	   (or (and (<= tm tn) (>= tm 0)) (math-reject-arg tm 'range))
	   (math-with-extra-prec 1
	     (math-factorial-iter tm (1+ (- tn tm)) '(float 1 0)))))))

(defun calcFunc-choose (n m)   ; [I I I] [F F F] [Public]
  (cond ((and (integerp n) (integerp m) (<= m n) (>= m 0))
	 (if (> m (/ n 2))
	     (math-choose-iter (- n m) n 1 1)
	   (math-choose-iter m n 1 1)))
	((not (math-realp n))
	 (math-reject-arg n 'realp))
	((not (math-realp m))
	 (math-reject-arg m 'realp))
	((not (math-num-integerp m))
	 (if (and (math-num-integerp n) (math-negp n))
	     (list 'calcFunc-choose n m)
	   (math-div (calcFunc-fact (math-float n))
		     (math-mul (calcFunc-fact m)
			       (calcFunc-fact (math-sub n m))))))
	((math-negp m) 0)
	((math-negp n)
	 (let ((val (calcFunc-choose (math-add (math-add n m) -1) m)))
	   (if (math-evenp (math-trunc m))
	       val
	     (math-neg val))))
	((and (math-num-integerp n)
	      (Math-lessp n m))
	 0)
	(t
	 (math-inexact-result)
	 (let ((tm (math-trunc m)))
	   (or (integerp tm) (math-reject-arg tm 'fixnump))
	   (if (> tm 100)
	       (math-div (calcFunc-fact (math-float n))
			 (math-mul (calcFunc-fact (math-float m))
				   (calcFunc-fact (math-float
						   (math-sub n m)))))
	     (math-with-extra-prec 1
	       (math-choose-float-iter tm n 1 1)))))))

(defun math-choose-iter (m n i c)
  (if (and (= (% i 5) 1) (> i 5))
      (math-working (format "choose(%d)" (1- i)) c))
  (if (<= i m)
      (math-choose-iter m (1- n) (1+ i)
			(math-quotient (math-mul c n) i))
    c))

(defun math-choose-float-iter (count n i c)
  (if (= (% i 5) 1)
      (math-working (format "choose(%d)" (1- i)) c))
  (if (> count 0)
      (math-choose-float-iter (1- count) (math-sub n 1) (1+ i)
			      (math-div (math-mul c n) i))
    c))


;;; Stirling numbers.

(defun calcFunc-stir1 (n m)
  (math-stirling-number n m 1))

(defun calcFunc-stir2 (n m)
  (math-stirling-number n m 0))

(defvar math-stirling-cache (vector [[1]] [[1]]))

;; The variable math-stirling-local-cache is local to
;; math-stirling-number, but is used by math-stirling-1
;; and math-stirling-2, which are called by math-stirling-number.
(defvar math-stirling-local-cache)

(defun math-stirling-number (n m k)
  (or (math-num-natnump n) (math-reject-arg n 'natnump))
  (or (math-num-natnump m) (math-reject-arg m 'natnump))
  (if (consp n) (setq n (math-trunc n)))
  (or (integerp n) (math-reject-arg n 'fixnump))
  (if (consp m) (setq m (math-trunc m)))
  (or (integerp m) (math-reject-arg m 'fixnump))
  (if (< n m)
      0
    (let ((math-stirling-local-cache (aref math-stirling-cache k)))
      (while (<= (length math-stirling-local-cache) n)
	(let ((i (1- (length math-stirling-local-cache)))
	      row)
	  (setq math-stirling-local-cache 
                (vconcat math-stirling-local-cache 
                         (make-vector (length math-stirling-local-cache) nil)))
	  (aset math-stirling-cache k math-stirling-local-cache)
	  (while (< (setq i (1+ i)) (length math-stirling-local-cache))
	    (aset math-stirling-local-cache i (setq row (make-vector (1+ i) nil)))
	    (aset row 0 0)
	    (aset row i 1))))
      (if (= k 1)
	  (math-stirling-1 n m)
	(math-stirling-2 n m)))))

(defun math-stirling-1 (n m)
  (or (aref (aref math-stirling-local-cache n) m)
      (aset (aref math-stirling-local-cache n) m
	    (math-add (math-stirling-1 (1- n) (1- m))
		      (math-mul (- 1 n) (math-stirling-1 (1- n) m))))))

(defun math-stirling-2 (n m)
  (or (aref (aref math-stirling-local-cache n) m)
      (aset (aref math-stirling-local-cache n) m
	    (math-add (math-stirling-2 (1- n) (1- m))
		      (math-mul m (math-stirling-2 (1- n) m))))))

(defvar math-random-table nil)
(defvar math-last-RandSeed nil)
(defvar math-random-ptr1 nil)
(defvar math-random-ptr2 nil)
(defvar math-random-shift nil)

;;; Produce a random 10-bit integer, with (random) if no seed provided,
;;; or else with Numerical Recipes algorithm ran3 / Knuth 3.2.2-A.

(defvar var-RandSeed)
(defvar math-random-cache nil)
(defvar math-gaussian-cache nil)

(defun math-init-random-base ()
  (if (and (boundp 'var-RandSeed) var-RandSeed)
      (if (eq (car-safe var-RandSeed) 'vec)
	  nil
	(if (Math-integerp var-RandSeed)
	    (let* ((seed (math-sub 161803 var-RandSeed))
		   (mj (1+ (math-mod seed 1000000)))
		   (mk (1+ (math-mod (math-quotient seed 1000000)
                                     1000000)))
		   (i 0))
	      (setq math-random-table (cons 'vec (make-list 55 mj)))
	      (while (<= (setq i (1+ i)) 54)
		(let* ((ii (% (* i 21) 55))
		       (p (nthcdr ii math-random-table)))
		  (setcar p mk)
		  (setq mk (- mj mk)
			mj (car p)))))
	  (math-reject-arg var-RandSeed "*RandSeed must be an integer"))
	(setq var-RandSeed (list 'vec var-RandSeed)
	      math-random-ptr1 math-random-table
	      math-random-cache nil
	      math-random-ptr2 (nthcdr 31 math-random-table))
	(let ((i 200))
	  (while (> (setq i (1- i)) 0)
	    (math-random-base))))
    (random t)
    (setq var-RandSeed nil
	  math-random-cache nil
	  math-random-shift -4)  ; assume RAND_MAX >= 16383
    ;; This exercises the random number generator and also helps
    ;; deduce a better value for RAND_MAX.
    (let ((i 0))
      (while (< (setq i (1+ i)) 30)
        (if (> (lsh (math-abs (random)) math-random-shift) 4095)
            (setq math-random-shift (1- math-random-shift))))))
  (setq math-last-RandSeed var-RandSeed
	math-gaussian-cache nil))

(defun math-random-base ()
  (if var-RandSeed
      (progn
	(setq math-random-ptr1 (or (cdr math-random-ptr1)
				   (cdr math-random-table))
	      math-random-ptr2 (or (cdr math-random-ptr2)
				   (cdr math-random-table)))
	(logand (lsh (setcar math-random-ptr1
			     (logand (- (car math-random-ptr1)
					(car math-random-ptr2)) 524287))
		     -6) 1023))
    (logand (lsh (random) math-random-shift) 1023)))


;;; Produce a random digit in the range 0..999.
;;; Avoid various pitfalls that may lurk in the built-in (random) function!
;;; Shuffling algorithm from Numerical Recipes, section 7.1.
(defvar math-random-last)
(defun math-random-three-digit-number ()
  "Return a random three digit number."
  (let (i)
    (or (and (boundp 'var-RandSeed) (eq var-RandSeed math-last-RandSeed))
	(math-init-random-base))
    (or math-random-cache
	(progn
	  (setq math-random-last (math-random-base)
		math-random-cache (make-vector 13 nil)
		i -1)
	  (while (< (setq i (1+ i)) 13)
	    (aset math-random-cache i (math-random-base)))))
    (while (progn
	     (setq i (/ math-random-last 79)   ; 0 <= i < 13
		   math-random-last (aref math-random-cache i))
	     (aset math-random-cache i (math-random-base))
	     (>= math-random-last 1000)))
    math-random-last))

;;; Produce an N-digit random integer.
(defun math-random-digits (n)
  "Produce a random N digit integer."
  (let* ((slop (% (- 3 (% n 3)) 3))
         (i (/ (+ n slop) 3))
         (rnum 0))
    (while (> i 0)
      (setq rnum 
            (math-add
             (math-random-three-digit-number)
             (math-mul rnum 1000)))
      (setq i (1- i)))
    (math-normalize (math-scale-right rnum slop))))

;;; Produce a uniformly-distributed random float 0 <= N < 1.
(defun math-random-float ()
  (math-make-float (math-random-digits calc-internal-prec)
		   (- calc-internal-prec)))

;;; Produce a Gaussian-distributed random float with mean=0, sigma=1.
(defun math-gaussian-float ()
  (math-with-extra-prec 2
    (if (and math-gaussian-cache
	     (= (car math-gaussian-cache) calc-internal-prec))
	(prog1
	    (cdr math-gaussian-cache)
	  (setq math-gaussian-cache nil))
      (let* ((v1 (math-add (math-mul (math-random-float) 2) -1))
	     (v2 (math-add (math-mul (math-random-float) 2) -1))
	     (r (math-add (math-sqr v1) (math-sqr v2))))
	(while (or (not (Math-lessp r 1)) (math-zerop r))
	  (setq v1 (math-add (math-mul (math-random-float) 2) -1)
		v2 (math-add (math-mul (math-random-float) 2) -1)
		r (math-add (math-sqr v1) (math-sqr v2))))
	(let ((fac (math-sqrt (math-mul (math-div (calcFunc-ln r) r) -2))))
	  (setq math-gaussian-cache (cons calc-internal-prec
					  (math-mul v1 fac)))
	  (math-mul v2 fac))))))

;;; Produce a random integer or real 0 <= N < MAX.
(defun calcFunc-random (max)
  (cond ((Math-zerop max)
	 (math-gaussian-float))
	((Math-integerp max)
	 (let* ((digs (math-numdigs max))
		(r (math-random-digits (+ digs 3))))
	   (math-mod r max)))
	((Math-realp max)
	 (math-mul (math-random-float) max))
	((and (eq (car max) 'intv) (math-constp max)
	      (Math-lessp (nth 2 max) (nth 3 max)))
	 (if (math-floatp max)
	     (let ((val (math-add (math-mul (math-random-float)
					    (math-sub (nth 3 max) (nth 2 max)))
				  (nth 2 max))))
	       (if (or (and (memq (nth 1 max) '(0 1))      ; almost not worth
			    (Math-equal val (nth 2 max)))  ;   checking!
		       (and (memq (nth 1 max) '(0 2))
			    (Math-equal val (nth 3 max))))
		   (calcFunc-random max)
		 val))
	   (let ((lo (if (memq (nth 1 max) '(0 1))
			 (math-add (nth 2 max) 1) (nth 2 max)))
		 (hi (if (memq (nth 1 max) '(1 3))
			 (math-add (nth 3 max) 1) (nth 3 max))))
	     (if (Math-lessp lo hi)
		 (math-add (calcFunc-random (math-sub hi lo)) lo)
	       (math-reject-arg max "*Empty interval")))))
	((eq (car max) 'vec)
	 (if (cdr max)
	     (nth (1+ (calcFunc-random (1- (length max)))) max)
	   (math-reject-arg max "*Empty list")))
	((and (eq (car max) 'sdev) (math-constp max) (Math-realp (nth 1 max)))
	 (math-add (math-mul (math-gaussian-float) (nth 2 max)) (nth 1 max)))
	(t (math-reject-arg max 'realp))))

;;; Choose N objects at random from the set MAX without duplicates.
(defun calcFunc-shuffle (n &optional max)
  (or max (setq max n n -1))
  (or (and (Math-num-integerp n)
	   (or (natnump (setq n (math-trunc n))) (eq n -1)))
      (math-reject-arg n 'integerp))
  (cond ((or (math-zerop max)
	     (math-floatp max)
	     (eq (car-safe max) 'sdev))
	 (if (< n 0)
	     (math-reject-arg n 'natnump)
	   (math-simple-shuffle n max)))
	((and (<= n 1) (>= n 0))
	 (math-simple-shuffle n max))
	((and (eq (car-safe max) 'intv) (math-constp max))
	 (let ((num (math-add (math-sub (nth 3 max) (nth 2 max))
			      (cdr (assq (nth 1 max)
					 '((0 . -1) (1 . 0)
					   (2 . 0) (3 . 1))))))
	       (min (math-add (nth 2 max) (if (memq (nth 1 max) '(0 1))
					      1 0))))
	   (if (< n 0) (setq n num))
	   (or (math-posp num) (math-reject-arg max 'range))
	   (and (Math-lessp num n) (math-reject-arg n 'range))
	   (if (Math-lessp n (math-quotient num 3))
	       (math-simple-shuffle n max)
	     (if (> (* n 4) (* num 3))
		 (math-add (math-sub min 1)
			   (math-shuffle-list n num (calcFunc-index num)))
	       (let ((tot 0)
		     (m 0)
		     (vec nil))
		 (while (< m n)
		   (if (< (calcFunc-random (- num tot)) (- n m))
		       (setq vec (cons (math-add min tot) vec)
			     m (1+ m)))
		   (setq tot (1+ tot)))
		 (math-shuffle-list n n (cons 'vec vec)))))))
	((eq (car-safe max) 'vec)
	 (let ((size (1- (length max))))
	   (if (< n 0) (setq n size))
	   (if (and (> n (/ size 2)) (<= n size))
	       (math-shuffle-list n size (copy-sequence max))
	     (let* ((vals (calcFunc-shuffle
			   n (list 'intv 3 1 (1- (length max)))))
		    (p vals))
	       (while (setq p (cdr p))
		 (setcar p (nth (car p) max)))
	       vals))))
	((math-integerp max)
	 (if (math-posp max)
	     (calcFunc-shuffle n (list 'intv 2 0 max))
	   (calcFunc-shuffle n (list 'intv 1 max 0))))
	(t (math-reject-arg max 'realp))))

(defun math-simple-shuffle (n max)
  (let ((vec nil)
	val)
    (while (>= (setq n (1- n)) 0)
      (while (math-member (setq val (calcFunc-random max)) vec))
      (setq vec (cons val vec)))
    (cons 'vec vec)))

(defun math-shuffle-list (n size vec)
  (let ((j size)
	k temp
	(p vec))
    (while (cdr (setq p (cdr p)))
      (setq k (calcFunc-random j)
	    j (1- j)
	    temp (nth k p))
      (setcar (nthcdr k p) (car p))
      (setcar p temp))
    (cons 'vec (nthcdr (- size n -1) vec))))

(defun math-member (x list)
  (while (and list (not (equal x (car list))))
    (setq list (cdr list)))
  list)


;;; Check if the integer N is prime.  [X I]
;;; Return (nil) if non-prime,
;;;        (nil N) if non-prime with known factor N,
;;;        (nil unknown) if non-prime with no known factors,
;;;        (t) if prime,
;;;        (maybe N P) if probably prime (after N iters with probability P%)
(defvar math-prime-test-cache '(-1))

(defvar math-prime-test-cache-k)
(defvar math-prime-test-cache-q)
(defvar math-prime-test-cache-nm1)

(defun math-prime-test (n iters)
  (if (and (Math-vectorp n) (cdr n))
      (setq n (nth (1- (length n)) n)))
  (if (Math-messy-integerp n)
      (setq n (math-trunc n)))
  (let ((res))
    (while (> iters 0)
      (setq res
	    (cond ((and (integerp n) (<= n 5003))
		   (list (= (math-next-small-prime n) n)))
		  ((not (Math-integerp n))
		   (error "Argument must be an integer"))
		  ((Math-integer-negp n)
		   '(nil))
		  ((Math-natnum-lessp n 8000000)
		   (setq n (math-fixnum n))
		   (let ((i -1) v)
		     (while (and (> (% n (setq v (aref math-primes-table
						       (setq i (1+ i)))))
				    0)
				 (< (* v v) n)))
		     (if (= (% n v) 0)
			 (list nil v)
		       '(t))))
		  ((not (equal n (car math-prime-test-cache)))
		   (cond ((= (% (nth 1 n) 2) 0) '(nil 2))
			 ((= (% (nth 1 n) 5) 0) '(nil 5))
			 (t (let ((q n) (sum 0))
                              (while (not (eq q 0))
                                (setq sum (%
                                           (+
                                            sum
                                            (calcFunc-mod 
                                             q 1000000))
                                           111111))
                                (setq q 
                                      (math-quotient 
                                       q 1000000)))
			      (cond ((= (% sum 3) 0) '(nil 3))
				    ((= (% sum 7) 0) '(nil 7))
				    ((= (% sum 11) 0) '(nil 11))
				    ((= (% sum 13) 0) '(nil 13))
				    ((= (% sum 37) 0) '(nil 37))
				    (t
				     (setq math-prime-test-cache-k 1
					   math-prime-test-cache-q
					   (math-div2 n)
					   math-prime-test-cache-nm1
					   (math-add n -1))
				     (while (math-evenp
					     math-prime-test-cache-q)
				       (setq math-prime-test-cache-k
					     (1+ math-prime-test-cache-k)
					     math-prime-test-cache-q
					     (math-div2
					      math-prime-test-cache-q)))
				     (setq iters (1+ iters))
				     (list 'maybe
					   0
					   (math-sub
					    100
					    (math-div
					     '(float 232 0)
					     (math-numdigs n))))))))))
		  ((not (eq (car (nth 1 math-prime-test-cache)) 'maybe))
		   (nth 1 math-prime-test-cache))
		  (t   ; Fermat step
		   (let* ((x (math-add (calcFunc-random (math-add n -2)) 2))
			  (y (math-pow-mod x math-prime-test-cache-q n))
			  (j 0))
		     (while (and (not (eq y 1))
				 (not (equal y math-prime-test-cache-nm1))
				 (< (setq j (1+ j)) math-prime-test-cache-k))
		       (setq y (math-mod (math-mul y y) n)))
		     (if (or (equal y math-prime-test-cache-nm1)
			     (and (eq y 1) (eq j 0)))
			 (list 'maybe
			       (1+ (nth 1 (nth 1 math-prime-test-cache)))
			       (math-mul (nth 2 (nth 1 math-prime-test-cache))
					 '(float 25 -2)))
		       '(nil unknown))))))
      (setq math-prime-test-cache (list n res)
	    iters (if (eq (car res) 'maybe)
		      (1- iters)
		    0)))
    res))

(defun calcFunc-prime (n &optional iters)
  (or (math-num-integerp n) (math-reject-arg n 'integerp))
  (or (not iters) (math-num-integerp iters) (math-reject-arg iters 'integerp))
  (if (car (math-prime-test (math-trunc n) (math-trunc (or iters 1))))
      1
    0))

;;; Theory: summing base-10^6 digits modulo 111111 is "casting out 999999s".
;;; Initial probability that N is prime is 1/ln(N) = log10(e)/log10(N).
;;; After culling [2,3,5,7,11,13,37], probability of primality is 5.36 x more.
;;; Initial reported probability of non-primality is thus 100% - this.
;;; Each Fermat step multiplies this probability by 25%.
;;; The Fermat step is algorithm P from Knuth section 4.5.4.


(defun calcFunc-prfac (n)
  (setq math-prime-factors-finished t)
  (if (Math-messy-integerp n)
      (setq n (math-trunc n)))
  (if (Math-natnump n)
      (if (Math-natnum-lessp 2 n)
	  (let (factors res p (i 0))
	    (while (and (not (eq n 1))
			(< i (length math-primes-table)))
	      (setq p (aref math-primes-table i))
	      (while (eq (cdr (setq res (cond ((eq n p) (cons 1 0))
					      ((eq n 1) (cons 0 1))
					      ((consp n) (math-idivmod n p))
					      (t (cons (/ n p) (% n p))))))
			 0)
		(math-working "factor" p)
		(setq factors (nconc factors (list p))
		      n (car res)))
	      (or (eq n 1)
		  (Math-natnum-lessp p (car res))
		  (setq factors (nconc factors (list n))
			n 1))
	      (setq i (1+ i)))
	    (or (setq math-prime-factors-finished (eq n 1))
		(setq factors (nconc factors (list n))))
	    (cons 'vec factors))
	(list 'vec n))
    (if (Math-integerp n)
	(if (eq n -1)
	    (list 'vec n)
	  (cons 'vec (cons -1 (cdr (calcFunc-prfac (math-neg n))))))
      (calc-record-why 'integerp n)
      (list 'calcFunc-prfac n))))

(defun calcFunc-totient (n)
  (if (Math-messy-integerp n)
      (setq n (math-trunc n)))
  (if (Math-natnump n)
      (if (Math-natnum-lessp n 2)
	  (if (Math-negp n)
	      (calcFunc-totient (math-abs n))
	    n)
	(let ((factors (cdr (calcFunc-prfac n)))
	      p)
	  (if math-prime-factors-finished
	      (progn
		(while factors
		  (setq p (car factors)
			n (math-mul (math-div n p) (math-add p -1)))
		  (while (equal p (car factors))
		    (setq factors (cdr factors))))
		n)
	    (calc-record-why "*Number too big to factor" n)
	    (list 'calcFunc-totient n))))
    (calc-record-why 'natnump n)
    (list 'calcFunc-totient n)))

(defun calcFunc-moebius (n)
  (if (Math-messy-integerp n)
      (setq n (math-trunc n)))
  (if (and (Math-natnump n) (not (eq n 0)))
      (if (Math-natnum-lessp n 2)
	  (if (Math-negp n)
	      (calcFunc-moebius (math-abs n))
	    1)
	(let ((factors (cdr (calcFunc-prfac n)))
	      (mu 1))
	  (if math-prime-factors-finished
	      (progn
		(while factors
		  (setq mu (if (equal (car factors) (nth 1 factors))
			       0 (math-neg mu))
			factors (cdr factors)))
		mu)
	    (calc-record-why "Number too big to factor" n)
	    (list 'calcFunc-moebius n))))
    (calc-record-why 'posintp n)
    (list 'calcFunc-moebius n)))


(defun calcFunc-nextprime (n &optional iters)
  (if (Math-integerp n)
      (if (Math-integer-negp n)
	  2
	(if (and (integerp n) (< n 5003))
	    (math-next-small-prime (1+ n))
	  (if (math-evenp n)
	      (setq n (math-add n -1)))
	  (let (res)
	    (while (not (car (setq res (math-prime-test
					(setq n (math-add n 2))
					(or iters 1))))))
	    (if (and calc-verbose-nextprime
		     (eq (car res) 'maybe))
		(calc-report-prime-test res)))
	  n))
    (if (Math-realp n)
	(calcFunc-nextprime (math-trunc n) iters)
      (math-reject-arg n 'integerp))))

(defun calcFunc-prevprime (n &optional iters)
  (if (Math-integerp n)
      (if (Math-lessp n 4)
	  2
	(if (math-evenp n)
	    (setq n (math-add n 1)))
	(let (res)
	  (while (not (car (setq res (math-prime-test
				      (setq n (math-add n -2))
				      (or iters 1))))))
	  (if (and calc-verbose-nextprime
		   (eq (car res) 'maybe))
	      (calc-report-prime-test res)))
	n)
    (if (Math-realp n)
	(calcFunc-prevprime (math-ceiling n) iters)
      (math-reject-arg n 'integerp))))

(defun math-next-small-prime (n)
  (if (and (integerp n) (> n 2))
      (let ((lo -1)
	    (hi (length math-primes-table))
	    mid)
	(while (> (- hi lo) 1)
	  (if (> n (aref math-primes-table
			 (setq mid (ash (+ lo hi) -1))))
	      (setq lo mid)
	    (setq hi mid)))
	(aref math-primes-table hi))
    2))

(provide 'calc-comb)

;;; calc-comb.el ends here
