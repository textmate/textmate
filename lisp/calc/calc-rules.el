;;; calc-rules.el --- rules for simplifying algebraic expressions in Calc

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

(defun calc-compile-rule-set (name rules)
  (prog2
   (message "Preparing rule set %s..." name)
   (math-read-plain-expr rules t)
   (message "Preparing rule set %s...done" name)))

(defun calc-CommuteRules ()
  "CommuteRules"
  (calc-compile-rule-set
   "CommuteRules" "[
iterations(1),
select(plain(a + b))		:=  select(plain(b + a)),
select(plain(a - b))		:=  select(plain((-b) + a)),
select(plain((1/a) * b))	:=  select(b / a),
select(plain(a * b))		:=  select(b * a),
select((1/a) / b)		:=  select((1/b) / a),
select(a / b)			:=  select((1/b) * a),
select((a^b) ^ c)		:=  select((a^c) ^ b),
select(log(a, b))		:=  select(1 / log(b, a)),
select(plain(a && b))		:=  select(b && a),
select(plain(a || b))		:=  select(b || a),
select(plain(a = b))		:=  select(b = a),
select(plain(a != b))		:=  select(b != a),
select(a < b)			:=  select(b > a),
select(a > b)			:=  select(b < a),
select(a <= b)			:=  select(b >= a),
select(a >= b)			:=  select(b <= a) ]"))

(defun calc-JumpRules ()
  "JumpRules"
  (calc-compile-rule-set
   "JumpRules" "[
iterations(1),
plain(select(x) = y)		:=  0 = select(-x) + y,
plain(a + select(x) = y)	:=  a = select(-x) + y,
plain(a - select(x) = y)	:=  a = select(x) + y,
plain(select(x) + a = y)	:=  a = select(-x) + y,
plain(a * select(x) = y)	:=  a = y / select(x),
plain(a / select(x) = y)	:=  a = select(x) * y,
plain(select(x) / a = y)	:=  1/a = y / select(x),
plain(a ^ select(2) = y)	:=  a = select(sqrt(y)),
plain(a ^ select(x) = y)	:=  a = y ^ select(1/x),
plain(select(x) ^ a = y)	:=  a = log(y, select(x)),
plain(log(a, select(x)) = y)	:=  a = select(x) ^ y,
plain(log(select(x), a) = y)	:=  a = select(x) ^ (1/y),
plain(y = select(x))		:=  y - select(x) = 0,
plain(y = a + select(x))	:=  y - select(x) = a,
plain(y = a - select(x))	:=  y + select(x) = a,
plain(y = select(x) + a)	:=  y - select(x) = a,
plain(y = a * select(x))	:=  y / select(x) = a,
plain(y = a / select(x))	:=  y * select(x) = a,
plain(y = select(x) / a)	:=  y / select(x) = 1/a,
plain(y = a ^ select(2))	:=  select(sqrt(y)) = a,
plain(y = a ^ select(x))	:=  y ^ select(1/x) = a,
plain(y = select(x) ^ a)	:=  log(y, select(x)) = a,
plain(y = log(a, select(x)))	:=  select(x) ^ y = a,
plain(y = log(select(x), a))	:=  select(x) ^ (1/y) = a ]"))

(defun calc-DistribRules ()
  "DistribRules"
  (calc-compile-rule-set
   "DistribRules" "[
iterations(1),
x * select(a + b)		:=  x*select(a) + x*b,
x * select(sum(a,b,c,d))	:=  sum(x*select(a),b,c,d),
x / select(a + b)		:=  1 / (select(a)/x + b/x),
select(a + b) / x		:=  select(a)/x + b/x,
sum(select(a),b,c,d) / x	:=  sum(select(a)/x,b,c,d),
x ^ select(a + b)		:=  x^select(a) * x^b,
x ^ select(sum(a,b,c,d))	:=  prod(x^select(a),b,c,d),
x ^ select(a * b)		:=  (x^a)^select(b),
x ^ select(a / b)		:=  (x^a)^select(1/b),
select(a + b) ^ n		:=  select(x)
				    :: integer(n) :: n >= 2
				    :: let(x, expandpow(a+b,n))
				    :: quote(matches(x,y+z)),
select(a + b) ^ x		:=  a*select(a+b)^(x-1) + b*select(a+b)^(x-1),
select(a * b) ^ x		:=  a^x * select(b)^x,
select(prod(a,b,c,d)) ^ x	:=  prod(select(a)^x,b,c,d),
select(a / b) ^ x		:=  select(a)^x / b^x,
select(- a) ^ x			:=  (-1)^x * select(a)^x,
plain(-select(a + b))		:=  select(-a) - b,
plain(-select(sum(a,b,c,d)))    :=  sum(select(-a),b,c,d),
plain(-select(a * b))	        :=  select(-a) * b,
plain(-select(a / b))	        :=  select(-a) / b,
sqrt(select(a * b))		:=  sqrt(select(a)) * sqrt(b),
sqrt(select(prod(a,b,c,d)))	:=  prod(sqrt(select(a)),b,c,d),
sqrt(select(a / b))		:=  sqrt(select(a)) / sqrt(b),
sqrt(select(- a))		:=  sqrt(-1) sqrt(select(a)),
exp(select(a + b))		:=  exp(select(a)) / exp(-b) :: negative(b),
exp(select(a + b))		:=  exp(select(a)) * exp(b),
exp(select(sum(a,b,c,d)))	:=  prod(exp(select(a)),b,c,d),
exp(select(a * b))		:=  exp(select(a)) ^ b :: constant(b),
exp(select(a * b))		:=  exp(select(a)) ^ b,
exp(select(a / b))		:=  exp(select(a)) ^ (1/b),
ln(select(a * b))		:=  ln(select(a)) + ln(b),
ln(select(prod(a,b,c,d)))	:=  sum(ln(select(a)),b,c,d),
ln(select(a / b))		:=  ln(select(a)) - ln(b),
ln(select(a ^ b))		:=  ln(select(a)) * b,
log10(select(a * b))		:=  log10(select(a)) + log10(b),
log10(select(prod(a,b,c,d)))	:=  sum(log10(select(a)),b,c,d),
log10(select(a / b))		:=  log10(select(a)) - log10(b),
log10(select(a ^ b))		:=  log10(select(a)) * b,
log(select(a * b), x)		:=  log(select(a), x) + log(b,x),
log(select(prod(a,b,c,d)),x)	:=  sum(log(select(a),x),b,c,d),
log(select(a / b), x)		:=  log(select(a), x) - log(b,x),
log(select(a ^ b), x)		:=  log(select(a), x) * b,
log(a, select(b))		:=  ln(a) / select(ln(b)),
sin(select(a + b))		:=  sin(select(a)) cos(b) + cos(a) sin(b),
sin(select(2 a))		:=  2 sin(select(a)) cos(a),
sin(select(n a))		:=  2sin((n-1) select(a)) cos(a) - sin((n-2) a)
				    :: integer(n) :: n > 2,
cos(select(a + b))		:=  cos(select(a)) cos(b) - sin(a) sin(b),
cos(select(2 a))		:=  2 cos(select(a))^2 - 1,
cos(select(n a))		:=  2cos((n-1) select(a)) cos(a) - cos((n-2) a)
				    :: integer(n) :: n > 2,
tan(select(a + b))		:=  (tan(select(a)) + tan(b)) /
				    (1 - tan(a) tan(b)),
tan(select(2 a))		:=  2 tan(select(a)) / (1 - tan(a)^2),
tan(select(n a))		:=  (tan((n-1) select(a)) + tan(a)) /
				    (1 - tan((n-1) a) tan(a))
				    :: integer(n) :: n > 2,
cot(select(a + b))		:=  (cot(select(a)) cot(b) - 1) /
				    (cot(a) + cot(b)),
sinh(select(a + b))		:=  sinh(select(a)) cosh(b) + cosh(a) sinh(b),
cosh(select(a + b))		:=  cosh(select(a)) cosh(b) + sinh(a) sinh(b),
tanh(select(a + b))		:=  (tanh(select(a)) + tanh(b)) /
				    (1 + tanh(a) tanh(b)),
coth(select(a + b))		:=  (coth(select(a)) coth(b) + 1) /
				    (coth(a) + coth(b)),
x && select(a || b)		:=  (x && select(a)) || (x && b),
select(a || b) && x		:=  (select(a) && x) || (b && x),
! select(a && b)		:=  (!a) || (!b),
! select(a || b)		:=  (!a) && (!b) ]"))

(defun calc-MergeRules ()
  "MergeRules"
  (calc-compile-rule-set
   "MergeRules" "[
iterations(1),
 (x*opt(a)) + select(x*b)	:=  x * (a + select(b)),
 (x*opt(a)) - select(x*b)	:=  x * (a - select(b)),
sum(select(x)*a,b,c,d)		:=  x * sum(select(a),b,c,d),
 (a/x) + select(b/x)		:=  (a + select(b)) / x,
 (a/x) - select(b/x)		:=  (a - select(b)) / x,
sum(a/select(x),b,c,d)		:=  sum(select(a),b,c,d) / x,
 (a/opt(b)) + select(c/d)	:=  ((select(a)*d) + (b*c)) / (b*d),
 (a/opt(b)) - select(c/d)	:=  ((select(a)*d) - (b*c)) / (b*d),
 (x^opt(a)) * select(x^b)	:=  x ^ (a + select(b)),
 (x^opt(a)) / select(x^b)	:=  x ^ (a - select(b)),
select(x^a) / (x^opt(b))	:=  x ^ (select(a) - b),
prod(select(x)^a,b,c,d)		:=  x ^ sum(select(a),b,c,d),
select(x^a) / (x^opt(b))	:=  x ^ (select(a) - b),
 (a^x) * select(b^x)		:=  select((a * b) ^x),
 (a^x) / select(b^x)		:=  select((b / b) ^ x),
select(a^x) / (b^x)		:=  select((a / b) ^ x),
prod(a^select(x),b,c,d)		:=  select(prod(a,b,c,d) ^ x),
 (a^x) * select(b^y)		:=  select((a * b^(y-x)) ^x),
 (a^x) / select(b^y)		:=  select((b / b^(y-x)) ^ x),
select(a^x) / (b^y)		:=  select((a / b^(y-x)) ^ x),
select(x^a) ^ b			:=  x ^ select(a * b),
 (x^a) ^ select(b)		:=  x ^ select(a * b),
select(sqrt(a)) ^ b		:=  select(a ^ (b / 2)),
sqrt(a) ^ select(b)		:=  select(a ^ (b / 2)),
sqrt(select(a) ^ b)		:=  select(a ^ (b / 2)),
sqrt(a ^ select(b))		:=  select(a ^ (b / 2)),
sqrt(a) * select(sqrt(b))	:=  select(sqrt(a * b)),
sqrt(a) / select(sqrt(b))	:=  select(sqrt(a / b)),
select(sqrt(a)) / sqrt(b)	:=  select(sqrt(a / b)),
prod(select(sqrt(a)),b,c,d)	:=  select(sqrt(prod(a,b,c,d))),
exp(a) * select(exp(b))		:=  select(exp(a + b)),
exp(a) / select(exp(b))		:=  select(exp(a - b)),
select(exp(a)) / exp(b)		:=  select(exp(a - b)),
prod(select(exp(a)),b,c,d)	:=  select(exp(sum(a,b,c,d))),
select(exp(a)) ^ b		:=  select(exp(a * b)),
exp(a) ^ select(b)		:=  select(exp(a * b)),
ln(a) + select(ln(b))		:=  select(ln(a * b)),
ln(a) - select(ln(b))		:=  select(ln(a / b)),
select(ln(a)) - ln(b)		:=  select(ln(a / b)),
sum(select(ln(a)),b,c,d)	:=  select(ln(prod(a,b,c,d))),
b * select(ln(a))		:=  select(ln(a ^ b)),
select(b) * ln(a)		:=  select(ln(a ^ b)),
select(ln(a)) / ln(b)		:=  select(log(a, b)),
ln(a) / select(ln(b))		:=  select(log(a, b)),
select(ln(a)) / b		:=  select(ln(a ^ (1/b))),
ln(a) / select(b)		:=  select(ln(a ^ (1/b))),
log10(a) + select(log10(b))	:=  select(log10(a * b)),
log10(a) - select(log10(b))	:=  select(log10(a / b)),
select(log10(a)) - log10(b)	:=  select(log10(a / b)),
sum(select(log10(a)),b,c,d)	:=  select(log10(prod(a,b,c,d))),
b * select(log10(a))		:=  select(log10(a ^ b)),
select(b) * log10(a)		:=  select(log10(a ^ b)),
select(log10(a)) / log10(b)	:=  select(log(a, b)),
log10(a) / select(log10(b))	:=  select(log(a, b)),
select(log10(a)) / b		:=  select(log10(a ^ (1/b))),
log10(a) / select(b)		:=  select(log10(a ^ (1/b))),
log(a,x) + select(log(b,x))	:=  select(log(a * b,x)),
log(a,x) - select(log(b,x))	:=  select(log(a / b,x)),
select(log(a,x)) - log(b,x)	:=  select(log(a / b,x)),
sum(select(log(a,x)),b,c,d)	:=  select(log(prod(a,b,c,d),x)),
b * select(log(a,x))		:=  select(log(a ^ b,x)),
select(b) * log(a,x)		:=  select(log(a ^ b,x)),
select(log(a,x)) / log(b,x)	:=  select(log(a, b)),
log(a,x) / select(log(b,x))	:=  select(log(a, b)),
select(log(a,x)) / b		:=  select(log(a ^ (1/b),x)),
log(a,x) / select(b)		:=  select(log(a ^ (1/b),x)),
select(x && a) || (x && opt(b)) :=  x && (select(a) || b) ]"))

(defun calc-NegateRules ()
  "NegateRules"
  (calc-compile-rule-set
   "NegateRules" "[
iterations(1),
a + select(x)			:=  a - select(-x),
a - select(x)			:=  a + select(-x),
sum(select(x),b,c,d)		:=  -sum(select(-x),b,c,d),
a * select(x)			:=  -a * select(-x),
a / select(x)			:=  -a / select(-x),
select(x) / a			:=  -select(-x) / a,
prod(select(x),b,c,d)		:=  (-1)^(d-c+1) * prod(select(-x),b,c,d),
select(x) ^ n			:=  select(-x) ^ a :: integer(n) :: n%2 = 0,
select(x) ^ n			:=  -(select(-x) ^ a) :: integer(n) :: n%2 = 1,
select(x) ^ a			:=  (-select(-x)) ^ a,
a ^ select(x)			:=  (1 / a)^select(-x),
abs(select(x))			:=  abs(select(-x)),
i sqrt(select(x))		:=  -sqrt(select(-x)),
sqrt(select(x))			:=  i sqrt(select(-x)),
re(select(x))			:=  -re(select(-x)),
im(select(x))			:=  -im(select(-x)),
conj(select(x))			:=  -conj(select(-x)),
trunc(select(x))		:=  -trunc(select(-x)),
round(select(x))		:=  -round(select(-x)),
floor(select(x))		:=  -ceil(select(-x)),
ceil(select(x))			:=  -floor(select(-x)),
ftrunc(select(x))		:=  -ftrunc(select(-x)),
fround(select(x))		:=  -fround(select(-x)),
ffloor(select(x))		:=  -fceil(select(-x)),
fceil(select(x))		:=  -ffloor(select(-x)),
exp(select(x))			:=  1 / exp(select(-x)),
sin(select(x))			:=  -sin(select(-x)),
cos(select(x))			:=  cos(select(-x)),
tan(select(x))			:=  -tan(select(-x)),
sec(select(x))			:=  sec(select(-x)),
csc(select(x))			:=  -csc(select(-x)),
cot(select(x))			:=  -cot(select(-x)),
arcsin(select(x))		:=  -arcsin(select(-x)),
arccos(select(x))		:=  4 arctan(1) - arccos(select(-x)),
arctan(select(x))		:=  -arctan(select(-x)),
sinh(select(x))			:=  -sinh(select(-x)),
cosh(select(x))			:=  cosh(select(-x)),
tanh(select(x))			:=  -tanh(select(-x)),
sech(select(x))			:=  sech(select(-x)),
csch(select(x))			:=  -csch(select(-x)),
coth(select(x))			:=  -coth(select(-x)),
arcsinh(select(x))		:=  -arcsinh(select(-x)),
arctanh(select(x))		:=  -arctanh(select(-x)),
select(x) = a			:=  select(-x) = -a,
select(x) != a			:=  select(-x) != -a,
select(x) < a			:=  select(-x) > -a,
select(x) > a			:=  select(-x) < -a,
select(x) <= a			:=  select(-x) >= -a,
select(x) >= a			:=  select(-x) <= -a,
a < select(x)			:=  -a > select(-x),
a > select(x)			:=  -a < select(-x),
a <= select(x)			:=  -a >= select(-x),
a >= select(x)			:=  -a <= select(-x),
select(x)			:=  -select(-x) ]"))

(defun calc-InvertRules ()
  "InvertRules"
  (calc-compile-rule-set
   "InvertRules" "[
iterations(1),
a * select(x)			:=  a / select(1/x),
a / select(x)			:=  a * select(1/x),
select(x) / a			:=  1 / (select(1/x) a),
prod(select(x),b,c,d)		:=  1 / prod(select(1/x),b,c,d),
abs(select(x))			:=  1 / abs(select(1/x)),
sqrt(select(x))			:=  1 / sqrt(select(1/x)),
ln(select(x))			:=  -ln(select(1/x)),
log10(select(x))		:=  -log10(select(1/x)),
log(select(x), a)		:=  -log(select(1/x), a),
log(a, select(x))		:=  -log(a, select(1/x)),
arctan(select(x))               :=  simplify(2 arctan(1))-arctan(select(1/x)),
select(x) = a			:=  select(1/x) = 1/a,
select(x) != a			:=  select(1/x) != 1/a,
select(x) < a			:=  select(1/x) > 1/a,
select(x) > a			:=  select(1/x) < 1/a,
select(x) <= a			:=  select(1/x) >= 1/a,
select(x) >= a			:=  select(1/x) <= 1/a,
a < select(x)			:=  1/a > select(1/x),
a > select(x)			:=  1/a < select(1/x),
a <= select(x)			:=  1/a >= select(1/x),
a >= select(x)			:=  1/a <= select(1/x),
select(x)			:=  1 / select(1/x) ]"))


(defun calc-FactorRules ()
  "FactorRules"
  (calc-compile-rule-set
   "FactorRules" "[
thecoefs(x, [z, a+b, c]) := thefactors(x, [d x + d a/c, (c/d) x + (b/d)])
        :: z = a b/c :: let(d := pgcd(pcont(c), pcont(b))),
thecoefs(x, [z, a, c]) := thefactors(x, [(r x + a/(2 r))^2])
        :: z = (a/2)^2/c :: let(r := esimplify(sqrt(c)))
        :: !matches(r, sqrt(rr)),
thecoefs(x, [z, 0, c]) := thefactors(x, [rc x + rz, rc x - rz])
        :: negative(z)
        :: let(rz := esimplify(sqrt(-z))) :: !matches(rz, sqrt(rzz))
        :: let(rc := esimplify(sqrt(c))) :: !matches(rc, sqrt(rcc)),
thecoefs(x, [z, 0, c]) := thefactors(x, [rz + rc x, rz - rc x])
        :: negative(c)
        :: let(rz := esimplify(sqrt(z))) :: !matches(rz, sqrt(rzz))
        :: let(rc := esimplify(sqrt(-c))) :: !matches(rc, sqrt(rcc))
 ]"))
;;(setq var-FactorRules 'calc-FactorRules)


(defun calc-IntegAfterRules ()
  "IntegAfterRules"
  (calc-compile-rule-set
   "IntegAfterRules" "[
 opt(a) ln(x) + opt(b) ln(y) := 2 a esimplify(arctanh(x-1))
     :: a + b = 0 :: nrat(x + y) = 2 || nrat(x - y) = 2,
 a * (b + c) := a b + a c :: constant(a)
 ]"))

;;(setq var-IntegAfterRules 'calc-IntegAfterRules)


(defun calc-FitRules ()
  "FitRules"
  (calc-compile-rule-set
   "FitRules" "[

schedule(1,2,3,4),
iterations(inf),

phase(1),
e^x  		:=  exp(x),
x^y		:=  exp(y ln(x))  :: !istrue(constant(y)),
x/y		:=  x fitinv(y),
fitinv(x y)	:=  fitinv(x) fitinv(y),
exp(a) exp(b)	:=  exp(a + b),
a exp(b)	:=  exp(ln(a) + b)  :: !hasfitvars(a),
fitinv(exp(a))  :=  exp(-a),
ln(a b)		:=  ln(a) + ln(b),
ln(fitinv(a))	:=  -ln(a),
log10(a b)	:=  log10(a) + log10(b),
log10(fitinv(a)) := -log10(a),
log(a,b)	:=  ln(a)/ln(b),
ln(exp(a))	:=  a,
a*(b+c)		:=  a*b + a*c,
(a+b)^n		:=  x  :: integer(n) :: n >= 2
		       :: let(x, expandpow(a+b,n))
		       :: quote(matches(x,y+z)),

phase(1,2),
fitmodel(y = x)   :=  fitmodel(0, y - x),
fitmodel(y, x+c)  :=  fitmodel(y-c, x)  :: !hasfitparams(c),
fitmodel(y, x c)  :=  fitmodel(y/c, x)  :: !hasfitparams(c),
fitmodel(y, x/(c opt(d)))  :=  fitmodel(y c, x/d)  :: !hasfitparams(c),
fitmodel(y, apply(f,[x]))  :=  fitmodel(yy, x)
			       :: hasfitparams(x)
			       :: let(FTemp() = yy,
			              solve(apply(f,[FTemp()]) = y,
					    FTemp())),
fitmodel(y, apply(f,[x,c]))  :=  fitmodel(yy, x)
				 :: !hasfitparams(c)
				 :: let(FTemp() = yy,
				        solve(apply(f,[FTemp(),c]) = y,
					      FTemp())),
fitmodel(y, apply(f,[c,x]))  :=  fitmodel(yy, x)
				 :: !hasfitparams(c)
				 :: let(FTemp() = yy,
				        solve(apply(f,[c,FTemp()]) = y,
					      FTemp())),

phase(2,3),
fitmodel(y, x)              :=  fitsystem(y, [], [], fitpart(1,1,x)),
fitpart(a,b,plain(x + y))   :=  fitpart(a,b,x) + fitpart(a,b,y),
fitpart(a,b,plain(x - y))   :=  fitpart(a,b,x) + fitpart(-a,b,y),
fitpart(a,b,plain(-x))	    :=  fitpart(-a,b,x),
fitpart(a,b,x opt(c))	    :=  fitpart(a,x b,c)  :: !hasfitvars(x),
fitpart(a,x opt(b),c)	    :=  fitpart(x a,b,c)  :: !hasfitparams(x),
fitpart(a,x y + x opt(z),c) :=	fitpart(a,x*(y+z),c),
fitpart(a,b,c)		    :=  fitpart2(a,b,c),

phase(3),
fitpart2(a1,b1,x) + fitpart2(a2,b2,x)  :=  fitpart(1, a1 b1 + a2 b2, x),
fitpart2(a1,x,c1) + fitpart2(a2,x,c2)  :=  fitpart2(1, x, a1 c1 + a2 c2),

phase(4),
fitinv(x)  	:=  1 / x,
exp(x + ln(y))  :=  y exp(x),
exp(x ln(y))	:=  y^x,
ln(x) + ln(y)	:=  ln(x y),
ln(x) - ln(y)	:=  ln(x/y),
x*y + x*z	:=  x*(y+z),
fitsystem(y, xv, pv, fitpart2(a,fitparam(b),c) + opt(d))
		:=  fitsystem(y, rcons(xv, a c),
      		              rcons(pv, fitdummy(b) = fitparam(b)), d)
		    :: b = vlen(pv)+1,
fitsystem(y, xv, pv, fitpart2(a,b,c) + opt(d))
		:=  fitsystem(y, rcons(xv, a c),
			      rcons(pv, fitdummy(vlen(pv)+1) = b), d),
fitsystem(y, xv, pv, 0)  :=  fitsystem(y, xv, cons(fvh,fvt))
			     :: !hasfitparams(xv)
			     :: let(cons(fvh,fvt),
				    solve(pv, table(fitparam(j), j, 1,
						    hasfitparams(pv)))),
fitparam(n) = x  :=  x ]"))

(provide 'calc-rules)

;;; calc-rules.el ends here
