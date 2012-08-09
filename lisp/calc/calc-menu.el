;;; calc-menu.el --- a menu for Calc

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

(defvar calc-arithmetic-menu
  (list "Arithmetic"
        (list "Basic"
              ["-(1:)"         calc-change-sign
               :keys "n" :active (>= (calc-stack-size) 1)]
              ["(2:) + (1:)"   calc-plus
               :keys "+" :active (>= (calc-stack-size) 2)]
              ["(2:) - (1:)"   calc-minus
               :keys "-" :active (>= (calc-stack-size) 2)]
              ["(2:) * (1:)"   calc-times
               :keys "*" :active (>= (calc-stack-size) 2)]
              ["(2:) / (1:)"   calc-divide
               :keys "/" :active (>= (calc-stack-size) 2)]
              ["(2:) ^ (1:)"   calc-power
               :keys "^" :active (>= (calc-stack-size) 2)]
              ["(2:) ^ (1/(1:))"
               (progn
                 (require 'calc-ext)
                 (let ((calc-inverse-flag t))
                   (call-interactively 'calc-power)))
               :keys "I ^"
               :active (>= (calc-stack-size) 2)
               :help "The (1:)th root of (2:)"]
              ["abs(1:)"
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-abs))
               :keys "A"
               :active (>= (calc-stack-size) 1)
               :help "Absolute value"]
              ["1/(1:)"
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-inv))
               :keys "&"
               :active (>= (calc-stack-size) 1)]
              ["sqrt(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-sqrt))
               :keys "Q"
               :active (>= (calc-stack-size) 1)]
              ["idiv(2:,1:)"
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-idiv))
               :keys "\\"
               :active (>= (calc-stack-size) 2)
               :help "The integer quotient of (2:) over (1:)"]
              ["(2:) mod (1:)"
               (progn
                 (require 'calc-misc)
                 (call-interactively 'calc-mod))
               :keys "%"
               :active (>= (calc-stack-size) 2)
               :help "The remainder when (2:) is divided by (1:)"])
        (list "Rounding"
              ["floor(1:)"
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-floor))
               :keys "F"
               :active (>= (calc-stack-size) 1)
               :help "The greatest integer less than or equal to (1:)"]
              ["ceiling(1:)"
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-ceiling))
               :keys "I F"
               :active (>= (calc-stack-size) 1)
               :help "The smallest integer greater than or equal to (1:)"]
              ["round(1:)"
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-round))
               :keys "R"
               :active (>= (calc-stack-size) 1)
               :help "The nearest integer to (1:)"]
              ["truncate(1:)"
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-trunc))
               :keys "I R"
               :active (>= (calc-stack-size) 1)
               :help "The integer part of (1:)"])
        (list "Complex Numbers"
              ["Re(1:)"
               (progn
                 (require 'calc-cplx)
                 (call-interactively 'calc-re))
               :keys "f r"
               :active (>= (calc-stack-size) 1)]
              ["Im(1:)"
               (progn
                 (require 'calc-cplx)
                 (call-interactively 'calc-im))
               :keys "f i"
               :active (>= (calc-stack-size) 1)]
              ["conj(1:)"
               (progn
                 (require 'calc-cplx)
                 (call-interactively 'calc-conj))
               :keys "J"
               :active (>= (calc-stack-size) 1)
               :help "The complex conjugate of (1:)"]
              ["length(1:)"
               (progn (require 'calc-arith)
                      (call-interactively 'calc-abs))
               :keys "A"
               :active (>= (calc-stack-size) 1)
               :help "The length (absolute value) of (1:)"]
              ["arg(1:)"
               (progn
                 (require 'calc-cplx)
                 (call-interactively 'calc-argument))
               :keys "G"
               :active (>= (calc-stack-size) 1)
               :help "The argument (polar angle) of (1:)"])
        (list "Conversion"
              ["Convert (1:) to a float"
               (progn
                 (require 'calc-ext)
                 (call-interactively 'calc-float))
               :keys "c f"
               :active (>= (calc-stack-size) 1)]
              ["Convert (1:) to a fraction"
               (progn
                 (require 'calc-ext)
                 (call-interactively 'calc-fraction))
               :keys "c F"
               :active (>= (calc-stack-size) 1)])
        (list "Binary"
              ["Set word size"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-word-size))
               :keys "b w"]
              ["Clip (1:) to word size"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-clip))
               :keys "b c"
               :active (>= (calc-stack-size) 1)
               :help "Reduce (1:) modulo 2^wordsize"]
              ["(2:) and (1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-and))
               :keys "b a"
               :active (>= (calc-stack-size) 2)
               :help "Bitwise AND [modulo 2^wordsize]"]
              ["(2:) or (1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-or))
               :keys "b o"
               :active (>= (calc-stack-size) 2)
               :help "Bitwise inclusive OR [modulo 2^wordsize]"]
              ["(2:) xor (1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-xor))
               :keys "b x"
               :active (>= (calc-stack-size) 2)
               :help "Bitwise exclusive OR [modulo 2^wordsize]"]
              ["diff(2:,1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-diff))
               :keys "b d"
               :active (>= (calc-stack-size) 2)
               :help "Bitwise difference [modulo 2^wordsize]"]
              ["not (1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-not))
               :keys "b n"
               :active (>= (calc-stack-size) 1)
               :help "Bitwise NOT [modulo 2^wordsize]"]
              ["left shift(1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-lshift-binary))
               :keys "b l"
               :active (>= (calc-stack-size) 1)
               :help "Shift (1:)[modulo 2^wordsize] one bit left"]
              ["right shift(1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-rshift-binary))
               :keys "b r"
               :active (>= (calc-stack-size) 1)
               :help "Shift (1:)[modulo 2^wordsize] one bit right, putting 0s on the left"]
              ["arithmetic right shift(1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-rshift-arith))
               :keys "b R"
               :active (>= (calc-stack-size) 1)
               :help "Shift (1:)[modulo 2^wordsize] one bit right, duplicating the leftmost bit"]
              ["rotate(1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-rotate-binary))
               :keys "b t"
               :active (>= (calc-stack-size) 1)
               :help "Rotate (1:)[modulo 2^wordsize] one bit left"])
        "-------"
        ["Help on Arithmetic"
         (calc-info-goto-node "Arithmetic")])
  "Menu for Calc's arithmetic functions.")

(defvar calc-scientific-function-menu
  (list "Scientific Functions"
        (list "Constants"
              ["pi"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-pi))
               :keys "P"]
              ["e"
               (progn
                 (require 'calc-math)
                 (let ((calc-hyperbolic-flag t))
                   (call-interactively 'calc-pi)))
               :keys "H P"]
              ["phi"
               (progn
                 (require 'calc-math)
                 (let ((calc-inverse-flag t)
                       (calc-hyperbolic-flag t))
                   (call-interactively 'calc-pi)))
               :keys "I H P"
               :help "The golden ratio"]
              ["gamma"
               (progn
                 (require 'calc-math)
                 (let ((calc-inverse-flag t))
                   (call-interactively 'calc-pi)))
               :keys "I P"
               :help "Euler's constant"])
        (list "Logs and Exps"
              ["ln(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-ln))
               :keys "L"
               :active (>= (calc-stack-size) 1)
               :help "The natural logarithm"]
              ["e^(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-exp))
               :keys "E"
               :active (>= (calc-stack-size) 1)]
              ["log(1:) [base 10]"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-log10))
               :keys "H L"
               :active (>= (calc-stack-size) 1)
               :help "The common logarithm"]
              ["10^(1:)"
               (progn
                 (require 'calc-math)
                 (let ((calc-inverse-flag t))
                   (call-interactively 'calc-log10)))
               :keys "I H L"
               :active (>= (calc-stack-size) 1)]
              ["log(2:) [base(1:)]"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-log))
               :keys "B"
               :active (>= (calc-stack-size) 2)
               :help "The logarithm with an arbitrary base"]
              ["(2:) ^ (1:)"
               calc-power
               :keys "^"
               :active (>= (calc-stack-size) 2)])
        (list "Trigonometric Functions"
              ["sin(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-sin))
               :keys "S"
               :active (>= (calc-stack-size) 1)]
              ["cos(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-cos))
               :keys "C"
               :active (>= (calc-stack-size) 1)]
              ["tan(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-tan))
               :keys "T"
               :active (>= (calc-stack-size) 1)]
              ["arcsin(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arcsin))
               :keys "I S"
               :active (>= (calc-stack-size) 1)]
              ["arccos(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arccos))
               :keys "I C"
               :active (>= (calc-stack-size) 1)]
              ["arctan(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arctan))
               :keys "I T"
               :active (>= (calc-stack-size) 1)]
              ["arctan2(2:,1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arctan2))
               :keys "f T"
               :active (>= (calc-stack-size) 2)]
              "--Angle Measure--"
              ["Radians"
               (progn
                 (require 'calc-math)
                 (calc-radians-mode))
               :keys "m r"
               :style radio
               :selected (eq calc-angle-mode 'rad)]
              ["Degrees"
               (progn
                 (require 'calc-math)
                 (calc-degrees-mode))
               :keys "m d"
               :style radio
               :selected (eq calc-angle-mode 'deg)]
              ["HMS"
               (progn
                 (require 'calc-math)
                 (calc-hms-mode))
               :keys "m h"
               :style radio
               :selected (eq calc-angle-mode 'hms)])
        (list "Hyperbolic Functions"
              ["sinh(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-sinh))
               :keys "H S"
               :active (>= (calc-stack-size) 1)]
              ["cosh(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-cosh))
               :keys "H C"
               :active (>= (calc-stack-size) 1)]
              ["tanh(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-tanh))
               :keys "H T"
               :active (>= (calc-stack-size) 1)]
              ["arcsinh(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arcsinh))
               :keys "I H S"
               :active (>= (calc-stack-size) 1)]
              ["arccosh(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arccosh))
               :keys "I H C"
               :active (>= (calc-stack-size) 1)]
              ["arctanh(1:)"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arctanh))
               :keys "I H T"
               :active (>= (calc-stack-size) 1)])
        (list "Advanced Math Functions"
              ["Gamma(1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-gamma))
               :keys "f g"
               :active (>= (calc-stack-size) 1)
               :help "The Euler Gamma function"]
              ["GammaP(2:,1:)"
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-inc-gamma))
               :keys "f G"
               :active (>= (calc-stack-size) 2)
               :help "The lower incomplete Gamma function"]
              ["Beta(2:,1:)"
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-beta))
               :keys "f b"
               :active (>= (calc-stack-size) 2)
               :help "The Euler Beta function"]
              ["BetaI(3:,2:,1:)"
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-inc-beta))
               :keys "f B"
               :active (>= (calc-stack-size) 3)
               :help "The incomplete Beta function"]
              ["erf(1:)"
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-erf))
               :keys "f e"
               :active (>= (calc-stack-size) 1)
               :help "The error function"]
              ["BesselJ(2:,1:)"
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-bessel-J))
               :keys "f j"
               :active (>= (calc-stack-size) 2)
               :help "The Bessel function of the first kind (of order (2:))"]
              ["BesselY(2:,1:)"
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-bessel-Y))
               :keys "f y"
               :active (>= (calc-stack-size) 2)
               :help "The Bessel function of the second kind (of order (2:))"])
        (list "Combinatorial Functions"
              ["gcd(2:,1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-gcd))
                 :keys "k g"
                 :active (>= (calc-stack-size) 2)]
              ["lcm(2:,1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-lcm))
               :keys "k l"
               :active (>= (calc-stack-size) 2)]
              ["factorial(1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-factorial))
               :keys "!"
               :active (>= (calc-stack-size) 1)]
              ["(2:) choose (1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-choose))
               :keys "k c"
               :active (>= (calc-stack-size) 2)]
              ["permutations(2:,1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-perm))
               :keys "H k c"
               :active (>= (calc-stack-size) 2)]
              ["Primality test for (1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-prime-test))
               :keys "k p"
               :active (>= (calc-stack-size) 1)
               :help "For large (1:), a probabilistic test"]
              ["Factor (1:) into primes"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-prime-factors))
               :keys "k f"
               :active (>= (calc-stack-size) 1)]
              ["Next prime after (1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-next-prime))
               :keys "k n"
               :active (>= (calc-stack-size) 1)]
              ["Previous prime before (1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-prev-prime))
               :keys "I k n"
               :active (>= (calc-stack-size) 1)]
              ["phi(1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-totient))
               :keys "k n"
               :active (>= (calc-stack-size) 1)
               :help "Euler's totient function"]
              ["random(1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-random))
               :keys "k r"
               :active (>= (calc-stack-size) 1)
               :help "A random number >=1 and < (1:)"])
        "----"
        ["Help on Scientific Functions"
         (calc-info-goto-node "Scientific Functions")])
  "Menu for Calc's scientific functions.")

(defvar calc-algebra-menu
  (list "Algebra"
        (list "Simplification"
              ["Simplify (1:)"
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-simplify))
               :keys "a s"
               :active (>= (calc-stack-size) 1)]
              ["Simplify (1:) with extended rules"
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-simplify-extended))
               :keys "a e"
               :active (>= (calc-stack-size) 1)
               :help "Apply possibly unsafe simplifications"])
        (list "Manipulation"
              ["Expand formula (1:)"
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-expand-formula))
               :keys "a \""
               :active (>= (calc-stack-size) 1)
               :help "Expand (1:) into its defining formula, if possible"]
              ["Evaluate variables in (1:)"
               (progn
                 (require 'calc-ext)
                 (call-interactively 'calc-evaluate))
               :keys "="
               :active (>= (calc-stack-size) 1)]
              ["Make substitution in (1:)"
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-substitute))
               :keys "a b"
               :active (>= (calc-stack-size) 1)
               :help
               "Substitute all occurrences of a sub-expression with a new sub-expression"])
        (list "Polynomials"
              ["Factor (1:)"
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-factor))
               :keys "a f"
               :active (>= (calc-stack-size) 1)]
              ["Collect terms in (1:)"
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-collect))
               :keys "a c"
               :active (>= (calc-stack-size) 1)
               :help "Arrange as a polynomial in a given variable"]
              ["Expand (1:)"
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-expand))
               :keys "a x"
               :active (>= (calc-stack-size) 1)
               :help "Apply distributive law everywhere"]
              ["Find roots of (1:)"
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-poly-roots))
               :keys "a P"
               :active (>= (calc-stack-size) 1)])
        (list "Calculus"
              ["Differentiate (1:)"
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-derivative))
               :keys "a d"
               :active (>= (calc-stack-size) 1)]
              ["Integrate (1:) [indefinite]"
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-integral))
               :keys "a i"
               :active (>= (calc-stack-size) 1)]
              ["Integrate (1:) [definite]"
               (progn
                 (require 'calcalg2)
                 (let ((var (read-string "Integration variable: ")))
                   (calc-tabular-command 'calcFunc-integ "Integration"
                                         "intg" nil var nil nil)))
               :keys "C-u a i"
               :active (>= (calc-stack-size) 1)]
              ["Integrate (1:) [numeric]"
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-num-integral))
               :keys "a I"
               :active (>= (calc-stack-size) 1)
               :help "Integrate using the open Romberg method"]
              ["Taylor expand (1:)"
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-taylor))
               :keys "a t"
               :active (>= (calc-stack-size) 1)]
              ["Minimize (2:) [initial guess = (1:)]"
               (progn
                 (require 'calcalg3)
                 (call-interactively 'calc-find-minimum))
               :keys "a N"
               :active (>= (calc-stack-size) 2)
               :help "Find a local minimum"]
              ["Maximize (2:) [initial guess = (1:)]"
               (progn
                 (require 'calcalg3)
                 (call-interactively 'calc-find-maximum))
               :keys "a X"
               :active (>= (calc-stack-size) 2)
               :help "Find a local maximum"])
        (list "Solving"
              ["Solve equation (1:)"
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-solve-for))
               :keys "a S"
               :active (>= (calc-stack-size) 1)]
              ["Solve equation (2:) numerically [initial guess = (1:)]"
               (progn
                 (require 'calcalg3)
                 (call-interactively 'calc-find-root))
               :keys "a R"
               :active (>= (calc-stack-size) 2)]
              ["Find roots of polynomial (1:)"
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-poly-roots))
               :keys "a P"
               :active (>= (calc-stack-size) 1)])
        (list "Curve Fitting"
              ["Fit (1:)=[x values, y values] to a curve"
               (progn
                 (require 'calcalg3)
                 (call-interactively 'calc-curve-fit))
               :keys "a F"
               :active (>= (calc-stack-size) 1)])
        "----"
        ["Help on Algebra"
         (calc-info-goto-node "Algebra")])
  "Menu for Calc's algebraic facilities.")


(defvar calc-graphics-menu
  (list "Graphics"
        ["Graph 2D [(1:)= y values, (2:)= x values]"
         (progn
           (require 'calc-graph)
           (call-interactively 'calc-graph-fast))
         :keys "g f"
         :active (>= (calc-stack-size) 2)]
        ["Graph 3D [(1:)= z values, (2:)= y values, (3:)= x values]"
         (progn
           (require 'calc-graph)
           (call-interactively 'calc-graph-fast-3d))
         :keys "g F"
         :active (>= (calc-stack-size) 3)]
        "----"
        ["Help on Graphics"
         (calc-info-goto-node "Graphics")])
  "Menu for Calc's graphics.")


(defvar calc-vectors-menu
  (list "Matrices/Vectors"
        (list "Matrices"
              ["(2:) + (1:)"   calc-plus
               :keys "+" :active (>= (calc-stack-size) 2)]
              ["(2:) - (1:)"   calc-minus
               :keys "-" :active (>= (calc-stack-size) 2)]
              ["(2:) * (1:)"   calc-times
               :keys "*" :active (>= (calc-stack-size) 2)]
              ["(1:)^(-1)"
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-inv))
               :keys "&"
               :active (>= (calc-stack-size) 1)]
              ["Create an identity matrix"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-ident))
               :keys "v i"]
              ["transpose(1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-transpose))
               :keys "v t"
               :active (>= (calc-stack-size) 1)]
              ["det(1:)"
               (progn
                 (require 'calc-mtx)
                 (call-interactively 'calc-mdet))
               :keys "V D"
               :active (>= (calc-stack-size) 1)]
              ["trace(1:)"
               (progn
                 (require 'calc-mtx)
                 (call-interactively 'calc-mtrace))
               :keys "V T"
               :active (>= (calc-stack-size) 1)]
              ["LUD decompose (1:)"
               (progn
                 (require 'calc-mtx)
                 (call-interactively 'calc-mlud))
               :keys "V L"
               :active (>= (calc-stack-size) 1)]
              ["Extract a row from (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-mrow))
               :keys "v r"
               :active (>= (calc-stack-size) 1)]
              ["Extract a column from (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-mcol))
               :keys "v c"
               :active (>= (calc-stack-size) 1)])
        (list "Vectors"
              ["Extract the first element of (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-head))
               :keys "v h"
               :active (>= (calc-stack-size) 1)]
              ["Extract an element from (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-mrow))
               :keys "v r"
               :active (>= (calc-stack-size) 1)]
              ["Reverse (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-reverse-vector))
               :keys "v v"
               :active (>= (calc-stack-size) 1)]
              ["Unpack (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-unpack))
               :keys "v u"
               :active (>= (calc-stack-size) 1)
               :help "Separate the elements of (1:)"]
              ["(2:) cross (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-cross))
               :keys "V C"
               :active (>= (calc-stack-size) 2)
               :help "The cross product in R^3"]
              ["(2:) dot (1:)"
               calc-mult
               :keys "*"
               :active (>= (calc-stack-size) 2)
               :help "The dot product"]
              ["Map a function across (1:)"
               (progn
                 (require 'calc-map)
                 (call-interactively 'calc-map))
               :keys "V M"
               :active (>= (calc-stack-size) 1)
               :help "Apply a function to each element"])
        (list "Vectors As Sets"
              ["Remove duplicates from (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-remove-duplicates))
               :keys "V +"
               :active (>= (calc-stack-size) 1)]
              ["(2:) union (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-set-union))
               :keys "V V"
               :active (>= (calc-stack-size) 2)]
              ["(2:) intersect (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-set-intersect))
               :keys "V ^"
               :active (>= (calc-stack-size) 2)]
              ["(2:) \\ (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-set-difference))
               :keys "V -"
               :help "Set difference"
               :active (>= (calc-stack-size) 2)])
        (list "Statistics On Vectors"
              ["length(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-count))
               :keys "u #"
               :active (>= (calc-stack-size) 1)
               :help "The number of data values"]
              ["sum(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-sum))
               :keys "u +"
               :active (>= (calc-stack-size) 1)
               :help "The sum of the data values"]
              ["max(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-max))
               :keys "u x"
               :active (>= (calc-stack-size) 1)
               :help "The maximum of the data values"]
              ["min(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-min))
               :keys "u N"
               :active (>= (calc-stack-size) 1)
               :help "The minimum of the data values"]
              ["mean(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-mean))
               :keys "u M"
               :active (>= (calc-stack-size) 1)
               :help "The average (arithmetic mean) of the data values"]
              ["mean(1:) with error"
              (progn
                (require 'calc-stat)
                (call-interactively 'calc-vector-mean-error))
              :keys "I u M"
              :active (>= (calc-stack-size) 1)
              :help "The average (arithmetic mean) of the data values as an error form"]
              ["sdev(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-sdev))
               :keys "u S"
               :active (>= (calc-stack-size) 1)
               :help "The sample sdev, sqrt[sum((values - mean)^2)/(N-1)]"]
              ["variance(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-variance))
               :keys "H u S"
               :active (>= (calc-stack-size) 1)
               :help "The sample variance, sum((values - mean)^2)/(N-1)"]
              ["population sdev(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-pop-sdev))
               :keys "I u S"
               :active (>= (calc-stack-size) 1)
               :help "The population sdev, sqrt[sum((values - mean)^2)/N]"]
              ["population variance(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-pop-variance))
               :keys "H I u S"
               :active (>= (calc-stack-size) 1)
               :help "The population variance, sum((values - mean)^2)/N"]
              ["median(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-median))
               :keys "H u M"
               :active (>= (calc-stack-size) 1)
               :help "The median of the data values"]
              ["harmonic mean(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-harmonic-mean))
               :keys "H I u M"
               :active (>= (calc-stack-size) 1)]
              ["geometric mean(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-geometric-mean))
               :keys "u G"
               :active (>= (calc-stack-size) 1)]
              ["arithmetic-geometric mean(1:)"
               (progn
                 (require 'calc-stat)
                 (let ((calc-hyperbolic-flag t))
                   (call-interactively 'calc-vector-geometric-mean)))
               :keys "H u G"
               :active (>= (calc-stack-size) 1)]
               ["RMS(1:)"
                (progn (require 'calc-arith)
                       (call-interactively 'calc-abs))
                :keys "A"
                :active (>= (calc-stack-size) 1)
                :help "The root-mean-square, or quadratic mean"])
        ["Abbreviate long vectors"
         (progn
           (require 'calc-mode)
           (call-interactively 'calc-full-vectors))
         :keys "v ."
         :style toggle
         :selected (not calc-full-vectors)]
        "----"
        ["Help on Matrices/Vectors"
         (calc-info-goto-node "Matrix Functions")])
  "Menu for Calc's vector and matrix functions.")

(defvar calc-units-menu
  (list "Units"
        ["Convert units in (1:)"
         (progn
           (require 'calc-units)
           (call-interactively 'calc-convert-units ))
         :keys "u c"
         :active (>= (calc-stack-size) 1)]
        ["Convert temperature in (1:)"
         (progn
           (require 'calc-units)
           (call-interactively 'calc-convert-temperature))
         :keys "u t"
         :active (>= (calc-stack-size) 1)]
        ["Simplify units in (1:)"
         (progn
           (require 'calc-units)
           (call-interactively 'calc-simplify-units))
         :keys "u s"
         :active (>= (calc-stack-size) 1)]
        ["View units table"
         (progn
           (require 'calc-units)
           (call-interactively 'calc-view-units-table))
         :keys "u V"]
        (list "Logarithmic Units"
              ["Convert (1:) to dB (power)"
               (progn
                 (require 'calc-units)
                 (call-interactively 'calc-db))
               :keys "l d"
               :active (>= (calc-stack-size) 1)]
              ["Convert (2:) to dB (power) with reference level (1:)"
               (progn
                 (require 'calc-units)
                 (let ((calc-option-flag t))
                   (call-interactively 'calc-db)))
               :keys "O l d"
               :active (>= (calc-stack-size) 2)]
              ["Convert (1:) to Np (power)"
               (progn
                 (require 'calc-units)
                 (call-interactively 'calc-np))
               :keys "l n"
               :active (>= (calc-stack-size) 1)]
              ["Convert (2:) to Np (power) with reference level (1:)"
               (progn
                 (require 'calc-units)
                 (let ((calc-option-flag t))
                   (call-interactively 'calc-np)))
               :keys "O l n"
               :active (>= (calc-stack-size) 2)]
              ["Convert (1:) to power quantity"
               (progn
                 (require 'calc-units)
                 (call-interactively 'calc-lu-quant))
               :keys "l q"
               :active (>= (calc-stack-size) 1)]
              ["Convert (2:) to power quantity with reference level (1:)"
               (progn
                 (require 'calc-units)
                 (let ((calc-option-flag t))
                   (call-interactively 'calc-lu-quant)))
               :keys "O l q"
               :active (>= (calc-stack-size) 2)]
              "----"
              ["Convert (1:) to dB (field)"
               (progn
                 (require 'calc-units)
                 (let ((calc-hyperbolic-flag t))
                   (call-interactively 'calc-db)))
               :keys "H l d"
               :active (>= (calc-stack-size) 1)]
              ["Convert (2:) to dB (field) with reference level (1:)"
               (progn
                 (require 'calc-units)
                 (let ((calc-option-flag t)
                       (calc-hyperbolic-flag t))
                   (call-interactively 'calc-db)))
               :keys "O H l d"
               :active (>= (calc-stack-size) 2)]
              ["Convert (1:) to Np (field)"
               (progn
                 (require 'calc-units)
                 (let ((calc-hyperbolic-flag t))
                   (call-interactively 'calc-np)))
               :keys "H l n"
               :active (>= (calc-stack-size) 1)]
              ["Convert (2:) to Np (field) with reference level (1:)"
               (progn
                 (require 'calc-units)
                 (let ((calc-option-flag t)
                       (calc-hyperbolic-flag t))
                   (call-interactively 'calc-np)))
               :keys "O H l d"
               :active (>= (calc-stack-size) 2)]
              ["Convert (1:) to field quantity"
               (progn
                 (require 'calc-units)
                 (let ((calc-hyperbolic-flag t))
                   (call-interactively 'calc-lu-quant)))
               :keys "H l q"
               :active (>= (calc-stack-size) 1)]
              ["Convert (2:) to field quantity with reference level (1:)"
               (progn
                 (require 'calc-units)
                 (let ((calc-option-flag t)
                       (calc-hyperbolic-flag))
                   (call-interactively 'calc-lu-quant)))
               :keys "O H l q"
               :active (>= (calc-stack-size) 2)])
        (list "Musical Notes"
              ["Convert (1:) to scientific pitch notation"
               (progn
                 (require 'calc-units)
                 (call-interactively 'calc-spn))
               :keys "l s"
               :active (>= (calc-stack-size) 1)]
              ["Convert (1:) to midi number"
               (progn
                 (require 'calc-units)
                 (call-interactively 'calc-midi))
               :keys "l m"
               :active (>= (calc-stack-size) 1)]
              ["Convert (1:) to frequency"
               (progn
                 (require 'calc-units)
                 (call-interactively 'calc-freq))
               :keys "l f"
               :active (>= (calc-stack-size) 1)])
        "----"
        ["Help on Units"
         (calc-info-goto-node "Units")])
  "Menu for Calc's units functions.")

(defvar calc-variables-menu
  (list "Variables"
        ["Store (1:) into a variable"
         (progn
           (require 'calc-store)
           (call-interactively 'calc-store))
         :keys "s s"
         :active (>= (calc-stack-size) 1)]
        ["Recall a variable value"
          (progn
            (require 'calc-store)
            (call-interactively 'calc-recall ))
         :keys "s r"]
        ["Edit the value of a variable"
         (progn
           (require 'calc-store)
           (call-interactively 'calc-edit-variable))
         :keys "s e"]
        ["Exchange (1:) with a variable value"
         (progn
           (require 'calc-store)
           (call-interactively 'calc-store-exchange))
         :keys "s x"
         :active (>= (calc-stack-size) 1)]
        ["Clear variable value"
         (progn
           (require 'calc-store)
           (call-interactively 'calc-unstore))
         :keys "s u"]
        ["Evaluate variables in (1:)"
         (progn
           (require 'calc-ext)
           (call-interactively 'calc-evaluate))
         :keys "="
         :active (>= (calc-stack-size) 1)]
        ["Evaluate (1:), assigning a value to a variable"
         (progn
           (require 'calc-store)
           (call-interactively 'calc-let))
         :keys "s l"
         :active (>= (calc-stack-size) 1)
         :help "Evaluate (1:) under a temporary assignment of a variable"]
        "----"
        ["Help on Variables"
         (calc-info-goto-node "Store and Recall")])
  "Menu for Calc's variables.")

(defvar calc-stack-menu
  (list "Stack"
        ["Remove (1:)"
         calc-pop
         :keys "DEL"
         :active (>= (calc-stack-size) 1)]
        ["Switch (1:) and (2:)"
         calc-roll-down
         :keys "TAB"
         :active (>= (calc-stack-size) 2)]
        ["Duplicate (1:)"
         calc-enter
         :keys "RET"
         :active (>= (calc-stack-size) 1)]
        ["Edit (1:)"
         (progn
           (require 'calc-yank)
           (call-interactively calc-edit))
         :keys "`"
         :active (>= (calc-stack-size) 1)]
        "----"
        ["Help on Stack"
         (calc-info-goto-node "Stack and Trail")])
  "Menu for Calc's stack functions.")

(defvar calc-errors-menu
  (list "Undo"
        ["Undo"
         (progn
           (require 'calc-undo)
           (call-interactively 'calc-undo))
         :keys "U"]
        ["Redo"
         (progn
           (require 'calc-undo)
           (call-interactively 'calc-redo))
         :keys "D"]
        "----"
        ["Help on Undo"
         (progn
           (calc-info-goto-node "Introduction")
           (Info-goto-node "Undo"))]))

(defvar calc-modes-menu
  (list "Modes"
        ["Precision"
         (progn
           (require 'calc-ext)
           (call-interactively 'calc-precision))
         :keys "p"
         :help "Set the precision for floating point calculations"]
        ["Fraction mode"
         (progn
           (require 'calc-frac)
           (call-interactively 'calc-frac-mode))
         :keys "m f"
         :style toggle
         :selected calc-prefer-frac
         :help "Leave integer quotients as fractions"]
        ["Symbolic mode"
         (lambda ()
           (interactive)
           (require 'calc-mode)
           (calc-symbolic-mode nil))
         :keys "m s"
         :style toggle
         :selected calc-symbolic-mode
         :help "Leave functions producing inexact answers in symbolic form"]
        ["Infinite mode"
         (lambda ()
           (interactive)
           (require 'calc-mode)
           (calc-infinite-mode nil))
         :keys "m i"
         :style toggle
         :selected calc-infinite-mode
         :help "Let expressions like 1/0 produce infinite results"]
        ["Abbreviate long vectors"
         (progn
           (require 'calc-mode)
           (call-interactively 'calc-full-vectors))
         :keys "v ."
         :style toggle
         :selected (not calc-full-vectors)]
        (list "Angle Measure"
              ["Radians"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-radians-mode))
               :keys "m r"
               :style radio
               :selected (eq calc-angle-mode 'rad)]
              ["Degrees"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-degrees-mode))
               :keys "m d"
               :style radio
               :selected (eq calc-angle-mode 'deg)]
              ["HMS"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-hms-mode))
               :keys "m h"
               :style radio
               :selected (eq calc-angle-mode 'hms)])
        (list "Radix"
              ["Decimal"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-decimal-radix))
               :keys "d 0"
               :style radio
               :selected (and (= calc-number-radix 10)
                              (not calc-twos-complement-mode))]
              ["Binary"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-binary-radix))
               :keys "d 2"
               :style radio
               :selected (and (= calc-number-radix 2)
                              (not calc-twos-complement-mode))]
              ["Octal"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-octal-radix))
               :keys "d 8"
               :style radio
               :selected (and (= calc-number-radix 8)
                              (not calc-twos-complement-mode))]
              ["Hexadecimal"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-hex-radix))
               :keys "d 6"
               :style radio
               :selected (and (= calc-number-radix 16)
                              (not calc-twos-complement-mode))]
              ["Other"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-radix))
               :keys "d r"
               :style radio
               :selected (and
                          (not calc-twos-complement-mode)
                          (not
                           (or
                            (= calc-number-radix 10)
                            (= calc-number-radix 2)
                            (= calc-number-radix 8)
                            (= calc-number-radix 16))))]
              ["--Two's Complement--"
               (lambda () ())
               :style radio
               :selected nil]
              ["Binary"
               (progn
                 (require 'calc-bin)
                 (call-interactively
                  (lambda () (interactive) (calc-binary-radix t))))
               :keys "C-u d 2"
               :style radio
               :selected (and (= calc-number-radix 2)
                              calc-twos-complement-mode)]
              ["Octal"
               (progn
                 (require 'calc-bin)
                 (call-interactively
                  (lambda () (interactive) (calc-octal-radix t))))
               :keys "C-u d 8"
               :style radio
               :selected (and (= calc-number-radix 8)
                              calc-twos-complement-mode)]
              ["Hexadecimal"
               (progn
                 (require 'calc-bin)
                 (call-interactively
                  (lambda () (interactive) (calc-hex-radix t))))
               :keys "C-u d 6"
               :style radio
               :selected (and (= calc-number-radix 16)
                              calc-twos-complement-mode)])
        (list "Float Format"
              ["Normal"
               (progn
                 (require 'calc-mode)
                 (call-interactively 'calc-normal-notation))
               :keys "d n"
               :style radio
               :selected (eq (car-safe calc-float-format) 'float)]
              ["Fixed point"
               (progn
                 (require 'calc-mode)
                 (call-interactively 'calc-fix-notation))
               :keys "d f"
               :style radio
               :selected (eq (car-safe calc-float-format) 'fix)]
              ["Scientific notation"
               (progn
                 (require 'calc-mode)
                 (call-interactively 'calc-sci-notation))
               :keys "d s"
               :style radio
               :selected (eq (car-safe calc-float-format) 'sci)]
              ["Engineering notation"
               (progn
                 (require 'calc-mode)
                 (call-interactively 'calc-eng-notation))
               :keys "d e"
               :style radio
               :selected (eq (car-safe calc-float-format) 'eng)])
        (list "Complex Format"
              ["Default"
               (progn
                 (require 'calc-cplx)
                 (calc-complex-notation))
               :style radio
               :selected (not calc-complex-format)
               :keys "d c"
               :help "Display complex numbers as ordered pairs."]
              ["i notation"
               (progn
                 (require 'calc-cplx)
                 (calc-i-notation))
               :style radio
               :selected (eq calc-complex-format 'i)
               :keys "d i"
               :help "Display complex numbers as a+bi."]
              ["j notation"
               (progn
                 (require 'calc-cplx)
                 (calc-i-notation))
               :style radio
               :selected (eq calc-complex-format 'j)
               :keys "d j"
               :help "Display complex numbers as a+bj."]
              ["Other"
               (calc-complex-notation)
               :style radio
               :selected (and calc-complex-format
                              (not (eq calc-complex-format 'i))
                              (not (eq calc-complex-format 'j)))
               :active nil]
              "----"
              ["Polar mode"
               (progn
                 (require 'calc-cplx)
                 (calc-polar-mode nil))
               :style toggle
               :selected (eq calc-complex-mode 'polar)
               :keys "m p"
               :help "Prefer polar form for complex numbers."])
        (list "Algebraic"
              ["Normal"
               (progn
                 (require 'calc-mode)
                 (cond
                  (calc-incomplete-algebraic-mode
                   (calc-algebraic-mode t))
                  (calc-algebraic-mode
                   (calc-algebraic-mode nil))))
               :style radio
               :selected (not calc-algebraic-mode)]
              ["Algebraic mode"
               (progn
                 (require 'calc-mode)
                 (if (or
                      calc-incomplete-algebraic-mode
                      (not calc-algebraic-mode))
                     (calc-algebraic-mode nil)))
               :keys "m a"
               :style radio
               :selected (and calc-algebraic-mode
                              (not calc-incomplete-algebraic-mode))
               :help "Keys which start numeric entry also start algebraic entry"]
              ["Incomplete algebraic mode"
               (progn
                 (require 'calc-mode)
                 (unless calc-incomplete-algebraic-mode
                   (calc-algebraic-mode t)))
               :keys "C-u m a"
               :style radio
               :selected calc-incomplete-algebraic-mode
               :help "Only ( and [ begin algebraic entry"]
              ["Total algebraic mode"
               (progn
                 (require 'calc-mode)
                 (unless (eq calc-algebraic-mode 'total)
                   (calc-total-algebraic-mode nil)))
               :keys "m t"
               :style radio
               :selected (eq calc-algebraic-mode 'total)
               :help "All regular letters and punctuation begin algebraic entry"])
        (list "Language"
              ["Normal"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-normal-language))
               :keys "d N"
               :style radio
               :selected (eq calc-language nil)]
              ["Big"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-big-language))
               :keys "d B"
               :style radio
               :selected (eq calc-language 'big)
               :help "Use textual approximations to various mathematical notations"]
              ["Flat"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-flat-language))
               :keys "d O"
               :style radio
               :selected (eq calc-language 'flat)
               :help "Write matrices on a single line"]
              ["C"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-c-language))
               :keys "d C"
               :style radio
               :selected (eq calc-language 'c)]
              ["Pascal"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-pascal-language))
               :keys "d P"
               :style radio
               :selected (eq calc-language 'pascal)]
              ["Fortran"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-fortran-language))
               :keys "d F"
               :style radio
               :selected (eq calc-language 'fortran)]
              ["TeX"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-tex-language))
               :keys "d T"
               :style radio
               :selected (eq calc-language 'tex)]
              ["LaTeX"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-latex-language))
               :keys "d L"
               :style radio
               :selected (eq calc-language 'latex)]
              ["Eqn"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-eqn-language))
               :keys "d E"
               :style radio
               :selected (eq calc-language 'eqn)]
              ["Yacas"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-yacas-language))
               :keys "d Y"
               :style radio
               :selected (eq calc-language 'yacas)]
              ["Maxima"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-maxima-language))
               :keys "d X"
               :style radio
               :selected (eq calc-language 'maxima)]
              ["Giac"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-giac-language))
               :keys "d A"
               :style radio
               :selected (eq calc-language 'giac)]
              ["Mma"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-mathematica-language))
               :keys "d M"
               :style radio
               :selected (eq calc-language 'math)]
              ["Maple"
               (progn
                 (require 'calc-lang)
                 (call-interactively 'calc-maple-language))
               :keys "d W"
               :style radio
               :selected (eq calc-language 'maple)])
        "----"
        ["Save mode settings" calc-save-modes :keys "m m"]
        "----"
        ["Help on Modes"
         (calc-info-goto-node "Mode settings")])
  "Menu for Calc's mode settings.")

(defvar  calc-help-menu
  (list "Help"
        ["Manual"
         calc-info
         :keys "h i"]
        ["Tutorial"
         calc-tutorial
         :keys "h t"]
        ["Summary"
         calc-info-summary
         :keys "h s"]
        "----"
        ["Help on Help"
         (progn
           (calc-info-goto-node "Introduction")
           (Info-goto-node "Help Commands"))])
  "Menu for Calc's help functions.")

(defvar calc-mode-map)

(easy-menu-define
  calc-menu
  calc-mode-map
  "Menu for Calc."
  (list "Calc"
        :visible '(eq major-mode 'calc-mode)
        calc-arithmetic-menu
        calc-scientific-function-menu
        calc-algebra-menu
        calc-graphics-menu
        calc-vectors-menu
        calc-units-menu
        calc-variables-menu
        calc-stack-menu
        calc-errors-menu
        calc-modes-menu
        calc-help-menu
        ["Reset"
         (progn
           (require 'calc-ext)
           (call-interactively 'calc-reset))
         :help "Reset Calc to its initial state"]
        ["Quit" calc-quit]))

(provide 'calc-menu)
