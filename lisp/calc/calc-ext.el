;;; calc-ext.el --- various extension functions for Calc

;; Copyright (C) 1990-1993, 2001-2012  Free Software Foundation, Inc.

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

(require 'calc)
(require 'calc-macs)

;; Declare functions which are defined elsewhere.
(declare-function math-clip "calc-bin" (a &optional w))
(declare-function math-round "calc-arith" (a &optional prec))
(declare-function math-simplify "calc-alg" (top-expr))
(declare-function math-simplify-extended "calc-alg" (a))
(declare-function math-simplify-units "calc-units" (a))
(declare-function calc-set-language "calc-lang" (lang &optional option no-refresh))
(declare-function calc-flush-caches "calc-stuff" (&optional inhibit-msg))
(declare-function calc-save-modes "calc-mode" ())
(declare-function calc-embedded-modes-change "calc-embed" (vars))
(declare-function calc-embedded-var-change "calc-embed" (var &optional buf))
(declare-function math-mul-float "calc-arith" (a b))
(declare-function math-arctan-raw "calc-math" (x))
(declare-function math-sqrt-raw "calc-math" (a &optional guess))
(declare-function math-sqrt-float "calc-math" (a &optional guess))
(declare-function math-exp-minus-1-raw "calc-math" (x))
(declare-function math-normalize-polar "calc-cplx" (a))
(declare-function math-normalize-hms "calc-forms" (a))
(declare-function math-normalize-mod "calc-forms" (a))
(declare-function math-make-sdev "calc-forms" (x sigma))
(declare-function math-make-intv "calc-forms" (mask lo hi))
(declare-function math-normalize-logical-op "calc-prog" (a))
(declare-function math-possible-signs "calc-arith" (a &optional origin))
(declare-function math-infinite-dir "calc-math" (a &optional inf))
(declare-function math-calcFunc-to-var "calc-map" (f))
(declare-function calc-embedded-evaluate-expr "calc-embed" (x))
(declare-function math-known-nonzerop "calc-arith" (a))
(declare-function math-read-expr-level "calc-aent" (exp-prec &optional exp-term))
(declare-function math-read-big-rec "calc-lang" (math-rb-h1 math-rb-v1 math-rb-h2 math-rb-v2 &optional baseline prec short))
(declare-function math-read-big-balance "calc-lang" (h v what &optional commas))
(declare-function math-format-date "calc-forms" (math-fd-date))
(declare-function math-vector-is-string "calccomp" (a))
(declare-function math-vector-to-string "calccomp" (a &optional quoted))
(declare-function math-format-radix-float "calc-bin" (a prec))
(declare-function math-compose-expr "calccomp" (a prec))
(declare-function math-abs "calc-arith" (a))
(declare-function math-format-bignum-binary "calc-bin" (a))
(declare-function math-format-bignum-octal "calc-bin" (a))
(declare-function math-format-bignum-hex "calc-bin" (a))
(declare-function math-format-bignum-radix "calc-bin" (a))
(declare-function math-compute-max-digits "calc-bin" (w r))
(declare-function math-map-vec "calc-vec" (f a))
(declare-function math-make-frac "calc-frac" (num den))


(defvar math-simplifying nil)
(defvar math-living-dangerously nil)   ; true if unsafe simplifications are okay.
(defvar math-integrating nil)

(defvar math-rewrite-selections nil)

(defvar math-compose-level 0)
(defvar math-comp-selected nil)
(defvar math-comp-tagged nil)
(defvar math-comp-sel-hpos nil)
(defvar math-comp-sel-vpos nil)
(defvar math-comp-sel-cpos nil)
(defvar math-compose-hash-args nil)

(defvar calc-alg-map)
(defvar calc-alg-esc-map)

;;; The following was made a function so that it could be byte-compiled.
(defun calc-init-extensions ()

  (define-key calc-mode-map ":" 'calc-fdiv)
  (define-key calc-mode-map "\\" 'calc-idiv)
  (define-key calc-mode-map "|" 'calc-concat)
  (define-key calc-mode-map "!" 'calc-factorial)
  (define-key calc-mode-map "C" 'calc-cos)
  (define-key calc-mode-map "E" 'calc-exp)
  (define-key calc-mode-map "H" 'calc-hyperbolic)
  (define-key calc-mode-map "I" 'calc-inverse)
  (define-key calc-mode-map "J" 'calc-conj)
  (define-key calc-mode-map "L" 'calc-ln)
  (define-key calc-mode-map "N" 'calc-eval-num)
  (define-key calc-mode-map "O" 'calc-option)
  (define-key calc-mode-map "P" 'calc-pi)
  (define-key calc-mode-map "Q" 'calc-sqrt)
  (define-key calc-mode-map "R" 'calc-round)
  (define-key calc-mode-map "S" 'calc-sin)
  (define-key calc-mode-map "T" 'calc-tan)
  (define-key calc-mode-map "U" 'calc-undo)
  (define-key calc-mode-map "X" 'calc-call-last-kbd-macro)
  (define-key calc-mode-map "o" 'calc-realign)
  (define-key calc-mode-map "p" 'calc-precision)
  (define-key calc-mode-map "w" 'calc-why)
  (define-key calc-mode-map "x" 'calc-execute-extended-command)
  (define-key calc-mode-map "y" 'calc-copy-to-buffer)

  (define-key calc-mode-map "(" 'calc-begin-complex)
  (define-key calc-mode-map ")" 'calc-end-complex)
  (define-key calc-mode-map "[" 'calc-begin-vector)
  (define-key calc-mode-map "]" 'calc-end-vector)
  (define-key calc-mode-map "," 'calc-comma)
  (define-key calc-mode-map ";" 'calc-semi)
  (define-key calc-mode-map "`" 'calc-edit)
  (define-key calc-mode-map "=" 'calc-evaluate)
  (define-key calc-mode-map "~" 'calc-num-prefix)
  (define-key calc-mode-map "<" 'calc-scroll-left)
  (define-key calc-mode-map ">" 'calc-scroll-right)
  (define-key calc-mode-map "{" 'calc-scroll-down)
  (define-key calc-mode-map "}" 'calc-scroll-up)
  (define-key calc-mode-map "\C-k" 'calc-kill)
  (define-key calc-mode-map "\M-k" 'calc-copy-as-kill)
  (define-key calc-mode-map "\C-w" 'calc-kill-region)
  (define-key calc-mode-map "\M-w" 'calc-copy-region-as-kill)
  (define-key calc-mode-map "\M-\C-w" 'kill-ring-save)
  (define-key calc-mode-map "\M-\C-m" 'calc-last-args)

  (define-key calc-mode-map "a" nil)
  (define-key calc-mode-map "a?" 'calc-a-prefix-help)
  (define-key calc-mode-map "aa" 'calc-apart)
  (define-key calc-mode-map "ab" 'calc-substitute)
  (define-key calc-mode-map "ac" 'calc-collect)
  (define-key calc-mode-map "ad" 'calc-derivative)
  (define-key calc-mode-map "ae" 'calc-simplify-extended)
  (define-key calc-mode-map "af" 'calc-factor)
  (define-key calc-mode-map "ag" 'calc-poly-gcd)
  (define-key calc-mode-map "ai" 'calc-integral)
  (define-key calc-mode-map "am" 'calc-match)
  (define-key calc-mode-map "an" 'calc-normalize-rat)
  (define-key calc-mode-map "ap" 'calc-poly-interp)
  (define-key calc-mode-map "ar" 'calc-rewrite)
  (define-key calc-mode-map "as" 'calc-simplify)
  (define-key calc-mode-map "at" 'calc-taylor)
  (define-key calc-mode-map "av" 'calc-alg-evaluate)
  (define-key calc-mode-map "ax" 'calc-expand)
  (define-key calc-mode-map "aA" 'calc-abs)
  (define-key calc-mode-map "aF" 'calc-curve-fit)
  (define-key calc-mode-map "aI" 'calc-num-integral)
  (define-key calc-mode-map "aM" 'calc-map-equation)
  (define-key calc-mode-map "aN" 'calc-find-minimum)
  (define-key calc-mode-map "aP" 'calc-poly-roots)
  (define-key calc-mode-map "aS" 'calc-solve-for)
  (define-key calc-mode-map "aR" 'calc-find-root)
  (define-key calc-mode-map "aT" 'calc-tabulate)
  (define-key calc-mode-map "aX" 'calc-find-maximum)
  (define-key calc-mode-map "a+" 'calc-summation)
  (define-key calc-mode-map "a-" 'calc-alt-summation)
  (define-key calc-mode-map "a*" 'calc-product)
  (define-key calc-mode-map "a\\" 'calc-poly-div)
  (define-key calc-mode-map "a%" 'calc-poly-rem)
  (define-key calc-mode-map "a/" 'calc-poly-div-rem)
  (define-key calc-mode-map "a=" 'calc-equal-to)
  (define-key calc-mode-map "a#" 'calc-not-equal-to)
  (define-key calc-mode-map "a<" 'calc-less-than)
  (define-key calc-mode-map "a>" 'calc-greater-than)
  (define-key calc-mode-map "a[" 'calc-less-equal)
  (define-key calc-mode-map "a]" 'calc-greater-equal)
  (define-key calc-mode-map "a." 'calc-remove-equal)
  (define-key calc-mode-map "a{" 'calc-in-set)
  (define-key calc-mode-map "a&" 'calc-logical-and)
  (define-key calc-mode-map "a|" 'calc-logical-or)
  (define-key calc-mode-map "a!" 'calc-logical-not)
  (define-key calc-mode-map "a:" 'calc-logical-if)
  (define-key calc-mode-map "a_" 'calc-subscript)
  (define-key calc-mode-map "a\"" 'calc-expand-formula)

  (define-key calc-mode-map "b" nil)
  (define-key calc-mode-map "b?" 'calc-b-prefix-help)
  (define-key calc-mode-map "ba" 'calc-and)
  (define-key calc-mode-map "bc" 'calc-clip)
  (define-key calc-mode-map "bd" 'calc-diff)
  (define-key calc-mode-map "bl" 'calc-lshift-binary)
  (define-key calc-mode-map "bn" 'calc-not)
  (define-key calc-mode-map "bo" 'calc-or)
  (define-key calc-mode-map "bp" 'calc-pack-bits)
  (define-key calc-mode-map "br" 'calc-rshift-binary)
  (define-key calc-mode-map "bt" 'calc-rotate-binary)
  (define-key calc-mode-map "bu" 'calc-unpack-bits)
  (define-key calc-mode-map "bw" 'calc-word-size)
  (define-key calc-mode-map "bx" 'calc-xor)
  (define-key calc-mode-map "bB" 'calc-log)
  (define-key calc-mode-map "bD" 'calc-fin-ddb)
  (define-key calc-mode-map "bF" 'calc-fin-fv)
  (define-key calc-mode-map "bI" 'calc-fin-irr)
  (define-key calc-mode-map "bL" 'calc-lshift-arith)
  (define-key calc-mode-map "bM" 'calc-fin-pmt)
  (define-key calc-mode-map "bN" 'calc-fin-npv)
  (define-key calc-mode-map "bP" 'calc-fin-pv)
  (define-key calc-mode-map "bR" 'calc-rshift-arith)
  (define-key calc-mode-map "bS" 'calc-fin-sln)
  (define-key calc-mode-map "bT" 'calc-fin-rate)
  (define-key calc-mode-map "bY" 'calc-fin-syd)
  (define-key calc-mode-map "b#" 'calc-fin-nper)
  (define-key calc-mode-map "b%" 'calc-percent-change)

  (define-key calc-mode-map "c" nil)
  (define-key calc-mode-map "c?" 'calc-c-prefix-help)
  (define-key calc-mode-map "cc" 'calc-clean)
  (define-key calc-mode-map "cd" 'calc-to-degrees)
  (define-key calc-mode-map "cf" 'calc-float)
  (define-key calc-mode-map "ch" 'calc-to-hms)
  (define-key calc-mode-map "cp" 'calc-polar)
  (define-key calc-mode-map "cr" 'calc-to-radians)
  (define-key calc-mode-map "cC" 'calc-cos)
  (define-key calc-mode-map "cF" 'calc-fraction)
  (define-key calc-mode-map "c%" 'calc-convert-percent)

  (define-key calc-mode-map "d" nil)
  (define-key calc-mode-map "d?" 'calc-d-prefix-help)
  (define-key calc-mode-map "d0" 'calc-decimal-radix)
  (define-key calc-mode-map "d2" 'calc-binary-radix)
  (define-key calc-mode-map "d6" 'calc-hex-radix)
  (define-key calc-mode-map "d8" 'calc-octal-radix)
  (define-key calc-mode-map "db" 'calc-line-breaking)
  (define-key calc-mode-map "dc" 'calc-complex-notation)
  (define-key calc-mode-map "dd" 'calc-date-notation)
  (define-key calc-mode-map "de" 'calc-eng-notation)
  (define-key calc-mode-map "df" 'calc-fix-notation)
  (define-key calc-mode-map "dg" 'calc-group-digits)
  (define-key calc-mode-map "dh" 'calc-hms-notation)
  (define-key calc-mode-map "di" 'calc-i-notation)
  (define-key calc-mode-map "dj" 'calc-j-notation)
  (define-key calc-mode-map "dl" 'calc-line-numbering)
  (define-key calc-mode-map "dn" 'calc-normal-notation)
  (define-key calc-mode-map "do" 'calc-over-notation)
  (define-key calc-mode-map "dp" 'calc-show-plain)
  (define-key calc-mode-map "dr" 'calc-radix)
  (define-key calc-mode-map "ds" 'calc-sci-notation)
  (define-key calc-mode-map "dt" 'calc-truncate-stack)
  (define-key calc-mode-map "dw" 'calc-auto-why)
  (define-key calc-mode-map "dz" 'calc-leading-zeros)
  (define-key calc-mode-map "dA" 'calc-giac-language)
  (define-key calc-mode-map "dB" 'calc-big-language)
  (define-key calc-mode-map "dD" 'calc-redo)
  (define-key calc-mode-map "dC" 'calc-c-language)
  (define-key calc-mode-map "dE" 'calc-eqn-language)
  (define-key calc-mode-map "dF" 'calc-fortran-language)
  (define-key calc-mode-map "dM" 'calc-mathematica-language)
  (define-key calc-mode-map "dN" 'calc-normal-language)
  (define-key calc-mode-map "dO" 'calc-flat-language)
  (define-key calc-mode-map "dP" 'calc-pascal-language)
  (define-key calc-mode-map "dT" 'calc-tex-language)
  (define-key calc-mode-map "dL" 'calc-latex-language)
  (define-key calc-mode-map "dU" 'calc-unformatted-language)
  (define-key calc-mode-map "dW" 'calc-maple-language)
  (define-key calc-mode-map "dX" 'calc-maxima-language)
  (define-key calc-mode-map "dY" 'calc-yacas-language)
  (define-key calc-mode-map "d[" 'calc-truncate-up)
  (define-key calc-mode-map "d]" 'calc-truncate-down)
  (define-key calc-mode-map "d." 'calc-point-char)
  (define-key calc-mode-map "d," 'calc-group-char)
  (define-key calc-mode-map "d\"" 'calc-display-strings)
  (define-key calc-mode-map "d<" 'calc-left-justify)
  (define-key calc-mode-map "d=" 'calc-center-justify)
  (define-key calc-mode-map "d>" 'calc-right-justify)
  (define-key calc-mode-map "d{" 'calc-left-label)
  (define-key calc-mode-map "d}" 'calc-right-label)
  (define-key calc-mode-map "d'" 'calc-display-raw)
  (define-key calc-mode-map "d " 'calc-refresh)
  (define-key calc-mode-map "d\r" 'calc-refresh-top)
  (define-key calc-mode-map "d@" 'calc-toggle-banner)

  (define-key calc-mode-map "f" nil)
  (define-key calc-mode-map "f?" 'calc-f-prefix-help)
  (define-key calc-mode-map "fb" 'calc-beta)
  (define-key calc-mode-map "fe" 'calc-erf)
  (define-key calc-mode-map "fg" 'calc-gamma)
  (define-key calc-mode-map "fh" 'calc-hypot)
  (define-key calc-mode-map "fi" 'calc-im)
  (define-key calc-mode-map "fj" 'calc-bessel-J)
  (define-key calc-mode-map "fn" 'calc-min)
  (define-key calc-mode-map "fr" 'calc-re)
  (define-key calc-mode-map "fs" 'calc-sign)
  (define-key calc-mode-map "fx" 'calc-max)
  (define-key calc-mode-map "fy" 'calc-bessel-Y)
  (define-key calc-mode-map "fA" 'calc-abssqr)
  (define-key calc-mode-map "fB" 'calc-inc-beta)
  (define-key calc-mode-map "fE" 'calc-expm1)
  (define-key calc-mode-map "fF" 'calc-floor)
  (define-key calc-mode-map "fG" 'calc-inc-gamma)
  (define-key calc-mode-map "fI" 'calc-ilog)
  (define-key calc-mode-map "fL" 'calc-lnp1)
  (define-key calc-mode-map "fM" 'calc-mant-part)
  (define-key calc-mode-map "fQ" 'calc-isqrt)
  (define-key calc-mode-map "fS" 'calc-scale-float)
  (define-key calc-mode-map "fT" 'calc-arctan2)
  (define-key calc-mode-map "fX" 'calc-xpon-part)
  (define-key calc-mode-map "f[" 'calc-decrement)
  (define-key calc-mode-map "f]" 'calc-increment)

  (define-key calc-mode-map "g" nil)
  (define-key calc-mode-map "g?" 'calc-g-prefix-help)
  (define-key calc-mode-map "ga" 'calc-graph-add)
  (define-key calc-mode-map "gb" 'calc-graph-border)
  (define-key calc-mode-map "gc" 'calc-graph-clear)
  (define-key calc-mode-map "gd" 'calc-graph-delete)
  (define-key calc-mode-map "gf" 'calc-graph-fast)
  (define-key calc-mode-map "gg" 'calc-graph-grid)
  (define-key calc-mode-map "gh" 'calc-graph-header)
  (define-key calc-mode-map "gk" 'calc-graph-key)
  (define-key calc-mode-map "gj" 'calc-graph-juggle)
  (define-key calc-mode-map "gl" 'calc-graph-log-x)
  (define-key calc-mode-map "gn" 'calc-graph-name)
  (define-key calc-mode-map "gp" 'calc-graph-plot)
  (define-key calc-mode-map "gq" 'calc-graph-quit)
  (define-key calc-mode-map "gr" 'calc-graph-range-x)
  (define-key calc-mode-map "gs" 'calc-graph-line-style)
  (define-key calc-mode-map "gt" 'calc-graph-title-x)
  (define-key calc-mode-map "gv" 'calc-graph-view-commands)
  (define-key calc-mode-map "gx" 'calc-graph-display)
  (define-key calc-mode-map "gz" 'calc-graph-zero-x)
  (define-key calc-mode-map "gA" 'calc-graph-add-3d)
  (define-key calc-mode-map "gC" 'calc-graph-command)
  (define-key calc-mode-map "gD" 'calc-graph-device)
  (define-key calc-mode-map "gF" 'calc-graph-fast-3d)
  (define-key calc-mode-map "gG" 'calc-argument)
  (define-key calc-mode-map "gH" 'calc-graph-hide)
  (define-key calc-mode-map "gK" 'calc-graph-kill)
  (define-key calc-mode-map "gL" 'calc-graph-log-y)
  (define-key calc-mode-map "gN" 'calc-graph-num-points)
  (define-key calc-mode-map "gO" 'calc-graph-output)
  (define-key calc-mode-map "gP" 'calc-graph-print)
  (define-key calc-mode-map "gR" 'calc-graph-range-y)
  (define-key calc-mode-map "gS" 'calc-graph-point-style)
  (define-key calc-mode-map "gT" 'calc-graph-title-y)
  (define-key calc-mode-map "gV" 'calc-graph-view-trail)
  (define-key calc-mode-map "gX" 'calc-graph-geometry)
  (define-key calc-mode-map "gZ" 'calc-graph-zero-y)
  (define-key calc-mode-map "g\C-l" 'calc-graph-log-z)
  (define-key calc-mode-map "g\C-r" 'calc-graph-range-z)
  (define-key calc-mode-map "g\C-t" 'calc-graph-title-z)

  (define-key calc-mode-map "h" 'calc-help-prefix)

  (define-key calc-mode-map "j" nil)
  (define-key calc-mode-map "j?" 'calc-j-prefix-help)
  (define-key calc-mode-map "ja" 'calc-select-additional)
  (define-key calc-mode-map "jb" 'calc-break-selections)
  (define-key calc-mode-map "jc" 'calc-clear-selections)
  (define-key calc-mode-map "jd" 'calc-show-selections)
  (define-key calc-mode-map "je" 'calc-enable-selections)
  (define-key calc-mode-map "jl" 'calc-select-less)
  (define-key calc-mode-map "jm" 'calc-select-more)
  (define-key calc-mode-map "jn" 'calc-select-next)
  (define-key calc-mode-map "jo" 'calc-select-once)
  (define-key calc-mode-map "jp" 'calc-select-previous)
  (define-key calc-mode-map "jr" 'calc-rewrite-selection)
  (define-key calc-mode-map "js" 'calc-select-here)
  (define-key calc-mode-map "jv" 'calc-sel-evaluate)
  (define-key calc-mode-map "ju" 'calc-unselect)
  (define-key calc-mode-map "jC" 'calc-sel-commute)
  (define-key calc-mode-map "jD" 'calc-sel-distribute)
  (define-key calc-mode-map "jE" 'calc-sel-jump-equals)
  (define-key calc-mode-map "jI" 'calc-sel-isolate)
  (define-key calc-mode-map "jJ" 'calc-conj)
  (define-key calc-mode-map "jL" 'calc-commute-left)
  (define-key calc-mode-map "jM" 'calc-sel-merge)
  (define-key calc-mode-map "jN" 'calc-sel-negate)
  (define-key calc-mode-map "jO" 'calc-select-once-maybe)
  (define-key calc-mode-map "jR" 'calc-commute-right)
  (define-key calc-mode-map "jS" 'calc-select-here-maybe)
  (define-key calc-mode-map "jU" 'calc-sel-unpack)
  (define-key calc-mode-map "j&" 'calc-sel-invert)
  (define-key calc-mode-map "j\r" 'calc-copy-selection)
  (define-key calc-mode-map "j\n" 'calc-copy-selection)
  (define-key calc-mode-map "j\010" 'calc-del-selection)
  (define-key calc-mode-map "j\177" 'calc-del-selection)
  (define-key calc-mode-map "j'" 'calc-enter-selection)
  (define-key calc-mode-map "j`" 'calc-edit-selection)
  (define-key calc-mode-map "j+" 'calc-sel-add-both-sides)
  (define-key calc-mode-map "j-" 'calc-sel-sub-both-sides)
  (define-key calc-mode-map "j*" 'calc-sel-mult-both-sides)
  (define-key calc-mode-map "j/" 'calc-sel-div-both-sides)
  (define-key calc-mode-map "j\"" 'calc-sel-expand-formula)

  (define-key calc-mode-map "k" nil)
  (define-key calc-mode-map "k?" 'calc-k-prefix-help)
  (define-key calc-mode-map "ka" 'calc-random-again)
  (define-key calc-mode-map "kb" 'calc-bernoulli-number)
  (define-key calc-mode-map "kc" 'calc-choose)
  (define-key calc-mode-map "kd" 'calc-double-factorial)
  (define-key calc-mode-map "ke" 'calc-euler-number)
  (define-key calc-mode-map "kf" 'calc-prime-factors)
  (define-key calc-mode-map "kg" 'calc-gcd)
  (define-key calc-mode-map "kh" 'calc-shuffle)
  (define-key calc-mode-map "kl" 'calc-lcm)
  (define-key calc-mode-map "km" 'calc-moebius)
  (define-key calc-mode-map "kn" 'calc-next-prime)
  (define-key calc-mode-map "kp" 'calc-prime-test)
  (define-key calc-mode-map "kr" 'calc-random)
  (define-key calc-mode-map "ks" 'calc-stirling-number)
  (define-key calc-mode-map "kt" 'calc-totient)
  (define-key calc-mode-map "kB" 'calc-utpb)
  (define-key calc-mode-map "kC" 'calc-utpc)
  (define-key calc-mode-map "kE" 'calc-extended-gcd)
  (define-key calc-mode-map "kF" 'calc-utpf)
  (define-key calc-mode-map "kK" 'calc-keep-args)
  (define-key calc-mode-map "kN" 'calc-utpn)
  (define-key calc-mode-map "kP" 'calc-utpp)
  (define-key calc-mode-map "kT" 'calc-utpt)

  (define-key calc-mode-map "l" nil)
  (define-key calc-mode-map "lq" 'calc-lu-quant)
  (define-key calc-mode-map "ld" 'calc-db)
  (define-key calc-mode-map "ln" 'calc-np)
  (define-key calc-mode-map "l+" 'calc-lu-plus)
  (define-key calc-mode-map "l-" 'calc-lu-minus)
  (define-key calc-mode-map "l*" 'calc-lu-times)
  (define-key calc-mode-map "l/" 'calc-lu-divide)
  (define-key calc-mode-map "ls" 'calc-spn)
  (define-key calc-mode-map "lm" 'calc-midi)
  (define-key calc-mode-map "lf" 'calc-freq)

  (define-key calc-mode-map "l?" 'calc-l-prefix-help)

  (define-key calc-mode-map "m" nil)
  (define-key calc-mode-map "m?" 'calc-m-prefix-help)
  (define-key calc-mode-map "ma" 'calc-algebraic-mode)
  (define-key calc-mode-map "md" 'calc-degrees-mode)
  (define-key calc-mode-map "me" 'calc-embedded-preserve-modes)
  (define-key calc-mode-map "mf" 'calc-frac-mode)
  (define-key calc-mode-map "mg" 'calc-get-modes)
  (define-key calc-mode-map "mh" 'calc-hms-mode)
  (define-key calc-mode-map "mi" 'calc-infinite-mode)
  (define-key calc-mode-map "mm" 'calc-save-modes)
  (define-key calc-mode-map "mp" 'calc-polar-mode)
  (define-key calc-mode-map "mr" 'calc-radians-mode)
  (define-key calc-mode-map "ms" 'calc-symbolic-mode)
  (define-key calc-mode-map "mt" 'calc-total-algebraic-mode)
  (define-key calc-mode-map "\emt" 'calc-total-algebraic-mode)
  (define-key calc-mode-map "\em\et" 'calc-total-algebraic-mode)
  (define-key calc-mode-map "mv" 'calc-matrix-mode)
  (define-key calc-mode-map "mw" 'calc-working)
  (define-key calc-mode-map "mx" 'calc-always-load-extensions)
  (define-key calc-mode-map "mA" 'calc-alg-simplify-mode)
  (define-key calc-mode-map "mB" 'calc-bin-simplify-mode)
  (define-key calc-mode-map "mC" 'calc-auto-recompute)
  (define-key calc-mode-map "mD" 'calc-default-simplify-mode)
  (define-key calc-mode-map "mE" 'calc-ext-simplify-mode)
  (define-key calc-mode-map "mF" 'calc-settings-file-name)
  (define-key calc-mode-map "mM" 'calc-more-recursion-depth)
  (define-key calc-mode-map "mN" 'calc-num-simplify-mode)
  (define-key calc-mode-map "mO" 'calc-no-simplify-mode)
  (define-key calc-mode-map "mR" 'calc-mode-record-mode)
  (define-key calc-mode-map "mS" 'calc-shift-prefix)
  (define-key calc-mode-map "mU" 'calc-units-simplify-mode)
  (define-key calc-mode-map "mX" 'calc-load-everything)

  (define-key calc-mode-map "r" nil)
  (define-key calc-mode-map "ri" 'calc-insert-register)
  (define-key calc-mode-map "rs" 'calc-copy-to-register)
  (define-key calc-mode-map "r?" 'calc-r-prefix-help)

  (define-key calc-mode-map "s" nil)
  (define-key calc-mode-map "s?" 'calc-s-prefix-help)
  (define-key calc-mode-map "sc" 'calc-copy-variable)
  (define-key calc-mode-map "sd" 'calc-declare-variable)
  (define-key calc-mode-map "se" 'calc-edit-variable)
  (define-key calc-mode-map "si" 'calc-insert-variables)
  (define-key calc-mode-map "sk" 'calc-copy-special-constant)
  (define-key calc-mode-map "sl" 'calc-let)
  (define-key calc-mode-map "sm" 'calc-store-map)
  (define-key calc-mode-map "sn" 'calc-store-neg)
  (define-key calc-mode-map "sp" 'calc-permanent-variable)
  (define-key calc-mode-map "sr" 'calc-recall)
  (define-key calc-mode-map "ss" 'calc-store)
  (define-key calc-mode-map "st" 'calc-store-into)
  (define-key calc-mode-map "su" 'calc-unstore)
  (define-key calc-mode-map "sx" 'calc-store-exchange)
  (define-key calc-mode-map "sA" 'calc-edit-AlgSimpRules)
  (define-key calc-mode-map "sD" 'calc-edit-Decls)
  (define-key calc-mode-map "sE" 'calc-edit-EvalRules)
  (define-key calc-mode-map "sF" 'calc-edit-FitRules)
  (define-key calc-mode-map "sG" 'calc-edit-GenCount)
  (define-key calc-mode-map "sH" 'calc-edit-Holidays)
  (define-key calc-mode-map "sI" 'calc-edit-IntegLimit)
  (define-key calc-mode-map "sL" 'calc-edit-LineStyles)
  (define-key calc-mode-map "sP" 'calc-edit-PointStyles)
  (define-key calc-mode-map "sR" 'calc-edit-PlotRejects)
  (define-key calc-mode-map "sS" 'calc-sin)
  (define-key calc-mode-map "sT" 'calc-edit-TimeZone)
  (define-key calc-mode-map "sU" 'calc-edit-Units)
  (define-key calc-mode-map "sX" 'calc-edit-ExtSimpRules)
  (define-key calc-mode-map "s+" 'calc-store-plus)
  (define-key calc-mode-map "s-" 'calc-store-minus)
  (define-key calc-mode-map "s*" 'calc-store-times)
  (define-key calc-mode-map "s/" 'calc-store-div)
  (define-key calc-mode-map "s^" 'calc-store-power)
  (define-key calc-mode-map "s|" 'calc-store-concat)
  (define-key calc-mode-map "s&" 'calc-store-inv)
  (define-key calc-mode-map "s[" 'calc-store-decr)
  (define-key calc-mode-map "s]" 'calc-store-incr)
  (define-key calc-mode-map "s:" 'calc-assign)
  (define-key calc-mode-map "s=" 'calc-evalto)

  (define-key calc-mode-map "t" nil)
  (define-key calc-mode-map "t?" 'calc-t-prefix-help)
  (define-key calc-mode-map "tb" 'calc-trail-backward)
  (define-key calc-mode-map "td" 'calc-trail-display)
  (define-key calc-mode-map "tf" 'calc-trail-forward)
  (define-key calc-mode-map "th" 'calc-trail-here)
  (define-key calc-mode-map "ti" 'calc-trail-in)
  (define-key calc-mode-map "tk" 'calc-trail-kill)
  (define-key calc-mode-map "tm" 'calc-trail-marker)
  (define-key calc-mode-map "tn" 'calc-trail-next)
  (define-key calc-mode-map "to" 'calc-trail-out)
  (define-key calc-mode-map "tp" 'calc-trail-previous)
  (define-key calc-mode-map "tr" 'calc-trail-isearch-backward)
  (define-key calc-mode-map "ts" 'calc-trail-isearch-forward)
  (define-key calc-mode-map "ty" 'calc-trail-yank)
  (define-key calc-mode-map "t[" 'calc-trail-first)
  (define-key calc-mode-map "t]" 'calc-trail-last)
  (define-key calc-mode-map "t<" 'calc-trail-scroll-left)
  (define-key calc-mode-map "t>" 'calc-trail-scroll-right)
  (define-key calc-mode-map "t{" 'calc-trail-backward)
  (define-key calc-mode-map "t}" 'calc-trail-forward)
  (define-key calc-mode-map "t." 'calc-full-trail-vectors)
  (define-key calc-mode-map "tC" 'calc-convert-time-zones)
  (define-key calc-mode-map "tD" 'calc-date)
  (define-key calc-mode-map "tI" 'calc-inc-month)
  (define-key calc-mode-map "tJ" 'calc-julian)
  (define-key calc-mode-map "tM" 'calc-new-month)
  (define-key calc-mode-map "tN" 'calc-now)
  (define-key calc-mode-map "tP" 'calc-date-part)
  (define-key calc-mode-map "tT" 'calc-tan)
  (define-key calc-mode-map "tU" 'calc-unix-time)
  (define-key calc-mode-map "tW" 'calc-new-week)
  (define-key calc-mode-map "tY" 'calc-new-year)
  (define-key calc-mode-map "tZ" 'calc-time-zone)
  (define-key calc-mode-map "t+" 'calc-business-days-plus)
  (define-key calc-mode-map "t-" 'calc-business-days-minus)

  (define-key calc-mode-map "u" 'nil)
  (define-key calc-mode-map "u?" 'calc-u-prefix-help)
  (define-key calc-mode-map "ua" 'calc-autorange-units)
  (define-key calc-mode-map "ub" 'calc-base-units)
  (define-key calc-mode-map "uc" 'calc-convert-units)
  (define-key calc-mode-map "ud" 'calc-define-unit)
  (define-key calc-mode-map "ue" 'calc-explain-units)
  (define-key calc-mode-map "ug" 'calc-get-unit-definition)
  (define-key calc-mode-map "up" 'calc-permanent-units)
  (define-key calc-mode-map "ur" 'calc-remove-units)
  (define-key calc-mode-map "us" 'calc-simplify-units)
  (define-key calc-mode-map "ut" 'calc-convert-temperature)
  (define-key calc-mode-map "uu" 'calc-undefine-unit)
  (define-key calc-mode-map "uv" 'calc-enter-units-table)
  (define-key calc-mode-map "ux" 'calc-extract-units)
  (define-key calc-mode-map "uV" 'calc-view-units-table)
  (define-key calc-mode-map "uC" 'calc-vector-covariance)
  (define-key calc-mode-map "uG" 'calc-vector-geometric-mean)
  (define-key calc-mode-map "uM" 'calc-vector-mean)
  (define-key calc-mode-map "uN" 'calc-vector-min)
  (define-key calc-mode-map "uS" 'calc-vector-sdev)
  (define-key calc-mode-map "uU" 'calc-undo)
  (define-key calc-mode-map "uX" 'calc-vector-max)
  (define-key calc-mode-map "u#" 'calc-vector-count)
  (define-key calc-mode-map "u+" 'calc-vector-sum)
  (define-key calc-mode-map "u*" 'calc-vector-product)

  (define-key calc-mode-map "v" 'nil)
  (define-key calc-mode-map "v?" 'calc-v-prefix-help)
  (define-key calc-mode-map "va" 'calc-arrange-vector)
  (define-key calc-mode-map "vb" 'calc-build-vector)
  (define-key calc-mode-map "vc" 'calc-mcol)
  (define-key calc-mode-map "vd" 'calc-diag)
  (define-key calc-mode-map "ve" 'calc-expand-vector)
  (define-key calc-mode-map "vf" 'calc-vector-find)
  (define-key calc-mode-map "vh" 'calc-head)
  (define-key calc-mode-map "vi" 'calc-ident)
  (define-key calc-mode-map "vk" 'calc-cons)
  (define-key calc-mode-map "vl" 'calc-vlength)
  (define-key calc-mode-map "vm" 'calc-mask-vector)
  (define-key calc-mode-map "vn" 'calc-rnorm)
  (define-key calc-mode-map "vp" 'calc-pack)
  (define-key calc-mode-map "vr" 'calc-mrow)
  (define-key calc-mode-map "vs" 'calc-subvector)
  (define-key calc-mode-map "vt" 'calc-transpose)
  (define-key calc-mode-map "vu" 'calc-unpack)
  (define-key calc-mode-map "vv" 'calc-reverse-vector)
  (define-key calc-mode-map "vx" 'calc-index)
  (define-key calc-mode-map "vA" 'calc-apply)
  (define-key calc-mode-map "vC" 'calc-cross)
  (define-key calc-mode-map "vK" 'calc-kron)
  (define-key calc-mode-map "vD" 'calc-mdet)
  (define-key calc-mode-map "vE" 'calc-set-enumerate)
  (define-key calc-mode-map "vF" 'calc-set-floor)
  (define-key calc-mode-map "vG" 'calc-grade)
  (define-key calc-mode-map "vH" 'calc-histogram)
  (define-key calc-mode-map "vI" 'calc-inner-product)
  (define-key calc-mode-map "vJ" 'calc-conj-transpose)
  (define-key calc-mode-map "vL" 'calc-mlud)
  (define-key calc-mode-map "vM" 'calc-map)
  (define-key calc-mode-map "vN" 'calc-cnorm)
  (define-key calc-mode-map "vO" 'calc-outer-product)
  (define-key calc-mode-map "vR" 'calc-reduce)
  (define-key calc-mode-map "vS" 'calc-sort)
  (define-key calc-mode-map "vT" 'calc-mtrace)
  (define-key calc-mode-map "vU" 'calc-accumulate)
  (define-key calc-mode-map "vV" 'calc-set-union)
  (define-key calc-mode-map "vX" 'calc-set-xor)
  (define-key calc-mode-map "v^" 'calc-set-intersect)
  (define-key calc-mode-map "v-" 'calc-set-difference)
  (define-key calc-mode-map "v~" 'calc-set-complement)
  (define-key calc-mode-map "v:" 'calc-set-span)
  (define-key calc-mode-map "v#" 'calc-set-cardinality)
  (define-key calc-mode-map "v+" 'calc-remove-duplicates)
  (define-key calc-mode-map "v&" 'calc-inv)
  (define-key calc-mode-map "v<" 'calc-matrix-left-justify)
  (define-key calc-mode-map "v=" 'calc-matrix-center-justify)
  (define-key calc-mode-map "v>" 'calc-matrix-right-justify)
  (define-key calc-mode-map "v." 'calc-full-vectors)
  (define-key calc-mode-map "v/" 'calc-break-vectors)
  (define-key calc-mode-map "v," 'calc-vector-commas)
  (define-key calc-mode-map "v[" 'calc-vector-brackets)
  (define-key calc-mode-map "v]" 'calc-matrix-brackets)
  (define-key calc-mode-map "v{" 'calc-vector-braces)
  (define-key calc-mode-map "v}" 'calc-matrix-brackets)
  (define-key calc-mode-map "v(" 'calc-vector-parens)
  (define-key calc-mode-map "v)" 'calc-matrix-brackets)
  ;; We can't rely on the automatic upper->lower conversion because
  ;; in the global map V is explicitly bound, so we need to bind it
  ;; explicitly as well :-(  --stef
  (define-key calc-mode-map "V" (lookup-key calc-mode-map "v"))

  (define-key calc-mode-map "z" 'nil)
  (define-key calc-mode-map "z?" 'calc-z-prefix-help)

  (define-key calc-mode-map "Z" 'nil)
  (define-key calc-mode-map "Z?" 'calc-shift-Z-prefix-help)
  (define-key calc-mode-map "ZC" 'calc-user-define-composition)
  (define-key calc-mode-map "ZD" 'calc-user-define)
  (define-key calc-mode-map "ZE" 'calc-user-define-edit)
  (define-key calc-mode-map "ZF" 'calc-user-define-formula)
  (define-key calc-mode-map "ZG" 'calc-get-user-defn)
  (define-key calc-mode-map "ZI" 'calc-user-define-invocation)
  (define-key calc-mode-map "ZK" 'calc-user-define-kbd-macro)
  (define-key calc-mode-map "ZP" 'calc-user-define-permanent)
  (define-key calc-mode-map "ZS" 'calc-edit-user-syntax)
  (define-key calc-mode-map "ZT" 'calc-timing)
  (define-key calc-mode-map "ZU" 'calc-user-undefine)
  (define-key calc-mode-map "Z[" 'calc-kbd-if)
  (define-key calc-mode-map "Z:" 'calc-kbd-else)
  (define-key calc-mode-map "Z|" 'calc-kbd-else-if)
  (define-key calc-mode-map "Z]" 'calc-kbd-end-if)
  (define-key calc-mode-map "Z<" 'calc-kbd-repeat)
  (define-key calc-mode-map "Z>" 'calc-kbd-end-repeat)
  (define-key calc-mode-map "Z(" 'calc-kbd-for)
  (define-key calc-mode-map "Z)" 'calc-kbd-end-for)
  (define-key calc-mode-map "Z{" 'calc-kbd-loop)
  (define-key calc-mode-map "Z}" 'calc-kbd-end-loop)
  (define-key calc-mode-map "Z/" 'calc-kbd-break)
  (define-key calc-mode-map "Z`" 'calc-kbd-push)
  (define-key calc-mode-map "Z'" 'calc-kbd-pop)
  (define-key calc-mode-map "Z=" 'calc-kbd-report)
  (define-key calc-mode-map "Z#" 'calc-kbd-query)

  (calc-init-prefixes)

  (mapc (function
	 (lambda (x)
	  (define-key calc-mode-map (format "c%c" x) 'calc-clean-num)
	  (define-key calc-mode-map (format "j%c" x) 'calc-select-part)
	  (define-key calc-mode-map (format "r%c" x) 'calc-recall-quick)
	  (define-key calc-mode-map (format "s%c" x) 'calc-store-quick)
	  (define-key calc-mode-map (format "t%c" x) 'calc-store-into-quick)
	  (define-key calc-mode-map (format "u%c" x) 'calc-quick-units)))
	"0123456789")

  (let ((i ?A))
    (while (<= i ?z)
      (if (eq (car-safe (aref (nth 1 calc-mode-map) i)) 'keymap)
	  (aset (nth 1 calc-mode-map) i
		(cons 'keymap (cons (cons ?\e (aref (nth 1 calc-mode-map) i))
				    (cdr (aref (nth 1 calc-mode-map) i))))))
      (setq i (1+ i))))

  (setq calc-alg-map (copy-keymap calc-mode-map)
	calc-alg-esc-map (copy-keymap esc-map))
  (let ((i 32))
    (while (< i 127)
      (or (memq i '(?' ?` ?= ??))
	  (aset (nth 1 calc-alg-map) i 'calc-auto-algebraic-entry))
      (or (memq i '(?# ?x ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
	  (aset (nth 1 calc-alg-esc-map) i (aref (nth 1 calc-mode-map) i)))
      (setq i (1+ i))))
  (define-key calc-alg-map "\e" calc-alg-esc-map)
  (define-key calc-alg-map "\e\t" 'calc-roll-up)
  (define-key calc-alg-map "\e\C-m" 'calc-last-args-stub)
  (define-key calc-alg-map "\e\177" 'calc-pop-above)

;;;; (Autoloads here)
  (mapc (function (lambda (x)
    (mapcar (function (lambda (func)
      (autoload func (car x)))) (cdr x))))
    '(

 ("calc-alg" calc-has-rules math-defsimplify
calc-modify-simplify-mode calcFunc-collect calcFunc-esimplify
calcFunc-islin calcFunc-islinnt calcFunc-lin calcFunc-linnt
calcFunc-simplify calcFunc-subst calcFunc-powerexpand math-beforep
math-build-polynomial-expr math-expand-formula math-expr-contains
math-expr-contains-count math-expr-depends math-expr-height
math-expr-subst math-expr-weight math-integer-plus math-is-linear
math-is-multiple math-is-polynomial math-linear-in math-multiple-of
math-poly-depends math-poly-mix math-poly-mul
math-poly-simplify math-poly-zerop math-polynomial-base
math-polynomial-p math-recompile-eval-rules math-simplify
math-simplify-exp math-simplify-extended math-simplify-sqrt
math-to-simple-fraction)

 ("calcalg2" calcFunc-asum calcFunc-deriv
calcFunc-ffinv calcFunc-finv calcFunc-fsolve calcFunc-gpoly
calcFunc-integ calcFunc-poly calcFunc-prod calcFunc-roots
calcFunc-solve calcFunc-sum calcFunc-table calcFunc-taylor
calcFunc-tderiv math-expr-calls math-integral-q02 math-integral-q12
math-integral-rational-funcs math-lcm-denoms math-looks-evenp
math-poly-all-roots math-prod-rec math-reject-solution math-solve-eqn
math-solve-for math-sum-rec math-try-integral)

 ("calcalg3" calcFunc-efit calcFunc-fit
calcFunc-fitdummy calcFunc-fitparam calcFunc-fitvar
calcFunc-hasfitparams calcFunc-hasfitvars calcFunc-maximize
calcFunc-minimize calcFunc-ninteg calcFunc-polint calcFunc-ratint
calcFunc-root calcFunc-wmaximize calcFunc-wminimize calcFunc-wroot
calcFunc-xfit math-find-minimum math-find-root math-ninteg-evaluate
math-ninteg-midpoint math-ninteg-romberg math-poly-interp)

 ("calc-arith" calcFunc-abs calcFunc-abssqr
calcFunc-add calcFunc-ceil calcFunc-decr calcFunc-deven calcFunc-dimag
calcFunc-dint calcFunc-div calcFunc-dnatnum calcFunc-dneg
calcFunc-dnonneg calcFunc-dnonzero calcFunc-dnumint calcFunc-dodd
calcFunc-dpos calcFunc-drange calcFunc-drat calcFunc-dreal
calcFunc-dscalar calcFunc-fceil calcFunc-ffloor calcFunc-float
calcFunc-fround calcFunc-frounde calcFunc-froundu calcFunc-ftrunc
calcFunc-idiv calcFunc-incr calcFunc-ldiv calcFunc-mant calcFunc-max calcFunc-min
calcFunc-mod calcFunc-mul calcFunc-neg calcFunc-percent calcFunc-pow
calcFunc-relch calcFunc-round calcFunc-rounde calcFunc-roundu
calcFunc-scf calcFunc-sub calcFunc-xpon math-abs math-abs-approx
math-add-objects-fancy math-add-or-sub math-add-symb-fancy
math-ceiling math-combine-prod math-combine-sum math-div-by-zero
math-div-objects-fancy math-div-symb-fancy math-div-zero
math-float-fancy math-floor-fancy math-floor-special math-guess-if-neg
math-intv-constp math-known-evenp math-known-imagp math-known-integerp
math-known-matrixp math-known-negp math-known-nonnegp
math-known-nonposp math-known-nonzerop math-known-num-integerp
math-known-oddp math-known-posp math-known-realp math-known-scalarp
math-max math-min math-mod-fancy math-mul-float math-mul-objects-fancy
math-mul-or-div math-mul-symb-fancy math-mul-zero math-neg-fancy
math-neg-float math-okay-neg math-possible-signs math-possible-types
math-pow-fancy math-pow-mod math-pow-of-zero math-pow-zero
math-quarter-integer math-round math-setup-declarations math-sqr
math-sqr-float math-trunc-fancy math-trunc-special)

 ("calc-bin" calcFunc-and calcFunc-ash
calcFunc-clip calcFunc-diff calcFunc-lsh calcFunc-not calcFunc-or
calcFunc-rash calcFunc-rot calcFunc-rsh calcFunc-xor math-clip
math-compute-max-digits math-convert-radix-digits math-float-parts
math-format-bignum-binary math-format-bignum-hex
math-format-bignum-octal math-format-bignum-radix math-format-binary
math-format-radix math-format-radix-float math-integer-log2
math-power-of-2 math-radix-float-power)

 ("calc-comb"  calc-report-prime-test
calcFunc-choose calcFunc-dfact calcFunc-egcd calcFunc-fact
calcFunc-gcd calcFunc-lcm calcFunc-moebius calcFunc-nextprime
calcFunc-perm calcFunc-prevprime calcFunc-prfac calcFunc-prime
calcFunc-random calcFunc-shuffle calcFunc-stir1 calcFunc-stir2
calcFunc-totient math-init-random-base math-member math-prime-test
math-random-base)

 ("calccomp" calcFunc-cascent calcFunc-cdescent
calcFunc-cheight calcFunc-cwidth math-comp-ascent math-comp-descent
math-comp-height math-comp-width math-compose-expr
math-composition-to-string math-stack-value-offset-fancy
math-vector-is-string math-vector-to-string)

 ("calc-cplx" calcFunc-arg calcFunc-conj
calcFunc-im calcFunc-polar calcFunc-re calcFunc-rect math-complex
math-fix-circular math-imaginary math-imaginary-i math-normalize-polar
math-polar math-want-polar)

 ("calc-embed" calc-do-embedded
calc-do-embedded-activate calc-embedded-evaluate-expr
calc-embedded-modes-change calc-embedded-var-change
calc-embedded-preserve-modes)

 ("calc-fin" calc-to-percentage calcFunc-ddb
calcFunc-fv calcFunc-fvb calcFunc-fvl calcFunc-irr calcFunc-irrb
calcFunc-nper calcFunc-nperb calcFunc-nperl calcFunc-npv calcFunc-npvb
calcFunc-pmt calcFunc-pmtb calcFunc-pv calcFunc-pvb calcFunc-pvl
calcFunc-rate calcFunc-rateb calcFunc-ratel calcFunc-sln calcFunc-syd)

 ("calc-forms" calcFunc-badd calcFunc-bsub
calcFunc-date calcFunc-day calcFunc-dsadj calcFunc-hms
calcFunc-holiday calcFunc-hour calcFunc-incmonth calcFunc-incyear
calcFunc-intv calcFunc-julian calcFunc-makemod calcFunc-minute
calcFunc-month calcFunc-newmonth calcFunc-newweek calcFunc-newyear
calcFunc-now calcFunc-pwday calcFunc-sdev calcFunc-second
calcFunc-time calcFunc-tzconv calcFunc-tzone calcFunc-unixtime
calcFunc-weekday calcFunc-year calcFunc-yearday math-combine-intervals
math-date-parts math-date-to-dt math-div-mod math-dt-to-date
math-format-date math-from-business-day math-from-hms math-make-intv
math-make-mod math-make-sdev math-mod-intv math-normalize-hms
math-normalize-mod math-parse-date math-read-angle-brackets
math-setup-add-holidays math-setup-holidays math-setup-year-holidays
math-sort-intv math-to-business-day math-to-hms)

 ("calc-frac" calc-add-fractions
calc-div-fractions calc-mul-fractions calcFunc-fdiv calcFunc-frac
math-make-frac)

 ("calc-funcs" calc-prob-dist calcFunc-bern
calcFunc-besJ calcFunc-besY calcFunc-beta calcFunc-betaB
calcFunc-betaI calcFunc-erf calcFunc-erfc calcFunc-euler
calcFunc-gamma calcFunc-gammaG calcFunc-gammaP calcFunc-gammaQ
calcFunc-gammag calcFunc-ltpb calcFunc-ltpc calcFunc-ltpf
calcFunc-ltpn calcFunc-ltpp calcFunc-ltpt calcFunc-utpb calcFunc-utpc
calcFunc-utpf calcFunc-utpn calcFunc-utpp calcFunc-utpt
math-bernoulli-number math-gammap1-raw)

 ("calc-graph" calc-graph-show-tty)

 ("calc-incom" calc-digit-dots)

 ("calc-keypd" calc-do-keypad
calc-keypad-x-left-click calc-keypad-x-middle-click
calc-keypad-x-right-click)

 ("calc-lang" calc-set-language
math-read-big-balance math-read-big-rec)

 ("calc-map" calc-get-operator calcFunc-accum
calcFunc-afixp calcFunc-anest calcFunc-apply calcFunc-call
calcFunc-fixp calcFunc-inner calcFunc-map calcFunc-mapa calcFunc-mapc
calcFunc-mapd calcFunc-mapeq calcFunc-mapeqp calcFunc-mapeqr
calcFunc-mapr calcFunc-nest calcFunc-outer calcFunc-raccum
calcFunc-reduce calcFunc-reducea calcFunc-reducec calcFunc-reduced
calcFunc-reducer calcFunc-rreduce calcFunc-rreducea calcFunc-rreducec
calcFunc-rreduced calcFunc-rreducer math-build-call
math-calcFunc-to-var math-multi-subst math-multi-subst-rec
math-var-to-calcFunc)

 ("calc-mtx" calcFunc-det calcFunc-lud calcFunc-tr
math-col-matrix math-lud-solve math-matrix-inv-raw math-matrix-lud
math-mul-mat-vec math-mul-mats math-row-matrix)

 ("calc-math" calcFunc-alog calcFunc-arccos
calcFunc-arccosh calcFunc-arcsin calcFunc-arcsincos calcFunc-arcsinh
calcFunc-arctan calcFunc-arctan2 calcFunc-arctanh calcFunc-csc
calcFunc-csch calcFunc-cos calcFunc-cosh calcFunc-cot calcFunc-coth
calcFunc-deg calcFunc-exp calcFunc-exp10 calcFunc-expm1
calcFunc-hypot calcFunc-ilog calcFunc-isqrt calcFunc-ln calcFunc-lnp1
calcFunc-log calcFunc-log10 calcFunc-nroot calcFunc-rad calcFunc-sec
calcFunc-sech calcFunc-sin
calcFunc-sincos calcFunc-sinh calcFunc-sqr calcFunc-sqrt calcFunc-tan
calcFunc-tanh math-arccos-raw math-arcsin-raw math-arctan-raw
math-arctan2-raw math-cos-raw math-cot-raw math-csc-raw
math-exp-minus-1-raw math-exp-raw
math-from-radians math-from-radians-2 math-hypot math-infinite-dir
math-isqrt-small math-ln-raw math-nearly-equal math-nearly-equal-float
math-nearly-zerop math-nearly-zerop-float math-nth-root
math-sin-cos-raw math-sin-raw math-sqrt math-sqrt-float math-sqrt-raw
math-tan-raw math-to-radians math-to-radians-2)

 ("calc-mode" math-get-modes-vec)

 ("calc-poly" calcFunc-apart calcFunc-expand
calcFunc-expandpow calcFunc-factor calcFunc-factors calcFunc-nrat
calcFunc-pcont calcFunc-pdeg calcFunc-pdiv calcFunc-pdivide
calcFunc-pdivrem calcFunc-pgcd calcFunc-plead calcFunc-pprim
calcFunc-prem math-accum-factors math-atomic-factorp
math-div-poly-const math-div-thru math-expand-power math-expand-term
math-factor-contains math-factor-expr math-factor-expr-part
math-factor-expr-try math-factor-finish math-factor-poly-coefs
math-factor-protect math-mul-thru math-padded-polynomial
math-partial-fractions math-poly-degree math-poly-deriv-coefs
math-poly-gcd-frac-list math-poly-modulus-rec math-ratpoly-p
math-to-ratpoly math-to-ratpoly-rec)

 ("calc-prog" calc-default-formula-arglist
calc-execute-kbd-macro calc-finish-user-syntax-edit
calc-fix-token-name calc-fix-user-formula calc-read-parse-table
calc-read-parse-table-part calc-subsetp calc-write-parse-table
calc-write-parse-table-part calcFunc-constant calcFunc-eq calcFunc-geq
calcFunc-gt calcFunc-if calcFunc-in calcFunc-integer calcFunc-istrue
calcFunc-land calcFunc-leq calcFunc-lnot calcFunc-lor calcFunc-lt
calcFunc-negative calcFunc-neq calcFunc-nonvar calcFunc-real
calcFunc-refers calcFunc-rmeq calcFunc-typeof calcFunc-variable
math-body-refers-to math-break math-composite-inequalities
math-do-defmath math-handle-for math-handle-foreach
math-normalize-logical-op math-return)

 ("calc-rewr" calcFunc-match calcFunc-matches
calcFunc-matchnot calcFunc-rewrite calcFunc-vmatches
math-apply-rewrites math-compile-patterns math-compile-rewrites
math-flatten-lands math-match-patterns math-rewrite
math-rewrite-heads)

 ("calc-rules" calc-CommuteRules calc-DistribRules calc-FactorRules
calc-FitRules calc-IntegAfterRules calc-InvertRules calc-JumpRules
calc-MergeRules calc-NegateRules
calc-compile-rule-set)

 ("calc-sel" calc-auto-selection
calc-delete-selection calc-encase-atoms calc-find-assoc-parent-formula
calc-find-parent-formula calc-find-sub-formula calc-prepare-selection
calc-preserve-point calc-replace-selections calc-replace-sub-formula
calc-roll-down-with-selections calc-roll-up-with-selections
calc-sel-error)

 ("calc-stat" calc-vector-op calcFunc-agmean
calcFunc-vcorr calcFunc-vcount calcFunc-vcov calcFunc-vflat
calcFunc-vgmean calcFunc-vhmean calcFunc-vmax calcFunc-vmean
calcFunc-vmeane calcFunc-vmedian calcFunc-vmin calcFunc-vpcov
calcFunc-vprod calcFunc-vpsdev calcFunc-vpvar calcFunc-vsdev
calcFunc-vsum calcFunc-vvar math-flatten-many-vecs)

 ("calc-store" calc-read-var-name
calc-store-value calc-var-name)

 ("calc-stuff" calc-explain-why calcFunc-clean
calcFunc-pclean calcFunc-pfloat calcFunc-pfrac)

 ("calc-units" calcFunc-usimplify calcFunc-lufadd calcFunc-lupadd
calcFunc-lufsub calcFunc-lupsub calcFunc-lufmul calcFunc-lupmul
calcFunc-lufdiv calcFunc-lupdiv calcFunc-lufquant calcFunc-lupquant
calcFunc-dbfield calcFunc-dbpower calcFunc-npfield
calcFunc-nppower calcFunc-spn calcFunc-midi calcFunc-freq
math-build-units-table math-build-units-table-buffer
math-check-unit-name math-convert-temperature math-convert-units
math-extract-units math-remove-units math-simplify-units
math-single-units-in-expr-p math-to-standard-units
math-units-in-expr-p)

 ("calc-vec" calcFunc-append calcFunc-appendrev
calcFunc-arrange calcFunc-cnorm calcFunc-cons calcFunc-cross
calcFunc-kron calcFunc-ctrn calcFunc-cvec calcFunc-diag calcFunc-find
calcFunc-getdiag calcFunc-grade calcFunc-head calcFunc-histogram
calcFunc-idn calcFunc-index calcFunc-mcol calcFunc-mdims
calcFunc-mrcol calcFunc-mrow calcFunc-mrrow calcFunc-pack
calcFunc-rcons calcFunc-rdup calcFunc-rev calcFunc-rgrade
calcFunc-rhead calcFunc-rnorm calcFunc-rsort calcFunc-rsubvec
calcFunc-rtail calcFunc-sort calcFunc-subscr calcFunc-subvec
calcFunc-tail calcFunc-trn calcFunc-unpack calcFunc-unpackt
calcFunc-vcard calcFunc-vcompl calcFunc-vconcat calcFunc-vconcatrev
calcFunc-vdiff calcFunc-vec calcFunc-venum calcFunc-vexp
calcFunc-vfloor calcFunc-vint calcFunc-vlen calcFunc-vmask
calcFunc-vpack calcFunc-vspan calcFunc-vunion calcFunc-vunpack
calcFunc-vxor math-check-for-commas math-clean-set math-copy-matrix
math-dimension-error math-dot-product math-flatten-vector math-map-vec
math-map-vec-2 math-mat-col math-mimic-ident math-prepare-set
math-read-brackets math-reduce-cols math-reduce-vec math-transpose)

 ("calc-yank" calc-alg-edit calc-clean-newlines
calc-do-grab-rectangle calc-do-grab-region calc-finish-stack-edit
calc-copy-to-register calc-insert-register
calc-append-to-register calc-prepend-to-register
calc-force-refresh calc-locate-cursor-element calc-show-edit-buffer)

))

  (mapcar (function (lambda (x)
    (mapcar (function (lambda (cmd)
      (autoload cmd (car x) nil t))) (cdr x))))
    '(

 ("calc-alg" calc-alg-evaluate calc-apart calc-collect calc-expand
calc-expand-formula calc-factor calc-normalize-rat calc-poly-div
calc-poly-div-rem calc-poly-gcd calc-poly-rem calc-simplify
calc-simplify-extended calc-substitute calc-powerexpand)

 ("calcalg2" calc-alt-summation calc-derivative
calc-dump-integral-cache calc-integral calc-num-integral
calc-poly-roots calc-product calc-solve-for calc-summation
calc-tabulate calc-taylor)

 ("calcalg3" calc-curve-fit calc-find-maximum calc-find-minimum
calc-find-root calc-poly-interp)

 ("calc-arith" calc-abs calc-abssqr calc-ceiling calc-decrement
calc-floor calc-idiv calc-increment calc-mant-part calc-max calc-min
calc-round calc-scale-float calc-sign calc-trunc calc-xpon-part)

 ("calc-bin" calc-and calc-binary-radix calc-clip calc-twos-complement-mode
calc-decimal-radix calc-diff calc-hex-radix calc-leading-zeros
calc-lshift-arith calc-lshift-binary calc-not calc-octal-radix calc-or calc-radix
calc-rotate-binary calc-rshift-arith calc-rshift-binary calc-word-size
calc-xor)

 ("calc-comb" calc-choose calc-double-factorial calc-extended-gcd
calc-factorial calc-gamma calc-gcd calc-lcm calc-moebius
calc-next-prime calc-perm calc-prev-prime calc-prime-factors
calc-prime-test calc-random calc-random-again calc-rrandom
calc-shuffle calc-totient)

 ("calc-cplx" calc-argument calc-complex-notation calc-i-notation
calc-im calc-j-notation calc-polar calc-polar-mode calc-re)

 ("calc-embed" calc-embedded-copy-formula-as-kill
calc-embedded-duplicate calc-embedded-edit calc-embedded-forget
calc-embedded-kill-formula calc-embedded-mark-formula
calc-embedded-new-formula calc-embedded-next calc-embedded-previous
calc-embedded-select calc-embedded-update-formula calc-embedded-word
calc-find-globals calc-show-plain)

 ("calc-fin" calc-convert-percent calc-fin-ddb calc-fin-fv
calc-fin-irr calc-fin-nper calc-fin-npv calc-fin-pmt calc-fin-pv
calc-fin-rate calc-fin-sln calc-fin-syd calc-percent-change)

 ("calc-forms" calc-business-days-minus calc-business-days-plus
calc-convert-time-zones calc-date calc-date-notation calc-date-part
calc-from-hms calc-hms-mode calc-hms-notation calc-inc-month
calc-julian calc-new-month calc-new-week calc-new-year calc-now
calc-time calc-time-zone calc-to-hms calc-unix-time)

 ("calc-frac" calc-fdiv calc-frac-mode calc-fraction
calc-over-notation calc-slash-notation)

 ("calc-funcs" calc-bernoulli-number calc-bessel-J calc-bessel-Y
calc-beta calc-erf calc-erfc calc-euler-number calc-inc-beta
calc-inc-gamma calc-stirling-number calc-utpb calc-utpc calc-utpf
calc-utpn calc-utpp calc-utpt)

 ("calc-graph" calc-graph-add calc-graph-add-3d calc-graph-border
calc-graph-clear calc-graph-command calc-graph-delete
calc-graph-device calc-graph-display calc-graph-fast
calc-graph-fast-3d calc-graph-geometry calc-graph-grid
calc-graph-header calc-graph-hide calc-graph-juggle calc-graph-key
calc-graph-kill calc-graph-line-style calc-graph-log-x
calc-graph-log-y calc-graph-log-z calc-graph-name
calc-graph-num-points calc-graph-output calc-graph-plot
calc-graph-point-style calc-graph-print calc-graph-quit
calc-graph-range-x calc-graph-range-y calc-graph-range-z
calc-graph-show-dumb calc-graph-title-x calc-graph-title-y
calc-graph-title-z calc-graph-view-commands calc-graph-view-trail
calc-graph-zero-x calc-graph-zero-y)

 ("calc-help" calc-a-prefix-help calc-b-prefix-help calc-c-prefix-help
calc-d-prefix-help calc-describe-function calc-describe-key
calc-describe-key-briefly calc-describe-variable calc-f-prefix-help
calc-full-help calc-g-prefix-help calc-help-prefix
calc-hyperbolic-prefix-help calc-inv-hyp-prefix-help calc-option-prefix-help
calc-inverse-prefix-help calc-j-prefix-help calc-k-prefix-help
calc-m-prefix-help calc-r-prefix-help calc-s-prefix-help
calc-t-prefix-help calc-u-prefix-help calc-l-prefix-help
calc-v-prefix-help)

 ("calc-incom" calc-begin-complex calc-begin-vector calc-comma
calc-dots calc-end-complex calc-end-vector calc-semi)

 ("calc-keypd" calc-keypad-menu calc-keypad-menu-back
calc-keypad-press)

 ("calc-lang" calc-big-language calc-c-language calc-eqn-language
calc-flat-language calc-fortran-language calc-maple-language
calc-yacas-language calc-maxima-language calc-giac-language
calc-mathematica-language calc-normal-language calc-pascal-language
calc-tex-language calc-latex-language calc-unformatted-language)

 ("calc-map" calc-accumulate calc-apply calc-inner-product calc-map
calc-map-equation calc-map-stack calc-outer-product calc-reduce)

 ("calc-mtx" calc-mdet calc-mlud calc-mtrace)

 ("calc-math" calc-arccos calc-arccosh calc-arcsin calc-arcsinh
calc-arctan calc-arctan2 calc-arctanh calc-conj calc-cos calc-cosh
calc-cot calc-coth calc-csc calc-csch
calc-degrees-mode calc-exp calc-expm1 calc-hypot calc-ilog
calc-imaginary calc-isqrt calc-ln calc-lnp1 calc-log calc-log10
calc-pi calc-radians-mode calc-sec calc-sech
calc-sin calc-sincos calc-sinh calc-sqrt
calc-tan calc-tanh calc-to-degrees calc-to-radians)

 ("calc-mode" calc-alg-simplify-mode calc-algebraic-mode
calc-always-load-extensions calc-auto-recompute calc-auto-why
calc-bin-simplify-mode calc-break-vectors calc-center-justify
calc-default-simplify-mode calc-display-raw calc-eng-notation
calc-ext-simplify-mode calc-fix-notation calc-full-trail-vectors
calc-full-vectors calc-get-modes calc-group-char calc-group-digits
calc-infinite-mode calc-left-justify calc-left-label
calc-line-breaking calc-line-numbering calc-matrix-brackets
calc-matrix-center-justify calc-matrix-left-justify calc-matrix-mode
calc-matrix-right-justify calc-mode-record-mode calc-no-simplify-mode
calc-normal-notation calc-num-simplify-mode calc-point-char
calc-right-justify calc-right-label calc-save-modes calc-sci-notation
calc-settings-file-name calc-shift-prefix calc-symbolic-mode
calc-total-algebraic-mode calc-truncate-down calc-truncate-stack
calc-truncate-up calc-units-simplify-mode calc-vector-braces
calc-vector-brackets calc-vector-commas calc-vector-parens
calc-working)

 ("calc-prog" calc-call-last-kbd-macro calc-edit-user-syntax
calc-equal-to calc-get-user-defn calc-greater-equal calc-greater-than
calc-in-set calc-kbd-break calc-kbd-else calc-kbd-else-if
calc-kbd-end-for calc-kbd-end-if calc-kbd-end-loop calc-kbd-end-repeat
calc-kbd-for calc-kbd-if calc-kbd-loop calc-kbd-pop calc-kbd-push
calc-kbd-query calc-kbd-repeat calc-kbd-report calc-less-equal
calc-less-than calc-logical-and calc-logical-if calc-logical-not
calc-logical-or calc-not-equal-to calc-pass-errors calc-remove-equal
calc-timing calc-user-define calc-user-define-composition
calc-user-define-edit calc-user-define-formula
calc-user-define-invocation calc-user-define-kbd-macro
calc-user-define-permanent calc-user-undefine)

 ("calc-rewr" calc-match calc-rewrite calc-rewrite-selection)

 ("calc-sel" calc-break-selections calc-clear-selections
calc-copy-selection calc-del-selection calc-edit-selection
calc-enable-selections calc-enter-selection calc-sel-add-both-sides
calc-sel-div-both-sides calc-sel-evaluate calc-sel-expand-formula
calc-sel-mult-both-sides calc-sel-sub-both-sides
calc-select-additional calc-select-here calc-select-here-maybe
calc-select-less calc-select-more calc-select-next calc-select-once
calc-select-once-maybe calc-select-part calc-select-previous
calc-show-selections calc-unselect)

 ("calcsel2" calc-commute-left calc-commute-right calc-sel-commute
calc-sel-distribute calc-sel-invert calc-sel-isolate
calc-sel-jump-equals calc-sel-merge calc-sel-negate calc-sel-unpack)

 ("calc-stat" calc-vector-correlation calc-vector-count
calc-vector-covariance calc-vector-geometric-mean
calc-vector-harmonic-mean calc-vector-max calc-vector-mean
calc-vector-mean-error calc-vector-median calc-vector-min
calc-vector-pop-covariance calc-vector-pop-sdev
calc-vector-pop-variance calc-vector-product calc-vector-sdev
calc-vector-sum calc-vector-variance)

 ("calc-store" calc-assign calc-copy-special-constant
calc-copy-variable calc-declare-variable
calc-edit-AlgSimpRules calc-edit-Decls calc-edit-EvalRules
calc-edit-ExtSimpRules calc-edit-FitRules calc-edit-GenCount
calc-edit-Holidays calc-edit-IntegLimit calc-edit-LineStyles
calc-edit-PlotRejects calc-edit-PointStyles calc-edit-TimeZone
calc-edit-Units calc-edit-variable calc-evalto calc-insert-variables
calc-let calc-permanent-variable calc-recall calc-recall-quick
calc-store calc-store-concat calc-store-decr calc-store-div
calc-store-exchange calc-store-incr calc-store-into
calc-store-into-quick calc-store-inv calc-store-map calc-store-minus
calc-store-neg calc-store-plus calc-store-power calc-store-quick
calc-store-times calc-subscript calc-unstore)

 ("calc-stuff" calc-clean calc-clean-num calc-flush-caches
calc-less-recursion-depth calc-more-recursion-depth calc-num-prefix
calc-why)

 ("calc-trail" calc-trail-backward calc-trail-first calc-trail-forward
calc-trail-in calc-trail-isearch-backward calc-trail-isearch-forward
calc-trail-kill calc-trail-last calc-trail-marker calc-trail-next
calc-trail-out calc-trail-previous calc-trail-scroll-left
calc-trail-scroll-right calc-trail-yank)

 ("calc-undo" calc-last-args calc-redo)

 ("calc-units" calc-autorange-units calc-base-units
calc-convert-temperature calc-convert-units calc-define-unit
calc-enter-units-table calc-explain-units calc-extract-units
calc-get-unit-definition calc-permanent-units calc-quick-units
calc-remove-units calc-simplify-units calc-undefine-unit
calc-view-units-table calc-lu-quant calc-db
calc-np calc-lu-plus calc-lu-minus
calc-lu-times calc-lu-divide calc-spn calc-midi
calc-freq)

 ("calc-vec" calc-arrange-vector calc-build-vector calc-cnorm
calc-conj-transpose calc-cons calc-cross calc-kron calc-diag
calc-display-strings calc-expand-vector calc-grade calc-head
calc-histogram calc-ident calc-index calc-mask-vector calc-mcol
calc-mrow calc-pack calc-pack-bits calc-remove-duplicates
calc-reverse-vector calc-rnorm calc-set-cardinality
calc-set-complement calc-set-difference calc-set-enumerate
calc-set-floor calc-set-intersect calc-set-span calc-set-union
calc-set-xor calc-sort calc-subvector calc-tail calc-transpose
calc-unpack calc-unpack-bits calc-vector-find calc-vlength)

 ("calc-yank" calc-copy-as-kill calc-copy-region-as-kill
calc-copy-to-buffer calc-edit calc-edit-cancel calc-edit-mode
calc-kill calc-kill-region calc-yank))))

(defun calc-init-prefixes ()
  (if calc-shift-prefix
      (progn
	(define-key calc-mode-map "A" (lookup-key calc-mode-map "a"))
	(define-key calc-mode-map "B" (lookup-key calc-mode-map "b"))
	(define-key calc-mode-map "C" (lookup-key calc-mode-map "c"))
	(define-key calc-mode-map "D" (lookup-key calc-mode-map "d"))
	(define-key calc-mode-map "F" (lookup-key calc-mode-map "f"))
	(define-key calc-mode-map "G" (lookup-key calc-mode-map "g"))
	(define-key calc-mode-map "J" (lookup-key calc-mode-map "j"))
	(define-key calc-mode-map "K" (lookup-key calc-mode-map "k"))
	(define-key calc-mode-map "M" (lookup-key calc-mode-map "m"))
	(define-key calc-mode-map "S" (lookup-key calc-mode-map "s"))
	(define-key calc-mode-map "T" (lookup-key calc-mode-map "t"))
	(define-key calc-mode-map "U" (lookup-key calc-mode-map "u")))
    (define-key calc-mode-map "A" 'calc-abs)
    (define-key calc-mode-map "B" 'calc-log)
    (define-key calc-mode-map "C" 'calc-cos)
    (define-key calc-mode-map "D" 'calc-redo)
    (define-key calc-mode-map "F" 'calc-floor)
    (define-key calc-mode-map "G" 'calc-argument)
    (define-key calc-mode-map "J" 'calc-conj)
    (define-key calc-mode-map "K" 'calc-keep-args)
    (define-key calc-mode-map "M" 'calc-more-recursion-depth)
    (define-key calc-mode-map "S" 'calc-sin)
    (define-key calc-mode-map "T" 'calc-tan)
    (define-key calc-mode-map "U" 'calc-undo)))

(calc-init-extensions)




;;;; Miscellaneous.

;; calc-command-flags is declared in calc.el
(defvar calc-command-flags)

(defun calc-clear-command-flag (f)
  (setq calc-command-flags (delq f calc-command-flags)))


(defun calc-record-message (tag &rest args)
  (let ((msg (apply 'format args)))
    (message "%s" msg)
    (calc-record msg tag))
  (calc-clear-command-flag 'clear-message))


(defun calc-normalize-fancy (val)
  (let ((simp (if (consp calc-simplify-mode)
		  (car calc-simplify-mode)
		calc-simplify-mode)))
    (cond ((eq simp 'binary)
	   (let ((s (math-normalize val)))
	     (if (math-realp s)
		 (math-clip (math-round s))
	       s)))
	  ((eq simp 'alg)
	   (math-simplify val))
	  ((eq simp 'ext)
	   (math-simplify-extended val))
	  ((eq simp 'units)
	   (math-simplify-units val))
	  (t  ; nil, none, num
	   (math-normalize val)))))


(defvar calc-help-map nil)

(if calc-help-map
    nil
  (setq calc-help-map (make-keymap))
  (define-key calc-help-map "b" 'calc-describe-bindings)
  (define-key calc-help-map "c" 'calc-describe-key-briefly)
  (define-key calc-help-map "f" 'calc-describe-function)
  (define-key calc-help-map "h" 'calc-full-help)
  (define-key calc-help-map "i" 'calc-info)
  (define-key calc-help-map "k" 'calc-describe-key)
  (define-key calc-help-map "n" 'calc-view-news)
  (define-key calc-help-map "s" 'calc-info-summary)
  (define-key calc-help-map "t" 'calc-tutorial)
  (define-key calc-help-map "v" 'calc-describe-variable)
  (define-key calc-help-map "\C-c" 'calc-describe-copying)
  (define-key calc-help-map "\C-d" 'calc-describe-distribution)
  (define-key calc-help-map "\C-n" 'calc-view-news)
  (define-key calc-help-map "\C-w" 'calc-describe-no-warranty)
  (define-key calc-help-map "?" 'calc-help-for-help)
  (define-key calc-help-map "\C-h" 'calc-help-for-help))

(defvar calc-prefix-help-phase 0)
(defun calc-do-prefix-help (msgs group key)
  (if calc-full-help-flag
      (list msgs group key)
    (if (cdr msgs)
	(progn
	  (setq calc-prefix-help-phase
		(if (eq this-command last-command)
		    (% (1+ calc-prefix-help-phase) (1+ (length msgs)))
		  0))
	  (let ((msg (nth calc-prefix-help-phase msgs)))
	    (message "%s" (if msg
			      (concat group ": " msg ":"
				      (make-string
				       (- (apply 'max (mapcar 'length msgs))
					  (length msg)) 32)
				      "  [MORE]"
				      (if key
					  (concat "  " (char-to-string key)
						  "-")
					""))
			    (if key (format "%c-" key) "")))))
      (setq calc-prefix-help-phase 0)
      (if key
	  (if msgs
	      (message "%s: %s: %c-" group (car msgs) key)
	    (message "%s: (none)  %c-" group key))
	(message "%s: %s" group (car msgs))))
    (and key (calc-unread-command key))))

;;;; Commands.


;;; General.

(defun calc-reset (arg)
  (interactive "P")
  (setq arg (if arg (prefix-numeric-value arg) nil))
  (cond
   ((and
     calc-embedded-info
     (equal (aref calc-embedded-info 0) (current-buffer))
     (<= (point) (aref calc-embedded-info 5))
     (>= (point) (aref calc-embedded-info 4)))
    (let ((cbuf (aref calc-embedded-info 1))
          (calc-embedded-quiet t))
      (save-window-excursion
        (calc-embedded nil)
        (set-buffer cbuf)
        (calc-reset arg))
      (calc-embedded nil)))
   ((eq major-mode 'calc-mode)
    (save-excursion
      (unless (and arg (> (abs arg) 0))
        (setq calc-stack nil))
      (setq calc-undo-list nil
            calc-redo-list nil)
      (let (calc-stack calc-user-parse-tables calc-standard-date-formats
                       calc-invocation-macro)
        (mapc (function (lambda (v) (set v nil))) calc-local-var-list)
        (if (and arg (<= arg 0))
            (calc-mode-var-list-restore-default-values)
          (calc-mode-var-list-restore-saved-values)))
      (calc-set-language nil nil t)
      (calc-mode)
      (calc-flush-caches t)
      (run-hooks 'calc-reset-hook))
    (calc-wrapper
     (let ((win (get-buffer-window (current-buffer))))
       (calc-realign 0)
       ;; Adjust the window height if the window is visible, but doesn't
       ;; take up the whole height of the frame.
       (if (and
            win
	    (not (window-full-height-p)))
           (let ((height (- (window-height win) 2)))
             (set-window-point win (point))
             (or (= height calc-window-height)
                 (let ((swin (selected-window)))
                   (select-window win)
                   (enlarge-window (- calc-window-height height))
                   (select-window swin)))))))
    (message "(Calculator reset)"))
   (t
    (message "(Not inside a Calc buffer)"))))

;; What a pain; scroll-left behaves differently when called non-interactively.
(defun calc-scroll-left (n)
  (interactive "P")
  (setq prefix-arg (or n (/ (window-width) 2)))
  (call-interactively #'scroll-left))

(defun calc-scroll-right (n)
  (interactive "P")
  (setq prefix-arg (or n (/ (window-width) 2)))
  (call-interactively #'scroll-right))

(defun calc-scroll-up (n)
  (interactive "P")
  (condition-case err
      (scroll-up (or n (/ (window-height) 2)))
    (error nil))
  (if (pos-visible-in-window-p (max 1 (- (point-max) 2)))
      (if (eq major-mode 'calc-mode)
	  (calc-realign)
	(goto-char (point-max))
	(set-window-start (selected-window)
			  (save-excursion
			    (forward-line (- (1- (window-height))))
			    (point)))
	(forward-line -1))))

(defun calc-scroll-down (n)
  (interactive "P")
  (or (pos-visible-in-window-p 1)
      (scroll-down (or n (/ (window-height) 2)))))


(defun calc-precision (n)
  (interactive "NPrecision: ")
  (calc-wrapper
   (if (< (prefix-numeric-value n) 3)
       (error "Precision must be at least 3 digits")
     (calc-change-mode 'calc-internal-prec (prefix-numeric-value n)
		       (and (memq (car calc-float-format) '(float sci eng))
			    (< (nth 1 calc-float-format)
				(if (= calc-number-radix 10) 0 1))))
     (calc-record calc-internal-prec "prec"))
   (message "Floating-point precision is %d digits" calc-internal-prec)))


(defun calc-inverse (&optional n)
  (interactive "P")
  (let* ((hyp-flag (if (or
                        (eq major-mode 'calc-keypad-mode)
                        (eq major-mode 'calc-trail-mode))
                       (with-current-buffer calc-main-buffer
                         calc-hyperbolic-flag)
                     calc-hyperbolic-flag))
         (opt-flag (if (or
                        (eq major-mode 'calc-keypad-mode)
                        (eq major-mode 'calc-trail-mode))
                       (with-current-buffer calc-main-buffer
                         calc-option-flag)
                     calc-option-flag))
         (msg
          (cond
           ((and opt-flag hyp-flag) "Option Inverse Hyperbolic...")
           (hyp-flag "Inverse Hyperbolic...")
           (opt-flag "Option Inverse...")
           (t "Inverse..."))))
    (calc-fancy-prefix 'calc-inverse-flag msg n)))

(defconst calc-fancy-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'calc-fancy-prefix-other-key)
    (define-key map (vector meta-prefix-char t) 'calc-fancy-prefix-other-key)
    (define-key map [switch-frame] nil)
    (define-key map [?\C-u] 'universal-argument)
    (define-key map [?0] 'digit-argument)
    (define-key map [?1] 'digit-argument)
    (define-key map [?2] 'digit-argument)
    (define-key map [?3] 'digit-argument)
    (define-key map [?4] 'digit-argument)
    (define-key map [?5] 'digit-argument)
    (define-key map [?6] 'digit-argument)
    (define-key map [?7] 'digit-argument)
    (define-key map [?8] 'digit-argument)
    (define-key map [?9] 'digit-argument)
    map)
  "Keymap used while processing calc-fancy-prefix.")

(defvar calc-is-keypad-press nil)
(defun calc-fancy-prefix (flag msg n)
  (let (prefix)
    (calc-wrapper
     (calc-set-command-flag 'keep-flags)
     (calc-set-command-flag 'no-align)
     (setq prefix (set flag (not (symbol-value flag)))
	   prefix-arg n)
     (message "%s" (if prefix msg "")))
    (and prefix
	 (not calc-is-keypad-press)
	 (if (boundp 'overriding-terminal-local-map)
	     (setq overriding-terminal-local-map calc-fancy-prefix-map)
	   (let ((event (calc-read-key t)))
	     (if (eq (setq last-command-event (car event)) ?\C-u)
		 (universal-argument)
	       (if (or (not (integerp last-command-event))
		       (and (>= last-command-event 0) (< last-command-event ? )
			    (not (memq last-command-event '(?\e)))))
		   (calc-wrapper))  ; clear flags if not a Calc command.
               (setq last-command-event (cdr event))
	       (if (or (not (integerp last-command-event))
		       (eq last-command-event ?-))
		   (calc-unread-command)
		 (digit-argument n))))))))

(defun calc-fancy-prefix-other-key (arg)
  (interactive "P")
  (if (and
       (not (eq last-command-event 'tab))
       (not (eq last-command-event 'M-tab))
       (or (not (integerp last-command-event))
           (and (>= last-command-event 0) (< last-command-event ? )
                (not (eq last-command-event meta-prefix-char)))))
     (calc-wrapper))  ; clear flags if not a Calc command.
  (setq prefix-arg arg)
  (calc-unread-command)
  (setq overriding-terminal-local-map nil))

(defun calc-invert-func ()
  (save-excursion
    (calc-select-buffer)
    (setq calc-inverse-flag (not (calc-is-inverse))
	  calc-hyperbolic-flag (calc-is-hyperbolic)
	  current-prefix-arg nil)))

(defun calc-is-inverse ()
  calc-inverse-flag)

(defun calc-hyperbolic (&optional n)
  (interactive "P")
  (let* ((inv-flag (if (or
                        (eq major-mode 'calc-keypad-mode)
                        (eq major-mode 'calc-trail-mode))
                       (with-current-buffer calc-main-buffer
                         calc-inverse-flag)
                     calc-inverse-flag))
         (opt-flag (if (or
                        (eq major-mode 'calc-keypad-mode)
                        (eq major-mode 'calc-trail-mode))
                       (with-current-buffer calc-main-buffer
                         calc-option-flag)
                     calc-option-flag))
         (msg
          (cond
           ((and opt-flag inv-flag) "Option Inverse Hyperbolic...")
           (opt-flag "Option Hyperbolic...")
           (inv-flag "Inverse Hyperbolic...")
           (t "Hyperbolic..."))))
    (calc-fancy-prefix 'calc-hyperbolic-flag msg n)))

(defun calc-hyperbolic-func ()
  (save-excursion
    (calc-select-buffer)
    (setq calc-inverse-flag (calc-is-inverse)
	  calc-hyperbolic-flag (not (calc-is-hyperbolic))
	  current-prefix-arg nil)))

(defun calc-is-hyperbolic ()
  calc-hyperbolic-flag)

(defun calc-option (&optional n)
  (interactive "P")
  (let* ((inv-flag (if (or
                        (eq major-mode 'calc-keypad-mode)
                        (eq major-mode 'calc-trail-mode))
                       (with-current-buffer calc-main-buffer
                         calc-inverse-flag)
                     calc-inverse-flag))
         (hyp-flag (if (or
                        (eq major-mode 'calc-keypad-mode)
                        (eq major-mode 'calc-trail-mode))
                       (with-current-buffer calc-main-buffer
                         calc-hyperbolic-flag)
                     calc-hyperbolic-flag))
         (msg
          (cond
           ((and hyp-flag inv-flag) "Option Inverse Hyperbolic...")
           (hyp-flag "Option Hyperbolic...")
           (inv-flag "Option Inverse...")
           (t "Option..."))))
    (calc-fancy-prefix 'calc-option-flag msg n)))

(defun calc-is-option ()
  calc-option-flag)

(defun calc-keep-args (&optional n)
  (interactive "P")
  (calc-fancy-prefix 'calc-keep-args-flag "Keep args..." n))


(defun calc-change-mode (var value &optional refresh option)
  (if option
      (setq value (if value
		      (> (prefix-numeric-value value) 0)
		    (not (symbol-value var)))))
  (or (consp var) (setq var (list var) value (list value)))
  (if calc-inverse-flag
      (let ((old nil))
	(or refresh (error "Not a display-mode command"))
	(calc-check-stack 1)
	(unwind-protect
	    (let ((v var))
	      (while v
		(setq old (cons (symbol-value (car v)) old))
		(set (car v) (car value))
		(setq v (cdr v)
		      value (cdr value)))
	      (calc-refresh-top 1)
	      (calc-refresh-evaltos)
	      (symbol-value (car var)))
	  (let ((v var))
	    (setq old (nreverse old))
	    (while v
	      (set (car v) (car old))
	      (setq v (cdr v)
		    old (cdr old)))
	    (if (eq (car var) 'calc-language)
		(calc-set-language calc-language calc-language-option t)))))
    (let ((chg nil)
	  (v var))
      (while v
	(or (equal (symbol-value (car v)) (car value))
	    (progn
	      (set (car v) (car value))
	      (if (eq (car v) 'calc-float-format)
		  (setq calc-full-float-format
			(list (if (eq (car (car value)) 'fix)
				  'float
				(car (car value)))
			      0)))
	      (setq chg t)))
	(setq v (cdr v)
	      value (cdr value)))
      (if chg
	  (progn
	    (or (and refresh (calc-do-refresh))
		(calc-refresh-evaltos))
	    (and (eq calc-mode-save-mode 'save)
		 (not (equal var '(calc-mode-save-mode)))
		 (calc-save-modes))))
      (if calc-embedded-info (calc-embedded-modes-change var))
      (symbol-value (car var)))))

(defun calc-toggle-banner ()
  "Toggle display of the friendly greeting calc normally shows above the stack."
  (interactive)
  (setq calc-show-banner (not calc-show-banner))
  (calc-refresh))

(defun calc-refresh-top (n)
  (interactive "p")
  (calc-wrapper
   (cond ((< n 0)
	  (setq n (- n))
	  (let ((entry (calc-top n 'entry))
		(calc-undo-list nil) (calc-redo-list nil))
	    (calc-pop-stack 1 n t)
	    (calc-push-list (list (car entry)) n (list (nth 2 entry)))))
	 ((= n 0)
	  (calc-refresh))
	 (t
	  (let ((entries (calc-top-list n 1 'entry))
		(calc-undo-list nil) (calc-redo-list nil))
	    (calc-pop-stack n 1 t)
	    (calc-push-list (mapcar 'car entries)
			    1
			    (mapcar (function (lambda (x) (nth 2 x)))
				    entries)))))))

(defvar calc-refreshing-evaltos nil)
(defvar calc-no-refresh-evaltos nil)
(defun calc-refresh-evaltos (&optional which-var)
  (and calc-any-evaltos calc-auto-recompute (not calc-no-refresh-evaltos)
       (let ((calc-refreshing-evaltos t)
	     (num (calc-stack-size))
	     (calc-undo-list nil) (calc-redo-list nil)
	     value new-val)
	 (while (> num 0)
	   (setq value (calc-top num 'entry))
	   (if (and (not (nth 2 value))
		    (setq value (car value))
		    (or (eq (car-safe value) 'calcFunc-evalto)
			(and (eq (car-safe value) 'vec)
			     (eq (car-safe (nth 1 value)) 'calcFunc-evalto))))
	       (progn
		 (setq new-val (math-normalize value))
		 (or (equal new-val value)
		     (progn
		       (calc-push-list (list new-val) num)
		       (calc-pop-stack 1 (1+ num) t)))))
	   (setq num (1- num)))))
  (and calc-embedded-active which-var
       (calc-embedded-var-change which-var)))

(defun calc-push (&rest vals)
  (calc-push-list vals))

(defun calc-pop-push (n &rest vals)
  (calc-pop-push-list n vals))

(defun calc-pop-push-record (n prefix &rest vals)
  (calc-pop-push-record-list n prefix vals))


(defun calc-evaluate (n)
  (interactive "p")
  (calc-slow-wrapper
   (if (= n 0)
       (setq n (calc-stack-size)))
   (calc-with-default-simplification
    (if (< n 0)
	(calc-pop-push-record-list 1 "eval"
				   (math-evaluate-expr (calc-top (- n)))
				   (- n))
      (calc-pop-push-record-list n "eval" (mapcar 'math-evaluate-expr
						  (calc-top-list n)))))
   (calc-handle-whys)))


(defun calc-eval-num (n)
  (interactive "P")
  (calc-slow-wrapper
   (let* ((nn (prefix-numeric-value n))
	  (calc-internal-prec (cond ((>= nn 3) nn)
				    ((< nn 0) (max (+ calc-internal-prec nn)
						   3))
				    (t calc-internal-prec)))
	  (calc-symbolic-mode nil))
     (calc-with-default-simplification
      (calc-pop-push-record 1 "num" (math-evaluate-expr (calc-top 1)))))
   (calc-handle-whys)))


(defvar calc-extended-command-history nil
  "The history list for calc-execute-extended-command.")

(defun calc-execute-extended-command (n)
  (interactive "P")
  (let* ((prompt (concat (calc-num-prefix-name n) "M-x "))
	 (cmd (intern
               (completing-read prompt obarray 'commandp t "calc-"
                                'calc-extended-command-history))))
    (setq prefix-arg n)
    (command-execute cmd)))


(defun calc-realign (&optional num)
  (interactive "P")
  (if (and num (eq major-mode 'calc-mode))
      (progn
	(calc-check-stack num)
	(calc-cursor-stack-index num)
	(and calc-line-numbering
	     (forward-char 4)))
    (if (and calc-embedded-info
	     (eq (current-buffer) (aref calc-embedded-info 0)))
	(progn
	  (goto-char (aref calc-embedded-info 2))
	  (if (with-current-buffer (aref calc-embedded-info 1)
                calc-show-plain)
	      (forward-line 1)))
      (calc-wrapper
       (if (get-buffer-window (current-buffer))
	   (set-window-hscroll (get-buffer-window (current-buffer)) 0))))))

(defvar math-cache-list nil)

(defun calc-var-value (v)
  (and (symbolp v)
       (boundp v)
       (symbol-value v)
       (if (symbolp (symbol-value v))
	   (set v (funcall (symbol-value v)))
	 (if (stringp (symbol-value v))
	     (let ((val (math-read-expr (symbol-value v))))
	       (if (eq (car-safe val) 'error)
		   (error "Bad format in variable contents: %s" (nth 2 val))
		 (set v val)))
	   (symbol-value v)))))

;;; In the following table, ( OP LOPS ROPS ) means that if an OP
;;; term appears as the first argument to any LOPS term, or as the
;;; second argument to any ROPS term, then they should be treated
;;; as one large term for purposes of associative selection.
(defconst calc-assoc-ops '( ( + ( + - ) ( + ) )
			    ( - ( + - ) ( + ) )
			    ( * ( * )   ( * ) )
			    ( / ( / )   (   ) )
			    ( | ( | )   ( | ) )
			    ( calcFunc-land ( calcFunc-land )
					    ( calcFunc-land ) )
			    ( calcFunc-lor ( calcFunc-lor )
					   ( calcFunc-lor ) ) ))


(defvar var-CommuteRules 'calc-CommuteRules)
(defvar var-JumpRules    'calc-JumpRules)
(defvar var-DistribRules 'calc-DistribRules)
(defvar var-MergeRules   'calc-MergeRules)
(defvar var-NegateRules  'calc-NegateRules)
(defvar var-InvertRules  'calc-InvertRules)


(defconst calc-tweak-eqn-table '( ( calcFunc-eq  calcFunc-eq  calcFunc-neq )
				  ( calcFunc-neq calcFunc-neq calcFunc-eq  )
				  ( calcFunc-lt  calcFunc-gt  calcFunc-geq )
				  ( calcFunc-gt  calcFunc-lt  calcFunc-leq )
				  ( calcFunc-leq calcFunc-geq calcFunc-gt  )
				  ( calcFunc-geq calcFunc-leq calcFunc-lt  ) ))




(defun calc-float (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "flt"
		  (if (calc-is-hyperbolic) 'calcFunc-float 'calcFunc-pfloat)
		  arg)))


(defvar calc-gnuplot-process nil)
(defvar calc-gnuplot-input)
(defvar calc-gnuplot-buffer)

(defun calc-gnuplot-alive ()
  (and calc-gnuplot-process
       calc-gnuplot-buffer
       (buffer-name calc-gnuplot-buffer)
       calc-gnuplot-input
       (buffer-name calc-gnuplot-input)
       (memq (process-status calc-gnuplot-process) '(run stop))))





(defun calc-load-everything ()
  (interactive)
  (require 'calc-aent)
  (require 'calc-alg)
  (require 'calc-arith)
  (require 'calc-bin)
  (require 'calc-comb)
  (require 'calc-cplx)
  (require 'calc-embed)
  (require 'calc-fin)
  (require 'calc-forms)
  (require 'calc-frac)
  (require 'calc-funcs)
  (require 'calc-graph)
  (require 'calc-help)
  (require 'calc-incom)
  (require 'calc-keypd)
  (require 'calc-lang)
  (require 'calc-macs)
  (require 'calc-map)
  (require 'calc-math)
  (require 'calc-misc)
  (require 'calc-mode)
  (require 'calc-mtx)
  (require 'calc-poly)
  (require 'calc-prog)
  (require 'calc-rewr)
  (require 'calc-rules)
  (require 'calc-sel)
  (require 'calc-stat)
  (require 'calc-store)
  (require 'calc-stuff)
  (require 'calc-trail)
  (require 'calc-undo)
  (require 'calc-units)
  (require 'calc-vec)
  (require 'calc-yank)
  (require 'calcalg2)
  (require 'calcalg3)
  (require 'calccomp)
  (require 'calcsel2)

  (message "All parts of Calc are now loaded"))


;;; Vector commands.

(defun calc-concat (arg)
  (interactive "P")
  (calc-wrapper
   (if (calc-is-inverse)
       (if (calc-is-hyperbolic)
	   (calc-enter-result 2 "apnd" (list 'calcFunc-append
					  (calc-top 1) (calc-top 2)))
	 (calc-enter-result 2 "|" (list 'calcFunc-vconcat
					(calc-top 1) (calc-top 2))))
     (if (calc-is-hyperbolic)
	 (calc-binary-op "apnd" 'calcFunc-append arg '(vec))
       (calc-binary-op "|" 'calcFunc-vconcat arg '(vec) nil '|)))))

(defun calc-append (arg)
  (interactive "P")
  (calc-hyperbolic-func)
  (calc-concat arg))


(defconst calc-arg-values '( ( var ArgA var-ArgA ) ( var ArgB var-ArgB )
			     ( var ArgC var-ArgC ) ( var ArgD var-ArgD )
			     ( var ArgE var-ArgE ) ( var ArgF var-ArgF )
			     ( var ArgG var-ArgG ) ( var ArgH var-ArgH )
			     ( var ArgI var-ArgI ) ( var ArgJ var-ArgJ )
))

(defun calc-invent-args (n)
  (nreverse (nthcdr (- (length calc-arg-values) n) (reverse calc-arg-values))))




;;; User menu.

(defun calc-user-key-map ()
  (if (featurep 'xemacs)
      (error "User-defined keys are not supported in XEmacs"))
  (let ((res (cdr (lookup-key calc-mode-map "z"))))
    (if (eq (car (car res)) 27)
	(cdr res)
      res)))

(defvar calc-z-prefix-buf nil)
(defvar calc-z-prefix-msgs nil)

(defun calc-z-prefix-help ()
  (interactive)
  (let* ((calc-z-prefix-msgs nil)
	 (calc-z-prefix-buf "")
	 (kmap (sort (copy-sequence (calc-user-key-map))
		     (function (lambda (x y) (< (car x) (car y))))))
	 (flags (apply 'logior
		       (mapcar (function
				(lambda (k)
				  (calc-user-function-classify (car k))))
			       kmap))))
    (if (= (logand flags 8) 0)
	(calc-user-function-list kmap 7)
      (calc-user-function-list kmap 1)
      (setq calc-z-prefix-msgs (cons calc-z-prefix-buf calc-z-prefix-msgs)
	    calc-z-prefix-buf "")
      (calc-user-function-list kmap 6))
    (if (/= flags 0)
	(setq calc-z-prefix-msgs (cons calc-z-prefix-buf calc-z-prefix-msgs)))
    (calc-do-prefix-help (nreverse calc-z-prefix-msgs) "user" ?z)))

(defun calc-user-function-classify (key)
  (cond ((/= key (downcase key))    ; upper-case
	 (if (assq (downcase key) (calc-user-key-map)) 9 1))
	((/= key (upcase key)) 2)   ; lower-case
	((= key ??) 0)
	(t 4)))   ; other

(defun calc-user-function-list (map flags)
  (and map
       (let* ((key (car (car map)))
	      (kind (calc-user-function-classify key))
	      (func (cdr (car map))))
	 (if (or (= (logand kind flags) 0)
		 (not (symbolp func)))
	     ()
	   (let* ((name (symbol-name func))
		  (name (if (string-match "\\`calc-" name)
			    (substring name 5) name))
		  (pos (string-match (char-to-string key) name))
		  (desc
		   (if (symbolp func)
		       (if (= (logand kind 3) 0)
			   (format "`%c' = %s" key name)
			 (if pos
			     (format "%s%c%s"
				     (downcase (substring name 0 pos))
				     (upcase key)
				     (downcase (substring name (1+ pos))))
			   (format "%c = %s"
				   (upcase key)
				   (downcase name))))
		     (char-to-string (upcase key)))))
	     (if (= (length calc-z-prefix-buf) 0)
		 (setq calc-z-prefix-buf (concat (if (= flags 1) "SHIFT + " "")
				   desc))
	       (if (> (+ (length calc-z-prefix-buf) (length desc)) 58)
		   (setq calc-z-prefix-msgs
                         (cons calc-z-prefix-buf calc-z-prefix-msgs)
			 calc-z-prefix-buf (concat (if (= flags 1) "SHIFT + " "")
				     desc))
		 (setq calc-z-prefix-buf (concat calc-z-prefix-buf ", " desc))))))
	 (calc-user-function-list (cdr map) flags))))



(defun calc-shift-Z-prefix-help ()
  (interactive)
  (calc-do-prefix-help
   '("Define, Undefine, Formula, Kbd-macro, Edit, Get-defn"
     "Composition, Syntax; Invocation; Permanent; Timing"
     "kbd-macros: [ (if), : (else), | (else-if), ] (end-if)"
     "kbd-macros: < > (repeat), ( ) (for), { } (loop)"
     "kbd-macros: / (break)"
     "kbd-macros: ` (save), ' (restore)")
   "user" ?Z))


;;;; Caches.

(defmacro math-defcache (name init form)
  (let ((cache-prec (intern (concat (symbol-name name) "-cache-prec")))
	(cache-val (intern (concat (symbol-name name) "-cache")))
	(last-prec (intern (concat (symbol-name name) "-last-prec")))
	(last-val (intern (concat (symbol-name name) "-last"))))
    (list 'progn
;	  (list 'defvar cache-prec (if init (math-numdigs (nth 1 init)) -100))
	  (list 'defvar cache-prec
                `(cond
                  ((consp ,init) (math-numdigs (nth 1 ,init)))
                  (,init
                   (nth 1 (math-numdigs (eval ,init))))
                  (t
                   -100)))
	  (list 'defvar cache-val
                `(cond
                  ((consp ,init) ,init)
                  (,init (eval ,init))
                  (t ,init)))
	  (list 'defvar last-prec -100)
	  (list 'defvar last-val nil)
	  (list 'setq 'math-cache-list
		(list 'cons
		      (list 'quote cache-prec)
		      (list 'cons
			    (list 'quote last-prec)
			    'math-cache-list)))
	  (list 'defun
		name ()
		(list 'or
		      (list '= last-prec 'calc-internal-prec)
		      (list 'setq
			    last-val
			    (list 'math-normalize
				  (list 'progn
					(list 'or
					      (list '>= cache-prec
						    'calc-internal-prec)
					      (list 'setq
						    cache-val
						    (list 'let
							  '((calc-internal-prec
							     (+ calc-internal-prec
								4)))
							  form)
						    cache-prec
						    '(+ calc-internal-prec 2)))
					cache-val))
			    last-prec 'calc-internal-prec))
		last-val))))
(put 'math-defcache 'lisp-indent-hook 2)

;;; Betcha didn't know that pi = 16 atan(1/5) - 4 atan(1/239).   [F] [Public]
(defconst math-approx-pi
  (math-read-number-simple "3.141592653589793238463")
  "An approximation for pi.")

(math-defcache math-pi math-approx-pi
  (math-add-float (math-mul-float '(float 16 0)
				  (math-arctan-raw '(float 2 -1)))
		  (math-mul-float '(float -4 0)
				  (math-arctan-raw
				   (math-float '(frac 1 239))))))

(math-defcache math-two-pi nil
  (math-mul-float (math-pi) '(float 2 0)))

(math-defcache math-pi-over-2 nil
  (math-mul-float (math-pi) '(float 5 -1)))

(math-defcache math-pi-over-4 nil
  (math-mul-float (math-pi) '(float 25 -2)))

(math-defcache math-pi-over-180 nil
  (math-div-float (math-pi) '(float 18 1)))

(math-defcache math-sqrt-pi nil
  (math-sqrt-float (math-pi)))

(math-defcache math-sqrt-2 nil
  (math-sqrt-float '(float 2 0)))

(math-defcache math-sqrt-12 nil
  (math-sqrt-float '(float 12 0)))

(math-defcache math-sqrt-two-pi nil
  (math-sqrt-float (math-two-pi)))

(defconst math-approx-sqrt-e
  (math-read-number-simple "1.648721270700128146849")
  "An approximation for sqrt(3).")

(math-defcache math-sqrt-e math-approx-sqrt-e
  (math-add-float '(float 1 0) (math-exp-minus-1-raw '(float 5 -1))))

(math-defcache math-e nil
  (math-pow (math-sqrt-e) 2))

(math-defcache math-phi nil
  (math-mul-float (math-add-float (math-sqrt-raw '(float 5 0)) '(float 1 0))
		  '(float 5 -1)))

(defconst math-approx-gamma-const
  (math-read-number-simple
   "0.5772156649015328606065120900824024310421593359399235988057672348848677267776646709369470632917467495")
  "An approximation for gamma.")

(math-defcache math-gamma-const nil
  math-approx-gamma-const)

(defun math-half-circle (symb)
  (if (eq calc-angle-mode 'rad)
      (if symb
	  '(var pi var-pi)
	(math-pi))
    180))

(defun math-full-circle (symb)
  (math-mul 2 (math-half-circle symb)))

(defun math-quarter-circle (symb)
  (math-div (math-half-circle symb) 2))

(defvar math-expand-formulas nil)

;;;; Miscellaneous math routines.

;;; True if A is an odd integer.  [P R R] [Public]
(defun math-oddp (a)
  (if (consp a)
      (and (memq (car a) '(bigpos bigneg))
	   (= (% (nth 1 a) 2) 1))
    (/= (% a 2) 0)))

;;; True if A is a small or big integer.  [P x] [Public]
(defun math-integerp (a)
  (or (integerp a)
      (memq (car-safe a) '(bigpos bigneg))))

;;; True if A is (numerically) a non-negative integer.  [P N] [Public]
(defun math-natnump (a)
  (or (natnump a)
      (eq (car-safe a) 'bigpos)))

;;; True if A is a rational (or integer).  [P x] [Public]
(defun math-ratp (a)
  (or (integerp a)
      (memq (car-safe a) '(bigpos bigneg frac))))

;;; True if A is a real (or rational).  [P x] [Public]
(defun math-realp (a)
  (or (integerp a)
      (memq (car-safe a) '(bigpos bigneg frac float))))

;;; True if A is a real or HMS form.  [P x] [Public]
(defun math-anglep (a)
  (or (integerp a)
      (memq (car-safe a) '(bigpos bigneg frac float hms))))

;;; True if A is a number of any kind.  [P x] [Public]
(defun math-numberp (a)
  (or (integerp a)
      (memq (car-safe a) '(bigpos bigneg frac float cplx polar))))

;;; True if A is a complex number or angle.  [P x] [Public]
(defun math-scalarp (a)
  (or (integerp a)
      (memq (car-safe a) '(bigpos bigneg frac float cplx polar hms))))

;;; True if A is a vector.  [P x] [Public]
(defun math-vectorp (a)
  (eq (car-safe a) 'vec))

;;; True if A is any vector or scalar data object.  [P x]
(defun math-objvecp (a)    ;  [Public]
  (or (integerp a)
      (memq (car-safe a) '(bigpos bigneg frac float cplx polar
				  hms date sdev intv mod vec incomplete))))

;;; True if A is an object not composed of sub-formulas .  [P x] [Public]
(defun math-primp (a)
  (or (integerp a)
      (memq (car-safe a) '(bigpos bigneg frac float cplx polar
				  hms date mod var))))

;;; True if A is numerically (but not literally) an integer.  [P x] [Public]
(defun math-messy-integerp (a)
  (cond
   ((eq (car-safe a) 'float) (>= (nth 2 a) 0))
   ((eq (car-safe a) 'frac) (Math-integerp (math-normalize a)))))

;;; True if A is numerically an integer.  [P x] [Public]
(defun math-num-integerp (a)
  (or (Math-integerp a)
      (Math-messy-integerp a)))

;;; True if A is (numerically) a non-negative integer.  [P N] [Public]
(defun math-num-natnump (a)
  (or (natnump a)
      (eq (car-safe a) 'bigpos)
      (and (eq (car-safe a) 'float)
	   (Math-natnump (nth 1 a))
	   (>= (nth 2 a) 0))))

;;; True if A is an integer or will evaluate to an integer.  [P x] [Public]
(defun math-provably-integerp (a)
  (or (Math-integerp a)
      (and (memq (car-safe a) '(calcFunc-trunc
				calcFunc-round
				calcFunc-rounde
				calcFunc-roundu
				calcFunc-floor
				calcFunc-ceil))
	   (= (length a) 2))))

;;; True if A is a real or will evaluate to a real.  [P x] [Public]
(defun math-provably-realp (a)
  (or (Math-realp a)
      (math-provably-integerp a)
      (memq (car-safe a) '(abs arg))))

;;; True if A is a non-real, complex number.  [P x] [Public]
(defun math-complexp (a)
  (memq (car-safe a) '(cplx polar)))

;;; True if A is a non-real, rectangular complex number.  [P x] [Public]
(defun math-rect-complexp (a)
  (eq (car-safe a) 'cplx))

;;; True if A is a non-real, polar complex number.  [P x] [Public]
(defun math-polar-complexp (a)
  (eq (car-safe a) 'polar))

;;; True if A is a matrix.  [P x] [Public]
(defun math-matrixp (a)
  (and (Math-vectorp a)
       (Math-vectorp (nth 1 a))
       (cdr (nth 1 a))
       (let ((len (length (nth 1 a))))
	 (setq a (cdr a))
	 (while (and (setq a (cdr a))
		     (Math-vectorp (car a))
		     (= (length (car a)) len)))
	 (null a))))

(defun math-matrixp-step (a len)   ; [P L]
  (or (null a)
      (and (Math-vectorp (car a))
	   (= (length (car a)) len)
	   (math-matrixp-step (cdr a) len))))

;;; True if A is a square matrix.  [P V] [Public]
(defun math-square-matrixp (a)
  (let ((dims (math-mat-dimens a)))
    (and (cdr dims)
	 (= (car dims) (nth 1 dims)))))

;;; True if MAT is an identity matrix.
(defun math-identity-matrix-p (mat &optional mul)
  (if (math-square-matrixp mat)
      (let ((a (if mul
                   (nth 1 (nth 1 mat))
                 1))
            (n (1- (length mat)))
            (i 1))
        (while (and (<= i n)
                    (math-ident-row-p (nth i mat) i a))
          (setq i (1+ i)))
        (if (> i n)
            a
          nil))))

(defun math-ident-row-p (row n &optional a)
  (unless a
    (setq a 1))
  (and
   (not (memq nil (mapcar
                   (lambda (x) (eq x 0))
                   (nthcdr (1+ n) row))))
   (not (memq nil (mapcar
                   (lambda (x) (eq x 0))
                   (butlast
                    (cdr row)
                    (- (length row) n)))))
   (eq (elt row n) a)))

;;; True if A is any scalar data object.  [P x]
(defun math-objectp (a)    ;  [Public]
  (or (integerp a)
      (memq (car-safe a) '(bigpos bigneg frac float cplx
				  polar hms date sdev intv mod))))

;;; Verify that A is an integer and return A in integer form.  [I N; - x]
(defun math-check-integer (a)   ;  [Public]
  (cond ((integerp a) a)  ; for speed
	((math-integerp a) a)
	((math-messy-integerp a)
	 (math-trunc a))
	(t (math-reject-arg a 'integerp))))

;;; Verify that A is a small integer and return A in integer form.  [S N; - x]
(defun math-check-fixnum (a &optional allow-inf)   ;  [Public]
  (cond ((integerp a) a)  ; for speed
	((Math-num-integerp a)
	 (let ((a (math-trunc a)))
	   (if (integerp a)
	       a
	     (if (or (Math-lessp (lsh -1 -1) a)
		     (Math-lessp a (- (lsh -1 -1))))
		 (math-reject-arg a 'fixnump)
	       (math-fixnum a)))))
	((and allow-inf (equal a '(var inf var-inf)))
	 (lsh -1 -1))
	((and allow-inf (equal a '(neg (var inf var-inf))))
	 (- (lsh -1 -1)))
	(t (math-reject-arg a 'fixnump))))

;;; Verify that A is an integer >= 0 and return A in integer form.  [I N; - x]
(defun math-check-natnum (a)    ;  [Public]
  (cond ((natnump a) a)
	((and (not (math-negp a))
	      (Math-num-integerp a))
	 (math-trunc a))
	(t (math-reject-arg a 'natnump))))

;;; Verify that A is in floating-point form, or force it to be a float.  [F N]
(defun math-check-float (a)    ; [Public]
  (cond ((eq (car-safe a) 'float) a)
	((Math-vectorp a) (math-map-vec 'math-check-float a))
	((Math-objectp a) (math-float a))
	(t a)))

;;; Verify that A is a constant.
(defun math-check-const (a &optional exp-ok)
  (if (or (math-constp a)
	  (and exp-ok math-expand-formulas))
      a
    (math-reject-arg a 'constp)))

;;; Some functions for working with error forms.
(defun math-get-value (x)
  "Get the mean value of the error form X.
If X is not an error form, return X."
  (if (eq (car-safe x) 'sdev)
      (nth 1 x)
    x))

(defun math-get-sdev (x &optional one)
  "Get the standard deviation of the error form X.
If X is not an error form, return 1."
  (if (eq (car-safe x) 'sdev)
      (nth 2 x)
    (if one 1 0)))

(defun math-contains-sdev-p (ls)
  "Non-nil if the list LS contains an error form."
  (let ((ls (if (eq (car-safe ls) 'vec) (cdr ls) ls)))
    (memq t (mapcar (lambda (x) (eq (car-safe x) 'sdev)) ls))))

;;; Coerce integer A to be a small integer.  [S I]
(defun math-fixnum (a)
  (if (consp a)
      (if (cdr a)
	  (if (eq (car a) 'bigneg)
	      (- (math-fixnum-big (cdr a)))
	    (math-fixnum-big (cdr a)))
	0)
    a))

(defun math-fixnum-big (a)
  (if (cdr a)
      (+ (car a) (* (math-fixnum-big (cdr a)) math-bignum-digit-size))
    (car a)))

(defvar math-simplify-only nil)

(defun math-normalize-fancy (a)
  (cond ((eq (car a) 'frac)
	 (math-make-frac (math-normalize (nth 1 a))
			 (math-normalize (nth 2 a))))
	((eq (car a) 'cplx)
	 (let ((real (math-normalize (nth 1 a)))
	       (imag (math-normalize (nth 2 a))))
	   (if (and (math-zerop imag)
		    (not math-simplify-only))   ; oh, what a kludge!
	       real
	     (list 'cplx real imag))))
	((eq (car a) 'polar)
	 (math-normalize-polar a))
	((eq (car a) 'hms)
	 (math-normalize-hms a))
	((eq (car a) 'date)
	 (list 'date (math-normalize (nth 1 a))))
	((eq (car a) 'mod)
	 (math-normalize-mod a))
	((eq (car a) 'sdev)
	 (let ((x (math-normalize (nth 1 a)))
	       (s (math-normalize (nth 2 a))))
	   (if (or (and (Math-objectp x) (not (Math-scalarp x)))
		   (and (Math-objectp s) (not (Math-scalarp s))))
	       (list 'calcFunc-sdev x s)
	     (math-make-sdev x s))))
	((eq (car a) 'intv)
	 (let ((mask (math-normalize (nth 1 a)))
	       (lo (math-normalize (nth 2 a)))
	       (hi (math-normalize (nth 3 a))))
	   (if (if (eq (car-safe lo) 'date)
		   (not (eq (car-safe hi) 'date))
		 (or (and (Math-objectp lo) (not (Math-anglep lo)))
		     (and (Math-objectp hi) (not (Math-anglep hi)))))
	       (list 'calcFunc-intv mask lo hi)
	     (math-make-intv mask lo hi))))
	((eq (car a) 'vec)
	 (cons 'vec (mapcar 'math-normalize (cdr a))))
	((eq (car a) 'quote)
	 (math-normalize (nth 1 a)))
	((eq (car a) 'special-const)
	 (calc-with-default-simplification
	  (math-normalize (nth 1 a))))
	((eq (car a) 'var)
	 (cons 'var (cdr a)))   ; need to re-cons for selection routines
	((eq (car a) 'calcFunc-if)
	 (math-normalize-logical-op a))
	((memq (car a) '(calcFunc-lambda calcFunc-quote calcFunc-condition))
	 (let ((calc-simplify-mode 'none))
	   (cons (car a) (mapcar 'math-normalize (cdr a)))))
	((eq (car a) 'calcFunc-evalto)
	 (setq a (or (nth 1 a) 0))
	 (or calc-refreshing-evaltos
	     (setq a (let ((calc-simplify-mode 'none)) (math-normalize a))))
	 (let ((b (if (and (eq (car-safe a) 'calcFunc-assign)
			   (= (length a) 3))
		      (nth 2 a)
		    a)))
	   (list 'calcFunc-evalto
		 a
		 (if (eq calc-simplify-mode 'none)
		     (math-normalize b)
		   (calc-with-default-simplification
		    (math-evaluate-expr b))))))
	((or (integerp (car a)) (consp (car a)))
	 (if (null (cdr a))
	     (math-normalize (car a))
	   (error "Can't use multi-valued function in an expression")))))

;; The variable math-normalize-a is local to math-normalize in calc.el,
;; but is used by math-normalize-nonstandard, which is called by
;; math-normalize.
(defvar math-normalize-a)

(defun math-normalize-nonstandard ()
  (if (consp calc-simplify-mode)
      (progn
	(setq calc-simplify-mode 'none
	      math-simplify-only (car-safe (cdr-safe math-normalize-a)))
	nil)
    (and (symbolp (car math-normalize-a))
	 (or (eq calc-simplify-mode 'none)
	     (and (eq calc-simplify-mode 'num)
		  (let ((aptr (setq math-normalize-a
                                    (cons
                                     (car math-normalize-a)
                                     (mapcar 'math-normalize
                                             (cdr math-normalize-a))))))
		    (while (and aptr (math-constp (car aptr)))
		      (setq aptr (cdr aptr)))
		    aptr)))
	 (cons (car math-normalize-a)
               (mapcar 'math-normalize (cdr math-normalize-a))))))


;;; Normalize a bignum digit list by trimming high-end zeros.  [L l]
(defun math-norm-bignum (a)
  (let ((digs a) (last nil))
    (while digs
      (or (eq (car digs) 0) (setq last digs))
      (setq digs (cdr digs)))
    (and last
	 (progn
	   (setcdr last nil)
	   a))))

(defun math-bignum-test (a)   ; [B N; B s; b b]
  (if (consp a)
      a
    (math-bignum a)))


;;; Return 0 for zero, -1 for negative, 1 for positive.  [S n] [Public]
(defun calcFunc-sign (a &optional x)
  (let ((signs (math-possible-signs a)))
    (cond ((eq signs 4) (or x 1))
	  ((eq signs 2) 0)
	  ((eq signs 1) (if x (math-neg x) -1))
	  ((math-looks-negp a) (math-neg (calcFunc-sign (math-neg a))))
	  (t (calc-record-why 'realp a)
	     (if x
		 (list 'calcFunc-sign a x)
	       (list 'calcFunc-sign a))))))

;;; Return 0 if A is numerically equal to B, <0 if less, >0 if more.
;;; Arguments must be normalized!  [S N N]
(defun math-compare (a b)
  (cond ((equal a b)
	 (if (and (consp a)
		  (memq (car a) '(var neg * /))
		  (math-infinitep a))
	     2
	   0))
	((and (integerp a) (Math-integerp b))
	 (if (consp b)
	     (if (eq (car b) 'bigpos) -1 1)
	   (if (< a b) -1 1)))
	((and (eq (car-safe a) 'bigpos) (Math-integerp b))
	 (if (eq (car-safe b) 'bigpos)
	     (math-compare-bignum (cdr a) (cdr b))
	   1))
	((and (eq (car-safe a) 'bigneg) (Math-integerp b))
	 (if (eq (car-safe b) 'bigneg)
	     (math-compare-bignum (cdr b) (cdr a))
	   -1))
	((eq (car-safe a) 'frac)
	 (if (eq (car-safe b) 'frac)
	     (math-compare (math-mul (nth 1 a) (nth 2 b))
			   (math-mul (nth 1 b) (nth 2 a)))
	   (math-compare (nth 1 a) (math-mul b (nth 2 a)))))
	((eq (car-safe b) 'frac)
	 (math-compare (math-mul a (nth 2 b)) (nth 1 b)))
	((and (eq (car-safe a) 'float) (eq (car-safe b) 'float))
	 (if (math-lessp-float a b) -1 1))
	((and (eq (car-safe a) 'date) (eq (car-safe b) 'date))
	 (math-compare (nth 1 a) (nth 1 b)))
	((and (or (Math-anglep a)
		  (and (eq (car a) 'cplx) (eq (nth 2 a) 0)))
	      (or (Math-anglep b)
		  (and (eq (car b) 'cplx) (eq (nth 2 b) 0))))
	 (calcFunc-sign (math-add a (math-neg b))))
	((and (eq (car-safe a) 'intv)
	      (or (Math-anglep b) (eq (car-safe b) 'date)))
	 (let ((res (math-compare (nth 2 a) b)))
	   (cond ((eq res 1) 1)
		 ((and (eq res 0) (memq (nth 1 a) '(0 1))) 1)
		 ((eq (setq res (math-compare (nth 3 a) b)) -1) -1)
		 ((and (eq res 0) (memq (nth 1 a) '(0 2))) -1)
		 (t 2))))
	((and (eq (car-safe b) 'intv)
	      (or (Math-anglep a) (eq (car-safe a) 'date)))
	 (let ((res (math-compare a (nth 2 b))))
	   (cond ((eq res -1) -1)
		 ((and (eq res 0) (memq (nth 1 b) '(0 1))) -1)
		 ((eq (setq res (math-compare a (nth 3 b))) 1) 1)
		 ((and (eq res 0) (memq (nth 1 b) '(0 2))) 1)
		 (t 2))))
	((and (eq (car-safe a) 'intv) (eq (car-safe b) 'intv))
	 (let ((res (math-compare (nth 3 a) (nth 2 b))))
	   (cond ((eq res -1) -1)
		 ((and (eq res 0) (or (memq (nth 1 a) '(0 2))
				      (memq (nth 1 b) '(0 1)))) -1)
		 ((eq (setq res (math-compare (nth 2 a) (nth 3 b))) 1) 1)
		 ((and (eq res 0) (or (memq (nth 1 a) '(0 1))
				      (memq (nth 1 b) '(0 2)))) 1)
		 (t 2))))
	((math-infinitep a)
	 (if (or (equal a '(var uinf var-uinf))
		 (equal a '(var nan var-nan)))
	     2
	   (let ((dira (math-infinite-dir a)))
	     (if (math-infinitep b)
		 (if (or (equal b '(var uinf var-uinf))
			 (equal b '(var nan var-nan)))
		     2
		   (let ((dirb (math-infinite-dir b)))
		     (cond ((and (eq dira 1) (eq dirb -1)) 1)
			   ((and (eq dira -1) (eq dirb 1)) -1)
			   (t 2))))
	       (cond ((eq dira 1) 1)
		     ((eq dira -1) -1)
		     (t 2))))))
	((math-infinitep b)
	 (if (or (equal b '(var uinf var-uinf))
		 (equal b '(var nan var-nan)))
	     2
	   (let ((dirb (math-infinite-dir b)))
	     (cond ((eq dirb 1) -1)
		   ((eq dirb -1) 1)
		   (t 2)))))
	((and (eq (car-safe a) 'calcFunc-exp)
	      (eq (car-safe b) '^)
	      (equal (nth 1 b) '(var e var-e)))
	 (math-compare (nth 1 a) (nth 2 b)))
	((and (eq (car-safe b) 'calcFunc-exp)
	      (eq (car-safe a) '^)
	      (equal (nth 1 a) '(var e var-e)))
	 (math-compare (nth 2 a) (nth 1 b)))
	((or (and (eq (car-safe a) 'calcFunc-sqrt)
		  (eq (car-safe b) '^)
		  (or (equal (nth 2 b) '(frac 1 2))
		      (equal (nth 2 b) '(float 5 -1))))
	     (and (eq (car-safe b) 'calcFunc-sqrt)
		  (eq (car-safe a) '^)
		  (or (equal (nth 2 a) '(frac 1 2))
		      (equal (nth 2 a) '(float 5 -1)))))
	 (math-compare (nth 1 a) (nth 1 b)))
	((eq (car-safe a) 'var)
	 2)
	(t
	 (if (and (consp a) (consp b)
		  (eq (car a) (car b))
		  (math-compare-lists (cdr a) (cdr b)))
	     0
	   2))))

;;; Compare two bignum digit lists, return -1 for A<B, 0 for A=B, 1 for A>B.
(defun math-compare-bignum (a b)   ; [S l l]
  (let ((res 0))
    (while (and a b)
      (if (< (car a) (car b))
	  (setq res -1)
	(if (> (car a) (car b))
	    (setq res 1)))
      (setq a (cdr a)
	    b (cdr b)))
    (if a
	(progn
	  (while (eq (car a) 0) (setq a (cdr a)))
	  (if a 1 res))
      (while (eq (car b) 0) (setq b (cdr b)))
      (if b -1 res))))

(defun math-compare-lists (a b)
  (cond ((null a) (null b))
	((null b) nil)
	(t (and (Math-equal (car a) (car b))
		(math-compare-lists (cdr a) (cdr b))))))

(defun math-lessp-float (a b)   ; [P F F]
  (let ((ediff (- (nth 2 a) (nth 2 b))))
    (if (>= ediff 0)
	(if (>= ediff (+ calc-internal-prec calc-internal-prec))
	    (if (eq (nth 1 a) 0)
		(Math-integer-posp (nth 1 b))
	      (Math-integer-negp (nth 1 a)))
	  (Math-lessp (math-scale-int (nth 1 a) ediff)
		      (nth 1 b)))
      (if (>= (setq ediff (- ediff))
	      (+ calc-internal-prec calc-internal-prec))
	  (if (eq (nth 1 b) 0)
	      (Math-integer-negp (nth 1 a))
	    (Math-integer-posp (nth 1 b)))
	(Math-lessp (nth 1 a)
		    (math-scale-int (nth 1 b) ediff))))))

;;; True if A is numerically equal to B.  [P N N] [Public]
(defun math-equal (a b)
  (= (math-compare a b) 0))

;;; True if A is numerically less than B.  [P R R] [Public]
(defun math-lessp (a b)
  (= (math-compare a b) -1))

;;; True if A is numerically equal to the integer B.  [P N S] [Public]
;;; B must not be a multiple of 10.
(defun math-equal-int (a b)
  (or (eq a b)
      (and (eq (car-safe a) 'float)
	   (eq (nth 1 a) b)
	   (= (nth 2 a) 0))))




;;; Return the dimensions of a matrix as a list.  [l x] [Public]
(defun math-mat-dimens (m)
  (if (math-vectorp m)
      (if (math-matrixp m)
	  (cons (1- (length m))
		(math-mat-dimens (nth 1 m)))
	(list (1- (length m))))
    nil))



(defun calc-binary-op-fancy (name func arg ident unary)
  (let ((n (prefix-numeric-value arg)))
    (cond ((> n 1)
	   (calc-enter-result n
			      name
			      (list 'calcFunc-reduce
				    (math-calcFunc-to-var func)
				    (cons 'vec (calc-top-list-n n)))))
	  ((= n 1)
	   (if unary
	       (calc-enter-result 1 name (list unary (calc-top-n 1)))))
	  ((= n 0)
	   (if ident
	       (calc-enter-result 0 name ident)
	     (error "Argument must be nonzero")))
	  (t
	   (let ((rhs (calc-top-n 1)))
	     (calc-enter-result (- 1 n)
				name
				(mapcar (function
					 (lambda (x)
					   (list func x rhs)))
					(calc-top-list-n (- n) 2))))))))

(defun calc-unary-op-fancy (name func arg)
  (let ((n (prefix-numeric-value arg)))
    (if (= n 0) (setq n (calc-stack-size)))
    (cond ((> n 0)
	   (calc-enter-result n
			      name
			      (mapcar (function
				       (lambda (x)
					 (list func x)))
				      (calc-top-list-n n))))
	  ((< n 0)
	   (calc-enter-result 1
			      name
			      (list func (calc-top-n (- n)))
			      (- n))))))

(defvar var-Holidays '(vec (var sat var-sat) (var sun var-sun)))
(defvar var-Decls (list 'vec))


(defun math-inexact-result ()
  (and calc-symbolic-mode
       (signal 'inexact-result nil)))

(defun math-overflow (&optional exp)
  (if (and exp (math-negp exp))
      (math-underflow)
    (signal 'math-overflow nil)))

(defun math-underflow ()
  (signal 'math-underflow nil))

;;; Compute the greatest common divisor of A and B.   [I I I] [Public]
(defun math-gcd (a b)
  (cond ((not (or (consp a) (consp b)))
	 (if (< a 0) (setq a (- a)))
	 (if (< b 0) (setq b (- b)))
	 (let (c)
	   (if (< a b)
	       (setq c b b a a c))
	   (while (> b 0)
	     (setq c b
		   b (% a b)
		   a c))
	   a))
	((eq a 0) b)
	((eq b 0) a)
	(t
	 (if (Math-integer-negp a) (setq a (math-neg a)))
	 (if (Math-integer-negp b) (setq b (math-neg b)))
	 (let (c)
	   (if (Math-natnum-lessp a b)
	       (setq c b b a a c))
	   (while (and (consp a) (not (eq b 0)))
	     (setq c b
		   b (math-imod a b)
		   a c))
	   (while (> b 0)
	     (setq c b
		   b (% a b)
		   a c))
	   a))))


;;;; Algebra.

;;; Evaluate variables in an expression.
(defun math-evaluate-expr (x)  ; [Public]
  (if calc-embedded-info
      (calc-embedded-evaluate-expr x)
    (calc-normalize (math-evaluate-expr-rec x))))

(defalias 'calcFunc-evalv 'math-evaluate-expr)

(defun calcFunc-evalvn (x &optional prec)
  (if prec
      (progn
	(or (math-num-integerp prec)
	    (if (and (math-vectorp prec)
		     (= (length prec) 2)
		     (math-num-integerp (nth 1 prec)))
		(setq prec (math-add (nth 1 prec) calc-internal-prec))
	      (math-reject-arg prec 'integerp)))
	(setq prec (math-trunc prec))
	(if (< prec 3) (setq prec 3))
	(if (> prec calc-internal-prec)
	    (math-normalize
	     (let ((calc-internal-prec prec))
	       (calcFunc-evalvn x)))
	  (let ((calc-internal-prec prec))
	    (calcFunc-evalvn x))))
    (let ((calc-symbolic-mode nil))
      (math-evaluate-expr x))))

(defun math-evaluate-expr-rec (x)
  (if (consp x)
      (if (memq (car x) '(calcFunc-quote calcFunc-condition
					 calcFunc-evalto calcFunc-assign))
	  (if (and (eq (car x) 'calcFunc-assign)
		   (= (length x) 3))
	      (list (car x) (nth 1 x) (math-evaluate-expr-rec (nth 2 x)))
	    x)
	(if (eq (car x) 'var)
	    (if (and (calc-var-value (nth 2 x))
		     (not (eq (car-safe (symbol-value (nth 2 x)))
			      'incomplete)))
		(let ((val (symbol-value (nth 2 x))))
		  (if (eq (car-safe val) 'special-const)
		      (if calc-symbolic-mode
			  x
			val)
		    val))
	      x)
	  (if (Math-primp x)
	      x
	    (cons (car x) (mapcar 'math-evaluate-expr-rec (cdr x))))))
    x))

(defun math-any-floats (expr)
  (if (Math-primp expr)
      (math-floatp expr)
    (while (and (setq expr (cdr expr)) (not (math-any-floats (car expr)))))
    expr))

(defvar var-FactorRules 'calc-FactorRules)

(defvar math-mt-many nil)
(defvar math-mt-func nil)

(defun math-map-tree (math-mt-func mmt-expr &optional math-mt-many)
  (or math-mt-many (setq math-mt-many 1000000))
  (math-map-tree-rec mmt-expr))

(defun math-map-tree-rec (mmt-expr)
  (or (= math-mt-many 0)
      (let ((mmt-done nil)
	    mmt-nextval)
	(while (not mmt-done)
	  (while (and (/= math-mt-many 0)
		      (setq mmt-nextval (funcall math-mt-func mmt-expr))
		      (not (equal mmt-expr mmt-nextval)))
	    (setq mmt-expr mmt-nextval
		  math-mt-many (if (> math-mt-many 0)
                                   (1- math-mt-many)
                                 (1+ math-mt-many))))
	  (if (or (Math-primp mmt-expr)
		  (<= math-mt-many 0))
	      (setq mmt-done t)
	    (setq mmt-nextval (cons (car mmt-expr)
				    (mapcar 'math-map-tree-rec
					    (cdr mmt-expr))))
	    (if (equal mmt-nextval mmt-expr)
		(setq mmt-done t)
	      (setq mmt-expr mmt-nextval))))))
  mmt-expr)

(defun math-is-true (expr)
  (if (Math-numberp expr)
      (not (Math-zerop expr))
    (math-known-nonzerop expr)))

(defun math-const-var (expr)
  (and (consp expr)
       (eq (car expr) 'var)
       (or (and (symbolp (nth 2 expr))
		(boundp (nth 2 expr))
		(eq (car-safe (symbol-value (nth 2 expr))) 'special-const))
	   (memq (nth 2 expr) '(var-inf var-uinf var-nan)))))

;; The variable math-integral-cache is originally declared in calcalg2.el,
;; but is set by math-defintegral and math-defintegral-2.
(defvar math-integral-cache)

(defmacro math-defintegral (funcs &rest code)
  (setq math-integral-cache nil)
  (cons 'progn
        (mapcar #'(lambda (func)
                    `(put ',func 'math-integral
                          (nconc
                           (get ',func 'math-integral)
                           (list
                            #'(lambda (u) ,@code)))))
                (if (symbolp funcs) (list funcs) funcs))))
(put 'math-defintegral 'lisp-indent-hook 1)

(defmacro math-defintegral-2 (funcs &rest code)
  (setq math-integral-cache nil)
  (cons 'progn
        (mapcar #'(lambda (func)
                    `(put ',func 'math-integral-2
                          (nconc
                            (get ',func 'math-integral-2)
                            (list #'(lambda (u v) ,@code)))))
                (if (symbolp funcs) (list funcs) funcs))))
(put 'math-defintegral-2 'lisp-indent-hook 1)

(defvar var-IntegAfterRules 'calc-IntegAfterRules)

(defvar var-FitRules 'calc-FitRules)

(defvar math-poly-base-variable nil)
(defvar math-poly-neg-powers nil)
(defvar math-poly-mult-powers 1)
(defvar math-poly-frac-powers nil)
(defvar math-poly-exp-base nil)

(defun math-build-var-name (name)
  (if (stringp name)
      (setq name (intern name)))
  (if (string-match "\\`var-." (symbol-name name))
      (list 'var (intern (substring (symbol-name name) 4)) name)
    (list 'var name (intern (concat "var-" (symbol-name name))))))

(defvar math-simplifying-units nil)
(defvar math-combining-units t)

;;; Nontrivial number parsing.

(defun math-read-number-fancy (s)
  (cond

   ;; Integer+fractions
   ((string-match "^\\([0-9]*\\)[:/]\\([0-9]*\\)[:/]\\([0-9]*\\)$" s)
    (let ((int (math-match-substring s 1))
	  (num (math-match-substring s 2))
	  (den (math-match-substring s 3)))
      (let ((int (if (> (length int) 0) (math-read-number int) 0))
	    (num (if (> (length num) 0) (math-read-number num) 1))
	    (den (if (> (length num) 0) (math-read-number den) 1)))
	(and int num den
	     (math-integerp int) (math-integerp num) (math-integerp den)
	     (not (math-zerop den))
	     (list 'frac (math-add num (math-mul int den)) den)))))

   ;; Fractions
   ((string-match "^\\([0-9]*\\)[:/]\\([0-9]*\\)$" s)
    (let ((num (math-match-substring s 1))
	  (den (math-match-substring s 2)))
      (let ((num (if (> (length num) 0) (math-read-number num) 1))
	    (den (if (> (length num) 0) (math-read-number den) 1)))
	(and num den (math-integerp num) (math-integerp den)
	     (not (math-zerop den))
	     (list 'frac num den)))))

   ;; Modulo forms
   ((string-match "^\\(.*\\) *mod *\\(.*\\)$" s)
    (let* ((n (math-match-substring s 1))
	   (m (math-match-substring s 2))
	   (n (math-read-number n))
	   (m (math-read-number m)))
      (and n m (math-anglep n) (math-anglep m)
	   (list 'mod n m))))

   ;; Error forms
   ((string-match "^\\(.*\\) *\\+/- *\\(.*\\)$" s)
    (let* ((x (math-match-substring s 1))
	   (sigma (math-match-substring s 2))
	   (x (math-read-number x))
	   (sigma (math-read-number sigma)))
      (and x sigma (math-scalarp x) (math-anglep sigma)
	   (list 'sdev x sigma))))

   ;; Hours (or degrees)
   ((or (string-match "^\\([^#^]+\\)[@oOhH]\\(.*\\)$" s)
	(string-match "^\\([^#^]+\\)[dD][eE]?[gG]?\\(.*\\)$" s))
    (let* ((hours (math-match-substring s 1))
	   (minsec (math-match-substring s 2))
	   (hours (math-read-number hours))
	   (minsec (if (> (length minsec) 0) (math-read-number minsec) 0)))
      (and hours minsec
	   (math-num-integerp hours)
	   (not (math-negp hours)) (not (math-negp minsec))
	   (cond ((math-num-integerp minsec)
		  (and (Math-lessp minsec 60)
		       (list 'hms hours minsec 0)))
		 ((and (eq (car-safe minsec) 'hms)
		       (math-zerop (nth 1 minsec)))
		  (math-add (list 'hms hours 0 0) minsec))
		 (t nil)))))

   ;; Minutes
   ((string-match "^\\([^'#^]+\\)[mM']\\(.*\\)$" s)
    (let* ((minutes (math-match-substring s 1))
	   (seconds (math-match-substring s 2))
	   (minutes (math-read-number minutes))
	   (seconds (if (> (length seconds) 0) (math-read-number seconds) 0)))
      (and minutes seconds
	   (math-num-integerp minutes)
	   (not (math-negp minutes)) (not (math-negp seconds))
	   (cond ((math-realp seconds)
		  (and (Math-lessp minutes 60)
		       (list 'hms 0 minutes seconds)))
		 ((and (eq (car-safe seconds) 'hms)
		       (math-zerop (nth 1 seconds))
		       (math-zerop (nth 2 seconds)))
		  (math-add (list 'hms 0 minutes 0) seconds))
		 (t nil)))))

   ;; Seconds
   ((string-match "^\\([^\"#^]+\\)[sS\"]$" s)
    (let ((seconds (math-read-number (math-match-substring s 1))))
      (and seconds (math-realp seconds)
	   (not (math-negp seconds))
	   (Math-lessp seconds 60)
	   (list 'hms 0 0 seconds))))

   ;; Integer+fraction with explicit radix
   ((string-match "^\\([0-9]+\\)\\(#\\|\\^\\^\\)\\([0-9a-zA-Z]*\\)[:/]\\([0-9a-zA-Z]*\\)[:/]\\([0-9a-zA-Z]\\)$" s)
    (let ((radix (string-to-number (math-match-substring s 1)))
	  (int (math-match-substring s 3))
	  (num (math-match-substring s 4))
	  (den (math-match-substring s 5)))
      (let ((int (if (> (length int) 0) (math-read-radix int radix) 0))
	    (num (if (> (length num) 0) (math-read-radix num radix) 1))
	    (den (if (> (length den) 0) (math-read-radix den radix) 1)))
	(and int num den (not (math-zerop den))
	     (list 'frac
		   (math-add num (math-mul int den))
		   den)))))

   ;; Fraction with explicit radix
   ((string-match "^\\([0-9]+\\)\\(#\\|\\^\\^\\)\\([0-9a-zA-Z]*\\)[:/]\\([0-9a-zA-Z]*\\)$" s)
    (let ((radix (string-to-number (math-match-substring s 1)))
	  (num (math-match-substring s 3))
	  (den (math-match-substring s 4)))
      (let ((num (if (> (length num) 0) (math-read-radix num radix) 1))
	    (den (if (> (length den) 0) (math-read-radix den radix) 1)))
	(and num den (not (math-zerop den)) (list 'frac num den)))))

   ;; Float with explicit radix and exponent
   ((or (string-match "^0*\\(\\([2-9]\\|1[0-4]\\)\\(#\\|\\^\\^\\)[0-9a-dA-D.]+\\)[eE]\\([-+]?[0-9]+\\)$" s)
	(string-match "^\\(\\([0-9]+\\)\\(#\\|\\^\\^\\)[0-9a-zA-Z.]+\\) *\\* *\\2\\.? *\\^ *\\([-+]?[0-9]+\\)$" s))
    (let ((radix (string-to-number (math-match-substring s 2)))
	  (mant (math-match-substring s 1))
	  (exp (math-match-substring s 4)))
      (let ((mant (math-read-number mant))
	    (exp (math-read-number exp)))
	(and mant exp
	     (math-mul mant (math-pow (math-float radix) exp))))))

   ;; Float with explicit radix, no exponent
   ((string-match "^\\([0-9]+\\)\\(#\\|\\^\\^\\)\\([0-9a-zA-Z]*\\)\\.\\([0-9a-zA-Z]*\\)$" s)
    (let ((radix (string-to-number (math-match-substring s 1)))
	  (int (math-match-substring s 3))
	  (fracs (math-match-substring s 4)))
      (let ((int (if (> (length int) 0) (math-read-radix int radix) 0))
	    (frac (if (> (length fracs) 0) (math-read-radix fracs radix) 0))
	    (calc-prefer-frac nil))
	(and int frac
	     (math-add int (math-div frac (math-pow radix (length fracs))))))))

   ;; Integer with explicit radix
   ((string-match "^\\([0-9]+\\)\\(#&?\\|\\^\\^\\)\\([0-9a-zA-Z]+\\)$" s)
    (math-read-radix (math-match-substring s 3)
		     (string-to-number (math-match-substring s 1))))

   ;; Two's complement with explicit radix
   ((string-match "^\\([0-9]+\\)\\(##\\)\\([0-9a-zA-Z]+\\)$" s)
    (let ((num (math-read-radix (math-match-substring s 3)
                                (string-to-number (math-match-substring s 1)))))
      (if (and
           (Math-lessp num math-2-word-size)
           (<= (math-compare math-half-2-word-size num) 0))
          (math-sub num math-2-word-size)
        num)))

   ;; C language hexadecimal notation
   ((and (eq calc-language 'c)
	 (string-match "^0[xX]\\([0-9a-fA-F]+\\)$" s))
    (let ((digs (math-match-substring s 1)))
      (math-read-radix digs 16)))

   ;; Pascal language hexadecimal notation
   ((and (eq calc-language 'pascal)
	 (string-match "^\\$\\([0-9a-fA-F]+\\)$" s))
    (let ((digs (math-match-substring s 1)))
      (math-read-radix digs 16)))

   ;; Fraction using "/" instead of ":"
   ((string-match "^\\([0-9]+\\)/\\([0-9/]+\\)$" s)
    (math-read-number (concat (math-match-substring s 1) ":"
			      (math-match-substring s 2))))

   ;; Syntax error!
   (t nil)))

(defun math-read-radix (s r)   ; [I X D]
  (setq s (upcase s))
  (let ((i 0)
	(res 0)
	dig)
    (while (and (< i (length s))
		(setq dig (math-read-radix-digit (elt s i)))
		(< dig r))
      (setq res (math-add (math-mul res r) dig)
	    i (1+ i)))
    (and (= i (length s))
	 res)))



;;; Expression parsing.

(defvar math-expr-data)

(defun math-read-expr (math-exp-str)
  (let ((math-exp-pos 0)
	(math-exp-old-pos 0)
	(math-exp-keep-spaces nil)
	math-exp-token math-expr-data)
    (setq math-exp-str (math-read-preprocess-string math-exp-str))
    (while (setq math-exp-token (string-match "\\.\\.\\([^.]\\|.[^.]\\)" math-exp-str))
      (setq math-exp-str (concat (substring math-exp-str 0 math-exp-token) "\\dots"
			    (substring math-exp-str (+ math-exp-token 2)))))
    (math-build-parse-table)
    (math-read-token)
    (let ((val (catch 'syntax (math-read-expr-level 0))))
      (if (stringp val)
	  (list 'error math-exp-old-pos val)
	(if (equal math-exp-token 'end)
	    val
	  (list 'error math-exp-old-pos "Syntax error"))))))

(defun math-read-plain-expr (exp-str &optional error-check)
  (let* ((calc-language nil)
	 (math-expr-opers (math-standard-ops))
	 (val (math-read-expr exp-str)))
    (and error-check
	 (eq (car-safe val) 'error)
	 (error "%s: %s" (nth 2 val) exp-str))
    val))


(defun math-read-string ()
  (let ((str (read-from-string (concat math-expr-data "\""))))
    (or (and (= (cdr str) (1+ (length math-expr-data)))
	     (stringp (car str)))
	(throw 'syntax "Error in string constant"))
    (math-read-token)
    (append '(vec) (car str) nil)))



;;; They said it couldn't be done...

(defun math-read-big-expr (str)
  (and (> (length calc-left-label) 0)
       (string-match (concat "^" (regexp-quote calc-left-label)) str)
       (setq str (concat (substring str 0 (match-beginning 0))
			 (substring str (match-end 0)))))
  (and (> (length calc-right-label) 0)
       (string-match (concat (regexp-quote calc-right-label) " *$") str)
       (setq str (concat (substring str 0 (match-beginning 0))
			 (substring str (match-end 0)))))
  (if (string-match "\\\\[^ \n|]" str)
      (if (eq calc-language 'latex)
	  (math-read-expr str)
	(let ((calc-language 'latex)
	      (calc-language-option nil)
	      (math-expr-opers (get 'latex 'math-oper-table))
	      (math-expr-function-mapping (get 'latex 'math-function-table))
	      (math-expr-variable-mapping (get 'latex 'math-variable-table)))
	  (math-read-expr str)))
    (let ((math-read-big-lines nil)
	  (pos 0)
	  (width 0)
	  (math-read-big-err-msg nil)
	  math-read-big-baseline math-read-big-h2
	  new-pos p)
      (while (setq new-pos (string-match "\n" str pos))
	(setq math-read-big-lines
              (cons (substring str pos new-pos) math-read-big-lines)
	      pos (1+ new-pos)))
      (setq math-read-big-lines
            (nreverse (cons (substring str pos) math-read-big-lines))
	    p math-read-big-lines)
      (while p
	(setq width (max width (length (car p)))
	      p (cdr p)))
      (if (math-read-big-bigp math-read-big-lines)
	  (or (catch 'syntax
		(math-read-big-rec 0 0 width (length math-read-big-lines)))
	      math-read-big-err-msg
	      '(error 0 "Syntax error"))
	(math-read-expr str)))))

(defun math-read-big-bigp (math-read-big-lines)
  (and (cdr math-read-big-lines)
       (let ((matrix nil)
	     (v 0)
	     (height (if (> (length (car math-read-big-lines)) 0) 1 0)))
	 (while (and (cdr math-read-big-lines)
		     (let* ((i 0)
			    j
			    (l1 (car math-read-big-lines))
			    (l2 (nth 1 math-read-big-lines))
			    (len (min (length l1) (length l2))))
		       (if (> (length l2) 0)
			   (setq height (1+ height)))
		       (while (and (< i len)
				   (or (memq (aref l1 i) '(?\  ?\- ?\_))
				       (memq (aref l2 i) '(?\  ?\-))
				       (and (memq (aref l1 i) '(?\| ?\,))
					    (= (aref l2 i) (aref l1 i)))
				       (and (eq (aref l1 i) ?\[)
					    (eq (aref l2 i) ?\[)
					    (let ((math-rb-h2 (length l1)))
					      (setq j (math-read-big-balance
						       (1+ i) v "[")))
					    (setq i (1- j)))))
			 (setq i (1+ i)))
		       (or (= i len)
			   (and (eq (aref l1 i) ?\[)
				(eq (aref l2 i) ?\[)
				(setq matrix t)
				nil))))
	   (setq math-read-big-lines (cdr math-read-big-lines)
		 v (1+ v)))
	 (or (and (> height 1)
		  (not (cdr math-read-big-lines)))
	     matrix))))

;;; Nontrivial "flat" formatting.

(defvar math-format-hash-args nil)
(defvar calc-can-abbrev-vectors nil)

(defun math-format-flat-expr-fancy (a prec)
  (cond
   ((eq (car a) 'incomplete)
    (format "<incomplete %s>" (nth 1 a)))
   ((eq (car a) 'vec)
    (if (or calc-full-trail-vectors (not calc-can-abbrev-vectors)
	    (< (length a) 7))
	(concat "[" (math-format-flat-vector (cdr a) ", "
					     (if (cdr (cdr a)) 0 1000)) "]")
      (concat "["
	      (math-format-flat-expr (nth 1 a) 0) ", "
	      (math-format-flat-expr (nth 2 a) 0) ", "
	      (math-format-flat-expr (nth 3 a) 0) ", ..., "
	      (math-format-flat-expr (nth (1- (length a)) a) 0) "]")))
   ((eq (car a) 'intv)
    (concat (if (memq (nth 1 a) '(0 1)) "(" "[")
	    (math-format-flat-expr (nth 2 a) 1000)
	    " .. "
	    (math-format-flat-expr (nth 3 a) 1000)
	    (if (memq (nth 1 a) '(0 2)) ")" "]")))
   ((eq (car a) 'date)
    (concat "<" (math-format-date a) ">"))
   ((and (eq (car a) 'calcFunc-lambda) (> (length a) 2))
    (let ((p (cdr a))
	  (ap calc-arg-values)
	  (math-format-hash-args (if (= (length a) 3) 1 t)))
      (while (and (cdr p) (equal (car p) (car ap)))
	(setq p (cdr p) ap (cdr ap)))
      (concat "<"
	      (if (cdr p)
		  (concat (math-format-flat-vector
			   (nreverse (cdr (reverse (cdr a)))) ", " 0)
			  " : ")
		"")
	      (math-format-flat-expr (nth (1- (length a)) a) 0)
	      ">")))
   ((eq (car a) 'var)
    (or (and math-format-hash-args
	     (let ((p calc-arg-values) (v 1))
	       (while (and p (not (equal (car p) a)))
		 (setq p (and (eq math-format-hash-args t) (cdr p))
		       v (1+ v)))
	       (and p
		    (if (eq math-format-hash-args 1)
			"#"
		      (format "#%d" v)))))
	(symbol-name (nth 1 a))))
   ((and (memq (car a) '(calcFunc-string calcFunc-bstring))
	 (= (length a) 2)
	 (math-vectorp (nth 1 a))
	 (math-vector-is-string (nth 1 a)))
    (concat (substring (symbol-name (car a)) 9)
	    "(" (math-vector-to-string (nth 1 a) t) ")"))
   (t
    (let ((op (math-assq2 (car a) (math-standard-ops))))
      (cond ((and op (= (length a) 3))
	     (if (> prec (min (nth 2 op) (nth 3 op)))
		 (concat "(" (math-format-flat-expr a 0) ")")
	       (let ((lhs (math-format-flat-expr (nth 1 a) (nth 2 op)))
		     (rhs (math-format-flat-expr (nth 2 a) (nth 3 op))))
		 (setq op (car op))
		 (if (or (equal op "^") (equal op "_"))
		     (if (= (aref lhs 0) ?-)
			 (setq lhs (concat "(" lhs ")")))
		   (setq op (concat " " op " ")))
		 (concat lhs op rhs))))
	    ((eq (car a) 'neg)
	     (concat "-" (math-format-flat-expr (nth 1 a) 1000)))
	    (t
	     (concat (math-remove-dashes
		      (if (string-match "\\`calcFunc-\\([a-zA-Zα-ωΑ-Ω0-9']+\\)\\'"
					(symbol-name (car a)))
			  (math-match-substring (symbol-name (car a)) 1)
			(symbol-name (car a))))
		     "("
		     (math-format-flat-vector (cdr a) ", " 0)
		     ")")))))))

(defun math-format-flat-vector (vec sep prec)
  (if vec
      (let ((buf (math-format-flat-expr (car vec) prec)))
	(while (setq vec (cdr vec))
	  (setq buf (concat buf sep (math-format-flat-expr (car vec) prec))))
	buf)
    ""))

(defun math-format-nice-expr (x w)
  (cond ((and (eq (car-safe x) 'vec)
	      (cdr (cdr x))
	      (let ((ops '(vec calcFunc-assign calcFunc-condition
			       calcFunc-schedule calcFunc-iterations
			       calcFunc-phase)))
		(or (memq (car-safe (nth 1 x)) ops)
		    (memq (car-safe (nth 2 x)) ops)
		    (memq (car-safe (nth 3 x)) ops)
		    calc-break-vectors)))
	 (concat "[ " (math-format-flat-vector (cdr x) ",\n  " 0) " ]"))
	(t
	 (let ((str (math-format-flat-expr x 0))
	       (pos 0) p)
	   (or (string-match "\"" str)
	       (while (<= (setq p (+ pos w)) (length str))
		 (while (and (> (setq p (1- p)) pos)
			     (not (= (aref str p) ? ))))
		 (if (> p (+ pos 5))
		     (setq str (concat (substring str 0 p)
				       "\n "
				       (substring str p))
			   pos (1+ p))
		   (setq pos (+ pos w)))))
	   str))))

(defun math-assq2 (v a)
  (while (and a (not (eq v (nth 1 (car a)))))
    (setq a (cdr a)))
  (car a))

(defun math-format-number-fancy (a prec)
  (cond
   ((eq (car a) 'float)    ; non-decimal radix
    (if (Math-integer-negp (nth 1 a))
	(concat "-" (math-format-number (math-neg a)))
      (let ((str (if (and calc-radix-formatter
			  (not (memq calc-language '(c pascal))))
		     (funcall calc-radix-formatter
			      calc-number-radix
			      (math-format-radix-float a prec))
		   (format "%d#%s" calc-number-radix
			   (math-format-radix-float a prec)))))
	(if (and prec (> prec 191) (string-match "\\*" str))
	    (concat "(" str ")")
	  str))))
   ((eq (car a) 'frac)
    (setq a (math-adjust-fraction a))
    (if (> (length (car calc-frac-format)) 1)
	(if (Math-integer-negp (nth 1 a))
	    (concat "-" (math-format-number (math-neg a)))
	  (let ((q (math-idivmod (nth 1 a) (nth 2 a))))
	    (concat (let ((calc-frac-format nil))
		      (math-format-number (car q)))
		    (substring (car calc-frac-format) 0 1)
		    (let ((math-radix-explicit-format nil)
			  (calc-frac-format nil))
		      (math-format-number (cdr q)))
		    (substring (car calc-frac-format) 1 2)
		    (let ((math-radix-explicit-format nil)
			  (calc-frac-format nil))
		      (math-format-number (nth 2 a))))))
      (concat (let ((calc-frac-format nil))
		(math-format-number (nth 1 a)))
	      (car calc-frac-format)
	      (let ((math-radix-explicit-format nil)
		    (calc-frac-format nil))
		(math-format-number (nth 2 a))))))
   ((eq (car a) 'cplx)
    (if (math-zerop (nth 2 a))
	(math-format-number (nth 1 a))
      (if (null calc-complex-format)
	  (concat "(" (math-format-number (nth 1 a))
		  ", " (math-format-number (nth 2 a)) ")")
	(if (math-zerop (nth 1 a))
	    (if (math-equal-int (nth 2 a) 1)
		(symbol-name calc-complex-format)
	      (if (math-equal-int (nth 2 a) -1)
		  (concat "-" (symbol-name calc-complex-format))
		(if prec
		    (math-compose-expr (list '* (nth 2 a) '(cplx 0 1)) prec)
		  (concat (math-format-number (nth 2 a)) " "
			  (symbol-name calc-complex-format)))))
	  (if prec
	      (math-compose-expr (list (if (math-negp (nth 2 a)) '- '+)
				       (nth 1 a)
				       (list 'cplx 0 (math-abs (nth 2 a))))
				 prec)
	    (concat (math-format-number (nth 1 a))
		    (if (math-negp (nth 2 a)) " - " " + ")
		    (math-format-number
		     (list 'cplx 0 (math-abs (nth 2 a))))))))))
   ((eq (car a) 'polar)
    (concat "(" (math-format-number (nth 1 a))
	    "; " (math-format-number (nth 2 a)) ")"))
   ((eq (car a) 'hms)
    (if (math-negp a)
	(concat "-" (math-format-number (math-neg a)))
      (let ((calc-number-radix 10)
            (calc-twos-complement-mode nil)
	    (calc-leading-zeros nil)
	    (calc-group-digits nil))
	(format calc-hms-format
		(let ((calc-frac-format '(":" nil)))
		  (math-format-number (nth 1 a)))
		(let ((calc-frac-format '(":" nil)))
		  (math-format-number (nth 2 a)))
		(math-format-number (nth 3 a))))))
   ((eq (car a) 'intv)
    (concat (if (memq (nth 1 a) '(0 1)) "(" "[")
	    (math-format-number (nth 2 a))
	    " .. "
	    (math-format-number (nth 3 a))
	    (if (memq (nth 1 a) '(0 2)) ")" "]")))
   ((eq (car a) 'sdev)
    (concat (math-format-number (nth 1 a))
	    " +/- "
	    (math-format-number (nth 2 a))))
   ((eq (car a) 'vec)
    (math-format-flat-expr a 0))
   (t (format "%s" a))))

(defun math-adjust-fraction (a)
  (if (nth 1 calc-frac-format)
      (progn
	(if (Math-integerp a) (setq a (list 'frac a 1)))
	(let ((g (math-quotient (nth 1 calc-frac-format)
				(math-gcd (nth 2 a)
					  (nth 1 calc-frac-format)))))
	  (list 'frac (math-mul (nth 1 a) g) (math-mul (nth 2 a) g))))
    a))

(defun math-format-bignum-fancy (a)   ; [X L]
  (let ((str (cond ((= calc-number-radix 10)
		    (math-format-bignum-decimal a))
		   ((= calc-number-radix 2)
		    (math-format-bignum-binary a))
		   ((= calc-number-radix 8)
		    (math-format-bignum-octal a))
		   ((= calc-number-radix 16)
		    (math-format-bignum-hex a))
		   (t (math-format-bignum-radix a)))))
    (if calc-leading-zeros
	(let* ((calc-internal-prec 6)
	       (digs (math-compute-max-digits (math-abs calc-word-size)
					      calc-number-radix))
	       (len (length str)))
	  (if (< len digs)
	      (setq str (concat (make-string (- digs len) ?0) str)))))
    (if calc-group-digits
	(let ((i (length str))
	      (g (if (integerp calc-group-digits)
		     (math-abs calc-group-digits)
		   (if (memq calc-number-radix '(2 16)) 4 3))))
	  (while (> i g)
	    (setq i (- i g)
		  str (concat (substring str 0 i)
			      calc-group-char
			      (substring str i))))
	  str))
    (if (and (/= calc-number-radix 10)
	     math-radix-explicit-format)
	(if calc-radix-formatter
	    (funcall calc-radix-formatter calc-number-radix str)
	  (format "%d#%s" calc-number-radix str))
      str)))


(defun math-group-float (str)   ; [X X]
  (let* ((pt (or (string-match "[^0-9a-zA-Z]" str) (length str)))
	 (g (if (integerp calc-group-digits) (math-abs calc-group-digits)
              (if (memq calc-number-radix '(2 16)) 4 3)))
	 (i pt))
    (if (and (integerp calc-group-digits) (< calc-group-digits 0))
	(while (< (setq i (+ (1+ i) g)) (length str))
	  (setq str (concat (substring str 0 i)
			    calc-group-char
			    (substring str i))
		i (+ i (1- (length calc-group-char))))))
    (setq i pt)
    (while (> i g)
      (setq i (- i g)
	    str (concat (substring str 0 i)
			calc-group-char
			(substring str i))))
    str))

;;; Users can redefine this in their .emacs files.
(defvar calc-keypad-user-menu nil
  "If non-nil, this describes an additional menu for calc-keypad.
It should contain a list of three rows.
Each row should be a list of six keys.
Each key should be a list of a label string, plus a Calc command name spec.
A command spec is a command name symbol, a keyboard macro string, a
list containing a numeric entry string, or nil.
A key may contain additional specs for Inverse, Hyperbolic, and Inv+Hyp.")

(run-hooks 'calc-ext-load-hook)

(provide 'calc-ext)

;; Local variables:
;; coding: utf-8
;; End:

;;; calc-ext.el ends here
