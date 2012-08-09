;;; calccomp.el --- composition functions for Calc

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

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

;;; A "composition" has one of the following forms:
;;;
;;;    "string"              A literal string
;;;
;;;    (horiz C1 C2 ...)     Horizontally abutted sub-compositions
;;;
;;;    (set LEVEL OFF)       Set left margin + offset for line-break level
;;;    (break LEVEL)         A potential line-break point
;;;
;;;    (vleft N C1 C2 ...)   Vertically stacked, left-justified sub-comps
;;;    (vcent N C1 C2 ...)   Vertically stacked, centered sub-comps
;;;    (vright N C1 C2 ...)  Vertically stacked, right-justified sub-comps
;;;                          N specifies baseline of the stack, 0=top line.
;;;
;;;    (supscr C1 C2)        Composition C1 with superscript C2
;;;    (subscr C1 C2)        Composition C1 with subscript C2
;;;    (rule X)              Horizontal line of X, full width of enclosing comp
;;;
;;;    (tag X C)             Composition C corresponds to sub-expression X

;; math-comp-just and math-comp-comma-spc are local to
;; math-compose-expr, but are used by math-compose-matrix, which is
;; called by math-compose-expr
(defvar math-comp-just)
(defvar math-comp-comma-spc)

;; math-comp-vector-prec is local to math-compose-expr, but is used by
;; math-compose-matrix and math-compose-rows, which are called by
;; math-compose-expr.
(defvar math-comp-vector-prec)

;; math-comp-left-bracket, math-comp-right-bracket and math-comp-comma are
;; local to math-compose-expr, but are used by math-compose-rows, which is
;; called by math-compose-expr.
(defvar math-comp-left-bracket)
(defvar math-comp-right-bracket)
(defvar math-comp-comma)

(defun math-compose-var (a)
  (let (v sn)
    (if (and math-compose-hash-args
             (let ((p calc-arg-values))
               (setq v 1)
               (while (and p (not (equal (car p) a)))
                 (setq p (and (eq math-compose-hash-args t) (cdr p))
                       v (1+ v)))
               p))
        (if (eq math-compose-hash-args 1)
            "#"
          (format "#%d" v))
      (setq sn (symbol-name (nth 1 a)))
      (if (memq calc-language calc-lang-allow-percentsigns)
          (setq sn (math-to-percentsigns sn)))
      (if (memq calc-language calc-lang-allow-underscores)
          (setq sn (math-to-underscores sn)))
      sn)))

(defun math-compose-expr (a prec)
  (let ((math-compose-level (1+ math-compose-level))
        (math-expr-opers (math-expr-ops))
        spfn)
    (cond
     ((or (and (eq a math-comp-selected) a)
	  (and math-comp-tagged
	       (not (eq math-comp-tagged a))))
      (let ((math-comp-selected nil))
	(and math-comp-tagged (setq math-comp-tagged a))
	(list 'tag a (math-compose-expr a prec))))
     ((and (not (consp a)) (not (integerp a)))
      (concat "'" (prin1-to-string a)))
     ((setq spfn (assq (car-safe a)
                       (get calc-language 'math-special-function-table)))
      (setq spfn (cdr spfn))
      (if (consp spfn)
          (funcall (car spfn) a spfn)
        (funcall spfn a)))
     ((math-scalarp a)
      (if (or (eq (car-safe a) 'frac)
	      (and (nth 1 calc-frac-format) (Math-integerp a)))
	  (if (and
               calc-language
               (not (memq calc-language
                          '(flat big unform))))
	      (let ((aa (math-adjust-fraction a))
		    (calc-frac-format nil))
		(math-compose-expr (list '/
					 (if (memq calc-language
                                                   calc-lang-slash-idiv)
					     (math-float (nth 1 aa))
					   (nth 1 aa))
					 (nth 2 aa)) prec))
	    (if (and (eq calc-language 'big)
		     (= (length (car calc-frac-format)) 1))
		(let* ((aa (math-adjust-fraction a))
		       (calc-frac-format nil)
		       (math-radix-explicit-format nil)
		       (c (list 'horiz
				(if (math-negp (nth 1 aa))
				    "- " "")
				(list 'vcent 1
				      (math-format-number
				       (math-abs (nth 1 aa)))
				      '(rule ?-)
				      (math-format-number (nth 2 aa))))))
		  (if (= calc-number-radix 10)
		      c
		    (list 'horiz "(" c
			  (list 'subscr ")"
				(int-to-string calc-number-radix)))))
	      (math-format-number a)))
	(if (not (eq calc-language 'big))
	    (math-format-number a prec)
	  (if (memq (car-safe a) '(cplx polar))
	      (if (math-zerop (nth 2 a))
		  (math-compose-expr (nth 1 a) prec)
		(list 'horiz "("
		      (math-compose-expr (nth 1 a) 0)
		      (if (eq (car a) 'cplx) ", " "; ")
		      (math-compose-expr (nth 2 a) 0) ")"))
	    (if (or (= calc-number-radix 10)
		    (not (Math-realp a))
		    (and calc-group-digits
			 (not (assoc calc-group-char '((",") (" "))))))
		(math-format-number a prec)
	      (let ((s (math-format-number a prec))
		    (c nil))
		(while (string-match (if (> calc-number-radix 14)
					 "\\([0-9]+\\)#\\([0-9a-zA-Z., ]+\\)"
				       "\\([0-9]+\\)#\\([0-9a-dA-D., ]+\\)")
				     s)
		  (setq c (nconc c (list (substring s 0 (match-beginning 0))
					 (list 'subscr
					       (math-match-substring s 2)
					       (math-match-substring s 1))))
			s (substring s (match-end 0))))
		(if (string-match
		     "\\*\\([0-9.]+\\)\\^\\(-?[0-9]+\\)\\()?\\)\\'" s)
		    (setq s (list 'horiz
				  (substring s 0 (match-beginning 0)) " "
				  (list 'supscr
					(math-match-substring s 1)
					(math-match-substring s 2))
				  (math-match-substring s 3))))
		(if c (cons 'horiz (nconc c (list s))) s)))))))
     ((and (get (car a) 'math-compose-forms)
	   (not (eq calc-language 'unform))
	   (let ((comps (get (car a) 'math-compose-forms))
		 temp temp2)
	     (or (and (setq temp (assq calc-language comps))
		      (or (and (setq temp2 (assq (1- (length a)) (cdr temp)))
			       (setq temp (apply (cdr temp2) (cdr a)))
			       (math-compose-expr temp prec))
			  (and (setq temp2 (assq nil (cdr temp)))
			       (funcall (cdr temp2) a))))
		 (and (setq temp (assq nil comps))
		      (or (and (setq temp2 (assq (1- (length a)) (cdr temp)))
			       (setq temp (apply (cdr temp2) (cdr a)))
			       (math-compose-expr temp prec))
			  (and (setq temp2 (assq nil (cdr temp)))
			       (funcall (cdr temp2) a))))))))
     ((eq (car a) 'vec)
      (let* ((math-comp-left-bracket (if calc-vector-brackets
			       (substring calc-vector-brackets 0 1) ""))
	     (math-comp-right-bracket (if calc-vector-brackets
				(substring calc-vector-brackets 1 2) ""))
	     (inner-brackets (memq 'R calc-matrix-brackets))
	     (outer-brackets (memq 'O calc-matrix-brackets))
	     (row-commas (memq 'C calc-matrix-brackets))
	     (math-comp-comma-spc (or calc-vector-commas " "))
	     (math-comp-comma (or calc-vector-commas ""))
	     (math-comp-vector-prec (if (or (and calc-vector-commas
				       (math-vector-no-parens a))
				  (memq 'P calc-matrix-brackets)) 0 1000))
	     (math-comp-just (cond ((eq calc-matrix-just 'right) 'vright)
                                      ((eq calc-matrix-just 'center) 'vcent)
                                      (t 'vleft)))
	     (break calc-break-vectors))
	(if (and (memq calc-language '(nil big))
		 (not calc-break-vectors)
		 (math-matrixp a) (not (math-matrixp (nth 1 a)))
		 (or calc-full-vectors
		     (and (< (length a) 7) (< (length (nth 1 a)) 7))
		     (progn (setq break t) nil)))
	    (if (progn
		  (setq math-comp-vector-prec (if (or (and calc-vector-commas
                                                           (math-vector-no-parens
                                                            (nth 1 a)))
                                                      (memq 'P calc-matrix-brackets))
                                                  0 1000))
		  (= (length a) 2))
		(list 'horiz
		      (concat math-comp-left-bracket math-comp-left-bracket " ")
		      (math-compose-vector (cdr (nth 1 a)) (concat math-comp-comma " ")
					   math-comp-vector-prec)
		      (concat " " math-comp-right-bracket math-comp-right-bracket))
	      (let* ((rows (1- (length a)))
		     (cols (1- (length (nth 1 a))))
		     (base (/ (1- rows) 2))
		     (calc-language 'flat))
		(append '(horiz)
			(list (append '(vleft)
				      (list base)
				      (list (concat (and outer-brackets
							 (concat math-comp-left-bracket
								 " "))
						    (and inner-brackets
							 (concat math-comp-left-bracket
								 " "))))
				      (make-list (1- rows)
						 (concat (and outer-brackets
							      "  ")
							 (and inner-brackets
							      (concat
							       math-comp-left-bracket
							       " "))))))
			(math-compose-matrix (cdr a) 1 cols base)
			(list (append '(vleft)
				      (list base)
				      (make-list (1- rows)
						 (if inner-brackets
						     (concat " "
							     math-comp-right-bracket
							     (and row-commas
								  math-comp-comma))
						   (if (and outer-brackets
							    row-commas)
						       ";" "")))
				      (list (concat
					     (and inner-brackets
						  (concat " "
							  math-comp-right-bracket))
					     (and outer-brackets
						  (concat
						   " "
						   math-comp-right-bracket)))))))))
	  (if (and calc-display-strings
		   (cdr a)
		   (math-vector-is-string a))
	      (math-vector-to-string a t)
	    (if (and break (cdr a)
		     (not (eq calc-language 'flat)))
		(let* ((full (or calc-full-vectors (< (length a) 7)))
		       (rows (if full (1- (length a)) 5))
		       (base (/ (1- rows) 2))
		       (calc-break-vectors nil))
		  (list 'horiz
			(cons 'vleft (cons base
					   (math-compose-rows
					    (cdr a)
					    (if full rows 3) t)))))
	      (if (or calc-full-vectors (< (length a) 7))
                  (if (and
                       (setq spfn (get calc-language 'math-matrix-formatter))
                       (math-matrixp a))
                      (funcall spfn a)
                    (list 'horiz
                          math-comp-left-bracket
                          (math-compose-vector (cdr a)
                                               (concat math-comp-comma " ")
                                               math-comp-vector-prec)
                          math-comp-right-bracket))
		(list 'horiz
		      math-comp-left-bracket
		      (math-compose-vector (list (nth 1 a) (nth 2 a) (nth 3 a))
					   (concat math-comp-comma " ")
                                           math-comp-vector-prec)
		      math-comp-comma
                      (if (setq spfn (get calc-language 'math-dots))
                          (concat " " spfn)
                        " ...")
		      math-comp-comma " "
		      (list 'break math-compose-level)
		      (math-compose-expr (nth (1- (length a)) a)
					 (if (equal math-comp-comma "") 1000 0))
		      math-comp-right-bracket)))))))
     ((eq (car a) 'incomplete)
      (if (cdr (cdr a))
	  (cond ((eq (nth 1 a) 'vec)
		 (list 'horiz "["
		       (math-compose-vector (cdr (cdr a)) ", " 0)
		       " ..."))
		((eq (nth 1 a) 'cplx)
		 (list 'horiz "("
		       (math-compose-vector (cdr (cdr a)) ", " 0)
		       ", ..."))
		((eq (nth 1 a) 'polar)
		 (list 'horiz "("
		       (math-compose-vector (cdr (cdr a)) "; " 0)
		       "; ..."))
		((eq (nth 1 a) 'intv)
		 (list 'horiz
		       (if (memq (nth 2 a) '(0 1)) "(" "[")
		       (math-compose-vector (cdr (cdr (cdr a))) " .. " 0)
		       " .. ..."))
		(t (format "%s" a)))
	(cond ((eq (nth 1 a) 'vec) "[ ...")
	      ((eq (nth 1 a) 'intv)
	       (if (memq (nth 2 a) '(0 1)) "( ..." "[ ..."))
	      (t "( ..."))))
     ((eq (car a) 'var)
      (let ((v (rassq (nth 2 a) math-expr-variable-mapping)))
	(if v
	    (symbol-name (car v))
          (if (setq spfn (get calc-language 'math-var-formatter))
              (funcall spfn a prec)
            (math-compose-var a)))))
     ((eq (car a) 'intv)
      (list 'horiz
            (if (memq (nth 1 a) '(0 1)) "(" "[")
	    (math-compose-expr (nth 2 a) 0)
            " .. "
	    (math-compose-expr (nth 3 a) 0)
            (if (memq (nth 1 a) '(0 2)) ")" "]")))
     ((eq (car a) 'date)
      (if (eq (car calc-date-format) 'X)
	  (math-format-date a)
	(concat "<" (math-format-date a) ">")))
     ((and (eq (car a) 'calcFunc-subscr)
           (setq spfn (get calc-language 'math-compose-subscr)))
      (funcall spfn a))
     ((and (eq (car a) 'calcFunc-subscr) (= (length a) 3)
	   (eq calc-language 'big))
      (let* ((a1 (math-compose-expr (nth 1 a) 1000))
	     (calc-language 'flat)
	     (a2 (math-compose-expr (nth 2 a) 0)))
	(if (or (eq (car-safe a1) 'subscr)
		(and (eq (car-safe a1) 'tag)
		     (eq (car-safe (nth 2 a1)) 'subscr)
		     (setq a1 (nth 2 a1))))
	    (list 'subscr
		  (nth 1 a1)
		  (list 'horiz
			(nth 2 a1)
			", "
			a2))
	  (list 'subscr a1 a2))))
     ((and (eq (car a) '^)
	   (eq calc-language 'big))
      (list 'supscr
	    (if (or (math-looks-negp (nth 1 a))
		    (memq (car-safe (nth 1 a)) '(^ / frac calcFunc-sqrt))
		    (and (eq (car-safe (nth 1 a)) 'cplx)
			 (math-negp (nth 1 (nth 1 a)))
			 (eq (nth 2 (nth 1 a)) 0)))
		(list 'horiz "(" (math-compose-expr (nth 1 a) 0) ")")
	      (math-compose-expr (nth 1 a) 201))
	    (let ((calc-language 'flat)
		  (calc-number-radix 10)
                  (calc-twos-complement-mode nil))
	      (math-compose-expr (nth 2 a) 0))))
     ((and (eq (car a) '/)
	   (eq calc-language 'big))
      (let ((a1 (let ((calc-language (if (memq (car-safe (nth 1 a)) '(/ frac))
					 'flat 'big)))
		  (math-compose-expr (nth 1 a) 0)))
	    (a2 (let ((calc-language (if (memq (car-safe (nth 2 a)) '(/ frac))
					 'flat 'big)))
		  (math-compose-expr (nth 2 a) 0))))
	(list 'vcent
	      (math-comp-height a1)
	      a1 '(rule ?-) a2)))
     ((and (eq (car a) 'calcFunc-lambda)
	   (> (length a) 2)
	   (memq calc-language '(nil flat big)))
      (let ((p (cdr a))
	    (ap calc-arg-values)
	    (math-compose-hash-args (if (= (length a) 3) 1 t)))
	(while (and (cdr p) (equal (car p) (car ap)))
	  (setq p (cdr p) ap (cdr ap)))
	(append '(horiz "<")
		(if (cdr p)
		    (list (math-compose-vector
			   (nreverse (cdr (reverse (cdr a)))) ", " 0)
			  " : ")
		  nil)
		(list (math-compose-expr (nth (1- (length a)) a) 0)
		      ">"))))
     ((and (eq (car a) 'calcFunc-string)
	   (= (length a) 2)
	   (math-vectorp (nth 1 a))
	   (math-vector-is-string (nth 1 a)))
      (if (eq calc-language 'unform)
	  (concat "string(" (math-vector-to-string (nth 1 a) t) ")")
	(math-vector-to-string (nth 1 a) nil)))
     ((and (eq (car a) 'calcFunc-bstring)
	   (= (length a) 2)
	   (math-vectorp (nth 1 a))
	   (math-vector-is-string (nth 1 a)))
      (if (eq calc-language 'unform)
	  (concat "bstring(" (math-vector-to-string (nth 1 a) t) ")")
	(let ((c nil)
	      (s (math-vector-to-string (nth 1 a) nil))
	      p)
	  (while (string-match "[^ ] +[^ ]" s)
	    (setq p (1- (match-end 0))
		  c (cons (list 'break math-compose-level)
			  (cons (substring s 0 p)
				c))
		  s (substring s p)))
	  (setq c (nreverse (cons s c)))
	  (or (= prec -123)
	      (setq c (cons (list 'set math-compose-level 2) c)))
	  (cons 'horiz c))))
     ((and (eq (car a) 'calcFunc-cprec)
	   (not (eq calc-language 'unform))
	   (= (length a) 3)
	   (integerp (nth 2 a)))
      (let ((c (math-compose-expr (nth 1 a) -1)))
	(if (> prec (nth 2 a))
            (if (setq spfn (get calc-language 'math-big-parens))
                (list 'horiz (car spfn) c (cdr spfn))
              (list 'horiz "(" c ")"))
	  c)))
     ((and (eq (car a) 'calcFunc-choriz)
	   (not (eq calc-language 'unform))
	   (memq (length a) '(2 3 4))
	   (math-vectorp (nth 1 a))
	   (if (integerp (nth 2 a))
	       (or (null (nth 3 a))
		   (and (math-vectorp (nth 3 a))
			(math-vector-is-string (nth 3 a))))
	     (or (null (nth 2 a))
		 (and (math-vectorp (nth 2 a))
		      (math-vector-is-string (nth 2 a))))))
      (let* ((cprec (and (integerp (nth 2 a)) (nth 2 a)))
	     (sep (nth (if cprec 3 2) a))
	     (bprec nil))
	(if sep
	    (math-compose-vector (cdr (nth 1 a))
				 (math-vector-to-string sep nil)
				 (or cprec prec))
	  (cons 'horiz (mapcar (function
				(lambda (x)
				  (if (eq (car-safe x) 'calcFunc-bstring)
				      (prog1
					  (math-compose-expr
					   x (or bprec cprec prec))
					(setq bprec -123))
				    (math-compose-expr x (or cprec prec)))))
			       (cdr (nth 1 a)))))))
     ((and (memq (car a) '(calcFunc-cvert calcFunc-clvert calcFunc-crvert))
	   (not (eq calc-language 'unform))
	   (memq (length a) '(2 3))
	   (math-vectorp (nth 1 a))
	   (or (null (nth 2 a))
	       (integerp (nth 2 a))))
      (let* ((base 0)
	     (v 0)
	     (prec (or (nth 2 a) prec))
	     (c (mapcar (function
			 (lambda (x)
			   (let ((b nil) (cc nil) a d)
			     (if (and (memq (car-safe x) '(calcFunc-cbase
							   calcFunc-ctbase
							   calcFunc-cbbase))
				      (memq (length x) '(1 2)))
				 (setq b (car x)
				       x (nth 1 x)))
			     (if (and (eq (car-safe x) 'calcFunc-crule)
				      (memq (length x) '(1 2))
				      (or (null (nth 1 x))
					  (and (math-vectorp (nth 1 x))
					       (= (length (nth 1 x)) 2)
					       (math-vector-is-string
						(nth 1 x)))
					  (and (natnump (nth 1 x))
					       (<= (nth 1 x) 255))))
				 (setq cc (list
					   'rule
					   (if (math-vectorp (nth 1 x))
					       (aref (math-vector-to-string
						      (nth 1 x) nil) 0)
					     (or (nth 1 x) ?-))))
			       (or (and (memq (car-safe x) '(calcFunc-cvspace
							     calcFunc-ctspace
							     calcFunc-cbspace))
					(memq (length x) '(2 3))
					(eq (nth 1 x) 0))
				   (null x)
				   (setq cc (math-compose-expr x prec))))
			     (setq a (if cc (math-comp-ascent cc) 0)
				   d (if cc (math-comp-descent cc) 0))
			     (if (eq b 'calcFunc-cbase)
				 (setq base (+ v a -1))
			       (if (eq b 'calcFunc-ctbase)
				   (setq base v)
				 (if (eq b 'calcFunc-cbbase)
				     (setq base (+ v a d -1)))))
			     (setq v (+ v a d))
			     cc)))
			(cdr (nth 1 a)))))
	(setq c (delq nil c))
	(if c
	    (cons (if (eq (car a) 'calcFunc-cvert) 'vcent
		    (if (eq (car a) 'calcFunc-clvert) 'vleft 'vright))
		  (cons base c))
	  " ")))
     ((and (memq (car a) '(calcFunc-csup calcFunc-csub))
	   (not (eq calc-language 'unform))
	   (memq (length a) '(3 4))
	   (or (null (nth 3 a))
	       (integerp (nth 3 a))))
      (list (if (eq (car a) 'calcFunc-csup) 'supscr 'subscr)
	    (math-compose-expr (nth 1 a) (or (nth 3 a) 0))
	    (math-compose-expr (nth 2 a) 0)))
     ((and (eq (car a) 'calcFunc-cflat)
	   (not (eq calc-language 'unform))
	   (memq (length a) '(2 3))
	   (or (null (nth 2 a))
	       (integerp (nth 2 a))))
      (let ((calc-language (if (memq calc-language '(nil big))
			       'flat calc-language)))
	(math-compose-expr (nth 1 a) (or (nth 2 a) 0))))
     ((and (eq (car a) 'calcFunc-cspace)
	   (memq (length a) '(2 3))
	   (natnump (nth 1 a)))
      (if (nth 2 a)
	  (cons 'horiz (make-list (nth 1 a)
				  (if (and (math-vectorp (nth 2 a))
					   (math-vector-is-string (nth 2 a)))
				      (math-vector-to-string (nth 2 a) nil)
				    (math-compose-expr (nth 2 a) 0))))
	(make-string (nth 1 a) ?\ )))
     ((and (memq (car a) '(calcFunc-cvspace calcFunc-ctspace calcFunc-cbspace))
	   (memq (length a) '(2 3))
	   (natnump (nth 1 a)))
      (if (= (nth 1 a) 0)
	  ""
	(let* ((c (if (nth 2 a)
		      (if (and (math-vectorp (nth 2 a))
			       (math-vector-is-string (nth 2 a)))
			  (math-vector-to-string (nth 2 a) nil)
			(math-compose-expr (nth 2 a) 0))
		    " "))
	       (ca (math-comp-ascent c))
	       (cd (math-comp-descent c)))
	  (cons 'vleft
		(cons (if (eq (car a) 'calcFunc-ctspace)
			  (1- ca)
			(if (eq (car a) 'calcFunc-cbspace)
			    (+ (* (1- (nth 1 a)) (+ ca cd)) (1- ca))
			  (/ (1- (* (nth 1 a) (+ ca cd))) 2)))
		      (make-list (nth 1 a) c))))))
     ((and (eq (car a) 'calcFunc-evalto)
	   (setq calc-any-evaltos t)
	   (setq spfn (get calc-language 'math-evalto))
	   (= math-compose-level (if math-comp-tagged 2 1))
	   (= (length a) 3))
      (list 'horiz
            (car spfn)
	    (math-compose-expr (nth 1 a) 0)
	    (cdr spfn)
	    (math-compose-expr (nth 2 a) 0)))
     (t
      (let ((op (and (not (eq calc-language 'unform))
		     (if (and (eq (car a) 'calcFunc-if) (= (length a) 4))
			 (assoc "?" math-expr-opers)
		       (math-assq2 (car a) math-expr-opers)))))
	(cond ((and op
		    (or (= (length a) 3) (eq (car a) 'calcFunc-if))
		    (/= (nth 3 op) -1))
	       (cond
		((> prec (or (nth 4 op) (min (nth 2 op) (nth 3 op))))
		 (if (and (memq calc-language '(tex latex))
			  (not (math-tex-expr-is-flat a)))
		     (if (eq (car-safe a) '/)
			 (list 'horiz "{" (math-compose-expr a -1) "}")
		       (list 'horiz "\\left( "
			     (math-compose-expr a -1)
			     " \\right)"))
		   (if (eq calc-language 'eqn)
		       (if (or (eq (car-safe a) '/)
			       (= (/ prec 100) 9))
			   (list 'horiz "{" (math-compose-expr a -1) "}")
			 (if (math-tex-expr-is-flat a)
			     (list 'horiz "( " (math-compose-expr a -1) " )")
			   (list 'horiz "{left ( "
				 (math-compose-expr a -1)
				 " right )}")))
		     (list 'horiz "(" (math-compose-expr a 0) ")"))))
		((and (memq calc-language '(tex latex))
		      (memq (car a) '(/ calcFunc-choose calcFunc-evalto))
		      (>= prec 0))
		 (list 'horiz "{" (math-compose-expr a -1) "}"))
		((eq (car a) 'calcFunc-if)
		 (list 'horiz
		       (math-compose-expr (nth 1 a) (nth 2 op))
		       " ? "
		       (math-compose-expr (nth 2 a) 0)
		       " : "
		       (math-compose-expr (nth 3 a) (nth 3 op))))
		(t
		 (let* ((math-comp-tagged (and math-comp-tagged
					       (not (math-primp a))
					       math-comp-tagged))
			(setlev (if (= prec (min (nth 2 op) (nth 3 op)))
				    (progn
				      (setq math-compose-level
					    (1- math-compose-level))
				      nil)
				  math-compose-level))
			(lhs (math-compose-expr (nth 1 a) (nth 2 op)))
			(rhs (math-compose-expr (nth 2 a) (nth 3 op))))
		   (and (equal (car op) "^")
			(eq (math-comp-first-char lhs) ?-)
			(setq lhs (list 'horiz "(" lhs ")")))
		   (and (memq calc-language '(tex latex))
			(or (equal (car op) "^") (equal (car op) "_"))
			(not (and (stringp rhs) (= (length rhs) 1)))
			(setq rhs (list 'horiz "{" rhs "}")))
		   (or (and (eq (car a) '*)
			    (or (null calc-language)
				(assoc "2x" math-expr-opers))
			    (let* ((prevt (math-prod-last-term (nth 1 a)))
				   (nextt (math-prod-first-term (nth 2 a)))
				   (prevc (or (math-comp-last-char lhs)
					      (and (memq (car-safe prevt)
							 '(^ calcFunc-subscr
							     calcFunc-sqrt
							     frac))
						   (eq calc-language 'big)
						   ?0)))
				   (nextc (or (math-comp-first-char rhs)
					      (and (memq (car-safe nextt)
							 '(calcFunc-sqrt
							   calcFunc-sum
							   calcFunc-prod
							   calcFunc-integ))
						   (eq calc-language 'big)
						   ?0))))
			      (and prevc nextc
				   (or (and (>= nextc ?a) (<= nextc ?z))
				       (and (>= nextc ?A) (<= nextc ?Z))
				       (and (>= nextc ?α) (<= nextc ?ω))
				       (and (>= nextc ?Α) (<= nextc ?Ω))
				       (and (>= nextc ?0) (<= nextc ?9))
				       (memq nextc '(?. ?_ ?#
							?\( ?\[ ?\{))
				       (and (eq nextc ?\\)
					    (not (string-match
						  "\\`\\\\left("
						  (math-comp-first-string
						   rhs)))))
				   (not (and (eq (car-safe prevt) 'var)
					     (eq nextc ?\()))
				   (list 'horiz
					 (list 'set setlev 1)
					 lhs
					 (list 'break math-compose-level)
					 " "
					 rhs))))
		       (list 'horiz
			     (list 'set setlev 1)
			     lhs
			     (list 'break math-compose-level)
			     (if (or (equal (car op) "^")
				     (equal (car op) "_")
				     (equal (car op) "**")
				     (and (equal (car op) "*")
					  (math-comp-last-char lhs)
					  (math-comp-first-char rhs))
				     (and (equal (car op) "/")
					  (math-num-integerp (nth 1 a))
					  (math-integerp (nth 2 a))))
				 (car op)
			       (if (and (eq calc-language 'big)
					(equal (car op) "=>"))
				   "  =>  "
				 (concat " " (car op) " ")))
			     rhs))))))
	      ((and op (= (length a) 2) (= (nth 3 op) -1))
	       (cond
		((or (> prec (or (nth 4 op) (nth 2 op)))
		     (and (not (eq (assoc (car op) math-expr-opers) op))
			  (> prec 0)))   ; don't write x% + y
		 (if (and (memq calc-language '(tex latex))
			  (not (math-tex-expr-is-flat a)))
		     (list 'horiz "\\left( "
			   (math-compose-expr a -1)
			   " \\right)")
		   (if (eq calc-language 'eqn)
		       (if (= (/ prec 100) 9)
			   (list 'horiz "{" (math-compose-expr a -1) "}")
			 (if (math-tex-expr-is-flat a)
			     (list 'horiz "{( " (math-compose-expr a -1) " )}")
			   (list 'horiz "{left ( "
				 (math-compose-expr a -1)
				 " right )}")))
		     (list 'horiz "(" (math-compose-expr a 0) ")"))))
		(t
		 (let ((lhs (math-compose-expr (nth 1 a) (nth 2 op))))
		 (list 'horiz
		       lhs
		       (if (or (> (length (car op)) 1)
			       (not (math-comp-is-flat lhs)))
			   (concat " " (car op))
			 (car op)))))))
	      ((and op (= (length a) 2) (= (nth 2 op) -1))
	       (cond
		((eq (nth 3 op) 0)
		 (let ((lr (and (memq calc-language '(tex latex))
				(not (math-tex-expr-is-flat (nth 1 a))))))
		   (list 'horiz
			 (if lr "\\left" "")
			 (if (string-match "\\`u\\([^a-zA-Zα-ωΑ-Ω]\\)\\'" (car op))
			     (substring (car op) 1)
			   (car op))
			 (if (or lr (> (length (car op)) 2)) " " "")
			 (math-compose-expr (nth 1 a) -1)
			 (if (or lr (> (length (car op)) 2)) " " "")
			 (if lr "\\right" "")
			 (car (nth 1 (memq op math-expr-opers))))))
		((> prec (or (nth 4 op) (nth 3 op)))
		 (if (and (memq calc-language '(tex latex))
			  (not (math-tex-expr-is-flat a)))
		     (list 'horiz "\\left( "
			   (math-compose-expr a -1)
			   " \\right)")
		   (if (eq calc-language 'eqn)
		       (if (= (/ prec 100) 9)
			   (list 'horiz "{" (math-compose-expr a -1) "}")
			 (if (math-tex-expr-is-flat a)
			     (list 'horiz "{( " (math-compose-expr a -1) " )}")
			   (list 'horiz "{left ( "
				 (math-compose-expr a -1)
				 " right )}")))
		     (list 'horiz "(" (math-compose-expr a 0) ")"))))
		(t
		 (let ((rhs (math-compose-expr (nth 1 a) (nth 3 op))))
		   (list 'horiz
			 (let ((ops (if (string-match "\\`u\\([^a-zA-Zα-ωΑ-Ω]\\)\\'"
						      (car op))
					(substring (car op) 1)
				      (car op))))
			   (if (or (> (length ops) 1)
				   (not (math-comp-is-flat rhs)))
			       (concat ops " ")
			     ops))
			 rhs)))))
	      ((and (eq calc-language 'big)
		    (setq op (get (car a) 'math-compose-big))
		    (funcall op a prec)))
	      ((and (setq op (assq calc-language
				   '( ( nil . math-compose-normal )
				      ( flat . math-compose-normal )
				      ( big . math-compose-normal )
				      ( c . math-compose-c )
				      ( pascal . math-compose-pascal )
				      ( fortran . math-compose-fortran )
				      ( tex . math-compose-tex )
				      ( latex . math-compose-latex )
				      ( eqn . math-compose-eqn )
                                      ( yacas . math-compose-yacas )
                                      ( maxima . math-compose-maxima )
                                      ( giac . math-compose-giac )
				      ( math . math-compose-math )
				      ( maple . math-compose-maple ))))
		    (setq op (get (car a) (cdr op)))
		    (funcall op a prec)))
	      (t
	       (let* ((func (car a))
		      (func2 (assq func '(( mod . calcFunc-makemod )
					  ( sdev . calcFunc-sdev )
					  ( + . calcFunc-add )
					  ( - . calcFunc-sub )
					  ( * . calcFunc-mul )
					  ( / . calcFunc-div )
					  ( % . calcFunc-mod )
					  ( ^ . calcFunc-pow )
					  ( neg . calcFunc-neg )
					  ( | . calcFunc-vconcat ))))
		      left right args)
		 (if func2
		     (setq func (cdr func2)))
		 (if (setq func2 (rassq func math-expr-function-mapping))
		     (setq func (car func2)))
		 (setq func (math-remove-dashes
			     (if (string-match
				  "\\`calcFunc-\\([a-zA-Zα-ωΑ-Ω0-9']+\\)\\'"
				  (symbol-name func))
				 (math-match-substring (symbol-name func) 1)
			       (symbol-name func))))
		 (if (memq calc-language calc-lang-allow-percentsigns)
		     (setq func (math-to-percentsigns func)))
		 (if (memq calc-language calc-lang-allow-underscores)
		     (setq func (math-to-underscores func)))
                 (if (setq spfn (get calc-language 'math-func-formatter))
                     (funcall spfn func a)

                   (list 'horiz func calc-function-open
		       (math-compose-vector (cdr a) ", " 0)
		       calc-function-close))))))))))


(defun math-prod-first-term (x)
  (while (eq (car-safe x) '*)
    (setq x (nth 1 x)))
  x)

(defun math-prod-last-term (x)
  (while (eq (car-safe x) '*)
    (setq x (nth 2 x)))
  x)

(defun math-compose-vector (a sep prec)
  (if a
      (cons 'horiz
	    (cons (list 'set math-compose-level)
		  (let ((c (list (math-compose-expr (car a) prec))))
		    (while (setq a (cdr a))
		      (setq c (cons (if (eq (car-safe (car a))
					    'calcFunc-bstring)
					(let ((math-compose-level
					       (1- math-compose-level)))
					  (math-compose-expr (car a) -123))
				      (math-compose-expr (car a) prec))
				    (cons (list 'break math-compose-level)
					  (cons sep c)))))
		    (nreverse c))))
    ""))

(defun math-vector-no-parens (a)
  (or (cdr (cdr a))
      (not (eq (car-safe (nth 1 a)) '*))))

(defun math-compose-matrix (a col cols base)
  (let ((col 0)
	(res nil))
    (while (<= (setq col (1+ col)) cols)
      (setq res (cons (cons math-comp-just
			    (cons base
				  (mapcar (function
					   (lambda (r)
					     (list 'horiz
						   (math-compose-expr
						    (nth col r)
						    math-comp-vector-prec)
						   (if (= col cols)
						       ""
						     (concat
                                                      math-comp-comma-spc " ")))))
					  a)))
		      res)))
    (nreverse res)))

(defun math-compose-rows (a count first)
  (if (cdr a)
      (if (<= count 0)
	  (if (< count 0)
	      (math-compose-rows (cdr a) -1 nil)
	    (cons (concat
                   (let ((mdots (get calc-language 'math-dots)))
                     (if mdots
                         (concat " " mdots)
                       "  ..."))
                   math-comp-comma)
		  (math-compose-rows (cdr a) -1 nil)))
	(cons (list 'horiz
		    (if first (concat math-comp-left-bracket " ") "  ")
		    (math-compose-expr (car a) math-comp-vector-prec)
		    math-comp-comma)
	      (math-compose-rows (cdr a) (1- count) nil)))
    (list (list 'horiz
		(if first (concat math-comp-left-bracket " ") "  ")
		(math-compose-expr (car a) math-comp-vector-prec)
		(concat " " math-comp-right-bracket)))))

(defun math-vector-is-string (a)
  (while (and (setq a (cdr a))
	      (or (and (natnump (car a))
		       (<= (car a) 255))
		  (and (eq (car-safe (car a)) 'cplx)
		       (natnump (nth 1 (car a)))
		       (eq (nth 2 (car a)) 0)
		       (<= (nth 1 (car a)) 255)))))
  (null a))

(defconst math-vector-to-string-chars '( ( ?\" . "\\\"" )
					 ( ?\\ . "\\\\" )
					 ( ?\a . "\\a" )
					 ( ?\b . "\\b" )
					 ( ?\e . "\\e" )
					 ( ?\f . "\\f" )
					 ( ?\n . "\\n" )
					 ( ?\r . "\\r" )
					 ( ?\t . "\\t" )
					 ( ?\^? . "\\^?" )))

(defun math-vector-to-string (a &optional quoted)
  (setq a (concat (mapcar (function (lambda (x) (if (consp x) (nth 1 x) x)))
			  (cdr a))))
  (if (string-match "[\000-\037\177\\\"]" a)
      (let ((p 0)
	    (pat (if quoted "[\000-\037\177\\\"]" "[\000-\037\177]"))
	    (codes (if quoted math-vector-to-string-chars '((?\^? . "^?"))))
	    (fmt (if quoted "\\^%c" "^%c"))
	    new)
	(while (setq p (string-match pat a p))
	  (if (setq new (assq (aref a p) codes))
	      (setq a (concat (substring a 0 p)
			      (cdr new)
			      (substring a (1+ p)))
		    p (+ p (length (cdr new))))
	    (setq a (concat (substring a 0 p)
			    (format fmt (+ (aref a p) 64))
			    (substring a (1+ p)))
		  p (+ p 2))))))
  (if quoted
      (concat "\"" a "\"")
    a))


(defun math-to-underscores (x)
  (if (string-match "\\`\\(.*\\)#\\(.*\\)\\'" x)
      (math-to-underscores
       (concat (math-match-substring x 1) "_" (math-match-substring x 2)))
    x))

(defun math-to-percentsigns (x)
  (if (string-match "\\`\\(.*\\)o'o\\(.*\\)\\'" x)
      (math-to-underscores
       (concat (math-match-substring x 1) "%" (math-match-substring x 2)))
    x))

(defun math-tex-expr-is-flat (a)
  (or (Math-integerp a)
      (memq (car a) '(float var))
      (and (memq (car a) '(+ - * neg))
	   (progn
	     (while (and (setq a (cdr a))
			 (math-tex-expr-is-flat (car a))))
	     (null a)))
      (and (memq (car a) '(^ calcFunc-subscr))
	   (math-tex-expr-is-flat (nth 1 a)))))

(put 'calcFunc-log 'math-compose-big 'math-compose-log)
(defun math-compose-log (a prec)
  (and (= (length a) 3)
       (list 'horiz
	     (list 'subscr "log"
		   (let ((calc-language 'flat))
		     (math-compose-expr (nth 2 a) 1000)))
	     "("
	     (math-compose-expr (nth 1 a) 1000)
	     ")")))

(put 'calcFunc-log10 'math-compose-big 'math-compose-log10)
(defun math-compose-log10 (a prec)
  (and (= (length a) 2)
       (list 'horiz
	     (list 'subscr "log" "10")
	     "("
	     (math-compose-expr (nth 1 a) 1000)
	     ")")))

(put 'calcFunc-deriv 'math-compose-big 'math-compose-deriv)
(put 'calcFunc-tderiv 'math-compose-big 'math-compose-deriv)
(defun math-compose-deriv (a prec)
  (when (= (length a) 3)
    (math-compose-expr (list '/
			     (list 'calcFunc-choriz
				   (list 'vec
					 '(calcFunc-string (vec ?d))
					 (nth 1 a)))
			     (list 'calcFunc-choriz
				   (list 'vec
					 '(calcFunc-string (vec ?d))
					 (nth 2 a))))
		       prec)))

(put 'calcFunc-sqrt 'math-compose-big 'math-compose-sqrt)
(defun math-compose-sqrt (a prec)
  (when (= (length a) 2)
    (let* ((c (math-compose-expr (nth 1 a) 0))
	   (a (math-comp-ascent c))
	   (d (math-comp-descent c))
	   (h (+ a d))
	   (w (math-comp-width c)))
      (list 'vleft
	    a
	    (concat (if (= h 1) " " "  ")
		    (make-string (+ w 2) ?\_))
	    (list 'horiz
		  (if (= h 1)
		      "V"
		    (append (list 'vleft (1- a))
			    (make-list (1- h) " |")
			    '("\\|")))
		  " "
		  c)))))

(put 'calcFunc-choose 'math-compose-big 'math-compose-choose)
(defun math-compose-choose (a prec)
  (let ((a1 (math-compose-expr (nth 1 a) 0))
	(a2 (math-compose-expr (nth 2 a) 0)))
    (list 'horiz
	  "("
	  (list 'vcent
		(math-comp-height a1)
		a1 " " a2)
	  ")")))

(put 'calcFunc-integ 'math-compose-big 'math-compose-integ)
(defun math-compose-integ (a prec)
  (and (memq (length a) '(3 5))
       (eq (car-safe (nth 2 a)) 'var)
       (let* ((parens (and (>= prec 196) (/= prec 1000)))
	      (var (math-compose-expr (nth 2 a) 0))
	      (over (and (eq (car-safe (nth 2 a)) 'var)
			 (or (and (eq (car-safe (nth 1 a)) '/)
				  (math-numberp (nth 1 (nth 1 a))))
			     (and (eq (car-safe (nth 1 a)) '^)
				  (math-looks-negp (nth 2 (nth 1 a)))))))
	      (expr (math-compose-expr (if over
					   (math-mul (nth 1 a)
						     (math-build-var-name
						      (format
						       "d%s"
						       (nth 1 (nth 2 a)))))
					 (nth 1 a)) 185))
	      (calc-language 'flat)
	      (low (and (nth 3 a) (math-compose-expr (nth 3 a) 0)))
	      (high (and (nth 4 a) (math-compose-expr (nth 4 a) 0))))
	 (list 'horiz
	       (if parens "(" "")
	       (append (list 'vcent (if high 3 2))
		       (and high (list (list 'horiz "  " high)))
		       '("  /"
			 " | "
			 " | "
			 " | "
			 "/  ")
		       (and low (list (list 'horiz low "  "))))
	       expr
	       (if over
		   ""
		 (list 'horiz " d" var))
	       (if parens ")" "")))))

(put 'calcFunc-sum 'math-compose-big 'math-compose-sum)
(defun math-compose-sum (a prec)
  (and (memq (length a) '(3 5 6))
       (let* ((expr (math-compose-expr (nth 1 a) 185))
	      (calc-language 'flat)
	      (var (math-compose-expr (nth 2 a) 0))
	      (low (and (nth 3 a) (math-compose-expr (nth 3 a) 0)))
	      (high (and (nth 4 a) (math-compose-vector (nthcdr 4 a) ", " 0))))
	 (list 'horiz
	       (if (memq prec '(180 201)) "(" "")
	       (append (list 'vcent (if high 3 2))
		       (and high (list high))
		       '("---- "
			 "\\    "
			 " >   "
			 "/    "
			 "---- ")
		       (if low
			   (list (list 'horiz var " = " low))
			 (list var)))
	       (if (memq (car-safe (nth 1 a)) '(calcFunc-sum calcFunc-prod))
		   " " "")
	       expr
	       (if (memq prec '(180 201)) ")" "")))))

(put 'calcFunc-prod 'math-compose-big 'math-compose-prod)
(defun math-compose-prod (a prec)
  (and (memq (length a) '(3 5 6))
       (let* ((expr (math-compose-expr (nth 1 a) 198))
	      (calc-language 'flat)
	      (var (math-compose-expr (nth 2 a) 0))
	      (low (and (nth 3 a) (math-compose-expr (nth 3 a) 0)))
	      (high (and (nth 4 a) (math-compose-vector (nthcdr 4 a) ", " 0))))
	 (list 'horiz
	       (if (memq prec '(196 201)) "(" "")
	       (append (list 'vcent (if high 3 2))
		       (and high (list high))
		       '("----- "
			 " | |  "
			 " | |  "
			 " | |  ")
		       (if low
			   (list (list 'horiz var " = " low))
			 (list var)))
	       (if (memq (car-safe (nth 1 a)) '(calcFunc-sum calcFunc-prod))
		   " " "")
	       expr
	       (if (memq prec '(196 201)) ")" "")))))

;; The variables math-svo-c, math-svo-wid and math-svo-off are local
;; to math-stack-value-offset in calc.el, but are used by
;; math-stack-value-offset-fancy, which is called by math-stack-value-offset..
(defvar math-svo-c)
(defvar math-svo-wid)
(defvar math-svo-off)

(defun math-stack-value-offset-fancy ()
  (let ((cwid (+ (math-comp-width math-svo-c))))
    (cond ((eq calc-display-just 'right)
	   (if calc-display-origin
	       (setq math-svo-wid (max calc-display-origin 5))
	     (if (integerp calc-line-breaking)
		 (setq math-svo-wid calc-line-breaking)))
	   (setq math-svo-off (- math-svo-wid cwid
			(max (- (length calc-right-label)
				(if (and (integerp calc-line-breaking)
					 calc-display-origin)
				    (max (- calc-line-breaking
					    calc-display-origin)
					 0)
				  0))
			     0))))
	  (t
	   (if calc-display-origin
	       (progn
		 (setq math-svo-off (- calc-display-origin (/ cwid 2)))
		 (if (integerp calc-line-breaking)
		     (setq math-svo-off (min math-svo-off (- calc-line-breaking cwid
					   (length calc-right-label)))))
		 (if (>= math-svo-off 0)
		     (setq math-svo-wid (max math-svo-wid (+ math-svo-off cwid)))))
	     (if (integerp calc-line-breaking)
		 (setq math-svo-wid calc-line-breaking))
	     (setq math-svo-off (/ (- math-svo-wid cwid) 2)))))
    (and (integerp calc-line-breaking)
	 (or (< math-svo-off 0)
	     (and calc-display-origin
		  (> calc-line-breaking calc-display-origin)))
	 (setq math-svo-wid calc-line-breaking))))


;;; Convert a composition to string form, with embedded \n's if necessary.

(defun math-composition-to-string (c &optional width)
  (or width (setq width (calc-window-width)))
  (if calc-display-raw
      (math-comp-to-string-raw c 0)
    (if (math-comp-is-flat c)
	(math-comp-to-string-flat c width)
      (math-vert-comp-to-string
       (math-comp-simplify c width)))))

(defvar math-comp-buf-string (make-vector 10 ""))
(defvar math-comp-buf-margin (make-vector 10 0))
(defvar math-comp-buf-level (make-vector 10 0))
(defun math-comp-is-flat (c)     ; check if c's height is 1.
  (cond ((not (consp c)) t)
	((memq (car c) '(set break)) t)
	((eq (car c) 'horiz)
	 (while (and (setq c (cdr c))
		     (math-comp-is-flat (car c))))
	 (null c))
	((memq (car c) '(vleft vcent vright))
	 (and (= (length c) 3)
	      (= (nth 1 c) 0)
	      (math-comp-is-flat (nth 2 c))))
	((eq (car c) 'tag)
	 (math-comp-is-flat (nth 2 c)))
	(t nil)))


;;; Convert a one-line composition to a string.  Break into multiple
;;; lines if necessary, choosing break points according to the structure
;;; of the formula.

;; The variables math-comp-full-width, math-comp-highlight, math-comp-word,
;; math-comp-level, math-comp-margin and math-comp-buf are local to
;; math-comp-to-string-flat, but are used by math-comp-to-string-flat-term,
;; which is called by math-comp-to-string-flat.
;; math-comp-highlight and math-comp-buf are also local to
;; math-comp-simplify-term and math-comp-simplify respectively, but are used
;; by math-comp-add-string.
(defvar math-comp-full-width)
(defvar math-comp-highlight)
(defvar math-comp-word)
(defvar math-comp-level)
(defvar math-comp-margin)
(defvar math-comp-buf)
;; The variable math-comp-pos is local to math-comp-to-string-flat, but
;; is used by math-comp-to-string-flat-term and math-comp-sel-first-term,
;; which are called by math-comp-to-string-flat.
(defvar math-comp-pos)

(defun math-comp-to-string-flat (c math-comp-full-width)
  (if math-comp-sel-hpos
      (let ((math-comp-pos 0))
	(math-comp-sel-flat-term c))
    (let ((math-comp-buf "")
	  (math-comp-word "")
	  (math-comp-pos 0)
	  (math-comp-margin 0)
	  (math-comp-highlight (and math-comp-selected calc-show-selections))
	  (math-comp-level -1))
      (math-comp-to-string-flat-term '(set -1 0))
      (math-comp-to-string-flat-term c)
      (math-comp-to-string-flat-term '(break -1))
      (let ((str (aref math-comp-buf-string 0))
	    (prefix ""))
	(and (> (length str) 0) (= (aref str 0) ? )
	     (> (length math-comp-buf) 0)
	     (let ((k (length math-comp-buf)))
	       (while (not (= (aref math-comp-buf (setq k (1- k))) ?\n)))
	       (aset math-comp-buf k ? )
	       (if (and (< (1+ k) (length math-comp-buf))
			(= (aref math-comp-buf (1+ k)) ? ))
		   (progn
		     (aset math-comp-buf (1+ k) ?\n)
		     (setq prefix " "))
		 (setq prefix "\n"))))
	(concat math-comp-buf prefix str)))))

(defun math-comp-to-string-flat-term (c)
  (cond ((not (consp c))
	 (if math-comp-highlight
	     (setq c (math-comp-highlight-string c)))
	 (setq math-comp-word (if (= (length math-comp-word) 0) c
                                (concat math-comp-word c))
	       math-comp-pos (+ math-comp-pos (length c))))

	((eq (car c) 'horiz)
	 (while (setq c (cdr c))
	   (math-comp-to-string-flat-term (car c))))

	((eq (car c) 'set)
	 (if (nth 1 c)
	     (progn
	       (setq math-comp-level (1+ math-comp-level))
	       (if (>= math-comp-level (length math-comp-buf-string))
		   (setq math-comp-buf-string (vconcat math-comp-buf-string
						       math-comp-buf-string)
			 math-comp-buf-margin (vconcat math-comp-buf-margin
						       math-comp-buf-margin)
			 math-comp-buf-level (vconcat math-comp-buf-level
						      math-comp-buf-level)))
	       (aset math-comp-buf-string math-comp-level "")
	       (aset math-comp-buf-margin math-comp-level (+ math-comp-pos
							(or (nth 2 c) 0)))
	       (aset math-comp-buf-level math-comp-level (nth 1 c)))))

	((eq (car c) 'break)
	 (if (not calc-line-breaking)
	     (setq math-comp-buf (concat math-comp-buf math-comp-word)
		   math-comp-word "")
	   (let ((i 0) str)
	     (if (and (> math-comp-pos math-comp-full-width)
		      (progn
			(while (progn
				 (setq str (aref math-comp-buf-string i))
				 (and (= (length str) 0) (< i math-comp-level)))
			  (setq i (1+ i)))
			(or (> (length str) 0) (> (length math-comp-buf) 0))))
		 (let ((prefix "") mrg wid)
		   (setq mrg (aref math-comp-buf-margin i))
		   (if (> mrg 12)  ; indenting too far, go back to far left
		       (setq mrg (if calc-line-numbering 5 1)))
		   (setq wid (+ (length str) math-comp-margin))
		   (and (> (length str) 0) (= (aref str 0) ? )
			(> (length math-comp-buf) 0)
			(let ((k (length math-comp-buf)))
			  (while (not (= (aref math-comp-buf (setq k (1- k))) ?\n)))
			  (aset math-comp-buf k ? )
			  (if (and (< (1+ k) (length math-comp-buf))
				   (= (aref math-comp-buf (1+ k)) ? ))
			      (progn
				(aset math-comp-buf (1+ k) ?\n)
				(setq prefix " "))
			    (setq prefix "\n"))))
		   (setq math-comp-buf (concat math-comp-buf prefix str "\n"
					  (make-string mrg ? ))
			 math-comp-pos (+ math-comp-pos (- mrg wid))
			 math-comp-margin mrg)
		   (aset math-comp-buf-string i "")
		   (while (<= (setq i (1+ i)) math-comp-level)
		     (if (> (aref math-comp-buf-margin i) wid)
			 (aset math-comp-buf-margin i
			       (+ (aref math-comp-buf-margin i)
				  (- mrg wid))))))))
	   (if (and (= (nth 1 c) (aref math-comp-buf-level math-comp-level))
		    (< math-comp-pos (+ (aref math-comp-buf-margin math-comp-level) 2)))
	       ()  ; avoid stupid breaks, e.g., "1 +\n really_long_expr"
	     (let ((str (aref math-comp-buf-string math-comp-level)))
	       (setq str (if (= (length str) 0)
			     math-comp-word
			   (concat str math-comp-word))
		     math-comp-word "")
	       (while (< (nth 1 c) (aref math-comp-buf-level math-comp-level))
		 (setq math-comp-level (1- math-comp-level))
		 (or (= (length (aref math-comp-buf-string math-comp-level)) 0)
		     (setq str (concat (aref math-comp-buf-string math-comp-level)
				       str))))
	       (aset math-comp-buf-string math-comp-level str)))))

	((eq (car c) 'tag)
	 (cond ((eq (nth 1 c) math-comp-selected)
		(let ((math-comp-highlight (not calc-show-selections)))
		  (math-comp-to-string-flat-term (nth 2 c))))
	       ((eq (nth 1 c) t)
		(let ((math-comp-highlight nil))
		  (math-comp-to-string-flat-term (nth 2 c))))
	       (t (math-comp-to-string-flat-term (nth 2 c)))))

	(t (math-comp-to-string-flat-term (nth 2 c)))))

(defun math-comp-highlight-string (s)
  (setq s (copy-sequence s))
  (if calc-highlight-selections-with-faces
      (if (not calc-show-selections)
          (propertize s 'face 'calc-selected-face)
        (propertize s 'face 'calc-nonselected-face))
    (let ((i (length s)))
      (while (>= (setq i (1- i)) 0)
        (or (memq (aref s i) '(32 ?\n))
            (aset s i (if calc-show-selections ?\. ?\#)))))
    s))

;; The variable math-comp-sel-tag is local to calc-find-selected-part
;; in calc-sel.el, but is used by math-comp-sel-flat-term and
;; math-comp-add-string-sel, which are called (indirectly) by
;; calc-find-selected-part.
(defvar math-comp-sel-tag)

(defun math-comp-sel-flat-term (c)
  (cond ((not (consp c))
	 (setq math-comp-pos (+ math-comp-pos (length c))))
	((memq (car c) '(set break)))
	((eq (car c) 'horiz)
	 (while (and (setq c (cdr c)) (< math-comp-sel-cpos 1000000))
	   (math-comp-sel-flat-term (car c))))
	((eq (car c) 'tag)
	 (if (<= math-comp-pos math-comp-sel-cpos)
	     (progn
	       (math-comp-sel-flat-term (nth 2 c))
	       (if (> math-comp-pos math-comp-sel-cpos)
		   (setq math-comp-sel-tag c
			 math-comp-sel-cpos 1000000)))
	   (math-comp-sel-flat-term (nth 2 c))))
	(t (math-comp-sel-flat-term (nth 2 c)))))


;;; Simplify a composition to a canonical form consisting of
;;;   (vleft n "string" "string" "string" ...)
;;; where 0 <= n < number-of-strings.

;; The variables math-comp-base, math-comp-hgt, math-comp-tag,
;; math-comp-hpos and math-comp-vpos are local to math-comp-simplify,
;; but are used by math-comp-add-string (math-comp-base, math-comp-hgt),
;; math-comp-add-string-sel (math-comp-tag) and math-comp-simplify-term
;; (math-comp-tag, math-comp-vpos, math-comp-hpos), which are called by
;; math-comp-simplify.
(defvar math-comp-base)
(defvar math-comp-hgt)
(defvar math-comp-tag)
(defvar math-comp-hpos)
(defvar math-comp-vpos)

(defun math-comp-simplify (c full-width)
  (let ((math-comp-buf (list ""))
	(math-comp-base 0)
	(math-comp-hgt 1)
	(math-comp-hpos 0)
	(math-comp-vpos 0)
	(math-comp-highlight (and math-comp-selected calc-show-selections))
	(math-comp-tag nil))
    (math-comp-simplify-term c)
    (cons 'vleft (cons math-comp-base math-comp-buf))))

(defun math-comp-add-string (s h v)
  (and (> (length s) 0)
       (let ((vv (+ v math-comp-base)))
	 (if math-comp-sel-hpos
	     (math-comp-add-string-sel h vv (length s) 1)
	   (if (< vv 0)
	       (setq math-comp-buf (nconc (make-list (- vv) "") math-comp-buf)
		     math-comp-base (- v)
		     math-comp-hgt (- math-comp-hgt vv)
		     vv 0)
	     (if (>= vv math-comp-hgt)
		 (setq math-comp-buf (nconc math-comp-buf
				       (make-list (1+ (- vv math-comp-hgt)) ""))
		       math-comp-hgt (1+ vv))))
	   (let ((str (nthcdr vv math-comp-buf)))
	     (setcar str (concat (car str)
				 (make-string (- h (length (car str))) 32)
				 (if math-comp-highlight
				     (math-comp-highlight-string s)
				   s))))))))

(defun math-comp-add-string-sel (x y w h)
  (if (and (<= y math-comp-sel-vpos)
	   (> (+ y h) math-comp-sel-vpos)
	   (<= x math-comp-sel-hpos)
	   (> (+ x w) math-comp-sel-hpos))
      (setq math-comp-sel-tag math-comp-tag
	    math-comp-sel-vpos 10000)))

(defun math-comp-simplify-term (c)
  (cond ((stringp c)
	 (math-comp-add-string c math-comp-hpos math-comp-vpos)
	 (setq math-comp-hpos (+ math-comp-hpos (length c))))
	((memq (car c) '(set break))
	 nil)
	((eq (car c) 'horiz)
	 (while (setq c (cdr c))
	   (math-comp-simplify-term (car c))))
	((memq (car c) '(vleft vcent vright))
	 (let* ((math-comp-vpos (+ (- math-comp-vpos (nth 1 c))
			      (1- (math-comp-ascent (nth 2 c)))))
		(widths (mapcar 'math-comp-width (cdr (cdr c))))
		(maxwid (apply 'max widths))
		(bias (cond ((eq (car c) 'vleft) 0)
			    ((eq (car c) 'vcent) 1)
			    (t 2))))
	   (setq c (cdr c))
	   (while (setq c (cdr c))
	     (if (eq (car-safe (car c)) 'rule)
		 (math-comp-add-string (make-string maxwid (nth 1 (car c)))
				       math-comp-hpos math-comp-vpos)
	       (let ((math-comp-hpos (+ math-comp-hpos (/ (* bias (- maxwid
							   (car widths)))
						2))))
		 (math-comp-simplify-term (car c))))
	     (and (cdr c)
		  (setq math-comp-vpos (+ math-comp-vpos
				     (+ (math-comp-descent (car c))
					(math-comp-ascent (nth 1 c))))
			widths (cdr widths))))
	   (setq math-comp-hpos (+ math-comp-hpos maxwid))))
	((eq (car c) 'supscr)
	 (let* ((asc (or 1 (math-comp-ascent (nth 1 c))))
		(desc (math-comp-descent (nth 2 c)))
		(oldh (prog1
			  math-comp-hpos
			(math-comp-simplify-term (nth 1 c))))
		(math-comp-vpos (- math-comp-vpos (+ asc desc))))
	   (math-comp-simplify-term (nth 2 c))
	   (if math-comp-sel-hpos
	       (math-comp-add-string-sel oldh
					 (- math-comp-vpos
					    -1
					    (math-comp-ascent (nth 2 c)))
					 (- math-comp-hpos oldh)
					 (math-comp-height c)))))
	((eq (car c) 'subscr)
	 (let* ((asc (math-comp-ascent (nth 2 c)))
		(desc (math-comp-descent (nth 1 c)))
		(oldv math-comp-vpos)
		(oldh (prog1
			  math-comp-hpos
			(math-comp-simplify-term (nth 1 c))))
		(math-comp-vpos (+ math-comp-vpos (+ asc desc))))
	   (math-comp-simplify-term (nth 2 c))
	   (if math-comp-sel-hpos
	       (math-comp-add-string-sel oldh oldv
					 (- math-comp-hpos oldh)
					 (math-comp-height c)))))
	((eq (car c) 'tag)
	 (cond ((eq (nth 1 c) math-comp-selected)
		(let ((math-comp-highlight (not calc-show-selections)))
		  (math-comp-simplify-term (nth 2 c))))
	       ((eq (nth 1 c) t)
		(let ((math-comp-highlight nil))
		  (math-comp-simplify-term (nth 2 c))))
	       (t (let ((math-comp-tag c))
		    (math-comp-simplify-term (nth 2 c))))))))


;;; Measuring a composition.

(defun math-comp-first-char (c)
  (cond ((stringp c)
	 (and (> (length c) 0)
	      (elt c 0)))
	((memq (car c) '(horiz subscr supscr))
	 (while (and (setq c (cdr c))
		     (math-comp-is-null (car c))))
	 (and c (math-comp-first-char (car c))))
	((eq (car c) 'tag)
	 (math-comp-first-char (nth 2 c)))))

(defun math-comp-first-string (c)
  (cond ((stringp c)
	 (and (> (length c) 0)
	      c))
	((eq (car c) 'horiz)
	 (while (and (setq c (cdr c))
		     (math-comp-is-null (car c))))
	 (and c (math-comp-first-string (car c))))
	((eq (car c) 'tag)
	 (math-comp-first-string (nth 2 c)))))

(defun math-comp-last-char (c)
  (cond ((stringp c)
	 (and (> (length c) 0)
	      (elt c (1- (length c)))))
	((eq (car c) 'horiz)
	 (let ((c (reverse (cdr c))))
	   (while (and c (math-comp-is-null (car c)))
	     (setq c (cdr c)))
	   (and c (math-comp-last-char (car c)))))
	((eq (car c) 'tag)
	 (math-comp-last-char (nth 2 c)))))

(defun math-comp-is-null (c)
  (cond ((stringp c) (= (length c) 0))
	((memq (car c) '(horiz subscr supscr))
	 (while (and (setq c (cdr c))
		     (math-comp-is-null (car c))))
	 (null c))
	((eq (car c) 'tag)
	 (math-comp-is-null (nth 2 c)))
	((memq (car c) '(set break)) t)))

(defun math-comp-width (c)
  (cond ((not (consp c)) (length c))
	((memq (car c) '(horiz subscr supscr))
	 (let ((accum 0))
	   (while (setq c (cdr c))
	     (setq accum (+ accum (math-comp-width (car c)))))
	   accum))
	((memq (car c) '(vcent vleft vright))
	 (setq c (cdr c))
	 (let ((accum 0))
	   (while (setq c (cdr c))
	     (setq accum (max accum (math-comp-width (car c)))))
	   accum))
	((eq (car c) 'tag)
	 (math-comp-width (nth 2 c)))
	(t 0)))

(defun math-comp-height (c)
  (if (stringp c)
      1
    (+ (math-comp-ascent c) (math-comp-descent c))))

(defun math-comp-ascent (c)
  (cond ((not (consp c)) 1)
	((eq (car c) 'horiz)
	 (let ((accum 0))
	   (while (setq c (cdr c))
	     (setq accum (max accum (math-comp-ascent (car c)))))
	   accum))
	((memq (car c) '(vcent vleft vright))
	 (if (> (nth 1 c) 0) (1+ (nth 1 c)) 1))
	((eq (car c) 'supscr)
	 (max (math-comp-ascent (nth 1 c)) (1+ (math-comp-height (nth 2 c)))))
	((eq (car c) 'subscr)
	 (math-comp-ascent (nth 1 c)))
	((eq (car c) 'tag)
	 (math-comp-ascent (nth 2 c)))
	(t 1)))

(defun math-comp-descent (c)
  (cond ((not (consp c)) 0)
	((eq (car c) 'horiz)
	 (let ((accum 0))
	   (while (setq c (cdr c))
	     (setq accum (max accum (math-comp-descent (car c)))))
	   accum))
	((memq (car c) '(vcent vleft vright))
	 (let ((accum (- (nth 1 c))))
	   (setq c (cdr c))
	   (while (setq c (cdr c))
	     (setq accum (+ accum (math-comp-height (car c)))))
	   (max (1- accum) 0)))
	((eq (car c) 'supscr)
	 (math-comp-descent (nth 1 c)))
	((eq (car c) 'subscr)
	 (+ (math-comp-descent (nth 1 c)) (math-comp-height (nth 2 c))))
	((eq (car c) 'tag)
	 (math-comp-descent (nth 2 c)))
	(t 0)))

(defun calcFunc-cwidth (a &optional prec)
  (if (and prec (not (integerp prec))) (math-reject-arg prec 'fixnump))
  (math-comp-width (math-compose-expr a (or prec 0))))

(defun calcFunc-cheight (a &optional prec)
  (if (and prec (not (integerp prec))) (math-reject-arg prec 'fixnump))
  (if (and (memq (car a) '(calcFunc-cvspace calcFunc-ctspace calcFunc-cbspace))
	   (memq (length a) '(2 3))
	   (eq (nth 1 a) 0))
      0
    (math-comp-height (math-compose-expr a (or prec 0)))))

(defun calcFunc-cascent (a &optional prec)
  (if (and prec (not (integerp prec))) (math-reject-arg prec 'fixnump))
  (if (and (memq (car a) '(calcFunc-cvspace calcFunc-ctspace calcFunc-cbspace))
	   (memq (length a) '(2 3))
	   (eq (nth 1 a) 0))
      0
    (math-comp-ascent (math-compose-expr a (or prec 0)))))

(defun calcFunc-cdescent (a &optional prec)
  (if (and prec (not (integerp prec))) (math-reject-arg prec 'fixnump))
  (math-comp-descent (math-compose-expr a (or prec 0))))


;;; Convert a simplified composition into string form.

(defun math-vert-comp-to-string (c)
  (if (stringp c)
      c
    (math-vert-comp-to-string-step (cdr (cdr c)))))

(defun math-vert-comp-to-string-step (c)
  (if (cdr c)
      (concat (car c) "\n" (math-vert-comp-to-string-step (cdr c)))
    (car c)))


;;; Convert a composition to a string in "raw" form (for debugging).

(defun math-comp-to-string-raw (c indent)
  (cond ((or (not (consp c)) (eq (car c) 'set))
	 (prin1-to-string c))
	((null (cdr c))
	 (concat "(" (symbol-name (car c)) ")"))
	(t
	 (let ((next-indent (+ indent 2 (length (symbol-name (car c))))))
	   (concat "("
		   (symbol-name (car c))
		   " "
		   (math-comp-to-string-raw (nth 1 c) next-indent)
		   (math-comp-to-string-raw-step (cdr (cdr c))
						 next-indent)
		   ")")))))

(defun math-comp-to-string-raw-step (cl indent)
  (if cl
      (concat "\n"
	      (make-string indent 32)
	      (math-comp-to-string-raw (car cl) indent)
	      (math-comp-to-string-raw-step (cdr cl) indent))
    ""))

(provide 'calccomp)

;; Local variables:
;; coding: utf-8
;; End:

;;; calccomp.el ends here
