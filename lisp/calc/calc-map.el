;;; calc-map.el --- higher-order functions for Calc

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

(defun calc-apply (&optional oper)
  (interactive)
  (calc-wrapper
   (let* ((sel-mode nil)
	  (calc-dollar-values (mapcar 'calc-get-stack-element
				      (nthcdr calc-stack-top calc-stack)))
	  (calc-dollar-used 0)
	  (oper (or oper (calc-get-operator "Apply"
					    (if (math-vectorp (calc-top 1))
						(1- (length (calc-top 1)))
					      -1))))
	  (expr (calc-top-n (1+ calc-dollar-used))))
     (message "Working...")
     (calc-set-command-flag 'clear-message)
     (calc-enter-result (1+ calc-dollar-used)
			(concat (substring "apl" 0 (- 4 (length (nth 2 oper))))
				(nth 2 oper))
			(list 'calcFunc-apply
			      (math-calcFunc-to-var (nth 1 oper))
			      expr)))))

(defun calc-reduce (&optional oper accum)
  (interactive)
  (calc-wrapper
   (let* ((sel-mode nil)
	  (nest (calc-is-hyperbolic))
	  (rev (calc-is-inverse))
	  (nargs (if (and nest (not rev)) 2 1))
	  (calc-dollar-values (mapcar 'calc-get-stack-element
				      (nthcdr calc-stack-top calc-stack)))
	  (calc-dollar-used 0)
	  (calc-mapping-dir (and (not accum) (not nest) ""))
	  (oper (or oper (calc-get-operator
			  (if nest
			      (concat (if accum "Accumulate " "")
				      (if rev "Fixed Point" "Nest"))
			    (concat (if rev "Inv " "")
				    (if accum "Accumulate" "Reduce")))
			  (if nest 1 2)))))
     (message "Working...")
     (calc-set-command-flag 'clear-message)
     (calc-enter-result (+ calc-dollar-used nargs)
			(concat (substring (if nest
					       (if rev "fxp" "nst")
					     (if accum "acc" "red"))
					   0 (- 4 (length (nth 2 oper))))
				(nth 2 oper))
			(if nest
			    (cons (if rev
				      (if accum 'calcFunc-afixp 'calcFunc-fixp)
				    (if accum 'calcFunc-anest 'calcFunc-nest))
				  (cons (math-calcFunc-to-var (nth 1 oper))
					(calc-top-list-n
					 nargs (1+ calc-dollar-used))))
			  (list (if accum
				    (if rev 'calcFunc-raccum 'calcFunc-accum)
				  (intern (concat "calcFunc-"
						  (if rev "r" "")
						  "reduce"
						  calc-mapping-dir)))
				(math-calcFunc-to-var (nth 1 oper))
				(calc-top-n (1+ calc-dollar-used))))))))

(defun calc-accumulate (&optional oper)
  (interactive)
  (calc-reduce oper t))

(defun calc-map (&optional oper)
  (interactive)
  (calc-wrapper
   (let* ((sel-mode nil)
	  (calc-dollar-values (mapcar 'calc-get-stack-element
				      (nthcdr calc-stack-top calc-stack)))
	  (calc-dollar-used 0)
	  (calc-mapping-dir "")
	  (oper (or oper (calc-get-operator "Map")))
	  (nargs (car oper)))
     (message "Working...")
     (calc-set-command-flag 'clear-message)
     (calc-enter-result (+ nargs calc-dollar-used)
			(concat (substring "map" 0 (- 4 (length (nth 2 oper))))
				(nth 2 oper))
			(cons (intern (concat "calcFunc-map" calc-mapping-dir))
			      (cons (math-calcFunc-to-var (nth 1 oper))
				    (calc-top-list-n
				     nargs
				     (1+ calc-dollar-used))))))))

(defun calc-map-equation (&optional oper)
  (interactive)
  (calc-wrapper
   (let* ((sel-mode nil)
	  (calc-dollar-values (mapcar 'calc-get-stack-element
				      (nthcdr calc-stack-top calc-stack)))
	  (calc-dollar-used 0)
	  (oper (or oper (calc-get-operator "Map-equation")))
	  (nargs (car oper)))
     (message "Working...")
     (calc-set-command-flag 'clear-message)
     (calc-enter-result (+ nargs calc-dollar-used)
			(concat (substring "map" 0 (- 4 (length (nth 2 oper))))
				(nth 2 oper))
			(cons (if (calc-is-inverse)
				  'calcFunc-mapeqr
				(if (calc-is-hyperbolic)
				    'calcFunc-mapeqp 'calcFunc-mapeq))
			      (cons (math-calcFunc-to-var (nth 1 oper))
				    (calc-top-list-n
				     nargs
				     (1+ calc-dollar-used))))))))

(defvar calc-verify-arglist t)
(defvar calc-mapping-dir nil)
(defun calc-map-stack ()
  "This is meant to be called by calc-keypad mode."
  (interactive)
  (let ((calc-verify-arglist nil))
    (calc-unread-command ?\$)
    (calc-map)))

(defun calc-outer-product (&optional oper)
  (interactive)
  (calc-wrapper
   (let* ((sel-mode nil)
	  (calc-dollar-values (mapcar 'calc-get-stack-element
				      (nthcdr calc-stack-top calc-stack)))
	  (calc-dollar-used 0)
	  (oper (or oper (calc-get-operator "Outer" 2))))
     (message "Working...")
     (calc-set-command-flag 'clear-message)
     (calc-enter-result (+ 2 calc-dollar-used)
			(concat (substring "out" 0 (- 4 (length (nth 2 oper))))
				(nth 2 oper))
			(cons 'calcFunc-outer
			      (cons (math-calcFunc-to-var (nth 1 oper))
				    (calc-top-list-n
				     2 (1+ calc-dollar-used))))))))

(defun calc-inner-product (&optional mul-oper add-oper)
  (interactive)
  (calc-wrapper
   (let* ((sel-mode nil)
	  (calc-dollar-values (mapcar 'calc-get-stack-element
				      (nthcdr calc-stack-top calc-stack)))
	  (calc-dollar-used 0)
	  (mul-oper (or mul-oper (calc-get-operator "Inner (Mult)" 2)))
	  (mul-used calc-dollar-used)
	  (calc-dollar-values (if (> mul-used 0)
				  (cdr calc-dollar-values)
				calc-dollar-values))
	  (calc-dollar-used 0)
	  (add-oper (or add-oper (calc-get-operator "Inner (Add)" 2))))
     (message "Working...")
     (calc-set-command-flag 'clear-message)
     (calc-enter-result (+ 2 mul-used calc-dollar-used)
			(concat "in"
				(substring (nth 2 mul-oper) 0 1)
				(substring (nth 2 add-oper) 0 1))
			(nconc (list 'calcFunc-inner
				     (math-calcFunc-to-var (nth 1 mul-oper))
				     (math-calcFunc-to-var (nth 1 add-oper)))
			       (calc-top-list-n
				2 (+ 1 mul-used calc-dollar-used)))))))

(defconst calc-oper-keys '( ( ( ?+ 2 calcFunc-add )
			      ( ?- 2 calcFunc-sub )
			      ( ?* 2 calcFunc-mul )
			      ( ?/ 2 calcFunc-div )
			      ( ?^ 2 calcFunc-pow )
			      ( ?| 2 calcFunc-vconcat )
			      ( ?% 2 calcFunc-mod )
			      ( ?\\ 2 calcFunc-idiv )
			      ( ?! 1 calcFunc-fact )
			      ( ?& 1 calcFunc-inv )
			      ( ?n 1 calcFunc-neg )
			      ( ?x user )
			      ( ?z user )
			      ( ?A 1 calcFunc-abs )
			      ( ?J 1 calcFunc-conj )
			      ( ?G 1 calcFunc-arg )
			      ( ?Q 1 calcFunc-sqrt )
			      ( ?N 2 calcFunc-min )
			      ( ?X 2 calcFunc-max )
			      ( ?F 1 calcFunc-floor )
			      ( ?R 1 calcFunc-round )
			      ( ?S 1 calcFunc-sin )
			      ( ?C 1 calcFunc-cos )
			      ( ?T 1 calcFunc-tan )
			      ( ?L 1 calcFunc-ln )
			      ( ?E 1 calcFunc-exp )
			      ( ?B 2 calcFunc-log ) )
			    ( ( ?F 1 calcFunc-ceil )     ; inverse
			      ( ?R 1 calcFunc-trunc )
			      ( ?Q 1 calcFunc-sqr )
			      ( ?S 1 calcFunc-arcsin )
			      ( ?C 1 calcFunc-arccos )
			      ( ?T 1 calcFunc-arctan )
			      ( ?L 1 calcFunc-exp )
			      ( ?E 1 calcFunc-ln )
			      ( ?B 2 calcFunc-alog )
			      ( ?^ 2 calcFunc-nroot )
			      ( ?| 2 calcFunc-vconcatrev ) )
			    ( ( ?F 1 calcFunc-ffloor )   ; hyperbolic
			      ( ?R 1 calcFunc-fround )
			      ( ?S 1 calcFunc-sinh )
			      ( ?C 1 calcFunc-cosh )
			      ( ?T 1 calcFunc-tanh )
			      ( ?L 1 calcFunc-log10 )
			      ( ?E 1 calcFunc-exp10 )
			      ( ?| 2 calcFunc-append ) )
			    ( ( ?F 1 calcFunc-fceil )    ; inverse-hyperbolic
			      ( ?R 1 calcFunc-ftrunc )
			      ( ?S 1 calcFunc-arcsinh )
			      ( ?C 1 calcFunc-arccosh )
			      ( ?T 1 calcFunc-arctanh )
			      ( ?L 1 calcFunc-exp10 )
			      ( ?E 1 calcFunc-log10 )
			      ( ?| 2 calcFunc-appendrev ) )))

(defconst calc-a-oper-keys '( ( ( ?a 3 calcFunc-apart )
				( ?b 3 calcFunc-subst )
				( ?c 2 calcFunc-collect )
				( ?d 2 calcFunc-deriv )
				( ?e 1 calcFunc-esimplify )
				( ?f 2 calcFunc-factor )
				( ?g 2 calcFunc-pgcd )
				( ?i 2 calcFunc-integ )
				( ?m 2 calcFunc-match )
				( ?n 1 calcFunc-nrat )
				( ?r 2 calcFunc-rewrite )
				( ?s 1 calcFunc-simplify )
				( ?t 3 calcFunc-taylor )
				( ?x 1 calcFunc-expand )
				( ?M 2 calcFunc-mapeq )
				( ?N 3 calcFunc-minimize )
				( ?P 2 calcFunc-roots )
				( ?R 3 calcFunc-root )
				( ?S 2 calcFunc-solve )
				( ?T 4 calcFunc-table )
				( ?X 3 calcFunc-maximize )
				( ?= 2 calcFunc-eq )
				( ?\# 2 calcFunc-neq )
				( ?< 2 calcFunc-lt )
				( ?> 2 calcFunc-gt )
				( ?\[ 2 calcFunc-leq )
				( ?\] 2 calcFunc-geq )
				( ?{ 2 calcFunc-in )
				( ?! 1 calcFunc-lnot )
				( ?& 2 calcFunc-land )
				( ?\| 2 calcFunc-lor )
				( ?: 3 calcFunc-if )
				( ?. 2 calcFunc-rmeq )
				( ?+ 4 calcFunc-sum )
				( ?- 4 calcFunc-asum )
				( ?* 4 calcFunc-prod )
				( ?_ 2 calcFunc-subscr )
				( ?\\ 2 calcFunc-pdiv )
				( ?% 2 calcFunc-prem )
				( ?/ 2 calcFunc-pdivrem ) )
			      ( ( ?m 2 calcFunc-matchnot )
				( ?M 2 calcFunc-mapeqr )
				( ?S 2 calcFunc-finv ) )
			      ( ( ?d 2 calcFunc-tderiv )
				( ?f 2 calcFunc-factors )
				( ?M 2 calcFunc-mapeqp )
				( ?N 3 calcFunc-wminimize )
				( ?R 3 calcFunc-wroot )
				( ?S 2 calcFunc-fsolve )
				( ?X 3 calcFunc-wmaximize )
				( ?/ 2 calcFunc-pdivide ) )
			      ( ( ?S 2 calcFunc-ffinv ) )))

(defconst calc-b-oper-keys '( ( ( ?a 2 calcFunc-and )
				( ?o 2 calcFunc-or )
				( ?x 2 calcFunc-xor )
				( ?d 2 calcFunc-diff )
				( ?n 1 calcFunc-not )
				( ?c 1 calcFunc-clip )
				( ?l 2 calcFunc-lsh )
				( ?r 2 calcFunc-rsh )
				( ?L 2 calcFunc-ash )
				( ?R 2 calcFunc-rash )
				( ?t 2 calcFunc-rot )
				( ?p 1 calcFunc-vpack )
				( ?u 1 calcFunc-vunpack )
				( ?D 4 calcFunc-ddb )
				( ?F 3 calcFunc-fv )
				( ?I 1 calcFunc-irr )
				( ?M 3 calcFunc-pmt )
				( ?N 2 calcFunc-npv )
				( ?P 3 calcFunc-pv )
				( ?S 3 calcFunc-sln )
				( ?T 3 calcFunc-rate )
				( ?Y 4 calcFunc-syd )
				( ?\# 3 calcFunc-nper )
				( ?\% 2 calcFunc-relch ) )
			      ( ( ?F 3 calcFunc-fvb )
				( ?I 1 calcFunc-irrb )
				( ?M 3 calcFunc-pmtb )
				( ?N 2 calcFunc-npvb )
				( ?P 3 calcFunc-pvb )
				( ?T 3 calcFunc-rateb )
				( ?\# 3 calcFunc-nperb ) )
			      ( ( ?F 3 calcFunc-fvl )
				( ?M 3 calcFunc-pmtl )
				( ?P 3 calcFunc-pvl )
				( ?T 3 calcFunc-ratel )
				( ?\# 3 calcFunc-nperl ) )))

(defconst calc-c-oper-keys '( ( ( ?d 1 calcFunc-deg )
				( ?r 1 calcFunc-rad )
				( ?h 1 calcFunc-hms )
				( ?f 1 calcFunc-float )
				( ?F 1 calcFunc-frac ) )))

(defconst calc-f-oper-keys '( ( ( ?b 2 calcFunc-beta )
				( ?e 1 calcFunc-erf )
				( ?g 1 calcFunc-gamma )
				( ?h 2 calcFunc-hypot )
				( ?i 1 calcFunc-im )
				( ?j 2 calcFunc-besJ )
				( ?n 2 calcFunc-min )
				( ?r 1 calcFunc-re )
				( ?s 1 calcFunc-sign )
				( ?x 2 calcFunc-max )
				( ?y 2 calcFunc-besY )
				( ?A 1 calcFunc-abssqr )
				( ?B 3 calcFunc-betaI )
				( ?E 1 calcFunc-expm1 )
				( ?G 2 calcFunc-gammaP )
				( ?I 2 calcFunc-ilog )
				( ?L 1 calcFunc-lnp1 )
				( ?M 1 calcFunc-mant )
				( ?Q 1 calcFunc-isqrt )
				( ?S 1 calcFunc-scf )
				( ?T 2 calcFunc-arctan2 )
				( ?X 1 calcFunc-xpon )
				( ?\[ 2 calcFunc-decr )
				( ?\] 2 calcFunc-incr ) )
			      ( ( ?e 1 calcFunc-erfc )
				( ?E 1 calcFunc-lnp1 )
				( ?G 2 calcFunc-gammaQ )
				( ?L 1 calcFunc-expm1 ) )
			      ( ( ?B 3 calcFunc-betaB )
				( ?G 2 calcFunc-gammag) )
			      ( ( ?G 2 calcFunc-gammaG ) )))

(defconst calc-k-oper-keys '( ( ( ?b 1 calcFunc-bern )
				( ?c 2 calcFunc-choose )
				( ?d 1 calcFunc-dfact )
				( ?e 1 calcFunc-euler )
				( ?f 1 calcFunc-prfac )
				( ?g 2 calcFunc-gcd )
				( ?h 2 calcFunc-shuffle )
				( ?l 2 calcFunc-lcm )
				( ?m 1 calcFunc-moebius )
				( ?n 1 calcFunc-nextprime )
				( ?r 1 calcFunc-random )
				( ?s 2 calcFunc-stir1 )
				( ?t 1 calcFunc-totient )
				( ?B 3 calcFunc-utpb )
				( ?C 2 calcFunc-utpc )
				( ?F 3 calcFunc-utpf )
				( ?N 3 calcFunc-utpn )
				( ?P 2 calcFunc-utpp )
				( ?T 2 calcFunc-utpt ) )
			      ( ( ?n 1 calcFunc-prevprime )
				( ?B 3 calcFunc-ltpb )
				( ?C 2 calcFunc-ltpc )
				( ?F 3 calcFunc-ltpf )
				( ?N 3 calcFunc-ltpn )
				( ?P 2 calcFunc-ltpp )
				( ?T 2 calcFunc-ltpt ) )
			      ( ( ?b 2 calcFunc-bern )
				( ?c 2 calcFunc-perm )
				( ?e 2 calcFunc-euler )
				( ?s 2 calcFunc-stir2 ) )))

(defconst calc-s-oper-keys '( ( ( ?: 2 calcFunc-assign )
				( ?= 1 calcFunc-evalto ) )))

(defconst calc-t-oper-keys '( ( ( ?C 3 calcFunc-tzconv )
				( ?D 1 calcFunc-date )
				( ?I 2 calcFunc-incmonth )
				( ?J 1 calcFunc-julian )
				( ?M 1 calcFunc-newmonth )
				( ?W 1 calcFunc-newweek )
				( ?U 1 calcFunc-unixtime )
				( ?Y 1 calcFunc-newyear ) )))

(defconst calc-u-oper-keys '( ( ( ?C 2 calcFunc-vcov )
				( ?G 1 calcFunc-vgmean )
				( ?M 1 calcFunc-vmean )
				( ?N 1 calcFunc-vmin )
				( ?S 1 calcFunc-vsdev )
				( ?X 1 calcFunc-vmax ) )
			      ( ( ?C 2 calcFunc-vpcov )
				( ?M 1 calcFunc-vmeane )
				( ?S 1 calcFunc-vpsdev ) )
			      ( ( ?C 2 calcFunc-vcorr )
				( ?G 1 calcFunc-agmean )
				( ?M 1 calcFunc-vmedian )
				( ?S 1 calcFunc-vvar ) )
			      ( ( ?M 1 calcFunc-vhmean )
				( ?S 1 calcFunc-vpvar ) )))

(defconst calc-v-oper-keys '( ( ( ?a 2 calcFunc-arrange )
				( ?b 2 calcFunc-cvec )
				( ?c 2 calcFunc-mcol )
				( ?d 2 calcFunc-diag )
				( ?e 2 calcFunc-vexp )
				( ?f 2 calcFunc-find )
				( ?h 1 calcFunc-head )
				( ?k 2 calcFunc-cons )
				( ?l 1 calcFunc-vlen )
				( ?m 2 calcFunc-vmask )
				( ?n 1 calcFunc-rnorm )
				( ?p 2 calcFunc-pack )
				( ?r 2 calcFunc-mrow )
				( ?s 3 calcFunc-subvec )
				( ?t 1 calcFunc-trn )
				( ?u 1 calcFunc-unpack )
				( ?v 1 calcFunc-rev )
				( ?x 1 calcFunc-index )
				( ?A 1 calcFunc-apply )
				( ?C 1 calcFunc-cross )
				( ?D 1 calcFunc-det )
				( ?E 1 calcFunc-venum )
				( ?F 1 calcFunc-vfloor )
				( ?G 1 calcFunc-grade )
				( ?H 2 calcFunc-histogram )
				( ?I 2 calcFunc-inner )
				( ?L 1 calcFunc-lud )
				( ?M 0 calcFunc-map )
				( ?N 1 calcFunc-cnorm )
				( ?O 2 calcFunc-outer )
				( ?R 1 calcFunc-reduce )
				( ?S 1 calcFunc-sort )
				( ?T 1 calcFunc-tr )
				( ?U 1 calcFunc-accum )
				( ?V 2 calcFunc-vunion )
				( ?X 2 calcFunc-vxor )
				( ?- 2 calcFunc-vdiff )
				( ?^ 2 calcFunc-vint )
				( ?~ 1 calcFunc-vcompl )
				( ?# 1 calcFunc-vcard )
				( ?: 1 calcFunc-vspan )
				( ?+ 1 calcFunc-rdup ) )
			      ( ( ?h 1 calcFunc-tail )
				( ?s 3 calcFunc-rsubvec )
				( ?G 1 calcFunc-rgrade )
				( ?R 1 calcFunc-rreduce )
				( ?S 1 calcFunc-rsort )
				( ?U 1 calcFunc-raccum ) )
			      ( ( ?e 3 calcFunc-vexp )
				( ?h 1 calcFunc-rhead )
				( ?k 2 calcFunc-rcons )
				( ?H 3 calcFunc-histogram )
				( ?R 2 calcFunc-nest )
				( ?U 2 calcFunc-anest ) )
			      ( ( ?h 1 calcFunc-rtail )
				( ?R 1 calcFunc-fixp )
				( ?U 1 calcFunc-afixp ) )))


;;; Return a list of the form (nargs func name)
(defvar calc-get-operator-history nil
  "History for calc-get-operator.")

(defun calc-get-operator (msg &optional nargs)
  (setq calc-aborted-prefix nil)
  (let ((inv nil) (hyp nil) (prefix nil) (forcenargs nil)
	done key oper (which 0)
	(msgs '( "(Press ? for help)"
		 "+, -, *, /, ^, %, \\, :, &, !, |, Neg"
		 "SHIFT + Abs, conJ, arG; maX, miN; Floor, Round; sQrt"
		 "SHIFT + Inv, Hyp; Sin, Cos, Tan; Exp, Ln, logB"
		 "Algebra + Simp, Esimp, Deriv, Integ, !, =, etc."
		 "Binary + And, Or, Xor, Diff; l/r/t/L/R shifts; Not, Clip"
		 "Conversions + Deg, Rad, HMS; Float; SHIFT + Fraction"
		 "Functions + Re, Im; Hypot; Mant, Expon, Scale; etc."
		 "Kombinatorics + Dfact, Lcm, Gcd, Choose; Random; etc."
		 "Time/date + newYear, Incmonth, etc."
		 "Vectors + Length, Row, Col, Diag, Mask, etc."
		 "_ = mapr/reducea, : = mapc/reduced, = = reducer"
		 "X or Z = any function by name; ' = alg entry; $ = stack")))
    (while (not done)
      (message "%s%s: %s: %s%s%s"
	       msg
	       (cond ((equal calc-mapping-dir "r") " rows")
		     ((equal calc-mapping-dir "c") " columns")
		     ((equal calc-mapping-dir "a") " across")
		     ((equal calc-mapping-dir "d") " down")
		     (t ""))
	       (if forcenargs
		   (format "(%d arg%s)"
			   forcenargs (if (= forcenargs 1) "" "s"))
		 (nth which msgs))
	       (if inv "Inv " "") (if hyp "Hyp " "")
	       (if prefix (concat (char-to-string prefix) "-") ""))
      (setq key (read-char))
      (if (>= key 128) (setq key (- key 128)))
      (cond ((memq key '(?\C-g ?q))
	     (keyboard-quit))
	    ((memq key '(?\C-u ?\e)))
	    ((= key ??)
	     (setq which (% (1+ which) (length msgs))))
	    ((and (= key ?I) (null prefix))
	     (setq inv (not inv)))
	    ((and (= key ?H) (null prefix))
	     (setq hyp (not hyp)))
	    ((and (eq key prefix) (not (eq key ?v)))
	     (setq prefix nil))
	    ((and (memq key '(?a ?b ?c ?f ?k ?s ?t ?u ?v ?V))
		  (null prefix))
	     (setq prefix (downcase key)))
	    ((and (eq key ?\=) (null prefix))
	     (if calc-mapping-dir
		 (setq calc-mapping-dir (if (equal calc-mapping-dir "r")
					    "" "r"))
	       (beep)))
	    ((and (eq key ?\_) (null prefix))
	     (if calc-mapping-dir
		 (if (string-match "map$" msg)
		     (setq calc-mapping-dir (if (equal calc-mapping-dir "r")
						"" "r"))
		   (setq calc-mapping-dir (if (equal calc-mapping-dir "a")
					      "" "a")))
	       (beep)))
	    ((and (eq key ?\:) (null prefix))
	     (if calc-mapping-dir
		 (if (string-match "map$" msg)
		     (setq calc-mapping-dir (if (equal calc-mapping-dir "c")
						"" "c"))
		   (setq calc-mapping-dir (if (equal calc-mapping-dir "d")
					      "" "d")))
	       (beep)))
	    ((and (>= key ?0) (<= key ?9) (null prefix))
	     (setq forcenargs (if (eq forcenargs (- key ?0)) nil (- key ?0)))
	     (and nargs forcenargs (/= nargs forcenargs) (>= nargs 0)
		  (error "Must be a %d-argument operator" nargs)))
	    ((memq key '(?\$ ?\'))
	     (let* ((math-arglist nil)
		    (has-args nil)
		    (record-entry nil)
		    (expr (if (eq key ?\$)
			      (progn
				(setq calc-dollar-used 1)
				(if calc-dollar-values
				    (car calc-dollar-values)
				  (error "Stack underflow")))
			    (let* ((calc-dollar-values calc-arg-values)
				   (calc-dollar-used 0)
				   (calc-hashes-used 0)
				   (func (calc-do-alg-entry "" "Function: " nil
                                                      'calc-get-operator-history)))
			      (setq record-entry t)
			      (or (= (length func) 1)
				  (error "Bad format"))
			      (if (> calc-dollar-used 0)
				  (progn
				    (setq has-args calc-dollar-used
					  math-arglist (calc-invent-args has-args))
				    (math-multi-subst (car func)
						      (reverse math-arglist)
						      math-arglist))
				(if (> calc-hashes-used 0)
				    (setq has-args calc-hashes-used
					  math-arglist (calc-invent-args has-args)))
				(car func))))))
	       (if (eq (car-safe expr) 'calcFunc-lambda)
		   (setq oper (list "$" (- (length expr) 2) expr)
			 done t)
		 (or has-args
		     (progn
		       (calc-default-formula-arglist expr)
		       (setq record-entry t
			     math-arglist (sort math-arglist 'string-lessp))
		       (if calc-verify-arglist
			   (setq math-arglist (read-from-minibuffer
					  "Function argument list: "
					  (if math-arglist
					      (prin1-to-string math-arglist)
					    "()")
					  minibuffer-local-map
					  t)))
		       (setq math-arglist (mapcar (function
					      (lambda (x)
						(list 'var
						      x
						      (intern
						       (concat
							"var-"
							(symbol-name x))))))
					     math-arglist))))
		 (setq oper (list "$"
				  (length math-arglist)
				  (append '(calcFunc-lambda) math-arglist
					  (list expr)))
		       done t))
	       (if record-entry
		   (calc-record (nth 2 oper) "oper"))))
	    ((setq oper (assq key (nth (if inv (if hyp 3 1) (if hyp 2 0))
				       (if prefix
					   (symbol-value
					    (intern (format "calc-%c-oper-keys"
							    prefix)))
					 calc-oper-keys))))
	     (if (eq (nth 1 oper) 'user)
		 (let ((func (intern
			      (completing-read "Function name: "
					       obarray 'fboundp
					       nil "calcFunc-"))))
		   (if (or forcenargs nargs)
		       (setq oper (list "z" (or forcenargs nargs) func)
			     done t)
		     (if (fboundp func)
			 (let* ((defn (symbol-function func)))
			   (and (symbolp defn)
				(setq defn (symbol-function defn)))
			   (if (eq (car-safe defn) 'lambda)
			       (let ((args (nth 1 defn))
				     (nargs 0))
				 (while (not (memq (car args) '(&optional
								&rest nil)))
				   (setq nargs (1+ nargs)
					 args (cdr args)))
				 (setq oper (list "z" nargs func)
				       done t))
			     (error
			      "Function is not suitable for this operation")))
		       (message "Number of arguments: ")
		       (let ((nargs (read-char)))
			 (if (and (>= nargs ?0) (<= nargs ?9))
			     (setq oper (list "z" (- nargs ?0) func)
				   done t)
			   (beep))))))
	       (if (or (and (eq prefix ?v) (memq key '(?A ?I ?M ?O ?R ?U)))
		       (and (eq prefix ?a) (eq key ?M)))
		   (let* ((dir (cond ((and (equal calc-mapping-dir "")
					   (string-match "map$" msg))
				      (setq calc-mapping-dir "r")
				      " rows")
				     ((equal calc-mapping-dir "r") " rows")
				     ((equal calc-mapping-dir "c") " columns")
				     ((equal calc-mapping-dir "a") " across")
				     ((equal calc-mapping-dir "d") " down")
				     (t "")))
			  (calc-mapping-dir (and (memq (nth 2 oper)
						       '(calcFunc-map
							 calcFunc-reduce
							 calcFunc-rreduce))
						 ""))
			  (oper2 (calc-get-operator
				  (format "%s%s, %s%s" msg dir
					  (substring (symbol-name (nth 2 oper))
						     9)
					  (if (eq key ?I) " (mult)" ""))
				  (cdr (assq (nth 2 oper)
					     '((calcFunc-reduce  . 2)
					       (calcFunc-rreduce . 2)
					       (calcFunc-accum   . 2)
					       (calcFunc-raccum  . 2)
					       (calcFunc-nest    . 2)
					       (calcFunc-anest   . 2)
					       (calcFunc-fixp    . 2)
					       (calcFunc-afixp   . 2))))))
			  (oper3 (if (eq (nth 2 oper) 'calcFunc-inner)
				     (calc-get-operator
				      (format "%s%s, inner (add)" msg dir))
				   '(0 0 0)))
			  (args nil)
			  (nargs (if (> (nth 1 oper) 0)
				     (nth 1 oper)
				   (car oper2)))
			  (n nargs)
			  (p calc-arg-values))
		     (while (and p (> n 0))
		       (or (math-expr-contains (nth 1 oper2) (car p))
			   (math-expr-contains (nth 1 oper3) (car p))
			   (setq args (nconc args (list (car p)))
				 n (1- n)))
		       (setq p (cdr p)))
		     (setq oper (list "" nargs
				      (append
				       '(calcFunc-lambda)
				       args
				       (list (math-build-call
					      (intern
					       (concat
						(symbol-name (nth 2 oper))
						calc-mapping-dir))
					      (cons (math-calcFunc-to-var
						     (nth 1 oper2))
						    (if (eq key ?I)
							(cons
							 (math-calcFunc-to-var
							  (nth 1 oper3))
							 args)
						      args))))))
			   done t))
		 (setq done t))))
	    (t (beep))))
    (and nargs (>= nargs 0)
	 (/= nargs (nth 1 oper))
	 (error "Must be a %d-argument operator" nargs))
    (append (if forcenargs
		(cons forcenargs (cdr (cdr oper)))
	      (cdr oper))
	    (list
	     (let ((name (concat (if inv "I" "") (if hyp "H" "")
				 (if prefix (char-to-string prefix) "")
				 (char-to-string key))))
	       (if (> (length name) 3)
		   (substring name 0 3)
		 name))))))


;;; Convert a variable name (as a formula) into a like-looking function name.
(defun math-var-to-calcFunc (f)
  (if (eq (car-safe f) 'var)
      (if (fboundp (nth 2 f))
	  (nth 2 f)
	(intern (concat "calcFunc-" (symbol-name (nth 1 f)))))
    (if (memq (car-safe f) '(lambda calcFunc-lambda))
	f
      (math-reject-arg f "*Expected a function name"))))

;;; Convert a function name into a like-looking variable name formula.
(defun math-calcFunc-to-var (f)
  (if (symbolp f)
      (let* ((func (or (cdr (assq f '( ( + . calcFunc-add )
				       ( - . calcFunc-sub )
				       ( * . calcFunc-mul )
				       ( / . calcFunc-div )
				       ( ^ . calcFunc-pow )
				       ( % . calcFunc-mod )
				       ( neg . calcFunc-neg )
				       ( | . calcFunc-vconcat ) )))
		       f))
	     (base (if (string-match "\\`calcFunc-\\(.+\\)\\'"
				     (symbol-name func))
		       (math-match-substring (symbol-name func) 1)
		     (symbol-name func))))
	(list 'var
	      (intern base)
	      (intern (concat "var-" base))))
    f))

;;; Expand a function call using "lambda" notation.
(defun math-build-call (f args)
  (if (eq (car-safe f) 'calcFunc-lambda)
      (if (= (length args) (- (length f) 2))
	  (math-multi-subst (nth (1- (length f)) f) (cdr f) args)
	(calc-record-why "*Wrong number of arguments" f)
	(cons 'calcFunc-call (cons (math-calcFunc-to-var f) args)))
    (if (and (eq f 'calcFunc-neg)
	     (= (length args) 1))
	(list 'neg (car args))
      (let ((func (assq f '( ( calcFunc-add . + )
			     ( calcFunc-sub . - )
			     ( calcFunc-mul . * )
			     ( calcFunc-div . / )
			     ( calcFunc-pow . ^ )
			     ( calcFunc-mod . % )
			     ( calcFunc-vconcat . | ) ))))
	(if (and func (= (length args) 2))
	    (cons (cdr func) args)
	  (cons f args))))))

;;; Do substitutions in parallel to avoid crosstalk.

;; The variables math-ms-temp and math-ms-args are local to 
;; math-multi-subst, but are used by math-multi-subst-rec, which 
;; is called by math-multi-subst.
(defvar math-ms-temp)
(defvar math-ms-args)

(defun math-multi-subst (expr olds news)
  (let ((math-ms-args nil)
	math-ms-temp)
    (while (and olds news)
      (setq math-ms-args (cons (cons (car olds) (car news)) math-ms-args)
	    olds (cdr olds)
	    news (cdr news)))
    (math-multi-subst-rec expr)))

(defun math-multi-subst-rec (expr)
  (cond ((setq math-ms-temp (assoc expr math-ms-args)) 
         (cdr math-ms-temp))
	((Math-primp expr) expr)
	((and (eq (car expr) 'calcFunc-lambda) (> (length expr) 2))
	 (let ((new (list (car expr)))
	       (math-ms-args math-ms-args))
	   (while (cdr (setq expr (cdr expr)))
	     (setq new (cons (car expr) new))
	     (if (assoc (car expr) math-ms-args)
		 (setq math-ms-args (cons (cons (car expr) (car expr)) 
                                          math-ms-args))))
	   (nreverse (cons (math-multi-subst-rec (car expr)) new))))
	(t
	 (cons (car expr)
	       (mapcar 'math-multi-subst-rec (cdr expr))))))

(defun calcFunc-call (f &rest args)
  (setq args (math-build-call (math-var-to-calcFunc f) args))
  (if (eq (car-safe args) 'calcFunc-call)
      args
    (math-normalize args)))

(defun calcFunc-apply (f args)
  (or (Math-vectorp args)
      (math-reject-arg args 'vectorp))
  (apply 'calcFunc-call (cons f (cdr args))))




;;; Map a function over a vector symbolically. [Public]
(defun math-symb-map (f mode args)
  (let* ((func (math-var-to-calcFunc f))
	 (nargs (length args))
	 (ptrs (vconcat args))
	 (vflags (make-vector nargs nil))
	 (heads '(vec))
	 (head nil)
	 (vec nil)
	 (i -1)
	 (math-working-step 0)
	 (math-working-step-2 nil)
	 len cols obj expr)
    (if (eq mode 'eqn)
	(setq mode 'elems
	      heads '(calcFunc-eq calcFunc-neq calcFunc-lt calcFunc-gt
				  calcFunc-leq calcFunc-geq))
      (while (and (< (setq i (1+ i)) nargs)
		  (not (math-matrixp (aref ptrs i)))))
      (if (< i nargs)
	  (if (eq mode 'elems)
	      (setq func (list 'lambda '(&rest x)
			       (list 'math-symb-map
				     (list 'quote f) '(quote elems) 'x))
		    mode 'rows)
	    (if (eq mode 'cols)
		(while (< i nargs)
		  (if (math-matrixp (aref ptrs i))
		      (aset ptrs i (math-transpose (aref ptrs i))))
		  (setq i (1+ i)))))
	(setq mode 'elems))
      (setq i -1))
    (while (< (setq i (1+ i)) nargs)
      (setq obj (aref ptrs i))
      (if (and (memq (car-safe obj) heads)
	       (or (eq mode 'elems)
		   (math-matrixp obj)))
	  (progn
	    (aset vflags i t)
	    (if head
		(if (cdr heads)
		    (setq head (nth
				(aref (aref [ [0 1 2 3 4 5]
					      [1 1 2 3 2 3]
					      [2 2 2 1 2 1]
					      [3 3 1 3 1 3]
					      [4 2 2 1 4 1]
					      [5 3 1 3 1 5] ]
					    (- 6 (length (memq head heads))))
				      (- 6 (length (memq (car obj) heads))))
				heads)))
	      (setq head (car obj)))
	    (if len
		(or (= (length obj) len)
		    (math-dimension-error))
	      (setq len (length obj))))))
    (or len
	(if (= nargs 1)
	    (math-reject-arg (aref ptrs 0) 'vectorp)
	  (math-reject-arg nil "At least one argument must be a vector")))
    (setq math-working-step-2 (1- len))
    (while (> (setq len (1- len)) 0)
      (setq expr nil
	    i -1)
      (while (< (setq i (1+ i)) nargs)
	(if (aref vflags i)
	    (progn
	      (aset ptrs i (cdr (aref ptrs i)))
	      (setq expr (nconc expr (list (car (aref ptrs i))))))
	  (setq expr (nconc expr (list (aref ptrs i))))))
      (setq math-working-step (1+ math-working-step)
	    vec (cons (math-normalize (math-build-call func expr)) vec)))
    (setq vec (cons head (nreverse vec)))
    (if (and (eq mode 'cols) (math-matrixp vec))
	(math-transpose vec)
      vec)))

(defun calcFunc-map (func &rest args)
  (math-symb-map func 'elems args))

(defun calcFunc-mapr (func &rest args)
  (math-symb-map func 'rows args))

(defun calcFunc-mapc (func &rest args)
  (math-symb-map func 'cols args))

(defun calcFunc-mapa (func arg)
  (if (math-matrixp arg)
      (math-symb-map func 'elems (cdr (math-transpose arg)))
    (math-symb-map func 'elems arg)))

(defun calcFunc-mapd (func arg)
  (if (math-matrixp arg)
      (math-symb-map func 'elems (cdr arg))
    (math-symb-map func 'elems arg)))

(defun calcFunc-mapeq (func &rest args)
  (if (and (or (equal func '(var mul var-mul))
	       (equal func '(var div var-div)))
	   (= (length args) 2))
      (if (math-negp (car args))
	  (let ((func (nth 1 (assq (car-safe (nth 1 args))
				   calc-tweak-eqn-table))))
	    (and func (setq args (list (car args)
				       (cons func (cdr (nth 1 args)))))))
	(if (math-negp (nth 1 args))
	    (let ((func (nth 1 (assq (car-safe (car args))
				     calc-tweak-eqn-table))))
	      (and func (setq args (list (cons func (cdr (car args)))
					 (nth 1 args))))))))
  (if (or (and (equal func '(var div var-div))
	       (assq (car-safe (nth 1 args)) calc-tweak-eqn-table))
	  (equal func '(var neg var-neg))
	  (equal func '(var inv var-inv)))
      (apply 'calcFunc-mapeqr func args)
    (apply 'calcFunc-mapeqp func args)))

(defun calcFunc-mapeqr (func &rest args)
  (setq args (mapcar (function (lambda (x)
				 (let ((func (assq (car-safe x)
						   calc-tweak-eqn-table)))
				   (if func
				       (cons (nth 1 func) (cdr x))
				     x))))
		     args))
  (apply 'calcFunc-mapeqp func args))

(defun calcFunc-mapeqp (func &rest args)
  (if (or (and (memq (car-safe (car args)) '(calcFunc-lt calcFunc-leq))
	       (memq (car-safe (nth 1 args)) '(calcFunc-gt calcFunc-geq)))
	  (and (memq (car-safe (car args)) '(calcFunc-gt calcFunc-geq))
	       (memq (car-safe (nth 1 args)) '(calcFunc-lt calcFunc-leq))))
      (setq args (cons (car args)
		       (cons (list (nth 1 (assq (car (nth 1 args))
						calc-tweak-eqn-table))
				   (nth 2 (nth 1 args))
				   (nth 1 (nth 1 args)))
			     (cdr (cdr args))))))
  (math-symb-map func 'eqn args))



;;; Reduce a function over a vector symbolically. [Public]
(defun calcFunc-reduce (func vec)
  (if (math-matrixp vec)
      (let (expr row)
	(setq func (math-var-to-calcFunc func))
	(while (setq vec (cdr vec))
	  (setq row (car vec))
	  (while (setq row (cdr row))
	    (setq expr (if expr
			   (if (Math-numberp expr)
			       (math-normalize
				(math-build-call func (list expr (car row))))
			     (math-build-call func (list expr (car row))))
			 (car row)))))
	(math-normalize expr))
    (calcFunc-reducer func vec)))

(defun calcFunc-rreduce (func vec)
  (if (math-matrixp vec)
      (let (expr row)
	(setq func (math-var-to-calcFunc func)
	      vec (reverse (cdr vec)))
	(while vec
	  (setq row (reverse (cdr (car vec))))
	  (while row
	    (setq expr (if expr
			   (math-build-call func (list (car row) expr))
			 (car row))
		  row (cdr row)))
	  (setq vec (cdr vec)))
	(math-normalize expr))
    (calcFunc-rreducer func vec)))

(defun calcFunc-reducer (func vec)
  (setq func (math-var-to-calcFunc func))
  (or (math-vectorp vec)
      (math-reject-arg vec 'vectorp))
  (let ((expr (car (setq vec (cdr vec)))))
    (if expr
	(progn
	  (condition-case err
	      (and (symbolp func)
		   (let ((lfunc (or (cdr (assq func
					       '( (calcFunc-add . math-add)
						  (calcFunc-sub . math-sub)
						  (calcFunc-mul . math-mul)
						  (calcFunc-div . math-div)
						  (calcFunc-pow . math-pow)
						  (calcFunc-mod . math-mod)
						  (calcFunc-vconcat .
						   math-concat) )))
				    func)))
		     (while (cdr vec)
		       (setq expr (funcall lfunc expr (nth 1 vec))
			     vec (cdr vec)))))
	    (error nil))
	  (while (setq vec (cdr vec))
	    (setq expr (math-build-call func (list expr (car vec)))))
	  (math-normalize expr))
      (or (math-identity-value func)
	  (math-reject-arg vec "*Vector is empty")))))

(defun math-identity-value (func)
  (cdr (assq func '( (calcFunc-add . 0) (calcFunc-sub . 0)
		     (calcFunc-mul . 1) (calcFunc-div . 1)
		     (calcFunc-idiv . 1) (calcFunc-fdiv . 1)
		     (calcFunc-min . (var inf var-inf))
		     (calcFunc-max . (neg (var inf var-inf)))
		     (calcFunc-vconcat . (vec))
		     (calcFunc-append . (vec)) ))))

(defun calcFunc-rreducer (func vec)
  (setq func (math-var-to-calcFunc func))
  (or (math-vectorp vec)
      (math-reject-arg vec 'vectorp))
  (if (eq func 'calcFunc-sub)   ; do this in a way that looks nicer
      (let ((expr (car (setq vec (cdr vec)))))
	(if expr
	    (progn
	      (while (setq vec (cdr vec))
		(setq expr (math-build-call func (list expr (car vec)))
		      func (if (eq func 'calcFunc-sub)
			       'calcFunc-add 'calcFunc-sub)))
	      (math-normalize expr))
	  0))
    (let ((expr (car (setq vec (reverse (cdr vec))))))
      (if expr
	  (progn
	    (while (setq vec (cdr vec))
	      (setq expr (math-build-call func (list (car vec) expr))))
	    (math-normalize expr))
	(or (math-identity-value func)
	    (math-reject-arg vec "*Vector is empty"))))))

(defun calcFunc-reducec (func vec)
  (if (math-matrixp vec)
      (calcFunc-reducer func (math-transpose vec))
    (calcFunc-reducer func vec)))

(defun calcFunc-rreducec (func vec)
  (if (math-matrixp vec)
      (calcFunc-rreducer func (math-transpose vec))
    (calcFunc-rreducer func vec)))

(defun calcFunc-reducea (func vec)
  (if (math-matrixp vec)
      (cons 'vec
	    (mapcar (function (lambda (x) (calcFunc-reducer func x)))
		    (cdr vec)))
    (calcFunc-reducer func vec)))

(defun calcFunc-rreducea (func vec)
  (if (math-matrixp vec)
      (cons 'vec
	    (mapcar (function (lambda (x) (calcFunc-rreducer func x)))
		    (cdr vec)))
    (calcFunc-rreducer func vec)))

(defun calcFunc-reduced (func vec)
  (if (math-matrixp vec)
      (cons 'vec
	    (mapcar (function (lambda (x) (calcFunc-reducer func x)))
		    (cdr (math-transpose vec))))
    (calcFunc-reducer func vec)))

(defun calcFunc-rreduced (func vec)
  (if (math-matrixp vec)
      (cons 'vec
	    (mapcar (function (lambda (x) (calcFunc-rreducer func x)))
		    (cdr (math-transpose vec))))
    (calcFunc-rreducer func vec)))

(defun calcFunc-accum (func vec)
  (setq func (math-var-to-calcFunc func))
  (or (math-vectorp vec)
      (math-reject-arg vec 'vectorp))
  (let* ((expr (car (setq vec (cdr vec))))
	 (res (list 'vec expr)))
    (or expr
	(math-reject-arg vec "*Vector is empty"))
    (while (setq vec (cdr vec))
      (setq expr (math-build-call func (list expr (car vec)))
	    res (nconc res (list expr))))
    (math-normalize res)))

(defun calcFunc-raccum (func vec)
  (setq func (math-var-to-calcFunc func))
  (or (math-vectorp vec)
      (math-reject-arg vec 'vectorp))
  (let* ((expr (car (setq vec (reverse (cdr vec)))))
	 (res (list expr)))
    (or expr
	(math-reject-arg vec "*Vector is empty"))
    (while (setq vec (cdr vec))
      (setq expr (math-build-call func (list (car vec) expr))
	    res (cons (list expr) res)))
    (math-normalize (cons 'vec res))))


(defun math-nest-calls (func base iters accum tol)
  (or (symbolp tol)
      (if (math-realp tol)
	  (or (math-numberp base) (math-reject-arg base 'numberp))
	(math-reject-arg tol 'realp)))
  (setq func (math-var-to-calcFunc func))
  (or (null iters)
      (if (equal iters '(var inf var-inf))
	  (setq iters nil)
	(progn
	  (if (math-messy-integerp iters)
	      (setq iters (math-trunc iters)))
	  (or (integerp iters) (math-reject-arg iters 'fixnump))
	  (or (not tol) (natnump iters) (math-reject-arg iters 'fixnatnump))
	  (if (< iters 0)
	      (let* ((dummy '(var DummyArg var-DummyArg))
		     (dummy2 '(var DummyArg2 var-DummyArg2))
		     (finv (math-solve-for (math-build-call func (list dummy2))
					   dummy dummy2 nil)))
		(or finv (math-reject-arg nil "*Unable to find an inverse"))
		(if (and (= (length finv) 2)
			 (equal (nth 1 finv) dummy))
		    (setq func (car finv))
		  (setq func (list 'calcFunc-lambda dummy finv)))
		(setq iters (- iters)))))))
  (math-with-extra-prec 1
    (let ((value base)
	  (ovalue nil)
	  (avalues (list base))
	  (math-working-step 0)
	  (math-working-step-2 iters))
      (while (and (or (null iters)
		      (>= (setq iters (1- iters)) 0))
		  (or (null tol)
		      (null ovalue)
		      (if (eq tol t)
			  (not (if (and (Math-numberp value)
					(Math-numberp ovalue))
				   (math-nearly-equal value ovalue)
				 (Math-equal value ovalue)))
			(if (math-numberp value)
			    (Math-lessp tol (math-abs (math-sub value ovalue)))
			  (math-reject-arg value 'numberp)))))
	(setq ovalue value
	      math-working-step (1+ math-working-step)
	      value (math-normalize (math-build-call func (list value))))
	(if accum
	    (setq avalues (cons value avalues))))
      (if accum
	  (cons 'vec (nreverse avalues))
	value))))

(defun calcFunc-nest (func base iters)
  (math-nest-calls func base iters nil nil))

(defun calcFunc-anest (func base iters)
  (math-nest-calls func base iters t nil))

(defun calcFunc-fixp (func base &optional iters tol)
  (math-nest-calls func base iters nil (or tol t)))

(defun calcFunc-afixp (func base &optional iters tol)
  (math-nest-calls func base iters t (or tol t)))


(defun calcFunc-outer (func a b)
  (or (math-vectorp a) (math-reject-arg a 'vectorp))
  (or (math-vectorp b) (math-reject-arg b 'vectorp))
  (setq func (math-var-to-calcFunc func))
  (let ((mat nil))
    (while (setq a (cdr a))
      (setq mat (cons (cons 'vec
			    (mapcar (function (lambda (x)
						(math-build-call func
								 (list (car a)
								       x))))
				    (cdr b)))
		      mat)))
    (math-normalize (cons 'vec (nreverse mat)))))


;; The variables math-inner-mul-func and math-inner-add-func are
;; local to calcFunc-inner, but are used by math-inner-mats,
;; which is called by math-inner-mats.
(defvar math-inner-mul-func)
(defvar math-inner-add-func)

(defun calcFunc-inner (math-inner-mul-func math-inner-add-func a b)
  (or (math-vectorp a) (math-reject-arg a 'vectorp))
  (or (math-vectorp b) (math-reject-arg b 'vectorp))
  (if (math-matrixp a)
      (if (math-matrixp b)
	  (if (= (length (nth 1 a)) (length b))
	      (math-inner-mats a b)
	    (math-dimension-error))
	(if (= (length (nth 1 a)) 2)
	    (if (= (length a) (length b))
		(math-inner-mats a (list 'vec b))
	      (math-dimension-error))
	  (if (= (length (nth 1 a)) (length b))
	      (math-mat-col (math-inner-mats a (math-col-matrix b))
			    1)
	    (math-dimension-error))))
    (if (math-matrixp b)
	(nth 1 (math-inner-mats (list 'vec a) b))
      (calcFunc-reduce math-inner-add-func (calcFunc-map math-inner-mul-func a b)))))

(defun math-inner-mats (a b)
  (let ((mat nil)
	(cols (length (nth 1 b)))
	row col ap bp accum)
    (while (setq a (cdr a))
      (setq col cols
	    row nil)
      (while (> (setq col (1- col)) 0)
	(setq row (cons (calcFunc-reduce math-inner-add-func
					 (calcFunc-map math-inner-mul-func
						       (car a)
						       (math-mat-col b col)))
			row)))
      (setq mat (cons (cons 'vec row) mat)))
    (cons 'vec (nreverse mat))))

(provide 'calc-map)

;;; calc-map.el ends here
