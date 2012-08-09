;;; calc-rewr.el --- rewriting functions for Calc

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

(defvar math-rewrite-default-iters 100)

;; The variable calc-rewr-sel is local to calc-rewrite-selection and 
;; calc-rewrite, but is used by calc-locate-selection-marker.
(defvar calc-rewr-sel)

(defun calc-rewrite-selection (rules-str &optional many prefix)
  (interactive "sRewrite rule(s): \np")
  (calc-slow-wrapper
   (calc-preserve-point)
   (let* ((num (max 1 (calc-locate-cursor-element (point))))
	  (reselect t)
	  (pop-rules nil)
          rules
	  (entry (calc-top num 'entry))
	  (expr (car entry))
	  (calc-rewr-sel (calc-auto-selection entry))
	  (math-rewrite-selections t)
	  (math-rewrite-default-iters 1))
     (if (or (null rules-str) (equal rules-str "") (equal rules-str "$"))
	 (if (= num 1)
	     (error "Can't use same stack entry for formula and rules")
	   (setq rules (calc-top-n 1 t)
		 pop-rules t))
       (setq rules (if (stringp rules-str)
		       (math-read-exprs rules-str) rules-str))
       (if (eq (car-safe rules) 'error)
	   (error "Bad format in expression: %s" (nth 1 rules)))
       (if (= (length rules) 1)
	   (setq rules (car rules))
	 (setq rules (cons 'vec rules)))
       (or (memq (car-safe rules) '(vec var calcFunc-assign
					calcFunc-condition))
	   (let ((rhs (math-read-expr
		       (read-string (concat "Rewrite from:    " rules-str
					    "  to: ")))))
	     (if (eq (car-safe rhs) 'error)
		 (error "Bad format in expression: %s" (nth 1 rhs)))
	     (setq rules (list 'calcFunc-assign rules rhs))))
       (or (eq (car-safe rules) 'var)
	   (calc-record rules "rule")))
     (if (eq many 0)
	 (setq many '(var inf var-inf))
       (if many (setq many (prefix-numeric-value many))))
     (if calc-rewr-sel
	 (setq expr (calc-replace-sub-formula (car entry)
					      calc-rewr-sel
					      (list 'calcFunc-select calc-rewr-sel)))
       (setq expr (car entry)
	     reselect nil
	     math-rewrite-selections nil))
     (setq expr (calc-encase-atoms
		 (calc-normalize
		  (math-rewrite
		   (calc-normalize expr)
		   rules many)))
	   calc-rewr-sel nil
	   expr (calc-locate-select-marker expr))
     (or (consp calc-rewr-sel) (setq calc-rewr-sel nil))
     (if pop-rules (calc-pop-stack 1))
     (calc-pop-push-record-list 1 (or prefix "rwrt") (list expr)
				(- num (if pop-rules 1 0))
				(list (and reselect calc-rewr-sel))))
   (calc-handle-whys)))

(defun calc-locate-select-marker (expr)
  (if (Math-primp expr)
      expr
    (if (and (eq (car expr) 'calcFunc-select)
	     (= (length expr) 2))
	(progn
	  (setq calc-rewr-sel (if calc-rewr-sel t (nth 1 expr)))
	  (nth 1 expr))
      (cons (car expr)
	    (mapcar 'calc-locate-select-marker (cdr expr))))))



(defun calc-rewrite (rules-str many)
  (interactive "sRewrite rule(s): \nP")
  (calc-slow-wrapper
   (let (n rules expr)
     (if (or (null rules-str) (equal rules-str "") (equal rules-str "$"))
	 (setq expr (calc-top-n 2)
	       rules (calc-top-n 1 t)
	       n 2)
       (setq rules (if (stringp rules-str)
		       (math-read-exprs rules-str) rules-str))
       (if (eq (car-safe rules) 'error)
	   (error "Bad format in expression: %s" (nth 1 rules)))
       (if (= (length rules) 1)
	   (setq rules (car rules))
	 (setq rules (cons 'vec rules)))
       (or (memq (car-safe rules) '(vec var calcFunc-assign
					calcFunc-condition))
	   (let ((rhs (math-read-expr
		       (read-string (concat "Rewrite from:    " rules-str
					    " to: ")))))
	     (if (eq (car-safe rhs) 'error)
		 (error "Bad format in expression: %s" (nth 1 rhs)))
	     (setq rules (list 'calcFunc-assign rules rhs))))
       (or (eq (car-safe rules) 'var)
	   (calc-record rules "rule"))
       (setq expr (calc-top-n 1)
	     n 1))
     (if (eq many 0)
	 (setq many '(var inf var-inf))
       (if many (setq many (prefix-numeric-value many))))
     (setq expr (calc-normalize (math-rewrite expr rules many)))
     (let (calc-rewr-sel)
       (setq expr (calc-locate-select-marker expr)))
     (calc-pop-push-record-list n "rwrt" (list expr)))
   (calc-handle-whys)))

(defun calc-match (pat &optional interactive)
  (interactive "sPattern: \np")
  (calc-slow-wrapper
   (let (n expr)
     (if (or (null pat) (equal pat "") (equal pat "$"))
	 (setq expr (calc-top-n 2)
	       pat (calc-top-n 1)
	       n 2)
       (setq pat (if (stringp pat) (math-read-expr pat) pat))
       (if (eq (car-safe pat) 'error)
	   (error "Bad format in expression: %s" (nth 1 pat)))
       (if (not (eq (car-safe pat) 'var))
	   (calc-record pat "pat"))
       (setq expr (calc-top-n 1)
	     n 1))
     (or (math-vectorp expr) (error "Argument must be a vector"))
     (if (calc-is-inverse)
	 (calc-enter-result n "mtcn" (math-match-patterns pat expr t))
       (calc-enter-result n "mtch" (math-match-patterns pat expr nil))))))


(defvar math-mt-many)

;; The variable math-rewrite-whole-expr is local to math-rewrite,
;; but is used by math-rewrite-phase
(defvar math-rewrite-whole-expr)

(defun math-rewrite (math-rewrite-whole-expr rules &optional math-mt-many)
  (let* ((crules (math-compile-rewrites rules))
         (heads (math-rewrite-heads math-rewrite-whole-expr))
         (trace-buffer (get-buffer "*Trace*"))
         (calc-display-just 'center)
         (calc-display-origin 39)
         (calc-line-breaking 78)
         (calc-line-numbering nil)
         (calc-show-selections t)
         (calc-why nil)
         (math-mt-func (function
                        (lambda (x)
                          (let ((result (math-apply-rewrites x (cdr crules)
                                                             heads crules)))
                            (if result
                                (progn
                                  (if trace-buffer
                                      (let ((fmt (math-format-stack-value
                                                  (list result nil nil))))
                                        (with-current-buffer trace-buffer
                                          (insert "\nrewrite to\n" fmt "\n"))))
                                  (setq heads (math-rewrite-heads result heads t))))
                            result)))))
    (if trace-buffer
	(let ((fmt (math-format-stack-value (list math-rewrite-whole-expr nil nil))))
	  (with-current-buffer trace-buffer
	    (setq truncate-lines t)
	    (goto-char (point-max))
	    (insert "\n\nBegin rewriting\n" fmt "\n"))))
    (or math-mt-many (setq math-mt-many (or (nth 1 (car crules))
				    math-rewrite-default-iters)))
    (if (equal math-mt-many '(var inf var-inf)) (setq math-mt-many 1000000))
    (if (equal math-mt-many '(neg (var inf var-inf))) (setq math-mt-many -1000000))
    (math-rewrite-phase (nth 3 (car crules)))
    (if trace-buffer
	(let ((fmt (math-format-stack-value (list math-rewrite-whole-expr nil nil))))
	  (with-current-buffer trace-buffer
	    (insert "\nDone rewriting"
		    (if (= math-mt-many 0) " (reached iteration limit)" "")
		    ":\n" fmt "\n"))))
    math-rewrite-whole-expr))

(defun math-rewrite-phase (sched)
  (while (and sched (/= math-mt-many 0))
    (if (listp (car sched))
	(while (let ((save-expr math-rewrite-whole-expr))
		 (math-rewrite-phase (car sched))
		 (not (equal math-rewrite-whole-expr save-expr))))
      (if (symbolp (car sched))
	  (progn
	    (setq math-rewrite-whole-expr 
                  (math-normalize (list (car sched) math-rewrite-whole-expr)))
	    (if trace-buffer
		(let ((fmt (math-format-stack-value
			    (list math-rewrite-whole-expr nil nil))))
		  (with-current-buffer trace-buffer
		    (insert "\ncall "
			    (substring (symbol-name (car sched)) 9)
			    ":\n" fmt "\n")))))
	(let ((math-rewrite-phase (car sched)))
	  (if trace-buffer
	      (with-current-buffer trace-buffer
		(insert (format "\n(Phase %d)\n" math-rewrite-phase))))
	  (while (let ((save-expr math-rewrite-whole-expr))
		   (setq math-rewrite-whole-expr (math-normalize
				     (math-map-tree-rec math-rewrite-whole-expr)))
		   (not (equal math-rewrite-whole-expr save-expr)))))))
    (setq sched (cdr sched))))

(defun calcFunc-rewrite (expr rules &optional many)
  (or (null many) (integerp many)
      (equal many '(var inf var-inf)) (equal many '(neg (var inf var-inf)))
      (math-reject-arg many 'fixnump))
  (condition-case err
      (math-rewrite expr rules (or many 1))
    (error (math-reject-arg rules (nth 1 err)))))

(defun calcFunc-match (pat vec)
  (or (math-vectorp vec) (math-reject-arg vec 'vectorp))
  (condition-case err
      (math-match-patterns pat vec nil)
    (error (math-reject-arg pat (nth 1 err)))))

(defun calcFunc-matchnot (pat vec)
  (or (math-vectorp vec) (math-reject-arg vec 'vectorp))
  (condition-case err
      (math-match-patterns pat vec t)
    (error (math-reject-arg pat (nth 1 err)))))

(defun math-match-patterns (pat vec &optional not-flag)
  (let ((newvec nil)
	(crules (math-compile-patterns pat)))
    (while (setq vec (cdr vec))
      (if (eq (not (math-apply-rewrites (car vec) crules))
	      not-flag)
	  (setq newvec (cons (car vec) newvec))))
    (cons 'vec (nreverse newvec))))

(defun calcFunc-matches (expr pat)
  (condition-case err
      (if (math-apply-rewrites expr (math-compile-patterns pat))
	  1
	0)
    (error (math-reject-arg pat (nth 1 err)))))

(defun calcFunc-vmatches (expr pat)
  (condition-case err
      (or (math-apply-rewrites expr (math-compile-patterns pat))
	  0)
    (error (math-reject-arg pat (nth 1 err)))))



;; A compiled rule set is an a-list of entries whose cars are functors,
;; and whose cdrs are lists of rules.  If there are rules with no
;; well-defined head functor, they are included on all lists and also
;; on an extra list whose car is nil.
;;
;; The first entry in the a-list is of the form (schedule A B C ...).
;;
;; Rule list entries take the form (regs prog head phases), where:
;;
;;   regs   is a vector of match registers.
;;
;;   prog   is a match program (see below).
;;
;;   head   is a rare function name appearing in the rule body (but not the
;;	     head of the whole rule), or nil if none.
;;
;;   phases is a list of phase numbers for which the rule is enabled.
;;
;; A match program is a list of match instructions.
;;
;; In the following, "part" is a register number that contains the
;; subexpression to be operated on.
;;
;; Register 0 is the whole expression being matched.  The others are
;; meta-variables in the pattern, temporaries used for matching and
;; backtracking, and constant expressions.
;;
;; (same part reg)
;;         The selected part must be math-equal to the contents of "reg".
;;
;; (same-neg part reg)
;;         The selected part must be math-equal to the negative of "reg".
;;
;; (copy part reg)
;;	    The selected part is copied into "reg".  (Rarely used.)
;;
;; (copy-neg part reg)
;;	    The negative of the selected part is copied into "reg".
;;
;; (integer part)
;;         The selected part must be an integer.
;;
;; (real part)
;;         The selected part must be a real.
;;
;; (constant part)
;;         The selected part must be a constant.
;;
;; (negative part)
;;	    The selected part must "look" negative.
;;
;; (rel part op reg)
;;         The selected part must satisfy "part op reg", where "op"
;;	    is one of the 6 relational ops, and "reg" is a register.
;;
;; (mod part modulo value)
;;         The selected part must satisfy "part % modulo = value", where
;;         "modulo" and "value" are constants.
;;
;; (func part head reg1 reg2 ... regn)
;;         The selected part must be an n-ary call to function "head".
;;         The arguments are stored in "reg1" through "regn".
;;
;; (func-def part head defs reg1 reg2 ... regn)
;;	    The selected part must be an n-ary call to function "head".
;;	    "Defs" is a list of value/register number pairs for default args.
;;	    If a match, assign default values to registers and then skip
;;	    immediately over any following "func-def" instructions and
;;	    the following "func" instruction.  If wrong number of arguments,
;;	    proceed to the following "func-def" or "func" instruction.
;;
;; (func-opt part head defs reg1)
;;	    Like func-def with "n=1", except that if the selected part is
;;	    not a call to "head", then the part itself successfully matches
;;	    "reg1" (and the defaults are assigned).
;;
;; (try part heads mark reg1 [def])
;;         The selected part must be a function of the correct type which is
;;         associative and/or commutative.  "Heads" is a list of acceptable
;;         types.  An initial assignment of arguments to "reg1" is tried.
;;	    If the program later fails, it backtracks to this instruction
;;	    and tries other assignments of arguments to "reg1".
;;	    If "def" exists and normal matching fails, backtrack and assign
;;	    "part" to "reg1", and "def" to "reg2" in the following "try2".
;;	    The "mark" is a vector of size 5; only "mark[3-4]" are initialized.
;;	    "mark[0]" points to the argument list; "mark[1]" points to the
;;	    current argument; "mark[2]" is 0 if there are two arguments,
;;	    1 if reg1 is matching single arguments, 2 if reg2 is matching
;;	    single arguments (a+b+c+d is never split as (a+b)+(c+d)), or
;;         3 if reg2 is matching "def"; "mark[3]" is 0 if the function must
;;	    have two arguments, 1 if phase-2 can be skipped, 2 if full
;;	    backtracking is necessary; "mark[4]" is t if the arguments have
;;	    been switched from the order given in the original pattern.
;;
;; (try2 try reg2)
;;         Every "try" will be followed by a "try2" whose "try" field is
;;	    a pointer to the corresponding "try".  The arguments which were
;;	    not stored in "reg1" by that "try" are now stored in "reg2".
;;
;; (alt instr nil mark)
;;	    Basic backtracking.  Execute the instruction sequence "instr".
;;	    If this fails, back up and execute following the "alt" instruction.
;;	    The "mark" must be the vector "[nil nil 4]".  The "instr" sequence
;;	    should execute "end-alt" at the end.
;;
;; (end-alt ptr)
;; 	    Register success of the first alternative of a previous "alt".
;;	    "Ptr" is a pointer to the next instruction following that "alt".
;;
;; (apply part reg1 reg2)
;;         The selected part must be a function call.  The functor
;;	    (as a variable name) is stored in "reg1"; the arguments
;;	    (as a vector) are stored in "reg2".
;;
;; (cons part reg1 reg2)
;;	    The selected part must be a nonempty vector.  The first element
;;	    of the vector is stored in "reg1"; the rest of the vector
;;	    (as another vector) is stored in "reg2".
;;
;; (rcons part reg1 reg2)
;;	    The selected part must be a nonempty vector.  The last element
;;	    of the vector is stored in "reg2"; the rest of the vector
;;	    (as another vector) is stored in "reg1".
;;
;; (select part reg)
;;         If the selected part is a unary call to function "select", its
;;         argument is stored in "reg"; otherwise (provided this is an `a r'
;;         and not a `g r' command) the selected part is stored in "reg".
;;
;; (cond expr)
;;         The "expr", with registers substituted, must simplify to
;;         a non-zero value.
;;
;; (let reg expr)
;;         Evaluate "expr" and store the result in "reg".  Always succeeds.
;;
;; (done rhs remember)
;;         Rewrite the expression to "rhs", with register substituted.
;;	    Normalize; if the result is different from the original
;;	    expression, the match has succeeded.  This is the last
;;	    instruction of every program.  If "remember" is non-nil,
;;         record the result of the match as a new literal rule.


;; Pseudo-functions related to rewrites:
;;
;;  In patterns:  quote, plain, condition, opt, apply, cons, select
;;
;;  In righthand sides:  quote, plain, eval, evalsimp, evalextsimp,
;;                       apply, cons, select
;;
;;  In conditions:  let + same as for righthand sides

;; Some optimizations that would be nice to have:
;;
;;  * Merge registers with disjoint lifetimes.
;;  * Merge constant registers with equivalent values.
;;
;;  * If an argument of a commutative op math-depends neither on the
;;    rest of the pattern nor on any of the conditions, then no backtracking
;;    should be done for that argument.  (This won't apply to very many
;;    cases.)
;;
;;  * If top functor is "select", and its argument is a unique function,
;;    add the rule to the lists for both "select" and that function.
;;    (Currently rules like this go on the "nil" list.)
;;    Same for "func-opt" functions.  (Though not urgent for these.)
;;
;;  * Shouldn't evaluate a "let" condition until the end, or until it
;;    would enable another condition to be evaluated.
;;

;; Some additional features to add / things to think about:
;;;
;;;  * Figure out what happens to "a +/- b" and "a +/- opt(b)".
;;;
;;;  * Same for interval forms.
;;;
;;;  * Have a name(v,pat) pattern which matches pat, and gives the
;;;    whole match the name v.  Beware of circular structures!
;;;

(defun math-compile-patterns (pats)
  (if (and (eq (car-safe pats) 'var)
	   (calc-var-value (nth 2 pats)))
      (let ((prop (get (nth 2 pats) 'math-pattern-cache)))
	(or prop
	    (put (nth 2 pats) 'math-pattern-cache (setq prop (list nil))))
	(or (eq (car prop) (symbol-value (nth 2 pats)))
	    (progn
	      (setcdr prop (math-compile-patterns
			    (symbol-value (nth 2 pats))))
	      (setcar prop (symbol-value (nth 2 pats)))))
	(cdr prop))
    (let ((math-rewrite-whole t))
      (cdr (math-compile-rewrites (cons
				   'vec
				   (mapcar (function (lambda (x)
						       (list 'vec x t)))
					   (if (eq (car-safe pats) 'vec)
					       (cdr pats)
					     (list pats)))))))))

(defvar math-rewrite-whole nil)
(defvar math-make-import-list nil)

;; The variable math-import-list is local to part of math-compile-rewrites,
;; but is also used in a different part, and so the local version could
;; be affected by the non-local version when math-compile-rewrites calls itself. 
(defvar math-import-list nil)

;; The variables math-regs, math-num-regs, math-prog-last, math-bound-vars, 
;; math-conds, math-copy-neg, math-rhs, math-pattern, math-remembering and
;; math-aliased-vars are local to math-compile-rewrites, 
;; but are used by many functions math-rwcomp-*, which are called by 
;; math-compile-rewrites.
(defvar math-regs)
(defvar math-num-regs)
(defvar math-prog-last)
(defvar math-bound-vars)
(defvar math-conds)
(defvar math-copy-neg)
(defvar math-rhs)
(defvar math-pattern)
(defvar math-remembering)
(defvar math-aliased-vars)

(defun math-compile-rewrites (rules &optional name)
  (if (eq (car-safe rules) 'var)
      (let ((prop (get (nth 2 rules) 'math-rewrite-cache))
	    (math-import-list nil)
	    (math-make-import-list t)
	    p)
	(or (calc-var-value (nth 2 rules))
	    (error "Rules variable %s has no stored value" (nth 1 rules)))
	(or prop
	    (put (nth 2 rules) 'math-rewrite-cache
		 (setq prop (list (list (cons (nth 2 rules) nil))))))
	(setq p (car prop))
	(while (and p (eq (symbol-value (car (car p))) (cdr (car p))))
	  (setq p (cdr p)))
	(or (null p)
	    (progn
	      (message "Compiling rule set %s..." (nth 1 rules))
	      (setcdr prop (math-compile-rewrites
			    (symbol-value (nth 2 rules))
			    (nth 2 rules)))
	      (message "Compiling rule set %s...done" (nth 1 rules))
	      (setcar prop (cons (cons (nth 2 rules)
				       (symbol-value (nth 2 rules)))
				 math-import-list))))
	(cdr prop))
    (if (or (not (eq (car-safe rules) 'vec))
	    (and (memq (length rules) '(3 4))
		 (let ((p rules))
		   (while (and (setq p (cdr p))
			       (memq (car-safe (car p))
				     '(vec
				       calcFunc-assign
				       calcFunc-condition
				       calcFunc-import
				       calcFunc-phase
				       calcFunc-schedule
				       calcFunc-iterations))))
		   p)))
	(setq rules (list rules))
      (setq rules (cdr rules)))
    (if (assq 'calcFunc-import rules)
	(let ((pp (setq rules (copy-sequence rules)))
	      p part)
	  (while (setq p (car (cdr pp)))
	    (if (eq (car-safe p) 'calcFunc-import)
		(progn
		  (setcdr pp (cdr (cdr pp)))
		  (or (and (eq (car-safe (nth 1 p)) 'var)
			   (setq part (calc-var-value (nth 2 (nth 1 p))))
			   (memq (car-safe part) '(vec
						   calcFunc-assign
						   calcFunc-condition)))
		      (error "Argument of import() must be a rules variable"))
		  (if math-make-import-list
		      (setq math-import-list
			    (cons (cons (nth 2 (nth 1 p))
					(symbol-value (nth 2 (nth 1 p))))
				  math-import-list)))
		  (while (setq p (cdr (cdr p)))
		    (or (cdr p)
			(error "import() must have odd number of arguments"))
		    (setq part (math-rwcomp-substitute part
						       (car p) (nth 1 p))))
		  (if (eq (car-safe part) 'vec)
		      (setq part (cdr part))
		    (setq part (list part)))
		  (setcdr pp (append part (cdr pp))))
	      (setq pp (cdr pp))))))
    (let ((rule-set nil)
	  (all-heads nil)
	  (nil-rules nil)
	  (rule-count 0)
	  (math-schedule nil)
	  (math-iterations nil)
	  (math-phases nil)
	  (math-all-phases nil)
	  (math-remembering nil)
	  math-pattern math-rhs math-conds)
      (while rules
	(cond
	 ((and (eq (car-safe (car rules)) 'calcFunc-iterations)
	       (= (length (car rules)) 2))
	  (or (integerp (nth 1 (car rules)))
	      (equal (nth 1 (car rules)) '(var inf var-inf))
	      (equal (nth 1 (car rules)) '(neg (var inf var-inf)))
	      (error "Invalid argument for iterations(n)"))
	  (or math-iterations
	      (setq math-iterations (nth 1 (car rules)))))
	 ((eq (car-safe (car rules)) 'calcFunc-schedule)
	  (or math-schedule
	      (setq math-schedule (math-parse-schedule (cdr (car rules))))))
	 ((eq (car-safe (car rules)) 'calcFunc-phase)
	  (setq math-phases (cdr (car rules)))
	  (if (equal math-phases '((var all var-all)))
	      (setq math-phases nil))
	  (let ((p math-phases))
	    (while p
	      (or (integerp (car p))
		  (error "Phase numbers must be small integers"))
	      (or (memq (car p) math-all-phases)
		  (setq math-all-phases (cons (car p) math-all-phases)))
	      (setq p (cdr p)))))
	 ((or (and (eq (car-safe (car rules)) 'vec)
		   (cdr (cdr (car rules)))
		   (not (nthcdr 4 (car rules)))
		   (setq math-conds (nth 3 (car rules))
			 math-rhs (nth 2 (car rules))
			 math-pattern (nth 1 (car rules))))
	      (progn
		(setq math-conds nil
		      math-pattern (car rules))
		(while (and (eq (car-safe math-pattern) 'calcFunc-condition)
			    (= (length math-pattern) 3))
		  (let ((cond (nth 2 math-pattern)))
		    (setq math-conds (if math-conds
					 (list 'calcFunc-land math-conds cond)
				       cond)
			  math-pattern (nth 1 math-pattern))))
		(and (eq (car-safe math-pattern) 'calcFunc-assign)
		     (= (length math-pattern) 3)
		     (setq math-rhs (nth 2 math-pattern)
			   math-pattern (nth 1 math-pattern)))))
	  (let* ((math-prog (list nil))
		 (math-prog-last math-prog)
		 (math-num-regs 1)
		 (math-regs (list (list nil 0 nil nil)))
		 (math-bound-vars nil)
		 (math-aliased-vars nil)
		 (math-copy-neg nil))
	    (setq math-conds (and math-conds (math-flatten-lands math-conds)))
	    (math-rwcomp-pattern math-pattern 0)
	    (while math-conds
	      (let ((expr (car math-conds)))
		(setq math-conds (cdr math-conds))
		(math-rwcomp-cond-instr expr)))
	    (math-rwcomp-instr 'done
			       (if (eq math-rhs t)
				   (cons 'vec
					 (delq
					  nil
					  (nreverse
					   (mapcar
					    (function
					     (lambda (v)
					       (and (car v)
						    (list
						     'calcFunc-assign
						     (math-build-var-name
						      (car v))
						     (math-rwcomp-register-expr
						      (nth 1 v))))))
					    math-regs))))
				 (math-rwcomp-match-vars math-rhs))
			       math-remembering)
	    (setq math-prog (cdr math-prog))
	    (let* ((heads (math-rewrite-heads math-pattern))
		   (rule (list (vconcat
				(nreverse
				 (mapcar (function (lambda (x) (nth 3 x)))
					 math-regs)))
			       math-prog
			       heads
			       math-phases))
		   (head (and (not (Math-primp math-pattern))
			      (not (and (eq (car (car math-prog)) 'try)
					(nth 5 (car math-prog))))
			      (not (memq (car (car math-prog)) '(func-opt
								 apply
								 select
								 alt)))
			      (if (memq (car (car math-prog)) '(func
								func-def))
				  (nth 2 (car math-prog))
				(if (eq (car math-pattern) 'calcFunc-quote)
				    (car-safe (nth 1 math-pattern))
				  (car math-pattern))))))
	      (let (found)
		(while heads
		  (if (setq found (assq (car heads) all-heads))
		      (setcdr found (1+ (cdr found)))
		    (setq all-heads (cons (cons (car heads) 1) all-heads)))
		  (setq heads (cdr heads))))
	      (if (eq head '-) (setq head '+))
	      (if (memq head '(calcFunc-cons calcFunc-rcons)) (setq head 'vec))
	      (if head
		  (progn
		    (nconc (or (assq head rule-set)
			       (car (setq rule-set (cons (cons head
							       (copy-sequence
								nil-rules))
							 rule-set))))
			   (list rule))
		    (if (eq head '*)
			(nconc (or (assq '/ rule-set)
				   (car (setq rule-set (cons (cons
							      '/
							      (copy-sequence
							       nil-rules))
							     rule-set))))
			       (list rule))))
		(setq nil-rules (nconc nil-rules (list rule)))
		(let ((ptr rule-set))
		  (while ptr
		    (nconc (car ptr) (list rule))
		    (setq ptr (cdr ptr))))))))
	 (t
	  (error "Rewrite rule set must be a vector of A := B rules")))
	(setq rules (cdr rules)))
      (if nil-rules
	  (setq rule-set (cons (cons nil nil-rules) rule-set)))
      (setq all-heads (mapcar 'car
			      (sort all-heads (function
					       (lambda (x y)
						 (< (cdr x) (cdr y)))))))
      (let ((set rule-set)
	    rule heads ptr)
	(while set
	  (setq rule (cdr (car set)))
	  (while rule
	    (if (consp (setq heads (nth 2 (car rule))))
		(progn
		  (setq heads (delq (car (car set)) heads)
			ptr all-heads)
		  (while (and ptr (not (memq (car ptr) heads)))
		    (setq ptr (cdr ptr)))
		  (setcar (nthcdr 2 (car rule)) (car ptr))))
	    (setq rule (cdr rule)))
	  (setq set (cdr set))))
      (let ((plus (assq '+ rule-set)))
	(if plus
	    (setq rule-set (cons (cons '- (cdr plus)) rule-set))))
      (cons (list 'schedule math-iterations name
		  (or math-schedule
		      (sort math-all-phases '<)
		      (list 1)))
	    rule-set))))

(defun math-flatten-lands (expr)
  (if (eq (car-safe expr) 'calcFunc-land)
      (append (math-flatten-lands (nth 1 expr))
	      (math-flatten-lands (nth 2 expr)))
    (list expr)))

;; The variables math-rewrite-heads-heads (i.e.; heads for math-rewrite-heads)
;; math-rewrite-heads-blanks and math-rewrite-heads-skips are local to 
;; math-rewrite-heads, but used by math-rewrite-heads-rec, which is called by 
;; math-rewrite-heads.
(defvar math-rewrite-heads-heads)
(defvar math-rewrite-heads-skips)
(defvar math-rewrite-heads-blanks)

(defun math-rewrite-heads (expr &optional more all)
  (let ((math-rewrite-heads-heads more)
	(math-rewrite-heads-skips (and (not all)
		    '(calcFunc-apply calcFunc-condition calcFunc-opt
				     calcFunc-por calcFunc-pnot)))
	(math-rewrite-heads-blanks (and (not all)
		     '(calcFunc-quote calcFunc-plain calcFunc-select
				      calcFunc-cons calcFunc-rcons
				      calcFunc-pand))))
    (or (Math-primp expr)
	(math-rewrite-heads-rec expr))
    math-rewrite-heads-heads))

(defun math-rewrite-heads-rec (expr)
  (or (memq (car expr) math-rewrite-heads-skips)
      (progn
	(or (memq (car expr) math-rewrite-heads-heads)
	    (memq (car expr) math-rewrite-heads-blanks)
	    (memq 'algebraic (get (car expr) 'math-rewrite-props))
	    (setq math-rewrite-heads-heads (cons (car expr) math-rewrite-heads-heads)))
	(while (setq expr (cdr expr))
	  (or (Math-primp (car expr))
	      (math-rewrite-heads-rec (car expr)))))))

(defun math-parse-schedule (sched)
  (mapcar (function
	   (lambda (s)
	     (if (integerp s)
		 s
	       (if (math-vectorp s)
		   (math-parse-schedule (cdr s))
		 (if (eq (car-safe s) 'var)
		     (math-var-to-calcFunc s)
		   (error "Improper component in rewrite schedule"))))))
	  sched))

(defun math-rwcomp-match-vars (expr)
  (if (Math-primp expr)
      (if (eq (car-safe expr) 'var)
	  (let ((entry (assq (nth 2 expr) math-regs)))
	    (if entry
		(math-rwcomp-register-expr (nth 1 entry))
	      expr))
	expr)
    (if (and (eq (car expr) 'calcFunc-quote)
	     (= (length expr) 2))
	(math-rwcomp-match-vars (nth 1 expr))
      (if (and (eq (car expr) 'calcFunc-plain)
	       (= (length expr) 2)
	       (not (Math-primp (nth 1 expr))))
	  (list (car expr)
		(cons (car (nth 1 expr))
		      (mapcar 'math-rwcomp-match-vars (cdr (nth 1 expr)))))
	(cons (car expr)
	      (mapcar 'math-rwcomp-match-vars (cdr expr)))))))

(defun math-rwcomp-register-expr (num)
  (let ((entry (nth (1- (- math-num-regs num)) math-regs)))
    (if (nth 2 entry)
	(list 'neg (list 'calcFunc-register (nth 1 entry)))
      (list 'calcFunc-register (nth 1 entry)))))

;; The variables math-rwcomp-subst-old, math-rwcomp-subst-new,
;; math-rwcomp-subst-old-func and math-rwcomp-subst-new-func
;; are local to math-rwcomp-substitute, but are used by
;; math-rwcomp-subst-rec, which is called by math-rwcomp-substitute.
(defvar math-rwcomp-subst-new)
(defvar math-rwcomp-subst-old)
(defvar math-rwcomp-subst-new-func)
(defvar math-rwcomp-subst-old-func)

(defun math-rwcomp-substitute (expr math-rwcomp-subst-old math-rwcomp-subst-new)
  (if (and (eq (car-safe math-rwcomp-subst-old) 'var)
	   (memq (car-safe math-rwcomp-subst-new) '(var calcFunc-lambda)))
      (let ((math-rwcomp-subst-old-func (math-var-to-calcFunc math-rwcomp-subst-old))
	    (math-rwcomp-subst-new-func (math-var-to-calcFunc math-rwcomp-subst-new)))
	(math-rwcomp-subst-rec expr))
    (let ((math-rwcomp-subst-old-func nil))
      (math-rwcomp-subst-rec expr))))

(defun math-rwcomp-subst-rec (expr)
  (cond ((equal expr math-rwcomp-subst-old) math-rwcomp-subst-new)
	((Math-primp expr) expr)
	(t (if (eq (car expr) math-rwcomp-subst-old-func)
	       (math-build-call math-rwcomp-subst-new-func 
                                (mapcar 'math-rwcomp-subst-rec
                                        (cdr expr)))
	     (cons (car expr)
		   (mapcar 'math-rwcomp-subst-rec (cdr expr)))))))

(defvar math-rwcomp-tracing nil)

(defun math-rwcomp-trace (instr)
  (when math-rwcomp-tracing
    (terpri) (princ instr))
  instr)

(defun math-rwcomp-instr (&rest instr)
  (setcdr math-prog-last
	  (setq math-prog-last (list (math-rwcomp-trace instr)))))

(defun math-rwcomp-multi-instr (tail &rest instr)
  (setcdr math-prog-last
	  (setq math-prog-last (list (math-rwcomp-trace (append instr tail))))))

(defun math-rwcomp-bind-var (reg var)
  (setcar (math-rwcomp-reg-entry reg) (nth 2 var))
  (setq math-bound-vars (cons (nth 2 var) math-bound-vars))
  (math-rwcomp-do-conditions))

(defun math-rwcomp-unbind-vars (mark)
  (while (not (eq math-bound-vars mark))
    (setcar (assq (car math-bound-vars) math-regs) nil)
    (setq math-bound-vars (cdr math-bound-vars))))

(defun math-rwcomp-do-conditions ()
  (let ((cond math-conds))
    (while cond
      (if (math-rwcomp-all-regs-done (car cond))
	  (let ((expr (car cond)))
	    (setq math-conds (delq (car cond) math-conds))
	    (setcar cond 1)
	    (math-rwcomp-cond-instr expr)))
      (setq cond (cdr cond)))))

(defun math-rwcomp-cond-instr (expr)
  (let (op arg)
    (cond ((and (eq (car-safe expr) 'calcFunc-matches)
		(= (length expr) 3)
		(eq (car-safe (setq arg (math-rwcomp-match-vars (nth 1 expr))))
		    'calcFunc-register))
	   (math-rwcomp-pattern (nth 2 expr) (nth 1 arg)))
	  ((math-numberp (setq expr (math-rwcomp-match-vars expr)))
	   (if (Math-zerop expr)
	       (math-rwcomp-instr 'backtrack)))
	  ((and (eq (car expr) 'calcFunc-let)
		(= (length expr) 3))
	   (let ((reg (math-rwcomp-reg)))
	     (math-rwcomp-instr 'let reg (nth 2 expr))
	     (math-rwcomp-pattern (nth 1 expr) reg)))
	  ((and (eq (car expr) 'calcFunc-let)
		(= (length expr) 2)
		(eq (car-safe (nth 1 expr)) 'calcFunc-assign)
		(= (length (nth 1 expr)) 3))
	   (let ((reg (math-rwcomp-reg)))
	     (math-rwcomp-instr 'let reg (nth 2 (nth 1 expr)))
	     (math-rwcomp-pattern (nth 1 (nth 1 expr)) reg)))
	  ((and (setq op (cdr (assq (car-safe expr)
				    '( (calcFunc-integer  . integer)
				       (calcFunc-real     . real)
				       (calcFunc-constant . constant)
				       (calcFunc-negative . negative) ))))
		(= (length expr) 2)
		(or (and (eq (car-safe (nth 1 expr)) 'neg)
			 (memq op '(integer real constant))
			 (setq arg (nth 1 (nth 1 expr))))
		    (setq arg (nth 1 expr)))
		(eq (car-safe (setq arg (nth 1 expr))) 'calcFunc-register))
	   (math-rwcomp-instr op (nth 1 arg)))
	  ((and (assq (car-safe expr) calc-tweak-eqn-table)
		(= (length expr) 3)
		(eq (car-safe (nth 1 expr)) 'calcFunc-register))
	   (if (math-constp (nth 2 expr))
	       (let ((reg (math-rwcomp-reg)))
		 (setcar (nthcdr 3 (car math-regs)) (nth 2 expr))
		 (math-rwcomp-instr 'rel (nth 1 (nth 1 expr))
				    (car expr) reg))
	     (if (eq (car (nth 2 expr)) 'calcFunc-register)
		 (math-rwcomp-instr 'rel (nth 1 (nth 1 expr))
				    (car expr) (nth 1 (nth 2 expr)))
	       (math-rwcomp-instr 'cond expr))))
	  ((and (eq (car-safe expr) 'calcFunc-eq)
		(= (length expr) 3)
		(eq (car-safe (nth 1 expr)) '%)
		(eq (car-safe (nth 1 (nth 1 expr))) 'calcFunc-register)
		(math-constp (nth 2 (nth 1 expr)))
		(math-constp (nth 2 expr)))
	   (math-rwcomp-instr 'mod (nth 1 (nth 1 (nth 1 expr)))
			      (nth 2 (nth 1 expr)) (nth 2 expr)))
	  ((equal expr '(var remember var-remember))
	   (setq math-remembering 1))
	  ((and (eq (car-safe expr) 'calcFunc-remember)
		(= (length expr) 2))
	   (setq math-remembering (if math-remembering
				      (list 'calcFunc-lor
					    math-remembering (nth 1 expr))
				    (nth 1 expr))))
	  (t (math-rwcomp-instr 'cond expr)))))

(defun math-rwcomp-same-instr (reg1 reg2 neg)
  (math-rwcomp-instr (if (eq (eq (nth 2 (math-rwcomp-reg-entry reg1))
				 (nth 2 (math-rwcomp-reg-entry reg2)))
			     neg)
			 'same-neg
		       'same)
		     reg1 reg2))

(defun math-rwcomp-copy-instr (reg1 reg2 neg)
  (if (eq (eq (nth 2 (math-rwcomp-reg-entry reg1))
	      (nth 2 (math-rwcomp-reg-entry reg2)))
	  neg)
      (math-rwcomp-instr 'copy-neg reg1 reg2)
    (or (eq reg1 reg2)
	(math-rwcomp-instr 'copy reg1 reg2))))

(defun math-rwcomp-reg ()
  (prog1
      math-num-regs
    (setq math-regs (cons (list nil math-num-regs nil 0) math-regs)
	  math-num-regs (1+ math-num-regs))))

(defun math-rwcomp-reg-entry (num)
  (nth (1- (- math-num-regs num)) math-regs))


(defun math-rwcomp-pattern (expr part &optional not-direct)
  (cond ((or (math-rwcomp-no-vars expr)
	     (and (eq (car expr) 'calcFunc-quote)
		  (= (length expr) 2)
		  (setq expr (nth 1 expr))))
 	 (if (eq (car-safe expr) 'calcFunc-register)
	     (math-rwcomp-same-instr part (nth 1 expr) nil)
	   (let ((reg (math-rwcomp-reg)))
	     (setcar (nthcdr 3 (car math-regs)) expr)
	     (math-rwcomp-same-instr part reg nil))))
 	((eq (car expr) 'var)
 	 (let ((entry (assq (nth 2 expr) math-regs)))
	   (if entry
	       (math-rwcomp-same-instr part (nth 1 entry) nil)
	     (if not-direct
 		 (let ((reg (math-rwcomp-reg)))
		   (math-rwcomp-pattern expr reg)
		   (math-rwcomp-copy-instr part reg nil))
	       (if (setq entry (assq (nth 2 expr) math-aliased-vars))
		   (progn
		     (setcar (math-rwcomp-reg-entry (nth 1 entry))
			     (nth 2 expr))
		     (setcar entry nil)
		     (math-rwcomp-copy-instr part (nth 1 entry) nil))
 		 (math-rwcomp-bind-var part expr))))))
 	((and (eq (car expr) 'calcFunc-select)
	      (= (length expr) 2))
 	 (let ((reg (math-rwcomp-reg)))
	   (math-rwcomp-instr 'select part reg)
	   (math-rwcomp-pattern (nth 1 expr) reg)))
 	((and (eq (car expr) 'calcFunc-opt)
	      (memq (length expr) '(2 3)))
 	 (error "opt( ) occurs in context where it is not allowed"))
 	((eq (car expr) 'neg)
 	 (if (eq (car (nth 1 expr)) 'var)
	     (let ((entry (assq (nth 2 (nth 1 expr)) math-regs)))
	       (if entry
		   (math-rwcomp-same-instr part (nth 1 entry) t)
		 (if math-copy-neg
		     (let ((reg (math-rwcomp-best-reg (nth 1 expr))))
		       (math-rwcomp-copy-instr part reg t)
		       (math-rwcomp-pattern (nth 1 expr) reg))
		   (setcar (cdr (cdr (math-rwcomp-reg-entry part))) t)
		   (math-rwcomp-pattern (nth 1 expr) part))))
	   (if (math-rwcomp-is-algebraic (nth 1 expr))
	       (math-rwcomp-cond-instr (list 'calcFunc-eq
					     (math-rwcomp-register-expr part)
					     expr))
	     (let ((reg (math-rwcomp-reg)))
	       (math-rwcomp-instr 'func part 'neg reg)
	       (math-rwcomp-pattern (nth 1 expr) reg)))))
 	((and (eq (car expr) 'calcFunc-apply)
	      (= (length expr) 3))
 	 (let ((reg1 (math-rwcomp-reg))
	       (reg2 (math-rwcomp-reg)))
	   (math-rwcomp-instr 'apply part reg1 reg2)
	   (math-rwcomp-pattern (nth 1 expr) reg1)
	   (math-rwcomp-pattern (nth 2 expr) reg2)))
 	((and (eq (car expr) 'calcFunc-cons)
	      (= (length expr) 3))
 	 (let ((reg1 (math-rwcomp-reg))
	       (reg2 (math-rwcomp-reg)))
	   (math-rwcomp-instr 'cons part reg1 reg2)
	   (math-rwcomp-pattern (nth 1 expr) reg1)
	   (math-rwcomp-pattern (nth 2 expr) reg2)))
 	((and (eq (car expr) 'calcFunc-rcons)
	      (= (length expr) 3))
 	 (let ((reg1 (math-rwcomp-reg))
	       (reg2 (math-rwcomp-reg)))
	   (math-rwcomp-instr 'rcons part reg1 reg2)
	   (math-rwcomp-pattern (nth 1 expr) reg1)
	   (math-rwcomp-pattern (nth 2 expr) reg2)))
 	((and (eq (car expr) 'calcFunc-condition)
	      (>= (length expr) 3))
 	 (math-rwcomp-pattern (nth 1 expr) part)
 	 (setq expr (cdr expr))
 	 (while (setq expr (cdr expr))
	   (let ((cond (math-flatten-lands (car expr))))
	     (while cond
	       (if (math-rwcomp-all-regs-done (car cond))
		   (math-rwcomp-cond-instr (car cond))
 		 (setq math-conds (cons (car cond) math-conds)))
	       (setq cond (cdr cond))))))
 	((and (eq (car expr) 'calcFunc-pand)
	      (= (length expr) 3))
 	 (math-rwcomp-pattern (nth 1 expr) part)
 	 (math-rwcomp-pattern (nth 2 expr) part))
 	((and (eq (car expr) 'calcFunc-por)
	      (= (length expr) 3))
 	 (math-rwcomp-instr 'alt nil nil [nil nil 4])
 	 (let ((math-conds nil)
	       (head math-prog-last)
	       (mark math-bound-vars)
	       (math-copy-neg t))
	   (math-rwcomp-pattern (nth 1 expr) part t)
	   (let ((amark math-aliased-vars)
		 (math-aliased-vars math-aliased-vars)
 		 (tail math-prog-last)
		 (p math-bound-vars)
		 entry)
	     (while (not (eq p mark))
	       (setq entry (assq (car p) math-regs)
		     math-aliased-vars (cons (list (car p) (nth 1 entry) nil)
					     math-aliased-vars)
		     p (cdr p))
	       (setcar (math-rwcomp-reg-entry (nth 1 entry)) nil))
	     (setcar (cdr (car head)) (cdr head))
	     (setcdr head nil)
	     (setq math-prog-last head)
	     (math-rwcomp-pattern (nth 2 expr) part)
	     (math-rwcomp-instr 'same 0 0)
	     (setcdr tail math-prog-last)
	     (setq p math-aliased-vars)
	     (while (not (eq p amark))
	       (if (car (car p))
		   (setcar (math-rwcomp-reg-entry (nth 1 (car p)))
			   (car (car p))))
	       (setq p (cdr p)))))
 	 (math-rwcomp-do-conditions))
 	((and (eq (car expr) 'calcFunc-pnot)
	      (= (length expr) 2))
 	 (math-rwcomp-instr 'alt nil nil [nil nil 4])
 	 (let ((head math-prog-last)
	       (mark math-bound-vars))
	   (math-rwcomp-pattern (nth 1 expr) part)
	   (math-rwcomp-unbind-vars mark)
	   (math-rwcomp-instr 'end-alt head)
	   (math-rwcomp-instr 'backtrack)
	   (setcar (cdr (car head)) (cdr head))
	   (setcdr head nil)
	   (setq math-prog-last head)))
 	(t (let ((props (get (car expr) 'math-rewrite-props)))
	     (if (and (eq (car expr) 'calcFunc-plain)
		      (= (length expr) 2)
		      (not (math-primp (nth 1 expr))))
 		 (setq expr (nth 1 expr))) ; but "props" is still nil
	     (if (and (memq 'algebraic props)
		      (math-rwcomp-is-algebraic expr))
 		 (math-rwcomp-cond-instr (list 'calcFunc-eq
					       (math-rwcomp-register-expr part)
					       expr))
	       (if (and (memq 'commut props)
 			(= (length expr) 3))
		   (let ((arg1 (nth 1 expr))
 			 (arg2 (nth 2 expr))
 			 try1 def code head (flip nil))
		     (if (eq (car expr) '-)
 			 (setq arg2 (math-rwcomp-neg arg2)))
		     (setq arg1 (cons arg1 (math-rwcomp-best-reg arg1))
			   arg2 (cons arg2 (math-rwcomp-best-reg arg2)))
		     (or (math-rwcomp-order arg1 arg2)
 			 (setq def arg1 arg1 arg2 arg2 def flip t))
		     (if (math-rwcomp-optional-arg (car expr) arg1)
 			 (error "Too many opt( ) arguments in this context"))
		     (setq def (math-rwcomp-optional-arg (car expr) arg2)
			   head (if (memq (car expr) '(+ -))
				    '(+ -)
				  (if (eq (car expr) '*)
				      '(* /)
				    (list (car expr))))
			   code (if (math-rwcomp-is-constrained
				     (car arg1) head)
				    (if (math-rwcomp-is-constrained
 					 (car arg2) head)
 					0 1)
				  2))
		     (math-rwcomp-multi-instr (and def (list def))
					      'try part head
					      (vector nil nil nil code flip)
					      (cdr arg1))
		     (setq try1 (car math-prog-last))
		     (math-rwcomp-pattern (car arg1) (cdr arg1))
		     (math-rwcomp-instr 'try2 try1 (cdr arg2))
		     (if (and (= part 0) (not def) (not math-rewrite-whole)
			      (not (eq math-rhs t))
 			      (setq def (get (car expr)
 					     'math-rewrite-default)))
 			 (let ((reg1 (math-rwcomp-reg))
 			       (reg2 (math-rwcomp-reg)))
 			   (if (= (aref (nth 3 try1) 3) 0)
 			       (aset (nth 3 try1) 3 1))
			   (math-rwcomp-instr 'try (cdr arg2)
					      (if (equal head '(* /))
						  '(*) head)
 					      (vector nil nil nil
 						      (if (= code 0)
 							  1 2)
 						      nil)
 					      reg1 def)
 			   (setq try1 (car math-prog-last))
 			   (math-rwcomp-pattern (car arg2) reg1)
 			   (math-rwcomp-instr 'try2 try1 reg2)
 			   (setq math-rhs (list (if (eq (car expr) '-)
 						    '+ (car expr))
 						math-rhs
 						(list 'calcFunc-register
 						      reg2))))
 		       (math-rwcomp-pattern (car arg2) (cdr arg2))))
 		 (let* ((args (mapcar (function
 				       (lambda (x)
 					 (cons x (math-rwcomp-best-reg x))))
 				      (cdr expr)))
 			(args2 (copy-sequence args))
 			(argp (reverse args2))
 			(defs nil)
 			(num 1))
 		   (while argp
 		     (let ((def (math-rwcomp-optional-arg (car expr)
 							  (car argp))))
 		       (if def
 			   (progn
 			     (setq args2 (delq (car argp) args2)
 				   defs (cons (cons def (cdr (car argp)))
 					      defs))
 			     (math-rwcomp-multi-instr
 			      (mapcar 'cdr args2)
 			      (if (or (and (memq 'unary1 props)
 					   (= (length args2) 1)
 					   (eq (car args2) (car args)))
 				      (and (memq 'unary2 props)
 					   (= (length args) 2)
 					   (eq (car args2) (nth 1 args))))
 				  'func-opt
 				'func-def)
 			      part (car expr)
 			      defs))))
 		     (setq argp (cdr argp)))
 		   (math-rwcomp-multi-instr (mapcar 'cdr args)
 					    'func part (car expr))
 		   (setq args (sort args 'math-rwcomp-order))
 		   (while args
 		     (math-rwcomp-pattern (car (car args)) (cdr (car args)))
 		     (setq num (1+ num)
 			   args (cdr args))))))))))

(defun math-rwcomp-best-reg (x)
  (or (and (eq (car-safe x) 'var)
	   (let ((entry (assq (nth 2 x) math-aliased-vars)))
	     (and entry
		  (not (nth 2 entry))
		  (not (nth 2 (math-rwcomp-reg-entry (nth 1 entry))))
		  (progn
		    (setcar (cdr (cdr entry)) t)
		    (nth 1 entry)))))
      (math-rwcomp-reg)))

(defun math-rwcomp-all-regs-done (expr)
  (if (Math-primp expr)
      (or (not (eq (car-safe expr) 'var))
	  (assq (nth 2 expr) math-regs)
	  (eq (nth 2 expr) 'var-remember)
	  (math-const-var expr))
    (if (and (eq (car expr) 'calcFunc-let)
	     (= (length expr) 3))
	(math-rwcomp-all-regs-done (nth 2 expr))
      (if (and (eq (car expr) 'calcFunc-let)
	       (= (length expr) 2)
	       (eq (car-safe (nth 1 expr)) 'calcFunc-assign)
	       (= (length (nth 1 expr)) 3))
	  (math-rwcomp-all-regs-done (nth 2 (nth 1 expr)))
	(while (and (setq expr (cdr expr))
		    (math-rwcomp-all-regs-done (car expr))))
	(null expr)))))

(defun math-rwcomp-no-vars (expr)
  (if (Math-primp expr)
      (or (not (eq (car-safe expr) 'var))
	  (math-const-var expr))
    (and (not (memq (car expr) '(calcFunc-condition
				 calcFunc-select calcFunc-quote
				 calcFunc-plain calcFunc-opt
				 calcFunc-por calcFunc-pand
				 calcFunc-pnot calcFunc-apply
				 calcFunc-cons calcFunc-rcons)))
	 (progn
	   (while (and (setq expr (cdr expr))
		       (math-rwcomp-no-vars (car expr))))
	   (null expr)))))

(defun math-rwcomp-is-algebraic (expr)
  (if (Math-primp expr)
      (or (not (eq (car-safe expr) 'var))
	  (math-const-var expr)
	  (assq (nth 2 expr) math-regs))
    (and (memq 'algebraic (get (car expr) 'math-rewrite-props))
	 (progn
	   (while (and (setq expr (cdr expr))
		       (math-rwcomp-is-algebraic (car expr))))
	   (null expr)))))

(defun math-rwcomp-is-constrained (expr not-these)
  (if (Math-primp expr)
      (not (eq (car-safe expr) 'var))
    (if (eq (car expr) 'calcFunc-plain)
	(math-rwcomp-is-constrained (nth 1 expr) not-these)
      (not (or (memq (car expr) '(neg calcFunc-select))
	       (memq (car expr) not-these)
	       (and (memq 'commut (get (car expr) 'math-rewrite-props))
		    (or (eq (car-safe (nth 1 expr)) 'calcFunc-opt)
			(eq (car-safe (nth 2 expr)) 'calcFunc-opt))))))))

(defun math-rwcomp-optional-arg (head argp)
  (let ((arg (car argp)))
    (if (eq (car-safe arg) 'calcFunc-opt)
	(and (memq (length arg) '(2 3))
	     (progn
	       (or (eq (car-safe (nth 1 arg)) 'var)
		   (error "First argument of opt( ) must be a variable"))
	       (setcar argp (nth 1 arg))
	       (if (= (length arg) 2)
		   (or (get head 'math-rewrite-default)
		       (error "opt( ) must include a default in this context"))
		 (nth 2 arg))))
      (and (eq (car-safe arg) 'neg)
	   (let* ((part (list (nth 1 arg)))
		  (partp (math-rwcomp-optional-arg head part)))
	     (and partp
		  (setcar argp (math-rwcomp-neg (car part)))
		  (math-neg partp)))))))

(defun math-rwcomp-neg (expr)
  (if (memq (car-safe expr) '(* /))
      (if (eq (car-safe (nth 1 expr)) 'var)
	  (list (car expr) (list 'neg (nth 1 expr)) (nth 2 expr))
	(if (eq (car-safe (nth 2 expr)) 'var)
	    (list (car expr) (nth 1 expr) (list 'neg (nth 2 expr)))
	  (math-neg expr)))
    (math-neg expr)))

(defun math-rwcomp-assoc-args (expr)
  (if (and (eq (car-safe (nth 1 expr)) (car expr))
	   (= (length (nth 1 expr)) 3))
      (math-rwcomp-assoc-args (nth 1 expr)))
  (if (and (eq (car-safe (nth 2 expr)) (car expr))
	   (= (length (nth 2 expr)) 3))
      (math-rwcomp-assoc-args (nth 2 expr))))

(defun math-rwcomp-addsub-args (expr)
  (if (memq (car-safe (nth 1 expr)) '(+ -))
      (math-rwcomp-addsub-args (nth 1 expr)))
  (if (eq (car expr) '-)
      ()
    (if (eq (car-safe (nth 2 expr)) '+)
	(math-rwcomp-addsub-args (nth 2 expr)))))

(defun math-rwcomp-order (a b)
  (< (math-rwcomp-priority (car a))
     (math-rwcomp-priority (car b))))

;; Order of priority:    0 Constants and other exact matches (first)
;;			 10 Functions (except below)
;;			 20 Meta-variables which occur more than once
;;			 30 Algebraic functions
;;			 40 Commutative/associative functions
;;			 50 Meta-variables which occur only once
;;		       +100 for every "!!!" (pnot) in the pattern
;;		      10000 Optional arguments (last)

(defun math-rwcomp-priority (expr)
  (+ (math-rwcomp-count-pnots expr)
     (cond ((eq (car-safe expr) 'calcFunc-opt)
	    10000)
	   ((math-rwcomp-no-vars expr)
	    0)
	   ((eq (car expr) 'calcFunc-quote)
	    0)
	   ((eq (car expr) 'var)
	    (if (assq (nth 2 expr) math-regs)
		0
	      (if (= (math-rwcomp-count-refs expr) 1)
		  50
		20)))
	   (t (let ((props (get (car expr) 'math-rewrite-props)))
		(if (or (memq 'commut props)
			(memq 'assoc props))
		    40
		  (if (memq 'algebraic props)
		      30
		    10)))))))

(defun math-rwcomp-count-refs (var)
  (let ((count (or (math-expr-contains-count math-pattern var) 0))
	(p math-conds))
    (while p
      (if (eq (car-safe (car p)) 'calcFunc-let)
	  (if (= (length (car p)) 3)
	      (setq count (+ count
			     (or (math-expr-contains-count (nth 2 (car p)) var)
				 0)))
	    (if (and (= (length (car p)) 2)
		     (eq (car-safe (nth 1 (car p))) 'calcFunc-assign)
		     (= (length (nth 1 (car p))) 3))
		(setq count (+ count
			       (or (math-expr-contains-count
				    (nth 2 (nth 1 (car p))) var) 0))))))
      (setq p (cdr p)))
    count))

(defun math-rwcomp-count-pnots (expr)
  (if (Math-primp expr)
      0
    (if (eq (car expr) 'calcFunc-pnot)
	100
      (let ((count 0))
	(while (setq expr (cdr expr))
	  (setq count (+ count (math-rwcomp-count-pnots (car expr)))))
	count))))

;; In the current implementation, all associative functions must
;; also be commutative.

(put '+		     'math-rewrite-props '(algebraic assoc commut))
(put '-		     'math-rewrite-props '(algebraic assoc commut)) ; see below
(put '*		     'math-rewrite-props '(algebraic assoc commut)) ; see below
(put '/		     'math-rewrite-props '(algebraic unary1))
(put '^		     'math-rewrite-props '(algebraic unary1))
(put '%		     'math-rewrite-props '(algebraic))
(put 'neg	     'math-rewrite-props '(algebraic))
(put 'calcFunc-idiv  'math-rewrite-props '(algebraic))
(put 'calcFunc-abs   'math-rewrite-props '(algebraic))
(put 'calcFunc-sign  'math-rewrite-props '(algebraic))
(put 'calcFunc-round 'math-rewrite-props '(algebraic))
(put 'calcFunc-rounde 'math-rewrite-props '(algebraic))
(put 'calcFunc-roundu 'math-rewrite-props '(algebraic))
(put 'calcFunc-trunc 'math-rewrite-props '(algebraic))
(put 'calcFunc-floor 'math-rewrite-props '(algebraic))
(put 'calcFunc-ceil  'math-rewrite-props '(algebraic))
(put 'calcFunc-re    'math-rewrite-props '(algebraic))
(put 'calcFunc-im    'math-rewrite-props '(algebraic))
(put 'calcFunc-conj  'math-rewrite-props '(algebraic))
(put 'calcFunc-arg   'math-rewrite-props '(algebraic))
(put 'calcFunc-and   'math-rewrite-props '(assoc commut))
(put 'calcFunc-or    'math-rewrite-props '(assoc commut))
(put 'calcFunc-xor   'math-rewrite-props '(assoc commut))
(put 'calcFunc-eq    'math-rewrite-props '(commut))
(put 'calcFunc-neq   'math-rewrite-props '(commut))
(put 'calcFunc-land  'math-rewrite-props '(assoc commut))
(put 'calcFunc-lor   'math-rewrite-props '(assoc commut))
(put 'calcFunc-beta  'math-rewrite-props '(commut))
(put 'calcFunc-gcd   'math-rewrite-props '(assoc commut))
(put 'calcFunc-lcm   'math-rewrite-props '(assoc commut))
(put 'calcFunc-max   'math-rewrite-props '(algebraic assoc commut))
(put 'calcFunc-min   'math-rewrite-props '(algebraic assoc commut))
(put 'calcFunc-vunion 'math-rewrite-props '(assoc commut))
(put 'calcFunc-vint  'math-rewrite-props '(assoc commut))
(put 'calcFunc-vxor  'math-rewrite-props '(assoc commut))

;; Note: "*" is not commutative for matrix args, but we pretend it is.
;; Also, "-" is not commutative but the code tweaks things so that it is.

(put '+		     'math-rewrite-default  0)
(put '-		     'math-rewrite-default  0)
(put '*		     'math-rewrite-default  1)
(put '/		     'math-rewrite-default  1)
(put '^		     'math-rewrite-default  1)
(put 'calcFunc-land  'math-rewrite-default  1)
(put 'calcFunc-lor   'math-rewrite-default  0)
(put 'calcFunc-vunion 'math-rewrite-default '(vec))
(put 'calcFunc-vint  'math-rewrite-default '(vec))
(put 'calcFunc-vdiff 'math-rewrite-default '(vec))
(put 'calcFunc-vxor  'math-rewrite-default '(vec))

(defmacro math-rwfail (&optional back)
  (list 'setq 'pc
	(list 'and
	      (if back
		  '(setq btrack (cdr btrack))
		'btrack)
	      ''((backtrack)))))

;; This monstrosity is necessary because the use of static vectors of
;; registers makes rewrite rules non-reentrant.  Yucko!
(defmacro math-rweval (form)
  (list 'let '((orig (car rules)))
	'(setcar rules (quote (nil nil nil no-phase)))
	(list 'unwind-protect
	      form
	      '(setcar rules orig))))

(defvar math-rewrite-phase 1)

;; The variable math-apply-rw-regs is local to math-apply-rewrites,
;; but is used by math-rwapply-replace-regs and math-rwapply-reg-looks-negp
;; which are called by math-apply-rewrites.
(defvar math-apply-rw-regs)

;; The variable math-apply-rw-ruleset is local to math-apply-rewrites,
;; but is used by math-rwapply-remember.
(defvar math-apply-rw-ruleset)

(defun math-apply-rewrites (expr rules &optional heads math-apply-rw-ruleset)
  (and
   (setq rules (cdr (or (assq (car-safe expr) rules)
			(assq nil rules))))
   (let ((result nil)
	 op math-apply-rw-regs inst part pc mark btrack
	 (tracing math-rwcomp-tracing)
	 (phase math-rewrite-phase))
     (while rules
       (or
	(and (setq part (nth 2 (car rules)))
	     heads
	     (not (memq part heads)))
	(and (setq part (nth 3 (car rules)))
	     (not (memq phase part)))
	(progn
	  (setq math-apply-rw-regs (car (car rules))
		pc (nth 1 (car rules))
		btrack nil)
	  (aset math-apply-rw-regs 0 expr)
	  (while pc

	    (and tracing
		 (progn (terpri) (princ (car pc))
			(if (and (natnump (nth 1 (car pc)))
				 (< (nth 1 (car pc)) (length math-apply-rw-regs)))
			    (princ 
                             (format "\n  part = %s"
                                     (aref math-apply-rw-regs (nth 1 (car pc))))))))

	    (cond ((eq (setq op (car (setq inst (car pc)))) 'func)
		   (if (and (consp 
                             (setq part (aref math-apply-rw-regs (car (cdr inst)))))
			    (eq (car part)
				(car (setq inst (cdr (cdr inst)))))
			    (progn
			      (while (and (setq inst (cdr inst)
						part (cdr part))
					  inst)
				(aset math-apply-rw-regs (car inst) (car part)))
			      (not (or inst part))))
		       (setq pc (cdr pc))
		     (math-rwfail)))

		  ((eq op 'same)
		   (if (or (equal (setq part (aref math-apply-rw-regs (nth 1 inst)))
				  (setq mark (aref math-apply-rw-regs (nth 2 inst))))
			   (Math-equal part mark))
		       (setq pc (cdr pc))
		     (math-rwfail)))

		  ((and (eq op 'try)
			calc-matrix-mode
			(not (eq calc-matrix-mode 'scalar))
			(eq (car (nth 2 inst)) '*)
			(consp (setq part (aref math-apply-rw-regs (car (cdr inst)))))
			(eq (car part) '*)
			(not (math-known-scalarp part)))
		   (setq mark (nth 3 inst)
			 pc (cdr pc))
		   (if (aref mark 4)
		       (progn
			 (aset math-apply-rw-regs (nth 4 inst) (nth 2 part))
			 (aset mark 1 (cdr (cdr part))))
		     (aset math-apply-rw-regs (nth 4 inst) (nth 1 part))
		     (aset mark 1 (cdr part)))
		   (aset mark 0 (cdr part))
		   (aset mark 2 0))

		  ((eq op 'try)
		   (if (and (consp (setq part 
                                         (aref math-apply-rw-regs (car (cdr inst)))))
			    (memq (car part) (nth 2 inst))
			    (= (length part) 3)
			    (or (not (eq (car part) '/))
				(Math-objectp (nth 2 part))))
		       (progn
			 (setq op nil
			       mark (car (cdr (setq inst (cdr (cdr inst))))))
			 (and
			  (memq 'assoc (get (car part) 'math-rewrite-props))
			  (not (= (aref mark 3) 0))
			  (while (if (and (consp (nth 1 part))
					  (memq (car (nth 1 part)) (car inst)))
				     (setq op (cons (if (eq (car part) '-)
							(math-rwapply-neg
							 (nth 2 part))
						      (nth 2 part))
						    op)
					   part (nth 1 part))
				   (if (and (consp (nth 2 part))
					    (memq (car (nth 2 part))
						  (car inst))
					    (not (eq (car (nth 2 part)) '-)))
				       (setq op (cons (nth 1 part) op)
					     part (nth 2 part))))))
			 (setq op (cons (nth 1 part)
					(cons (if (eq (car part) '-)
						  (math-rwapply-neg
						   (nth 2 part))
						(if (eq (car part) '/)
						    (math-rwapply-inv
						     (nth 2 part))
						  (nth 2 part)))
					      op))
			       btrack (cons pc btrack)
			       pc (cdr pc))
			 (aset math-apply-rw-regs (nth 2 inst) (car op))
			 (aset mark 0 op)
			 (aset mark 1 op)
			 (aset mark 2 (if (cdr (cdr op)) 1 0)))
		     (if (nth 5 inst)
			 (if (and (consp part)
				  (eq (car part) 'neg)
				  (eq (car (nth 2 inst)) '*)
				  (eq (nth 5 inst) 1))
			     (progn
			       (setq mark (nth 3 inst)
				     pc (cdr pc))
			       (aset math-apply-rw-regs (nth 4 inst) (nth 1 part))
			       (aset mark 1 -1)
			       (aset mark 2 4))
			   (setq mark (nth 3 inst)
				 pc (cdr pc))
			   (aset math-apply-rw-regs (nth 4 inst) part)
			   (aset mark 2 3))
		       (math-rwfail))))

		  ((eq op 'try2)
		   (setq part (nth 1 inst)   ; try instr
			 mark (nth 3 part)
			 op (aref mark 2)
			 pc (cdr pc))
		   (aset math-apply-rw-regs (nth 2 inst)
			 (cond
			  ((eq op 0)
			   (if (eq (aref mark 0) (aref mark 1))
			       (nth 1 (aref mark 0))
			     (car (aref mark 0))))
			  ((eq op 1)
			   (setq mark (delq (car (aref mark 1))
					    (copy-sequence (aref mark 0)))
				 op (car (nth 2 part)))
			   (if (eq op '*)
			       (progn
				 (setq mark (nreverse mark)
				       part (list '* (nth 1 mark) (car mark))
				       mark (cdr mark))
				 (while (setq mark (cdr mark))
				   (setq part (list '* (car mark) part))))
			     (setq part (car mark)
				   mark (cdr mark)
				   part (if (and (eq op '+)
						 (consp (car mark))
						 (eq (car (car mark)) 'neg))
					    (list '- part
						  (nth 1 (car mark)))
					  (list op part (car mark))))
			     (while (setq mark (cdr mark))
			       (setq part (if (and (eq op '+)
						   (consp (car mark))
						   (eq (car (car mark)) 'neg))
					      (list '- part
						    (nth 1 (car mark)))
					    (list op part (car mark))))))
			   part)
			  ((eq op 2)
			   (car (aref mark 1)))
			  ((eq op 3) (nth 5 part))
			  (t (aref mark 1)))))

		  ((eq op 'select)
		   (setq pc (cdr pc))
		   (if (and (consp (setq part (aref math-apply-rw-regs (nth 1 inst))))
			    (eq (car part) 'calcFunc-select))
		       (aset math-apply-rw-regs (nth 2 inst) (nth 1 part))
		     (if math-rewrite-selections
			 (math-rwfail)
		       (aset math-apply-rw-regs (nth 2 inst) part))))

		  ((eq op 'same-neg)
		   (if (or (equal (setq part (aref math-apply-rw-regs (nth 1 inst)))
				  (setq mark (math-neg
					      (aref math-apply-rw-regs (nth 2 inst)))))
			   (Math-equal part mark))
		       (setq pc (cdr pc))
		     (math-rwfail)))

		  ((eq op 'backtrack)
		   (setq inst (car (car btrack))   ; "try" or "alt" instr
			 pc (cdr (car btrack))
			 mark (or (nth 3 inst) [nil nil 4])
			 op (aref mark 2))
		   (cond ((eq op 0)
			  (if (setq op (cdr (aref mark 1)))
			      (aset math-apply-rw-regs (nth 4 inst) 
                                    (car (aset mark 1 op)))
			    (if (nth 5 inst)
				(progn
				  (aset mark 2 3)
				  (aset math-apply-rw-regs (nth 4 inst)
					(aref math-apply-rw-regs (nth 1 inst))))
			      (math-rwfail t))))
			 ((eq op 1)
			  (if (setq op (cdr (aref mark 1)))
			      (aset math-apply-rw-regs (nth 4 inst) 
                                    (car (aset mark 1 op)))
			    (if (= (aref mark 3) 1)
				(if (nth 5 inst)
				    (progn
				      (aset mark 2 3)
				      (aset math-apply-rw-regs (nth 4 inst)
					    (aref math-apply-rw-regs (nth 1 inst))))
				  (math-rwfail t))
			      (aset mark 2 2)
			      (aset mark 1 (cons nil (aref mark 0)))
			      (math-rwfail))))
			 ((eq op 2)
			  (if (setq op (cdr (aref mark 1)))
			      (progn
				(setq mark (delq (car (aset mark 1 op))
						 (copy-sequence
						  (aref mark 0)))
				      op (car (nth 2 inst)))
				(if (eq op '*)
				    (progn
				      (setq mark (nreverse mark)
					    part (list '* (nth 1 mark)
						       (car mark))
					    mark (cdr mark))
				      (while (setq mark (cdr mark))
					(setq part (list '* (car mark)
							 part))))
				  (setq part (car mark)
					mark (cdr mark)
					part (if (and (eq op '+)
						      (consp (car mark))
						      (eq (car (car mark))
							  'neg))
						 (list '- part
						       (nth 1 (car mark)))
					       (list op part (car mark))))
				  (while (setq mark (cdr mark))
				    (setq part (if (and (eq op '+)
							(consp (car mark))
							(eq (car (car mark))
							    'neg))
						   (list '- part
							 (nth 1 (car mark)))
						 (list op part (car mark))))))
				(aset math-apply-rw-regs (nth 4 inst) part))
			    (if (nth 5 inst)
				(progn
				  (aset mark 2 3)
				  (aset math-apply-rw-regs (nth 4 inst)
					(aref math-apply-rw-regs (nth 1 inst))))
			      (math-rwfail t))))
			 ((eq op 4)
			  (setq btrack (cdr btrack)))
			 (t (math-rwfail t))))

		  ((eq op 'integer)
		   (if (Math-integerp (setq part 
                                            (aref math-apply-rw-regs (nth 1 inst))))
		       (setq pc (cdr pc))
		     (if (Math-primp part)
			 (math-rwfail)
		       (setq part (math-rweval (math-simplify part)))
		       (if (Math-integerp part)
			   (setq pc (cdr pc))
			 (math-rwfail)))))

		  ((eq op 'real)
		   (if (Math-realp (setq part (aref math-apply-rw-regs (nth 1 inst))))
		       (setq pc (cdr pc))
		     (if (Math-primp part)
			 (math-rwfail)
		       (setq part (math-rweval (math-simplify part)))
		       (if (Math-realp part)
			   (setq pc (cdr pc))
			 (math-rwfail)))))

		  ((eq op 'constant)
		   (if (math-constp (setq part (aref math-apply-rw-regs (nth 1 inst))))
		       (setq pc (cdr pc))
		     (if (Math-primp part)
			 (math-rwfail)
		       (setq part (math-rweval (math-simplify part)))
		       (if (math-constp part)
			   (setq pc (cdr pc))
			 (math-rwfail)))))

		  ((eq op 'negative)
		   (if (math-looks-negp (setq part 
                                              (aref math-apply-rw-regs (nth 1 inst))))
		       (setq pc (cdr pc))
		     (if (Math-primp part)
			 (math-rwfail)
		       (setq part (math-rweval (math-simplify part)))
		       (if (math-looks-negp part)
			   (setq pc (cdr pc))
			 (math-rwfail)))))

		  ((eq op 'rel)
		   (setq part (math-compare (aref math-apply-rw-regs (nth 1 inst))
					    (aref math-apply-rw-regs (nth 3 inst)))
			 op (nth 2 inst))
		   (if (= part 2)
		       (setq part (math-rweval
				   (math-simplify
				    (calcFunc-sign
				     (math-sub 
                                      (aref math-apply-rw-regs (nth 1 inst))
                                      (aref math-apply-rw-regs (nth 3 inst))))))))
		   (if (cond ((eq op 'calcFunc-eq)
			      (eq part 0))
			     ((eq op 'calcFunc-neq)
			      (memq part '(-1 1)))
			     ((eq op 'calcFunc-lt)
			      (eq part -1))
			     ((eq op 'calcFunc-leq)
			      (memq part '(-1 0)))
			     ((eq op 'calcFunc-gt)
			      (eq part 1))
			     ((eq op 'calcFunc-geq)
			      (memq part '(0 1))))
		       (setq pc (cdr pc))
		     (math-rwfail)))

		  ((eq op 'func-def)
		   (if (and 
                        (consp (setq part (aref math-apply-rw-regs (car (cdr inst)))))
                        (eq (car part)
                            (car (setq inst (cdr (cdr inst))))))
		       (progn
			 (setq inst (cdr inst)
			       mark (car inst))
			 (while (and (setq inst (cdr inst)
					   part (cdr part))
				     inst)
			   (aset math-apply-rw-regs (car inst) (car part)))
			 (if (or inst part)
			     (setq pc (cdr pc))
			   (while (eq (car (car (setq pc (cdr pc))))
				      'func-def))
			   (setq pc (cdr pc))   ; skip over "func"
			   (while mark
			     (aset math-apply-rw-regs (cdr (car mark)) (car (car mark)))
			     (setq mark (cdr mark)))))
		     (math-rwfail)))

		  ((eq op 'func-opt)
		   (if (or (not 
                            (and 
                             (consp
                              (setq part (aref math-apply-rw-regs (car (cdr inst)))))
                             (eq (car part) (nth 2 inst))))
			   (and (= (length part) 2)
				(setq part (nth 1 part))))
		       (progn
			 (setq mark (nth 3 inst))
			 (aset math-apply-rw-regs (nth 4 inst) part)
			 (while (eq (car (car (setq pc (cdr pc)))) 'func-def))
			 (setq pc (cdr pc))   ; skip over "func"
			 (while mark
			   (aset math-apply-rw-regs (cdr (car mark)) (car (car mark)))
			   (setq mark (cdr mark))))
		     (setq pc (cdr pc))))

		  ((eq op 'mod)
		   (if (if (Math-zerop 
                            (setq part (aref math-apply-rw-regs (nth 1 inst))))
			   (Math-zerop (nth 3 inst))
			 (and (not (Math-zerop (nth 2 inst)))
			      (progn
				(setq part (math-mod part (nth 2 inst)))
				(or (Math-numberp part)
				    (setq part (math-rweval
						(math-simplify part))))
				(Math-equal part (nth 3 inst)))))
		       (setq pc (cdr pc))
		     (math-rwfail)))

		  ((eq op 'apply)
		   (if (and (consp 
                             (setq part (aref math-apply-rw-regs (car (cdr inst)))))
			    (not (Math-objvecp part))
			    (not (eq (car part) 'var)))
		       (progn
			 (aset math-apply-rw-regs (nth 2 inst)
			       (math-calcFunc-to-var (car part)))
			 (aset math-apply-rw-regs (nth 3 inst)
			       (cons 'vec (cdr part)))
			 (setq pc (cdr pc)))
		     (math-rwfail)))

		  ((eq op 'cons)
		   (if (and (consp 
                             (setq part (aref math-apply-rw-regs (car (cdr inst)))))
			    (eq (car part) 'vec)
			    (cdr part))
		       (progn
			 (aset math-apply-rw-regs (nth 2 inst) (nth 1 part))
			 (aset math-apply-rw-regs (nth 3 inst) 
                               (cons 'vec (cdr (cdr part))))
			 (setq pc (cdr pc)))
		     (math-rwfail)))

		  ((eq op 'rcons)
		   (if (and (consp 
                             (setq part (aref math-apply-rw-regs (car (cdr inst)))))
			    (eq (car part) 'vec)
			    (cdr part))
		       (progn
			 (aset math-apply-rw-regs (nth 2 inst) (calcFunc-rhead part))
			 (aset math-apply-rw-regs (nth 3 inst) (calcFunc-rtail part))
			 (setq pc (cdr pc)))
		     (math-rwfail)))

		  ((eq op 'cond)
		   (if (math-is-true
			(math-rweval
			 (math-simplify
			  (math-rwapply-replace-regs (nth 1 inst)))))
		       (setq pc (cdr pc))
		     (math-rwfail)))

		  ((eq op 'let)
		   (aset math-apply-rw-regs (nth 1 inst)
			 (math-rweval
			  (math-normalize
			   (math-rwapply-replace-regs (nth 2 inst)))))
		   (setq pc (cdr pc)))

		  ((eq op 'copy)
		   (aset math-apply-rw-regs (nth 2 inst) 
                         (aref math-apply-rw-regs (nth 1 inst)))
		   (setq pc (cdr pc)))

		  ((eq op 'copy-neg)
		   (aset math-apply-rw-regs (nth 2 inst)
			 (math-rwapply-neg (aref math-apply-rw-regs (nth 1 inst))))
		   (setq pc (cdr pc)))

		  ((eq op 'alt)
		   (setq btrack (cons pc btrack)
			 pc (nth 1 inst)))

		  ((eq op 'end-alt)
		   (while (and btrack (not (eq (car btrack) (nth 1 inst))))
		     (setq btrack (cdr btrack)))
		   (setq btrack (cdr btrack)
			 pc (cdr pc)))

		  ((eq op 'done)
		   (setq result (math-rwapply-replace-regs (nth 1 inst)))
		   (if (or (and (eq (car-safe result) '+)
				(eq (nth 2 result) 0))
			   (and (eq (car-safe result) '*)
				(eq (nth 2 result) 1)))
		       (setq result (nth 1 result)))
		   (setq part (and (nth 2 inst)
				   (math-is-true
				    (math-rweval
				     (math-simplify
				      (math-rwapply-replace-regs
				       (nth 2 inst)))))))
		   (if (or (equal result expr)
			   (equal (setq result (math-normalize result)) expr))
		       (setq result nil)
		     (if part (math-rwapply-remember expr result))
		     (setq rules nil))
		   (setq pc nil))

		  (t (error "%s is not a valid rewrite opcode" op))))))
       (setq rules (cdr rules)))
     result)))

(defun math-rwapply-neg (expr)
  (if (and (consp expr)
	   (memq (car expr) '(* /)))
      (if (Math-objectp (nth 2 expr))
	  (list (car expr) (nth 1 expr) (math-neg (nth 2 expr)))
	(list (car expr)
	      (if (Math-objectp (nth 1 expr))
		  (math-neg (nth 1 expr))
		(list '* -1 (nth 1 expr)))
	      (nth 2 expr)))
    (math-neg expr)))

(defun math-rwapply-inv (expr)
  (if (and (Math-integerp expr)
	   calc-prefer-frac)
      (math-make-frac 1 expr)
    (list '/ 1 expr)))

(defun math-rwapply-replace-regs (expr)
  (cond ((Math-primp expr)
	 expr)
	((eq (car expr) 'calcFunc-register)
	 (setq expr (aref math-apply-rw-regs (nth 1 expr)))
	 (if (eq (car-safe expr) '*)
	     (if (eq (nth 1 expr) -1)
		 (math-neg (nth 2 expr))
	       (if (eq (nth 1 expr) 1)
		   (nth 2 expr)
		 expr))
	   expr))
	((and (eq (car expr) 'calcFunc-eval)
	      (= (length expr) 2))
	 (calc-with-default-simplification
	  (math-normalize (math-rwapply-replace-regs (nth 1 expr)))))
	((and (eq (car expr) 'calcFunc-evalsimp)
	      (= (length expr) 2))
	 (math-simplify (math-rwapply-replace-regs (nth 1 expr))))
	((and (eq (car expr) 'calcFunc-evalextsimp)
	      (= (length expr) 2))
	 (math-simplify-extended (math-rwapply-replace-regs (nth 1 expr))))
	((and (eq (car expr) 'calcFunc-apply)
	      (= (length expr) 3))
	 (let ((func (math-rwapply-replace-regs (nth 1 expr)))
	       (args (math-rwapply-replace-regs (nth 2 expr)))
	       call)
	   (if (and (math-vectorp args)
		    (not (eq (car-safe (setq call (math-build-call
						   (math-var-to-calcFunc func)
						   (cdr args))))
			     'calcFunc-call)))
	       call
	     (list 'calcFunc-apply func args))))
	((and (eq (car expr) 'calcFunc-cons)
	      (= (length expr) 3))
	 (let ((head (math-rwapply-replace-regs (nth 1 expr)))
	       (tail (math-rwapply-replace-regs (nth 2 expr))))
	   (if (math-vectorp tail)
	       (cons 'vec (cons head (cdr tail)))
	     (list 'calcFunc-cons head tail))))
	((and (eq (car expr) 'calcFunc-rcons)
	      (= (length expr) 3))
	 (let ((head (math-rwapply-replace-regs (nth 1 expr)))
	       (tail (math-rwapply-replace-regs (nth 2 expr))))
	   (if (math-vectorp head)
	       (append head (list tail))
	     (list 'calcFunc-rcons head tail))))
	((and (eq (car expr) 'neg)
	      (math-rwapply-reg-looks-negp (nth 1 expr)))
	 (math-rwapply-reg-neg (nth 1 expr)))
	((and (eq (car expr) 'neg)
	      (eq (car-safe (nth 1 expr)) 'calcFunc-register)
	      (math-scalarp (aref math-apply-rw-regs (nth 1 (nth 1 expr)))))
	 (math-neg (math-rwapply-replace-regs (nth 1 expr))))
	((and (eq (car expr) '+)
	      (math-rwapply-reg-looks-negp (nth 1 expr)))
	 (list '- (math-rwapply-replace-regs (nth 2 expr))
	       (math-rwapply-reg-neg (nth 1 expr))))
	((and (eq (car expr) '+)
	      (math-rwapply-reg-looks-negp (nth 2 expr)))
	 (list '- (math-rwapply-replace-regs (nth 1 expr))
	       (math-rwapply-reg-neg (nth 2 expr))))
	((and (eq (car expr) '-)
	      (math-rwapply-reg-looks-negp (nth 2 expr)))
	 (list '+ (math-rwapply-replace-regs (nth 1 expr))
	       (math-rwapply-reg-neg (nth 2 expr))))
	((eq (car expr) '*)
	 (cond ((eq (nth 1 expr) -1)
		(if (math-rwapply-reg-looks-negp (nth 2 expr))
		    (math-rwapply-reg-neg (nth 2 expr))
		  (math-neg (math-rwapply-replace-regs (nth 2 expr)))))
	       ((eq (nth 1 expr) 1)
		(math-rwapply-replace-regs (nth 2 expr)))
	       ((eq (nth 2 expr) -1)
		(if (math-rwapply-reg-looks-negp (nth 1 expr))
		    (math-rwapply-reg-neg (nth 1 expr))
		  (math-neg (math-rwapply-replace-regs (nth 1 expr)))))
	       ((eq (nth 2 expr) 1)
		(math-rwapply-replace-regs (nth 1 expr)))
	       (t
		(let ((arg1 (math-rwapply-replace-regs (nth 1 expr)))
		      (arg2 (math-rwapply-replace-regs (nth 2 expr))))
		  (cond ((and (eq (car-safe arg1) '/)
			      (eq (nth 1 arg1) 1))
			 (list '/ arg2 (nth 2 arg1)))
			((and (eq (car-safe arg2) '/)
			      (eq (nth 1 arg2) 1))
			 (list '/ arg1 (nth 2 arg2)))
			(t (list '* arg1 arg2)))))))
	((eq (car expr) '/)
	 (let ((arg1 (math-rwapply-replace-regs (nth 1 expr)))
	       (arg2 (math-rwapply-replace-regs (nth 2 expr))))
	   (if (eq (car-safe arg2) '/)
	       (list '/ (list '* arg1 (nth 2 arg2)) (nth 1 arg2))
	     (list '/ arg1 arg2))))
	((and (eq (car expr) 'calcFunc-plain)
	      (= (length expr) 2))
	 (if (Math-primp (nth 1 expr))
	     (nth 1 expr)
	   (if (eq (car (nth 1 expr)) 'calcFunc-register)
	       (aref math-apply-rw-regs (nth 1 (nth 1 expr)))
	     (cons (car (nth 1 expr)) (mapcar 'math-rwapply-replace-regs
					      (cdr (nth 1 expr)))))))
	(t (cons (car expr) (mapcar 'math-rwapply-replace-regs (cdr expr))))))

(defun math-rwapply-reg-looks-negp (expr)
  (if (eq (car-safe expr) 'calcFunc-register)
      (math-looks-negp (aref math-apply-rw-regs (nth 1 expr)))
    (if (memq (car-safe expr) '(* /))
	(or (math-rwapply-reg-looks-negp (nth 1 expr))
	    (math-rwapply-reg-looks-negp (nth 2 expr))))))

(defun math-rwapply-reg-neg (expr)  ; expr must satisfy rwapply-reg-looks-negp
  (if (eq (car expr) 'calcFunc-register)
      (math-neg (math-rwapply-replace-regs expr))
    (if (math-rwapply-reg-looks-negp (nth 1 expr))
	(math-rwapply-replace-regs (list (car expr)
					 (math-rwapply-reg-neg (nth 1 expr))
					 (nth 2 expr)))
      (math-rwapply-replace-regs (list (car expr)
				       (nth 1 expr)
				       (math-rwapply-reg-neg (nth 2 expr)))))))

(defun math-rwapply-remember (old new)
  (let ((varval (symbol-value (nth 2 (car math-apply-rw-ruleset))))
	(rules (assq (car-safe old) math-apply-rw-ruleset)))
    (if (and (eq (car-safe varval) 'vec)
	     (not (memq (car-safe old) '(nil schedule + -)))
	     rules)
	(progn
	  (setcdr varval (cons (list 'calcFunc-assign
				     (if (math-rwcomp-no-vars old)
					 old
				       (list 'calcFunc-quote old))
				     new)
			       (cdr varval)))
	  (setcdr rules (cons (list (vector nil old)
				    (list (list 'same 0 1)
					  (list 'done new nil))
				    nil nil)
			      (cdr rules)))))))

(provide 'calc-rewr)

;;; calc-rewr.el ends here
