;;; byte-opt.el --- the optimization passes of the emacs-lisp byte compiler -*- lexical-binding: t -*-

;; Copyright (C) 1991, 1994, 2000-2012  Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;;	Hallvard Furuseth <hbf@ulrik.uio.no>
;; Maintainer: FSF
;; Keywords: internal
;; Package: emacs

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

;; ========================================================================
;; "No matter how hard you try, you can't make a racehorse out of a pig.
;; You can, however, make a faster pig."
;;
;; Or, to put it another way, the Emacs byte compiler is a VW Bug.  This code
;; makes it be a VW Bug with fuel injection and a turbocharger...  You're
;; still not going to make it go faster than 70 mph, but it might be easier
;; to get it there.
;;

;; TO DO:
;;
;; (apply (lambda (x &rest y) ...) 1 (foo))
;;
;; maintain a list of functions known not to access any global variables
;; (actually, give them a 'dynamically-safe property) and then
;;   (let ( v1 v2 ... vM vN ) <...dynamically-safe...> )  ==>
;;   (let ( v1 v2 ... vM ) vN <...dynamically-safe...> )
;; by recursing on this, we might be able to eliminate the entire let.
;; However certain variables should never have their bindings optimized
;; away, because they affect everything.
;;   (put 'debug-on-error 'binding-is-magic t)
;;   (put 'debug-on-abort 'binding-is-magic t)
;;   (put 'debug-on-next-call 'binding-is-magic t)
;;   (put 'inhibit-quit 'binding-is-magic t)
;;   (put 'quit-flag 'binding-is-magic t)
;;   (put 't 'binding-is-magic t)
;;   (put 'nil 'binding-is-magic t)
;; possibly also
;;   (put 'gc-cons-threshold 'binding-is-magic t)
;;   (put 'track-mouse 'binding-is-magic t)
;; others?
;;
;; Simple defsubsts often produce forms like
;;    (let ((v1 (f1)) (v2 (f2)) ...)
;;       (FN v1 v2 ...))
;; It would be nice if we could optimize this to
;;    (FN (f1) (f2) ...)
;; but we can't unless FN is dynamically-safe (it might be dynamically
;; referring to the bindings that the lambda arglist established.)
;; One of the uncountable lossages introduced by dynamic scope...
;;
;; Maybe there should be a control-structure that says "turn on
;; fast-and-loose type-assumptive optimizations here."  Then when
;; we see a form like (car foo) we can from then on assume that
;; the variable foo is of type cons, and optimize based on that.
;; But, this won't win much because of (you guessed it) dynamic
;; scope.  Anything down the stack could change the value.
;; (Another reason it doesn't work is that it is perfectly valid
;; to call car with a null argument.)  A better approach might
;; be to allow type-specification of the form
;;   (put 'foo 'arg-types '(float (list integer) dynamic))
;;   (put 'foo 'result-type 'bool)
;; It should be possible to have these types checked to a certain
;; degree.
;;
;; collapse common subexpressions
;;
;; It would be nice if redundant sequences could be factored out as well,
;; when they are known to have no side-effects:
;;   (list (+ a b c) (+ a b c))   -->  a b add c add dup list-2
;; but beware of traps like
;;   (cons (list x y) (list x y))
;;
;; Tail-recursion elimination is not really possible in Emacs Lisp.
;; Tail-recursion elimination is almost always impossible when all variables
;; have dynamic scope, but given that the "return" byteop requires the
;; binding stack to be empty (rather than emptying it itself), there can be
;; no truly tail-recursive Emacs Lisp functions that take any arguments or
;; make any bindings.
;;
;; Here is an example of an Emacs Lisp function which could safely be
;; byte-compiled tail-recursively:
;;
;;  (defun tail-map (fn list)
;;    (cond (list
;;           (funcall fn (car list))
;;           (tail-map fn (cdr list)))))
;;
;; However, if there was even a single let-binding around the COND,
;; it could not be byte-compiled, because there would be an "unbind"
;; byte-op between the final "call" and "return."  Adding a
;; Bunbind_all byteop would fix this.
;;
;;   (defun foo (x y z) ... (foo a b c))
;;   ... (const foo) (varref a) (varref b) (varref c) (call 3) END: (return)
;;   ... (varref a) (varbind x) (varref b) (varbind y) (varref c) (varbind z) (goto 0) END: (unbind-all) (return)
;;   ... (varref a) (varset x) (varref b) (varset y) (varref c) (varset z) (goto 0) END: (return)
;;
;; this also can be considered tail recursion:
;;
;;   ... (const foo) (varref a) (call 1) (goto X) ... X: (return)
;; could generalize this by doing the optimization
;;   (goto X) ... X: (return)  -->  (return)
;;
;; But this doesn't solve all of the problems: although by doing tail-
;; recursion elimination in this way, the call-stack does not grow, the
;; binding-stack would grow with each recursive step, and would eventually
;; overflow.  I don't believe there is any way around this without lexical
;; scope.
;;
;; Wouldn't it be nice if Emacs Lisp had lexical scope.
;;
;; Idea: the form (lexical-scope) in a file means that the file may be
;; compiled lexically.  This proclamation is file-local.  Then, within
;; that file, "let" would establish lexical bindings, and "let-dynamic"
;; would do things the old way.  (Or we could use CL "declare" forms.)
;; We'd have to notice defvars and defconsts, since those variables should
;; always be dynamic, and attempting to do a lexical binding of them
;; should simply do a dynamic binding instead.
;; But!  We need to know about variables that were not necessarily defvared
;; in the file being compiled (doing a boundp check isn't good enough.)
;; Fdefvar() would have to be modified to add something to the plist.
;;
;; A major disadvantage of this scheme is that the interpreter and compiler
;; would have different semantics for files compiled with (dynamic-scope).
;; Since this would be a file-local optimization, there would be no way to
;; modify the interpreter to obey this (unless the loader was hacked
;; in some grody way, but that's a really bad idea.)

;; Other things to consider:

;; ;; Associative math should recognize subcalls to identical function:
;; (disassemble (lambda (x) (+ (+ (foo) 1) (+ (bar) 2))))
;; ;; This should generate the same as (1+ x) and (1- x)

;; (disassemble (lambda (x) (cons (+ x 1) (- x 1))))
;; ;; An awful lot of functions always return a non-nil value.  If they're
;; ;; error free also they may act as true-constants.

;; (disassemble (lambda (x) (and (point) (foo))))
;; ;; When
;; ;;   - all but one arguments to a function are constant
;; ;;   - the non-constant argument is an if-expression (cond-expression?)
;; ;; then the outer function can be distributed.  If the guarding
;; ;; condition is side-effect-free [assignment-free] then the other
;; ;; arguments may be any expressions.  Since, however, the code size
;; ;; can increase this way they should be "simple".  Compare:

;; (disassemble (lambda (x) (eq (if (point) 'a 'b) 'c)))
;; (disassemble (lambda (x) (if (point) (eq 'a 'c) (eq 'b 'c))))

;; ;; (car (cons A B)) -> (prog1 A B)
;; (disassemble (lambda (x) (car (cons (foo) 42))))

;; ;; (cdr (cons A B)) -> (progn A B)
;; (disassemble (lambda (x) (cdr (cons 42 (foo)))))

;; ;; (car (list A B ...)) -> (prog1 A B ...)
;; (disassemble (lambda (x) (car (list (foo) 42 (bar)))))

;; ;; (cdr (list A B ...)) -> (progn A (list B ...))
;; (disassemble (lambda (x) (cdr (list 42 (foo) (bar)))))


;;; Code:

(require 'bytecomp)
(eval-when-compile (require 'cl))

(defun byte-compile-log-lap-1 (format &rest args)
  ;; Newer byte codes for stack-ref make the slot 0 non-nil again.
  ;; But the "old disassembler" is *really* ancient by now.
  ;; (if (aref byte-code-vector 0)
  ;;     (error "The old version of the disassembler is loaded.  Reload new-bytecomp as well"))
  (byte-compile-log-1
   (apply 'format format
     (let (c a)
       (mapcar (lambda (arg)
		  (if (not (consp arg))
		      (if (and (symbolp arg)
			       (string-match "^byte-" (symbol-name arg)))
			  (intern (substring (symbol-name arg) 5))
			arg)
		    (if (integerp (setq c (car arg)))
			(error "non-symbolic byte-op %s" c))
		    (if (eq c 'TAG)
			(setq c arg)
		      (setq a (cond ((memq c byte-goto-ops)
				     (car (cdr (cdr arg))))
				    ((memq c byte-constref-ops)
				     (car (cdr arg)))
				    (t (cdr arg))))
		      (setq c (symbol-name c))
		      (if (string-match "^byte-." c)
			  (setq c (intern (substring c 5)))))
		    (if (eq c 'constant) (setq c 'const))
		    (if (and (eq (cdr arg) 0)
			     (not (memq c '(unbind call const))))
			c
		      (format "(%s %s)" c a))))
	       args)))))

(defmacro byte-compile-log-lap (format-string &rest args)
  `(and (memq byte-optimize-log '(t byte))
	(byte-compile-log-lap-1 ,format-string ,@args)))


;;; byte-compile optimizers to support inlining

(put 'inline 'byte-optimizer 'byte-optimize-inline-handler)

(defun byte-optimize-inline-handler (form)
  "byte-optimize-handler for the `inline' special-form."
  (cons 'progn
	(mapcar
	 (lambda (sexp)
	   (let ((f (car-safe sexp)))
	     (if (and (symbolp f)
		      (or (cdr (assq f byte-compile-function-environment))
			  (not (or (not (fboundp f))
				   (cdr (assq f byte-compile-macro-environment))
				   (and (consp (setq f (symbol-function f)))
					(eq (car f) 'macro))
				   (subrp f)))))
		 (byte-compile-inline-expand sexp)
	       sexp)))
	 (cdr form))))

(defun byte-compile-inline-expand (form)
  (let* ((name (car form))
         (localfn (cdr (assq name byte-compile-function-environment)))
	 (fn (or localfn (and (fboundp name) (symbol-function name)))))
    (when (and (consp fn) (eq (car fn) 'autoload))
      (load (nth 1 fn))
      (setq fn (or (and (fboundp name) (symbol-function name))
                   (cdr (assq name byte-compile-function-environment)))))
    (pcase fn
      (`nil
       (byte-compile-warn "attempt to inline `%s' before it was defined"
                          name)
       form)
      (`(autoload . ,_)
       (error "File `%s' didn't define `%s'" (nth 1 fn) name))
      ((and (pred symbolp) (guard (not (eq fn t)))) ;A function alias.
       (byte-compile-inline-expand (cons fn (cdr form))))
      ((pred byte-code-function-p)
       ;; (message "Inlining byte-code for %S!" name)
       ;; The byte-code will be really inlined in byte-compile-unfold-bcf.
       `(,fn ,@(cdr form)))
      ((or (and `(lambda ,args . ,body) (let env nil))
           `(closure ,env ,args . ,body))
       (if (not (or (eq fn localfn)     ;From the same file => same mode.
                    (eq (not lexical-binding) (not env)))) ;Same mode.
           ;; While byte-compile-unfold-bcf can inline dynbind byte-code into
           ;; letbind byte-code (or any other combination for that matter), we
           ;; can only inline dynbind source into dynbind source or letbind
           ;; source into letbind source.
           ;; FIXME: we could of course byte-compile the inlined function
           ;; first, and then inline its byte-code.
           form
         (let ((renv ()))
           ;; Turn the function's closed vars (if any) into local let bindings.
           (dolist (binding env)
             (cond
              ((consp binding)
               ;; We check shadowing by the args, so that the `let' can be
               ;; moved within the lambda, which can then be unfolded.
               ;; FIXME: Some of those bindings might be unused in `body'.
               (unless (memq (car binding) args) ;Shadowed.
                 (push `(,(car binding) ',(cdr binding)) renv)))
              ((eq binding t))
              (t (push `(defvar ,binding) body))))
           (let ((newfn (byte-compile-preprocess
                         (if (null renv)
                             `(lambda ,args ,@body)
                           `(lambda ,args (let ,(nreverse renv) ,@body))))))
             (if (eq (car-safe newfn) 'function)
                 (byte-compile-unfold-lambda `(,(cadr newfn) ,@(cdr form)))
               (byte-compile-log-warning
                (format "Inlining closure %S failed" name))
               form)))))

      (t ;; Give up on inlining.
       form))))

;; ((lambda ...) ...)
(defun byte-compile-unfold-lambda (form &optional name)
  ;; In lexical-binding mode, let and functions don't bind vars in the same way
  ;; (let obey special-variable-p, but functions don't).  But luckily, this
  ;; doesn't matter here, because function's behavior is underspecified so it
  ;; can safely be turned into a `let', even though the reverse is not true.
  (or name (setq name "anonymous lambda"))
  (let ((lambda (car form))
	(values (cdr form)))
    (let ((arglist (nth 1 lambda))
	  (body (cdr (cdr lambda)))
	  optionalp restp
	  bindings)
      (if (and (stringp (car body)) (cdr body))
	  (setq body (cdr body)))
      (if (and (consp (car body)) (eq 'interactive (car (car body))))
	  (setq body (cdr body)))
      ;; FIXME: The checks below do not belong in an optimization phase.
      (while arglist
	(cond ((eq (car arglist) '&optional)
	       ;; ok, I'll let this slide because funcall_lambda() does...
	       ;; (if optionalp (error "multiple &optional keywords in %s" name))
	       (if restp (error "&optional found after &rest in %s" name))
	       (if (null (cdr arglist))
		   (error "nothing after &optional in %s" name))
	       (setq optionalp t))
	      ((eq (car arglist) '&rest)
	       ;; ...but it is by no stretch of the imagination a reasonable
	       ;; thing that funcall_lambda() allows (&rest x y) and
	       ;; (&rest x &optional y) in arglists.
	       (if (null (cdr arglist))
		   (error "nothing after &rest in %s" name))
	       (if (cdr (cdr arglist))
		   (error "multiple vars after &rest in %s" name))
	       (setq restp t))
	      (restp
	       (setq bindings (cons (list (car arglist)
					  (and values (cons 'list values)))
				    bindings)
		     values nil))
	      ((and (not optionalp) (null values))
	       (byte-compile-warn "attempt to open-code `%s' with too few arguments" name)
	       (setq arglist nil values 'too-few))
	      (t
	       (setq bindings (cons (list (car arglist) (car values))
				    bindings)
		     values (cdr values))))
	(setq arglist (cdr arglist)))
      (if values
	  (progn
	    (or (eq values 'too-few)
		(byte-compile-warn
		 "attempt to open-code `%s' with too many arguments" name))
	    form)

	;; The following leads to infinite recursion when loading a
	;; file containing `(defsubst f () (f))', and then trying to
	;; byte-compile that file.
	;(setq body (mapcar 'byte-optimize-form body)))

	(let ((newform
	       (if bindings
		   (cons 'let (cons (nreverse bindings) body))
		 (cons 'progn body))))
	  (byte-compile-log "  %s\t==>\t%s" form newform)
	  newform)))))


;;; implementing source-level optimizers

(defun byte-optimize-form-code-walker (form for-effect)
  ;;
  ;; For normal function calls, We can just mapcar the optimizer the cdr.  But
  ;; we need to have special knowledge of the syntax of the special forms
  ;; like let and defun (that's why they're special forms :-).  (Actually,
  ;; the important aspect is that they are subrs that don't evaluate all of
  ;; their args.)
  ;;
  (let ((fn (car-safe form))
	tmp)
    (cond ((not (consp form))
	   (if (not (and for-effect
			 (or byte-compile-delete-errors
			     (not (symbolp form))
			     (eq form t))))
	     form))
	  ((eq fn 'quote)
	   (if (cdr (cdr form))
	       (byte-compile-warn "malformed quote form: `%s'"
				  (prin1-to-string form)))
	   ;; map (quote nil) to nil to simplify optimizer logic.
	   ;; map quoted constants to nil if for-effect (just because).
	   (and (nth 1 form)
		(not for-effect)
		form))
	  ((eq 'lambda (car-safe fn))
	   (let ((newform (byte-compile-unfold-lambda form)))
	     (if (eq newform form)
		 ;; Some error occurred, avoid infinite recursion
		 form
	       (byte-optimize-form-code-walker newform for-effect))))
	  ((memq fn '(let let*))
	   ;; recursively enter the optimizer for the bindings and body
	   ;; of a let or let*.  This for depth-firstness: forms that
	   ;; are more deeply nested are optimized first.
	   (cons fn
	     (cons
	      (mapcar (lambda (binding)
			 (if (symbolp binding)
			     binding
			   (if (cdr (cdr binding))
			       (byte-compile-warn "malformed let binding: `%s'"
						  (prin1-to-string binding)))
			   (list (car binding)
				 (byte-optimize-form (nth 1 binding) nil))))
		      (nth 1 form))
	      (byte-optimize-body (cdr (cdr form)) for-effect))))
	  ((eq fn 'cond)
	   (cons fn
		 (mapcar (lambda (clause)
			    (if (consp clause)
				(cons
				 (byte-optimize-form (car clause) nil)
				 (byte-optimize-body (cdr clause) for-effect))
			      (byte-compile-warn "malformed cond form: `%s'"
						 (prin1-to-string clause))
			      clause))
			 (cdr form))))
	  ((eq fn 'progn)
	   ;; as an extra added bonus, this simplifies (progn <x>) --> <x>
	   (if (cdr (cdr form))
	       (progn
		 (setq tmp (byte-optimize-body (cdr form) for-effect))
		 (if (cdr tmp) (cons 'progn tmp) (car tmp)))
	     (byte-optimize-form (nth 1 form) for-effect)))
	  ((eq fn 'prog1)
	   (if (cdr (cdr form))
	       (cons 'prog1
		     (cons (byte-optimize-form (nth 1 form) for-effect)
			   (byte-optimize-body (cdr (cdr form)) t)))
	     (byte-optimize-form (nth 1 form) for-effect)))
	  ((eq fn 'prog2)
	   (cons 'prog2
	     (cons (byte-optimize-form (nth 1 form) t)
	       (cons (byte-optimize-form (nth 2 form) for-effect)
		     (byte-optimize-body (cdr (cdr (cdr form))) t)))))

	  ((memq fn '(save-excursion save-restriction save-current-buffer))
	   ;; those subrs which have an implicit progn; it's not quite good
	   ;; enough to treat these like normal function calls.
	   ;; This can turn (save-excursion ...) into (save-excursion) which
	   ;; will be optimized away in the lap-optimize pass.
	   (cons fn (byte-optimize-body (cdr form) for-effect)))

	  ((eq fn 'with-output-to-temp-buffer)
	   ;; this is just like the above, except for the first argument.
	   (cons fn
	     (cons
	      (byte-optimize-form (nth 1 form) nil)
	      (byte-optimize-body (cdr (cdr form)) for-effect))))

	  ((eq fn 'if)
	   (when (< (length form) 3)
	     (byte-compile-warn "too few arguments for `if'"))
	   (cons fn
	     (cons (byte-optimize-form (nth 1 form) nil)
	       (cons
		(byte-optimize-form (nth 2 form) for-effect)
		(byte-optimize-body (nthcdr 3 form) for-effect)))))

	  ((memq fn '(and or))  ; Remember, and/or are control structures.
	   ;; Take forms off the back until we can't any more.
	   ;; In the future it could conceivably be a problem that the
	   ;; subexpressions of these forms are optimized in the reverse
	   ;; order, but it's ok for now.
	   (if for-effect
	       (let ((backwards (reverse (cdr form))))
		 (while (and backwards
			     (null (setcar backwards
					   (byte-optimize-form (car backwards)
							       for-effect))))
		   (setq backwards (cdr backwards)))
		 (if (and (cdr form) (null backwards))
		     (byte-compile-log
		      "  all subforms of %s called for effect; deleted" form))
		 (and backwards
		      (cons fn (nreverse (mapcar 'byte-optimize-form
                                                 backwards)))))
	     (cons fn (mapcar 'byte-optimize-form (cdr form)))))

	  ((eq fn 'interactive)
	   (byte-compile-warn "misplaced interactive spec: `%s'"
			      (prin1-to-string form))
	   nil)

	  ((memq fn '(defun defmacro function condition-case))
	   ;; These forms are compiled as constants or by breaking out
	   ;; all the subexpressions and compiling them separately.
	   form)

	  ((eq fn 'unwind-protect)
	   ;; the "protected" part of an unwind-protect is compiled (and thus
	   ;; optimized) as a top-level form, so don't do it here.  But the
	   ;; non-protected part has the same for-effect status as the
	   ;; unwind-protect itself.  (The protected part is always for effect,
	   ;; but that isn't handled properly yet.)
	   (cons fn
		 (cons (byte-optimize-form (nth 1 form) for-effect)
		       (cdr (cdr form)))))

	  ((eq fn 'catch)
	   ;; the body of a catch is compiled (and thus optimized) as a
	   ;; top-level form, so don't do it here.  The tag is never
	   ;; for-effect.  The body should have the same for-effect status
	   ;; as the catch form itself, but that isn't handled properly yet.
	   (cons fn
		 (cons (byte-optimize-form (nth 1 form) nil)
		       (cdr (cdr form)))))

	  ((eq fn 'ignore)
	   ;; Don't treat the args to `ignore' as being
	   ;; computed for effect.  We want to avoid the warnings
	   ;; that might occur if they were treated that way.
	   ;; However, don't actually bother calling `ignore'.
	   `(prog1 nil . ,(mapcar 'byte-optimize-form (cdr form))))

          ;; Needed as long as we run byte-optimize-form after cconv.
          ((eq fn 'internal-make-closure) form)

          ((byte-code-function-p fn)
           (cons fn (mapcar #'byte-optimize-form (cdr form))))

	  ((not (symbolp fn))
	   (byte-compile-warn "`%s' is a malformed function"
			      (prin1-to-string fn))
	   form)

	  ((and for-effect (setq tmp (get fn 'side-effect-free))
		(or byte-compile-delete-errors
		    (eq tmp 'error-free)
		    ;; Detect the expansion of (pop foo).
		    ;; There is no need to compile the call to `car' there.
		    (and (eq fn 'car)
			 (eq (car-safe (cadr form)) 'prog1)
			 (let ((var (cadr (cadr form)))
			       (last (nth 2 (cadr form))))
			   (and (symbolp var)
				(null (nthcdr 3 (cadr form)))
				(eq (car-safe last) 'setq)
				(eq (cadr last) var)
				(eq (car-safe (nth 2 last)) 'cdr)
				(eq (cadr (nth 2 last)) var))))
		    (progn
		      (byte-compile-warn "value returned from %s is unused"
					 (prin1-to-string form))
		      nil)))
	   (byte-compile-log "  %s called for effect; deleted" fn)
	   ;; appending a nil here might not be necessary, but it can't hurt.
	   (byte-optimize-form
	    (cons 'progn (append (cdr form) '(nil))) t))

	  (t
	   ;; Otherwise, no args can be considered to be for-effect,
	   ;; even if the called function is for-effect, because we
	   ;; don't know anything about that function.
	   (let ((args (mapcar #'byte-optimize-form (cdr form))))
	     (if (and (get fn 'pure)
		      (byte-optimize-all-constp args))
		   (list 'quote (apply fn (mapcar #'eval args)))
	       (cons fn args)))))))

(defun byte-optimize-all-constp (list)
  "Non-nil if all elements of LIST satisfy `byte-compile-constp'."
  (let ((constant t))
    (while (and list constant)
      (unless (byte-compile-constp (car list))
	(setq constant nil))
      (setq list (cdr list)))
    constant))

(defun byte-optimize-form (form &optional for-effect)
  "The source-level pass of the optimizer."
  ;;
  ;; First, optimize all sub-forms of this one.
  (setq form (byte-optimize-form-code-walker form for-effect))
  ;;
  ;; after optimizing all subforms, optimize this form until it doesn't
  ;; optimize any further.  This means that some forms will be passed through
  ;; the optimizer many times, but that's necessary to make the for-effect
  ;; processing do as much as possible.
  ;;
  (let (opt new)
    (if (and (consp form)
	     (symbolp (car form))
	     (or (and for-effect
		      ;; we don't have any of these yet, but we might.
		      (setq opt (get (car form) 'byte-for-effect-optimizer)))
		 (setq opt (get (car form) 'byte-optimizer)))
	     (not (eq form (setq new (funcall opt form)))))
	(progn
;;	  (if (equal form new) (error "bogus optimizer -- %s" opt))
	  (byte-compile-log "  %s\t==>\t%s" form new)
	  (setq new (byte-optimize-form new for-effect))
	  new)
      form)))


(defun byte-optimize-body (forms all-for-effect)
  ;; Optimize the cdr of a progn or implicit progn; all forms is a list of
  ;; forms, all but the last of which are optimized with the assumption that
  ;; they are being called for effect.  the last is for-effect as well if
  ;; all-for-effect is true.  returns a new list of forms.
  (let ((rest forms)
	(result nil)
	fe new)
    (while rest
      (setq fe (or all-for-effect (cdr rest)))
      (setq new (and (car rest) (byte-optimize-form (car rest) fe)))
      (if (or new (not fe))
	  (setq result (cons new result)))
      (setq rest (cdr rest)))
    (nreverse result)))


;; some source-level optimizers
;;
;; when writing optimizers, be VERY careful that the optimizer returns
;; something not EQ to its argument if and ONLY if it has made a change.
;; This implies that you cannot simply destructively modify the list;
;; you must return something not EQ to it if you make an optimization.
;;
;; It is now safe to optimize code such that it introduces new bindings.

(defsubst byte-compile-trueconstp (form)
  "Return non-nil if FORM always evaluates to a non-nil value."
  (while (eq (car-safe form) 'progn)
    (setq form (car (last (cdr form)))))
  (cond ((consp form)
         (case (car form)
           (quote (cadr form))
           ;; Can't use recursion in a defsubst.
           ;; (progn (byte-compile-trueconstp (car (last (cdr form)))))
           ))
        ((not (symbolp form)))
        ((eq form t))
        ((keywordp form))))

(defsubst byte-compile-nilconstp (form)
  "Return non-nil if FORM always evaluates to a nil value."
  (while (eq (car-safe form) 'progn)
    (setq form (car (last (cdr form)))))
  (cond ((consp form)
         (case (car form)
           (quote (null (cadr form)))
           ;; Can't use recursion in a defsubst.
           ;; (progn (byte-compile-nilconstp (car (last (cdr form)))))
           ))
        ((not (symbolp form)) nil)
        ((null form))))

;; If the function is being called with constant numeric args,
;; evaluate as much as possible at compile-time.  This optimizer
;; assumes that the function is associative, like + or *.
(defun byte-optimize-associative-math (form)
  (let ((args nil)
	(constants nil)
	(rest (cdr form)))
    (while rest
      (if (numberp (car rest))
	  (setq constants (cons (car rest) constants))
	  (setq args (cons (car rest) args)))
      (setq rest (cdr rest)))
    (if (cdr constants)
	(if args
	    (list (car form)
		  (apply (car form) constants)
		  (if (cdr args)
		      (cons (car form) (nreverse args))
		      (car args)))
	    (apply (car form) constants))
	form)))

;; If the function is being called with constant numeric args,
;; evaluate as much as possible at compile-time.  This optimizer
;; assumes that the function satisfies
;;   (op x1 x2 ... xn) == (op ...(op (op x1 x2) x3) ...xn)
;; like - and /.
(defun byte-optimize-nonassociative-math (form)
  (if (or (not (numberp (car (cdr form))))
	  (not (numberp (car (cdr (cdr form))))))
      form
    (let ((constant (car (cdr form)))
	  (rest (cdr (cdr form))))
      (while (numberp (car rest))
	(setq constant (funcall (car form) constant (car rest))
	      rest (cdr rest)))
      (if rest
	  (cons (car form) (cons constant rest))
	  constant))))

;;(defun byte-optimize-associative-two-args-math (form)
;;  (setq form (byte-optimize-associative-math form))
;;  (if (consp form)
;;      (byte-optimize-two-args-left form)
;;      form))

;;(defun byte-optimize-nonassociative-two-args-math (form)
;;  (setq form (byte-optimize-nonassociative-math form))
;;  (if (consp form)
;;      (byte-optimize-two-args-right form)
;;      form))

(defun byte-optimize-approx-equal (x y)
  (<= (* (abs (- x y)) 100) (abs (+ x y))))

;; Collect all the constants from FORM, after the STARTth arg,
;; and apply FUN to them to make one argument at the end.
;; For functions that can handle floats, that optimization
;; can be incorrect because reordering can cause an overflow
;; that would otherwise be avoided by encountering an arg that is a float.
;; We avoid this problem by (1) not moving float constants and
;; (2) not moving anything if it would cause an overflow.
(defun byte-optimize-delay-constants-math (form start fun)
  ;; Merge all FORM's constants from number START, call FUN on them
  ;; and put the result at the end.
  (let ((rest (nthcdr (1- start) form))
	(orig form)
	;; t means we must check for overflow.
	(overflow (memq fun '(+ *))))
    (while (cdr (setq rest (cdr rest)))
      (if (integerp (car rest))
	  (let (constants)
	    (setq form (copy-sequence form)
		  rest (nthcdr (1- start) form))
	    (while (setq rest (cdr rest))
	      (cond ((integerp (car rest))
		     (setq constants (cons (car rest) constants))
		     (setcar rest nil))))
	    ;; If necessary, check now for overflow
	    ;; that might be caused by reordering.
	    (if (and overflow
		     ;; We have overflow if the result of doing the arithmetic
		     ;; on floats is not even close to the result
		     ;; of doing it on integers.
		     (not (byte-optimize-approx-equal
			    (apply fun (mapcar 'float constants))
			    (float (apply fun constants)))))
		(setq form orig)
	      (setq form (nconc (delq nil form)
				(list (apply fun (nreverse constants)))))))))
    form))

(defsubst byte-compile-butlast (form)
  (nreverse (cdr (reverse form))))

(defun byte-optimize-plus (form)
  ;; Don't call `byte-optimize-delay-constants-math' (bug#1334).
  ;;(setq form (byte-optimize-delay-constants-math form 1 '+))
  (if (memq 0 form) (setq form (delq 0 (copy-sequence form))))
  ;; For (+ constants...), byte-optimize-predicate does the work.
  (when (memq nil (mapcar 'numberp (cdr form)))
    (cond
     ;; (+ x 1) --> (1+ x) and (+ x -1) --> (1- x).
     ((and (= (length form) 3)
	   (or (memq (nth 1 form) '(1 -1))
	       (memq (nth 2 form) '(1 -1))))
      (let (integer other)
	(if (memq (nth 1 form) '(1 -1))
	    (setq integer (nth 1 form) other (nth 2 form))
	  (setq integer (nth 2 form) other (nth 1 form)))
	(setq form
	      (list (if (eq integer 1) '1+ '1-) other))))
     ;; Here, we could also do
     ;;  (+ x y ... 1) --> (1+ (+ x y ...))
     ;;  (+ x y ... -1) --> (1- (+ x y ...))
     ;; The resulting bytecode is smaller, but is it faster? -- cyd
     ))
  (byte-optimize-predicate form))

(defun byte-optimize-minus (form)
  ;; Don't call `byte-optimize-delay-constants-math' (bug#1334).
  ;;(setq form (byte-optimize-delay-constants-math form 2 '+))
  ;; Remove zeros.
  (when (and (nthcdr 3 form)
	     (memq 0 (cddr form)))
    (setq form (nconc (list (car form) (cadr form))
		      (delq 0 (copy-sequence (cddr form)))))
    ;; After the above, we must turn (- x) back into (- x 0)
    (or (cddr form)
	(setq form (nconc form (list 0)))))
  ;; For (- constants..), byte-optimize-predicate does the work.
  (when (memq nil (mapcar 'numberp (cdr form)))
    (cond
     ;; (- x 1) --> (1- x)
     ((equal (nthcdr 2 form) '(1))
      (setq form (list '1- (nth 1 form))))
     ;; (- x -1) --> (1+ x)
     ((equal (nthcdr 2 form) '(-1))
      (setq form (list '1+ (nth 1 form))))
     ;; (- 0 x) --> (- x)
     ((and (eq (nth 1 form) 0)
	   (= (length form) 3))
      (setq form (list '- (nth 2 form))))
     ;; Here, we could also do
     ;;  (- x y ... 1) --> (1- (- x y ...))
     ;;  (- x y ... -1) --> (1+ (- x y ...))
     ;; The resulting bytecode is smaller, but is it faster? -- cyd
     ))
  (byte-optimize-predicate form))

(defun byte-optimize-multiply (form)
  (setq form (byte-optimize-delay-constants-math form 1 '*))
  ;; For (* constants..), byte-optimize-predicate does the work.
  (when (memq nil (mapcar 'numberp (cdr form)))
    ;; After `byte-optimize-predicate', if there is a INTEGER constant
    ;; in FORM, it is in the last element.
    (let ((last (car (reverse (cdr form)))))
      (cond
       ;; Would handling (* ... 0) here cause floating point errors?
       ;; See bug#1334.
       ((eq 1 last) (setq form (byte-compile-butlast form)))
       ((eq -1 last)
	(setq form (list '- (if (nthcdr 3 form)
				(byte-compile-butlast form)
			      (nth 1 form))))))))
  (byte-optimize-predicate form))

(defun byte-optimize-divide (form)
  (setq form (byte-optimize-delay-constants-math form 2 '*))
  ;; After `byte-optimize-predicate', if there is a INTEGER constant
  ;; in FORM, it is in the last element.
  (let ((last (car (reverse (cdr (cdr form))))))
    (cond
     ;; Runtime error (leave it intact).
     ((or (null last)
	  (eq last 0)
	  (memql 0.0 (cddr form))))
     ;; No constants in expression
     ((not (numberp last)))
     ;; For (* constants..), byte-optimize-predicate does the work.
     ((null (memq nil (mapcar 'numberp (cdr form)))))
     ;; (/ x y.. 1) --> (/ x y..)
     ((and (eq last 1) (nthcdr 3 form))
      (setq form (byte-compile-butlast form)))
     ;; (/ x -1), (/ x .. -1)  --> (- x), (- (/ x ..))
     ((eq last -1)
      (setq form (list '- (if (nthcdr 3 form)
			      (byte-compile-butlast form)
			    (nth 1 form)))))))
  (byte-optimize-predicate form))

(defun byte-optimize-logmumble (form)
  (setq form (byte-optimize-delay-constants-math form 1 (car form)))
  (byte-optimize-predicate
   (cond ((memq 0 form)
	  (setq form (if (eq (car form) 'logand)
			 (cons 'progn (cdr form))
		       (delq 0 (copy-sequence form)))))
	 ((and (eq (car-safe form) 'logior)
	       (memq -1 form))
	  (cons 'progn (cdr form)))
	 (form))))


(defun byte-optimize-binary-predicate (form)
  (if (byte-compile-constp (nth 1 form))
      (if (byte-compile-constp (nth 2 form))
	  (condition-case ()
	      (list 'quote (eval form))
	    (error form))
	;; This can enable some lapcode optimizations.
	(list (car form) (nth 2 form) (nth 1 form)))
    form))

(defun byte-optimize-predicate (form)
  (let ((ok t)
	(rest (cdr form)))
    (while (and rest ok)
      (setq ok (byte-compile-constp (car rest))
	    rest (cdr rest)))
    (if ok
	(condition-case ()
	    (list 'quote (eval form))
	  (error form))
	form)))

(defun byte-optimize-identity (form)
  (if (and (cdr form) (null (cdr (cdr form))))
      (nth 1 form)
    (byte-compile-warn "identity called with %d arg%s, but requires 1"
		       (length (cdr form))
		       (if (= 1 (length (cdr form))) "" "s"))
    form))

(put 'identity 'byte-optimizer 'byte-optimize-identity)

(put '+   'byte-optimizer 'byte-optimize-plus)
(put '*   'byte-optimizer 'byte-optimize-multiply)
(put '-   'byte-optimizer 'byte-optimize-minus)
(put '/   'byte-optimizer 'byte-optimize-divide)
(put 'max 'byte-optimizer 'byte-optimize-associative-math)
(put 'min 'byte-optimizer 'byte-optimize-associative-math)

(put '=   'byte-optimizer 'byte-optimize-binary-predicate)
(put 'eq  'byte-optimizer 'byte-optimize-binary-predicate)
(put 'equal   'byte-optimizer 'byte-optimize-binary-predicate)
(put 'string= 'byte-optimizer 'byte-optimize-binary-predicate)
(put 'string-equal 'byte-optimizer 'byte-optimize-binary-predicate)

(put '<   'byte-optimizer 'byte-optimize-predicate)
(put '>   'byte-optimizer 'byte-optimize-predicate)
(put '<=  'byte-optimizer 'byte-optimize-predicate)
(put '>=  'byte-optimizer 'byte-optimize-predicate)
(put '1+  'byte-optimizer 'byte-optimize-predicate)
(put '1-  'byte-optimizer 'byte-optimize-predicate)
(put 'not 'byte-optimizer 'byte-optimize-predicate)
(put 'null  'byte-optimizer 'byte-optimize-predicate)
(put 'memq  'byte-optimizer 'byte-optimize-predicate)
(put 'consp 'byte-optimizer 'byte-optimize-predicate)
(put 'listp 'byte-optimizer 'byte-optimize-predicate)
(put 'symbolp 'byte-optimizer 'byte-optimize-predicate)
(put 'stringp 'byte-optimizer 'byte-optimize-predicate)
(put 'string< 'byte-optimizer 'byte-optimize-predicate)
(put 'string-lessp 'byte-optimizer 'byte-optimize-predicate)

(put 'logand 'byte-optimizer 'byte-optimize-logmumble)
(put 'logior 'byte-optimizer 'byte-optimize-logmumble)
(put 'logxor 'byte-optimizer 'byte-optimize-logmumble)
(put 'lognot 'byte-optimizer 'byte-optimize-predicate)

(put 'car 'byte-optimizer 'byte-optimize-predicate)
(put 'cdr 'byte-optimizer 'byte-optimize-predicate)
(put 'car-safe 'byte-optimizer 'byte-optimize-predicate)
(put 'cdr-safe 'byte-optimizer 'byte-optimize-predicate)


;; I'm not convinced that this is necessary.  Doesn't the optimizer loop
;; take care of this? - Jamie
;; I think this may some times be necessary to reduce ie (quote 5) to 5,
;; so arithmetic optimizers recognize the numeric constant.  - Hallvard
(put 'quote 'byte-optimizer 'byte-optimize-quote)
(defun byte-optimize-quote (form)
  (if (or (consp (nth 1 form))
	  (and (symbolp (nth 1 form))
	       (not (byte-compile-const-symbol-p form))))
      form
    (nth 1 form)))

(defun byte-optimize-zerop (form)
  (cond ((numberp (nth 1 form))
	 (eval form))
	(byte-compile-delete-errors
	 (list '= (nth 1 form) 0))
	(form)))

(put 'zerop 'byte-optimizer 'byte-optimize-zerop)

(defun byte-optimize-and (form)
  ;; Simplify if less than 2 args.
  ;; if there is a literal nil in the args to `and', throw it and following
  ;; forms away, and surround the `and' with (progn ... nil).
  (cond ((null (cdr form)))
	((memq nil form)
	 (list 'progn
	       (byte-optimize-and
		(prog1 (setq form (copy-sequence form))
		  (while (nth 1 form)
		    (setq form (cdr form)))
		  (setcdr form nil)))
	       nil))
	((null (cdr (cdr form)))
	 (nth 1 form))
	((byte-optimize-predicate form))))

(defun byte-optimize-or (form)
  ;; Throw away nil's, and simplify if less than 2 args.
  ;; If there is a literal non-nil constant in the args to `or', throw away all
  ;; following forms.
  (if (memq nil form)
      (setq form (delq nil (copy-sequence form))))
  (let ((rest form))
    (while (cdr (setq rest (cdr rest)))
      (if (byte-compile-trueconstp (car rest))
	  (setq form (copy-sequence form)
		rest (setcdr (memq (car rest) form) nil))))
    (if (cdr (cdr form))
	(byte-optimize-predicate form)
      (nth 1 form))))

(defun byte-optimize-cond (form)
  ;; if any clauses have a literal nil as their test, throw them away.
  ;; if any clause has a literal non-nil constant as its test, throw
  ;; away all following clauses.
  (let (rest)
    ;; This must be first, to reduce (cond (t ...) (nil)) to (progn t ...)
    (while (setq rest (assq nil (cdr form)))
      (setq form (delq rest (copy-sequence form))))
    (if (memq nil (cdr form))
	(setq form (delq nil (copy-sequence form))))
    (setq rest form)
    (while (setq rest (cdr rest))
      (cond ((byte-compile-trueconstp (car-safe (car rest)))
             ;; This branch will always be taken: kill the subsequent ones.
	     (cond ((eq rest (cdr form)) ;First branch of `cond'.
		    (setq form `(progn ,@(car rest))))
		   ((cdr rest)
		    (setq form (copy-sequence form))
		    (setcdr (memq (car rest) form) nil)))
	     (setq rest nil))
            ((and (consp (car rest))
                  (byte-compile-nilconstp (caar rest)))
             ;; This branch will never be taken: kill its body.
             (setcdr (car rest) nil)))))
  ;;
  ;; Turn (cond (( <x> )) ... ) into (or <x> (cond ... ))
  (if (eq 'cond (car-safe form))
      (let ((clauses (cdr form)))
	(if (and (consp (car clauses))
		 (null (cdr (car clauses))))
	    (list 'or (car (car clauses))
		  (byte-optimize-cond
		   (cons (car form) (cdr (cdr form)))))
	  form))
    form))

(defun byte-optimize-if (form)
  ;; (if (progn <insts> <test>) <rest>) ==> (progn <insts> (if <test> <rest>))
  ;; (if <true-constant> <then> <else...>) ==> <then>
  ;; (if <false-constant> <then> <else...>) ==> (progn <else...>)
  ;; (if <test> nil <else...>) ==> (if (not <test>) (progn <else...>))
  ;; (if <test> <then> nil) ==> (if <test> <then>)
  (let ((clause (nth 1 form)))
    (cond ((and (eq (car-safe clause) 'progn)
                ;; `clause' is a proper list.
                (null (cdr (last clause))))
           (if (null (cddr clause))
               ;; A trivial `progn'.
               (byte-optimize-if `(if ,(cadr clause) ,@(nthcdr 2 form)))
             (nconc (butlast clause)
                    (list
                     (byte-optimize-if
                      `(if ,(car (last clause)) ,@(nthcdr 2 form)))))))
          ((byte-compile-trueconstp clause)
	   `(progn ,clause ,(nth 2 form)))
	  ((byte-compile-nilconstp clause)
           `(progn ,clause ,@(nthcdr 3 form)))
	  ((nth 2 form)
	   (if (equal '(nil) (nthcdr 3 form))
	       (list 'if clause (nth 2 form))
	     form))
	  ((or (nth 3 form) (nthcdr 4 form))
	   (list 'if
		 ;; Don't make a double negative;
		 ;; instead, take away the one that is there.
		 (if (and (consp clause) (memq (car clause) '(not null))
			  (= (length clause) 2)) ; (not xxxx) or (not (xxxx))
		     (nth 1 clause)
		   (list 'not clause))
		 (if (nthcdr 4 form)
		     (cons 'progn (nthcdr 3 form))
		   (nth 3 form))))
	  (t
	   (list 'progn clause nil)))))

(defun byte-optimize-while (form)
  (when (< (length form) 2)
    (byte-compile-warn "too few arguments for `while'"))
  (if (nth 1 form)
      form))

(put 'and   'byte-optimizer 'byte-optimize-and)
(put 'or    'byte-optimizer 'byte-optimize-or)
(put 'cond  'byte-optimizer 'byte-optimize-cond)
(put 'if    'byte-optimizer 'byte-optimize-if)
(put 'while 'byte-optimizer 'byte-optimize-while)

;; byte-compile-negation-optimizer lives in bytecomp.el
(put '/= 'byte-optimizer 'byte-compile-negation-optimizer)
(put 'atom 'byte-optimizer 'byte-compile-negation-optimizer)
(put 'nlistp 'byte-optimizer 'byte-compile-negation-optimizer)


(defun byte-optimize-funcall (form)
  ;; (funcall (lambda ...) ...) ==> ((lambda ...) ...)
  ;; (funcall foo ...) ==> (foo ...)
  (let ((fn (nth 1 form)))
    (if (memq (car-safe fn) '(quote function))
	(cons (nth 1 fn) (cdr (cdr form)))
      form)))

(defun byte-optimize-apply (form)
  ;; If the last arg is a literal constant, turn this into a funcall.
  ;; The funcall optimizer can then transform (funcall 'foo ...) -> (foo ...).
  (let ((fn (nth 1 form))
	(last (nth (1- (length form)) form))) ; I think this really is fastest
    (or (if (or (null last)
		(eq (car-safe last) 'quote))
	    (if (listp (nth 1 last))
		(let ((butlast (nreverse (cdr (reverse (cdr (cdr form)))))))
		  (nconc (list 'funcall fn) butlast
			 (mapcar (lambda (x) (list 'quote x)) (nth 1 last))))
	      (byte-compile-warn
	       "last arg to apply can't be a literal atom: `%s'"
	       (prin1-to-string last))
	      nil))
	form)))

(put 'funcall 'byte-optimizer 'byte-optimize-funcall)
(put 'apply   'byte-optimizer 'byte-optimize-apply)


(put 'let 'byte-optimizer 'byte-optimize-letX)
(put 'let* 'byte-optimizer 'byte-optimize-letX)
(defun byte-optimize-letX (form)
  (cond ((null (nth 1 form))
	 ;; No bindings
	 (cons 'progn (cdr (cdr form))))
	((or (nth 2 form) (nthcdr 3 form))
	 form)
	 ;; The body is nil
	((eq (car form) 'let)
	 (append '(progn) (mapcar 'car-safe (mapcar 'cdr-safe (nth 1 form)))
		 '(nil)))
	(t
	 (let ((binds (reverse (nth 1 form))))
	   (list 'let* (reverse (cdr binds)) (nth 1 (car binds)) nil)))))


(put 'nth 'byte-optimizer 'byte-optimize-nth)
(defun byte-optimize-nth (form)
  (if (= (safe-length form) 3)
      (if (memq (nth 1 form) '(0 1))
	  (list 'car (if (zerop (nth 1 form))
			 (nth 2 form)
		       (list 'cdr (nth 2 form))))
	(byte-optimize-predicate form))
    form))

(put 'nthcdr 'byte-optimizer 'byte-optimize-nthcdr)
(defun byte-optimize-nthcdr (form)
  (if (= (safe-length form) 3)
      (if (memq (nth 1 form) '(0 1 2))
	  (let ((count (nth 1 form)))
	    (setq form (nth 2 form))
	    (while (>= (setq count (1- count)) 0)
	      (setq form (list 'cdr form)))
	    form)
	(byte-optimize-predicate form))
    form))

;; Fixme: delete-char -> delete-region (byte-coded)
;; optimize string-as-unibyte, string-as-multibyte, string-make-unibyte,
;; string-make-multibyte for constant args.

(put 'featurep 'byte-optimizer 'byte-optimize-featurep)
(defun byte-optimize-featurep (form)
  ;; Emacs-21's byte-code doesn't run under XEmacs or SXEmacs anyway, so we
  ;; can safely optimize away this test.
  (if (member (cdr-safe form) '(((quote xemacs)) ((quote sxemacs))))
      nil
    (if (member (cdr-safe form) '(((quote emacs))))
	t
      form)))

(put 'set 'byte-optimizer 'byte-optimize-set)
(defun byte-optimize-set (form)
  (let ((var (car-safe (cdr-safe form))))
    (cond
     ((and (eq (car-safe var) 'quote) (consp (cdr var)))
      `(setq ,(cadr var) ,@(cddr form)))
     ((and (eq (car-safe var) 'make-local-variable)
	   (eq (car-safe (setq var (car-safe (cdr var)))) 'quote)
	   (consp (cdr var)))
      `(progn ,(cadr form) (setq ,(cadr var) ,@(cddr form))))
     (t form))))

;; enumerating those functions which need not be called if the returned
;; value is not used.  That is, something like
;;    (progn (list (something-with-side-effects) (yow))
;;           (foo))
;; may safely be turned into
;;    (progn (progn (something-with-side-effects) (yow))
;;           (foo))
;; Further optimizations will turn (progn (list 1 2 3) 'foo) into 'foo.

;; Some of these functions have the side effect of allocating memory
;; and it would be incorrect to replace two calls with one.
;; But we don't try to do those kinds of optimizations,
;; so it is safe to list such functions here.
;; Some of these functions return values that depend on environment
;; state, so that constant folding them would be wrong,
;; but we don't do constant folding based on this list.

;; However, at present the only optimization we normally do
;; is delete calls that need not occur, and we only do that
;; with the error-free functions.

;; I wonder if I missed any :-\)
(let ((side-effect-free-fns
       '(% * + - / /= 1+ 1- < <= = > >= abs acos append aref ash asin atan
	 assoc assq
	 boundp buffer-file-name buffer-local-variables buffer-modified-p
	 buffer-substring byte-code-function-p
	 capitalize car-less-than-car car cdr ceiling char-after char-before
	 char-equal char-to-string char-width
	 compare-strings concat coordinates-in-window-p
	 copy-alist copy-sequence copy-marker cos count-lines
	 decode-char
	 decode-time default-boundp default-value documentation downcase
	 elt encode-char exp expt encode-time error-message-string
	 fboundp fceiling featurep ffloor
	 file-directory-p file-exists-p file-locked-p file-name-absolute-p
	 file-newer-than-file-p file-readable-p file-symlink-p file-writable-p
	 float float-time floor format format-time-string frame-visible-p
	 fround ftruncate
	 get gethash get-buffer get-buffer-window getenv get-file-buffer
	 hash-table-count
	 int-to-string intern-soft
	 keymap-parent
	 length local-variable-if-set-p local-variable-p log log10 logand
	 logb logior lognot logxor lsh langinfo
	 make-list make-string make-symbol
	 marker-buffer max member memq min mod multibyte-char-to-unibyte
	 next-window nth nthcdr number-to-string
	 parse-colon-path plist-get plist-member
	 prefix-numeric-value previous-window prin1-to-string propertize
	 degrees-to-radians
	 radians-to-degrees rassq rassoc read-from-string regexp-quote
	 region-beginning region-end reverse round
	 sin sqrt string string< string= string-equal string-lessp string-to-char
	 string-to-int string-to-number substring sxhash symbol-function
	 symbol-name symbol-plist symbol-value string-make-unibyte
	 string-make-multibyte string-as-multibyte string-as-unibyte
	 string-to-multibyte
	 tan truncate
	 unibyte-char-to-multibyte upcase user-full-name
	 user-login-name user-original-login-name user-variable-p
	 vconcat
	 window-buffer window-dedicated-p window-edges window-height
	 window-hscroll window-minibuffer-p window-width
	 zerop))
      (side-effect-and-error-free-fns
       '(arrayp atom
	 bobp bolp bool-vector-p
	 buffer-end buffer-list buffer-size buffer-string bufferp
	 car-safe case-table-p cdr-safe char-or-string-p characterp
	 charsetp commandp cons consp
	 current-buffer current-global-map current-indentation
	 current-local-map current-minor-mode-maps current-time
	 current-time-string current-time-zone
	 eobp eolp eq equal eventp
	 floatp following-char framep
	 get-largest-window get-lru-window
	 hash-table-p
	 identity ignore integerp integer-or-marker-p interactive-p
	 invocation-directory invocation-name
	 keymapp
	 line-beginning-position line-end-position list listp
	 make-marker mark mark-marker markerp max-char
	 memory-limit minibuffer-window
	 mouse-movement-p
	 natnump nlistp not null number-or-marker-p numberp
	 one-window-p overlayp
	 point point-marker point-min point-max preceding-char primary-charset
	 processp
	 recent-keys recursion-depth
	 safe-length selected-frame selected-window sequencep
	 standard-case-table standard-syntax-table stringp subrp symbolp
	 syntax-table syntax-table-p
	 this-command-keys this-command-keys-vector this-single-command-keys
	 this-single-command-raw-keys
	 user-real-login-name user-real-uid user-uid
	 vector vectorp visible-frame-list
	 wholenump window-configuration-p window-live-p windowp)))
  (while side-effect-free-fns
    (put (car side-effect-free-fns) 'side-effect-free t)
    (setq side-effect-free-fns (cdr side-effect-free-fns)))
  (while side-effect-and-error-free-fns
    (put (car side-effect-and-error-free-fns) 'side-effect-free 'error-free)
    (setq side-effect-and-error-free-fns (cdr side-effect-and-error-free-fns)))
  nil)


;; pure functions are side-effect free functions whose values depend
;; only on their arguments. For these functions, calls with constant
;; arguments can be evaluated at compile time. This may shift run time
;; errors to compile time.

(let ((pure-fns
       '(concat symbol-name regexp-opt regexp-quote string-to-syntax)))
  (while pure-fns
    (put (car pure-fns) 'pure t)
    (setq pure-fns (cdr pure-fns)))
  nil)

(defconst byte-constref-ops
  '(byte-constant byte-constant2 byte-varref byte-varset byte-varbind))

;; Used and set dynamically in byte-decompile-bytecode-1.
(defvar bytedecomp-op)
(defvar bytedecomp-ptr)

;; This function extracts the bitfields from variable-length opcodes.
;; Originally defined in disass.el (which no longer uses it.)
(defun disassemble-offset (bytes)
  "Don't call this!"
  ;; Fetch and return the offset for the current opcode.
  ;; Return nil if this opcode has no offset.
  (cond ((< bytedecomp-op byte-nth)
	 (let ((tem (logand bytedecomp-op 7)))
	   (setq bytedecomp-op (logand bytedecomp-op 248))
	   (cond ((eq tem 6)
		  ;; Offset in next byte.
		  (setq bytedecomp-ptr (1+ bytedecomp-ptr))
		  (aref bytes bytedecomp-ptr))
		 ((eq tem 7)
		  ;; Offset in next 2 bytes.
		  (setq bytedecomp-ptr (1+ bytedecomp-ptr))
		  (+ (aref bytes bytedecomp-ptr)
		     (progn (setq bytedecomp-ptr (1+ bytedecomp-ptr))
			    (lsh (aref bytes bytedecomp-ptr) 8))))
		 (t tem))))		;Offset was in opcode.
	((>= bytedecomp-op byte-constant)
	 (prog1 (- bytedecomp-op byte-constant)	;Offset in opcode.
	   (setq bytedecomp-op byte-constant)))
	((or (and (>= bytedecomp-op byte-constant2)
                  (<= bytedecomp-op byte-goto-if-not-nil-else-pop))
             (= bytedecomp-op byte-stack-set2))
	 ;; Offset in next 2 bytes.
	 (setq bytedecomp-ptr (1+ bytedecomp-ptr))
	 (+ (aref bytes bytedecomp-ptr)
	    (progn (setq bytedecomp-ptr (1+ bytedecomp-ptr))
		   (lsh (aref bytes bytedecomp-ptr) 8))))
	((and (>= bytedecomp-op byte-listN)
	      (<= bytedecomp-op byte-discardN))
	 (setq bytedecomp-ptr (1+ bytedecomp-ptr)) ;Offset in next byte.
	 (aref bytes bytedecomp-ptr))))

(defvar byte-compile-tag-number)

;; This de-compiler is used for inline expansion of compiled functions,
;; and by the disassembler.
;;
;; This list contains numbers, which are pc values,
;; before each instruction.
(defun byte-decompile-bytecode (bytes constvec)
  "Turn BYTECODE into lapcode, referring to CONSTVEC."
  (let ((byte-compile-constants nil)
	(byte-compile-variables nil)
	(byte-compile-tag-number 0))
    (byte-decompile-bytecode-1 bytes constvec)))

;; As byte-decompile-bytecode, but updates
;; byte-compile-{constants, variables, tag-number}.
;; If MAKE-SPLICEABLE is true, then `return' opcodes are replaced
;; with `goto's destined for the end of the code.
;; That is for use by the compiler.
;; If MAKE-SPLICEABLE is nil, we are being called for the disassembler.
;; In that case, we put a pc value into the list
;; before each insn (or its label).
(defun byte-decompile-bytecode-1 (bytes constvec &optional make-spliceable)
  (let ((length (length bytes))
        (bytedecomp-ptr 0) optr tags bytedecomp-op offset
	lap tmp)
    (while (not (= bytedecomp-ptr length))
      (or make-spliceable
	  (push bytedecomp-ptr lap))
      (setq bytedecomp-op (aref bytes bytedecomp-ptr)
	    optr bytedecomp-ptr
            ;; This uses dynamic-scope magic.
            offset (disassemble-offset bytes))
      (let ((opcode (aref byte-code-vector bytedecomp-op)))
	(assert opcode)
	(setq bytedecomp-op opcode))
      (cond ((memq bytedecomp-op byte-goto-ops)
	     ;; It's a pc.
	     (setq offset
		   (cdr (or (assq offset tags)
                            (let ((new (cons offset (byte-compile-make-tag))))
                              (push new tags)
                              new)))))
	    ((cond ((eq bytedecomp-op 'byte-constant2)
		    (setq bytedecomp-op 'byte-constant) t)
		   ((memq bytedecomp-op byte-constref-ops)))
	     (setq tmp (if (>= offset (length constvec))
			   (list 'out-of-range offset)
			 (aref constvec offset))
		   offset (if (eq bytedecomp-op 'byte-constant)
			      (byte-compile-get-constant tmp)
			    (or (assq tmp byte-compile-variables)
                                (let ((new (list tmp)))
                                  (push new byte-compile-variables)
                                  new)))))
	    ((eq bytedecomp-op 'byte-stack-set2)
	     (setq bytedecomp-op 'byte-stack-set))
	    ((and (eq bytedecomp-op 'byte-discardN) (>= offset #x80))
	     ;; The top bit of the operand for byte-discardN is a flag,
	     ;; saying whether the top-of-stack is preserved.  In
	     ;; lapcode, we represent this by using a different opcode
	     ;; (with the flag removed from the operand).
	     (setq bytedecomp-op 'byte-discardN-preserve-tos)
	     (setq offset (- offset #x80))))
      ;; lap = ( [ (pc . (op . arg)) ]* )
      (push (cons optr (cons bytedecomp-op (or offset 0)))
            lap)
      (setq bytedecomp-ptr (1+ bytedecomp-ptr)))
    (let ((rest lap))
      (while rest
	(cond ((numberp (car rest)))
	      ((setq tmp (assq (car (car rest)) tags))
	       ;; This addr is jumped to.
	       (setcdr rest (cons (cons nil (cdr tmp))
				  (cdr rest)))
	       (setq tags (delq tmp tags))
	       (setq rest (cdr rest))))
	(setq rest (cdr rest))))
    (if tags (error "optimizer error: missed tags %s" tags))
    ;; Remove addrs, lap = ( [ (op . arg) | (TAG tagno) ]* )
    (mapcar (function (lambda (elt)
			(if (numberp elt)
			    elt
			  (cdr elt))))
	    (nreverse lap))))


;;; peephole optimizer

(defconst byte-tagref-ops (cons 'TAG byte-goto-ops))

(defconst byte-conditional-ops
  '(byte-goto-if-nil byte-goto-if-not-nil byte-goto-if-nil-else-pop
    byte-goto-if-not-nil-else-pop))

(defconst byte-after-unbind-ops
   '(byte-constant byte-dup
     byte-symbolp byte-consp byte-stringp byte-listp byte-numberp byte-integerp
     byte-eq byte-not
     byte-cons byte-list1 byte-list2	; byte-list3 byte-list4
     byte-interactive-p)
   ;; How about other side-effect-free-ops?  Is it safe to move an
   ;; error invocation (such as from nth) out of an unwind-protect?
   ;; No, it is not, because the unwind-protect forms can alter
   ;; the inside of the object to which nth would apply.
   ;; For the same reason, byte-equal was deleted from this list.
   "Byte-codes that can be moved past an unbind.")

(defconst byte-compile-side-effect-and-error-free-ops
  '(byte-constant byte-dup byte-symbolp byte-consp byte-stringp byte-listp
    byte-integerp byte-numberp byte-eq byte-equal byte-not byte-car-safe
    byte-cdr-safe byte-cons byte-list1 byte-list2 byte-point byte-point-max
    byte-point-min byte-following-char byte-preceding-char
    byte-current-column byte-eolp byte-eobp byte-bolp byte-bobp
    byte-current-buffer byte-stack-ref))

(defconst byte-compile-side-effect-free-ops
  (nconc
   '(byte-varref byte-nth byte-memq byte-car byte-cdr byte-length byte-aref
     byte-symbol-value byte-get byte-concat2 byte-concat3 byte-sub1 byte-add1
     byte-eqlsign byte-gtr byte-lss byte-leq byte-geq byte-diff byte-negate
     byte-plus byte-max byte-min byte-mult byte-char-after byte-char-syntax
     byte-buffer-substring byte-string= byte-string< byte-nthcdr byte-elt
     byte-member byte-assq byte-quo byte-rem)
   byte-compile-side-effect-and-error-free-ops))

;; This crock is because of the way DEFVAR_BOOL variables work.
;; Consider the code
;;
;;	(defun foo (flag)
;;	  (let ((old-pop-ups pop-up-windows)
;;		(pop-up-windows flag))
;;	    (cond ((not (eq pop-up-windows old-pop-ups))
;;		   (setq old-pop-ups pop-up-windows)
;;		   ...))))
;;
;; Uncompiled, old-pop-ups will always be set to nil or t, even if FLAG is
;; something else.  But if we optimize
;;
;;	varref flag
;;	varbind pop-up-windows
;;	varref pop-up-windows
;;	not
;; to
;;	varref flag
;;	dup
;;	varbind pop-up-windows
;;	not
;;
;; we break the program, because it will appear that pop-up-windows and
;; old-pop-ups are not EQ when really they are.  So we have to know what
;; the BOOL variables are, and not perform this optimization on them.

;; The variable `byte-boolean-vars' is now primitive and updated
;; automatically by DEFVAR_BOOL.

(defun byte-optimize-lapcode (lap &optional _for-effect)
  "Simple peephole optimizer.  LAP is both modified and returned.
If FOR-EFFECT is non-nil, the return value is assumed to be of no importance."
  (let (lap0
	lap1
	lap2
	(keep-going 'first-time)
	(add-depth 0)
	rest tmp tmp2 tmp3
	(side-effect-free (if byte-compile-delete-errors
			      byte-compile-side-effect-free-ops
			    byte-compile-side-effect-and-error-free-ops)))
    (while keep-going
      (or (eq keep-going 'first-time)
	  (byte-compile-log-lap "  ---- next pass"))
      (setq rest lap
	    keep-going nil)
      (while rest
	(setq lap0 (car rest)
	      lap1 (nth 1 rest)
	      lap2 (nth 2 rest))

	;; You may notice that sequences like "dup varset discard" are
	;; optimized but sequences like "dup varset TAG1: discard" are not.
	;; You may be tempted to change this; resist that temptation.
	(cond ;;
	      ;; <side-effect-free> pop -->  <deleted>
	      ;;  ...including:
	      ;; const-X pop   -->  <deleted>
	      ;; varref-X pop  -->  <deleted>
	      ;; dup pop       -->  <deleted>
	      ;;
	      ((and (eq 'byte-discard (car lap1))
		    (memq (car lap0) side-effect-free))
	       (setq keep-going t)
	       (setq tmp (aref byte-stack+-info (symbol-value (car lap0))))
	       (setq rest (cdr rest))
	       (cond ((= tmp 1)
		      (byte-compile-log-lap
 		       "  %s discard\t-->\t<deleted>" lap0)
		      (setq lap (delq lap0 (delq lap1 lap))))
		     ((= tmp 0)
		      (byte-compile-log-lap
		       "  %s discard\t-->\t<deleted> discard" lap0)
		      (setq lap (delq lap0 lap)))
		     ((= tmp -1)
		      (byte-compile-log-lap
		       "  %s discard\t-->\tdiscard discard" lap0)
		      (setcar lap0 'byte-discard)
		      (setcdr lap0 0))
		     ((error "Optimizer error: too much on the stack"))))
	      ;;
	      ;; goto*-X X:  -->  X:
	      ;;
	      ((and (memq (car lap0) byte-goto-ops)
		    (eq (cdr lap0) lap1))
	       (cond ((eq (car lap0) 'byte-goto)
		      (setq lap (delq lap0 lap))
		      (setq tmp "<deleted>"))
		     ((memq (car lap0) byte-goto-always-pop-ops)
		      (setcar lap0 (setq tmp 'byte-discard))
		      (setcdr lap0 0))
		     ((error "Depth conflict at tag %d" (nth 2 lap0))))
	       (and (memq byte-optimize-log '(t byte))
		    (byte-compile-log "  (goto %s) %s:\t-->\t%s %s:"
				      (nth 1 lap1) (nth 1 lap1)
				      tmp (nth 1 lap1)))
	       (setq keep-going t))
	      ;;
	      ;; varset-X varref-X  -->  dup varset-X
	      ;; varbind-X varref-X  -->  dup varbind-X
	      ;; const/dup varset-X varref-X --> const/dup varset-X const/dup
	      ;; const/dup varbind-X varref-X --> const/dup varbind-X const/dup
	      ;; The latter two can enable other optimizations.
	      ;;
              ;; For lexical variables, we could do the same
              ;;   stack-set-X+1 stack-ref-X  -->  dup stack-set-X+2
              ;; but this is a very minor gain, since dup is stack-ref-0,
              ;; i.e. it's only better if X>5, and even then it comes
              ;; at the cost of an extra stack slot.  Let's not bother.
	      ((and (eq 'byte-varref (car lap2))
                    (eq (cdr lap1) (cdr lap2))
                    (memq (car lap1) '(byte-varset byte-varbind)))
	       (if (and (setq tmp (memq (car (cdr lap2)) byte-boolean-vars))
			(not (eq (car lap0) 'byte-constant)))
		   nil
		 (setq keep-going t)
		 (if (memq (car lap0) '(byte-constant byte-dup))
		     (progn
		       (setq tmp (if (or (not tmp)
					 (byte-compile-const-symbol-p
					  (car (cdr lap0))))
				     (cdr lap0)
				   (byte-compile-get-constant t)))
		       (byte-compile-log-lap "  %s %s %s\t-->\t%s %s %s"
					     lap0 lap1 lap2 lap0 lap1
					     (cons (car lap0) tmp))
		       (setcar lap2 (car lap0))
		       (setcdr lap2 tmp))
		   (byte-compile-log-lap "  %s %s\t-->\tdup %s" lap1 lap2 lap1)
		   (setcar lap2 (car lap1))
		   (setcar lap1 'byte-dup)
		   (setcdr lap1 0)
		   ;; The stack depth gets locally increased, so we will
		   ;; increase maxdepth in case depth = maxdepth here.
		   ;; This can cause the third argument to byte-code to
		   ;; be larger than necessary.
		   (setq add-depth 1))))
	      ;;
	      ;; dup varset-X discard  -->  varset-X
	      ;; dup varbind-X discard  -->  varbind-X
              ;; dup stack-set-X discard  -->  stack-set-X-1
	      ;; (the varbind variant can emerge from other optimizations)
	      ;;
	      ((and (eq 'byte-dup (car lap0))
		    (eq 'byte-discard (car lap2))
		    (memq (car lap1) '(byte-varset byte-varbind
                                       byte-stack-set)))
	       (byte-compile-log-lap "  dup %s discard\t-->\t%s" lap1 lap1)
	       (setq keep-going t
		     rest (cdr rest))
               (if (eq 'byte-stack-set (car lap1)) (decf (cdr lap1)))
	       (setq lap (delq lap0 (delq lap2 lap))))
	      ;;
	      ;; not goto-X-if-nil              -->  goto-X-if-non-nil
	      ;; not goto-X-if-non-nil          -->  goto-X-if-nil
	      ;;
	      ;; it is wrong to do the same thing for the -else-pop variants.
	      ;;
	      ((and (eq 'byte-not (car lap0))
		    (memq (car lap1) '(byte-goto-if-nil byte-goto-if-not-nil)))
	       (byte-compile-log-lap "  not %s\t-->\t%s"
				     lap1
				     (cons
				      (if (eq (car lap1) 'byte-goto-if-nil)
					  'byte-goto-if-not-nil
					'byte-goto-if-nil)
				      (cdr lap1)))
	       (setcar lap1 (if (eq (car lap1) 'byte-goto-if-nil)
				'byte-goto-if-not-nil
				'byte-goto-if-nil))
	       (setq lap (delq lap0 lap))
	       (setq keep-going t))
	      ;;
	      ;; goto-X-if-nil     goto-Y X:  -->  goto-Y-if-non-nil X:
	      ;; goto-X-if-non-nil goto-Y X:  -->  goto-Y-if-nil     X:
	      ;;
	      ;; it is wrong to do the same thing for the -else-pop variants.
	      ;;
	      ((and (memq (car lap0)
                          '(byte-goto-if-nil byte-goto-if-not-nil))	; gotoX
		    (eq 'byte-goto (car lap1))			; gotoY
		    (eq (cdr lap0) lap2))			; TAG X
	       (let ((inverse (if (eq 'byte-goto-if-nil (car lap0))
				  'byte-goto-if-not-nil 'byte-goto-if-nil)))
		 (byte-compile-log-lap "  %s %s %s:\t-->\t%s %s:"
				       lap0 lap1 lap2
				       (cons inverse (cdr lap1)) lap2)
		 (setq lap (delq lap0 lap))
		 (setcar lap1 inverse)
		 (setq keep-going t)))
	      ;;
	      ;; const goto-if-* --> whatever
	      ;;
	      ((and (eq 'byte-constant (car lap0))
		    (memq (car lap1) byte-conditional-ops)
                    ;; If the `byte-constant's cdr is not a cons cell, it has
                    ;; to be an index into the constant pool); even though
                    ;; it'll be a constant, that constant is not known yet
                    ;; (it's typically a free variable of a closure, so will
                    ;; only be known when the closure will be built at
                    ;; run-time).
                    (consp (cdr lap0)))
	       (cond ((if (memq (car lap1) '(byte-goto-if-nil
                                             byte-goto-if-nil-else-pop))
                          (car (cdr lap0))
                        (not (car (cdr lap0))))
		      (byte-compile-log-lap "  %s %s\t-->\t<deleted>"
					    lap0 lap1)
		      (setq rest (cdr rest)
			    lap (delq lap0 (delq lap1 lap))))
		     (t
		      (byte-compile-log-lap "  %s %s\t-->\t%s"
					    lap0 lap1
					    (cons 'byte-goto (cdr lap1)))
		      (when (memq (car lap1) byte-goto-always-pop-ops)
			(setq lap (delq lap0 lap)))
		      (setcar lap1 'byte-goto)))
               (setq keep-going t))
	      ;;
	      ;; varref-X varref-X  -->  varref-X dup
	      ;; varref-X [dup ...] varref-X  -->  varref-X [dup ...] dup
	      ;; stackref-X [dup ...] stackref-X+N --> stackref-X [dup ...] dup
	      ;; We don't optimize the const-X variations on this here,
	      ;; because that would inhibit some goto optimizations; we
	      ;; optimize the const-X case after all other optimizations.
	      ;;
	      ((and (memq (car lap0) '(byte-varref byte-stack-ref))
		    (progn
		      (setq tmp (cdr rest))
                      (setq tmp2 0)
		      (while (eq (car (car tmp)) 'byte-dup)
			(setq tmp2 (1+ tmp2))
                        (setq tmp (cdr tmp)))
		      t)
		    (eq (if (eq 'byte-stack-ref (car lap0))
                            (+ tmp2 1 (cdr lap0))
                          (cdr lap0))
                        (cdr (car tmp)))
		    (eq (car lap0) (car (car tmp))))
	       (if (memq byte-optimize-log '(t byte))
		   (let ((str ""))
		     (setq tmp2 (cdr rest))
		     (while (not (eq tmp tmp2))
		       (setq tmp2 (cdr tmp2)
			     str (concat str " dup")))
		     (byte-compile-log-lap "  %s%s %s\t-->\t%s%s dup"
					   lap0 str lap0 lap0 str)))
	       (setq keep-going t)
	       (setcar (car tmp) 'byte-dup)
	       (setcdr (car tmp) 0)
	       (setq rest tmp))
	      ;;
	      ;; TAG1: TAG2: --> TAG1: <deleted>
	      ;; (and other references to TAG2 are replaced with TAG1)
	      ;;
	      ((and (eq (car lap0) 'TAG)
		    (eq (car lap1) 'TAG))
	       (and (memq byte-optimize-log '(t byte))
		    (byte-compile-log "  adjacent tags %d and %d merged"
				      (nth 1 lap1) (nth 1 lap0)))
	       (setq tmp3 lap)
	       (while (setq tmp2 (rassq lap0 tmp3))
		 (setcdr tmp2 lap1)
		 (setq tmp3 (cdr (memq tmp2 tmp3))))
	       (setq lap (delq lap0 lap)
		     keep-going t))
	      ;;
	      ;; unused-TAG: --> <deleted>
	      ;;
	      ((and (eq 'TAG (car lap0))
		    (not (rassq lap0 lap)))
	       (and (memq byte-optimize-log '(t byte))
		    (byte-compile-log "  unused tag %d removed" (nth 1 lap0)))
	       (setq lap (delq lap0 lap)
		     keep-going t))
	      ;;
	      ;; goto   ... --> goto   <delete until TAG or end>
	      ;; return ... --> return <delete until TAG or end>
	      ;;
	      ((and (memq (car lap0) '(byte-goto byte-return))
		    (not (memq (car lap1) '(TAG nil))))
	       (setq tmp rest)
	       (let ((i 0)
		     (opt-p (memq byte-optimize-log '(t lap)))
		     str deleted)
		 (while (and (setq tmp (cdr tmp))
			     (not (eq 'TAG (car (car tmp)))))
		   (if opt-p (setq deleted (cons (car tmp) deleted)
				   str (concat str " %s")
				   i (1+ i))))
		 (if opt-p
		     (let ((tagstr
			    (if (eq 'TAG (car (car tmp)))
				(format "%d:" (car (cdr (car tmp))))
			      (or (car tmp) ""))))
		       (if (< i 6)
			   (apply 'byte-compile-log-lap-1
				  (concat "  %s" str
					  " %s\t-->\t%s <deleted> %s")
				  lap0
				  (nconc (nreverse deleted)
					 (list tagstr lap0 tagstr)))
			 (byte-compile-log-lap
			  "  %s <%d unreachable op%s> %s\t-->\t%s <deleted> %s"
			  lap0 i (if (= i 1) "" "s")
			  tagstr lap0 tagstr))))
		 (rplacd rest tmp))
	       (setq keep-going t))
	      ;;
	      ;; <safe-op> unbind --> unbind <safe-op>
	      ;; (this may enable other optimizations.)
	      ;;
	      ((and (eq 'byte-unbind (car lap1))
		    (memq (car lap0) byte-after-unbind-ops))
	       (byte-compile-log-lap "  %s %s\t-->\t%s %s" lap0 lap1 lap1 lap0)
	       (setcar rest lap1)
	       (setcar (cdr rest) lap0)
	       (setq keep-going t))
	      ;;
	      ;; varbind-X unbind-N         -->  discard unbind-(N-1)
	      ;; save-excursion unbind-N    -->  unbind-(N-1)
	      ;; save-restriction unbind-N  -->  unbind-(N-1)
	      ;;
	      ((and (eq 'byte-unbind (car lap1))
		    (memq (car lap0) '(byte-varbind byte-save-excursion
				       byte-save-restriction))
		    (< 0 (cdr lap1)))
	       (if (zerop (setcdr lap1 (1- (cdr lap1))))
		   (delq lap1 rest))
	       (if (eq (car lap0) 'byte-varbind)
		   (setcar rest (cons 'byte-discard 0))
		 (setq lap (delq lap0 lap)))
	       (byte-compile-log-lap "  %s %s\t-->\t%s %s"
		 lap0 (cons (car lap1) (1+ (cdr lap1)))
		 (if (eq (car lap0) 'byte-varbind)
		     (car rest)
		   (car (cdr rest)))
		 (if (and (/= 0 (cdr lap1))
			  (eq (car lap0) 'byte-varbind))
		     (car (cdr rest))
		   ""))
	       (setq keep-going t))
	      ;;
	      ;; goto*-X ... X: goto-Y  --> goto*-Y
	      ;; goto-X ...  X: return  --> return
	      ;;
	      ((and (memq (car lap0) byte-goto-ops)
		    (memq (car (setq tmp (nth 1 (memq (cdr lap0) lap))))
			  '(byte-goto byte-return)))
	       (cond ((and (not (eq tmp lap0))
			   (or (eq (car lap0) 'byte-goto)
			       (eq (car tmp) 'byte-goto)))
		      (byte-compile-log-lap "  %s [%s]\t-->\t%s"
					    (car lap0) tmp tmp)
		      (if (eq (car tmp) 'byte-return)
			  (setcar lap0 'byte-return))
		      (setcdr lap0 (cdr tmp))
		      (setq keep-going t))))
	      ;;
	      ;; goto-*-else-pop X ... X: goto-if-* --> whatever
	      ;; goto-*-else-pop X ... X: discard --> whatever
	      ;;
	      ((and (memq (car lap0) '(byte-goto-if-nil-else-pop
				       byte-goto-if-not-nil-else-pop))
		    (memq (car (car (setq tmp (cdr (memq (cdr lap0) lap)))))
			  (eval-when-compile
			   (cons 'byte-discard byte-conditional-ops)))
		    (not (eq lap0 (car tmp))))
	       (setq tmp2 (car tmp))
	       (setq tmp3 (assq (car lap0) '((byte-goto-if-nil-else-pop
					      byte-goto-if-nil)
					     (byte-goto-if-not-nil-else-pop
					      byte-goto-if-not-nil))))
	       (if (memq (car tmp2) tmp3)
		   (progn (setcar lap0 (car tmp2))
			  (setcdr lap0 (cdr tmp2))
			  (byte-compile-log-lap "  %s-else-pop [%s]\t-->\t%s"
						(car lap0) tmp2 lap0))
		 ;; Get rid of the -else-pop's and jump one step further.
		 (or (eq 'TAG (car (nth 1 tmp)))
		     (setcdr tmp (cons (byte-compile-make-tag)
				       (cdr tmp))))
		 (byte-compile-log-lap "  %s [%s]\t-->\t%s <skip>"
				       (car lap0) tmp2 (nth 1 tmp3))
		 (setcar lap0 (nth 1 tmp3))
		 (setcdr lap0 (nth 1 tmp)))
	       (setq keep-going t))
	      ;;
	      ;; const goto-X ... X: goto-if-* --> whatever
	      ;; const goto-X ... X: discard   --> whatever
	      ;;
	      ((and (eq (car lap0) 'byte-constant)
		    (eq (car lap1) 'byte-goto)
		    (memq (car (car (setq tmp (cdr (memq (cdr lap1) lap)))))
			  (eval-when-compile
			    (cons 'byte-discard byte-conditional-ops)))
		    (not (eq lap1 (car tmp))))
	       (setq tmp2 (car tmp))
	       (cond ((when (consp (cdr lap0))
		        (memq (car tmp2)
			      (if (null (car (cdr lap0)))
				  '(byte-goto-if-nil byte-goto-if-nil-else-pop)
				'(byte-goto-if-not-nil
				  byte-goto-if-not-nil-else-pop))))
		      (byte-compile-log-lap "  %s goto [%s]\t-->\t%s %s"
					    lap0 tmp2 lap0 tmp2)
		      (setcar lap1 (car tmp2))
		      (setcdr lap1 (cdr tmp2))
		      ;; Let next step fix the (const,goto-if*) sequence.
		      (setq rest (cons nil rest))
		      (setq keep-going t))
		     ((or (consp (cdr lap0))
			  (eq (car tmp2) 'byte-discard))
		      ;; Jump one step further
		      (byte-compile-log-lap
		       "  %s goto [%s]\t-->\t<deleted> goto <skip>"
		       lap0 tmp2)
		      (or (eq 'TAG (car (nth 1 tmp)))
			  (setcdr tmp (cons (byte-compile-make-tag)
					    (cdr tmp))))
		      (setcdr lap1 (car (cdr tmp)))
		      (setq lap (delq lap0 lap))
		      (setq keep-going t))))
	      ;;
	      ;; X: varref-Y    ...     varset-Y goto-X  -->
	      ;; X: varref-Y Z: ... dup varset-Y goto-Z
	      ;; (varset-X goto-BACK, BACK: varref-X --> copy the varref down.)
	      ;; (This is so usual for while loops that it is worth handling).
              ;;
              ;; Here again, we could do it for stack-ref/stack-set, but
	      ;; that's replacing a stack-ref-Y with a stack-ref-0, which
              ;; is a very minor improvement (if any), at the cost of
	      ;; more stack use and more byte-code.  Let's not do it.
	      ;;
	      ((and (eq (car lap1) 'byte-varset)
		    (eq (car lap2) 'byte-goto)
		    (not (memq (cdr lap2) rest)) ;Backwards jump
		    (eq (car (car (setq tmp (cdr (memq (cdr lap2) lap)))))
			'byte-varref)
		    (eq (cdr (car tmp)) (cdr lap1))
		    (not (memq (car (cdr lap1)) byte-boolean-vars)))
	       ;;(byte-compile-log-lap "  Pulled %s to end of loop" (car tmp))
	       (let ((newtag (byte-compile-make-tag)))
		 (byte-compile-log-lap
		  "  %s: %s ... %s %s\t-->\t%s: %s %s: ... %s %s %s"
		  (nth 1 (cdr lap2)) (car tmp)
                  lap1 lap2
		  (nth 1 (cdr lap2)) (car tmp)
		  (nth 1 newtag) 'byte-dup lap1
		  (cons 'byte-goto newtag)
		  )
		 (setcdr rest (cons (cons 'byte-dup 0) (cdr rest)))
		 (setcdr tmp (cons (setcdr lap2 newtag) (cdr tmp))))
	       (setq add-depth 1)
	       (setq keep-going t))
	      ;;
	      ;; goto-X Y: ... X: goto-if*-Y  -->  goto-if-not-*-X+1 Y:
	      ;; (This can pull the loop test to the end of the loop)
	      ;;
	      ((and (eq (car lap0) 'byte-goto)
		    (eq (car lap1) 'TAG)
		    (eq lap1
			(cdr (car (setq tmp (cdr (memq (cdr lap0) lap))))))
		    (memq (car (car tmp))
			  '(byte-goto byte-goto-if-nil byte-goto-if-not-nil
				      byte-goto-if-nil-else-pop)))
;;	       (byte-compile-log-lap "  %s %s, %s %s  --> moved conditional"
;;				     lap0 lap1 (cdr lap0) (car tmp))
	       (let ((newtag (byte-compile-make-tag)))
		 (byte-compile-log-lap
		  "%s %s: ... %s: %s\t-->\t%s ... %s:"
		  lap0 (nth 1 lap1) (nth 1 (cdr lap0)) (car tmp)
		  (cons (cdr (assq (car (car tmp))
				   '((byte-goto-if-nil . byte-goto-if-not-nil)
				     (byte-goto-if-not-nil . byte-goto-if-nil)
				     (byte-goto-if-nil-else-pop .
				      byte-goto-if-not-nil-else-pop)
				     (byte-goto-if-not-nil-else-pop .
				      byte-goto-if-nil-else-pop))))
			newtag)

		  (nth 1 newtag)
		  )
		 (setcdr tmp (cons (setcdr lap0 newtag) (cdr tmp)))
		 (if (eq (car (car tmp)) 'byte-goto-if-nil-else-pop)
		     ;; We can handle this case but not the -if-not-nil case,
		     ;; because we won't know which non-nil constant to push.
		   (setcdr rest (cons (cons 'byte-constant
					    (byte-compile-get-constant nil))
				      (cdr rest))))
	       (setcar lap0 (nth 1 (memq (car (car tmp))
					 '(byte-goto-if-nil-else-pop
					   byte-goto-if-not-nil
					   byte-goto-if-nil
					   byte-goto-if-not-nil
					   byte-goto byte-goto))))
	       )
	       (setq keep-going t))
	      )
	(setq rest (cdr rest)))
      )
    ;; Cleanup stage:
    ;; Rebuild byte-compile-constants / byte-compile-variables.
    ;; Simple optimizations that would inhibit other optimizations if they
    ;; were done in the optimizing loop, and optimizations which there is no
    ;; need to do more than once.
    (setq byte-compile-constants nil
	  byte-compile-variables nil)
    (setq rest lap)
    (byte-compile-log-lap "  ---- final pass")
    (while rest
      (setq lap0 (car rest)
	    lap1 (nth 1 rest))
      (if (memq (car lap0) byte-constref-ops)
	  (if (memq (car lap0) '(byte-constant byte-constant2))
	      (unless (memq (cdr lap0) byte-compile-constants)
		(setq byte-compile-constants (cons (cdr lap0)
						   byte-compile-constants)))
	    (unless (memq (cdr lap0) byte-compile-variables)
	      (setq byte-compile-variables (cons (cdr lap0)
						 byte-compile-variables)))))
      (cond (;;
	     ;; const-C varset-X const-C  -->  const-C dup varset-X
	     ;; const-C varbind-X const-C  -->  const-C dup varbind-X
	     ;;
	     (and (eq (car lap0) 'byte-constant)
		  (eq (car (nth 2 rest)) 'byte-constant)
		  (eq (cdr lap0) (cdr (nth 2 rest)))
		  (memq (car lap1) '(byte-varbind byte-varset)))
	     (byte-compile-log-lap "  %s %s %s\t-->\t%s dup %s"
				   lap0 lap1 lap0 lap0 lap1)
	     (setcar (cdr (cdr rest)) (cons (car lap1) (cdr lap1)))
	     (setcar (cdr rest) (cons 'byte-dup 0))
	     (setq add-depth 1))
	    ;;
	    ;; const-X  [dup/const-X ...]   -->  const-X  [dup ...] dup
	    ;; varref-X [dup/varref-X ...]  -->  varref-X [dup ...] dup
	    ;;
	    ((memq (car lap0) '(byte-constant byte-varref))
	     (setq tmp rest
		   tmp2 nil)
	     (while (progn
		      (while (eq 'byte-dup (car (car (setq tmp (cdr tmp))))))
		      (and (eq (cdr lap0) (cdr (car tmp)))
			   (eq (car lap0) (car (car tmp)))))
	       (setcar tmp (cons 'byte-dup 0))
	       (setq tmp2 t))
	     (if tmp2
		 (byte-compile-log-lap
		  "  %s [dup/%s]...\t-->\t%s dup..." lap0 lap0 lap0)))
	    ;;
	    ;; unbind-N unbind-M  -->  unbind-(N+M)
	    ;;
	    ((and (eq 'byte-unbind (car lap0))
		  (eq 'byte-unbind (car lap1)))
	     (byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1
				   (cons 'byte-unbind
					 (+ (cdr lap0) (cdr lap1))))
	     (setq lap (delq lap0 lap))
	     (setcdr lap1 (+ (cdr lap1) (cdr lap0))))

	    ;;
	    ;; stack-set-M [discard/discardN ...]  -->  discardN-preserve-tos
	    ;; stack-set-M [discard/discardN ...]  -->  discardN
	    ;;
	    ((and (eq (car lap0) 'byte-stack-set)
	          (memq (car lap1) '(byte-discard byte-discardN))
	          (progn
	            ;; See if enough discard operations follow to expose or
	            ;; destroy the value stored by the stack-set.
	            (setq tmp (cdr rest))
	            (setq tmp2 (1- (cdr lap0)))
	            (setq tmp3 0)
	            (while (memq (car (car tmp)) '(byte-discard byte-discardN))
	              (setq tmp3
                            (+ tmp3 (if (eq (car (car tmp)) 'byte-discard)
                                        1
                                      (cdr (car tmp)))))
	              (setq tmp (cdr tmp)))
	            (>= tmp3 tmp2)))
	     ;; Do the optimization.
	     (setq lap (delq lap0 lap))
             (setcar lap1
                     (if (= tmp2 tmp3)
                         ;; The value stored is the new TOS, so pop one more
                         ;; value (to get rid of the old value) using the
                         ;; TOS-preserving discard operator.
                         'byte-discardN-preserve-tos
                       ;; Otherwise, the value stored is lost, so just use a
                       ;; normal discard.
                       'byte-discardN))
             (setcdr lap1 (1+ tmp3))
	     (setcdr (cdr rest) tmp)
	     (byte-compile-log-lap "  %s [discard/discardN]...\t-->\t%s"
	        		   lap0 lap1))

	    ;;
	    ;; discard/discardN/discardN-preserve-tos-X discard/discardN-Y  -->
	    ;; discardN-(X+Y)
	    ;;
	    ((and (memq (car lap0)
			'(byte-discard byte-discardN
			  byte-discardN-preserve-tos))
		  (memq (car lap1) '(byte-discard byte-discardN)))
	     (setq lap (delq lap0 lap))
	     (byte-compile-log-lap
	      "  %s %s\t-->\t(discardN %s)"
	      lap0 lap1
	      (+ (if (eq (car lap0) 'byte-discard) 1 (cdr lap0))
		 (if (eq (car lap1) 'byte-discard) 1 (cdr lap1))))
	     (setcdr lap1 (+ (if (eq (car lap0) 'byte-discard) 1 (cdr lap0))
			     (if (eq (car lap1) 'byte-discard) 1 (cdr lap1))))
	     (setcar lap1 'byte-discardN))

	    ;;
	    ;; discardN-preserve-tos-X discardN-preserve-tos-Y  -->
	    ;; discardN-preserve-tos-(X+Y)
	    ;;
	    ((and (eq (car lap0) 'byte-discardN-preserve-tos)
		  (eq (car lap1) 'byte-discardN-preserve-tos))
	     (setq lap (delq lap0 lap))
	     (setcdr lap1 (+ (cdr lap0) (cdr lap1)))
	     (byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1 (car rest)))

	    ;;
	    ;; discardN-preserve-tos return  -->  return
	    ;; dup return  -->  return
	    ;; stack-set-N return  -->  return     ; where N is TOS-1
	    ;;
	    ((and (eq (car lap1) 'byte-return)
	          (or (memq (car lap0) '(byte-discardN-preserve-tos byte-dup))
	              (and (eq (car lap0) 'byte-stack-set)
	        	   (= (cdr lap0) 1))))
	     ;; The byte-code interpreter will pop the stack for us, so
	     ;; we can just leave stuff on it.
	     (setq lap (delq lap0 lap))
	     (byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1 lap1))
            )
      (setq rest (cdr rest)))
    (setq byte-compile-maxdepth (+ byte-compile-maxdepth add-depth)))
  lap)

(provide 'byte-opt)


;; To avoid "lisp nesting exceeds max-lisp-eval-depth" when this file compiles
;; itself, compile some of its most used recursive functions (at load time).
;;
(eval-when-compile
 (or (byte-code-function-p (symbol-function 'byte-optimize-form))
     (assq 'byte-code (symbol-function 'byte-optimize-form))
     (let ((byte-optimize nil)
	   (byte-compile-warnings nil))
       (mapc (lambda (x)
	       (or noninteractive (message "compiling %s..." x))
	       (byte-compile x)
	       (or noninteractive (message "compiling %s...done" x)))
	     '(byte-optimize-form
	       byte-optimize-body
	       byte-optimize-predicate
	       byte-optimize-binary-predicate
	       ;; Inserted some more than necessary, to speed it up.
	       byte-optimize-form-code-walker
	       byte-optimize-lapcode))))
 nil)

;;; byte-opt.el ends here
