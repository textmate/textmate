;;; cconv.el --- Closure conversion for statically scoped Emacs lisp. -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

;; Author: Igor Kuzmin <kzuminig@iro.umontreal.ca>
;; Maintainer: FSF
;; Keywords: lisp
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

;; This takes a piece of Elisp code, and eliminates all free variables from
;; lambda expressions.  The user entry points are cconv-closure-convert and
;; cconv-closure-convert-toplevel (for toplevel forms).
;; All macros should be expanded beforehand.
;;
;; Here is a brief explanation how this code works.
;; Firstly, we analyze the tree by calling cconv-analyse-form.
;; This function finds all mutated variables, all functions that are suitable
;; for lambda lifting and all variables captured by closure. It passes the tree
;; once, returning a list of three lists.
;;
;; Then we calculate the intersection of the first and third lists returned by
;; cconv-analyse form to find all mutated variables that are captured by
;; closure.

;; Armed with this data, we call cconv-closure-convert-rec, that rewrites the
;; tree recursively, lifting lambdas where possible, building closures where it
;; is needed and eliminating mutable variables used in closure.
;;
;; We do following replacements :
;; (lambda (v1 ...) ... fv1 fv2 ...) => (lambda (v1 ... fv1 fv2 ) ... fv1 fv2 .)
;; if the function is suitable for lambda lifting (if all calls are known)
;;
;; (lambda (v0 ...) ... fv0 .. fv1 ...)  =>
;; (internal-make-closure (v0 ...) (fv1 ...)
;;   ... (internal-get-closed-var 0) ...  (internal-get-closed-var 1) ...)
;;
;; If the function has no free variables, we don't do anything.
;;
;; If a variable is mutated (updated by setq), and it is used in a closure
;; we wrap its definition with list: (list val) and we also replace
;; var => (car var) wherever this variable is used, and also
;; (setq var value) => (setcar var value) where it is updated.
;;
;; If defun argument is closure mutable, we letbind it and wrap it's
;; definition with list.
;; (defun foo (... mutable-arg ...) ...) =>
;; (defun foo (... m-arg ...) (let ((m-arg (list m-arg))) ...))
;;
;;; Code:

;; TODO: (not just for cconv but also for the lexbind changes in general)
;; - let (e)debug find the value of lexical variables from the stack.
;; - make eval-region do the eval-sexp-add-defvars dance.
;; - byte-optimize-form should be applied before cconv.
;;   OTOH, the warnings emitted by cconv-analyze need to come before optimize
;;   since afterwards they can because obnoxious (warnings about an "unused
;;   variable" should not be emitted when the variable use has simply been
;;   optimized away).
;; - turn defun and defmacro into macros (and remove special handling of
;;   `declare' afterwards).
;; - let macros specify that some let-bindings come from the same source,
;;   so the unused warning takes all uses into account.
;; - let interactive specs return a function to build the args (to stash into
;;   command-history).
;; - canonize code in macro-expand so we don't have to handle (let (var) body)
;;   and other oddities.
;; - new byte codes for unwind-protect, catch, and condition-case so that
;;   closures aren't needed at all.
;; - inline source code of different binding mode by first compiling it.
;; - a reference to a var that is known statically to always hold a constant
;;   should be turned into a byte-constant rather than a byte-stack-ref.
;;   Hmm... right, that's called constant propagation and could be done here,
;;   but when that constant is a function, we have to be careful to make sure
;;   the bytecomp only compiles it once.
;; - Since we know here when a variable is not mutated, we could pass that
;;   info to the byte-compiler, e.g. by using a new `immutable-let'.
;; - add tail-calls to bytecode.c and the byte compiler.
;; - call known non-escaping functions with `goto' rather than `call'.
;; - optimize mapcar to a while loop.

;; (defmacro dlet (binders &rest body)
;;   ;; Works in both lexical and non-lexical mode.
;;   `(progn
;;      ,@(mapcar (lambda (binder)
;;                  `(defvar ,(if (consp binder) (car binder) binder)))
;;                binders)
;;      (let ,binders ,@body)))

;; (defmacro llet (binders &rest body)
;;   ;; Only works in lexical-binding mode.
;;   `(funcall
;;     (lambda ,(mapcar (lambda (binder) (if (consp binder) (car binder) binder))
;;                 binders)
;;       ,@body)
;;     ,@(mapcar (lambda (binder) (if (consp binder) (cadr binder)))
;;               binders)))

(eval-when-compile (require 'cl))

(defconst cconv-liftwhen 6
  "Try to do lambda lifting if the number of arguments + free variables
is less than this number.")
;; List of all the variables that are both captured by a closure
;; and mutated.  Each entry in the list takes the form
;; (BINDER . PARENTFORM) where BINDER is the (VAR VAL) that introduces the
;; variable (or is just (VAR) for variables not introduced by let).
(defvar cconv-captured+mutated)

;; List of candidates for lambda lifting.
;; Each candidate has the form (BINDER . PARENTFORM).  A candidate
;; is a variable that is only passed to `funcall' or `apply'.
(defvar cconv-lambda-candidates)

;; Alist associating to each function body the list of its free variables.
(defvar cconv-freevars-alist)

;;;###autoload
(defun cconv-closure-convert (form)
  "Main entry point for closure conversion.
-- FORM is a piece of Elisp code after macroexpansion.
-- TOPLEVEL(optional) is a boolean variable, true if we are at the root of AST

Returns a form where all lambdas don't have any free variables."
  ;; (message "Entering cconv-closure-convert...")
  (let ((cconv-freevars-alist '())
	(cconv-lambda-candidates '())
	(cconv-captured+mutated '()))
    ;; Analyze form - fill these variables with new information.
    (cconv-analyse-form form '())
    (setq cconv-freevars-alist (nreverse cconv-freevars-alist))
    (cconv-convert form nil nil))) ; Env initially empty.

(defconst cconv--dummy-var (make-symbol "ignored"))

(defun cconv--set-diff (s1 s2)
  "Return elements of set S1 that are not in set S2."
  (let ((res '()))
    (dolist (x s1)
      (unless (memq x s2) (push x res)))
    (nreverse res)))

(defun cconv--set-diff-map (s m)
  "Return elements of set S that are not in Dom(M)."
  (let ((res '()))
    (dolist (x s)
      (unless (assq x m) (push x res)))
    (nreverse res)))

(defun cconv--map-diff (m1 m2)
  "Return the submap of map M1 that has Dom(M2) removed."
  (let ((res '()))
    (dolist (x m1)
      (unless (assq (car x) m2) (push x res)))
    (nreverse res)))

(defun cconv--map-diff-elem (m x)
  "Return the map M minus any mapping for X."
  ;; Here we assume that X appears at most once in M.
  (let* ((b (assq x m))
         (res (if b (remq b m) m)))
    (assert (null (assq x res))) ;; Check the assumption was warranted.
    res))

(defun cconv--map-diff-set (m s)
  "Return the map M minus any mapping for elements of S."
  ;; Here we assume that X appears at most once in M.
  (let ((res '()))
    (dolist (b m)
      (unless (memq (car b) s) (push b res)))
    (nreverse res)))

(defun cconv--convert-function (args body env parentform)
  (assert (equal body (caar cconv-freevars-alist)))
  (let* ((fvs (cdr (pop cconv-freevars-alist)))
         (body-new '())
         (letbind '())
         (envector ())
         (i 0)
         (new-env ()))
    ;; Build the "formal and actual envs" for the closure-converted function.
    (dolist (fv fvs)
      (let ((exp (or (cdr (assq fv env)) fv)))
        (pcase exp
          ;; If `fv' is a variable that's wrapped in a cons-cell,
          ;; we want to put the cons-cell itself in the closure,
          ;; rather than just a copy of its current content.
          (`(car ,iexp . ,_)
           (push iexp envector)
           (push `(,fv . (car (internal-get-closed-var ,i))) new-env))
          (_
           (push exp envector)
           (push `(,fv . (internal-get-closed-var ,i)) new-env))))
      (setq i (1+ i)))
    (setq envector (nreverse envector))
    (setq new-env (nreverse new-env))

    (dolist (arg args)
      (if (not (member (cons (list arg) parentform) cconv-captured+mutated))
          (if (assq arg new-env) (push `(,arg) new-env))
        (push `(,arg . (car ,arg)) new-env)
        (push `(,arg (list ,arg)) letbind)))

    (setq body-new (mapcar (lambda (form)
                             (cconv-convert form new-env nil))
                           body))

    (when letbind
      (let ((special-forms '()))
        ;; Keep special forms at the beginning of the body.
        (while (or (stringp (car body-new)) ;docstring.
                   (memq (car-safe (car body-new)) '(interactive declare)))
          (push (pop body-new) special-forms))
        (setq body-new
              `(,@(nreverse special-forms) (let ,letbind . ,body-new)))))

    (cond
     ((null envector)                   ;if no freevars - do nothing
      `(function (lambda ,args . ,body-new)))
     (t
      `(internal-make-closure
        ,args ,envector . ,body-new)))))

(defun cconv-convert (form env extend)
  ;; This function actually rewrites the tree.
  "Return FORM with all its lambdas changed so they are closed.
ENV is a lexical environment mapping variables to the expression
used to get its value.  This is used for variables that are copied into
closures, moved into cons cells, ...
ENV is a list where each entry takes the shape either:
 (VAR . (car EXP)): VAR has been moved into the car of a cons-cell, and EXP
    is an expression that evaluates to this cons-cell.
 (VAR . (internal-get-closed-var N)): VAR has been copied into the closure
    environment's Nth slot.
 (VAR . (apply-partially F ARG1 ARG2 ..)): VAR has been λ-lifted and takes
    additional arguments ARGs.
EXTEND is a list of variables which might need to be accessed even from places
where they are shadowed, because some part of ENV causes them to be used at
places where they originally did not directly appear."
  (assert (not (delq nil (mapcar (lambda (mapping)
                                   (if (eq (cadr mapping) 'apply-partially)
                                       (cconv--set-diff (cdr (cddr mapping))
                                                        extend)))
                                 env))))

  ;; What's the difference between fvrs and envs?
  ;; Suppose that we have the code
  ;; (lambda (..) fvr (let ((fvr 1)) (+ fvr 1)))
  ;; only the first occurrence of fvr should be replaced by
  ;; (aref env ...).
  ;; So initially envs and fvrs are the same thing, but when we descend to
  ;; the 'let, we delete fvr from fvrs. Why we don't delete fvr from envs?
  ;; Because in envs the order of variables is important. We use this list
  ;; to find the number of a specific variable in the environment vector,
  ;; so we never touch it(unless we enter to the other closure).
  ;;(if (listp form) (print (car form)) form)
  (pcase form
    (`(,(and letsym (or `let* `let)) ,binders . ,body)

					; let and let* special forms
     (let ((binders-new '())
           (new-env env)
           (new-extend extend))

       (dolist (binder binders)
         (let* ((value nil)
                (var (if (not (consp binder))
                         (prog1 binder (setq binder (list binder)))
                       (setq value (cadr binder))
                       (car binder)))
                (new-val
                 (cond
                  ;; Check if var is a candidate for lambda lifting.
                  ((and (member (cons binder form) cconv-lambda-candidates)
                        (progn
                          (assert (and (eq (car value) 'function)
                                       (eq (car (cadr value)) 'lambda)))
                          (assert (equal (cddr (cadr value))
                                         (caar cconv-freevars-alist)))
                          ;; Peek at the freevars to decide whether to λ-lift.
                          (let* ((fvs (cdr (car cconv-freevars-alist)))
                                 (fun (cadr value))
                                 (funargs (cadr fun))
                                 (funcvars (append fvs funargs)))
					; lambda lifting condition
                            (and fvs (>= cconv-liftwhen (length funcvars))))))
					; Lift.
                   (let* ((fvs (cdr (pop cconv-freevars-alist)))
                          (fun (cadr value))
                          (funargs (cadr fun))
                          (funcvars (append fvs funargs))
                          (funcbody (cddr fun))
                          (funcbody-env ()))
                     (push `(,var . (apply-partially ,var . ,fvs)) new-env)
                     (dolist (fv fvs)
                       (pushnew fv new-extend)
                       (if (and (eq 'car (car-safe (cdr (assq fv env))))
                                (not (memq fv funargs)))
                           (push `(,fv . (car ,fv)) funcbody-env)))
                     `(function (lambda ,funcvars .
                                  ,(mapcar (lambda (form)
                                             (cconv-convert
                                              form funcbody-env nil))
                                           funcbody)))))

                  ;; Check if it needs to be turned into a "ref-cell".
                  ((member (cons binder form) cconv-captured+mutated)
                   ;; Declared variable is mutated and captured.
                   (push `(,var . (car ,var)) new-env)
                   `(list ,(cconv-convert value env extend)))

                  ;; Normal default case.
                  (t
                   (if (assq var new-env) (push `(,var) new-env))
                   (cconv-convert value env extend)))))

           ;; The piece of code below letbinds free variables of a λ-lifted
           ;; function if they are redefined in this let, example:
           ;;   (let* ((fun (lambda (x) (+ x y))) (y 1)) (funcall fun 1))
           ;; Here we can not pass y as parameter because it is redefined.
           ;; So we add a (closed-y y) declaration.  We do that even if the
           ;; function is not used inside this let(*).  The reason why we
           ;; ignore this case is that we can't "look forward" to see if the
           ;; function is called there or not.  To treat this case better we'd
           ;; need to traverse the tree one more time to collect this data, and
           ;; I think that it's not worth it.
           (when (memq var new-extend)
             (let ((closedsym
                    (make-symbol (concat "closed-" (symbol-name var)))))
               (setq new-env
                     (mapcar (lambda (mapping)
                               (if (not (eq (cadr mapping) 'apply-partially))
                                   mapping
                                 (assert (eq (car mapping) (nth 2 mapping)))
                                 (list* (car mapping)
                                        'apply-partially
                                        (car mapping)
                                        (mapcar (lambda (arg)
                                                  (if (eq var arg)
                                                      closedsym arg))
                                                (nthcdr 3 mapping)))))
                             new-env))
               (setq new-extend (remq var new-extend))
               (push closedsym new-extend)
               (push `(,closedsym ,var) binders-new)))

           ;; We push the element after redefined free variables are
           ;; processed.  This is important to avoid the bug when free
           ;; variable and the function have the same name.
           (push (list var new-val) binders-new)

           (when (eq letsym 'let*)
             (setq env new-env)
             (setq extend new-extend))
           ))                           ; end of dolist over binders

       `(,letsym ,(nreverse binders-new)
                 . ,(mapcar (lambda (form)
                              (cconv-convert
                               form new-env new-extend))
                            body))))
					;end of let let* forms

                                  ; first element is lambda expression
    (`(,(and `(lambda . ,_) fun) . ,args)
     ;; FIXME: it's silly to create a closure just to call it.
     ;; Running byte-optimize-form earlier will resolve this.
     `(funcall
       ,(cconv-convert `(function ,fun) env extend)
       ,@(mapcar (lambda (form)
                   (cconv-convert form env extend))
                 args)))

    (`(cond . ,cond-forms)              ; cond special form
     `(cond . ,(mapcar (lambda (branch)
                         (mapcar (lambda (form)
                                   (cconv-convert form env extend))
                                 branch))
                       cond-forms)))

    (`(function (lambda ,args . ,body) . ,_)
     (cconv--convert-function args body env form))

    (`(internal-make-closure . ,_)
     (byte-compile-report-error
      "Internal error in compiler: cconv called twice?"))

    (`(quote . ,_) form)
    (`(function . ,_) form)

					;defconst, defvar
    (`(,(and sym (or `defconst `defvar)) ,definedsymbol . ,forms)
     `(,sym ,definedsymbol
            . ,(mapcar (lambda (form) (cconv-convert form env extend))
                       forms)))

					;defun, defmacro
    (`(,(and sym (or `defun `defmacro))
       ,func ,args . ,body)
     (assert (equal body (caar cconv-freevars-alist)))
     (assert (null (cdar cconv-freevars-alist)))

     (let ((new (cconv--convert-function args body env form)))
       (pcase new
         (`(function (lambda ,newargs . ,new-body))
          (assert (equal args newargs))
          `(,sym ,func ,args . ,new-body))
         (t (byte-compile-report-error
             (format "Internal error in cconv of (%s %s ...)" sym func))))))

					;condition-case
    (`(condition-case ,var ,protected-form . ,handlers)
     (let ((newform (cconv--convert-function
                     () (list protected-form) env form)))
       `(condition-case :fun-body ,newform
	  ,@(mapcar (lambda (handler)
                      (list (car handler)
                            (cconv--convert-function
                             (list (or var cconv--dummy-var))
                             (cdr handler) env form)))
                    handlers))))

    (`(,(and head (or `catch `unwind-protect)) ,form . ,body)
     `(,head ,(cconv-convert form env extend)
        :fun-body ,(cconv--convert-function () body env form)))

    (`(track-mouse . ,body)
     `(track-mouse
        :fun-body ,(cconv--convert-function () body env form)))

    (`(setq . ,forms)                   ; setq special form
     (let ((prognlist ()))
       (while forms
         (let* ((sym (pop forms))
                (sym-new (or (cdr (assq sym env)) sym))
                (value (cconv-convert (pop forms) env extend)))
           (push (pcase sym-new
                   ((pred symbolp) `(setq ,sym-new ,value))
                   (`(car ,iexp) `(setcar ,iexp ,value))
                   ;; This "should never happen", but for variables which are
                   ;; mutated+captured+unused, we may end up trying to `setq'
                   ;; on a closed-over variable, so just drop the setq.
                   (_ ;; (byte-compile-report-error
                    ;;  (format "Internal error in cconv of (setq %s ..)"
                    ;;          sym-new))
                    value))
                 prognlist)))
       (if (cdr prognlist)
           `(progn . ,(nreverse prognlist))
         (car prognlist))))

    (`(,(and (or `funcall `apply) callsym) ,fun . ,args)
     ;; These are not special forms but we treat them separately for the needs
     ;; of lambda lifting.
     (let ((mapping (cdr (assq fun env))))
       (pcase mapping
         (`(apply-partially ,_ . ,(and fvs `(,_ . ,_)))
          (assert (eq (cadr mapping) fun))
          `(,callsym ,fun
                     ,@(mapcar (lambda (fv)
                                 (let ((exp (or (cdr (assq fv env)) fv)))
                                   (pcase exp
                                     (`(car ,iexp . ,_) iexp)
                                     (_ exp))))
                               fvs)
                     ,@(mapcar (lambda (arg)
                                 (cconv-convert arg env extend))
                               args)))
         (_ `(,callsym ,@(mapcar (lambda (arg)
                                   (cconv-convert arg env extend))
                                 (cons fun args)))))))

    (`(interactive . ,forms)
     `(interactive . ,(mapcar (lambda (form)
                                (cconv-convert form nil nil))
                              forms)))

    (`(declare . ,_) form)              ;The args don't contain code.

    (`(,func . ,forms)
     ;; First element is function or whatever function-like forms are: or, and,
     ;; if, progn, prog1, prog2, while, until
     `(,func . ,(mapcar (lambda (form)
                          (cconv-convert form env extend))
                        forms)))

    (_ (or (cdr (assq form env)) form))))

(unless (fboundp 'byte-compile-not-lexical-var-p)
  ;; Only used to test the code in non-lexbind Emacs.
  (defalias 'byte-compile-not-lexical-var-p 'boundp))

(defun cconv--analyse-use (vardata form varkind)
  "Analyze the use of a variable.
VARDATA should be (BINDER READ MUTATED CAPTURED CALLED).
VARKIND is the name of the kind of variable.
FORM is the parent form that binds this var."
  ;; use = `(,binder ,read ,mutated ,captured ,called)
  (pcase vardata
    (`(,_ nil nil nil nil) nil)
    (`((,(and (pred (lambda (var) (eq ?_ (aref (symbol-name var) 0)))) var) . ,_)
       ,_ ,_ ,_ ,_)
     (byte-compile-log-warning
      (format "%s `%S' not left unused" varkind var))))
  (pcase vardata
    (`((,var . ,_) nil ,_ ,_ nil)
     ;; FIXME: This gives warnings in the wrong order, with imprecise line
     ;; numbers and without function name info.
     (unless (or ;; Uninterned symbols typically come from macro-expansion, so
              ;; it is often non-trivial for the programmer to avoid such
              ;; unused vars.
              (not (intern-soft var))
              (eq ?_ (aref (symbol-name var) 0))
	      ;; As a special exception, ignore "ignore".
	      (eq var 'ignored))
       (byte-compile-log-warning (format "Unused lexical %s `%S'"
                                         varkind var))))
    ;; If it's unused, there's no point converting it into a cons-cell, even if
    ;; it's captured and mutated.
    (`(,binder ,_ t t ,_)
     (push (cons binder form) cconv-captured+mutated))
    (`(,(and binder `(,_ (function (lambda . ,_)))) nil nil nil t)
     (push (cons binder form) cconv-lambda-candidates))))

(defun cconv--analyse-function (args body env parentform)
  (let* ((newvars nil)
         (freevars (list body))
         ;; We analyze the body within a new environment where all uses are
         ;; nil, so we can distinguish uses within that function from uses
         ;; outside of it.
         (envcopy
          (mapcar (lambda (vdata) (list (car vdata) nil nil nil nil)) env))
         (newenv envcopy))
    ;; Push it before recursing, so cconv-freevars-alist contains entries in
    ;; the order they'll be used by closure-convert-rec.
    (push freevars cconv-freevars-alist)
    (dolist (arg args)
      (cond
       ((byte-compile-not-lexical-var-p arg)
        (byte-compile-log-warning
         (format "Argument %S is not a lexical variable" arg)))
       ((eq ?& (aref (symbol-name arg) 0)) nil) ;Ignore &rest, &optional, ...
       (t (let ((varstruct (list arg nil nil nil nil)))
            (push (cons (list arg) (cdr varstruct)) newvars)
            (push varstruct newenv)))))
    (dolist (form body)                   ;Analyze body forms.
      (cconv-analyse-form form newenv))
    ;; Summarize resulting data about arguments.
    (dolist (vardata newvars)
      (cconv--analyse-use vardata parentform "argument"))
    ;; Transfer uses collected in `envcopy' (via `newenv') back to `env';
    ;; and compute free variables.
    (while env
      (assert (and envcopy (eq (caar env) (caar envcopy))))
      (let ((free nil)
            (x (cdr (car env)))
            (y (cdr (car envcopy))))
        (while x
          (when (car y) (setcar x t) (setq free t))
          (setq x (cdr x) y (cdr y)))
        (when free
          (push (caar env) (cdr freevars))
          (setf (nth 3 (car env)) t))
        (setq env (cdr env) envcopy (cdr envcopy))))))

(defun cconv-analyse-form (form env)
  "Find mutated variables and variables captured by closure.
Analyze lambdas if they are suitable for lambda lifting.
- FORM is a piece of Elisp code after macroexpansion.
- ENV is an alist mapping each enclosing lexical variable to its info.
   I.e. each element has the form (VAR . (READ MUTATED CAPTURED CALLED)).
This function does not return anything but instead fills the
`cconv-captured+mutated' and `cconv-lambda-candidates' variables
and updates the data stored in ENV."
  (pcase form
					; let special form
    (`(,(and (or `let* `let) letsym) ,binders . ,body-forms)

     (let ((orig-env env)
           (newvars nil)
           (var nil)
           (value nil))
       (dolist (binder binders)
         (if (not (consp binder))
             (progn
               (setq var binder)      ; treat the form (let (x) ...) well
               (setq binder (list binder))
               (setq value nil))
           (setq var (car binder))
           (setq value (cadr binder))

           (cconv-analyse-form value (if (eq letsym 'let*) env orig-env)))

         (unless (byte-compile-not-lexical-var-p var)
           (let ((varstruct (list var nil nil nil nil)))
             (push (cons binder (cdr varstruct)) newvars)
             (push varstruct env))))

       (dolist (form body-forms)          ; Analyze body forms.
         (cconv-analyse-form form env))

       (dolist (vardata newvars)
         (cconv--analyse-use vardata form "variable"))))

					; defun special form
    (`(,(or `defun `defmacro) ,func ,vrs . ,body-forms)
     (when env
       (byte-compile-log-warning
        (format "Function %S will ignore its context %S"
                func (mapcar #'car env))
        t :warning))
     (cconv--analyse-function vrs body-forms nil form))

    (`(function (lambda ,vrs . ,body-forms))
     (cconv--analyse-function vrs body-forms env form))

    (`(setq . ,forms)
     ;; If a local variable (member of env) is modified by setq then
     ;; it is a mutated variable.
     (while forms
       (let ((v (assq (car forms) env))) ; v = non nil if visible
         (when v (setf (nth 2 v) t)))
       (cconv-analyse-form (cadr forms) env)
       (setq forms (cddr forms))))

    (`((lambda . ,_) . ,_)             ; first element is lambda expression
     (dolist (exp `((function ,(car form)) . ,(cdr form)))
       (cconv-analyse-form exp env)))

    (`(cond . ,cond-forms)              ; cond special form
     (dolist (forms cond-forms)
       (dolist (form forms) (cconv-analyse-form form env))))

    (`(quote . ,_) nil)                 ; quote form
    (`(function . ,_) nil)              ; same as quote

    (`(condition-case ,var ,protected-form . ,handlers)
     ;; FIXME: The bytecode for condition-case forces us to wrap the
     ;; form and handlers in closures (for handlers, it's understandable
     ;; but not for the protected form).
     (cconv--analyse-function () (list protected-form) env form)
     (dolist (handler handlers)
       (cconv--analyse-function (if var (list var)) (cdr handler) env form)))

    ;; FIXME: The bytecode for catch forces us to wrap the body.
    (`(,(or `catch `unwind-protect) ,form . ,body)
     (cconv-analyse-form form env)
     (cconv--analyse-function () body env form))

    ;; FIXME: The lack of bytecode for track-mouse forces us to wrap the body.
    ;; `track-mouse' really should be made into a macro.
    (`(track-mouse . ,body)
     (cconv--analyse-function () body env form))

    (`(,(or `defconst `defvar) ,var ,value . ,_)
     (push var byte-compile-bound-variables)
     (cconv-analyse-form value env))

    (`(,(or `funcall `apply) ,fun . ,args)
     ;; Here we ignore fun because funcall and apply are the only two
     ;; functions where we can pass a candidate for lambda lifting as
     ;; argument.  So, if we see fun elsewhere, we'll delete it from
     ;; lambda candidate list.
     (let ((fdata (and (symbolp fun) (assq fun env))))
       (if fdata
           (setf (nth 4 fdata) t)
         (cconv-analyse-form fun env)))
     (dolist (form args) (cconv-analyse-form form env)))

    (`(interactive . ,forms)
     ;; These appear within the function body but they don't have access
     ;; to the function's arguments.
     ;; We could extend this to allow interactive specs to refer to
     ;; variables in the function's enclosing environment, but it doesn't
     ;; seem worth the trouble.
     (dolist (form forms) (cconv-analyse-form form nil)))

    (`(declare . ,_) nil)               ;The args don't contain code.

    (`(,_ . ,body-forms)    ; First element is a function or whatever.
     (dolist (form body-forms) (cconv-analyse-form form env)))

    ((pred symbolp)
     (let ((dv (assq form env)))        ; dv = declared and visible
       (when dv
         (setf (nth 1 dv) t))))))

(provide 'cconv)
;;; cconv.el ends here
