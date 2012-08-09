;;; eieio.el --- Enhanced Implementation of Emacs Interpreted Objects
;;;              or maybe Eric's Implementation of Emacs Interpreted Objects

;; Copyright (C) 1995-1996, 1998-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 1.3
;; Keywords: OO, lisp

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
;;
;; EIEIO is a series of Lisp routines which implements a subset of
;; CLOS, the Common Lisp Object System.  In addition, EIEIO also adds
;; a few new features which help it integrate more strongly with the
;; Emacs running environment.
;;
;; See eieio.texi for complete documentation on using this package.
;;
;; Note: the implementation of the c3 algorithm is based on:
;;   Kim Barrett et al.: A Monotonic Superclass Linearization for Dylan
;;   Retrieved from:
;;   http://192.220.96.201/dylan/linearization-oopsla96.html

;; There is funny stuff going on with typep and deftype.  This
;; is the only way I seem to be able to make this stuff load properly.

;; @TODO - fix :initform to be a form, not a quoted value
;; @TODO - Prefix non-clos functions with `eieio-'.

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar eieio-version "1.3"
  "Current version of EIEIO.")

(defun eieio-version ()
  "Display the current version of EIEIO."
  (interactive)
  (message eieio-version))

(eval-and-compile
;; About the above.  EIEIO must process its own code when it compiles
;; itself, thus, by eval-and-compiling ourselves, we solve the problem.

;; Compatibility
(if (fboundp 'compiled-function-arglist)

    ;; XEmacs can only access a compiled functions arglist like this:
    (defalias 'eieio-compiled-function-arglist 'compiled-function-arglist)

  ;; Emacs doesn't have this function, but since FUNC is a vector, we can just
  ;; grab the appropriate element.
  (defun eieio-compiled-function-arglist (func)
    "Return the argument list for the compiled function FUNC."
    (aref func 0))

  )


;;;
;; Variable declarations.
;;

(defvar eieio-hook nil
  "*This hook is executed, then cleared each time `defclass' is called.")

(defvar eieio-error-unsupported-class-tags nil
  "Non-nil to throw an error if an encountered tag is unsupported.
This may prevent classes from CLOS applications from being used with EIEIO
since EIEIO does not support all CLOS tags.")

(defvar eieio-skip-typecheck nil
  "*If non-nil, skip all slot typechecking.
Set this to t permanently if a program is functioning well to get a
small speed increase.  This variable is also used internally to handle
default setting for optimization purposes.")

(defvar eieio-optimize-primary-methods-flag t
  "Non-nil means to optimize the method dispatch on primary methods.")

;; State Variables
;; FIXME: These two constants below should have an `eieio-' prefix added!!
(defvar this nil
  "Inside a method, this variable is the object in question.
DO NOT SET THIS YOURSELF unless you are trying to simulate friendly slots.

Note: Embedded methods are no longer supported.  The variable THIS is
still set for CLOS methods for the sake of routines like
`call-next-method'.")

(defvar scoped-class nil
  "This is set to a class when a method is running.
This is so we know we are allowed to check private parts or how to
execute a `call-next-method'.  DO NOT SET THIS YOURSELF!")

(defvar eieio-initializing-object  nil
  "Set to non-nil while initializing an object.")

(defconst eieio-unbound
  (if (and (boundp 'eieio-unbound) (symbolp eieio-unbound))
      eieio-unbound
    (make-symbol "unbound"))
  "Uninterned symbol representing an unbound slot in an object.")

;; This is a bootstrap for eieio-default-superclass so it has a value
;; while it is being built itself.
(defvar eieio-default-superclass nil)

;; FIXME: The constants below should have an `eieio-' prefix added!!
(defconst class-symbol 1 "Class's symbol (self-referencing.).")
(defconst class-parent 2 "Class parent slot.")
(defconst class-children 3 "Class children class slot.")
(defconst class-symbol-obarray 4 "Obarray permitting fast access to variable position indexes.")
;; @todo
;; the word "public" here is leftovers from the very first version.
;; Get rid of it!
(defconst class-public-a 5 "Class attribute index.")
(defconst class-public-d 6 "Class attribute defaults index.")
(defconst class-public-doc 7 "Class documentation strings for attributes.")
(defconst class-public-type 8 "Class type for a slot.")
(defconst class-public-custom 9 "Class custom type for a slot.")
(defconst class-public-custom-label 10 "Class custom group for a slot.")
(defconst class-public-custom-group 11 "Class custom group for a slot.")
(defconst class-public-printer 12 "Printer for a slot.")
(defconst class-protection 13 "Class protection for a slot.")
(defconst class-initarg-tuples 14 "Class initarg tuples list.")
(defconst class-class-allocation-a 15 "Class allocated attributes.")
(defconst class-class-allocation-doc 16 "Class allocated documentation.")
(defconst class-class-allocation-type 17 "Class allocated value type.")
(defconst class-class-allocation-custom 18 "Class allocated custom descriptor.")
(defconst class-class-allocation-custom-label 19 "Class allocated custom descriptor.")
(defconst class-class-allocation-custom-group 20 "Class allocated custom group.")
(defconst class-class-allocation-printer 21 "Class allocated printer for a slot.")
(defconst class-class-allocation-protection 22 "Class allocated protection list.")
(defconst class-class-allocation-values 23 "Class allocated value vector.")
(defconst class-default-object-cache 24
  "Cache index of what a newly created object would look like.
This will speed up instantiation time as only a `copy-sequence' will
be needed, instead of looping over all the values and setting them
from the default.")
(defconst class-options 25
  "Storage location of tagged class options.
Stored outright without modifications or stripping.")

(defconst class-num-slots 26
  "Number of slots in the class definition object.")

(defconst object-class 1 "Index in an object vector where the class is stored.")
(defconst object-name 2 "Index in an object where the name is stored.")

(defconst method-static 0 "Index into :static tag on a method.")
(defconst method-before 1 "Index into :before tag on a method.")
(defconst method-primary 2 "Index into :primary tag on a method.")
(defconst method-after 3 "Index into :after tag on a method.")
(defconst method-num-lists 4 "Number of indexes into methods vector in which groups of functions are kept.")
(defconst method-generic-before 4 "Index into generic :before tag on a method.")
(defconst method-generic-primary 5 "Index into generic :primary tag on a method.")
(defconst method-generic-after 6 "Index into generic :after tag on a method.")
(defconst method-num-slots 7 "Number of indexes into a method's vector.")

(defsubst eieio-specialized-key-to-generic-key (key)
  "Convert a specialized KEY into a generic method key."
  (cond ((eq key method-static) 0) ;; don't convert
	((< key method-num-lists) (+ key 3)) ;; The conversion
	(t key) ;; already generic.. maybe.
	))


;;; Important macros used in eieio.
;;
(defmacro class-v (class)
  "Internal: Return the class vector from the CLASS symbol."
  ;; No check: If eieio gets this far, it's probably been checked already.
  `(get ,class 'eieio-class-definition))

(defmacro class-p (class)
  "Return t if CLASS is a valid class vector.
CLASS is a symbol."
  ;; this new method is faster since it doesn't waste time checking lots of
  ;; things.
  `(condition-case nil
       (eq (aref (class-v ,class) 0) 'defclass)
     (error nil)))

(defmacro eieio-object-p (obj)
  "Return non-nil if OBJ is an EIEIO object."
  `(condition-case nil
       (let ((tobj ,obj))
	 (and (eq (aref tobj 0) 'object)
	      (class-p (aref tobj object-class))))
     (error nil)))
(defalias 'object-p 'eieio-object-p)

(defmacro class-constructor (class)
  "Return the symbol representing the constructor of CLASS."
  `(aref (class-v ,class) class-symbol))

(defmacro generic-p (method)
  "Return t if symbol METHOD is a generic function.
Only methods have the symbol `eieio-method-obarray' as a property
\(which contains a list of all bindings to that method type.)"
  `(and (fboundp ,method) (get ,method 'eieio-method-obarray)))

(defun generic-primary-only-p (method)
  "Return t if symbol METHOD is a generic function with only primary methods.
Only methods have the symbol `eieio-method-obarray' as a property (which
contains a list of all bindings to that method type.)
Methods with only primary implementations are executed in an optimized way."
  (and (generic-p method)
       (let ((M (get method 'eieio-method-tree)))
	 (and (< 0 (length (aref M method-primary)))
	      (not (aref M method-static))
	      (not (aref M method-before))
	      (not (aref M method-after))
	      (not (aref M method-generic-before))
	      (not (aref M method-generic-primary))
	      (not (aref M method-generic-after))))
       ))

(defun generic-primary-only-one-p (method)
  "Return t if symbol METHOD is a generic function with only primary methods.
Only methods have the symbol `eieio-method-obarray' as a property (which
contains a list of all bindings to that method type.)
Methods with only primary implementations are executed in an optimized way."
  (and (generic-p method)
       (let ((M (get method 'eieio-method-tree)))
	 (and (= 1 (length (aref M method-primary)))
	      (not (aref M method-static))
	      (not (aref M method-before))
	      (not (aref M method-after))
	      (not (aref M method-generic-before))
	      (not (aref M method-generic-primary))
	      (not (aref M method-generic-after))))
       ))

(defmacro class-option-assoc (list option)
  "Return from LIST the found OPTION, or nil if it doesn't exist."
  `(car-safe (cdr (memq ,option ,list))))

(defmacro class-option (class option)
  "Return the value stored for CLASS' OPTION.
Return nil if that option doesn't exist."
  `(class-option-assoc (aref (class-v ,class) class-options) ',option))

(defmacro class-abstract-p (class)
  "Return non-nil if CLASS is abstract.
Abstract classes cannot be instantiated."
  `(class-option ,class :abstract))

(defmacro class-method-invocation-order (class)
  "Return the invocation order of CLASS.
Abstract classes cannot be instantiated."
  `(or (class-option ,class :method-invocation-order)
       :breadth-first))


;;; Defining a new class
;;
(defmacro defclass (name superclass slots &rest options-and-doc)
  "Define NAME as a new class derived from SUPERCLASS with SLOTS.
OPTIONS-AND-DOC is used as the class' options and base documentation.
SUPERCLASS is a list of superclasses to inherit from, with SLOTS
being the slots residing in that class definition.  NOTE: Currently
only one slot may exist in SUPERCLASS as multiple inheritance is not
yet supported.  Supported tags are:

  :initform   - Initializing form.
  :initarg    - Tag used during initialization.
  :accessor   - Tag used to create a function to access this slot.
  :allocation - Specify where the value is stored.
                Defaults to `:instance', but could also be `:class'.
  :writer     - A function symbol which will `write' an object's slot.
  :reader     - A function symbol which will `read' an object.
  :type       - The type of data allowed in this slot (see `typep').
  :documentation
              - A string documenting use of this slot.

The following are extensions on CLOS:
  :protection - Specify protection for this slot.
                Defaults to `:public'.  Also use `:protected', or `:private'.
  :custom     - When customizing an object, the custom :type.  Public only.
  :label      - A text string label used for a slot when customizing.
  :group      - Name of a customization group this slot belongs in.
  :printer    - A function to call to print the value of a slot.
                See `eieio-override-prin1' as an example.

A class can also have optional options.  These options happen in place
of documentation (including a :documentation tag), in addition to
documentation, or not at all.  Supported options are:

  :documentation - The doc-string used for this class.

Options added to EIEIO:

  :allow-nil-initform - Non-nil to skip typechecking of null initforms.
  :custom-groups      - List of custom group names.  Organizes slots into
                        reasonable groups for customizations.
  :abstract           - Non-nil to prevent instances of this class.
                        If a string, use as an error string if someone does
                        try to make an instance.
  :method-invocation-order
                      - Control the method invocation order if there is
                        multiple inheritance.  Valid values are:
                         :breadth-first - The default.
                         :depth-first

Options in CLOS not supported in EIEIO:

  :metaclass - Class to use in place of `standard-class'
  :default-initargs - Initargs to use when initializing new objects of
                      this class.

Due to the way class options are set up, you can add any tags you wish,
and reference them using the function `class-option'."
  ;; We must `eval-and-compile' this so that when we byte compile
  ;; an eieio program, there is no need to load it ahead of time.
  ;; It also provides lots of nice debugging errors at compile time.
  `(eval-and-compile
     (eieio-defclass ',name ',superclass ',slots ',options-and-doc)))

(defvar eieio-defclass-autoload-map (make-vector 7 nil)
  "Symbol map of superclasses we find in autoloads.")

;; We autoload this because it's used in `make-autoload'.
;;;###autoload
(defun eieio-defclass-autoload (cname superclasses filename doc)
  "Create autoload symbols for the EIEIO class CNAME.
SUPERCLASSES are the superclasses that CNAME inherits from.
DOC is the docstring for CNAME.
This function creates a mock-class for CNAME and adds it into
SUPERCLASSES as children.
It creates an autoload function for CNAME's constructor."
  ;; Assume we've already debugged inputs.

  (let* ((oldc (when (class-p cname) (class-v cname)))
	 (newc (make-vector class-num-slots nil))
	 )
    (if oldc
	nil ;; Do nothing if we already have this class.

      ;; Create the class in NEWC, but don't fill anything else in.
      (aset newc 0 'defclass)
      (aset newc class-symbol cname)

      (let ((clear-parent nil))
	;; No parents?
	(when (not superclasses)
	  (setq superclasses '(eieio-default-superclass)
		clear-parent t)
	  )

	;; Hook our new class into the existing structures so we can
	;; autoload it later.
	(dolist (SC superclasses)


	  ;; TODO - If we create an autoload that is in the map, that
	  ;;        map needs to be cleared!


	  ;; Does our parent exist?
	  (if (not (class-p SC))

	      ;; Create a symbol for this parent, and then store this
	      ;; parent on that symbol.
	      (let ((sym (intern (symbol-name SC) eieio-defclass-autoload-map)))
		(if (not (boundp sym))
		    (set sym (list cname))
		  (add-to-list sym cname))
		)

	    ;; We have a parent, save the child in there.
	    (when (not (member cname (aref (class-v SC) class-children)))
	      (aset (class-v SC) class-children
		    (cons cname (aref (class-v SC) class-children)))))

	  ;; save parent in child
	  (aset newc class-parent (cons SC (aref newc class-parent)))
	  )

	;; turn this into a usable self-pointing symbol
	(set cname cname)

	;; Store the new class vector definition into the symbol.  We need to
	;; do this first so that we can call defmethod for the accessor.
	;; The vector will be updated by the following while loop and will not
	;; need to be stored a second time.
	(put cname 'eieio-class-definition newc)

	;; Clear the parent
	(if clear-parent (aset newc class-parent nil))

	;; Create an autoload on top of our constructor function.
	(autoload cname filename doc nil nil)
	(autoload (intern (concat (symbol-name cname) "-p")) filename "" nil nil)
	(autoload (intern (concat (symbol-name cname) "-child-p")) filename "" nil nil)

	))))

(defsubst eieio-class-un-autoload (cname)
  "If class CNAME is in an autoload state, load its file."
  (when (eq (car-safe (symbol-function cname)) 'autoload)
    (load-library (car (cdr (symbol-function cname))))))

(defun eieio-defclass (cname superclasses slots options-and-doc)
  ;; FIXME: Most of this should be moved to the `defclass' macro.
  "Define CNAME as a new subclass of SUPERCLASSES.
SLOTS are the slots residing in that class definition, and options or
documentation OPTIONS-AND-DOC is the toplevel documentation for this class.
See `defclass' for more information."
  ;; Run our eieio-hook each time, and clear it when we are done.
  ;; This way people can add hooks safely if they want to modify eieio
  ;; or add definitions when eieio is loaded or something like that.
  (run-hooks 'eieio-hook)
  (setq eieio-hook nil)

  (if (not (symbolp cname)) (signal 'wrong-type-argument '(symbolp cname)))
  (if (not (listp superclasses)) (signal 'wrong-type-argument '(listp superclasses)))

  (let* ((pname (if superclasses superclasses nil))
	 (newc (make-vector class-num-slots nil))
	 (oldc (when (class-p cname) (class-v cname)))
	 (groups nil) ;; list of groups id'd from slots
	 (options nil)
	 (clearparent nil))

    (aset newc 0 'defclass)
    (aset newc class-symbol cname)

    ;; If this class already existed, and we are updating its structure,
    ;; make sure we keep the old child list.  This can cause bugs, but
    ;; if no new slots are created, it also saves time, and prevents
    ;; method table breakage, particularly when the users is only
    ;; byte compiling an EIEIO file.
    (if oldc
	(aset newc class-children (aref oldc class-children))
      ;; If the old class did not exist, but did exist in the autoload map, then adopt those children.
      ;; This is like the above, but deals with autoloads nicely.
      (let ((sym (intern-soft (symbol-name cname) eieio-defclass-autoload-map)))
	(when sym
	  (condition-case nil
	      (aset newc class-children (symbol-value sym))
	    (error nil))
	  (unintern (symbol-name cname) eieio-defclass-autoload-map)
	  ))
      )

    (cond ((and (stringp (car options-and-doc))
		(/= 1 (% (length options-and-doc) 2)))
	   (error "Too many arguments to `defclass'"))
	  ((and (symbolp (car options-and-doc))
		(/= 0 (% (length options-and-doc) 2)))
	   (error "Too many arguments to `defclass'"))
	  )

    (setq options
	  (if (stringp (car options-and-doc))
	      (cons :documentation options-and-doc)
	    options-and-doc))

    (if pname
	(progn
	  (while pname
	    (if (and (car pname) (symbolp (car pname)))
		(if (not (class-p (car pname)))
		    ;; bad class
		    (error "Given parent class %s is not a class" (car pname))
		  ;; good parent class...
		  ;; save new child in parent
		  (when (not (member cname (aref (class-v (car pname)) class-children)))
		    (aset (class-v (car pname)) class-children
			  (cons cname (aref (class-v (car pname)) class-children))))
		  ;; Get custom groups, and store them into our local copy.
		  (mapc (lambda (g) (add-to-list 'groups g))
			(class-option (car pname) :custom-groups))
		  ;; save parent in child
		  (aset newc class-parent (cons (car pname) (aref newc class-parent))))
	      (error "Invalid parent class %s" pname))
	    (setq pname (cdr pname)))
	  ;; Reverse the list of our parents so that they are prioritized in
	  ;; the same order as specified in the code.
	  (aset newc class-parent (nreverse (aref newc class-parent))) )
      ;; If there is nothing to loop over, then inherit from the
      ;; default superclass.
      (unless (eq cname 'eieio-default-superclass)
	;; adopt the default parent here, but clear it later...
	(setq clearparent t)
	;; save new child in parent
	(if (not (member cname (aref (class-v 'eieio-default-superclass) class-children)))
	    (aset (class-v 'eieio-default-superclass) class-children
		  (cons cname (aref (class-v 'eieio-default-superclass) class-children))))
	;; save parent in child
	(aset newc class-parent (list eieio-default-superclass))))

    ;; turn this into a usable self-pointing symbol
    (set cname cname)

    ;; These two tests must be created right away so we can have self-
    ;; referencing classes.  ei, a class whose slot can contain only
    ;; pointers to itself.

    ;; Create the test function
    (let ((csym (intern (concat (symbol-name cname) "-p"))))
      (fset csym
	    (list 'lambda (list 'obj)
		  (format "Test OBJ to see if it an object of type %s" cname)
		  (list 'and '(eieio-object-p obj)
			(list 'same-class-p 'obj cname)))))

    ;; Make sure the method invocation order  is a valid value.
    (let ((io (class-option-assoc options :method-invocation-order)))
      (when (and io (not (member io '(:depth-first :breadth-first :c3))))
	(error "Method invocation order %s is not allowed" io)
	))

    ;; Create a handy child test too
    (let ((csym (intern (concat (symbol-name cname) "-child-p"))))
      (fset csym
	    `(lambda (obj)
	       ,(format
		  "Test OBJ to see if it an object is a child of type %s"
		  cname)
	       (and (eieio-object-p obj)
		    (object-of-class-p obj ,cname))))

      ;; When using typep, (typep OBJ 'myclass) returns t for objects which
      ;; are subclasses of myclass.  For our predicates, however, it is
      ;; important for EIEIO to be backwards compatible, where
      ;; myobject-p, and myobject-child-p are different.
      ;; "cl" uses this technique to specify symbols with specific typep
      ;; test, so we can let typep have the CLOS documented behavior
      ;; while keeping our above predicate clean.

      ;; It would be cleaner to use `defsetf' here, but that requires cl
      ;; at runtime.
      (put cname 'cl-deftype-handler
	   (list 'lambda () `(list 'satisfies (quote ,csym)))))

    ;; before adding new slots, let's add all the methods and classes
    ;; in from the parent class
    (eieio-copy-parents-into-subclass newc superclasses)

    ;; Store the new class vector definition into the symbol.  We need to
    ;; do this first so that we can call defmethod for the accessor.
    ;; The vector will be updated by the following while loop and will not
    ;; need to be stored a second time.
    (put cname 'eieio-class-definition newc)

    ;; Query each slot in the declaration list and mangle into the
    ;; class structure I have defined.
    (while slots
      (let* ((slot1  (car slots))
	     (name    (car slot1))
	     (slot   (cdr slot1))
	     (acces   (plist-get slot ':accessor))
	     (init    (or (plist-get slot ':initform)
			  (if (member ':initform slot) nil
			    eieio-unbound)))
	     (initarg (plist-get slot ':initarg))
	     (docstr  (plist-get slot ':documentation))
	     (prot    (plist-get slot ':protection))
	     (reader  (plist-get slot ':reader))
	     (writer  (plist-get slot ':writer))
	     (alloc   (plist-get slot ':allocation))
	     (type    (plist-get slot ':type))
	     (custom  (plist-get slot ':custom))
	     (label   (plist-get slot ':label))
	     (customg (plist-get slot ':group))
	     (printer (plist-get slot ':printer))

	     (skip-nil (class-option-assoc options :allow-nil-initform))
	     )

	(if eieio-error-unsupported-class-tags
	    (let ((tmp slot))
	      (while tmp
		(if (not (member (car tmp) '(:accessor
					     :initform
					     :initarg
					     :documentation
					     :protection
					     :reader
					     :writer
					     :allocation
					     :type
					     :custom
					     :label
					     :group
					     :printer
					     :allow-nil-initform
					     :custom-groups)))
		    (signal 'invalid-slot-type (list (car tmp))))
		(setq tmp (cdr (cdr tmp))))))

	;; Clean up the meaning of protection.
	(cond ((or (eq prot 'public) (eq prot :public)) (setq prot nil))
	      ((or (eq prot 'protected) (eq prot :protected)) (setq prot 'protected))
	      ((or (eq prot 'private) (eq prot :private)) (setq prot 'private))
	      ((eq prot nil) nil)
	      (t (signal 'invalid-slot-type (list ':protection prot))))

	;; Make sure the :allocation parameter has a valid value.
	(if (not (or (not alloc) (eq alloc :class) (eq alloc :instance)))
	    (signal 'invalid-slot-type (list ':allocation alloc)))

	;; The default type specifier is supposed to be t, meaning anything.
	(if (not type) (setq type t))

	;; Label is nil, or a string
	(if (not (or (null label) (stringp label)))
	    (signal 'invalid-slot-type (list ':label label)))

	;; Is there an initarg, but allocation of class?
	(if (and initarg (eq alloc :class))
	    (message "Class allocated slots do not need :initarg"))

	;; intern the symbol so we can use it blankly
	(if initarg (set initarg initarg))

	;; The customgroup should be a list of symbols
	(cond ((null customg)
	       (setq customg '(default)))
	      ((not (listp customg))
	       (setq customg (list customg))))
	;; The customgroup better be a symbol, or list of symbols.
	(mapc (lambda (cg)
		(if (not (symbolp cg))
		    (signal 'invalid-slot-type (list ':group cg))))
		customg)

	;; First up, add this slot into our new class.
	(eieio-add-new-slot newc name init docstr type custom label customg printer
			     prot initarg alloc 'defaultoverride skip-nil)

	;; We need to id the group, and store them in a group list attribute.
	(mapc (lambda (cg) (add-to-list 'groups cg)) customg)

	;; anyone can have an accessor function.  This creates a function
	;; of the specified name, and also performs a `defsetf' if applicable
	;; so that users can `setf' the space returned by this function
	(if acces
	    (progn
	      (eieio--defmethod
               acces (if (eq alloc :class) :static :primary) cname
               `(lambda (this)
                  ,(format
		       "Retrieves the slot `%s' from an object of class `%s'"
		       name cname)
                  (if (slot-boundp this ',name)
                      (eieio-oref this ',name)
			    ;; Else - Some error?  nil?
			    nil)))

	      ;; Provide a setf method.  It would be cleaner to use
	      ;; defsetf, but that would require CL at runtime.
	      (put acces 'setf-method
		   `(lambda (widget)
		      (let* ((--widget-sym-- (make-symbol "--widget--"))
			     (--store-sym-- (make-symbol "--store--")))
			(list
			 (list --widget-sym--)
			 (list widget)
			 (list --store-sym--)
			 (list 'eieio-oset --widget-sym-- '',name --store-sym--)
			 (list 'getfoo --widget-sym--)))))))

	;; If a writer is defined, then create a generic method of that
	;; name whose purpose is to set the value of the slot.
	(if writer
            (eieio--defmethod
             writer nil cname
             `(lambda (this value)
                ,(format "Set the slot `%s' of an object of class `%s'"
			      name cname)
                (setf (slot-value this ',name) value))))
	;; If a reader is defined, then create a generic method
	;; of that name whose purpose is to access this slot value.
	(if reader
            (eieio--defmethod
             reader nil cname
             `(lambda (this)
                ,(format "Access the slot `%s' from object of class `%s'"
			      name cname)
                (slot-value this ',name))))
	)
      (setq slots (cdr slots)))

    ;; Now that everything has been loaded up, all our lists are backwards!  Fix that up now.
    (aset newc class-public-a (nreverse (aref newc class-public-a)))
    (aset newc class-public-d (nreverse (aref newc class-public-d)))
    (aset newc class-public-doc (nreverse (aref newc class-public-doc)))
    (aset newc class-public-type
	  (apply 'vector (nreverse (aref newc class-public-type))))
    (aset newc class-public-custom (nreverse (aref newc class-public-custom)))
    (aset newc class-public-custom-label (nreverse (aref newc class-public-custom-label)))
    (aset newc class-public-custom-group (nreverse (aref newc class-public-custom-group)))
    (aset newc class-public-printer (nreverse (aref newc class-public-printer)))
    (aset newc class-protection (nreverse (aref newc class-protection)))
    (aset newc class-initarg-tuples (nreverse (aref newc class-initarg-tuples)))

    ;; The storage for class-class-allocation-type needs to be turned into
    ;; a vector now.
    (aset newc class-class-allocation-type
	  (apply 'vector (aref newc class-class-allocation-type)))

    ;; Also, take class allocated values, and vectorize them for speed.
    (aset newc class-class-allocation-values
	  (apply 'vector (aref newc class-class-allocation-values)))

    ;; Attach slot symbols into an obarray, and store the index of
    ;; this slot as the variable slot in this new symbol.  We need to
    ;; know about primes, because obarrays are best set in vectors of
    ;; prime number length, and we also need to make our vector small
    ;; to save space, and also optimal for the number of items we have.
    (let* ((cnt 0)
	   (pubsyms (aref newc class-public-a))
	   (prots (aref newc class-protection))
	   (l (length pubsyms))
	   (vl (let ((primes '( 3 5 7 11 13 17 19 23 29 31 37 41 43 47
				  53 59 61 67 71 73 79 83 89 97 101 )))
		 (while (and primes (< (car primes) l))
		   (setq primes (cdr primes)))
		 (car primes)))
	   (oa (make-vector vl 0))
	   (newsym))
      (while pubsyms
	(setq newsym (intern (symbol-name (car pubsyms)) oa))
	(set newsym cnt)
	(setq cnt (1+ cnt))
	(if (car prots) (put newsym 'protection (car prots)))
	(setq pubsyms (cdr pubsyms)
	      prots (cdr prots)))
      (aset newc class-symbol-obarray oa)
      )

    ;; Create the constructor function
    (if (class-option-assoc options :abstract)
	;; Abstract classes cannot be instantiated.  Say so.
	(let ((abs (class-option-assoc options :abstract)))
	  (if (not (stringp abs))
	      (setq abs (format "Class %s is abstract" cname)))
	  (fset cname
		`(lambda (&rest stuff)
		   ,(format "You cannot create a new object of type %s" cname)
		   (error ,abs))))

      ;; Non-abstract classes need a constructor.
      (fset cname
	    `(lambda (newname &rest slots)
	       ,(format "Create a new object with name NAME of class type %s" cname)
	       (apply 'constructor ,cname newname slots)))
      )

    ;; Set up a specialized doc string.
    ;; Use stored value since it is calculated in a non-trivial way
    (put cname 'variable-documentation
	 (class-option-assoc options :documentation))

    ;; We have a list of custom groups.  Store them into the options.
    (let ((g (class-option-assoc options :custom-groups)))
      (mapc (lambda (cg) (add-to-list 'g cg)) groups)
      (if (memq :custom-groups options)
	  (setcar (cdr (memq :custom-groups options)) g)
	(setq options (cons :custom-groups (cons g options)))))

    ;; Set up the options we have collected.
    (aset newc class-options options)

    ;; if this is a superclass, clear out parent (which was set to the
    ;; default superclass eieio-default-superclass)
    (if clearparent (aset newc class-parent nil))

    ;; Create the cached default object.
    (let ((cache (make-vector (+ (length (aref newc class-public-a))
				 3) nil)))
      (aset cache 0 'object)
      (aset cache object-class cname)
      (aset cache object-name 'default-cache-object)
      (let ((eieio-skip-typecheck t))
	;; All type-checking has been done to our satisfaction
	;; before this call.  Don't waste our time in this call..
	(eieio-set-defaults cache t))
      (aset newc class-default-object-cache cache))

    ;; Return our new class object
    ;; newc
    cname
    ))

(defun eieio-perform-slot-validation-for-default (slot spec value skipnil)
  "For SLOT, signal if SPEC does not match VALUE.
If SKIPNIL is non-nil, then if VALUE is nil return t instead."
  (if (and (not (eieio-eval-default-p value))
	   (not eieio-skip-typecheck)
	   (not (and skipnil (null value)))
	   (not (eieio-perform-slot-validation spec value)))
      (signal 'invalid-slot-type (list slot spec value))))

(defun eieio-add-new-slot (newc a d doc type cust label custg print prot init alloc
				 &optional defaultoverride skipnil)
  "Add into NEWC attribute A.
If A already exists in NEWC, then do nothing.  If it doesn't exist,
then also add in D (default), DOC, TYPE, CUST, LABEL, CUSTG, PRINT, PROT, and INIT arg.
Argument ALLOC specifies if the slot is allocated per instance, or per class.
If optional DEFAULTOVERRIDE is non-nil, then if A exists in NEWC,
we must override its value for a default.
Optional argument SKIPNIL indicates if type checking should be skipped
if default value is nil."
  ;; Make sure we duplicate those items that are sequences.
  (condition-case nil
      (if (sequencep d) (setq d (copy-sequence d)))
    ;; This copy can fail on a cons cell with a non-cons in the cdr.  Let's skip it if it doesn't work.
    (error nil))
  (if (sequencep type) (setq type (copy-sequence type)))
  (if (sequencep cust) (setq cust (copy-sequence cust)))
  (if (sequencep custg) (setq custg (copy-sequence custg)))

  ;; To prevent override information w/out specification of storage,
  ;; we need to do this little hack.
  (if (member a (aref newc class-class-allocation-a)) (setq alloc ':class))

  (if (or (not alloc) (and (symbolp alloc) (eq alloc ':instance)))
      ;; In this case, we modify the INSTANCE version of a given slot.

      (progn

	;; Only add this element if it is so-far unique
	(if (not (member a (aref newc class-public-a)))
	    (progn
	      (eieio-perform-slot-validation-for-default a type d skipnil)
	      (aset newc class-public-a (cons a (aref newc class-public-a)))
	      (aset newc class-public-d (cons d (aref newc class-public-d)))
	      (aset newc class-public-doc (cons doc (aref newc class-public-doc)))
	      (aset newc class-public-type (cons type (aref newc class-public-type)))
	      (aset newc class-public-custom (cons cust (aref newc class-public-custom)))
	      (aset newc class-public-custom-label (cons label (aref newc class-public-custom-label)))
	      (aset newc class-public-custom-group (cons custg (aref newc class-public-custom-group)))
	      (aset newc class-public-printer (cons print (aref newc class-public-printer)))
	      (aset newc class-protection (cons prot (aref newc class-protection)))
	      (aset newc class-initarg-tuples (cons (cons init a) (aref newc class-initarg-tuples)))
	      )
	  ;; When defaultoverride is true, we are usually adding new local
	  ;; attributes which must override the default value of any slot
	  ;; passed in by one of the parent classes.
	  (when defaultoverride
	    ;; There is a match, and we must override the old value.
	    (let* ((ca (aref newc class-public-a))
		   (np (member a ca))
		   (num (- (length ca) (length np)))
		   (dp (if np (nthcdr num (aref newc class-public-d))
			 nil))
		   (tp (if np (nth num (aref newc class-public-type))))
		   )
	      (if (not np)
		  (error "EIEIO internal error overriding default value for %s"
			 a)
		;; If type is passed in, is it the same?
		(if (not (eq type t))
		    (if (not (equal type tp))
			(error
			 "Child slot type `%s' does not match inherited type `%s' for `%s'"
			 type tp a)))
		;; If we have a repeat, only update the initarg...
		(unless (eq d eieio-unbound)
		  (eieio-perform-slot-validation-for-default a tp d skipnil)
		  (setcar dp d))
		;; If we have a new initarg, check for it.
		(when init
		  (let* ((inits (aref newc class-initarg-tuples))
			 (inita (rassq a inits)))
		    ;; Replace the CAR of the associate INITA.
		    ;;(message "Initarg: %S replace %s" inita init)
		    (setcar inita init)
		    ))

		;; PLN Tue Jun 26 11:57:06 2007 : The protection is
		;; checked and SHOULD match the superclass
		;; protection. Otherwise an error is thrown. However
		;; I wonder if a more flexible schedule might be
		;; implemented.
		;;
		;; EML - We used to have (if prot... here,
		;;       but a prot of 'nil means public.
		;;
		(let ((super-prot (nth num (aref newc class-protection)))
		      )
		  (if (not (eq prot super-prot))
		      (error "Child slot protection `%s' does not match inherited protection `%s' for `%s'"
			     prot super-prot a)))
		;; End original PLN

		;; PLN Tue Jun 26 11:57:06 2007 :
		;; Do a non redundant combination of ancient custom
		;; groups and new ones.
		(when custg
		  (let* ((groups
			  (nthcdr num (aref newc class-public-custom-group)))
			 (list1 (car groups))
			 (list2 (if (listp custg) custg (list custg))))
		    (if (< (length list1) (length list2))
			(setq list1 (prog1 list2 (setq list2 list1))))
		    (dolist (elt list2)
		      (unless (memq elt list1)
			(push elt list1)))
		    (setcar groups list1)))
		;;  End PLN

		;;  PLN Mon Jun 25 22:44:34 2007 : If a new cust is
		;;  set, simply replaces the old one.
		(when cust
		  ;; (message "Custom type redefined to %s" cust)
		  (setcar (nthcdr num (aref newc class-public-custom)) cust))

		;; If a new label is specified, it simply replaces
		;; the old one.
		(when label
		  ;; (message "Custom label redefined to %s" label)
		  (setcar (nthcdr num (aref newc class-public-custom-label)) label))
		;;  End PLN

		;; PLN Sat Jun 30 17:24:42 2007 : when a new
		;; doc is specified, simply replaces the old one.
		(when doc
		  ;;(message "Documentation redefined to %s" doc)
		  (setcar (nthcdr num (aref newc class-public-doc))
			  doc))
		;; End PLN

		;; If a new printer is specified, it simply replaces
		;; the old one.
		(when print
		  ;; (message "printer redefined to %s" print)
		  (setcar (nthcdr num (aref newc class-public-printer)) print))

		)))
	  ))

    ;; CLASS ALLOCATED SLOTS
    (let ((value (eieio-default-eval-maybe d)))
      (if (not (member a (aref newc class-class-allocation-a)))
	  (progn
	    (eieio-perform-slot-validation-for-default a type value skipnil)
	    ;; Here we have found a :class version of a slot.  This
	    ;; requires a very different approach.
	    (aset newc class-class-allocation-a (cons a (aref newc class-class-allocation-a)))
	    (aset newc class-class-allocation-doc (cons doc (aref newc class-class-allocation-doc)))
	    (aset newc class-class-allocation-type (cons type (aref newc class-class-allocation-type)))
	    (aset newc class-class-allocation-custom (cons cust (aref newc class-class-allocation-custom)))
	    (aset newc class-class-allocation-custom-label (cons label (aref newc class-class-allocation-custom-label)))
	    (aset newc class-class-allocation-custom-group (cons custg (aref newc class-class-allocation-custom-group)))
	    (aset newc class-class-allocation-protection (cons prot (aref newc class-class-allocation-protection)))
	    ;; Default value is stored in the 'values section, since new objects
	    ;; can't initialize from this element.
	    (aset newc class-class-allocation-values (cons value (aref newc class-class-allocation-values))))
	(when defaultoverride
	  ;; There is a match, and we must override the old value.
	  (let* ((ca (aref newc class-class-allocation-a))
		 (np (member a ca))
		 (num (- (length ca) (length np)))
		 (dp (if np
			 (nthcdr num
				 (aref newc class-class-allocation-values))
		       nil))
		 (tp (if np (nth num (aref newc class-class-allocation-type))
		       nil)))
	    (if (not np)
		(error "EIEIO internal error overriding default value for %s"
		       a)
	      ;; If type is passed in, is it the same?
	      (if (not (eq type t))
		  (if (not (equal type tp))
		      (error
		       "Child slot type `%s' does not match inherited type `%s' for `%s'"
		       type tp a)))
	      ;; EML - Note: the only reason to override a class bound slot
	      ;;       is to change the default, so allow unbound in.

	      ;; If we have a repeat, only update the value...
	      (eieio-perform-slot-validation-for-default a tp value skipnil)
	      (setcar dp value))

	    ;; PLN Tue Jun 26 11:57:06 2007 : The protection is
	    ;; checked and SHOULD match the superclass
	    ;; protection. Otherwise an error is thrown. However
	    ;; I wonder if a more flexible schedule might be
	    ;; implemented.
	    (let ((super-prot
		   (car (nthcdr num (aref newc class-class-allocation-protection)))))
	      (if (not (eq prot super-prot))
		  (error "Child slot protection `%s' does not match inherited protection `%s' for `%s'"
			 prot super-prot a)))
	    ;; Do a non redundant combination of ancient custom groups
	    ;; and new ones.
	    (when custg
	      (let* ((groups
		      (nthcdr num (aref newc class-class-allocation-custom-group)))
		     (list1 (car groups))
		     (list2 (if (listp custg) custg (list custg))))
		(if (< (length list1) (length list2))
		    (setq list1 (prog1 list2 (setq list2 list1))))
		(dolist (elt list2)
		  (unless (memq elt list1)
		    (push elt list1)))
		(setcar groups list1)))

	    ;; PLN Sat Jun 30 17:24:42 2007 : when a new
	    ;; doc is specified, simply replaces the old one.
	    (when doc
	      ;;(message "Documentation redefined to %s" doc)
	      (setcar (nthcdr num (aref newc class-class-allocation-doc))
		      doc))
	    ;; End PLN

	    ;; If a new printer is specified, it simply replaces
	    ;; the old one.
	    (when print
	      ;; (message "printer redefined to %s" print)
	      (setcar (nthcdr num (aref newc class-class-allocation-printer)) print))

	    ))
	))
    ))

(defun eieio-copy-parents-into-subclass (newc parents)
  "Copy into NEWC the slots of PARENTS.
Follow the rules of not overwriting early parents when applying to
the new child class."
  (let ((ps (aref newc class-parent))
	(sn (class-option-assoc (aref newc class-options)
				':allow-nil-initform)))
    (while ps
      ;; First, duplicate all the slots of the parent.
      (let ((pcv (class-v (car ps))))
	(let ((pa (aref pcv class-public-a))
	      (pd (aref pcv class-public-d))
	      (pdoc (aref pcv class-public-doc))
	      (ptype (aref pcv class-public-type))
	      (pcust (aref pcv class-public-custom))
	      (plabel (aref pcv class-public-custom-label))
	      (pcustg (aref pcv class-public-custom-group))
	      (printer (aref pcv class-public-printer))
	      (pprot (aref pcv class-protection))
	      (pinit (aref pcv class-initarg-tuples))
	      (i 0))
	  (while pa
	    (eieio-add-new-slot newc
				 (car pa) (car pd) (car pdoc) (aref ptype i)
				 (car pcust) (car plabel) (car pcustg)
				 (car printer)
				 (car pprot) (car-safe (car pinit)) nil nil sn)
	    ;; Increment each value.
	    (setq pa (cdr pa)
		  pd (cdr pd)
		  pdoc (cdr pdoc)
		  i (1+ i)
		  pcust (cdr pcust)
		  plabel (cdr plabel)
		  pcustg (cdr pcustg)
		  printer (cdr printer)
		  pprot (cdr pprot)
		  pinit (cdr pinit))
	    )) ;; while/let
	;; Now duplicate all the class alloc slots.
	(let ((pa (aref pcv class-class-allocation-a))
	      (pdoc (aref pcv class-class-allocation-doc))
	      (ptype (aref pcv class-class-allocation-type))
	      (pcust (aref pcv class-class-allocation-custom))
	      (plabel (aref pcv class-class-allocation-custom-label))
	      (pcustg (aref pcv class-class-allocation-custom-group))
	      (printer (aref pcv class-class-allocation-printer))
	      (pprot (aref pcv class-class-allocation-protection))
	      (pval (aref pcv class-class-allocation-values))
	      (i 0))
	  (while pa
	    (eieio-add-new-slot newc
				 (car pa) (aref pval i) (car pdoc) (aref ptype i)
				 (car pcust) (car plabel) (car pcustg)
				 (car printer)
				 (car pprot) nil ':class sn)
	    ;; Increment each value.
	    (setq pa (cdr pa)
		  pdoc (cdr pdoc)
		  pcust (cdr pcust)
		  plabel (cdr plabel)
		  pcustg (cdr pcustg)
		  printer (cdr printer)
		  pprot (cdr pprot)
		  i (1+ i))
	    ))) ;; while/let
      ;; Loop over each parent class
      (setq ps (cdr ps)))
    ))

;;; CLOS style implementation of object creators.
;;
(defun make-instance (class &rest initargs)
  "Make a new instance of CLASS based on INITARGS.
CLASS is a class symbol.  For example:

  (make-instance 'foo)

  INITARGS is a property list with keywords based on the :initarg
for each slot.  For example:

  (make-instance 'foo :slot1 value1 :slotN valueN)

Compatibility note:

If the first element of INITARGS is a string, it is used as the
name of the class.

In EIEIO, the class' constructor requires a name for use when printing.
`make-instance' in CLOS doesn't use names the way Emacs does, so the
class is used as the name slot instead when INITARGS doesn't start with
a string."
  (if (and (car initargs) (stringp (car initargs)))
      (apply (class-constructor class) initargs)
    (apply  (class-constructor class)
	    (cond ((symbolp class) (symbol-name class))
		  (t (format "%S" class)))
	    initargs)))


;;; CLOS methods and generics
;;

(put 'eieio--defalias 'byte-hunk-handler
     #'byte-compile-file-form-defalias) ;;(get 'defalias 'byte-hunk-handler)
(defun eieio--defalias (name body)
  "Like `defalias', but with less side-effects.
More specifically, it has no side-effects at all when the new function
definition is the same (`eq') as the old one."
  (unless (and (fboundp name)
               (eq (symbol-function name) body))
    (defalias name body)))

(defmacro defgeneric (method args &optional doc-string)
  "Create a generic function METHOD.
DOC-STRING is the base documentation for this class.  A generic
function has no body, as its purpose is to decide which method body
is appropriate to use.  Uses `defmethod' to create methods, and calls
`defgeneric' for you.  With this implementation the ARGS are
currently ignored.  You can use `defgeneric' to apply specialized
top level documentation to a method."
  `(eieio--defalias ',method
                    (eieio--defgeneric-init-form ',method ,doc-string)))

(defun eieio--defgeneric-init-form (method doc-string)
  "Form to use for the initial definition of a generic."
  (cond
   ((or (not (fboundp method))
        (eq 'autoload (car-safe (symbol-function method))))
    ;; Make sure the method tables are installed.
    (eieiomt-install method)
    ;; Construct the actual body of this function.
    (eieio-defgeneric-form method doc-string))
   ((generic-p method) (symbol-function method))           ;Leave it as-is.
   (t (error "You cannot create a generic/method over an existing symbol: %s"
             method))))

(defun eieio-defgeneric-form (method doc-string)
  "The lambda form that would be used as the function defined on METHOD.
All methods should call the same EIEIO function for dispatch.
DOC-STRING is the documentation attached to METHOD."
  `(lambda (&rest local-args)
     ,doc-string
     (eieio-generic-call (quote ,method) local-args)))

(defsubst eieio-defgeneric-reset-generic-form (method)
  "Setup METHOD to call the generic form."
  (let ((doc-string (documentation method)))
    (fset method (eieio-defgeneric-form method doc-string))))

(defun eieio-defgeneric-form-primary-only (method doc-string)
  "The lambda form that would be used as the function defined on METHOD.
All methods should call the same EIEIO function for dispatch.
DOC-STRING is the documentation attached to METHOD."
  `(lambda (&rest local-args)
     ,doc-string
     (eieio-generic-call-primary-only (quote ,method) local-args)))

(defsubst eieio-defgeneric-reset-generic-form-primary-only (method)
  "Setup METHOD to call the generic form."
  (let ((doc-string (documentation method)))
    (fset method (eieio-defgeneric-form-primary-only method doc-string))))

(defun eieio-defgeneric-form-primary-only-one (method doc-string
						      class
						      impl
						      )
  "The lambda form that would be used as the function defined on METHOD.
All methods should call the same EIEIO function for dispatch.
DOC-STRING is the documentation attached to METHOD.
CLASS is the class symbol needed for private method access.
IMPL is the symbol holding the method implementation."
  ;; NOTE: I tried out byte compiling this little fcn.  Turns out it
  ;; is faster to execute this for not byte-compiled.  ie, install this,
  ;; then measure calls going through here.  I wonder why.
  (require 'bytecomp)
  (let ((byte-compile-warnings nil))
    (byte-compile
     `(lambda (&rest local-args)
	,doc-string
	;; This is a cool cheat.  Usually we need to look up in the
	;; method table to find out if there is a method or not.  We can
	;; instead make that determination at load time when there is
	;; only one method.  If the first arg is not a child of the class
	;; of that one implementation, then clearly, there is no method def.
	(if (not (eieio-object-p (car local-args)))
	    ;; Not an object.  Just signal.
	    (signal 'no-method-definition
                    (list ',method local-args))

	  ;; We do have an object.  Make sure it is the right type.
	  (if ,(if (eq class eieio-default-superclass)
		   nil  ; default superclass means just an obj.  Already asked.
		 `(not (child-of-class-p (aref (car local-args) object-class)
					 ',class)))

	      ;; If not the right kind of object, call no applicable
	      (apply 'no-applicable-method (car local-args)
		     ',method local-args)

	    ;; It is ok, do the call.
	    ;; Fill in inter-call variables then evaluate the method.
	    (let ((scoped-class ',class)
		  (eieio-generic-call-next-method-list nil)
		  (eieio-generic-call-key method-primary)
		  (eieio-generic-call-methodname ',method)
		  (eieio-generic-call-arglst local-args)
		  )
	      (apply #',impl local-args)
              ;;(,impl local-args)
	      )))))))

(defsubst eieio-defgeneric-reset-generic-form-primary-only-one (method)
  "Setup METHOD to call the generic form."
  (let* ((doc-string (documentation method))
	 (M (get method 'eieio-method-tree))
	 (entry (car (aref M method-primary)))
	 )
    (fset method (eieio-defgeneric-form-primary-only-one
		  method doc-string
		  (car entry)
		  (cdr entry)
		  ))))

(defun eieio-unbind-method-implementations (method)
  "Make the generic method METHOD have no implementations.
It will leave the original generic function in place,
but remove reference to all implementations of METHOD."
  (put method 'eieio-method-tree nil)
  (put method 'eieio-method-obarray nil))

(defmacro defmethod (method &rest args)
  "Create a new METHOD through `defgeneric' with ARGS.

The optional second argument KEY is a specifier that
modifies how the method is called, including:
   :before  - Method will be called before the :primary
   :primary - The default if not specified
   :after   - Method will be called after the :primary
   :static  - First arg could be an object or class
The next argument is the ARGLIST.  The ARGLIST specifies the arguments
to the method as with `defun'.  The first argument can have a type
specifier, such as:
  ((VARNAME CLASS) ARG2 ...)
where VARNAME is the name of the local variable for the method being
created.  The CLASS is a class symbol for a class made with `defclass'.
A DOCSTRING comes after the ARGLIST, and is optional.
All the rest of the args are the BODY of the method.  A method will
return the value of the last form in the BODY.

Summary:

 (defmethod mymethod [:before | :primary | :after | :static]
                     ((typearg class-name) arg2 &optional opt &rest rest)
    \"doc-string\"
     body)"
  (let* ((key (if (keywordp (car args)) (pop args)))
	 (params (car args))
	 (arg1 (car params))
         (fargs (if (consp arg1)
                   (cons (car arg1) (cdr params))
                 params))
	 (class (if (consp arg1) (nth 1 arg1)))
         (code `(lambda ,fargs ,@(cdr args))))
    `(progn
       ;; Make sure there is a generic and the byte-compiler sees it.
       (defgeneric ,method ,args
         ,(or (documentation code)
              (format "Generically created method `%s'." method)))
       (eieio--defmethod ',method ',key ',class #',code))))

(defun eieio--defmethod (method kind argclass code)
  "Work part of the `defmethod' macro defining METHOD with ARGS."
  (let ((key
         ;; find optional keys
         (cond ((or (eq ':BEFORE kind)
                    (eq ':before kind))
                method-before)
               ((or (eq ':AFTER kind)
                    (eq ':after kind))
                method-after)
               ((or (eq ':PRIMARY kind)
                    (eq ':primary kind))
                method-primary)
               ((or (eq ':STATIC kind)
                    (eq ':static kind))
                method-static)
               ;; Primary key
               (t method-primary))))
    ;; Make sure there is a generic (when called from defclass).
    (eieio--defalias
     method (eieio--defgeneric-init-form
             method (or (documentation code)
                        (format "Generically created method `%s'." method))))
    ;; create symbol for property to bind to.  If the first arg is of
    ;; the form (varname vartype) and `vartype' is a class, then
    ;; that class will be the type symbol.  If not, then it will fall
    ;; under the type `primary' which is a non-specific calling of the
    ;; function.
    (if argclass
        (if (not (class-p argclass))
            (error "Unknown class type %s in method parameters"
                   argclass))
      (if (= key -1)
	  (signal 'wrong-type-argument (list :static 'non-class-arg)))
      ;; generics are higher
      (setq key (eieio-specialized-key-to-generic-key key)))
    ;; Put this lambda into the symbol so we can find it
    (eieiomt-add method code key argclass)
    )

  (when eieio-optimize-primary-methods-flag
    ;; Optimizing step:
    ;;
    ;; If this method, after this setup, only has primary methods, then
    ;; we can setup the generic that way.
    (if (generic-primary-only-p method)
	;; If there is only one primary method, then we can go one more
	;; optimization step.
	(if (generic-primary-only-one-p method)
	    (eieio-defgeneric-reset-generic-form-primary-only-one method)
	  (eieio-defgeneric-reset-generic-form-primary-only method))
      (eieio-defgeneric-reset-generic-form method)))

  method)

;;; Slot type validation

;; This is a hideous hack for replacing `typep' from cl-macs, to avoid
;; requiring the CL library at run-time.  It can be eliminated if/when
;; `typep' is merged into Emacs core.
(defun eieio--typep (val type)
  (if (symbolp type)
      (cond ((get type 'cl-deftype-handler)
	     (eieio--typep val (funcall (get type 'cl-deftype-handler))))
	    ((eq type t) t)
	    ((eq type 'null)   (null val))
	    ((eq type 'atom)   (atom val))
	    ((eq type 'float)  (and (numberp val) (not (integerp val))))
	    ((eq type 'real)   (numberp val))
	    ((eq type 'fixnum) (integerp val))
	    ((memq type '(character string-char)) (characterp val))
	    (t
	     (let* ((name (symbol-name type))
		    (namep (intern (concat name "p"))))
	       (if (fboundp namep)
		   (funcall `(lambda () (,namep val)))
		 (funcall `(lambda ()
			     (,(intern (concat name "-p")) val)))))))
    (cond ((get (car type) 'cl-deftype-handler)
	   (eieio--typep val (apply (get (car type) 'cl-deftype-handler)
				    (cdr type))))
	  ((memq (car type) '(integer float real number))
	   (and (eieio--typep val (car type))
		(or (memq (cadr type) '(* nil))
		    (if (consp (cadr type))
			(> val (car (cadr type)))
		      (>= val (cadr type))))
		(or (memq (caddr type) '(* nil))
		    (if (consp (car (cddr type)))
			(< val (caar (cddr type)))
		      (<= val (car (cddr type)))))))
	  ((memq (car type) '(and or not))
	   (eval (cons (car type)
		       (mapcar (lambda (x)
				 `(eieio--typep (quote ,val) (quote ,x)))
			       (cdr type)))))
	  ((memq (car type) '(member member*))
	   (memql val (cdr type)))
	  ((eq (car type) 'satisfies)
	   (funcall `(lambda () (,(cadr type) val))))
	  (t (error "Bad type spec: %s" type)))))

(defun eieio-perform-slot-validation (spec value)
  "Return non-nil if SPEC does not match VALUE."
  (or (eq spec t)			; t always passes
      (eq value eieio-unbound)		; unbound always passes
      (eieio--typep value spec)))

(defun eieio-validate-slot-value (class slot-idx value slot)
  "Make sure that for CLASS referencing SLOT-IDX, VALUE is valid.
Checks the :type specifier.
SLOT is the slot that is being checked, and is only used when throwing
an error."
  (if eieio-skip-typecheck
      nil
    ;; Trim off object IDX junk added in for the object index.
    (setq slot-idx (- slot-idx 3))
    (let ((st (aref (aref (class-v class) class-public-type) slot-idx)))
      (if (not (eieio-perform-slot-validation st value))
	  (signal 'invalid-slot-type (list class slot st value))))))

(defun eieio-validate-class-slot-value (class slot-idx value slot)
  "Make sure that for CLASS referencing SLOT-IDX, VALUE is valid.
Checks the :type specifier.
SLOT is the slot that is being checked, and is only used when throwing
an error."
  (if eieio-skip-typecheck
      nil
    (let ((st (aref (aref (class-v class) class-class-allocation-type)
		    slot-idx)))
      (if (not (eieio-perform-slot-validation st value))
	  (signal 'invalid-slot-type (list class slot st value))))))

(defun eieio-barf-if-slot-unbound (value instance slotname fn)
  "Throw a signal if VALUE is a representation of an UNBOUND slot.
INSTANCE is the object being referenced.  SLOTNAME is the offending
slot.  If the slot is ok, return VALUE.
Argument FN is the function calling this verifier."
  (if (and (eq value eieio-unbound) (not eieio-skip-typecheck))
      (slot-unbound instance (object-class instance) slotname fn)
    value))

;;; Get/Set slots in an object.
;;
(defmacro oref (obj slot)
  "Retrieve the value stored in OBJ in the slot named by SLOT.
Slot is the name of the slot when created by `defclass' or the label
created by the :initarg tag."
  `(eieio-oref ,obj (quote ,slot)))

(defun eieio-oref (obj slot)
  "Return the value in OBJ at SLOT in the object vector."
  (if (not (or (eieio-object-p obj) (class-p obj)))
      (signal 'wrong-type-argument (list '(or eieio-object-p class-p) obj)))
  (if (not (symbolp slot))
      (signal 'wrong-type-argument (list 'symbolp slot)))
  (if (class-p obj) (eieio-class-un-autoload obj))
  (let* ((class (if (class-p obj) obj (aref obj object-class)))
	 (c (eieio-slot-name-index class obj slot)))
    (if (not c)
	;; It might be missing because it is a :class allocated slot.
	;; Let's check that info out.
	(if (setq c (eieio-class-slot-name-index class slot))
	    ;; Oref that slot.
	    (aref (aref (class-v class) class-class-allocation-values) c)
	  ;; The slot-missing method is a cool way of allowing an object author
	  ;; to intercept missing slot definitions.  Since it is also the LAST
	  ;; thing called in this fn, its return value would be retrieved.
	  (slot-missing obj slot 'oref)
	  ;;(signal 'invalid-slot-name (list (object-name obj) slot))
	  )
      (if (not (eieio-object-p obj))
	  (signal 'wrong-type-argument (list 'eieio-object-p obj)))
      (eieio-barf-if-slot-unbound (aref obj c) obj slot 'oref))))

(defalias 'slot-value 'eieio-oref)
(defalias 'set-slot-value 'eieio-oset)

(defmacro oref-default (obj slot)
  "Get the default value of OBJ (maybe a class) for SLOT.
The default value is the value installed in a class with the :initform
tag.  SLOT can be the slot name, or the tag specified by the :initarg
tag in the `defclass' call."
  `(eieio-oref-default ,obj (quote ,slot)))

(defun eieio-oref-default (obj slot)
  "Do the work for the macro `oref-default' with similar parameters.
Fills in OBJ's SLOT with its default value."
  (if (not (or (eieio-object-p obj) (class-p obj))) (signal 'wrong-type-argument (list 'eieio-object-p obj)))
  (if (not (symbolp slot)) (signal 'wrong-type-argument (list 'symbolp slot)))
  (let* ((cl (if (eieio-object-p obj) (aref obj object-class) obj))
	 (c (eieio-slot-name-index cl obj slot)))
    (if (not c)
	;; It might be missing because it is a :class allocated slot.
	;; Let's check that info out.
	(if (setq c
		  (eieio-class-slot-name-index cl slot))
	    ;; Oref that slot.
	    (aref (aref (class-v cl) class-class-allocation-values)
		  c)
	  (slot-missing obj slot 'oref-default)
	  ;;(signal 'invalid-slot-name (list (class-name cl) slot))
	  )
      (eieio-barf-if-slot-unbound
       (let ((val (nth (- c 3) (aref (class-v cl) class-public-d))))
	 (eieio-default-eval-maybe val))
       obj cl 'oref-default))))

(defsubst eieio-eval-default-p (val)
  "Whether the default value VAL should be evaluated for use."
  (and (consp val) (symbolp (car val)) (fboundp (car val))))

(defun eieio-default-eval-maybe (val)
  "Check VAL, and return what `oref-default' would provide."
  (cond
   ;; Is it a function call?  If so, evaluate it.
   ((eieio-eval-default-p val)
    (eval val))
   ;;;; check for quoted things, and unquote them
   ;;((and (consp val) (eq (car val) 'quote))
   ;; (car (cdr val)))
   ;; return it verbatim
   (t val)))

;;; Object Set macros
;;
(defmacro oset (obj slot value)
  "Set the value in OBJ for slot SLOT to VALUE.
SLOT is the slot name as specified in `defclass' or the tag created
with in the :initarg slot.  VALUE can be any Lisp object."
  `(eieio-oset ,obj (quote ,slot) ,value))

(defun eieio-oset (obj slot value)
  "Do the work for the macro `oset'.
Fills in OBJ's SLOT with VALUE."
  (if (not (eieio-object-p obj)) (signal 'wrong-type-argument (list 'eieio-object-p obj)))
  (if (not (symbolp slot)) (signal 'wrong-type-argument (list 'symbolp slot)))
  (let ((c (eieio-slot-name-index (object-class-fast obj) obj slot)))
    (if (not c)
	;; It might be missing because it is a :class allocated slot.
	;; Let's check that info out.
	(if (setq c
		  (eieio-class-slot-name-index (aref obj object-class) slot))
	    ;; Oset that slot.
	    (progn
	      (eieio-validate-class-slot-value (object-class-fast obj) c value slot)
	      (aset (aref (class-v (aref obj object-class))
			  class-class-allocation-values)
		    c value))
	  ;; See oref for comment on `slot-missing'
	  (slot-missing obj slot 'oset value)
	  ;;(signal 'invalid-slot-name (list (object-name obj) slot))
	  )
      (eieio-validate-slot-value (object-class-fast obj) c value slot)
      (aset obj c value))))

(defmacro oset-default (class slot value)
  "Set the default slot in CLASS for SLOT to VALUE.
The default value is usually set with the :initform tag during class
creation.  This allows users to change the default behavior of classes
after they are created."
  `(eieio-oset-default ,class (quote ,slot) ,value))

(defun eieio-oset-default (class slot value)
  "Do the work for the macro `oset-default'.
Fills in the default value in CLASS' in SLOT with VALUE."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (symbolp slot)) (signal 'wrong-type-argument (list 'symbolp slot)))
  (let* ((scoped-class class)
	 (c (eieio-slot-name-index class nil slot)))
    (if (not c)
	;; It might be missing because it is a :class allocated slot.
	;; Let's check that info out.
	(if (setq c (eieio-class-slot-name-index class slot))
	    (progn
	      ;; Oref that slot.
	      (eieio-validate-class-slot-value class c value slot)
	      (aset (aref (class-v class) class-class-allocation-values) c
		    value))
	  (signal 'invalid-slot-name (list (class-name class) slot)))
      (eieio-validate-slot-value class c value slot)
      ;; Set this into the storage for defaults.
      (setcar (nthcdr (- c 3) (aref (class-v class) class-public-d))
	      value)
      ;; Take the value, and put it into our cache object.
      (eieio-oset (aref (class-v class) class-default-object-cache)
		  slot value)
      )))

;;; Handy CLOS macros
;;
(defmacro with-slots (spec-list object &rest body)
  "Bind SPEC-LIST lexically to slot values in OBJECT, and execute BODY.
This establishes a lexical environment for referring to the slots in
the instance named by the given slot-names as though they were
variables.  Within such a context the value of the slot can be
specified by using its slot name, as if it were a lexically bound
variable.  Both setf and setq can be used to set the value of the
slot.

SPEC-LIST is of a form similar to `let'.  For example:

  ((VAR1 SLOT1)
    SLOT2
    SLOTN
   (VARN+1 SLOTN+1))

Where each VAR is the local variable given to the associated
SLOT.  A slot specified without a variable name is given a
variable name of the same name as the slot."
  (declare (indent 2))
  ;; Transform the spec-list into a symbol-macrolet spec-list.
  (let ((mappings (mapcar (lambda (entry)
			    (let ((var  (if (listp entry) (car entry) entry))
				  (slot (if (listp entry) (cadr entry) entry)))
			      (list var `(slot-value ,object ',slot))))
			  spec-list)))
    (append (list 'symbol-macrolet mappings)
	    body)))

;;; Simple generators, and query functions.  None of these would do
;;  well embedded into an object.
;;
(defmacro object-class-fast (obj) "Return the class struct defining OBJ with no check."
  `(aref ,obj object-class))

(defun class-name (class) "Return a Lisp like symbol name for CLASS."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  ;; I think this is supposed to return a symbol, but to me CLASS is a symbol,
  ;; and I wanted a string.  Arg!
  (format "#<class %s>" (symbol-name class)))

(defun object-name (obj &optional extra)
  "Return a Lisp like symbol string for object OBJ.
If EXTRA, include that in the string returned to represent the symbol."
  (if (not (eieio-object-p obj)) (signal 'wrong-type-argument (list 'eieio-object-p obj)))
  (format "#<%s %s%s>" (symbol-name (object-class-fast obj))
	  (aref obj object-name) (or extra "")))

(defun object-name-string (obj) "Return a string which is OBJ's name."
  (if (not (eieio-object-p obj)) (signal 'wrong-type-argument (list 'eieio-object-p obj)))
  (aref obj object-name))

(defun object-set-name-string (obj name) "Set the string which is OBJ's NAME."
  (if (not (eieio-object-p obj)) (signal 'wrong-type-argument (list 'eieio-object-p obj)))
  (if (not (stringp name)) (signal 'wrong-type-argument (list 'stringp name)))
  (aset obj object-name name))

(defun object-class (obj) "Return the class struct defining OBJ."
  (if (not (eieio-object-p obj)) (signal 'wrong-type-argument (list 'eieio-object-p obj)))
  (object-class-fast obj))
(defalias 'class-of 'object-class)

(defun object-class-name (obj) "Return a Lisp like symbol name for OBJ's class."
  (if (not (eieio-object-p obj)) (signal 'wrong-type-argument (list 'eieio-object-p obj)))
  (class-name (object-class-fast obj)))

(defmacro class-parents-fast (class) "Return parent classes to CLASS with no check."
  `(aref (class-v ,class) class-parent))

(defun class-parents (class)
  "Return parent classes to CLASS.  (overload of variable).

The CLOS function `class-direct-superclasses' is aliased to this function."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (class-parents-fast class))

(defmacro class-children-fast (class) "Return child classes to CLASS with no check."
  `(aref (class-v ,class) class-children))

(defun class-children (class)
"Return child classes to CLASS.

The CLOS function `class-direct-subclasses' is aliased to this function."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (class-children-fast class))

(defun eieio-c3-candidate (class remaining-inputs)
  "Returns CLASS if it can go in the result now, otherwise nil"
  ;; Ensure CLASS is not in any position but the first in any of the
  ;; element lists of REMAINING-INPUTS.
  (and (not (let ((found nil))
	      (while (and remaining-inputs (not found))
		(setq found (member class (cdr (car remaining-inputs)))
		      remaining-inputs (cdr remaining-inputs)))
	      found))
       class))

(defun eieio-c3-merge-lists (reversed-partial-result remaining-inputs)
  "Merge REVERSED-PARTIAL-RESULT REMAINING-INPUTS in a consistent order, if possible.
If a consistent order does not exist, signal an error."
  (if (let ((tail remaining-inputs)
	    (found nil))
	(while (and tail (not found))
	  (setq found (car tail) tail (cdr tail)))
	(not found))
      ;; If all remaining inputs are empty lists, we are done.
      (nreverse reversed-partial-result)
    ;; Otherwise, we try to find the next element of the result. This
    ;; is achieved by considering the first element of each
    ;; (non-empty) input list and accepting a candidate if it is
    ;; consistent with the rests of the input lists.
    (let* ((found nil)
	   (tail remaining-inputs)
	   (next (progn
		   (while (and tail (not found))
		     (setq found (and (car tail)
				      (eieio-c3-candidate (caar tail)
							  remaining-inputs))
			   tail (cdr tail)))
		   found)))
      (if next
	  ;; The graph is consistent so far, add NEXT to result and
	  ;; merge input lists, dropping NEXT from their heads where
	  ;; applicable.
	  (eieio-c3-merge-lists
	   (cons next reversed-partial-result)
	   (mapcar (lambda (l) (if (eq (first l) next) (rest l) l))
		   remaining-inputs))
	;; The graph is inconsistent, give up
	(signal 'inconsistent-class-hierarchy (list remaining-inputs))))))

(defun eieio-class-precedence-dfs (class)
  "Return all parents of CLASS in depth-first order."
  (let* ((parents (class-parents-fast class))
	 (classes (copy-sequence
		   (apply #'append
			  (list class)
			  (or
			   (mapcar
			    (lambda (parent)
			      (cons parent
				    (eieio-class-precedence-dfs parent)))
			    parents)
			   '((eieio-default-superclass))))))
	 (tail classes))
    ;; Remove duplicates.
    (while tail
      (setcdr tail (delq (car tail) (cdr tail)))
      (setq tail (cdr tail)))
    classes))

(defun eieio-class-precedence-bfs (class)
  "Return all parents of CLASS in breadth-first order."
  (let ((result)
	(queue (or (class-parents-fast class)
		   '(eieio-default-superclass))))
    (while queue
      (let ((head (pop queue)))
	(unless (member head result)
	  (push head result)
	  (unless (eq head 'eieio-default-superclass)
	    (setq queue (append queue (or (class-parents-fast head)
					  '(eieio-default-superclass))))))))
    (cons class (nreverse result)))
  )

(defun eieio-class-precedence-c3 (class)
  "Return all parents of CLASS in c3 order."
  (let ((parents (class-parents-fast class)))
    (eieio-c3-merge-lists
     (list class)
     (append
      (or
       (mapcar
	(lambda (x)
	  (eieio-class-precedence-c3 x))
	parents)
       '((eieio-default-superclass)))
      (list parents))))
  )

(defun class-precedence-list (class)
  "Return (transitively closed) list of parents of CLASS.
The order, in which the parents are returned depends on the
method invocation orders of the involved classes."
  (if (or (null class) (eq class 'eieio-default-superclass))
      nil
    (case (class-method-invocation-order class)
      (:depth-first
       (eieio-class-precedence-dfs class))
      (:breadth-first
       (eieio-class-precedence-bfs class))
      (:c3
       (eieio-class-precedence-c3 class))))
  )

;; Official CLOS functions.
(defalias 'class-direct-superclasses 'class-parents)
(defalias 'class-direct-subclasses 'class-children)

(defmacro class-parent-fast (class) "Return first parent class to CLASS with no check."
  `(car (class-parents-fast ,class)))

(defmacro class-parent (class) "Return first parent class to CLASS.  (overload of variable)."
  `(car (class-parents ,class)))

(defmacro same-class-fast-p (obj class) "Return t if OBJ is of class-type CLASS with no error checking."
  `(eq (aref ,obj object-class) ,class))

(defun same-class-p (obj class) "Return t if OBJ is of class-type CLASS."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (eieio-object-p obj)) (signal 'wrong-type-argument (list 'eieio-object-p obj)))
  (same-class-fast-p obj class))

(defun object-of-class-p (obj class)
  "Return non-nil if OBJ is an instance of CLASS or CLASS' subclasses."
  (if (not (eieio-object-p obj)) (signal 'wrong-type-argument (list 'eieio-object-p obj)))
  ;; class will be checked one layer down
  (child-of-class-p (aref obj object-class) class))
;; Backwards compatibility
(defalias 'obj-of-class-p 'object-of-class-p)

(defun child-of-class-p (child class)
  "Return non-nil if CHILD class is a subclass of CLASS."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (if (not (class-p child)) (signal 'wrong-type-argument (list 'class-p child)))
  (let ((p nil))
    (while (and child (not (eq child class)))
      (setq p (append p (aref (class-v child) class-parent))
	    child (car p)
	    p (cdr p)))
    (if child t)))

(defun object-slots (obj)
  "Return list of slots available in OBJ."
  (if (not (eieio-object-p obj)) (signal 'wrong-type-argument (list 'eieio-object-p obj)))
  (aref (class-v (object-class-fast obj)) class-public-a))

(defun class-slot-initarg (class slot) "Fetch from CLASS, SLOT's :initarg."
  (if (not (class-p class)) (signal 'wrong-type-argument (list 'class-p class)))
  (let ((ia (aref (class-v class) class-initarg-tuples))
	(f nil))
    (while (and ia (not f))
      (if (eq (cdr (car ia)) slot)
	  (setq f (car (car ia))))
      (setq ia (cdr ia)))
    f))

;;; CLOS queries into classes and slots
;;
(defun slot-boundp (object slot)
  "Return non-nil if OBJECT's SLOT is bound.
Setting a slot's value makes it bound.  Calling `slot-makeunbound' will
make a slot unbound.
OBJECT can be an instance or a class."
  ;; Skip typechecking while retrieving this value.
  (let ((eieio-skip-typecheck t))
    ;; Return nil if the magic symbol is in there.
    (not (eq (cond
	      ((eieio-object-p object) (eieio-oref object slot))
	      ((class-p object)        (eieio-oref-default object slot))
	      (t (signal 'wrong-type-argument (list 'eieio-object-p object))))
	     eieio-unbound))))

(defun slot-makeunbound (object slot)
  "In OBJECT, make SLOT unbound."
  (eieio-oset object slot eieio-unbound))

(defun slot-exists-p (object-or-class slot)
  "Return non-nil if OBJECT-OR-CLASS has SLOT."
  (let ((cv (class-v (cond ((eieio-object-p object-or-class)
			    (object-class object-or-class))
			   ((class-p object-or-class)
			    object-or-class))
		     )))
    (or (memq slot (aref cv class-public-a))
	(memq slot (aref cv class-class-allocation-a)))
    ))

(defun find-class (symbol &optional errorp)
  "Return the class that SYMBOL represents.
If there is no class, nil is returned if ERRORP is nil.
If ERRORP is non-nil, `wrong-argument-type' is signaled."
  (if (not (class-p symbol))
      (if errorp (signal 'wrong-type-argument (list 'class-p symbol))
	nil)
    (class-v symbol)))

;;; Slightly more complex utility functions for objects
;;
(defun object-assoc (key slot list)
  "Return an object if KEY is `equal' to SLOT's value of an object in LIST.
LIST is a list of objects whose slots are searched.
Objects in LIST do not need to have a slot named SLOT, nor does
SLOT need to be bound.  If these errors occur, those objects will
be ignored."
  (if (not (listp list)) (signal 'wrong-type-argument (list 'listp list)))
  (while (and list (not (condition-case nil
			    ;; This prevents errors for missing slots.
			    (equal key (eieio-oref (car list) slot))
			  (error nil))))
    (setq list (cdr list)))
  (car list))

(defun object-assoc-list (slot list)
  "Return an association list with the contents of SLOT as the key element.
LIST must be a list of objects with SLOT in it.
This is useful when you need to do completing read on an object group."
  (if (not (listp list)) (signal 'wrong-type-argument (list 'listp list)))
  (let ((assoclist nil))
    (while list
      (setq assoclist (cons (cons (eieio-oref (car list) slot)
				  (car list))
			    assoclist))
      (setq list (cdr list)))
    (nreverse assoclist)))

(defun object-assoc-list-safe (slot list)
  "Return an association list with the contents of SLOT as the key element.
LIST must be a list of objects, but those objects do not need to have
SLOT in it.  If it does not, then that element is left out of the association
list."
  (if (not (listp list)) (signal 'wrong-type-argument (list 'listp list)))
  (let ((assoclist nil))
    (while list
      (if (slot-exists-p (car list) slot)
	  (setq assoclist (cons (cons (eieio-oref (car list) slot)
				      (car list))
				assoclist)))
      (setq list (cdr list)))
    (nreverse assoclist)))

(defun object-add-to-list (object slot item &optional append)
  "In OBJECT's SLOT, add ITEM to the list of elements.
Optional argument APPEND indicates we need to append to the list.
If ITEM already exists in the list in SLOT, then it is not added.
Comparison is done with `equal' through the `member' function call.
If SLOT is unbound, bind it to the list containing ITEM."
  (let (ov)
    ;; Find the originating list.
    (if (not (slot-boundp object slot))
	(setq ov (list item))
      (setq ov (eieio-oref object slot))
      ;; turn it into a list.
      (unless (listp ov)
	(setq ov (list ov)))
      ;; Do the combination
      (if (not (member item ov))
	  (setq ov
		(if append
		    (append ov (list item))
		  (cons item ov)))))
    ;; Set back into the slot.
    (eieio-oset object slot ov)))

(defun object-remove-from-list (object slot item)
  "In OBJECT's SLOT, remove occurrences of ITEM.
Deletion is done with `delete', which deletes by side effect,
and comparisons are done with `equal'.
If SLOT is unbound, do nothing."
  (if (not (slot-boundp object slot))
      nil
    (eieio-oset object slot (delete item (eieio-oref object slot)))))

;;; EIEIO internal search functions
;;
(defun eieio-slot-originating-class-p (start-class slot)
  "Return non-nil if START-CLASS is the first class to define SLOT.
This is for testing if `scoped-class' is the class that defines SLOT
so that we can protect private slots."
  (let ((par (class-parents start-class))
	(ret t))
    (if (not par)
	t
      (while (and par ret)
	(if (intern-soft (symbol-name slot)
			 (aref (class-v (car par))
			       class-symbol-obarray))
	    (setq ret nil))
	(setq par (cdr par)))
      ret)))

(defun eieio-slot-name-index (class obj slot)
  "In CLASS for OBJ find the index of the named SLOT.
The slot is a symbol which is installed in CLASS by the `defclass'
call.  OBJ can be nil, but if it is an object, and the slot in question
is protected, access will be allowed if OBJ is a child of the currently
`scoped-class'.
If SLOT is the value created with :initarg instead,
reverse-lookup that name, and recurse with the associated slot value."
  ;; Removed checks to outside this call
  (let* ((fsym (intern-soft (symbol-name slot)
			    (aref (class-v class)
				  class-symbol-obarray)))
	 (fsi (if (symbolp fsym) (symbol-value fsym) nil)))
    (if (integerp fsi)
	(cond
	 ((not (get fsym 'protection))
	  (+ 3 fsi))
	 ((and (eq (get fsym 'protection) 'protected)
	       scoped-class
	       (or (child-of-class-p class scoped-class)
		   (and (eieio-object-p obj)
			(child-of-class-p class (object-class obj)))))
	  (+ 3 fsi))
	 ((and (eq (get fsym 'protection) 'private)
	       (or (and scoped-class
			(eieio-slot-originating-class-p scoped-class slot))
		   eieio-initializing-object))
	  (+ 3 fsi))
	 (t nil))
      (let ((fn (eieio-initarg-to-attribute class slot)))
	(if fn (eieio-slot-name-index class obj fn) nil)))))

(defun eieio-class-slot-name-index (class slot)
  "In CLASS find the index of the named SLOT.
The slot is a symbol which is installed in CLASS by the `defclass'
call.  If SLOT is the value created with :initarg instead,
reverse-lookup that name, and recurse with the associated slot value."
  ;; This will happen less often, and with fewer slots.  Do this the
  ;; storage cheap way.
  (let* ((a (aref (class-v class) class-class-allocation-a))
	 (l1 (length a))
	 (af (memq slot a))
	 (l2 (length af)))
    ;; Slot # is length of the total list, minus the remaining list of
    ;; the found slot.
    (if af (- l1 l2))))

;;; CLOS generics internal function handling
;;
(defvar eieio-generic-call-methodname nil
  "When using `call-next-method', provides a context on how to do it.")
(defvar eieio-generic-call-arglst nil
  "When using `call-next-method', provides a context for parameters.")
(defvar eieio-generic-call-key nil
  "When using `call-next-method', provides a context for the current key.
Keys are a number representing :before, :primary, and :after methods.")
(defvar eieio-generic-call-next-method-list nil
  "When executing a PRIMARY or STATIC method, track the 'next-method'.
During executions, the list is first generated, then as each next method
is called, the next method is popped off the stack.")

(defvar eieio-pre-method-execution-hooks nil
  "*Hooks run just before a method is executed.
The hook function must accept one argument, the list of forms
about to be executed.")

(defun eieio-generic-call (method args)
  "Call METHOD with ARGS.
ARGS provides the context on which implementation to use.
This should only be called from a generic function."
  ;; We must expand our arguments first as they are always
  ;; passed in as quoted symbols
  (let ((newargs nil) (mclass nil)  (lambdas nil) (tlambdas nil) (keys nil)
	(eieio-generic-call-methodname method)
	(eieio-generic-call-arglst args)
	(firstarg nil)
	(primarymethodlist nil))
    ;; get a copy
    (setq newargs args
	  firstarg (car newargs))
    ;; Is the class passed in autoloaded?
    ;; Since class names are also constructors, they can be autoloaded
    ;; via the autoload command.  Check for this, and load them in.
    ;; It's ok if it doesn't turn out to be a class.  Probably want that
    ;; function loaded anyway.
    (if (and (symbolp firstarg)
	     (fboundp firstarg)
	     (listp (symbol-function firstarg))
	     (eq 'autoload (car (symbol-function firstarg))))
	(load (nth 1 (symbol-function firstarg))))
    ;; Determine the class to use.
    (cond ((eieio-object-p firstarg)
	   (setq mclass (object-class-fast firstarg)))
	  ((class-p firstarg)
	   (setq mclass firstarg))
	  )
    ;; Make sure the class is a valid class
    ;; mclass can be nil (meaning a generic for should be used.
    ;; mclass cannot have a value that is not a class, however.
    (when (and (not (null mclass)) (not (class-p mclass)))
      (error "Cannot dispatch method %S on class %S"
	     method mclass)
      )
    ;; Now create a list in reverse order of all the calls we have
    ;; make in order to successfully do this right.  Rules:
    ;; 1) Only call generics if scoped-class is not defined
    ;;    This prevents multiple calls in the case of recursion
    ;; 2) Only call static if this is a static method.
    ;; 3) Only call specifics if the definition allows for them.
    ;; 4) Call in order based on :before, :primary, and :after
    (when (eieio-object-p firstarg)
      ;; Non-static calls do all this stuff.

      ;; :after methods
      (setq tlambdas
	    (if mclass
		(eieiomt-method-list method method-after mclass)
	      (list (eieio-generic-form method method-after nil)))
	    ;;(or (and mclass (eieio-generic-form method method-after mclass))
	    ;;	(eieio-generic-form method method-after nil))
	    )
      (setq lambdas (append tlambdas lambdas)
	    keys (append (make-list (length tlambdas) method-after) keys))

      ;; :primary methods
      (setq tlambdas
	    (or (and mclass (eieio-generic-form method method-primary mclass))
		(eieio-generic-form method method-primary nil)))
      (when tlambdas
	(setq lambdas (cons tlambdas lambdas)
	      keys (cons method-primary keys)
	      primarymethodlist
	      (eieiomt-method-list method method-primary mclass)))

      ;; :before methods
      (setq tlambdas
	    (if mclass
		(eieiomt-method-list method method-before mclass)
	      (list (eieio-generic-form method method-before nil)))
	    ;;(or (and mclass (eieio-generic-form method method-before mclass))
	    ;;	(eieio-generic-form method method-before nil))
	    )
      (setq lambdas (append tlambdas lambdas)
	    keys (append (make-list (length tlambdas) method-before) keys))
      )

    (if mclass
	;; For the case of a class,
	;; if there were no methods found, then there could be :static methods.
	(when (not lambdas)
	  (setq tlambdas
		(eieio-generic-form method method-static mclass))
	  (setq lambdas (cons tlambdas lambdas)
		keys (cons method-static keys)
		primarymethodlist  ;; Re-use even with bad name here
		(eieiomt-method-list method method-static mclass)))
      ;; For the case of no class (ie - mclass == nil) then there may
      ;; be a primary method.
      (setq tlambdas
	    (eieio-generic-form method method-primary nil))
      (when tlambdas
	(setq lambdas (cons tlambdas lambdas)
	      keys (cons method-primary keys)
	      primarymethodlist
	      (eieiomt-method-list method method-primary nil)))
      )

    (run-hook-with-args 'eieio-pre-method-execution-hooks
			primarymethodlist)

    ;; Now loop through all occurrences forms which we must execute
    ;; (which are happily sorted now) and execute them all!
    (let ((rval nil) (lastval nil) (rvalever nil) (found nil))
      (while lambdas
	(if (car lambdas)
	    (let* ((scoped-class (cdr (car lambdas)))
		   (eieio-generic-call-key (car keys))
		   (has-return-val
		    (or (= eieio-generic-call-key method-primary)
			(= eieio-generic-call-key method-static)))
		   (eieio-generic-call-next-method-list
		    ;; Use the cdr, as the first element is the fcn
		    ;; we are calling right now.
		    (when has-return-val (cdr primarymethodlist)))
		   )
	      (setq found t)
	      ;;(setq rval (apply (car (car lambdas)) newargs))
	      (setq lastval (apply (car (car lambdas)) newargs))
	      (when has-return-val
	      	(setq rval lastval
	      	      rvalever t))
	      ))
	(setq lambdas (cdr lambdas)
	      keys (cdr keys)))
      (if (not found)
	  (if (eieio-object-p (car args))
	      (setq rval (apply 'no-applicable-method (car args) method args)
		    rvalever t)
	    (signal
	     'no-method-definition
	     (list method args))))
      ;; Right Here... it could be that lastval is returned when
      ;; rvalever is nil.  Is that right?
      rval)))

(defun eieio-generic-call-primary-only (method args)
  "Call METHOD with ARGS for methods with only :PRIMARY implementations.
ARGS provides the context on which implementation to use.
This should only be called from a generic function.

This method is like `eieio-generic-call', but only
implementations in the :PRIMARY slot are queried.  After many
years of use, it appears that over 90% of methods in use
have :PRIMARY implementations only.  We can therefore optimize
for this common case to improve performance."
  ;; We must expand our arguments first as they are always
  ;; passed in as quoted symbols
  (let ((newargs nil) (mclass nil)  (lambdas nil)
	(eieio-generic-call-methodname method)
	(eieio-generic-call-arglst args)
	(firstarg nil)
	(primarymethodlist nil)
	)
    ;; get a copy
    (setq newargs args
	  firstarg (car newargs))

    ;; Determine the class to use.
    (cond ((eieio-object-p firstarg)
	   (setq mclass (object-class-fast firstarg)))
	  ((not firstarg)
	   (error "Method %s called on nil" method))
	  ((not (eieio-object-p firstarg))
	   (error "Primary-only method %s called on something not an object" method))
	  (t
	   (error "EIEIO Error: Improperly classified method %s as primary only"
		  method)
	  ))
    ;; Make sure the class is a valid class
    ;; mclass can be nil (meaning a generic for should be used.
    ;; mclass cannot have a value that is not a class, however.
    (when (null mclass)
      (error "Cannot dispatch method %S on class %S" method mclass)
      )

    ;; :primary methods
    (setq lambdas (eieio-generic-form method method-primary mclass))
    (setq primarymethodlist  ;; Re-use even with bad name here
	  (eieiomt-method-list method method-primary mclass))

    ;; Now loop through all occurrences forms which we must execute
    ;; (which are happily sorted now) and execute them all!
    (let* ((rval nil) (lastval nil) (rvalever nil)
	   (scoped-class (cdr lambdas))
	   (eieio-generic-call-key method-primary)
	   ;; Use the cdr, as the first element is the fcn
	   ;; we are calling right now.
	   (eieio-generic-call-next-method-list (cdr primarymethodlist))
	   )

      (if (or (not lambdas) (not (car lambdas)))

	  ;; No methods found for this impl...
	  (if (eieio-object-p (car args))
	      (setq rval (apply 'no-applicable-method (car args) method args)
		    rvalever t)
	    (signal
	     'no-method-definition
	     (list method args)))

	;; Do the regular implementation here.

	(run-hook-with-args 'eieio-pre-method-execution-hooks
			    lambdas)

	(setq lastval (apply (car lambdas) newargs))
	(setq rval lastval
	      rvalever t)
	)

      ;; Right Here... it could be that lastval is returned when
      ;; rvalever is nil.  Is that right?
      rval)))

(defun eieiomt-method-list (method key class)
  "Return an alist list of methods lambdas.
METHOD is the method name.
KEY represents either :before, or :after methods.
CLASS is the starting class to search from in the method tree.
If CLASS is nil, then an empty list of methods should be returned."
  ;; Note: eieiomt - the MT means MethodTree.  See more comments below
  ;; for the rest of the eieiomt methods.

  ;; Collect lambda expressions stored for the class and its parent
  ;; classes.
  (let (lambdas)
    (dolist (ancestor (class-precedence-list class))
      ;; Lookup the form to use for the PRIMARY object for the next level
      (let ((tmpl (eieio-generic-form method key ancestor)))
	(when (and tmpl
		   (or (not lambdas)
		       ;; This prevents duplicates coming out of the
		       ;; class method optimizer.  Perhaps we should
		       ;; just not optimize before/afters?
		       (not (member tmpl lambdas))))
	  (push tmpl lambdas))))

    ;; Return collected lambda. For :after methods, return in current
    ;; order (most general class last); Otherwise, reverse order.
    (if (eq key method-after)
	lambdas
      (nreverse lambdas))))

(defun next-method-p ()
  "Return non-nil if there is a next method.
Returns a list of lambda expressions which is the `next-method'
order."
  eieio-generic-call-next-method-list)

(defun call-next-method (&rest replacement-args)
  "Call the superclass method from a subclass method.
The superclass method is specified in the current method list,
and is called the next method.

If REPLACEMENT-ARGS is non-nil, then use them instead of
`eieio-generic-call-arglst'.  The generic arg list are the
arguments passed in at the top level.

Use `next-method-p' to find out if there is a next method to call."
  (if (not scoped-class)
      (error "`call-next-method' not called within a class specific method"))
  (if (and (/= eieio-generic-call-key method-primary)
	   (/= eieio-generic-call-key method-static))
      (error "Cannot `call-next-method' except in :primary or :static methods")
    )
  (let ((newargs (or replacement-args eieio-generic-call-arglst))
	(next (car eieio-generic-call-next-method-list))
	)
    (if (or (not next) (not (car next)))
	(apply 'no-next-method (car newargs) (cdr newargs))
      (let* ((eieio-generic-call-next-method-list
	      (cdr eieio-generic-call-next-method-list))
	     (eieio-generic-call-arglst newargs)
	     (scoped-class (cdr next))
	     (fcn (car next))
	     )
	(apply fcn newargs)
	))))

;;;
;; eieio-method-tree : eieiomt-
;;
;; Stored as eieio-method-tree in property list of a generic method
;;
;; (eieio-method-tree . [BEFORE PRIMARY AFTER
;;                       genericBEFORE genericPRIMARY genericAFTER])
;; and
;; (eieio-method-obarray . [BEFORE PRIMARY AFTER
;;                          genericBEFORE genericPRIMARY genericAFTER])
;;    where the association is a vector.
;;    (aref 0  -- all static methods.
;;    (aref 1  -- all methods classified as :before
;;    (aref 2  -- all methods classified as :primary
;;    (aref 3  -- all methods classified as :after
;;    (aref 4  -- a generic classified as :before
;;    (aref 5  -- a generic classified as :primary
;;    (aref 6  -- a generic classified as :after
;;
(defvar eieiomt-optimizing-obarray nil
  "While mapping atoms, this contain the obarray being optimized.")

(defun eieiomt-install (method-name)
  "Install the method tree, and obarray onto METHOD-NAME.
Do not do the work if they already exist."
  (let ((emtv (get method-name 'eieio-method-tree))
	(emto (get method-name 'eieio-method-obarray)))
    (if (or (not emtv) (not emto))
	(progn
	  (setq emtv (put method-name 'eieio-method-tree
			  (make-vector method-num-slots nil))
		emto (put method-name 'eieio-method-obarray
			  (make-vector method-num-slots nil)))
	  (aset emto 0 (make-vector 11 0))
	  (aset emto 1 (make-vector 11 0))
	  (aset emto 2 (make-vector 41 0))
	  (aset emto 3 (make-vector 11 0))
	  ))))

(defun eieiomt-add (method-name method key class)
  "Add to METHOD-NAME the forms METHOD in a call position KEY for CLASS.
METHOD-NAME is the name created by a call to `defgeneric'.
METHOD are the forms for a given implementation.
KEY is an integer (see comment in eieio.el near this function) which
is associated with the :static :before :primary and :after tags.
It also indicates if CLASS is defined or not.
CLASS is the class this method is associated with."
  (if (or (> key method-num-slots) (< key 0))
      (error "eieiomt-add: method key error!"))
  (let ((emtv (get method-name 'eieio-method-tree))
	(emto (get method-name 'eieio-method-obarray)))
    ;; Make sure the method tables are available.
    (if (or (not emtv) (not emto))
	(error "Programmer error: eieiomt-add"))
    ;; only add new cells on if it doesn't already exist!
    (if (assq class (aref emtv key))
	(setcdr (assq class (aref emtv key)) method)
      (aset emtv key (cons (cons class method) (aref emtv key))))
    ;; Add function definition into newly created symbol, and store
    ;; said symbol in the correct obarray, otherwise use the
    ;; other array to keep this stuff
    (if (< key method-num-lists)
	(let ((nsym (intern (symbol-name class) (aref emto key))))
	  (fset nsym method)))
    ;; Now optimize the entire obarray
    (if (< key method-num-lists)
	(let ((eieiomt-optimizing-obarray (aref emto key)))
	  ;; @todo - Is this overkill?  Should we just clear the symbol?
	  (mapatoms 'eieiomt-sym-optimize eieiomt-optimizing-obarray)))
    ))

(defun eieiomt-next (class)
  "Return the next parent class for CLASS.
If CLASS is a superclass, return variable `eieio-default-superclass'.
If CLASS is variable `eieio-default-superclass' then return nil.
This is different from function `class-parent' as class parent returns
nil for superclasses.  This function performs no type checking!"
  ;; No type-checking because all calls are made from functions which
  ;; are safe and do checking for us.
  (or (class-parents-fast class)
      (if (eq class 'eieio-default-superclass)
	  nil
	'(eieio-default-superclass))))

(defun eieiomt-sym-optimize (s)
  "Find the next class above S which has a function body for the optimizer."
  ;; Set the value to nil in case there is no nearest cell.
  (set s nil)
  ;; Find the nearest cell that has a function body. If we find one,
  ;; we replace the nil from above.
  (let ((external-symbol (intern-soft (symbol-name s))))
    (catch 'done
      (dolist (ancestor (rest (class-precedence-list external-symbol)))
	(let ((ov (intern-soft (symbol-name ancestor)
			       eieiomt-optimizing-obarray)))
	  (when (fboundp ov)
	    (set s ov) ;; store ov as our next symbol
	    (throw 'done ancestor)))))))

(defun eieio-generic-form (method key class)
 "Return the lambda form belonging to METHOD using KEY based upon CLASS.
If CLASS is not a class then use `generic' instead.  If class has
no form, but has a parent class, then trace to that parent class.
The first time a form is requested from a symbol, an optimized path
is memorized for faster future use."
 (let ((emto (aref (get method 'eieio-method-obarray)
		   (if class key (eieio-specialized-key-to-generic-key key)))))
   (if (class-p class)
       ;; 1) find our symbol
       (let ((cs (intern-soft (symbol-name class) emto)))
	 (if (not cs)
	     ;; 2) If there isn't one, then make one.
	     ;;    This can be slow since it only occurs once
	     (progn
	       (setq cs (intern (symbol-name class) emto))
	       ;; 2.1) Cache its nearest neighbor with a quick optimize
	       ;;      which should only occur once for this call ever
	       (let ((eieiomt-optimizing-obarray emto))
		 (eieiomt-sym-optimize cs))))
	 ;; 3) If it's bound return this one.
	 (if (fboundp  cs)
	     (cons cs (aref (class-v class) class-symbol))
	   ;; 4) If it's not bound then this variable knows something
	   (if (symbol-value cs)
	       (progn
		 ;; 4.1) This symbol holds the next class in its value
		 (setq class (symbol-value cs)
		       cs (intern-soft (symbol-name class) emto))
		 ;; 4.2) The optimizer should always have chosen a
		 ;;      function-symbol
		 ;;(if (fboundp cs)
		 (cons cs (aref (class-v (intern (symbol-name class)))
				class-symbol))
		   ;;(error "EIEIO optimizer: erratic data loss!"))
		 )
	       ;; There never will be a funcall...
	       nil)))
     ;; for a generic call, what is a list, is the function body we want.
     (let ((emtl (aref (get method 'eieio-method-tree)
 		       (if class key (eieio-specialized-key-to-generic-key key)))))
       (if emtl
	   ;; The car of EMTL is supposed to be a class, which in this
	   ;; case is nil, so skip it.
	   (cons (cdr (car emtl)) nil)
	 nil)))))

;;;
;; Way to assign slots based on a list.  Used for constructors, or
;; even resetting an object at run-time
;;
(defun eieio-set-defaults (obj &optional set-all)
  "Take object OBJ, and reset all slots to their defaults.
If SET-ALL is non-nil, then when a default is nil, that value is
reset.  If SET-ALL is nil, the slots are only reset if the default is
not nil."
  (let ((scoped-class (aref obj object-class))
	(eieio-initializing-object t)
	(pub (aref (class-v (aref obj object-class)) class-public-a)))
    (while pub
      (let ((df (eieio-oref-default obj (car pub))))
	(if (or df set-all)
	    (eieio-oset obj (car pub) df)))
      (setq pub (cdr pub)))))

(defun eieio-initarg-to-attribute (class initarg)
  "For CLASS, convert INITARG to the actual attribute name.
If there is no translation, pass it in directly (so we can cheat if
need be... May remove that later...)"
  (let ((tuple (assoc initarg (aref (class-v class) class-initarg-tuples))))
    (if tuple
	(cdr tuple)
      nil)))

(defun eieio-attribute-to-initarg (class attribute)
  "In CLASS, convert the ATTRIBUTE into the corresponding init argument tag.
This is usually a symbol that starts with `:'."
  (let ((tuple (rassoc attribute (aref (class-v class) class-initarg-tuples))))
    (if tuple
	(car tuple)
      nil)))


;;; Here are some special types of errors
;;
(intern "no-method-definition")
(put 'no-method-definition 'error-conditions '(no-method-definition error))
(put 'no-method-definition 'error-message "No method definition")

(intern "no-next-method")
(put 'no-next-method 'error-conditions '(no-next-method error))
(put 'no-next-method 'error-message "No next method")

(intern "invalid-slot-name")
(put 'invalid-slot-name 'error-conditions '(invalid-slot-name error))
(put 'invalid-slot-name 'error-message "Invalid slot name")

(intern "invalid-slot-type")
(put 'invalid-slot-type 'error-conditions '(invalid-slot-type error nil))
(put 'invalid-slot-type 'error-message "Invalid slot type")

(intern "unbound-slot")
(put 'unbound-slot 'error-conditions '(unbound-slot error nil))
(put 'unbound-slot 'error-message "Unbound slot")

(intern "inconsistent-class-hierarchy")
(put 'inconsistent-class-hierarchy 'error-conditions
     '(inconsistent-class-hierarchy error nil))
(put 'inconsistent-class-hierarchy 'error-message "Inconsistent class hierarchy")

;;; Here are some CLOS items that need the CL package
;;

(defsetf slot-value (obj slot) (store) (list 'eieio-oset obj slot store))
(defsetf eieio-oref (obj slot) (store) (list 'eieio-oset obj slot store))

;; The below setf method was written by Arnd Kohrs <kohrs@acm.org>
(define-setf-method oref (obj slot)
  (with-no-warnings
    (require 'cl)
    (let ((obj-temp (gensym))
	  (slot-temp (gensym))
	  (store-temp (gensym)))
      (list (list obj-temp slot-temp)
	    (list obj `(quote ,slot))
	    (list store-temp)
	    (list 'set-slot-value obj-temp slot-temp
		  store-temp)
	    (list 'slot-value obj-temp slot-temp)))))


;;;
;; We want all objects created by EIEIO to have some default set of
;; behaviors so we can create object utilities, and allow various
;; types of error checking.  To do this, create the default EIEIO
;; class, and when no parent class is specified, use this as the
;; default.  (But don't store it in the other classes as the default,
;; allowing for transparent support.)
;;

(defclass eieio-default-superclass nil
  nil
  "Default parent class for classes with no specified parent class.
Its slots are automatically adopted by classes with no specified parents.
This class is not stored in the `parent' slot of a class vector."
  :abstract t)

(defalias 'standard-class 'eieio-default-superclass)

(defgeneric constructor (class newname &rest slots)
  "Default constructor for CLASS `eieio-default-superclass'.")

(defmethod constructor :static
  ((class eieio-default-superclass) newname &rest slots)
  "Default constructor for CLASS `eieio-default-superclass'.
NEWNAME is the name to be given to the constructed object.
SLOTS are the initialization slots used by `shared-initialize'.
This static method is called when an object is constructed.
It allocates the vector used to represent an EIEIO object, and then
calls `shared-initialize' on that object."
  (let* ((new-object (copy-sequence (aref (class-v class)
					  class-default-object-cache))))
    ;; Update the name for the newly created object.
    (aset new-object object-name newname)
    ;; Call the initialize method on the new object with the slots
    ;; that were passed down to us.
    (initialize-instance new-object slots)
    ;; Return the created object.
    new-object))

(defgeneric shared-initialize (obj slots)
  "Set slots of OBJ with SLOTS which is a list of name/value pairs.
Called from the constructor routine.")

(defmethod shared-initialize ((obj eieio-default-superclass) slots)
  "Set slots of OBJ with SLOTS which is a list of name/value pairs.
Called from the constructor routine."
  (let ((scoped-class (aref obj object-class)))
    (while slots
      (let ((rn (eieio-initarg-to-attribute (object-class-fast obj)
					    (car slots))))
	(if (not rn)
	    (slot-missing obj (car slots) 'oset (car (cdr slots)))
	  (eieio-oset obj rn (car (cdr slots)))))
      (setq slots (cdr (cdr slots))))))

(defgeneric initialize-instance (this &optional slots)
  "Construct the new object THIS based on SLOTS.")

(defmethod initialize-instance ((this eieio-default-superclass)
				&optional slots)
  "Construct the new object THIS based on SLOTS.
SLOTS is a tagged list where odd numbered elements are tags, and
even numbered elements are the values to store in the tagged slot.
If you overload the `initialize-instance', there you will need to
call `shared-initialize' yourself, or you can call `call-next-method'
to have this constructor called automatically.  If these steps are
not taken, then new objects of your class will not have their values
dynamically set from SLOTS."
    ;; First, see if any of our defaults are `lambda', and
    ;; re-evaluate them and apply the value to our slots.
    (let* ((scoped-class (class-v (aref this object-class)))
	   (slot (aref scoped-class class-public-a))
	   (defaults (aref scoped-class class-public-d)))
      (while slot
	;; For each slot, see if we need to evaluate it.
	;;
	;; Paul Landes said in an email:
	;; > CL evaluates it if it can, and otherwise, leaves it as
	;; > the quoted thing as you already have.  This is by the
	;; > Sonya E. Keene book and other things I've look at on the
	;; > web.
	(let ((dflt (eieio-default-eval-maybe (car defaults))))
	  (when (not (eq dflt (car defaults)))
	    (eieio-oset this (car slot) dflt) ))
	;; Next.
	(setq slot (cdr slot)
	      defaults (cdr defaults))))
    ;; Shared initialize will parse our slots for us.
    (shared-initialize this slots))

(defgeneric slot-missing (object slot-name operation &optional new-value)
  "Method invoked when an attempt to access a slot in OBJECT fails.")

(defmethod slot-missing ((object eieio-default-superclass) slot-name
			 operation &optional new-value)
  "Method invoked when an attempt to access a slot in OBJECT fails.
SLOT-NAME is the name of the failed slot, OPERATION is the type of access
that was requested, and optional NEW-VALUE is the value that was desired
to be set.

This method is called from `oref', `oset', and other functions which
directly reference slots in EIEIO objects."
  (signal 'invalid-slot-name (list (object-name object)
				   slot-name)))

(defgeneric slot-unbound (object class slot-name fn)
  "Slot unbound is invoked during an attempt to reference an unbound slot.")

(defmethod slot-unbound ((object eieio-default-superclass)
			 class slot-name fn)
  "Slot unbound is invoked during an attempt to reference an unbound slot.
OBJECT is the instance of the object being reference.  CLASS is the
class of OBJECT, and SLOT-NAME is the offending slot.  This function
throws the signal `unbound-slot'.  You can overload this function and
return the value to use in place of the unbound value.
Argument FN is the function signaling this error.
Use `slot-boundp' to determine if a slot is bound or not.

In CLOS, the argument list is (CLASS OBJECT SLOT-NAME), but
EIEIO can only dispatch on the first argument, so the first two are swapped."
  (signal 'unbound-slot (list (class-name class) (object-name object)
			      slot-name fn)))

(defgeneric no-applicable-method (object method &rest args)
  "Called if there are no implementations for OBJECT in METHOD.")

(defmethod no-applicable-method ((object eieio-default-superclass)
				 method &rest args)
  "Called if there are no implementations for OBJECT in METHOD.
OBJECT is the object which has no method implementation.
ARGS are the arguments that were passed to METHOD.

Implement this for a class to block this signal.  The return
value becomes the return value of the original method call."
  (signal 'no-method-definition (list method (object-name object)))
  )

(defgeneric no-next-method (object &rest args)
"Called from `call-next-method' when no additional methods are available.")

(defmethod no-next-method ((object eieio-default-superclass)
			   &rest args)
  "Called from `call-next-method' when no additional methods are available.
OBJECT is othe object being called on `call-next-method'.
ARGS are the arguments it is called by.
This method signals `no-next-method' by default.  Override this
method to not throw an error, and its return value becomes the
return value of `call-next-method'."
  (signal 'no-next-method (list (object-name object) args))
)

(defgeneric clone (obj &rest params)
  "Make a copy of OBJ, and then supply PARAMS.
PARAMS is a parameter list of the same form used by `initialize-instance'.

When overloading `clone', be sure to call `call-next-method'
first and modify the returned object.")

(defmethod clone ((obj eieio-default-superclass) &rest params)
  "Make a copy of OBJ, and then apply PARAMS."
  (let ((nobj (copy-sequence obj))
	(nm (aref obj object-name))
	(passname (and params (stringp (car params))))
	(num 1))
    (if params (shared-initialize nobj (if passname (cdr params) params)))
    (if (not passname)
	(save-match-data
	  (if (string-match "-\\([0-9]+\\)" nm)
	      (setq num (1+ (string-to-number (match-string 1 nm)))
		    nm (substring nm 0 (match-beginning 0))))
	  (aset nobj object-name (concat nm "-" (int-to-string num))))
      (aset nobj object-name (car params)))
    nobj))

(defgeneric destructor (this &rest params)
  "Destructor for cleaning up any dynamic links to our object.")

(defmethod destructor ((this eieio-default-superclass) &rest params)
  "Destructor for cleaning up any dynamic links to our object.
Argument THIS is the object being destroyed.  PARAMS are additional
ignored parameters."
  ;; No cleanup... yet.
  )

(defgeneric object-print (this &rest strings)
  "Pretty printer for object THIS.  Call function `object-name' with STRINGS.

It is sometimes useful to put a summary of the object into the
default #<notation> string when using EIEIO browsing tools.
Implement this method to customize the summary.")

(defmethod object-print ((this eieio-default-superclass) &rest strings)
  "Pretty printer for object THIS.  Call function `object-name' with STRINGS.
The default method for printing object THIS is to use the
function `object-name'.

It is sometimes useful to put a summary of the object into the
default #<notation> string when using EIEIO browsing tools.

Implement this function and specify STRINGS in a call to
`call-next-method' to provide additional summary information.
When passing in extra strings from child classes, always remember
to prepend a space."
  (object-name this (apply 'concat strings)))

(defvar eieio-print-depth 0
  "When printing, keep track of the current indentation depth.")

(defgeneric object-write (this &optional comment)
  "Write out object THIS to the current stream.
Optional COMMENT will add comments to the beginning of the output.")

(defmethod object-write ((this eieio-default-superclass) &optional comment)
  "Write object THIS out to the current stream.
This writes out the vector version of this object.  Complex and recursive
object are discouraged from being written.
  If optional COMMENT is non-nil, include comments when outputting
this object."
  (when comment
    (princ ";; Object ")
    (princ (object-name-string this))
    (princ "\n")
    (princ comment)
    (princ "\n"))
  (let* ((cl (object-class this))
	 (cv (class-v cl)))
    ;; Now output readable lisp to recreate this object
    ;; It should look like this:
    ;; (<constructor> <name> <slot> <slot> ... )
    ;; Each slot's slot is writen using its :writer.
    (princ (make-string (* eieio-print-depth 2) ? ))
    (princ "(")
    (princ (symbol-name (class-constructor (object-class this))))
    (princ " \"")
    (princ (object-name-string this))
    (princ "\"\n")
    ;; Loop over all the public slots
    (let ((publa (aref cv class-public-a))
	  (publd (aref cv class-public-d))
	  (publp (aref cv class-public-printer))
	  (eieio-print-depth (1+ eieio-print-depth)))
      (while publa
	(when (slot-boundp this (car publa))
	  (let ((i (class-slot-initarg cl (car publa)))
		(v (eieio-oref this (car publa)))
		)
	    (unless (or (not i) (equal v (car publd)))
	      (princ (make-string (* eieio-print-depth 2) ? ))
	      (princ (symbol-name i))
	      (princ " ")
	      (if (car publp)
		  ;; Use our public printer
		  (funcall (car publp) v)
		;; Use our generic override prin1 function.
		(eieio-override-prin1 v))
	      (princ "\n"))))
	(setq publa (cdr publa) publd (cdr publd)
	      publp (cdr publp)))
      (princ (make-string (* eieio-print-depth 2) ? )))
    (princ ")\n")))

(defun eieio-override-prin1 (thing)
  "Perform a `prin1' on THING taking advantage of object knowledge."
  (cond ((eieio-object-p thing)
	 (object-write thing))
	((listp thing)
	 (eieio-list-prin1 thing))
	((class-p thing)
	 (princ (class-name thing)))
	((symbolp thing)
	 (princ (concat "'" (symbol-name thing))))
	(t (prin1 thing))))

(defun eieio-list-prin1 (list)
  "Display LIST where list may contain objects."
  (if (not (eieio-object-p (car list)))
      (progn
	(princ "'")
	(prin1 list))
    (princ "(list ")
    (if (eieio-object-p (car list)) (princ "\n "))
    (while list
      (if (eieio-object-p (car list))
	  (object-write (car list))
	(princ "'")
	(prin1 (car list)))
      (princ " ")
      (setq list (cdr list)))
    (princ (make-string (* eieio-print-depth 2) ? ))
    (princ ")")))


;;; Unimplemented functions from CLOS
;;
(defun change-class (obj class)
  "Change the class of OBJ to type CLASS.
This may create or delete slots, but does not affect the return value
of `eq'."
  (error "EIEIO: `change-class' is unimplemented"))

)


;;; Obsolete backward compatibility functions.
;; Needed to run byte-code compiled with the EIEIO of Emacs-23.

(defun eieio-defmethod (method args)
  "Obsolete work part of an old version of the `defmethod' macro."
  (let ((key nil) (body nil) (firstarg nil) (argfix nil) (argclass nil) loopa)
    ;; find optional keys
    (setq key
	  (cond ((or (eq ':BEFORE (car args))
		     (eq ':before (car args)))
		 (setq args (cdr args))
		 method-before)
		((or (eq ':AFTER (car args))
		     (eq ':after (car args)))
		 (setq args (cdr args))
		 method-after)
		((or (eq ':PRIMARY (car args))
		     (eq ':primary (car args)))
		 (setq args (cdr args))
		 method-primary)
		((or (eq ':STATIC (car args))
		     (eq ':static (car args)))
		 (setq args (cdr args))
		 method-static)
		;; Primary key
		(t method-primary)))
    ;; get body, and fix contents of args to be the arguments of the fn.
    (setq body (cdr args)
	  args (car args))
    (setq loopa args)
    ;; Create a fixed version of the arguments
    (while loopa
      (setq argfix (cons (if (listp (car loopa)) (car (car loopa)) (car loopa))
			 argfix))
      (setq loopa (cdr loopa)))
    ;; make sure there is a generic
    (eieio-defgeneric
     method
     (if (stringp (car body))
	 (car body) (format "Generically created method `%s'." method)))
    ;; create symbol for property to bind to.  If the first arg is of
    ;; the form (varname vartype) and `vartype' is a class, then
    ;; that class will be the type symbol.  If not, then it will fall
    ;; under the type `primary' which is a non-specific calling of the
    ;; function.
    (setq firstarg (car args))
    (if (listp firstarg)
	(progn
	  (setq argclass  (nth 1 firstarg))
	  (if (not (class-p argclass))
	      (error "Unknown class type %s in method parameters"
		     (nth 1 firstarg))))
      (if (= key -1)
	  (signal 'wrong-type-argument (list :static 'non-class-arg)))
      ;; generics are higher
      (setq key (eieio-specialized-key-to-generic-key key)))
    ;; Put this lambda into the symbol so we can find it
    (if (byte-code-function-p (car-safe body))
	(eieiomt-add method (car-safe body) key argclass)
      (eieiomt-add method (append (list 'lambda (reverse argfix)) body)
		   key argclass))
    )

  (when eieio-optimize-primary-methods-flag
    ;; Optimizing step:
    ;;
    ;; If this method, after this setup, only has primary methods, then
    ;; we can setup the generic that way.
    (if (generic-primary-only-p method)
	;; If there is only one primary method, then we can go one more
	;; optimization step.
	(if (generic-primary-only-one-p method)
	    (eieio-defgeneric-reset-generic-form-primary-only-one method)
	  (eieio-defgeneric-reset-generic-form-primary-only method))
      (eieio-defgeneric-reset-generic-form method)))

  method)
(make-obsolete 'eieio-defmethod 'eieio--defmethod "24.1")

(defun eieio-defgeneric (method doc-string)
  "Obsolete work part of an old version of the `defgeneric' macro."
  (if (and (fboundp method) (not (generic-p method))
	   (or (byte-code-function-p (symbol-function method))
	       (not (eq 'autoload (car (symbol-function method)))))
	   )
      (error "You cannot create a generic/method over an existing symbol: %s"
	     method))
  ;; Don't do this over and over.
  (unless (fboundp 'method)
    ;; This defun tells emacs where the first definition of this
    ;; method is defined.
    `(defun ,method nil)
    ;; Make sure the method tables are installed.
    (eieiomt-install method)
    ;; Apply the actual body of this function.
    (fset method (eieio-defgeneric-form method doc-string))
    ;; Return the method
    'method))
(make-obsolete 'eieio-defgeneric nil "24.1")

;;; Interfacing with edebug
;;
(defun eieio-edebug-prin1-to-string (object &optional noescape)
  "Display EIEIO OBJECT in fancy format.
Overrides the edebug default.
Optional argument NOESCAPE is passed to `prin1-to-string' when appropriate."
  (cond ((class-p object) (class-name object))
	((eieio-object-p object) (object-print object))
	((and (listp object) (or (class-p (car object))
				 (eieio-object-p (car object))))
	 (concat "(" (mapconcat 'eieio-edebug-prin1-to-string object " ") ")"))
	(t (prin1-to-string object noescape))))

(add-hook 'edebug-setup-hook
	  (lambda ()
	    (def-edebug-spec defmethod
	      (&define			; this means we are defining something
	       [&or name ("setf" :name setf name)]
	       ;; ^^ This is the methods symbol
	       [ &optional symbolp ]    ; this is key :before etc
	       list              ; arguments
	       [ &optional stringp ]    ; documentation string
	       def-body	                ; part to be debugged
	       ))
	    ;; The rest of the macros
	    (def-edebug-spec oref (form quote))
	    (def-edebug-spec oref-default (form quote))
	    (def-edebug-spec oset (form quote form))
	    (def-edebug-spec oset-default (form quote form))
	    (def-edebug-spec class-v form)
	    (def-edebug-spec class-p form)
	    (def-edebug-spec eieio-object-p form)
	    (def-edebug-spec class-constructor form)
	    (def-edebug-spec generic-p form)
	    (def-edebug-spec with-slots (list list def-body))
	    ;; I suspect this isn't the best way to do this, but when
	    ;; cust-print was used on my system all my objects
	    ;; appeared as "#1 =" which was not useful.  This allows
	    ;; edebug to print my objects in the nice way they were
	    ;; meant to with `object-print' and `class-name'
	    ;; (defalias 'edebug-prin1-to-string 'eieio-edebug-prin1-to-string)
	    )
	  )

;;; Interfacing with imenu in emacs lisp mode
;;    (Only if the expression is defined)
;;
(if (eval-when-compile (boundp 'list-imenu-generic-expression))
(progn

(defun eieio-update-lisp-imenu-expression ()
  "Examine `lisp-imenu-generic-expression' and modify it to find `defmethod'."
  (let ((exp lisp-imenu-generic-expression))
    (while exp
      ;; it's of the form '( ( title expr indx ) ... )
      (let* ((subcar (cdr (car exp)))
	     (substr (car subcar)))
	(if (and (not (string-match "|method\\\\" substr))
		 (string-match "|advice\\\\" substr))
	    (setcar subcar
		    (replace-match "|advice\\|method\\" t t substr 0))))
      (setq exp (cdr exp)))))

(eieio-update-lisp-imenu-expression)

))

;;; Autoloading some external symbols, and hooking into the help system
;;


;;; Start of automatically extracted autoloads.

;;;### (autoloads (customize-object) "eieio-custom" "eieio-custom.el"
;;;;;;  "9cf80224540c52045d515a4c2c833543")
;;; Generated autoloads from eieio-custom.el

(autoload 'customize-object "eieio-custom" "\
Customize OBJ in a custom buffer.
Optional argument GROUP is the sub-group of slots to display.

\(fn OBJ &optional GROUP)" nil nil)

;;;***

;;;### (autoloads (eieio-help-mode-augmentation-maybee eieio-describe-generic
;;;;;;  eieio-describe-constructor eieio-describe-class eieio-browse)
;;;;;;  "eieio-opt" "eieio-opt.el" "e2814881441ad23759409687502f0ee1")
;;; Generated autoloads from eieio-opt.el

(autoload 'eieio-browse "eieio-opt" "\
Create an object browser window to show all objects.
If optional ROOT-CLASS, then start with that, otherwise start with
variable `eieio-default-superclass'.

\(fn &optional ROOT-CLASS)" t nil)

(defalias 'describe-class 'eieio-describe-class)

(autoload 'eieio-describe-class "eieio-opt" "\
Describe a CLASS defined by a string or symbol.
If CLASS is actually an object, then also display current values of that object.
Optional HEADERFCN should be called to insert a few bits of info first.

\(fn CLASS &optional HEADERFCN)" t nil)

(autoload 'eieio-describe-constructor "eieio-opt" "\
Describe the constructor function FCN.
Uses `eieio-describe-class' to describe the class being constructed.

\(fn FCN)" t nil)

(defalias 'describe-generic 'eieio-describe-generic)

(autoload 'eieio-describe-generic "eieio-opt" "\
Describe the generic function GENERIC.
Also extracts information about all methods specific to this generic.

\(fn GENERIC)" t nil)

(autoload 'eieio-help-mode-augmentation-maybee "eieio-opt" "\
For buffers thrown into help mode, augment for EIEIO.
Arguments UNUSED are not used.

\(fn &rest UNUSED)" nil nil)

;;;***

;;; End of automatically extracted autoloads.

(provide 'eieio)

;;; eieio ends here
