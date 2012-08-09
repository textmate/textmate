;;; cl-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (cl-prettyexpand cl-macroexpand-all cl-remprop
;;;;;;  cl-do-remf cl-set-getf getf get* tailp list-length nreconc
;;;;;;  revappend concatenate subseq cl-float-limits random-state-p
;;;;;;  make-random-state random* signum rem* mod* round* truncate*
;;;;;;  ceiling* floor* isqrt lcm gcd cl-progv-before cl-set-frame-visible-p
;;;;;;  cl-map-overlays cl-map-intervals cl-map-keymap-recursively
;;;;;;  notevery notany every some mapcon mapcan mapl maplist map
;;;;;;  cl-mapcar-many equalp coerce) "cl-extra" "cl-extra.el" "c172dda6770ce18b556561481bfefbb2")
;;; Generated autoloads from cl-extra.el

(autoload 'coerce "cl-extra" "\
Coerce OBJECT to type TYPE.
TYPE is a Common Lisp type specifier.

\(fn OBJECT TYPE)" nil nil)

(autoload 'equalp "cl-extra" "\
Return t if two Lisp objects have similar structures and contents.
This is like `equal', except that it accepts numerically equal
numbers of different types (float vs. integer), and also compares
strings case-insensitively.

\(fn X Y)" nil nil)

(autoload 'cl-mapcar-many "cl-extra" "\


\(fn CL-FUNC CL-SEQS)" nil nil)

(autoload 'map "cl-extra" "\
Map a FUNCTION across one or more SEQUENCEs, returning a sequence.
TYPE is the sequence type to return.

\(fn TYPE FUNCTION SEQUENCE...)" nil nil)

(autoload 'maplist "cl-extra" "\
Map FUNCTION to each sublist of LIST or LISTs.
Like `mapcar', except applies to lists and their cdr's rather than to
the elements themselves.

\(fn FUNCTION LIST...)" nil nil)

(autoload 'mapl "cl-extra" "\
Like `maplist', but does not accumulate values returned by the function.

\(fn FUNCTION LIST...)" nil nil)

(autoload 'mapcan "cl-extra" "\
Like `mapcar', but nconc's together the values returned by the function.

\(fn FUNCTION SEQUENCE...)" nil nil)

(autoload 'mapcon "cl-extra" "\
Like `maplist', but nconc's together the values returned by the function.

\(fn FUNCTION LIST...)" nil nil)

(autoload 'some "cl-extra" "\
Return true if PREDICATE is true of any element of SEQ or SEQs.
If so, return the true (non-nil) value returned by PREDICATE.

\(fn PREDICATE SEQ...)" nil nil)

(autoload 'every "cl-extra" "\
Return true if PREDICATE is true of every element of SEQ or SEQs.

\(fn PREDICATE SEQ...)" nil nil)

(autoload 'notany "cl-extra" "\
Return true if PREDICATE is false of every element of SEQ or SEQs.

\(fn PREDICATE SEQ...)" nil nil)

(autoload 'notevery "cl-extra" "\
Return true if PREDICATE is false of some element of SEQ or SEQs.

\(fn PREDICATE SEQ...)" nil nil)

(defalias 'cl-map-keymap 'map-keymap)

(autoload 'cl-map-keymap-recursively "cl-extra" "\


\(fn CL-FUNC-REC CL-MAP &optional CL-BASE)" nil nil)

(autoload 'cl-map-intervals "cl-extra" "\


\(fn CL-FUNC &optional CL-WHAT CL-PROP CL-START CL-END)" nil nil)

(autoload 'cl-map-overlays "cl-extra" "\


\(fn CL-FUNC &optional CL-BUFFER CL-START CL-END CL-ARG)" nil nil)

(autoload 'cl-set-frame-visible-p "cl-extra" "\


\(fn FRAME VAL)" nil nil)

(autoload 'cl-progv-before "cl-extra" "\


\(fn SYMS VALUES)" nil nil)

(autoload 'gcd "cl-extra" "\
Return the greatest common divisor of the arguments.

\(fn &rest ARGS)" nil nil)

(autoload 'lcm "cl-extra" "\
Return the least common multiple of the arguments.

\(fn &rest ARGS)" nil nil)

(autoload 'isqrt "cl-extra" "\
Return the integer square root of the argument.

\(fn X)" nil nil)

(autoload 'floor* "cl-extra" "\
Return a list of the floor of X and the fractional part of X.
With two arguments, return floor and remainder of their quotient.

\(fn X &optional Y)" nil nil)

(autoload 'ceiling* "cl-extra" "\
Return a list of the ceiling of X and the fractional part of X.
With two arguments, return ceiling and remainder of their quotient.

\(fn X &optional Y)" nil nil)

(autoload 'truncate* "cl-extra" "\
Return a list of the integer part of X and the fractional part of X.
With two arguments, return truncation and remainder of their quotient.

\(fn X &optional Y)" nil nil)

(autoload 'round* "cl-extra" "\
Return a list of X rounded to the nearest integer and the remainder.
With two arguments, return rounding and remainder of their quotient.

\(fn X &optional Y)" nil nil)

(autoload 'mod* "cl-extra" "\
The remainder of X divided by Y, with the same sign as Y.

\(fn X Y)" nil nil)

(autoload 'rem* "cl-extra" "\
The remainder of X divided by Y, with the same sign as X.

\(fn X Y)" nil nil)

(autoload 'signum "cl-extra" "\
Return 1 if X is positive, -1 if negative, 0 if zero.

\(fn X)" nil nil)

(autoload 'random* "cl-extra" "\
Return a random nonnegative number less than LIM, an integer or float.
Optional second arg STATE is a random-state object.

\(fn LIM &optional STATE)" nil nil)

(autoload 'make-random-state "cl-extra" "\
Return a copy of random-state STATE, or of `*random-state*' if omitted.
If STATE is t, return a new state object seeded from the time of day.

\(fn &optional STATE)" nil nil)

(autoload 'random-state-p "cl-extra" "\
Return t if OBJECT is a random-state object.

\(fn OBJECT)" nil nil)

(autoload 'cl-float-limits "cl-extra" "\
Initialize the Common Lisp floating-point parameters.
This sets the values of: `most-positive-float', `most-negative-float',
`least-positive-float', `least-negative-float', `float-epsilon',
`float-negative-epsilon', `least-positive-normalized-float', and
`least-negative-normalized-float'.

\(fn)" nil nil)

(autoload 'subseq "cl-extra" "\
Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end.

\(fn SEQ START &optional END)" nil nil)

(autoload 'concatenate "cl-extra" "\
Concatenate, into a sequence of type TYPE, the argument SEQUENCEs.

\(fn TYPE SEQUENCE...)" nil nil)

(autoload 'revappend "cl-extra" "\
Equivalent to (append (reverse X) Y).

\(fn X Y)" nil nil)

(autoload 'nreconc "cl-extra" "\
Equivalent to (nconc (nreverse X) Y).

\(fn X Y)" nil nil)

(autoload 'list-length "cl-extra" "\
Return the length of list X.  Return nil if list is circular.

\(fn X)" nil nil)

(autoload 'tailp "cl-extra" "\
Return true if SUBLIST is a tail of LIST.

\(fn SUBLIST LIST)" nil nil)

(autoload 'get* "cl-extra" "\
Return the value of SYMBOL's PROPNAME property, or DEFAULT if none.

\(fn SYMBOL PROPNAME &optional DEFAULT)" nil nil)

(autoload 'getf "cl-extra" "\
Search PROPLIST for property PROPNAME; return its value or DEFAULT.
PROPLIST is a list of the sort returned by `symbol-plist'.

\(fn PROPLIST PROPNAME &optional DEFAULT)" nil nil)

(autoload 'cl-set-getf "cl-extra" "\


\(fn PLIST TAG VAL)" nil nil)

(autoload 'cl-do-remf "cl-extra" "\


\(fn PLIST TAG)" nil nil)

(autoload 'cl-remprop "cl-extra" "\
Remove from SYMBOL's plist the property PROPNAME and its value.

\(fn SYMBOL PROPNAME)" nil nil)

(defalias 'remprop 'cl-remprop)

(defalias 'cl-gethash 'gethash)

(defalias 'cl-puthash 'puthash)

(defalias 'cl-remhash 'remhash)

(defalias 'cl-clrhash 'clrhash)

(defalias 'cl-maphash 'maphash)

(defalias 'cl-make-hash-table 'make-hash-table)

(defalias 'cl-hash-table-p 'hash-table-p)

(defalias 'cl-hash-table-count 'hash-table-count)

(autoload 'cl-macroexpand-all "cl-extra" "\
Expand all macro calls through a Lisp FORM.
This also does some trivial optimizations to make the form prettier.

\(fn FORM &optional ENV)" nil nil)

(autoload 'cl-prettyexpand "cl-extra" "\


\(fn FORM &optional FULL)" nil nil)

;;;***

;;;### (autoloads (defsubst* compiler-macroexpand define-compiler-macro
;;;;;;  assert check-type typep deftype cl-struct-setf-expander defstruct
;;;;;;  define-modify-macro callf2 callf letf* letf rotatef shiftf
;;;;;;  remf cl-do-pop psetf setf get-setf-method defsetf define-setf-method
;;;;;;  declare the locally multiple-value-setq multiple-value-bind
;;;;;;  lexical-let* lexical-let symbol-macrolet macrolet labels
;;;;;;  flet progv psetq do-all-symbols do-symbols dotimes dolist
;;;;;;  do* do loop return-from return block etypecase typecase ecase
;;;;;;  case load-time-value eval-when destructuring-bind function*
;;;;;;  defmacro* defun* gentemp gensym) "cl-macs" "cl-macs.el" "0be85e9c7ef309d2ccbac18b9b0f1d42")
;;; Generated autoloads from cl-macs.el

(autoload 'gensym "cl-macs" "\
Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"G\".

\(fn &optional PREFIX)" nil nil)

(autoload 'gentemp "cl-macs" "\
Generate a new interned symbol with a unique name.
The name is made by appending a number to PREFIX, default \"G\".

\(fn &optional PREFIX)" nil nil)

(autoload 'defun* "cl-macs" "\
Define NAME as a function.
Like normal `defun', except ARGLIST allows full Common Lisp conventions,
and BODY is implicitly surrounded by (block NAME ...).

\(fn NAME ARGLIST [DOCSTRING] BODY...)" nil (quote macro))

(autoload 'defmacro* "cl-macs" "\
Define NAME as a macro.
Like normal `defmacro', except ARGLIST allows full Common Lisp conventions,
and BODY is implicitly surrounded by (block NAME ...).

\(fn NAME ARGLIST [DOCSTRING] BODY...)" nil (quote macro))

(autoload 'function* "cl-macs" "\
Introduce a function.
Like normal `function', except that if argument is a lambda form,
its argument list allows full Common Lisp conventions.

\(fn FUNC)" nil (quote macro))

(autoload 'destructuring-bind "cl-macs" "\


\(fn ARGS EXPR &rest BODY)" nil (quote macro))

(autoload 'eval-when "cl-macs" "\
Control when BODY is evaluated.
If `compile' is in WHEN, BODY is evaluated when compiled at top-level.
If `load' is in WHEN, BODY is evaluated when loaded after top-level compile.
If `eval' is in WHEN, BODY is evaluated when interpreted or at non-top-level.

\(fn (WHEN...) BODY...)" nil (quote macro))

(autoload 'load-time-value "cl-macs" "\
Like `progn', but evaluates the body at load time.
The result of the body appears to the compiler as a quoted constant.

\(fn FORM &optional READ-ONLY)" nil (quote macro))

(autoload 'case "cl-macs" "\
Eval EXPR and choose among clauses on that value.
Each clause looks like (KEYLIST BODY...).  EXPR is evaluated and compared
against each key in each KEYLIST; the corresponding BODY is evaluated.
If no clause succeeds, case returns nil.  A single atom may be used in
place of a KEYLIST of one atom.  A KEYLIST of t or `otherwise' is
allowed only in the final clause, and matches if no other keys match.
Key values are compared by `eql'.

\(fn EXPR (KEYLIST BODY...)...)" nil (quote macro))

(autoload 'ecase "cl-macs" "\
Like `case', but error if no case fits.
`otherwise'-clauses are not allowed.

\(fn EXPR (KEYLIST BODY...)...)" nil (quote macro))

(autoload 'typecase "cl-macs" "\
Evals EXPR, chooses among clauses on that value.
Each clause looks like (TYPE BODY...).  EXPR is evaluated and, if it
satisfies TYPE, the corresponding BODY is evaluated.  If no clause succeeds,
typecase returns nil.  A TYPE of t or `otherwise' is allowed only in the
final clause, and matches if no other keys match.

\(fn EXPR (TYPE BODY...)...)" nil (quote macro))

(autoload 'etypecase "cl-macs" "\
Like `typecase', but error if no case fits.
`otherwise'-clauses are not allowed.

\(fn EXPR (TYPE BODY...)...)" nil (quote macro))

(autoload 'block "cl-macs" "\
Define a lexically-scoped block named NAME.
NAME may be any symbol.  Code inside the BODY forms can call `return-from'
to jump prematurely out of the block.  This differs from `catch' and `throw'
in two respects:  First, the NAME is an unevaluated symbol rather than a
quoted symbol or other form; and second, NAME is lexically rather than
dynamically scoped:  Only references to it within BODY will work.  These
references may appear inside macro expansions, but not inside functions
called from BODY.

\(fn NAME &rest BODY)" nil (quote macro))

(autoload 'return "cl-macs" "\
Return from the block named nil.
This is equivalent to `(return-from nil RESULT)'.

\(fn &optional RESULT)" nil (quote macro))

(autoload 'return-from "cl-macs" "\
Return from the block named NAME.
This jumps out to the innermost enclosing `(block NAME ...)' form,
returning RESULT from that form (or nil if RESULT is omitted).
This is compatible with Common Lisp, but note that `defun' and
`defmacro' do not create implicit blocks as they do in Common Lisp.

\(fn NAME &optional RESULT)" nil (quote macro))

(autoload 'loop "cl-macs" "\
The Common Lisp `loop' macro.
Valid clauses are:
  for VAR from/upfrom/downfrom NUM to/upto/downto/above/below NUM by NUM,
  for VAR in LIST by FUNC, for VAR on LIST by FUNC, for VAR = INIT then EXPR,
  for VAR across ARRAY, repeat NUM, with VAR = INIT, while COND, until COND,
  always COND, never COND, thereis COND, collect EXPR into VAR,
  append EXPR into VAR, nconc EXPR into VAR, sum EXPR into VAR,
  count EXPR into VAR, maximize EXPR into VAR, minimize EXPR into VAR,
  if COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...],
  unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...],
  do EXPRS..., initially EXPRS..., finally EXPRS..., return EXPR,
  finally return EXPR, named NAME.

\(fn CLAUSE...)" nil (quote macro))

(autoload 'do "cl-macs" "\
The Common Lisp `do' loop.

\(fn ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)" nil (quote macro))

(autoload 'do* "cl-macs" "\
The Common Lisp `do*' loop.

\(fn ((VAR INIT [STEP])...) (END-TEST [RESULT...]) BODY...)" nil (quote macro))

(autoload 'dolist "cl-macs" "\
Loop over a list.
Evaluate BODY with VAR bound to each `car' from LIST, in turn.
Then evaluate RESULT to get return value, default nil.
An implicit nil block is established around the loop.

\(fn (VAR LIST [RESULT]) BODY...)" nil (quote macro))

(autoload 'dotimes "cl-macs" "\
Loop a certain number of times.
Evaluate BODY with VAR bound to successive integers from 0, inclusive,
to COUNT, exclusive.  Then evaluate RESULT to get return value, default
nil.

\(fn (VAR COUNT [RESULT]) BODY...)" nil (quote macro))

(autoload 'do-symbols "cl-macs" "\
Loop over all symbols.
Evaluate BODY with VAR bound to each interned symbol, or to each symbol
from OBARRAY.

\(fn (VAR [OBARRAY [RESULT]]) BODY...)" nil (quote macro))

(autoload 'do-all-symbols "cl-macs" "\


\(fn SPEC &rest BODY)" nil (quote macro))

(autoload 'psetq "cl-macs" "\
Set SYMs to the values VALs in parallel.
This is like `setq', except that all VAL forms are evaluated (in order)
before assigning any symbols SYM to the corresponding values.

\(fn SYM VAL SYM VAL ...)" nil (quote macro))

(autoload 'progv "cl-macs" "\
Bind SYMBOLS to VALUES dynamically in BODY.
The forms SYMBOLS and VALUES are evaluated, and must evaluate to lists.
Each symbol in the first list is bound to the corresponding value in the
second list (or made unbound if VALUES is shorter than SYMBOLS); then the
BODY forms are executed and their result is returned.  This is much like
a `let' form, except that the list of symbols can be computed at run-time.

\(fn SYMBOLS VALUES &rest BODY)" nil (quote macro))

(autoload 'flet "cl-macs" "\
Make temporary function definitions.
This is an analogue of `let' that operates on the function cell of FUNC
rather than its value cell.  The FORMs are evaluated with the specified
function definitions in place, then the definitions are undone (the FUNCs
go back to their previous definitions, or lack thereof).

\(fn ((FUNC ARGLIST BODY...) ...) FORM...)" nil (quote macro))

(autoload 'labels "cl-macs" "\
Make temporary function bindings.
This is like `flet', except the bindings are lexical instead of dynamic.
Unlike `flet', this macro is fully compliant with the Common Lisp standard.

\(fn ((FUNC ARGLIST BODY...) ...) FORM...)" nil (quote macro))

(autoload 'macrolet "cl-macs" "\
Make temporary macro definitions.
This is like `flet', but for macros instead of functions.

\(fn ((NAME ARGLIST BODY...) ...) FORM...)" nil (quote macro))

(autoload 'symbol-macrolet "cl-macs" "\
Make symbol macro definitions.
Within the body FORMs, references to the variable NAME will be replaced
by EXPANSION, and (setq NAME ...) will act like (setf EXPANSION ...).

\(fn ((NAME EXPANSION) ...) FORM...)" nil (quote macro))

(autoload 'lexical-let "cl-macs" "\
Like `let', but lexically scoped.
The main visible difference is that lambdas inside BODY will create
lexical closures as in Common Lisp.

\(fn BINDINGS BODY)" nil (quote macro))

(autoload 'lexical-let* "cl-macs" "\
Like `let*', but lexically scoped.
The main visible difference is that lambdas inside BODY, and in
successive bindings within BINDINGS, will create lexical closures
as in Common Lisp.  This is similar to the behavior of `let*' in
Common Lisp.

\(fn BINDINGS BODY)" nil (quote macro))

(autoload 'multiple-value-bind "cl-macs" "\
Collect multiple return values.
FORM must return a list; the BODY is then executed with the first N elements
of this list bound (`let'-style) to each of the symbols SYM in turn.  This
is analogous to the Common Lisp `multiple-value-bind' macro, using lists to
simulate true multiple return values.  For compatibility, (values A B C) is
a synonym for (list A B C).

\(fn (SYM...) FORM BODY)" nil (quote macro))

(autoload 'multiple-value-setq "cl-macs" "\
Collect multiple return values.
FORM must return a list; the first N elements of this list are stored in
each of the symbols SYM in turn.  This is analogous to the Common Lisp
`multiple-value-setq' macro, using lists to simulate true multiple return
values.  For compatibility, (values A B C) is a synonym for (list A B C).

\(fn (SYM...) FORM)" nil (quote macro))

(autoload 'locally "cl-macs" "\


\(fn &rest BODY)" nil (quote macro))

(autoload 'the "cl-macs" "\


\(fn TYPE FORM)" nil (quote macro))

(autoload 'declare "cl-macs" "\
Declare SPECS about the current function while compiling.
For instance

  (declare (warn 0))

will turn off byte-compile warnings in the function.
See Info node `(cl)Declarations' for details.

\(fn &rest SPECS)" nil (quote macro))

(autoload 'define-setf-method "cl-macs" "\
Define a `setf' method.
This method shows how to handle `setf's to places of the form (NAME ARGS...).
The argument forms ARGS are bound according to ARGLIST, as if NAME were
going to be expanded as a macro, then the BODY forms are executed and must
return a list of five elements: a temporary-variables list, a value-forms
list, a store-variables list (of length one), a store-form, and an access-
form.  See `defsetf' for a simpler way to define most setf-methods.

\(fn NAME ARGLIST BODY...)" nil (quote macro))

(autoload 'defsetf "cl-macs" "\
Define a `setf' method.
This macro is an easy-to-use substitute for `define-setf-method' that works
well for simple place forms.  In the simple `defsetf' form, `setf's of
the form (setf (NAME ARGS...) VAL) are transformed to function or macro
calls of the form (FUNC ARGS... VAL).  Example:

  (defsetf aref aset)

Alternate form: (defsetf NAME ARGLIST (STORE) BODY...).
Here, the above `setf' call is expanded by binding the argument forms ARGS
according to ARGLIST, binding the value form VAL to STORE, then executing
BODY, which must return a Lisp form that does the necessary `setf' operation.
Actually, ARGLIST and STORE may be bound to temporary variables which are
introduced automatically to preserve proper execution order of the arguments.
Example:

  (defsetf nth (n x) (v) (list 'setcar (list 'nthcdr n x) v))

\(fn NAME [FUNC | ARGLIST (STORE) BODY...])" nil (quote macro))

(autoload 'get-setf-method "cl-macs" "\
Return a list of five values describing the setf-method for PLACE.
PLACE may be any Lisp form which can appear as the PLACE argument to
a macro like `setf' or `incf'.

\(fn PLACE &optional ENV)" nil nil)

(autoload 'setf "cl-macs" "\
Set each PLACE to the value of its VAL.
This is a generalized version of `setq'; the PLACEs may be symbolic
references such as (car x) or (aref x i), as well as plain symbols.
For example, (setf (cadar x) y) is equivalent to (setcar (cdar x) y).
The return value is the last VAL in the list.

\(fn PLACE VAL PLACE VAL ...)" nil (quote macro))

(autoload 'psetf "cl-macs" "\
Set PLACEs to the values VALs in parallel.
This is like `setf', except that all VAL forms are evaluated (in order)
before assigning any PLACEs to the corresponding values.

\(fn PLACE VAL PLACE VAL ...)" nil (quote macro))

(autoload 'cl-do-pop "cl-macs" "\


\(fn PLACE)" nil nil)

(autoload 'remf "cl-macs" "\
Remove TAG from property list PLACE.
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The form returns true if TAG was found and removed, nil otherwise.

\(fn PLACE TAG)" nil (quote macro))

(autoload 'shiftf "cl-macs" "\
Shift left among PLACEs.
Example: (shiftf A B C) sets A to B, B to C, and returns the old A.
Each PLACE may be a symbol, or any generalized variable allowed by `setf'.

\(fn PLACE... VAL)" nil (quote macro))

(autoload 'rotatef "cl-macs" "\
Rotate left among PLACEs.
Example: (rotatef A B C) sets A to B, B to C, and C to A.  It returns nil.
Each PLACE may be a symbol, or any generalized variable allowed by `setf'.

\(fn PLACE...)" nil (quote macro))

(autoload 'letf "cl-macs" "\
Temporarily bind to PLACEs.
This is the analogue of `let', but with generalized variables (in the
sense of `setf') for the PLACEs.  Each PLACE is set to the corresponding
VALUE, then the BODY forms are executed.  On exit, either normally or
because of a `throw' or error, the PLACEs are set back to their original
values.  Note that this macro is *not* available in Common Lisp.
As a special case, if `(PLACE)' is used instead of `(PLACE VALUE)',
the PLACE is not modified before executing BODY.

\(fn ((PLACE VALUE) ...) BODY...)" nil (quote macro))

(autoload 'letf* "cl-macs" "\
Temporarily bind to PLACEs.
This is the analogue of `let*', but with generalized variables (in the
sense of `setf') for the PLACEs.  Each PLACE is set to the corresponding
VALUE, then the BODY forms are executed.  On exit, either normally or
because of a `throw' or error, the PLACEs are set back to their original
values.  Note that this macro is *not* available in Common Lisp.
As a special case, if `(PLACE)' is used instead of `(PLACE VALUE)',
the PLACE is not modified before executing BODY.

\(fn ((PLACE VALUE) ...) BODY...)" nil (quote macro))

(autoload 'callf "cl-macs" "\
Set PLACE to (FUNC PLACE ARGS...).
FUNC should be an unquoted function name.  PLACE may be a symbol,
or any generalized variable allowed by `setf'.

\(fn FUNC PLACE ARGS...)" nil (quote macro))

(autoload 'callf2 "cl-macs" "\
Set PLACE to (FUNC ARG1 PLACE ARGS...).
Like `callf', but PLACE is the second argument of FUNC, not the first.

\(fn FUNC ARG1 PLACE ARGS...)" nil (quote macro))

(autoload 'define-modify-macro "cl-macs" "\
Define a `setf'-like modify macro.
If NAME is called, it combines its PLACE argument with the other arguments
from ARGLIST using FUNC: (define-modify-macro incf (&optional (n 1)) +)

\(fn NAME ARGLIST FUNC &optional DOC)" nil (quote macro))

(autoload 'defstruct "cl-macs" "\
Define a struct type.
This macro defines a new data type called NAME that stores data
in SLOTs.  It defines a `make-NAME' constructor, a `copy-NAME'
copier, a `NAME-p' predicate, and slot accessors named `NAME-SLOT'.
You can use the accessors to set the corresponding slots, via `setf'.

NAME may instead take the form (NAME OPTIONS...), where each
OPTION is either a single keyword or (KEYWORD VALUE).
See Info node `(cl)Structures' for a list of valid keywords.

Each SLOT may instead take the form (SLOT SLOT-OPTS...), where
SLOT-OPTS are keyword-value pairs for that slot.  Currently, only
one keyword is supported, `:read-only'.  If this has a non-nil
value, that slot cannot be set via `setf'.

\(fn NAME SLOTS...)" nil (quote macro))

(autoload 'cl-struct-setf-expander "cl-macs" "\


\(fn X NAME ACCESSOR PRED-FORM POS)" nil nil)

(autoload 'deftype "cl-macs" "\
Define NAME as a new data type.
The type name can then be used in `typecase', `check-type', etc.

\(fn NAME ARGLIST &rest BODY)" nil (quote macro))

(autoload 'typep "cl-macs" "\
Check that OBJECT is of type TYPE.
TYPE is a Common Lisp-style type specifier.

\(fn OBJECT TYPE)" nil nil)

(autoload 'check-type "cl-macs" "\
Verify that FORM is of type TYPE; signal an error if not.
STRING is an optional description of the desired type.

\(fn FORM TYPE &optional STRING)" nil (quote macro))

(autoload 'assert "cl-macs" "\
Verify that FORM returns non-nil; signal an error if not.
Second arg SHOW-ARGS means to include arguments of FORM in message.
Other args STRING and ARGS... are arguments to be passed to `error'.
They are not evaluated unless the assertion fails.  If STRING is
omitted, a default message listing FORM itself is used.

\(fn FORM &optional SHOW-ARGS STRING &rest ARGS)" nil (quote macro))

(autoload 'define-compiler-macro "cl-macs" "\
Define a compiler-only macro.
This is like `defmacro', but macro expansion occurs only if the call to
FUNC is compiled (i.e., not interpreted).  Compiler macros should be used
for optimizing the way calls to FUNC are compiled; the form returned by
BODY should do the same thing as a call to the normal function called
FUNC, though possibly more efficiently.  Note that, like regular macros,
compiler macros are expanded repeatedly until no further expansions are
possible.  Unlike regular macros, BODY can decide to \"punt\" and leave the
original function call alone by declaring an initial `&whole foo' parameter
and then returning foo.

\(fn FUNC ARGS &rest BODY)" nil (quote macro))

(autoload 'compiler-macroexpand "cl-macs" "\


\(fn FORM)" nil nil)

(autoload 'defsubst* "cl-macs" "\
Define NAME as a function.
Like `defun', except the function is automatically declared `inline',
ARGLIST allows full Common Lisp conventions, and BODY is implicitly
surrounded by (block NAME ...).

\(fn NAME ARGLIST [DOCSTRING] BODY...)" nil (quote macro))

;;;***

;;;### (autoloads (tree-equal nsublis sublis nsubst-if-not nsubst-if
;;;;;;  nsubst subst-if-not subst-if subsetp nset-exclusive-or set-exclusive-or
;;;;;;  nset-difference set-difference nintersection intersection
;;;;;;  nunion union rassoc-if-not rassoc-if rassoc* assoc-if-not
;;;;;;  assoc-if assoc* cl-adjoin member-if-not member-if member*
;;;;;;  merge stable-sort sort* search mismatch count-if-not count-if
;;;;;;  count position-if-not position-if position find-if-not find-if
;;;;;;  find nsubstitute-if-not nsubstitute-if nsubstitute substitute-if-not
;;;;;;  substitute-if substitute delete-duplicates remove-duplicates
;;;;;;  delete-if-not delete-if delete* remove-if-not remove-if remove*
;;;;;;  replace fill reduce) "cl-seq" "cl-seq.el" "99095e49c83af1c8bec0fdcf517b3f95")
;;; Generated autoloads from cl-seq.el

(autoload 'reduce "cl-seq" "\
Reduce two-argument FUNCTION across SEQ.

Keywords supported:  :start :end :from-end :initial-value :key

\(fn FUNCTION SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'fill "cl-seq" "\
Fill the elements of SEQ with ITEM.

Keywords supported:  :start :end

\(fn SEQ ITEM [KEYWORD VALUE]...)" nil nil)

(autoload 'replace "cl-seq" "\
Replace the elements of SEQ1 with the elements of SEQ2.
SEQ1 is destructively modified, then returned.

Keywords supported:  :start1 :end1 :start2 :end2

\(fn SEQ1 SEQ2 [KEYWORD VALUE]...)" nil nil)

(autoload 'remove* "cl-seq" "\
Remove all occurrences of ITEM in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.

Keywords supported:  :test :test-not :key :count :start :end :from-end

\(fn ITEM SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'remove-if "cl-seq" "\
Remove all items satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.

Keywords supported:  :key :count :start :end :from-end

\(fn PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'remove-if-not "cl-seq" "\
Remove all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.

Keywords supported:  :key :count :start :end :from-end

\(fn PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'delete* "cl-seq" "\
Remove all occurrences of ITEM in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.

Keywords supported:  :test :test-not :key :count :start :end :from-end

\(fn ITEM SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'delete-if "cl-seq" "\
Remove all items satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.

Keywords supported:  :key :count :start :end :from-end

\(fn PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'delete-if-not "cl-seq" "\
Remove all items not satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.

Keywords supported:  :key :count :start :end :from-end

\(fn PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'remove-duplicates "cl-seq" "\
Return a copy of SEQ with all duplicate elements removed.

Keywords supported:  :test :test-not :key :start :end :from-end

\(fn SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'delete-duplicates "cl-seq" "\
Remove all duplicate elements from SEQ (destructively).

Keywords supported:  :test :test-not :key :start :end :from-end

\(fn SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'substitute "cl-seq" "\
Substitute NEW for OLD in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.

Keywords supported:  :test :test-not :key :count :start :end :from-end

\(fn NEW OLD SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'substitute-if "cl-seq" "\
Substitute NEW for all items satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.

Keywords supported:  :key :count :start :end :from-end

\(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'substitute-if-not "cl-seq" "\
Substitute NEW for all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.

Keywords supported:  :key :count :start :end :from-end

\(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'nsubstitute "cl-seq" "\
Substitute NEW for OLD in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.

Keywords supported:  :test :test-not :key :count :start :end :from-end

\(fn NEW OLD SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'nsubstitute-if "cl-seq" "\
Substitute NEW for all items satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.

Keywords supported:  :key :count :start :end :from-end

\(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'nsubstitute-if-not "cl-seq" "\
Substitute NEW for all items not satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.

Keywords supported:  :key :count :start :end :from-end

\(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'find "cl-seq" "\
Find the first occurrence of ITEM in SEQ.
Return the matching ITEM, or nil if not found.

Keywords supported:  :test :test-not :key :start :end :from-end

\(fn ITEM SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'find-if "cl-seq" "\
Find the first item satisfying PREDICATE in SEQ.
Return the matching item, or nil if not found.

Keywords supported:  :key :start :end :from-end

\(fn PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'find-if-not "cl-seq" "\
Find the first item not satisfying PREDICATE in SEQ.
Return the matching item, or nil if not found.

Keywords supported:  :key :start :end :from-end

\(fn PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'position "cl-seq" "\
Find the first occurrence of ITEM in SEQ.
Return the index of the matching item, or nil if not found.

Keywords supported:  :test :test-not :key :start :end :from-end

\(fn ITEM SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'position-if "cl-seq" "\
Find the first item satisfying PREDICATE in SEQ.
Return the index of the matching item, or nil if not found.

Keywords supported:  :key :start :end :from-end

\(fn PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'position-if-not "cl-seq" "\
Find the first item not satisfying PREDICATE in SEQ.
Return the index of the matching item, or nil if not found.

Keywords supported:  :key :start :end :from-end

\(fn PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'count "cl-seq" "\
Count the number of occurrences of ITEM in SEQ.

Keywords supported:  :test :test-not :key :start :end

\(fn ITEM SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'count-if "cl-seq" "\
Count the number of items satisfying PREDICATE in SEQ.

Keywords supported:  :key :start :end

\(fn PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'count-if-not "cl-seq" "\
Count the number of items not satisfying PREDICATE in SEQ.

Keywords supported:  :key :start :end

\(fn PREDICATE SEQ [KEYWORD VALUE]...)" nil nil)

(autoload 'mismatch "cl-seq" "\
Compare SEQ1 with SEQ2, return index of first mismatching element.
Return nil if the sequences match.  If one sequence is a prefix of the
other, the return value indicates the end of the shorter sequence.

Keywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end

\(fn SEQ1 SEQ2 [KEYWORD VALUE]...)" nil nil)

(autoload 'search "cl-seq" "\
Search for SEQ1 as a subsequence of SEQ2.
Return the index of the leftmost element of the first match found;
return nil if there are no matches.

Keywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end

\(fn SEQ1 SEQ2 [KEYWORD VALUE]...)" nil nil)

(autoload 'sort* "cl-seq" "\
Sort the argument SEQ according to PREDICATE.
This is a destructive function; it reuses the storage of SEQ if possible.

Keywords supported:  :key

\(fn SEQ PREDICATE [KEYWORD VALUE]...)" nil nil)

(autoload 'stable-sort "cl-seq" "\
Sort the argument SEQ stably according to PREDICATE.
This is a destructive function; it reuses the storage of SEQ if possible.

Keywords supported:  :key

\(fn SEQ PREDICATE [KEYWORD VALUE]...)" nil nil)

(autoload 'merge "cl-seq" "\
Destructively merge the two sequences to produce a new sequence.
TYPE is the sequence type to return, SEQ1 and SEQ2 are the two argument
sequences, and PREDICATE is a `less-than' predicate on the elements.

Keywords supported:  :key

\(fn TYPE SEQ1 SEQ2 PREDICATE [KEYWORD VALUE]...)" nil nil)

(autoload 'member* "cl-seq" "\
Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM.

Keywords supported:  :test :test-not :key

\(fn ITEM LIST [KEYWORD VALUE]...)" nil nil)

(autoload 'member-if "cl-seq" "\
Find the first item satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.

Keywords supported:  :key

\(fn PREDICATE LIST [KEYWORD VALUE]...)" nil nil)

(autoload 'member-if-not "cl-seq" "\
Find the first item not satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.

Keywords supported:  :key

\(fn PREDICATE LIST [KEYWORD VALUE]...)" nil nil)

(autoload 'cl-adjoin "cl-seq" "\


\(fn CL-ITEM CL-LIST &rest CL-KEYS)" nil nil)

(autoload 'assoc* "cl-seq" "\
Find the first item whose car matches ITEM in LIST.

Keywords supported:  :test :test-not :key

\(fn ITEM LIST [KEYWORD VALUE]...)" nil nil)

(autoload 'assoc-if "cl-seq" "\
Find the first item whose car satisfies PREDICATE in LIST.

Keywords supported:  :key

\(fn PREDICATE LIST [KEYWORD VALUE]...)" nil nil)

(autoload 'assoc-if-not "cl-seq" "\
Find the first item whose car does not satisfy PREDICATE in LIST.

Keywords supported:  :key

\(fn PREDICATE LIST [KEYWORD VALUE]...)" nil nil)

(autoload 'rassoc* "cl-seq" "\
Find the first item whose cdr matches ITEM in LIST.

Keywords supported:  :test :test-not :key

\(fn ITEM LIST [KEYWORD VALUE]...)" nil nil)

(autoload 'rassoc-if "cl-seq" "\
Find the first item whose cdr satisfies PREDICATE in LIST.

Keywords supported:  :key

\(fn PREDICATE LIST [KEYWORD VALUE]...)" nil nil)

(autoload 'rassoc-if-not "cl-seq" "\
Find the first item whose cdr does not satisfy PREDICATE in LIST.

Keywords supported:  :key

\(fn PREDICATE LIST [KEYWORD VALUE]...)" nil nil)

(autoload 'union "cl-seq" "\
Combine LIST1 and LIST2 using a set-union operation.
The resulting list contains all items that appear in either LIST1 or LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.

Keywords supported:  :test :test-not :key

\(fn LIST1 LIST2 [KEYWORD VALUE]...)" nil nil)

(autoload 'nunion "cl-seq" "\
Combine LIST1 and LIST2 using a set-union operation.
The resulting list contains all items that appear in either LIST1 or LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.

Keywords supported:  :test :test-not :key

\(fn LIST1 LIST2 [KEYWORD VALUE]...)" nil nil)

(autoload 'intersection "cl-seq" "\
Combine LIST1 and LIST2 using a set-intersection operation.
The resulting list contains all items that appear in both LIST1 and LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.

Keywords supported:  :test :test-not :key

\(fn LIST1 LIST2 [KEYWORD VALUE]...)" nil nil)

(autoload 'nintersection "cl-seq" "\
Combine LIST1 and LIST2 using a set-intersection operation.
The resulting list contains all items that appear in both LIST1 and LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.

Keywords supported:  :test :test-not :key

\(fn LIST1 LIST2 [KEYWORD VALUE]...)" nil nil)

(autoload 'set-difference "cl-seq" "\
Combine LIST1 and LIST2 using a set-difference operation.
The resulting list contains all items that appear in LIST1 but not LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.

Keywords supported:  :test :test-not :key

\(fn LIST1 LIST2 [KEYWORD VALUE]...)" nil nil)

(autoload 'nset-difference "cl-seq" "\
Combine LIST1 and LIST2 using a set-difference operation.
The resulting list contains all items that appear in LIST1 but not LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.

Keywords supported:  :test :test-not :key

\(fn LIST1 LIST2 [KEYWORD VALUE]...)" nil nil)

(autoload 'set-exclusive-or "cl-seq" "\
Combine LIST1 and LIST2 using a set-exclusive-or operation.
The resulting list contains all items appearing in exactly one of LIST1, LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.

Keywords supported:  :test :test-not :key

\(fn LIST1 LIST2 [KEYWORD VALUE]...)" nil nil)

(autoload 'nset-exclusive-or "cl-seq" "\
Combine LIST1 and LIST2 using a set-exclusive-or operation.
The resulting list contains all items appearing in exactly one of LIST1, LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.

Keywords supported:  :test :test-not :key

\(fn LIST1 LIST2 [KEYWORD VALUE]...)" nil nil)

(autoload 'subsetp "cl-seq" "\
Return true if LIST1 is a subset of LIST2.
I.e., if every element of LIST1 also appears in LIST2.

Keywords supported:  :test :test-not :key

\(fn LIST1 LIST2 [KEYWORD VALUE]...)" nil nil)

(autoload 'subst-if "cl-seq" "\
Substitute NEW for elements matching PREDICATE in TREE (non-destructively).
Return a copy of TREE with all matching elements replaced by NEW.

Keywords supported:  :key

\(fn NEW PREDICATE TREE [KEYWORD VALUE]...)" nil nil)

(autoload 'subst-if-not "cl-seq" "\
Substitute NEW for elts not matching PREDICATE in TREE (non-destructively).
Return a copy of TREE with all non-matching elements replaced by NEW.

Keywords supported:  :key

\(fn NEW PREDICATE TREE [KEYWORD VALUE]...)" nil nil)

(autoload 'nsubst "cl-seq" "\
Substitute NEW for OLD everywhere in TREE (destructively).
Any element of TREE which is `eql' to OLD is changed to NEW (via a call
to `setcar').

Keywords supported:  :test :test-not :key

\(fn NEW OLD TREE [KEYWORD VALUE]...)" nil nil)

(autoload 'nsubst-if "cl-seq" "\
Substitute NEW for elements matching PREDICATE in TREE (destructively).
Any element of TREE which matches is changed to NEW (via a call to `setcar').

Keywords supported:  :key

\(fn NEW PREDICATE TREE [KEYWORD VALUE]...)" nil nil)

(autoload 'nsubst-if-not "cl-seq" "\
Substitute NEW for elements not matching PREDICATE in TREE (destructively).
Any element of TREE which matches is changed to NEW (via a call to `setcar').

Keywords supported:  :key

\(fn NEW PREDICATE TREE [KEYWORD VALUE]...)" nil nil)

(autoload 'sublis "cl-seq" "\
Perform substitutions indicated by ALIST in TREE (non-destructively).
Return a copy of TREE with all matching elements replaced.

Keywords supported:  :test :test-not :key

\(fn ALIST TREE [KEYWORD VALUE]...)" nil nil)

(autoload 'nsublis "cl-seq" "\
Perform substitutions indicated by ALIST in TREE (destructively).
Any matching element of TREE is changed via a call to `setcar'.

Keywords supported:  :test :test-not :key

\(fn ALIST TREE [KEYWORD VALUE]...)" nil nil)

(autoload 'tree-equal "cl-seq" "\
Return t if trees TREE1 and TREE2 have `eql' leaves.
Atoms are compared by `eql'; cons cells are compared recursively.

Keywords supported:  :test :test-not :key

\(fn TREE1 TREE2 [KEYWORD VALUE]...)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cl-loaddefs.el ends here
