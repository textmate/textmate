/* Evaluator for GNU Emacs Lisp interpreter.
   Copyright (C) 1985-1987, 1993-1995, 1999-2012  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */


#include <config.h>
#include <limits.h>
#include <setjmp.h>
#include <stdio.h>
#include "lisp.h"
#include "blockinput.h"
#include "commands.h"
#include "keyboard.h"
#include "dispextern.h"
#include "frame.h"		/* For XFRAME.  */

#if HAVE_X_WINDOWS
#include "xterm.h"
#endif

struct backtrace
{
  struct backtrace *next;
  Lisp_Object *function;
  Lisp_Object *args;	/* Points to vector of args.  */
  ptrdiff_t nargs;	/* Length of vector.  */
  /* Nonzero means call value of debugger when done with this operation.  */
  unsigned int debug_on_exit : 1;
};

static struct backtrace *backtrace_list;

#if !BYTE_MARK_STACK
static
#endif
struct catchtag *catchlist;

/* Chain of condition handlers currently in effect.
   The elements of this chain are contained in the stack frames
   of Fcondition_case and internal_condition_case.
   When an error is signaled (by calling Fsignal, below),
   this chain is searched for an element that applies.  */

#if !BYTE_MARK_STACK
static
#endif
struct handler *handlerlist;

#ifdef DEBUG_GCPRO
/* Count levels of GCPRO to detect failure to UNGCPRO.  */
int gcpro_level;
#endif

Lisp_Object Qautoload, Qmacro, Qexit, Qinteractive, Qcommandp, Qdefun;
Lisp_Object Qinhibit_quit;
Lisp_Object Qand_rest;
static Lisp_Object Qand_optional;
static Lisp_Object Qdebug_on_error;
static Lisp_Object Qdeclare;
Lisp_Object Qinternal_interpreter_environment, Qclosure;

static Lisp_Object Qdebug;

/* This holds either the symbol `run-hooks' or nil.
   It is nil at an early stage of startup, and when Emacs
   is shutting down.  */

Lisp_Object Vrun_hooks;

/* Non-nil means record all fset's and provide's, to be undone
   if the file being autoloaded is not fully loaded.
   They are recorded by being consed onto the front of Vautoload_queue:
   (FUN . ODEF) for a defun, (0 . OFEATURES) for a provide.  */

Lisp_Object Vautoload_queue;

/* Current number of specbindings allocated in specpdl.  */

EMACS_INT specpdl_size;

/* Pointer to beginning of specpdl.  */

struct specbinding *specpdl;

/* Pointer to first unused element in specpdl.  */

struct specbinding *specpdl_ptr;

/* Depth in Lisp evaluations and function calls.  */

static EMACS_INT lisp_eval_depth;

/* The value of num_nonmacro_input_events as of the last time we
   started to enter the debugger.  If we decide to enter the debugger
   again when this is still equal to num_nonmacro_input_events, then we
   know that the debugger itself has an error, and we should just
   signal the error instead of entering an infinite loop of debugger
   invocations.  */

static int when_entered_debugger;

/* The function from which the last `signal' was called.  Set in
   Fsignal.  */

Lisp_Object Vsignaling_function;

/* Set to non-zero while processing X events.  Checked in Feval to
   make sure the Lisp interpreter isn't called from a signal handler,
   which is unsafe because the interpreter isn't reentrant.  */

int handling_signal;

/* If non-nil, Lisp code must not be run since some part of Emacs is
   in an inconsistent state.  Currently, x-create-frame uses this to
   avoid triggering window-configuration-change-hook while the new
   frame is half-initialized.  */
Lisp_Object inhibit_lisp_code;

static Lisp_Object funcall_lambda (Lisp_Object, ptrdiff_t, Lisp_Object *);
static void unwind_to_catch (struct catchtag *, Lisp_Object) NO_RETURN;
static int interactive_p (int);
static Lisp_Object apply_lambda (Lisp_Object fun, Lisp_Object args);
static Lisp_Object Ffetch_bytecode (Lisp_Object);

void
init_eval_once (void)
{
  enum { size = 50 };
  specpdl = (struct specbinding *) xmalloc (size * sizeof (struct specbinding));
  specpdl_size = size;
  specpdl_ptr = specpdl;
  /* Don't forget to update docs (lispref node "Local Variables").  */
  max_specpdl_size = 1300; /* 1000 is not enough for CEDET's c-by.el.  */
  max_lisp_eval_depth = 600;

  Vrun_hooks = Qnil;
}

void
init_eval (void)
{
  specpdl_ptr = specpdl;
  catchlist = 0;
  handlerlist = 0;
  backtrace_list = 0;
  Vquit_flag = Qnil;
  debug_on_next_call = 0;
  lisp_eval_depth = 0;
#ifdef DEBUG_GCPRO
  gcpro_level = 0;
#endif
  /* This is less than the initial value of num_nonmacro_input_events.  */
  when_entered_debugger = -1;
}

/* Unwind-protect function used by call_debugger.  */

static Lisp_Object
restore_stack_limits (Lisp_Object data)
{
  max_specpdl_size = XINT (XCAR (data));
  max_lisp_eval_depth = XINT (XCDR (data));
  return Qnil;
}

/* Call the Lisp debugger, giving it argument ARG.  */

static Lisp_Object
call_debugger (Lisp_Object arg)
{
  int debug_while_redisplaying;
  int count = SPECPDL_INDEX ();
  Lisp_Object val;
  EMACS_INT old_max = max_specpdl_size;

  /* Temporarily bump up the stack limits,
     so the debugger won't run out of stack.  */

  max_specpdl_size += 1;
  record_unwind_protect (restore_stack_limits,
			 Fcons (make_number (old_max),
				make_number (max_lisp_eval_depth)));
  max_specpdl_size = old_max;

  if (lisp_eval_depth + 40 > max_lisp_eval_depth)
    max_lisp_eval_depth = lisp_eval_depth + 40;

  if (max_specpdl_size - 100 < SPECPDL_INDEX ())
    max_specpdl_size = SPECPDL_INDEX () + 100;

#ifdef HAVE_WINDOW_SYSTEM
  if (display_hourglass_p)
    cancel_hourglass ();
#endif

  debug_on_next_call = 0;
  when_entered_debugger = num_nonmacro_input_events;

  /* Resetting redisplaying_p to 0 makes sure that debug output is
     displayed if the debugger is invoked during redisplay.  */
  debug_while_redisplaying = redisplaying_p;
  redisplaying_p = 0;
  specbind (intern ("debugger-may-continue"),
	    debug_while_redisplaying ? Qnil : Qt);
  specbind (Qinhibit_redisplay, Qnil);
  specbind (Qdebug_on_error, Qnil);

#if 0 /* Binding this prevents execution of Lisp code during
	 redisplay, which necessarily leads to display problems.  */
  specbind (Qinhibit_eval_during_redisplay, Qt);
#endif

  val = apply1 (Vdebugger, arg);

  /* Interrupting redisplay and resuming it later is not safe under
     all circumstances.  So, when the debugger returns, abort the
     interrupted redisplay by going back to the top-level.  */
  if (debug_while_redisplaying)
    Ftop_level ();

  return unbind_to (count, val);
}

static void
do_debug_on_call (Lisp_Object code)
{
  debug_on_next_call = 0;
  backtrace_list->debug_on_exit = 1;
  call_debugger (Fcons (code, Qnil));
}

/* NOTE!!! Every function that can call EVAL must protect its args
   and temporaries from garbage collection while it needs them.
   The definition of `For' shows what you have to do.  */

DEFUN ("or", For, Sor, 0, UNEVALLED, 0,
       doc: /* Eval args until one of them yields non-nil, then return that value.
The remaining args are not evalled at all.
If all args return nil, return nil.
usage: (or CONDITIONS...)  */)
  (Lisp_Object args)
{
  register Lisp_Object val = Qnil;
  struct gcpro gcpro1;

  GCPRO1 (args);

  while (CONSP (args))
    {
      val = eval_sub (XCAR (args));
      if (!NILP (val))
	break;
      args = XCDR (args);
    }

  UNGCPRO;
  return val;
}

DEFUN ("and", Fand, Sand, 0, UNEVALLED, 0,
       doc: /* Eval args until one of them yields nil, then return nil.
The remaining args are not evalled at all.
If no arg yields nil, return the last arg's value.
usage: (and CONDITIONS...)  */)
  (Lisp_Object args)
{
  register Lisp_Object val = Qt;
  struct gcpro gcpro1;

  GCPRO1 (args);

  while (CONSP (args))
    {
      val = eval_sub (XCAR (args));
      if (NILP (val))
	break;
      args = XCDR (args);
    }

  UNGCPRO;
  return val;
}

DEFUN ("if", Fif, Sif, 2, UNEVALLED, 0,
       doc: /* If COND yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE's.
THEN must be one expression, but ELSE... can be zero or more expressions.
If COND yields nil, and there are no ELSE's, the value is nil.
usage: (if COND THEN ELSE...)  */)
  (Lisp_Object args)
{
  register Lisp_Object cond;
  struct gcpro gcpro1;

  GCPRO1 (args);
  cond = eval_sub (Fcar (args));
  UNGCPRO;

  if (!NILP (cond))
    return eval_sub (Fcar (Fcdr (args)));
  return Fprogn (Fcdr (Fcdr (args)));
}

DEFUN ("cond", Fcond, Scond, 0, UNEVALLED, 0,
       doc: /* Try each clause until one succeeds.
Each clause looks like (CONDITION BODY...).  CONDITION is evaluated
and, if the value is non-nil, this clause succeeds:
then the expressions in BODY are evaluated and the last one's
value is the value of the cond-form.
If no clause succeeds, cond returns nil.
If a clause has one element, as in (CONDITION),
CONDITION's value if non-nil is returned from the cond-form.
usage: (cond CLAUSES...)  */)
  (Lisp_Object args)
{
  register Lisp_Object clause, val;
  struct gcpro gcpro1;

  val = Qnil;
  GCPRO1 (args);
  while (!NILP (args))
    {
      clause = Fcar (args);
      val = eval_sub (Fcar (clause));
      if (!NILP (val))
	{
	  if (!EQ (XCDR (clause), Qnil))
	    val = Fprogn (XCDR (clause));
	  break;
	}
      args = XCDR (args);
    }
  UNGCPRO;

  return val;
}

DEFUN ("progn", Fprogn, Sprogn, 0, UNEVALLED, 0,
       doc: /* Eval BODY forms sequentially and return value of last one.
usage: (progn BODY...)  */)
  (Lisp_Object args)
{
  register Lisp_Object val = Qnil;
  struct gcpro gcpro1;

  GCPRO1 (args);

  while (CONSP (args))
    {
      val = eval_sub (XCAR (args));
      args = XCDR (args);
    }

  UNGCPRO;
  return val;
}

DEFUN ("prog1", Fprog1, Sprog1, 1, UNEVALLED, 0,
       doc: /* Eval FIRST and BODY sequentially; return value from FIRST.
The value of FIRST is saved during the evaluation of the remaining args,
whose values are discarded.
usage: (prog1 FIRST BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object val;
  register Lisp_Object args_left;
  struct gcpro gcpro1, gcpro2;
  register int argnum = 0;

  if (NILP (args))
    return Qnil;

  args_left = args;
  val = Qnil;
  GCPRO2 (args, val);

  do
    {
      Lisp_Object tem = eval_sub (XCAR (args_left));
      if (!(argnum++))
	val = tem;
      args_left = XCDR (args_left);
    }
  while (CONSP (args_left));

  UNGCPRO;
  return val;
}

DEFUN ("prog2", Fprog2, Sprog2, 2, UNEVALLED, 0,
       doc: /* Eval FORM1, FORM2 and BODY sequentially; return value from FORM2.
The value of FORM2 is saved during the evaluation of the
remaining args, whose values are discarded.
usage: (prog2 FORM1 FORM2 BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object val;
  register Lisp_Object args_left;
  struct gcpro gcpro1, gcpro2;
  register int argnum = -1;

  val = Qnil;

  if (NILP (args))
    return Qnil;

  args_left = args;
  val = Qnil;
  GCPRO2 (args, val);

  do
    {
      Lisp_Object tem = eval_sub (XCAR (args_left));
      if (!(argnum++))
	val = tem;
      args_left = XCDR (args_left);
    }
  while (CONSP (args_left));

  UNGCPRO;
  return val;
}

DEFUN ("setq", Fsetq, Ssetq, 0, UNEVALLED, 0,
       doc: /* Set each SYM to the value of its VAL.
The symbols SYM are variables; they are literal (not evaluated).
The values VAL are expressions; they are evaluated.
Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
The second VAL is not computed until after the first SYM is set, and so on;
each VAL can use the new value of variables set earlier in the `setq'.
The return value of the `setq' form is the value of the last VAL.
usage: (setq [SYM VAL]...)  */)
  (Lisp_Object args)
{
  register Lisp_Object args_left;
  register Lisp_Object val, sym, lex_binding;
  struct gcpro gcpro1;

  if (NILP (args))
    return Qnil;

  args_left = args;
  GCPRO1 (args);

  do
    {
      val = eval_sub (Fcar (Fcdr (args_left)));
      sym = Fcar (args_left);

      /* Like for eval_sub, we do not check declared_special here since
	 it's been done when let-binding.  */
      if (!NILP (Vinternal_interpreter_environment) /* Mere optimization!  */
	  && SYMBOLP (sym)
	  && !NILP (lex_binding
		    = Fassq (sym, Vinternal_interpreter_environment)))
	XSETCDR (lex_binding, val); /* SYM is lexically bound.  */
      else
	Fset (sym, val);	/* SYM is dynamically bound.  */

      args_left = Fcdr (Fcdr (args_left));
    }
  while (!NILP (args_left));

  UNGCPRO;
  return val;
}

DEFUN ("quote", Fquote, Squote, 1, UNEVALLED, 0,
       doc: /* Return the argument, without evaluating it.  `(quote x)' yields `x'.
Warning: `quote' does not construct its return value, but just returns
the value that was pre-constructed by the Lisp reader (see info node
`(elisp)Printed Representation').
This means that '(a . b) is not identical to (cons 'a 'b): the former
does not cons.  Quoting should be reserved for constants that will
never be modified by side-effects, unless you like self-modifying code.
See the common pitfall in info node `(elisp)Rearrangement' for an example
of unexpected results when a quoted object is modified.
usage: (quote ARG)  */)
  (Lisp_Object args)
{
  if (!NILP (Fcdr (args)))
    xsignal2 (Qwrong_number_of_arguments, Qquote, Flength (args));
  return Fcar (args);
}

DEFUN ("function", Ffunction, Sfunction, 1, UNEVALLED, 0,
       doc: /* Like `quote', but preferred for objects which are functions.
In byte compilation, `function' causes its argument to be compiled.
`quote' cannot do that.
usage: (function ARG)  */)
  (Lisp_Object args)
{
  Lisp_Object quoted = XCAR (args);

  if (!NILP (Fcdr (args)))
    xsignal2 (Qwrong_number_of_arguments, Qfunction, Flength (args));

  if (!NILP (Vinternal_interpreter_environment)
      && CONSP (quoted)
      && EQ (XCAR (quoted), Qlambda))
    /* This is a lambda expression within a lexical environment;
       return an interpreted closure instead of a simple lambda.  */
    return Fcons (Qclosure, Fcons (Vinternal_interpreter_environment,
				   XCDR (quoted)));
  else
    /* Simply quote the argument.  */
    return quoted;
}


DEFUN ("interactive-p", Finteractive_p, Sinteractive_p, 0, 0, 0,
       doc: /* Return t if the containing function was run directly by user input.
This means that the function was called with `call-interactively'
\(which includes being called as the binding of a key)
and input is currently coming from the keyboard (not a keyboard macro),
and Emacs is not running in batch mode (`noninteractive' is nil).

The only known proper use of `interactive-p' is in deciding whether to
display a helpful message, or how to display it.  If you're thinking
of using it for any other purpose, it is quite likely that you're
making a mistake.  Think: what do you want to do when the command is
called from a keyboard macro?

To test whether your function was called with `call-interactively',
either (i) add an extra optional argument and give it an `interactive'
spec that specifies non-nil unconditionally (such as \"p\"); or (ii)
use `called-interactively-p'.  */)
  (void)
{
  return interactive_p (1) ? Qt : Qnil;
}


DEFUN ("called-interactively-p", Fcalled_interactively_p, Scalled_interactively_p, 0, 1, 0,
       doc: /* Return t if the containing function was called by `call-interactively'.
If KIND is `interactive', then only return t if the call was made
interactively by the user, i.e. not in `noninteractive' mode nor
when `executing-kbd-macro'.
If KIND is `any', on the other hand, it will return t for any kind of
interactive call, including being called as the binding of a key, or
from a keyboard macro, or in `noninteractive' mode.

The only known proper use of `interactive' for KIND is in deciding
whether to display a helpful message, or how to display it.  If you're
thinking of using it for any other purpose, it is quite likely that
you're making a mistake.  Think: what do you want to do when the
command is called from a keyboard macro?

This function is meant for implementing advice and other
function-modifying features.  Instead of using this, it is sometimes
cleaner to give your function an extra optional argument whose
`interactive' spec specifies non-nil unconditionally (\"p\" is a good
way to do this), or via (not (or executing-kbd-macro noninteractive)).  */)
  (Lisp_Object kind)
{
  return ((INTERACTIVE || !EQ (kind, intern ("interactive")))
	  && interactive_p (1)) ? Qt : Qnil;
}


/*  Return 1 if function in which this appears was called using
    call-interactively.

    EXCLUDE_SUBRS_P non-zero means always return 0 if the function
    called is a built-in.  */

static int
interactive_p (int exclude_subrs_p)
{
  struct backtrace *btp;
  Lisp_Object fun;

  btp = backtrace_list;

  /* If this isn't a byte-compiled function, there may be a frame at
     the top for Finteractive_p.  If so, skip it.  */
  fun = Findirect_function (*btp->function, Qnil);
  if (SUBRP (fun) && (XSUBR (fun) == &Sinteractive_p
		      || XSUBR (fun) == &Scalled_interactively_p))
    btp = btp->next;

  /* If we're running an Emacs 18-style byte-compiled function, there
     may be a frame for Fbytecode at the top level.  In any version of
     Emacs there can be Fbytecode frames for subexpressions evaluated
     inside catch and condition-case.  Skip past them.

     If this isn't a byte-compiled function, then we may now be
     looking at several frames for special forms.  Skip past them.  */
  while (btp
	 && (EQ (*btp->function, Qbytecode)
	     || btp->nargs == UNEVALLED))
    btp = btp->next;

  /* `btp' now points at the frame of the innermost function that isn't
     a special form, ignoring frames for Finteractive_p and/or
     Fbytecode at the top.  If this frame is for a built-in function
     (such as load or eval-region) return nil.  */
  fun = Findirect_function (*btp->function, Qnil);
  if (exclude_subrs_p && SUBRP (fun))
    return 0;

  /* `btp' points to the frame of a Lisp function that called interactive-p.
     Return t if that function was called interactively.  */
  if (btp && btp->next && EQ (*btp->next->function, Qcall_interactively))
    return 1;
  return 0;
}


DEFUN ("defun", Fdefun, Sdefun, 2, UNEVALLED, 0,
       doc: /* Define NAME as a function.
The definition is (lambda ARGLIST [DOCSTRING] BODY...).
See also the function `interactive'.
usage: (defun NAME ARGLIST [DOCSTRING] BODY...)  */)
  (Lisp_Object args)
{
  register Lisp_Object fn_name;
  register Lisp_Object defn;

  fn_name = Fcar (args);
  CHECK_SYMBOL (fn_name);
  defn = Fcons (Qlambda, Fcdr (args));
  if (!NILP (Vinternal_interpreter_environment)) /* Mere optimization!  */
    defn = Ffunction (Fcons (defn, Qnil));
  if (!NILP (Vpurify_flag))
    defn = Fpurecopy (defn);
  if (CONSP (XSYMBOL (fn_name)->function)
      && EQ (XCAR (XSYMBOL (fn_name)->function), Qautoload))
    LOADHIST_ATTACH (Fcons (Qt, fn_name));
  Ffset (fn_name, defn);
  LOADHIST_ATTACH (Fcons (Qdefun, fn_name));
  return fn_name;
}

DEFUN ("defmacro", Fdefmacro, Sdefmacro, 2, UNEVALLED, 0,
       doc: /* Define NAME as a macro.
The actual definition looks like
 (macro lambda ARGLIST [DOCSTRING] [DECL] BODY...).
When the macro is called, as in (NAME ARGS...),
the function (lambda ARGLIST BODY...) is applied to
the list ARGS... as it appears in the expression,
and the result should be a form to be evaluated instead of the original.

DECL is a declaration, optional, which can specify how to indent
calls to this macro, how Edebug should handle it, and which argument
should be treated as documentation.  It looks like this:
  (declare SPECS...)
The elements can look like this:
  (indent INDENT)
	Set NAME's `lisp-indent-function' property to INDENT.

  (debug DEBUG)
	Set NAME's `edebug-form-spec' property to DEBUG.  (This is
	equivalent to writing a `def-edebug-spec' for the macro.)

  (doc-string ELT)
	Set NAME's `doc-string-elt' property to ELT.

usage: (defmacro NAME ARGLIST [DOCSTRING] [DECL] BODY...)  */)
  (Lisp_Object args)
{
  register Lisp_Object fn_name;
  register Lisp_Object defn;
  Lisp_Object lambda_list, doc, tail;

  fn_name = Fcar (args);
  CHECK_SYMBOL (fn_name);
  lambda_list = Fcar (Fcdr (args));
  tail = Fcdr (Fcdr (args));

  doc = Qnil;
  if (STRINGP (Fcar (tail)))
    {
      doc = XCAR (tail);
      tail = XCDR (tail);
    }

  if (CONSP (Fcar (tail))
      && EQ (Fcar (Fcar (tail)), Qdeclare))
    {
      if (!NILP (Vmacro_declaration_function))
	{
	  struct gcpro gcpro1;
	  GCPRO1 (args);
	  call2 (Vmacro_declaration_function, fn_name, Fcar (tail));
	  UNGCPRO;
	}

      tail = Fcdr (tail);
    }

  if (NILP (doc))
    tail = Fcons (lambda_list, tail);
  else
    tail = Fcons (lambda_list, Fcons (doc, tail));

  defn = Fcons (Qlambda, tail);
  if (!NILP (Vinternal_interpreter_environment)) /* Mere optimization!  */
    defn = Ffunction (Fcons (defn, Qnil));
  defn = Fcons (Qmacro, defn);

  if (!NILP (Vpurify_flag))
    defn = Fpurecopy (defn);
  if (CONSP (XSYMBOL (fn_name)->function)
      && EQ (XCAR (XSYMBOL (fn_name)->function), Qautoload))
    LOADHIST_ATTACH (Fcons (Qt, fn_name));
  Ffset (fn_name, defn);
  LOADHIST_ATTACH (Fcons (Qdefun, fn_name));
  return fn_name;
}


DEFUN ("defvaralias", Fdefvaralias, Sdefvaralias, 2, 3, 0,
       doc: /* Make NEW-ALIAS a variable alias for symbol BASE-VARIABLE.
Aliased variables always have the same value; setting one sets the other.
Third arg DOCSTRING, if non-nil, is documentation for NEW-ALIAS.  If it is
omitted or nil, NEW-ALIAS gets the documentation string of BASE-VARIABLE,
or of the variable at the end of the chain of aliases, if BASE-VARIABLE is
itself an alias.  If NEW-ALIAS is bound, and BASE-VARIABLE is not,
then the value of BASE-VARIABLE is set to that of NEW-ALIAS.
The return value is BASE-VARIABLE.  */)
  (Lisp_Object new_alias, Lisp_Object base_variable, Lisp_Object docstring)
{
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (new_alias);
  CHECK_SYMBOL (base_variable);

  sym = XSYMBOL (new_alias);

  if (sym->constant)
    /* Not sure why, but why not?  */
    error ("Cannot make a constant an alias");

  switch (sym->redirect)
    {
    case SYMBOL_FORWARDED:
      error ("Cannot make an internal variable an alias");
    case SYMBOL_LOCALIZED:
      error ("Don't know how to make a localized variable an alias");
    }

  /* http://lists.gnu.org/archive/html/emacs-devel/2008-04/msg00834.html
     If n_a is bound, but b_v is not, set the value of b_v to n_a,
     so that old-code that affects n_a before the aliasing is setup
     still works.  */
  if (NILP (Fboundp (base_variable)))
    set_internal (base_variable, find_symbol_value (new_alias), Qnil, 1);

  {
    struct specbinding *p;

    for (p = specpdl_ptr - 1; p >= specpdl; p--)
      if (p->func == NULL
	  && (EQ (new_alias,
		  CONSP (p->symbol) ? XCAR (p->symbol) : p->symbol)))
	error ("Don't know how to make a let-bound variable an alias");
  }

  sym->declared_special = 1;
  XSYMBOL (base_variable)->declared_special = 1;
  sym->redirect = SYMBOL_VARALIAS;
  SET_SYMBOL_ALIAS (sym, XSYMBOL (base_variable));
  sym->constant = SYMBOL_CONSTANT_P (base_variable);
  LOADHIST_ATTACH (new_alias);
  /* Even if docstring is nil: remove old docstring.  */
  Fput (new_alias, Qvariable_documentation, docstring);

  return base_variable;
}


DEFUN ("defvar", Fdefvar, Sdefvar, 1, UNEVALLED, 0,
       doc: /* Define SYMBOL as a variable, and return SYMBOL.
You are not required to define a variable in order to use it, but
defining it lets you supply an initial value and documentation, which
can be referred to by the Emacs help facilities and other programming
tools.  The `defvar' form also declares the variable as \"special\",
so that it is always dynamically bound even if `lexical-binding' is t.

The optional argument INITVALUE is evaluated, and used to set SYMBOL,
only if SYMBOL's value is void.  If SYMBOL is buffer-local, its
default value is what is set; buffer-local values are not affected.
If INITVALUE is missing, SYMBOL's value is not set.

If SYMBOL has a local binding, then this form affects the local
binding.  This is usually not what you want.  Thus, if you need to
load a file defining variables, with this form or with `defconst' or
`defcustom', you should always load that file _outside_ any bindings
for these variables.  \(`defconst' and `defcustom' behave similarly in
this respect.)

The optional argument DOCSTRING is a documentation string for the
variable.

To define a user option, use `defcustom' instead of `defvar'.
The function `user-variable-p' also identifies a variable as a user
option if its DOCSTRING starts with *, but this behavior is obsolete.
usage: (defvar SYMBOL &optional INITVALUE DOCSTRING)  */)
  (Lisp_Object args)
{
  register Lisp_Object sym, tem, tail;

  sym = Fcar (args);
  tail = Fcdr (args);
  if (!NILP (Fcdr (Fcdr (tail))))
    error ("Too many arguments");

  tem = Fdefault_boundp (sym);
  if (!NILP (tail))
    {
      /* Do it before evaluating the initial value, for self-references.  */
      XSYMBOL (sym)->declared_special = 1;

      if (SYMBOL_CONSTANT_P (sym))
	{
	  /* For upward compatibility, allow (defvar :foo (quote :foo)).  */
	  Lisp_Object tem1 = Fcar (tail);
	  if (! (CONSP (tem1)
		 && EQ (XCAR (tem1), Qquote)
		 && CONSP (XCDR (tem1))
		 && EQ (XCAR (XCDR (tem1)), sym)))
	    error ("Constant symbol `%s' specified in defvar",
		   SDATA (SYMBOL_NAME (sym)));
	}

      if (NILP (tem))
	Fset_default (sym, eval_sub (Fcar (tail)));
      else
	{ /* Check if there is really a global binding rather than just a let
	     binding that shadows the global unboundness of the var.  */
	  volatile struct specbinding *pdl = specpdl_ptr;
	  while (--pdl >= specpdl)
	    {
	      if (EQ (pdl->symbol, sym) && !pdl->func
		  && EQ (pdl->old_value, Qunbound))
		{
		  message_with_string ("Warning: defvar ignored because %s is let-bound",
				       SYMBOL_NAME (sym), 1);
		  break;
		}
	    }
	}
      tail = Fcdr (tail);
      tem = Fcar (tail);
      if (!NILP (tem))
	{
	  if (!NILP (Vpurify_flag))
	    tem = Fpurecopy (tem);
	  Fput (sym, Qvariable_documentation, tem);
	}
      LOADHIST_ATTACH (sym);
    }
  else if (!NILP (Vinternal_interpreter_environment)
	   && !XSYMBOL (sym)->declared_special)
    /* A simple (defvar foo) with lexical scoping does "nothing" except
       declare that var to be dynamically scoped *locally* (i.e. within
       the current file or let-block).  */
    Vinternal_interpreter_environment =
      Fcons (sym, Vinternal_interpreter_environment);
  else
    {
      /* Simple (defvar <var>) should not count as a definition at all.
	 It could get in the way of other definitions, and unloading this
	 package could try to make the variable unbound.  */
    }

  return sym;
}

DEFUN ("defconst", Fdefconst, Sdefconst, 2, UNEVALLED, 0,
       doc: /* Define SYMBOL as a constant variable.
This declares that neither programs nor users should ever change the
value.  This constancy is not actually enforced by Emacs Lisp, but
SYMBOL is marked as a special variable so that it is never lexically
bound.

The `defconst' form always sets the value of SYMBOL to the result of
evalling INITVALUE.  If SYMBOL is buffer-local, its default value is
what is set; buffer-local values are not affected.  If SYMBOL has a
local binding, then this form sets the local binding's value.
However, you should normally not make local bindings for variables
defined with this form.

The optional DOCSTRING specifies the variable's documentation string.
usage: (defconst SYMBOL INITVALUE [DOCSTRING])  */)
  (Lisp_Object args)
{
  register Lisp_Object sym, tem;

  sym = Fcar (args);
  if (!NILP (Fcdr (Fcdr (Fcdr (args)))))
    error ("Too many arguments");

  tem = eval_sub (Fcar (Fcdr (args)));
  if (!NILP (Vpurify_flag))
    tem = Fpurecopy (tem);
  Fset_default (sym, tem);
  XSYMBOL (sym)->declared_special = 1;
  tem = Fcar (Fcdr (Fcdr (args)));
  if (!NILP (tem))
    {
      if (!NILP (Vpurify_flag))
	tem = Fpurecopy (tem);
      Fput (sym, Qvariable_documentation, tem);
    }
  Fput (sym, Qrisky_local_variable, Qt);
  LOADHIST_ATTACH (sym);
  return sym;
}

/* Error handler used in Fuser_variable_p.  */
static Lisp_Object
user_variable_p_eh (Lisp_Object ignore)
{
  return Qnil;
}

static Lisp_Object
lisp_indirect_variable (Lisp_Object sym)
{
  struct Lisp_Symbol *s = indirect_variable (XSYMBOL (sym));
  XSETSYMBOL (sym, s);
  return sym;
}

DEFUN ("user-variable-p", Fuser_variable_p, Suser_variable_p, 1, 1, 0,
       doc: /* Return t if VARIABLE is intended to be set and modified by users.
\(The alternative is a variable used internally in a Lisp program.)

This function returns t if (i) the first character of its
documentation is `*', or (ii) it is customizable (its property list
contains a non-nil value of `standard-value' or `custom-autoload'), or
\(iii) it is an alias for a user variable.

But condition (i) is considered obsolete, so for most purposes this is
equivalent to `custom-variable-p'.  */)
  (Lisp_Object variable)
{
  Lisp_Object documentation;

  if (!SYMBOLP (variable))
      return Qnil;

  /* If indirect and there's an alias loop, don't check anything else.  */
  if (XSYMBOL (variable)->redirect == SYMBOL_VARALIAS
      && NILP (internal_condition_case_1 (lisp_indirect_variable, variable,
					  Qt, user_variable_p_eh)))
    return Qnil;

  while (1)
    {
      documentation = Fget (variable, Qvariable_documentation);
      if (INTEGERP (documentation) && XINT (documentation) < 0)
	return Qt;
      if (STRINGP (documentation)
	  && ((unsigned char) SREF (documentation, 0) == '*'))
	return Qt;
      /* If it is (STRING . INTEGER), a negative integer means a user variable.  */
      if (CONSP (documentation)
	  && STRINGP (XCAR (documentation))
	  && INTEGERP (XCDR (documentation))
	  && XINT (XCDR (documentation)) < 0)
	return Qt;
      /* Customizable?  See `custom-variable-p'.  */
      if ((!NILP (Fget (variable, intern ("standard-value"))))
	  || (!NILP (Fget (variable, intern ("custom-autoload")))))
	return Qt;

      if (!(XSYMBOL (variable)->redirect == SYMBOL_VARALIAS))
	return Qnil;

      /* An indirect variable?  Let's follow the chain.  */
      XSETSYMBOL (variable, SYMBOL_ALIAS (XSYMBOL (variable)));
    }
}

DEFUN ("let*", FletX, SletX, 1, UNEVALLED, 0,
       doc: /* Bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
Each VALUEFORM can refer to the symbols already bound by this VARLIST.
usage: (let* VARLIST BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object varlist, var, val, elt, lexenv;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (args, elt, varlist);

  lexenv = Vinternal_interpreter_environment;

  varlist = Fcar (args);
  while (CONSP (varlist))
    {
      QUIT;

      elt = XCAR (varlist);
      if (SYMBOLP (elt))
	{
	  var = elt;
	  val = Qnil;
	}
      else if (! NILP (Fcdr (Fcdr (elt))))
	signal_error ("`let' bindings can have only one value-form", elt);
      else
	{
	  var = Fcar (elt);
	  val = eval_sub (Fcar (Fcdr (elt)));
	}

      if (!NILP (lexenv) && SYMBOLP (var)
	  && !XSYMBOL (var)->declared_special
	  && NILP (Fmemq (var, Vinternal_interpreter_environment)))
	/* Lexically bind VAR by adding it to the interpreter's binding
	   alist.  */
	{
	  Lisp_Object newenv
	    = Fcons (Fcons (var, val), Vinternal_interpreter_environment);
	  if (EQ (Vinternal_interpreter_environment, lexenv))
	    /* Save the old lexical environment on the specpdl stack,
	       but only for the first lexical binding, since we'll never
	       need to revert to one of the intermediate ones.  */
	    specbind (Qinternal_interpreter_environment, newenv);
	  else
	    Vinternal_interpreter_environment = newenv;
	}
      else
	specbind (var, val);

      varlist = XCDR (varlist);
    }
  UNGCPRO;
  val = Fprogn (Fcdr (args));
  return unbind_to (count, val);
}

DEFUN ("let", Flet, Slet, 1, UNEVALLED, 0,
       doc: /* Bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
All the VALUEFORMs are evalled before any symbols are bound.
usage: (let VARLIST BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object *temps, tem, lexenv;
  register Lisp_Object elt, varlist;
  int count = SPECPDL_INDEX ();
  ptrdiff_t argnum;
  struct gcpro gcpro1, gcpro2;
  USE_SAFE_ALLOCA;

  varlist = Fcar (args);

  /* Make space to hold the values to give the bound variables.  */
  elt = Flength (varlist);
  SAFE_ALLOCA_LISP (temps, XFASTINT (elt));

  /* Compute the values and store them in `temps'.  */

  GCPRO2 (args, *temps);
  gcpro2.nvars = 0;

  for (argnum = 0; CONSP (varlist); varlist = XCDR (varlist))
    {
      QUIT;
      elt = XCAR (varlist);
      if (SYMBOLP (elt))
	temps [argnum++] = Qnil;
      else if (! NILP (Fcdr (Fcdr (elt))))
	signal_error ("`let' bindings can have only one value-form", elt);
      else
	temps [argnum++] = eval_sub (Fcar (Fcdr (elt)));
      gcpro2.nvars = argnum;
    }
  UNGCPRO;

  lexenv = Vinternal_interpreter_environment;

  varlist = Fcar (args);
  for (argnum = 0; CONSP (varlist); varlist = XCDR (varlist))
    {
      Lisp_Object var;

      elt = XCAR (varlist);
      var = SYMBOLP (elt) ? elt : Fcar (elt);
      tem = temps[argnum++];

      if (!NILP (lexenv) && SYMBOLP (var)
	  && !XSYMBOL (var)->declared_special
	  && NILP (Fmemq (var, Vinternal_interpreter_environment)))
	/* Lexically bind VAR by adding it to the lexenv alist.  */
	lexenv = Fcons (Fcons (var, tem), lexenv);
      else
	/* Dynamically bind VAR.  */
	specbind (var, tem);
    }

  if (!EQ (lexenv, Vinternal_interpreter_environment))
    /* Instantiate a new lexical environment.  */
    specbind (Qinternal_interpreter_environment, lexenv);

  elt = Fprogn (Fcdr (args));
  SAFE_FREE ();
  return unbind_to (count, elt);
}

DEFUN ("while", Fwhile, Swhile, 1, UNEVALLED, 0,
       doc: /* If TEST yields non-nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns nil.
usage: (while TEST BODY...)  */)
  (Lisp_Object args)
{
  Lisp_Object test, body;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (test, body);

  test = Fcar (args);
  body = Fcdr (args);
  while (!NILP (eval_sub (test)))
    {
      QUIT;
      Fprogn (body);
    }

  UNGCPRO;
  return Qnil;
}

DEFUN ("macroexpand", Fmacroexpand, Smacroexpand, 1, 2, 0,
       doc: /* Return result of expanding macros at top level of FORM.
If FORM is not a macro call, it is returned unchanged.
Otherwise, the macro is expanded and the expansion is considered
in place of FORM.  When a non-macro-call results, it is returned.

The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation.  */)
  (Lisp_Object form, Lisp_Object environment)
{
  /* With cleanups from Hallvard Furuseth.  */
  register Lisp_Object expander, sym, def, tem;

  while (1)
    {
      /* Come back here each time we expand a macro call,
	 in case it expands into another macro call.  */
      if (!CONSP (form))
	break;
      /* Set SYM, give DEF and TEM right values in case SYM is not a symbol. */
      def = sym = XCAR (form);
      tem = Qnil;
      /* Trace symbols aliases to other symbols
	 until we get a symbol that is not an alias.  */
      while (SYMBOLP (def))
	{
	  QUIT;
	  sym = def;
	  tem = Fassq (sym, environment);
	  if (NILP (tem))
	    {
	      def = XSYMBOL (sym)->function;
	      if (!EQ (def, Qunbound))
		continue;
	    }
	  break;
	}
      /* Right now TEM is the result from SYM in ENVIRONMENT,
	 and if TEM is nil then DEF is SYM's function definition.  */
      if (NILP (tem))
	{
	  /* SYM is not mentioned in ENVIRONMENT.
	     Look at its function definition.  */
	  if (EQ (def, Qunbound) || !CONSP (def))
	    /* Not defined or definition not suitable.  */
	    break;
	  if (EQ (XCAR (def), Qautoload))
	    {
	      /* Autoloading function: will it be a macro when loaded?  */
	      tem = Fnth (make_number (4), def);
	      if (EQ (tem, Qt) || EQ (tem, Qmacro))
		/* Yes, load it and try again.  */
		{
		  struct gcpro gcpro1;
		  GCPRO1 (form);
		  do_autoload (def, sym);
		  UNGCPRO;
		  continue;
		}
	      else
		break;
	    }
	  else if (!EQ (XCAR (def), Qmacro))
	    break;
	  else expander = XCDR (def);
	}
      else
	{
	  expander = XCDR (tem);
	  if (NILP (expander))
	    break;
	}
      form = apply1 (expander, XCDR (form));
    }
  return form;
}

DEFUN ("catch", Fcatch, Scatch, 1, UNEVALLED, 0,
       doc: /* Eval BODY allowing nonlocal exits using `throw'.
TAG is evalled to get the tag to use; it must not be nil.

Then the BODY is executed.
Within BODY, a call to `throw' with the same TAG exits BODY and this `catch'.
If no throw happens, `catch' returns the value of the last BODY form.
If a throw happens, it specifies the value to return from `catch'.
usage: (catch TAG BODY...)  */)
  (Lisp_Object args)
{
  register Lisp_Object tag;
  struct gcpro gcpro1;

  GCPRO1 (args);
  tag = eval_sub (Fcar (args));
  UNGCPRO;
  return internal_catch (tag, Fprogn, Fcdr (args));
}

/* Set up a catch, then call C function FUNC on argument ARG.
   FUNC should return a Lisp_Object.
   This is how catches are done from within C code. */

Lisp_Object
internal_catch (Lisp_Object tag, Lisp_Object (*func) (Lisp_Object), Lisp_Object arg)
{
  /* This structure is made part of the chain `catchlist'.  */
  struct catchtag c;

  /* Fill in the components of c, and put it on the list.  */
  c.next = catchlist;
  c.tag = tag;
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.handlerlist = handlerlist;
  c.lisp_eval_depth = lisp_eval_depth;
  c.pdlcount = SPECPDL_INDEX ();
  c.poll_suppress_count = poll_suppress_count;
  c.interrupt_input_blocked = interrupt_input_blocked;
  c.gcpro = gcprolist;
  c.byte_stack = byte_stack_list;
  catchlist = &c;

  /* Call FUNC.  */
  if (! _setjmp (c.jmp))
    c.val = (*func) (arg);

  /* Throw works by a longjmp that comes right here.  */
  catchlist = c.next;
  return c.val;
}

/* Unwind the specbind, catch, and handler stacks back to CATCH, and
   jump to that CATCH, returning VALUE as the value of that catch.

   This is the guts Fthrow and Fsignal; they differ only in the way
   they choose the catch tag to throw to.  A catch tag for a
   condition-case form has a TAG of Qnil.

   Before each catch is discarded, unbind all special bindings and
   execute all unwind-protect clauses made above that catch.  Unwind
   the handler stack as we go, so that the proper handlers are in
   effect for each unwind-protect clause we run.  At the end, restore
   some static info saved in CATCH, and longjmp to the location
   specified in the

   This is used for correct unwinding in Fthrow and Fsignal.  */

static void
unwind_to_catch (struct catchtag *catch, Lisp_Object value)
{
  register int last_time;

  /* Save the value in the tag.  */
  catch->val = value;

  /* Restore certain special C variables.  */
  set_poll_suppress_count (catch->poll_suppress_count);
  UNBLOCK_INPUT_TO (catch->interrupt_input_blocked);
  handling_signal = 0;
  immediate_quit = 0;

  do
    {
      last_time = catchlist == catch;

      /* Unwind the specpdl stack, and then restore the proper set of
	 handlers.  */
      unbind_to (catchlist->pdlcount, Qnil);
      handlerlist = catchlist->handlerlist;
      catchlist = catchlist->next;
    }
  while (! last_time);

#if HAVE_X_WINDOWS
  /* If x_catch_errors was done, turn it off now.
     (First we give unbind_to a chance to do that.)  */
#if 0 /* This would disable x_catch_errors after x_connection_closed.
	 The catch must remain in effect during that delicate
	 state. --lorentey  */
  x_fully_uncatch_errors ();
#endif
#endif

  byte_stack_list = catch->byte_stack;
  gcprolist = catch->gcpro;
#ifdef DEBUG_GCPRO
  gcpro_level = gcprolist ? gcprolist->level + 1 : 0;
#endif
  backtrace_list = catch->backlist;
  lisp_eval_depth = catch->lisp_eval_depth;

  _longjmp (catch->jmp, 1);
}

DEFUN ("throw", Fthrow, Sthrow, 2, 2, 0,
       doc: /* Throw to the catch for TAG and return VALUE from it.
Both TAG and VALUE are evalled.  */)
  (register Lisp_Object tag, Lisp_Object value)
{
  register struct catchtag *c;

  if (!NILP (tag))
    for (c = catchlist; c; c = c->next)
      {
	if (EQ (c->tag, tag))
	  unwind_to_catch (c, value);
      }
  xsignal2 (Qno_catch, tag, value);
}


DEFUN ("unwind-protect", Funwind_protect, Sunwind_protect, 1, UNEVALLED, 0,
       doc: /* Do BODYFORM, protecting with UNWINDFORMS.
If BODYFORM completes normally, its value is returned
after executing the UNWINDFORMS.
If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway.
usage: (unwind-protect BODYFORM UNWINDFORMS...)  */)
  (Lisp_Object args)
{
  Lisp_Object val;
  int count = SPECPDL_INDEX ();

  record_unwind_protect (Fprogn, Fcdr (args));
  val = eval_sub (Fcar (args));
  return unbind_to (count, val);
}

DEFUN ("condition-case", Fcondition_case, Scondition_case, 2, UNEVALLED, 0,
       doc: /* Regain control when an error is signaled.
Executes BODYFORM and returns its value if no error happens.
Each element of HANDLERS looks like (CONDITION-NAME BODY...)
where the BODY is made of Lisp expressions.

A handler is applicable to an error
if CONDITION-NAME is one of the error's condition names.
If an error happens, the first applicable handler is run.

The car of a handler may be a list of condition names instead of a
single condition name; then it handles all of them.  If the special
condition name `debug' is present in this list, it allows another
condition in the list to run the debugger if `debug-on-error' and the
other usual mechanisms says it should (otherwise, `condition-case'
suppresses the debugger).

When a handler handles an error, control returns to the `condition-case'
and it executes the handler's BODY...
with VAR bound to (ERROR-SYMBOL . SIGNAL-DATA) from the error.
\(If VAR is nil, the handler can't access that information.)
Then the value of the last BODY form is returned from the `condition-case'
expression.

See also the function `signal' for more info.
usage: (condition-case VAR BODYFORM &rest HANDLERS)  */)
  (Lisp_Object args)
{
  register Lisp_Object bodyform, handlers;
  volatile Lisp_Object var;

  var      = Fcar (args);
  bodyform = Fcar (Fcdr (args));
  handlers = Fcdr (Fcdr (args));

  return internal_lisp_condition_case (var, bodyform, handlers);
}

/* Like Fcondition_case, but the args are separate
   rather than passed in a list.  Used by Fbyte_code.  */

Lisp_Object
internal_lisp_condition_case (volatile Lisp_Object var, Lisp_Object bodyform,
			      Lisp_Object handlers)
{
  Lisp_Object val;
  struct catchtag c;
  struct handler h;

  CHECK_SYMBOL (var);

  for (val = handlers; CONSP (val); val = XCDR (val))
    {
      Lisp_Object tem;
      tem = XCAR (val);
      if (! (NILP (tem)
	     || (CONSP (tem)
		 && (SYMBOLP (XCAR (tem))
		     || CONSP (XCAR (tem))))))
	error ("Invalid condition handler: %s",
	       SDATA (Fprin1_to_string (tem, Qt)));
    }

  c.tag = Qnil;
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.handlerlist = handlerlist;
  c.lisp_eval_depth = lisp_eval_depth;
  c.pdlcount = SPECPDL_INDEX ();
  c.poll_suppress_count = poll_suppress_count;
  c.interrupt_input_blocked = interrupt_input_blocked;
  c.gcpro = gcprolist;
  c.byte_stack = byte_stack_list;
  if (_setjmp (c.jmp))
    {
      if (!NILP (h.var))
	specbind (h.var, c.val);
      val = Fprogn (Fcdr (h.chosen_clause));

      /* Note that this just undoes the binding of h.var; whoever
	 longjumped to us unwound the stack to c.pdlcount before
	 throwing. */
      unbind_to (c.pdlcount, Qnil);
      return val;
    }
  c.next = catchlist;
  catchlist = &c;

  h.var = var;
  h.handler = handlers;
  h.next = handlerlist;
  h.tag = &c;
  handlerlist = &h;

  val = eval_sub (bodyform);
  catchlist = c.next;
  handlerlist = h.next;
  return val;
}

/* Call the function BFUN with no arguments, catching errors within it
   according to HANDLERS.  If there is an error, call HFUN with
   one argument which is the data that describes the error:
   (SIGNALNAME . DATA)

   HANDLERS can be a list of conditions to catch.
   If HANDLERS is Qt, catch all errors.
   If HANDLERS is Qerror, catch all errors
   but allow the debugger to run if that is enabled.  */

Lisp_Object
internal_condition_case (Lisp_Object (*bfun) (void), Lisp_Object handlers,
			 Lisp_Object (*hfun) (Lisp_Object))
{
  Lisp_Object val;
  struct catchtag c;
  struct handler h;

  c.tag = Qnil;
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.handlerlist = handlerlist;
  c.lisp_eval_depth = lisp_eval_depth;
  c.pdlcount = SPECPDL_INDEX ();
  c.poll_suppress_count = poll_suppress_count;
  c.interrupt_input_blocked = interrupt_input_blocked;
  c.gcpro = gcprolist;
  c.byte_stack = byte_stack_list;
  if (_setjmp (c.jmp))
    {
      return (*hfun) (c.val);
    }
  c.next = catchlist;
  catchlist = &c;
  h.handler = handlers;
  h.var = Qnil;
  h.next = handlerlist;
  h.tag = &c;
  handlerlist = &h;

  val = (*bfun) ();
  catchlist = c.next;
  handlerlist = h.next;
  return val;
}

/* Like internal_condition_case but call BFUN with ARG as its argument.  */

Lisp_Object
internal_condition_case_1 (Lisp_Object (*bfun) (Lisp_Object), Lisp_Object arg,
			   Lisp_Object handlers, Lisp_Object (*hfun) (Lisp_Object))
{
  Lisp_Object val;
  struct catchtag c;
  struct handler h;

  c.tag = Qnil;
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.handlerlist = handlerlist;
  c.lisp_eval_depth = lisp_eval_depth;
  c.pdlcount = SPECPDL_INDEX ();
  c.poll_suppress_count = poll_suppress_count;
  c.interrupt_input_blocked = interrupt_input_blocked;
  c.gcpro = gcprolist;
  c.byte_stack = byte_stack_list;
  if (_setjmp (c.jmp))
    {
      return (*hfun) (c.val);
    }
  c.next = catchlist;
  catchlist = &c;
  h.handler = handlers;
  h.var = Qnil;
  h.next = handlerlist;
  h.tag = &c;
  handlerlist = &h;

  val = (*bfun) (arg);
  catchlist = c.next;
  handlerlist = h.next;
  return val;
}

/* Like internal_condition_case_1 but call BFUN with ARG1 and ARG2 as
   its arguments.  */

Lisp_Object
internal_condition_case_2 (Lisp_Object (*bfun) (Lisp_Object, Lisp_Object),
			   Lisp_Object arg1,
			   Lisp_Object arg2,
			   Lisp_Object handlers,
			   Lisp_Object (*hfun) (Lisp_Object))
{
  Lisp_Object val;
  struct catchtag c;
  struct handler h;

  c.tag = Qnil;
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.handlerlist = handlerlist;
  c.lisp_eval_depth = lisp_eval_depth;
  c.pdlcount = SPECPDL_INDEX ();
  c.poll_suppress_count = poll_suppress_count;
  c.interrupt_input_blocked = interrupt_input_blocked;
  c.gcpro = gcprolist;
  c.byte_stack = byte_stack_list;
  if (_setjmp (c.jmp))
    {
      return (*hfun) (c.val);
    }
  c.next = catchlist;
  catchlist = &c;
  h.handler = handlers;
  h.var = Qnil;
  h.next = handlerlist;
  h.tag = &c;
  handlerlist = &h;

  val = (*bfun) (arg1, arg2);
  catchlist = c.next;
  handlerlist = h.next;
  return val;
}

/* Like internal_condition_case but call BFUN with NARGS as first,
   and ARGS as second argument.  */

Lisp_Object
internal_condition_case_n (Lisp_Object (*bfun) (ptrdiff_t, Lisp_Object *),
			   ptrdiff_t nargs,
			   Lisp_Object *args,
			   Lisp_Object handlers,
			   Lisp_Object (*hfun) (Lisp_Object))
{
  Lisp_Object val;
  struct catchtag c;
  struct handler h;

  c.tag = Qnil;
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.handlerlist = handlerlist;
  c.lisp_eval_depth = lisp_eval_depth;
  c.pdlcount = SPECPDL_INDEX ();
  c.poll_suppress_count = poll_suppress_count;
  c.interrupt_input_blocked = interrupt_input_blocked;
  c.gcpro = gcprolist;
  c.byte_stack = byte_stack_list;
  if (_setjmp (c.jmp))
    {
      return (*hfun) (c.val);
    }
  c.next = catchlist;
  catchlist = &c;
  h.handler = handlers;
  h.var = Qnil;
  h.next = handlerlist;
  h.tag = &c;
  handlerlist = &h;

  val = (*bfun) (nargs, args);
  catchlist = c.next;
  handlerlist = h.next;
  return val;
}


static Lisp_Object find_handler_clause (Lisp_Object, Lisp_Object);
static int maybe_call_debugger (Lisp_Object conditions, Lisp_Object sig,
				Lisp_Object data);

void
process_quit_flag (void)
{
  Lisp_Object flag = Vquit_flag;
  Vquit_flag = Qnil;
  if (EQ (flag, Qkill_emacs))
    Fkill_emacs (Qnil);
  if (EQ (Vthrow_on_input, flag))
    Fthrow (Vthrow_on_input, Qt);
  Fsignal (Qquit, Qnil);
}

DEFUN ("signal", Fsignal, Ssignal, 2, 2, 0,
       doc: /* Signal an error.  Args are ERROR-SYMBOL and associated DATA.
This function does not return.

An error symbol is a symbol with an `error-conditions' property
that is a list of condition names.
A handler for any of those names will get to handle this signal.
The symbol `error' should normally be one of them.

DATA should be a list.  Its elements are printed as part of the error message.
See Info anchor `(elisp)Definition of signal' for some details on how this
error message is constructed.
If the signal is handled, DATA is made available to the handler.
See also the function `condition-case'.  */)
  (Lisp_Object error_symbol, Lisp_Object data)
{
  /* When memory is full, ERROR-SYMBOL is nil,
     and DATA is (REAL-ERROR-SYMBOL . REAL-DATA).
     That is a special case--don't do this in other situations.  */
  Lisp_Object conditions;
  Lisp_Object string;
  Lisp_Object real_error_symbol
    = (NILP (error_symbol) ? Fcar (data) : error_symbol);
  register Lisp_Object clause = Qnil;
  struct handler *h;
  struct backtrace *bp;

  immediate_quit = handling_signal = 0;
  abort_on_gc = 0;
  if (gc_in_progress || waiting_for_input)
    abort ();

#if 0 /* rms: I don't know why this was here,
	 but it is surely wrong for an error that is handled.  */
#ifdef HAVE_WINDOW_SYSTEM
  if (display_hourglass_p)
    cancel_hourglass ();
#endif
#endif

  /* This hook is used by edebug.  */
  if (! NILP (Vsignal_hook_function)
      && ! NILP (error_symbol))
    {
      /* Edebug takes care of restoring these variables when it exits.  */
      if (lisp_eval_depth + 20 > max_lisp_eval_depth)
	max_lisp_eval_depth = lisp_eval_depth + 20;

      if (SPECPDL_INDEX () + 40 > max_specpdl_size)
	max_specpdl_size = SPECPDL_INDEX () + 40;

      call2 (Vsignal_hook_function, error_symbol, data);
    }

  conditions = Fget (real_error_symbol, Qerror_conditions);

  /* Remember from where signal was called.  Skip over the frame for
     `signal' itself.  If a frame for `error' follows, skip that,
     too.  Don't do this when ERROR_SYMBOL is nil, because that
     is a memory-full error.  */
  Vsignaling_function = Qnil;
  if (backtrace_list && !NILP (error_symbol))
    {
      bp = backtrace_list->next;
      if (bp && bp->function && EQ (*bp->function, Qerror))
	bp = bp->next;
      if (bp && bp->function)
	Vsignaling_function = *bp->function;
    }

  for (h = handlerlist; h; h = h->next)
    {
      clause = find_handler_clause (h->handler, conditions);
      if (!NILP (clause))
	break;
    }

  if (/* Don't run the debugger for a memory-full error.
	 (There is no room in memory to do that!) */
      !NILP (error_symbol)
      && (!NILP (Vdebug_on_signal)
	  /* If no handler is present now, try to run the debugger.  */
	  || NILP (clause)
	  /* A `debug' symbol in the handler list disables the normal
	     suppression of the debugger.  */
	  || (CONSP (clause) && CONSP (XCAR (clause))
	      && !NILP (Fmemq (Qdebug, XCAR (clause))))
	  /* Special handler that means "print a message and run debugger
	     if requested".  */
	  || EQ (h->handler, Qerror)))
    {
      int debugger_called
	= maybe_call_debugger (conditions, error_symbol, data);
      /* We can't return values to code which signaled an error, but we
	 can continue code which has signaled a quit.  */
      if (debugger_called && EQ (real_error_symbol, Qquit))
	return Qnil;
    }

  if (!NILP (clause))
    {
      Lisp_Object unwind_data
	= (NILP (error_symbol) ? data : Fcons (error_symbol, data));

      h->chosen_clause = clause;
      unwind_to_catch (h->tag, unwind_data);
    }
  else
    {
      if (catchlist != 0)
	Fthrow (Qtop_level, Qt);
    }

  if (! NILP (error_symbol))
    data = Fcons (error_symbol, data);

  string = Ferror_message_string (data);
  fatal ("%s", SDATA (string));
}

/* Internal version of Fsignal that never returns.
   Used for anything but Qquit (which can return from Fsignal).  */

void
xsignal (Lisp_Object error_symbol, Lisp_Object data)
{
  Fsignal (error_symbol, data);
  abort ();
}

/* Like xsignal, but takes 0, 1, 2, or 3 args instead of a list.  */

void
xsignal0 (Lisp_Object error_symbol)
{
  xsignal (error_symbol, Qnil);
}

void
xsignal1 (Lisp_Object error_symbol, Lisp_Object arg)
{
  xsignal (error_symbol, list1 (arg));
}

void
xsignal2 (Lisp_Object error_symbol, Lisp_Object arg1, Lisp_Object arg2)
{
  xsignal (error_symbol, list2 (arg1, arg2));
}

void
xsignal3 (Lisp_Object error_symbol, Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  xsignal (error_symbol, list3 (arg1, arg2, arg3));
}

/* Signal `error' with message S, and additional arg ARG.
   If ARG is not a genuine list, make it a one-element list.  */

void
signal_error (const char *s, Lisp_Object arg)
{
  Lisp_Object tortoise, hare;

  hare = tortoise = arg;
  while (CONSP (hare))
    {
      hare = XCDR (hare);
      if (!CONSP (hare))
	break;

      hare = XCDR (hare);
      tortoise = XCDR (tortoise);

      if (EQ (hare, tortoise))
	break;
    }

  if (!NILP (hare))
    arg = Fcons (arg, Qnil);	/* Make it a list.  */

  xsignal (Qerror, Fcons (build_string (s), arg));
}


/* Return nonzero if LIST is a non-nil atom or
   a list containing one of CONDITIONS.  */

static int
wants_debugger (Lisp_Object list, Lisp_Object conditions)
{
  if (NILP (list))
    return 0;
  if (! CONSP (list))
    return 1;

  while (CONSP (conditions))
    {
      Lisp_Object this, tail;
      this = XCAR (conditions);
      for (tail = list; CONSP (tail); tail = XCDR (tail))
	if (EQ (XCAR (tail), this))
	  return 1;
      conditions = XCDR (conditions);
    }
  return 0;
}

/* Return 1 if an error with condition-symbols CONDITIONS,
   and described by SIGNAL-DATA, should skip the debugger
   according to debugger-ignored-errors.  */

static int
skip_debugger (Lisp_Object conditions, Lisp_Object data)
{
  Lisp_Object tail;
  int first_string = 1;
  Lisp_Object error_message;

  error_message = Qnil;
  for (tail = Vdebug_ignored_errors; CONSP (tail); tail = XCDR (tail))
    {
      if (STRINGP (XCAR (tail)))
	{
	  if (first_string)
	    {
	      error_message = Ferror_message_string (data);
	      first_string = 0;
	    }

	  if (fast_string_match (XCAR (tail), error_message) >= 0)
	    return 1;
	}
      else
	{
	  Lisp_Object contail;

	  for (contail = conditions; CONSP (contail); contail = XCDR (contail))
	    if (EQ (XCAR (tail), XCAR (contail)))
	      return 1;
	}
    }

  return 0;
}

/* Call the debugger if calling it is currently enabled for CONDITIONS.
   SIG and DATA describe the signal.  There are two ways to pass them:
    = SIG is the error symbol, and DATA is the rest of the data.
    = SIG is nil, and DATA is (SYMBOL . REST-OF-DATA).
      This is for memory-full errors only.  */
static int
maybe_call_debugger (Lisp_Object conditions, Lisp_Object sig, Lisp_Object data)
{
  Lisp_Object combined_data;

  combined_data = Fcons (sig, data);

  if (
      /* Don't try to run the debugger with interrupts blocked.
	 The editing loop would return anyway.  */
      ! INPUT_BLOCKED_P
      /* Does user want to enter debugger for this kind of error?  */
      && (EQ (sig, Qquit)
	  ? debug_on_quit
	  : wants_debugger (Vdebug_on_error, conditions))
      && ! skip_debugger (conditions, combined_data)
      /* RMS: What's this for?  */
      && when_entered_debugger < num_nonmacro_input_events)
    {
      call_debugger (Fcons (Qerror, Fcons (combined_data, Qnil)));
      return 1;
    }

  return 0;
}

static Lisp_Object
find_handler_clause (Lisp_Object handlers, Lisp_Object conditions)
{
  register Lisp_Object h;

  /* t is used by handlers for all conditions, set up by C code.  */
  if (EQ (handlers, Qt))
    return Qt;

  /* error is used similarly, but means print an error message
     and run the debugger if that is enabled.  */
  if (EQ (handlers, Qerror))
    return Qt;

  for (h = handlers; CONSP (h); h = XCDR (h))
    {
      Lisp_Object handler = XCAR (h);
      Lisp_Object condit, tem;

      if (!CONSP (handler))
	continue;
      condit = XCAR (handler);
      /* Handle a single condition name in handler HANDLER.  */
      if (SYMBOLP (condit))
	{
	  tem = Fmemq (Fcar (handler), conditions);
	  if (!NILP (tem))
	    return handler;
	}
      /* Handle a list of condition names in handler HANDLER.  */
      else if (CONSP (condit))
	{
	  Lisp_Object tail;
	  for (tail = condit; CONSP (tail); tail = XCDR (tail))
	    {
	      tem = Fmemq (XCAR (tail), conditions);
	      if (!NILP (tem))
		return handler;
	    }
	}
    }

  return Qnil;
}


/* Dump an error message; called like vprintf.  */
void
verror (const char *m, va_list ap)
{
  char buf[4000];
  ptrdiff_t size = sizeof buf;
  ptrdiff_t size_max = STRING_BYTES_BOUND + 1;
  char *buffer = buf;
  ptrdiff_t used;
  Lisp_Object string;

  used = evxprintf (&buffer, &size, buf, size_max, m, ap);
  string = make_string (buffer, used);
  if (buffer != buf)
    xfree (buffer);

  xsignal1 (Qerror, string);
}


/* Dump an error message; called like printf.  */

/* VARARGS 1 */
void
error (const char *m, ...)
{
  va_list ap;
  va_start (ap, m);
  verror (m, ap);
  va_end (ap);
}

DEFUN ("commandp", Fcommandp, Scommandp, 1, 2, 0,
       doc: /* Non-nil if FUNCTION makes provisions for interactive calling.
This means it contains a description for how to read arguments to give it.
The value is nil for an invalid function or a symbol with no function
definition.

Interactively callable functions include strings and vectors (treated
as keyboard macros), lambda-expressions that contain a top-level call
to `interactive', autoload definitions made by `autoload' with non-nil
fourth argument, and some of the built-in functions of Lisp.

Also, a symbol satisfies `commandp' if its function definition does so.

If the optional argument FOR-CALL-INTERACTIVELY is non-nil,
then strings and vectors are not accepted.  */)
  (Lisp_Object function, Lisp_Object for_call_interactively)
{
  register Lisp_Object fun;
  register Lisp_Object funcar;
  Lisp_Object if_prop = Qnil;

  fun = function;

  fun = indirect_function (fun); /* Check cycles. */
  if (NILP (fun) || EQ (fun, Qunbound))
    return Qnil;

  /* Check an `interactive-form' property if present, analogous to the
     function-documentation property. */
  fun = function;
  while (SYMBOLP (fun))
    {
      Lisp_Object tmp = Fget (fun, Qinteractive_form);
      if (!NILP (tmp))
	if_prop = Qt;
      fun = Fsymbol_function (fun);
    }

  /* Emacs primitives are interactive if their DEFUN specifies an
     interactive spec.  */
  if (SUBRP (fun))
    return XSUBR (fun)->intspec ? Qt : if_prop;

  /* Bytecode objects are interactive if they are long enough to
     have an element whose index is COMPILED_INTERACTIVE, which is
     where the interactive spec is stored.  */
  else if (COMPILEDP (fun))
    return ((ASIZE (fun) & PSEUDOVECTOR_SIZE_MASK) > COMPILED_INTERACTIVE
	    ? Qt : if_prop);

  /* Strings and vectors are keyboard macros.  */
  if (STRINGP (fun) || VECTORP (fun))
    return (NILP (for_call_interactively) ? Qt : Qnil);

  /* Lists may represent commands.  */
  if (!CONSP (fun))
    return Qnil;
  funcar = XCAR (fun);
  if (EQ (funcar, Qclosure))
    return (!NILP (Fassq (Qinteractive, Fcdr (Fcdr (XCDR (fun)))))
	    ? Qt : if_prop);
  else if (EQ (funcar, Qlambda))
    return !NILP (Fassq (Qinteractive, Fcdr (XCDR (fun)))) ? Qt : if_prop;
  else if (EQ (funcar, Qautoload))
    return !NILP (Fcar (Fcdr (Fcdr (XCDR (fun))))) ? Qt : if_prop;
  else
    return Qnil;
}

DEFUN ("autoload", Fautoload, Sautoload, 2, 5, 0,
       doc: /* Define FUNCTION to autoload from FILE.
FUNCTION is a symbol; FILE is a file name string to pass to `load'.
Third arg DOCSTRING is documentation for the function.
Fourth arg INTERACTIVE if non-nil says function can be called interactively.
Fifth arg TYPE indicates the type of the object:
   nil or omitted says FUNCTION is a function,
   `keymap' says FUNCTION is really a keymap, and
   `macro' or t says FUNCTION is really a macro.
Third through fifth args give info about the real definition.
They default to nil.
If FUNCTION is already defined other than as an autoload,
this does nothing and returns nil.  */)
  (Lisp_Object function, Lisp_Object file, Lisp_Object docstring, Lisp_Object interactive, Lisp_Object type)
{
  CHECK_SYMBOL (function);
  CHECK_STRING (file);

  /* If function is defined and not as an autoload, don't override.  */
  if (!EQ (XSYMBOL (function)->function, Qunbound)
      && !(CONSP (XSYMBOL (function)->function)
	   && EQ (XCAR (XSYMBOL (function)->function), Qautoload)))
    return Qnil;

  if (NILP (Vpurify_flag))
    /* Only add entries after dumping, because the ones before are
       not useful and else we get loads of them from the loaddefs.el.  */
    LOADHIST_ATTACH (Fcons (Qautoload, function));
  else
    /* We don't want the docstring in purespace (instead,
       Snarf-documentation should (hopefully) overwrite it).
       We used to use 0 here, but that leads to accidental sharing in
       purecopy's hash-consing, so we use a (hopefully) unique integer
       instead.  */
    docstring = make_number (XPNTR (function));
  return Ffset (function,
		Fpurecopy (list5 (Qautoload, file, docstring,
				  interactive, type)));
}

Lisp_Object
un_autoload (Lisp_Object oldqueue)
{
  register Lisp_Object queue, first, second;

  /* Queue to unwind is current value of Vautoload_queue.
     oldqueue is the shadowed value to leave in Vautoload_queue.  */
  queue = Vautoload_queue;
  Vautoload_queue = oldqueue;
  while (CONSP (queue))
    {
      first = XCAR (queue);
      second = Fcdr (first);
      first = Fcar (first);
      if (EQ (first, make_number (0)))
	Vfeatures = second;
      else
	Ffset (first, second);
      queue = XCDR (queue);
    }
  return Qnil;
}

/* Load an autoloaded function.
   FUNNAME is the symbol which is the function's name.
   FUNDEF is the autoload definition (a list).  */

void
do_autoload (Lisp_Object fundef, Lisp_Object funname)
{
  int count = SPECPDL_INDEX ();
  Lisp_Object fun;
  struct gcpro gcpro1, gcpro2, gcpro3;

  /* This is to make sure that loadup.el gives a clear picture
     of what files are preloaded and when.  */
  if (! NILP (Vpurify_flag))
    error ("Attempt to autoload %s while preparing to dump",
	   SDATA (SYMBOL_NAME (funname)));

  fun = funname;
  CHECK_SYMBOL (funname);
  GCPRO3 (fun, funname, fundef);

  /* Preserve the match data.  */
  record_unwind_save_match_data ();

  /* If autoloading gets an error (which includes the error of failing
     to define the function being called), we use Vautoload_queue
     to undo function definitions and `provide' calls made by
     the function.  We do this in the specific case of autoloading
     because autoloading is not an explicit request "load this file",
     but rather a request to "call this function".

     The value saved here is to be restored into Vautoload_queue.  */
  record_unwind_protect (un_autoload, Vautoload_queue);
  Vautoload_queue = Qt;
  Fload (Fcar (Fcdr (fundef)), Qnil, Qt, Qnil, Qt);

  /* Once loading finishes, don't undo it.  */
  Vautoload_queue = Qt;
  unbind_to (count, Qnil);

  fun = Findirect_function (fun, Qnil);

  if (!NILP (Fequal (fun, fundef)))
    error ("Autoloading failed to define function %s",
	   SDATA (SYMBOL_NAME (funname)));
  UNGCPRO;
}


DEFUN ("eval", Feval, Seval, 1, 2, 0,
       doc: /* Evaluate FORM and return its value.
If LEXICAL is t, evaluate using lexical scoping.  */)
  (Lisp_Object form, Lisp_Object lexical)
{
  int count = SPECPDL_INDEX ();
  specbind (Qinternal_interpreter_environment,
	    NILP (lexical) ? Qnil : Fcons (Qt, Qnil));
  return unbind_to (count, eval_sub (form));
}

/* Eval a sub-expression of the current expression (i.e. in the same
   lexical scope).  */
Lisp_Object
eval_sub (Lisp_Object form)
{
  Lisp_Object fun, val, original_fun, original_args;
  Lisp_Object funcar;
  struct backtrace backtrace;
  struct gcpro gcpro1, gcpro2, gcpro3;

  if (handling_signal)
    abort ();

  if (SYMBOLP (form))
    {
      /* Look up its binding in the lexical environment.
	 We do not pay attention to the declared_special flag here, since we
	 already did that when let-binding the variable.  */
      Lisp_Object lex_binding
	= !NILP (Vinternal_interpreter_environment) /* Mere optimization!  */
	? Fassq (form, Vinternal_interpreter_environment)
	: Qnil;
      if (CONSP (lex_binding))
	return XCDR (lex_binding);
      else
	return Fsymbol_value (form);
    }

  if (!CONSP (form))
    return form;

  QUIT;
  if ((consing_since_gc > gc_cons_threshold
       && consing_since_gc > gc_relative_threshold)
      ||
      (!NILP (Vmemory_full) && consing_since_gc > memory_full_cons_threshold))
    {
      GCPRO1 (form);
      Fgarbage_collect ();
      UNGCPRO;
    }

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
	max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
	error ("Lisp nesting exceeds `max-lisp-eval-depth'");
    }

  original_fun = Fcar (form);
  original_args = Fcdr (form);

  backtrace.next = backtrace_list;
  backtrace_list = &backtrace;
  backtrace.function = &original_fun; /* This also protects them from gc.  */
  backtrace.args = &original_args;
  backtrace.nargs = UNEVALLED;
  backtrace.debug_on_exit = 0;

  if (debug_on_next_call)
    do_debug_on_call (Qt);

  /* At this point, only original_fun and original_args
     have values that will be used below.  */
 retry:

  /* Optimize for no indirection.  */
  fun = original_fun;
  if (SYMBOLP (fun) && !EQ (fun, Qunbound)
      && (fun = XSYMBOL (fun)->function, SYMBOLP (fun)))
    fun = indirect_function (fun);

  if (SUBRP (fun))
    {
      Lisp_Object numargs;
      Lisp_Object argvals[8];
      Lisp_Object args_left;
      register int i, maxargs;

      args_left = original_args;
      numargs = Flength (args_left);

      CHECK_CONS_LIST ();

      if (XINT (numargs) < XSUBR (fun)->min_args
	  || (XSUBR (fun)->max_args >= 0
	      && XSUBR (fun)->max_args < XINT (numargs)))
	xsignal2 (Qwrong_number_of_arguments, original_fun, numargs);

      else if (XSUBR (fun)->max_args == UNEVALLED)
	val = (XSUBR (fun)->function.aUNEVALLED) (args_left);
      else if (XSUBR (fun)->max_args == MANY)
	{
	  /* Pass a vector of evaluated arguments.  */
	  Lisp_Object *vals;
	  ptrdiff_t argnum = 0;
	  USE_SAFE_ALLOCA;

	  SAFE_ALLOCA_LISP (vals, XINT (numargs));

	  GCPRO3 (args_left, fun, fun);
	  gcpro3.var = vals;
	  gcpro3.nvars = 0;

	  while (!NILP (args_left))
	    {
	      vals[argnum++] = eval_sub (Fcar (args_left));
	      args_left = Fcdr (args_left);
	      gcpro3.nvars = argnum;
	    }

	  backtrace.args = vals;
	  backtrace.nargs = XINT (numargs);

	  val = (XSUBR (fun)->function.aMANY) (XINT (numargs), vals);
	  UNGCPRO;
	  SAFE_FREE ();
	}
      else
	{
	  GCPRO3 (args_left, fun, fun);
	  gcpro3.var = argvals;
	  gcpro3.nvars = 0;

	  maxargs = XSUBR (fun)->max_args;
	  for (i = 0; i < maxargs; args_left = Fcdr (args_left))
	    {
	      argvals[i] = eval_sub (Fcar (args_left));
	      gcpro3.nvars = ++i;
	    }

	  UNGCPRO;

	  backtrace.args = argvals;
	  backtrace.nargs = XINT (numargs);

	  switch (i)
	    {
	    case 0:
	      val = (XSUBR (fun)->function.a0 ());
	      break;
	    case 1:
	      val = (XSUBR (fun)->function.a1 (argvals[0]));
	      break;
	    case 2:
	      val = (XSUBR (fun)->function.a2 (argvals[0], argvals[1]));
	      break;
	    case 3:
	      val = (XSUBR (fun)->function.a3
		     (argvals[0], argvals[1], argvals[2]));
	      break;
	    case 4:
	      val = (XSUBR (fun)->function.a4
		     (argvals[0], argvals[1], argvals[2], argvals[3]));
	      break;
	    case 5:
	      val = (XSUBR (fun)->function.a5
		     (argvals[0], argvals[1], argvals[2], argvals[3],
		      argvals[4]));
	      break;
	    case 6:
	      val = (XSUBR (fun)->function.a6
		     (argvals[0], argvals[1], argvals[2], argvals[3],
		      argvals[4], argvals[5]));
	      break;
	    case 7:
	      val = (XSUBR (fun)->function.a7
		     (argvals[0], argvals[1], argvals[2], argvals[3],
		      argvals[4], argvals[5], argvals[6]));
	      break;

	    case 8:
	      val = (XSUBR (fun)->function.a8
		     (argvals[0], argvals[1], argvals[2], argvals[3],
		      argvals[4], argvals[5], argvals[6], argvals[7]));
	      break;

	    default:
	      /* Someone has created a subr that takes more arguments than
		 is supported by this code.  We need to either rewrite the
		 subr to use a different argument protocol, or add more
		 cases to this switch.  */
	      abort ();
	    }
	}
    }
  else if (COMPILEDP (fun))
    val = apply_lambda (fun, original_args);
  else
    {
      if (EQ (fun, Qunbound))
	xsignal1 (Qvoid_function, original_fun);
      if (!CONSP (fun))
	xsignal1 (Qinvalid_function, original_fun);
      funcar = XCAR (fun);
      if (!SYMBOLP (funcar))
	xsignal1 (Qinvalid_function, original_fun);
      if (EQ (funcar, Qautoload))
	{
	  do_autoload (fun, original_fun);
	  goto retry;
	}
      if (EQ (funcar, Qmacro))
	val = eval_sub (apply1 (Fcdr (fun), original_args));
      else if (EQ (funcar, Qlambda)
	       || EQ (funcar, Qclosure))
	val = apply_lambda (fun, original_args);
      else
	xsignal1 (Qinvalid_function, original_fun);
    }
  CHECK_CONS_LIST ();

  lisp_eval_depth--;
  if (backtrace.debug_on_exit)
    val = call_debugger (Fcons (Qexit, Fcons (val, Qnil)));
  backtrace_list = backtrace.next;

  return val;
}

DEFUN ("apply", Fapply, Sapply, 2, MANY, 0,
       doc: /* Call FUNCTION with our remaining args, using our last arg as list of args.
Then return the value FUNCTION returns.
Thus, (apply '+ 1 2 '(3 4)) returns 10.
usage: (apply FUNCTION &rest ARGUMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t i, numargs;
  register Lisp_Object spread_arg;
  register Lisp_Object *funcall_args;
  Lisp_Object fun, retval;
  struct gcpro gcpro1;
  USE_SAFE_ALLOCA;

  fun = args [0];
  funcall_args = 0;
  spread_arg = args [nargs - 1];
  CHECK_LIST (spread_arg);

  numargs = XINT (Flength (spread_arg));

  if (numargs == 0)
    return Ffuncall (nargs - 1, args);
  else if (numargs == 1)
    {
      args [nargs - 1] = XCAR (spread_arg);
      return Ffuncall (nargs, args);
    }

  numargs += nargs - 2;

  /* Optimize for no indirection.  */
  if (SYMBOLP (fun) && !EQ (fun, Qunbound)
      && (fun = XSYMBOL (fun)->function, SYMBOLP (fun)))
    fun = indirect_function (fun);
  if (EQ (fun, Qunbound))
    {
      /* Let funcall get the error.  */
      fun = args[0];
      goto funcall;
    }

  if (SUBRP (fun))
    {
      if (numargs < XSUBR (fun)->min_args
	  || (XSUBR (fun)->max_args >= 0 && XSUBR (fun)->max_args < numargs))
	goto funcall;		/* Let funcall get the error.  */
      else if (XSUBR (fun)->max_args >= 0 && XSUBR (fun)->max_args > numargs)
	{
	  /* Avoid making funcall cons up a yet another new vector of arguments
	     by explicitly supplying nil's for optional values.  */
	  SAFE_ALLOCA_LISP (funcall_args, 1 + XSUBR (fun)->max_args);
	  for (i = numargs; i < XSUBR (fun)->max_args;)
	    funcall_args[++i] = Qnil;
	  GCPRO1 (*funcall_args);
	  gcpro1.nvars = 1 + XSUBR (fun)->max_args;
	}
    }
 funcall:
  /* We add 1 to numargs because funcall_args includes the
     function itself as well as its arguments.  */
  if (!funcall_args)
    {
      SAFE_ALLOCA_LISP (funcall_args, 1 + numargs);
      GCPRO1 (*funcall_args);
      gcpro1.nvars = 1 + numargs;
    }

  memcpy (funcall_args, args, nargs * sizeof (Lisp_Object));
  /* Spread the last arg we got.  Its first element goes in
     the slot that it used to occupy, hence this value of I.  */
  i = nargs - 1;
  while (!NILP (spread_arg))
    {
      funcall_args [i++] = XCAR (spread_arg);
      spread_arg = XCDR (spread_arg);
    }

  /* By convention, the caller needs to gcpro Ffuncall's args.  */
  retval = Ffuncall (gcpro1.nvars, funcall_args);
  UNGCPRO;
  SAFE_FREE ();

  return retval;
}

/* Run hook variables in various ways.  */

static Lisp_Object
funcall_nil (ptrdiff_t nargs, Lisp_Object *args)
{
  Ffuncall (nargs, args);
  return Qnil;
}

DEFUN ("run-hooks", Frun_hooks, Srun_hooks, 0, MANY, 0,
       doc: /* Run each hook in HOOKS.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments.

Major modes should not use this function directly to run their mode
hook; they should use `run-mode-hooks' instead.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hooks &rest HOOKS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object hook[1];
  ptrdiff_t i;

  for (i = 0; i < nargs; i++)
    {
      hook[0] = args[i];
      run_hook_with_args (1, hook, funcall_nil);
    }

  return Qnil;
}

DEFUN ("run-hook-with-args", Frun_hook_with_args,
       Srun_hook_with_args, 1, MANY, 0,
       doc: /* Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
value, that value may be a function or a list of functions to be
called to run the hook.  If the value is a function, it is called with
the given arguments and its return value is returned.  If it is a list
of functions, those functions are called, in order,
with the given arguments ARGS.
It is best not to depend on the value returned by `run-hook-with-args',
as that may change.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hook-with-args HOOK &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return run_hook_with_args (nargs, args, funcall_nil);
}

DEFUN ("run-hook-with-args-until-success", Frun_hook_with_args_until_success,
       Srun_hook_with_args_until_success, 1, MANY, 0,
       doc: /* Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
value, that value may be a function or a list of functions to be
called to run the hook.  If the value is a function, it is called with
the given arguments and its return value is returned.
If it is a list of functions, those functions are called, in order,
with the given arguments ARGS, until one of them
returns a non-nil value.  Then we return that value.
However, if they all return nil, we return nil.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hook-with-args-until-success HOOK &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return run_hook_with_args (nargs, args, Ffuncall);
}

static Lisp_Object
funcall_not (ptrdiff_t nargs, Lisp_Object *args)
{
  return NILP (Ffuncall (nargs, args)) ? Qt : Qnil;
}

DEFUN ("run-hook-with-args-until-failure", Frun_hook_with_args_until_failure,
       Srun_hook_with_args_until_failure, 1, MANY, 0,
       doc: /* Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
value, that value may be a function or a list of functions to be
called to run the hook.  If the value is a function, it is called with
the given arguments and its return value is returned.
If it is a list of functions, those functions are called, in order,
with the given arguments ARGS, until one of them returns nil.
Then we return nil.  However, if they all return non-nil, we return non-nil.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.
usage: (run-hook-with-args-until-failure HOOK &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return NILP (run_hook_with_args (nargs, args, funcall_not)) ? Qt : Qnil;
}

static Lisp_Object
run_hook_wrapped_funcall (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object tmp = args[0], ret;
  args[0] = args[1];
  args[1] = tmp;
  ret = Ffuncall (nargs, args);
  args[1] = args[0];
  args[0] = tmp;
  return ret;
}

DEFUN ("run-hook-wrapped", Frun_hook_wrapped, Srun_hook_wrapped, 2, MANY, 0,
       doc: /* Run HOOK, passing each function through WRAP-FUNCTION.
I.e. instead of calling each function FUN directly with arguments ARGS,
it calls WRAP-FUNCTION with arguments FUN and ARGS.
As soon as a call to WRAP-FUNCTION returns non-nil, `run-hook-wrapped'
aborts and returns that value.
usage: (run-hook-wrapped HOOK WRAP-FUNCTION &rest ARGS)  */)
     (ptrdiff_t nargs, Lisp_Object *args)
{
  return run_hook_with_args (nargs, args, run_hook_wrapped_funcall);
}

/* ARGS[0] should be a hook symbol.
   Call each of the functions in the hook value, passing each of them
   as arguments all the rest of ARGS (all NARGS - 1 elements).
   FUNCALL specifies how to call each function on the hook.
   The caller (or its caller, etc) must gcpro all of ARGS,
   except that it isn't necessary to gcpro ARGS[0].  */

Lisp_Object
run_hook_with_args (ptrdiff_t nargs, Lisp_Object *args,
		    Lisp_Object (*funcall) (ptrdiff_t nargs, Lisp_Object *args))
{
  Lisp_Object sym, val, ret = Qnil;
  struct gcpro gcpro1, gcpro2, gcpro3;

  /* If we are dying or still initializing,
     don't do anything--it would probably crash if we tried.  */
  if (NILP (Vrun_hooks))
    return Qnil;

  sym = args[0];
  val = find_symbol_value (sym);

  if (EQ (val, Qunbound) || NILP (val))
    return ret;
  else if (!CONSP (val) || EQ (XCAR (val), Qlambda))
    {
      args[0] = val;
      return funcall (nargs, args);
    }
  else
    {
      Lisp_Object global_vals = Qnil;
      GCPRO3 (sym, val, global_vals);

      for (;
	   CONSP (val) && NILP (ret);
	   val = XCDR (val))
	{
	  if (EQ (XCAR (val), Qt))
	    {
	      /* t indicates this hook has a local binding;
		 it means to run the global binding too.  */
	      global_vals = Fdefault_value (sym);
	      if (NILP (global_vals)) continue;

	      if (!CONSP (global_vals) || EQ (XCAR (global_vals), Qlambda))
		{
		  args[0] = global_vals;
		  ret = funcall (nargs, args);
		}
	      else
		{
		  for (;
		       CONSP (global_vals) && NILP (ret);
		       global_vals = XCDR (global_vals))
		    {
		      args[0] = XCAR (global_vals);
		      /* In a global value, t should not occur.  If it does, we
			 must ignore it to avoid an endless loop.  */
		      if (!EQ (args[0], Qt))
			ret = funcall (nargs, args);
		    }
		}
	    }
	  else
	    {
	      args[0] = XCAR (val);
	      ret = funcall (nargs, args);
	    }
	}

      UNGCPRO;
      return ret;
    }
}

/* Run the hook HOOK, giving each function the two args ARG1 and ARG2.  */

void
run_hook_with_args_2 (Lisp_Object hook, Lisp_Object arg1, Lisp_Object arg2)
{
  Lisp_Object temp[3];
  temp[0] = hook;
  temp[1] = arg1;
  temp[2] = arg2;

  Frun_hook_with_args (3, temp);
}

/* Apply fn to arg.  */
Lisp_Object
apply1 (Lisp_Object fn, Lisp_Object arg)
{
  struct gcpro gcpro1;

  GCPRO1 (fn);
  if (NILP (arg))
    RETURN_UNGCPRO (Ffuncall (1, &fn));
  gcpro1.nvars = 2;
  {
    Lisp_Object args[2];
    args[0] = fn;
    args[1] = arg;
    gcpro1.var = args;
    RETURN_UNGCPRO (Fapply (2, args));
  }
}

/* Call function fn on no arguments.  */
Lisp_Object
call0 (Lisp_Object fn)
{
  struct gcpro gcpro1;

  GCPRO1 (fn);
  RETURN_UNGCPRO (Ffuncall (1, &fn));
}

/* Call function fn with 1 argument arg1.  */
/* ARGSUSED */
Lisp_Object
call1 (Lisp_Object fn, Lisp_Object arg1)
{
  struct gcpro gcpro1;
  Lisp_Object args[2];

  args[0] = fn;
  args[1] = arg1;
  GCPRO1 (args[0]);
  gcpro1.nvars = 2;
  RETURN_UNGCPRO (Ffuncall (2, args));
}

/* Call function fn with 2 arguments arg1, arg2.  */
/* ARGSUSED */
Lisp_Object
call2 (Lisp_Object fn, Lisp_Object arg1, Lisp_Object arg2)
{
  struct gcpro gcpro1;
  Lisp_Object args[3];
  args[0] = fn;
  args[1] = arg1;
  args[2] = arg2;
  GCPRO1 (args[0]);
  gcpro1.nvars = 3;
  RETURN_UNGCPRO (Ffuncall (3, args));
}

/* Call function fn with 3 arguments arg1, arg2, arg3.  */
/* ARGSUSED */
Lisp_Object
call3 (Lisp_Object fn, Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
  struct gcpro gcpro1;
  Lisp_Object args[4];
  args[0] = fn;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  GCPRO1 (args[0]);
  gcpro1.nvars = 4;
  RETURN_UNGCPRO (Ffuncall (4, args));
}

/* Call function fn with 4 arguments arg1, arg2, arg3, arg4.  */
/* ARGSUSED */
Lisp_Object
call4 (Lisp_Object fn, Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3,
       Lisp_Object arg4)
{
  struct gcpro gcpro1;
  Lisp_Object args[5];
  args[0] = fn;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  args[4] = arg4;
  GCPRO1 (args[0]);
  gcpro1.nvars = 5;
  RETURN_UNGCPRO (Ffuncall (5, args));
}

/* Call function fn with 5 arguments arg1, arg2, arg3, arg4, arg5.  */
/* ARGSUSED */
Lisp_Object
call5 (Lisp_Object fn, Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3,
       Lisp_Object arg4, Lisp_Object arg5)
{
  struct gcpro gcpro1;
  Lisp_Object args[6];
  args[0] = fn;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  args[4] = arg4;
  args[5] = arg5;
  GCPRO1 (args[0]);
  gcpro1.nvars = 6;
  RETURN_UNGCPRO (Ffuncall (6, args));
}

/* Call function fn with 6 arguments arg1, arg2, arg3, arg4, arg5, arg6.  */
/* ARGSUSED */
Lisp_Object
call6 (Lisp_Object fn, Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3,
       Lisp_Object arg4, Lisp_Object arg5, Lisp_Object arg6)
{
  struct gcpro gcpro1;
  Lisp_Object args[7];
  args[0] = fn;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  args[4] = arg4;
  args[5] = arg5;
  args[6] = arg6;
  GCPRO1 (args[0]);
  gcpro1.nvars = 7;
  RETURN_UNGCPRO (Ffuncall (7, args));
}

/* Call function fn with 7 arguments arg1, arg2, arg3, arg4, arg5, arg6, arg7.  */
/* ARGSUSED */
Lisp_Object
call7 (Lisp_Object fn, Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3,
       Lisp_Object arg4, Lisp_Object arg5, Lisp_Object arg6, Lisp_Object arg7)
{
  struct gcpro gcpro1;
  Lisp_Object args[8];
  args[0] = fn;
  args[1] = arg1;
  args[2] = arg2;
  args[3] = arg3;
  args[4] = arg4;
  args[5] = arg5;
  args[6] = arg6;
  args[7] = arg7;
  GCPRO1 (args[0]);
  gcpro1.nvars = 8;
  RETURN_UNGCPRO (Ffuncall (8, args));
}

/* The caller should GCPRO all the elements of ARGS.  */

DEFUN ("functionp", Ffunctionp, Sfunctionp, 1, 1, 0,
       doc: /* Non-nil if OBJECT is a function.  */)
     (Lisp_Object object)
{
  if (SYMBOLP (object) && !NILP (Ffboundp (object)))
    {
      object = Findirect_function (object, Qt);

      if (CONSP (object) && EQ (XCAR (object), Qautoload))
	{
	  /* Autoloaded symbols are functions, except if they load
	     macros or keymaps.  */
	  int i;
	  for (i = 0; i < 4 && CONSP (object); i++)
	    object = XCDR (object);

	  return (CONSP (object) && !NILP (XCAR (object))) ? Qnil : Qt;
	}
    }

  if (SUBRP (object))
    return (XSUBR (object)->max_args != UNEVALLED) ? Qt : Qnil;
  else if (COMPILEDP (object))
    return Qt;
  else if (CONSP (object))
    {
      Lisp_Object car = XCAR (object);
      return (EQ (car, Qlambda) || EQ (car, Qclosure)) ? Qt : Qnil;
    }
  else
    return Qnil;
}

DEFUN ("funcall", Ffuncall, Sfuncall, 1, MANY, 0,
       doc: /* Call first argument as a function, passing remaining arguments to it.
Return the value that function returns.
Thus, (funcall 'cons 'x 'y) returns (x . y).
usage: (funcall FUNCTION &rest ARGUMENTS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object fun, original_fun;
  Lisp_Object funcar;
  ptrdiff_t numargs = nargs - 1;
  Lisp_Object lisp_numargs;
  Lisp_Object val;
  struct backtrace backtrace;
  register Lisp_Object *internal_args;
  ptrdiff_t i;

  QUIT;
  if ((consing_since_gc > gc_cons_threshold
       && consing_since_gc > gc_relative_threshold)
      ||
      (!NILP (Vmemory_full) && consing_since_gc > memory_full_cons_threshold))
    Fgarbage_collect ();

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
	max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
	error ("Lisp nesting exceeds `max-lisp-eval-depth'");
    }

  backtrace.next = backtrace_list;
  backtrace_list = &backtrace;
  backtrace.function = &args[0];
  backtrace.args = &args[1];
  backtrace.nargs = nargs - 1;
  backtrace.debug_on_exit = 0;

  if (debug_on_next_call)
    do_debug_on_call (Qlambda);

  CHECK_CONS_LIST ();

  original_fun = args[0];

 retry:

  /* Optimize for no indirection.  */
  fun = original_fun;
  if (SYMBOLP (fun) && !EQ (fun, Qunbound)
      && (fun = XSYMBOL (fun)->function, SYMBOLP (fun)))
    fun = indirect_function (fun);

  if (SUBRP (fun))
    {
      if (numargs < XSUBR (fun)->min_args
	  || (XSUBR (fun)->max_args >= 0 && XSUBR (fun)->max_args < numargs))
	{
	  XSETFASTINT (lisp_numargs, numargs);
	  xsignal2 (Qwrong_number_of_arguments, original_fun, lisp_numargs);
	}

      else if (XSUBR (fun)->max_args == UNEVALLED)
	xsignal1 (Qinvalid_function, original_fun);

      else if (XSUBR (fun)->max_args == MANY)
	val = (XSUBR (fun)->function.aMANY) (numargs, args + 1);
      else
	{
	  if (XSUBR (fun)->max_args > numargs)
	    {
	      internal_args = (Lisp_Object *) alloca (XSUBR (fun)->max_args * sizeof (Lisp_Object));
	      memcpy (internal_args, args + 1, numargs * sizeof (Lisp_Object));
	      for (i = numargs; i < XSUBR (fun)->max_args; i++)
		internal_args[i] = Qnil;
	    }
	  else
	    internal_args = args + 1;
	  switch (XSUBR (fun)->max_args)
	    {
	    case 0:
	      val = (XSUBR (fun)->function.a0 ());
	      break;
	    case 1:
	      val = (XSUBR (fun)->function.a1 (internal_args[0]));
	      break;
	    case 2:
	      val = (XSUBR (fun)->function.a2
		     (internal_args[0], internal_args[1]));
	      break;
	    case 3:
	      val = (XSUBR (fun)->function.a3
		     (internal_args[0], internal_args[1], internal_args[2]));
	      break;
	    case 4:
	      val = (XSUBR (fun)->function.a4
		     (internal_args[0], internal_args[1], internal_args[2],
		     internal_args[3]));
	      break;
	    case 5:
	      val = (XSUBR (fun)->function.a5
		     (internal_args[0], internal_args[1], internal_args[2],
		      internal_args[3], internal_args[4]));
	      break;
	    case 6:
	      val = (XSUBR (fun)->function.a6
		     (internal_args[0], internal_args[1], internal_args[2],
		      internal_args[3], internal_args[4], internal_args[5]));
	      break;
	    case 7:
	      val = (XSUBR (fun)->function.a7
		     (internal_args[0], internal_args[1], internal_args[2],
		      internal_args[3], internal_args[4], internal_args[5],
		      internal_args[6]));
	      break;

	    case 8:
	      val = (XSUBR (fun)->function.a8
		     (internal_args[0], internal_args[1], internal_args[2],
		      internal_args[3], internal_args[4], internal_args[5],
		      internal_args[6], internal_args[7]));
	      break;

	    default:

	      /* If a subr takes more than 8 arguments without using MANY
		 or UNEVALLED, we need to extend this function to support it.
		 Until this is done, there is no way to call the function.  */
	      abort ();
	    }
	}
    }
  else if (COMPILEDP (fun))
    val = funcall_lambda (fun, numargs, args + 1);
  else
    {
      if (EQ (fun, Qunbound))
	xsignal1 (Qvoid_function, original_fun);
      if (!CONSP (fun))
	xsignal1 (Qinvalid_function, original_fun);
      funcar = XCAR (fun);
      if (!SYMBOLP (funcar))
	xsignal1 (Qinvalid_function, original_fun);
      if (EQ (funcar, Qlambda)
	  || EQ (funcar, Qclosure))
	val = funcall_lambda (fun, numargs, args + 1);
      else if (EQ (funcar, Qautoload))
	{
	  do_autoload (fun, original_fun);
	  CHECK_CONS_LIST ();
	  goto retry;
	}
      else
	xsignal1 (Qinvalid_function, original_fun);
    }
  CHECK_CONS_LIST ();
  lisp_eval_depth--;
  if (backtrace.debug_on_exit)
    val = call_debugger (Fcons (Qexit, Fcons (val, Qnil)));
  backtrace_list = backtrace.next;
  return val;
}

static Lisp_Object
apply_lambda (Lisp_Object fun, Lisp_Object args)
{
  Lisp_Object args_left;
  ptrdiff_t i, numargs;
  register Lisp_Object *arg_vector;
  struct gcpro gcpro1, gcpro2, gcpro3;
  register Lisp_Object tem;
  USE_SAFE_ALLOCA;

  numargs = XFASTINT (Flength (args));
  SAFE_ALLOCA_LISP (arg_vector, numargs);
  args_left = args;

  GCPRO3 (*arg_vector, args_left, fun);
  gcpro1.nvars = 0;

  for (i = 0; i < numargs; )
    {
      tem = Fcar (args_left), args_left = Fcdr (args_left);
      tem = eval_sub (tem);
      arg_vector[i++] = tem;
      gcpro1.nvars = i;
    }

  UNGCPRO;

  backtrace_list->args = arg_vector;
  backtrace_list->nargs = i;
  tem = funcall_lambda (fun, numargs, arg_vector);

  /* Do the debug-on-exit now, while arg_vector still exists.  */
  if (backtrace_list->debug_on_exit)
    tem = call_debugger (Fcons (Qexit, Fcons (tem, Qnil)));
  /* Don't do it again when we return to eval.  */
  backtrace_list->debug_on_exit = 0;
  SAFE_FREE ();
  return tem;
}

/* Apply a Lisp function FUN to the NARGS evaluated arguments in ARG_VECTOR
   and return the result of evaluation.
   FUN must be either a lambda-expression or a compiled-code object.  */

static Lisp_Object
funcall_lambda (Lisp_Object fun, ptrdiff_t nargs,
		register Lisp_Object *arg_vector)
{
  Lisp_Object val, syms_left, next, lexenv;
  int count = SPECPDL_INDEX ();
  ptrdiff_t i;
  int optional, rest;

  if (CONSP (fun))
    {
      if (EQ (XCAR (fun), Qclosure))
	{
	  fun = XCDR (fun);	/* Drop `closure'.  */
	  lexenv = XCAR (fun);
	  CHECK_LIST_CONS (fun, fun);
	}
      else
	lexenv = Qnil;
      syms_left = XCDR (fun);
      if (CONSP (syms_left))
	syms_left = XCAR (syms_left);
      else
	xsignal1 (Qinvalid_function, fun);
    }
  else if (COMPILEDP (fun))
    {
      syms_left = AREF (fun, COMPILED_ARGLIST);
      if (INTEGERP (syms_left))
	/* A byte-code object with a non-nil `push args' slot means we
	   shouldn't bind any arguments, instead just call the byte-code
	   interpreter directly; it will push arguments as necessary.

	   Byte-code objects with either a non-existent, or a nil value for
	   the `push args' slot (the default), have dynamically-bound
	   arguments, and use the argument-binding code below instead (as do
	   all interpreted functions, even lexically bound ones).  */
	{
	  /* If we have not actually read the bytecode string
	     and constants vector yet, fetch them from the file.  */
	  if (CONSP (AREF (fun, COMPILED_BYTECODE)))
	    Ffetch_bytecode (fun);
	  return exec_byte_code (AREF (fun, COMPILED_BYTECODE),
				 AREF (fun, COMPILED_CONSTANTS),
				 AREF (fun, COMPILED_STACK_DEPTH),
				 syms_left,
				 nargs, arg_vector);
	}
      lexenv = Qnil;
    }
  else
    abort ();

  i = optional = rest = 0;
  for (; CONSP (syms_left); syms_left = XCDR (syms_left))
    {
      QUIT;

      next = XCAR (syms_left);
      if (!SYMBOLP (next))
	xsignal1 (Qinvalid_function, fun);

      if (EQ (next, Qand_rest))
	rest = 1;
      else if (EQ (next, Qand_optional))
	optional = 1;
      else
	{
	  Lisp_Object arg;
	  if (rest)
	    {
	      arg = Flist (nargs - i, &arg_vector[i]);
	      i = nargs;
	    }
	  else if (i < nargs)
	    arg = arg_vector[i++];
	  else if (!optional)
	    xsignal2 (Qwrong_number_of_arguments, fun, make_number (nargs));
	  else
	    arg = Qnil;

	  /* Bind the argument.  */
	  if (!NILP (lexenv) && SYMBOLP (next))
	    /* Lexically bind NEXT by adding it to the lexenv alist.  */
	    lexenv = Fcons (Fcons (next, arg), lexenv);
	  else
	    /* Dynamically bind NEXT.  */
	    specbind (next, arg);
	}
    }

  if (!NILP (syms_left))
    xsignal1 (Qinvalid_function, fun);
  else if (i < nargs)
    xsignal2 (Qwrong_number_of_arguments, fun, make_number (nargs));

  if (!EQ (lexenv, Vinternal_interpreter_environment))
    /* Instantiate a new lexical environment.  */
    specbind (Qinternal_interpreter_environment, lexenv);

  if (CONSP (fun))
    val = Fprogn (XCDR (XCDR (fun)));
  else
    {
      /* If we have not actually read the bytecode string
	 and constants vector yet, fetch them from the file.  */
      if (CONSP (AREF (fun, COMPILED_BYTECODE)))
	Ffetch_bytecode (fun);
      val = exec_byte_code (AREF (fun, COMPILED_BYTECODE),
			    AREF (fun, COMPILED_CONSTANTS),
			    AREF (fun, COMPILED_STACK_DEPTH),
			    Qnil, 0, 0);
    }

  return unbind_to (count, val);
}

DEFUN ("fetch-bytecode", Ffetch_bytecode, Sfetch_bytecode,
       1, 1, 0,
       doc: /* If byte-compiled OBJECT is lazy-loaded, fetch it now.  */)
  (Lisp_Object object)
{
  Lisp_Object tem;

  if (COMPILEDP (object) && CONSP (AREF (object, COMPILED_BYTECODE)))
    {
      tem = read_doc_string (AREF (object, COMPILED_BYTECODE));
      if (!CONSP (tem))
	{
	  tem = AREF (object, COMPILED_BYTECODE);
	  if (CONSP (tem) && STRINGP (XCAR (tem)))
	    error ("Invalid byte code in %s", SDATA (XCAR (tem)));
	  else
	    error ("Invalid byte code");
	}
      ASET (object, COMPILED_BYTECODE, XCAR (tem));
      ASET (object, COMPILED_CONSTANTS, XCDR (tem));
    }
  return object;
}

static void
grow_specpdl (void)
{
  register int count = SPECPDL_INDEX ();
  int max_size =
    min (max_specpdl_size,
	 min (max (PTRDIFF_MAX, SIZE_MAX) / sizeof (struct specbinding),
	      INT_MAX));
  int size;
  if (max_size <= specpdl_size)
    {
      if (max_specpdl_size < 400)
	max_size = max_specpdl_size = 400;
      if (max_size <= specpdl_size)
	signal_error ("Variable binding depth exceeds max-specpdl-size", Qnil);
    }
  size = specpdl_size < max_size / 2 ? 2 * specpdl_size : max_size;
  specpdl = xnrealloc (specpdl, size, sizeof *specpdl);
  specpdl_size = size;
  specpdl_ptr = specpdl + count;
}

/* `specpdl_ptr->symbol' is a field which describes which variable is
   let-bound, so it can be properly undone when we unbind_to.
   It can have the following two shapes:
   - SYMBOL : if it's a plain symbol, it means that we have let-bound
     a symbol that is not buffer-local (at least at the time
     the let binding started).  Note also that it should not be
     aliased (i.e. when let-binding V1 that's aliased to V2, we want
     to record V2 here).
   - (SYMBOL WHERE . BUFFER) : this means that it is a let-binding for
     variable SYMBOL which can be buffer-local.  WHERE tells us
     which buffer is affected (or nil if the let-binding affects the
     global value of the variable) and BUFFER tells us which buffer was
     current (i.e. if WHERE is non-nil, then BUFFER==WHERE, otherwise
     BUFFER did not yet have a buffer-local value).  */

void
specbind (Lisp_Object symbol, Lisp_Object value)
{
  struct Lisp_Symbol *sym;

  eassert (!handling_signal);

  CHECK_SYMBOL (symbol);
  sym = XSYMBOL (symbol);
  if (specpdl_ptr == specpdl + specpdl_size)
    grow_specpdl ();

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS:
      sym = indirect_variable (sym); XSETSYMBOL (symbol, sym); goto start;
    case SYMBOL_PLAINVAL:
      /* The most common case is that of a non-constant symbol with a
	 trivial value.  Make that as fast as we can.  */
      specpdl_ptr->symbol = symbol;
      specpdl_ptr->old_value = SYMBOL_VAL (sym);
      specpdl_ptr->func = NULL;
      ++specpdl_ptr;
      if (!sym->constant)
	SET_SYMBOL_VAL (sym, value);
      else
	set_internal (symbol, value, Qnil, 1);
      break;
    case SYMBOL_LOCALIZED:
      if (SYMBOL_BLV (sym)->frame_local)
	error ("Frame-local vars cannot be let-bound");
    case SYMBOL_FORWARDED:
      {
	Lisp_Object ovalue = find_symbol_value (symbol);
	specpdl_ptr->func = 0;
	specpdl_ptr->old_value = ovalue;

	eassert (sym->redirect != SYMBOL_LOCALIZED
		 || (EQ (SYMBOL_BLV (sym)->where,
			 SYMBOL_BLV (sym)->frame_local ?
			 Fselected_frame () : Fcurrent_buffer ())));

	if (sym->redirect == SYMBOL_LOCALIZED
	    || BUFFER_OBJFWDP (SYMBOL_FWD (sym)))
	  {
	    Lisp_Object where, cur_buf = Fcurrent_buffer ();

	    /* For a local variable, record both the symbol and which
	       buffer's or frame's value we are saving.  */
	    if (!NILP (Flocal_variable_p (symbol, Qnil)))
	      {
		eassert (sym->redirect != SYMBOL_LOCALIZED
			 || (BLV_FOUND (SYMBOL_BLV (sym))
			     && EQ (cur_buf, SYMBOL_BLV (sym)->where)));
		where = cur_buf;
	      }
	    else if (sym->redirect == SYMBOL_LOCALIZED
		     && BLV_FOUND (SYMBOL_BLV (sym)))
	      where = SYMBOL_BLV (sym)->where;
	    else
	      where = Qnil;

	    /* We're not using the `unused' slot in the specbinding
	       structure because this would mean we have to do more
	       work for simple variables.  */
	    /* FIXME: The third value `current_buffer' is only used in
	       let_shadows_buffer_binding_p which is itself only used
	       in set_internal for local_if_set.  */
	    eassert (NILP (where) || EQ (where, cur_buf));
	    specpdl_ptr->symbol = Fcons (symbol, Fcons (where, cur_buf));

	    /* If SYMBOL is a per-buffer variable which doesn't have a
	       buffer-local value here, make the `let' change the global
	       value by changing the value of SYMBOL in all buffers not
	       having their own value.  This is consistent with what
	       happens with other buffer-local variables.  */
	    if (NILP (where)
		&& sym->redirect == SYMBOL_FORWARDED)
	      {
		eassert (BUFFER_OBJFWDP (SYMBOL_FWD (sym)));
		++specpdl_ptr;
		Fset_default (symbol, value);
		return;
	      }
	  }
	else
	  specpdl_ptr->symbol = symbol;

	specpdl_ptr++;
	set_internal (symbol, value, Qnil, 1);
	break;
      }
    default: abort ();
    }
}

void
record_unwind_protect (Lisp_Object (*function) (Lisp_Object), Lisp_Object arg)
{
  eassert (!handling_signal);

  if (specpdl_ptr == specpdl + specpdl_size)
    grow_specpdl ();
  specpdl_ptr->func = function;
  specpdl_ptr->symbol = Qnil;
  specpdl_ptr->old_value = arg;
  specpdl_ptr++;
}

Lisp_Object
unbind_to (int count, Lisp_Object value)
{
  Lisp_Object quitf = Vquit_flag;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (value, quitf);
  Vquit_flag = Qnil;

  while (specpdl_ptr != specpdl + count)
    {
      /* Copy the binding, and decrement specpdl_ptr, before we do
	 the work to unbind it.  We decrement first
	 so that an error in unbinding won't try to unbind
	 the same entry again, and we copy the binding first
	 in case more bindings are made during some of the code we run.  */

      struct specbinding this_binding;
      this_binding = *--specpdl_ptr;

      if (this_binding.func != 0)
	(*this_binding.func) (this_binding.old_value);
      /* If the symbol is a list, it is really (SYMBOL WHERE
	 . CURRENT-BUFFER) where WHERE is either nil, a buffer, or a
	 frame.  If WHERE is a buffer or frame, this indicates we
	 bound a variable that had a buffer-local or frame-local
	 binding.  WHERE nil means that the variable had the default
	 value when it was bound.  CURRENT-BUFFER is the buffer that
	 was current when the variable was bound.  */
      else if (CONSP (this_binding.symbol))
	{
	  Lisp_Object symbol, where;

	  symbol = XCAR (this_binding.symbol);
	  where = XCAR (XCDR (this_binding.symbol));

	  if (NILP (where))
	    Fset_default (symbol, this_binding.old_value);
	  /* If `where' is non-nil, reset the value in the appropriate
	     local binding, but only if that binding still exists.  */
	  else if (BUFFERP (where)
		   ? !NILP (Flocal_variable_p (symbol, where))
		   : !NILP (Fassq (symbol, XFRAME (where)->param_alist)))
	    set_internal (symbol, this_binding.old_value, where, 1);
	}
      /* If variable has a trivial value (no forwarding), we can
	 just set it.  No need to check for constant symbols here,
	 since that was already done by specbind.  */
      else if (XSYMBOL (this_binding.symbol)->redirect == SYMBOL_PLAINVAL)
	SET_SYMBOL_VAL (XSYMBOL (this_binding.symbol),
			this_binding.old_value);
      else
	/* NOTE: we only ever come here if make_local_foo was used for
	   the first time on this var within this let.  */
	Fset_default (this_binding.symbol, this_binding.old_value);
    }

  if (NILP (Vquit_flag) && !NILP (quitf))
    Vquit_flag = quitf;

  UNGCPRO;
  return value;
}

DEFUN ("special-variable-p", Fspecial_variable_p, Sspecial_variable_p, 1, 1, 0,
       doc: /* Return non-nil if SYMBOL's global binding has been declared special.
A special variable is one that will be bound dynamically, even in a
context where binding is lexical by default.  */)
  (Lisp_Object symbol)
{
   CHECK_SYMBOL (symbol);
   return XSYMBOL (symbol)->declared_special ? Qt : Qnil;
}


DEFUN ("backtrace-debug", Fbacktrace_debug, Sbacktrace_debug, 2, 2, 0,
       doc: /* Set the debug-on-exit flag of eval frame LEVEL levels down to FLAG.
The debugger is entered when that frame exits, if the flag is non-nil.  */)
  (Lisp_Object level, Lisp_Object flag)
{
  register struct backtrace *backlist = backtrace_list;
  register int i;

  CHECK_NUMBER (level);

  for (i = 0; backlist && i < XINT (level); i++)
    {
      backlist = backlist->next;
    }

  if (backlist)
    backlist->debug_on_exit = !NILP (flag);

  return flag;
}

DEFUN ("backtrace", Fbacktrace, Sbacktrace, 0, 0, "",
       doc: /* Print a trace of Lisp function calls currently active.
Output stream used is value of `standard-output'.  */)
  (void)
{
  register struct backtrace *backlist = backtrace_list;
  Lisp_Object tail;
  Lisp_Object tem;
  struct gcpro gcpro1;
  Lisp_Object old_print_level = Vprint_level;

  if (NILP (Vprint_level))
    XSETFASTINT (Vprint_level, 8);

  tail = Qnil;
  GCPRO1 (tail);

  while (backlist)
    {
      write_string (backlist->debug_on_exit ? "* " : "  ", 2);
      if (backlist->nargs == UNEVALLED)
	{
	  Fprin1 (Fcons (*backlist->function, *backlist->args), Qnil);
	  write_string ("\n", -1);
	}
      else
	{
	  tem = *backlist->function;
	  Fprin1 (tem, Qnil);	/* This can QUIT.  */
	  write_string ("(", -1);
	  if (backlist->nargs == MANY)
	    {			/* FIXME: Can this happen?  */
	      int i;
	      for (tail = *backlist->args, i = 0;
		   !NILP (tail);
		   tail = Fcdr (tail), i = 1)
		{
		  if (i) write_string (" ", -1);
		  Fprin1 (Fcar (tail), Qnil);
		}
	    }
	  else
	    {
	      ptrdiff_t i;
	      for (i = 0; i < backlist->nargs; i++)
		{
		  if (i) write_string (" ", -1);
		  Fprin1 (backlist->args[i], Qnil);
		}
	    }
	  write_string (")\n", -1);
	}
      backlist = backlist->next;
    }

  Vprint_level = old_print_level;
  UNGCPRO;
  return Qnil;
}

DEFUN ("backtrace-frame", Fbacktrace_frame, Sbacktrace_frame, 1, 1, NULL,
       doc: /* Return the function and arguments NFRAMES up from current execution point.
If that frame has not evaluated the arguments yet (or is a special form),
the value is (nil FUNCTION ARG-FORMS...).
If that frame has evaluated its arguments and called its function already,
the value is (t FUNCTION ARG-VALUES...).
A &rest arg is represented as the tail of the list ARG-VALUES.
FUNCTION is whatever was supplied as car of evaluated list,
or a lambda expression for macro calls.
If NFRAMES is more than the number of frames, the value is nil.  */)
  (Lisp_Object nframes)
{
  register struct backtrace *backlist = backtrace_list;
  register EMACS_INT i;
  Lisp_Object tem;

  CHECK_NATNUM (nframes);

  /* Find the frame requested.  */
  for (i = 0; backlist && i < XFASTINT (nframes); i++)
    backlist = backlist->next;

  if (!backlist)
    return Qnil;
  if (backlist->nargs == UNEVALLED)
    return Fcons (Qnil, Fcons (*backlist->function, *backlist->args));
  else
    {
      if (backlist->nargs == MANY) /* FIXME: Can this happen?  */
	tem = *backlist->args;
      else
	tem = Flist (backlist->nargs, backlist->args);

      return Fcons (Qt, Fcons (*backlist->function, tem));
    }
}


#if BYTE_MARK_STACK
void
mark_backtrace (void)
{
  register struct backtrace *backlist;
  ptrdiff_t i;

  for (backlist = backtrace_list; backlist; backlist = backlist->next)
    {
      mark_object (*backlist->function);

      if (backlist->nargs == UNEVALLED
	  || backlist->nargs == MANY) /* FIXME: Can this happen?  */
	i = 1;
      else
	i = backlist->nargs;
      while (i--)
	mark_object (backlist->args[i]);
    }
}
#endif

void
syms_of_eval (void)
{
  DEFVAR_INT ("max-specpdl-size", max_specpdl_size,
	      doc: /* *Limit on number of Lisp variable bindings and `unwind-protect's.
If Lisp code tries to increase the total number past this amount,
an error is signaled.
You can safely use a value considerably larger than the default value,
if that proves inconveniently small.  However, if you increase it too far,
Emacs could run out of memory trying to make the stack bigger.  */);

  DEFVAR_INT ("max-lisp-eval-depth", max_lisp_eval_depth,
	      doc: /* *Limit on depth in `eval', `apply' and `funcall' before error.

This limit serves to catch infinite recursions for you before they cause
actual stack overflow in C, which would be fatal for Emacs.
You can safely make it considerably larger than its default value,
if that proves inconveniently small.  However, if you increase it too far,
Emacs could overflow the real C stack, and crash.  */);

  DEFVAR_LISP ("quit-flag", Vquit_flag,
	       doc: /* Non-nil causes `eval' to abort, unless `inhibit-quit' is non-nil.
If the value is t, that means do an ordinary quit.
If the value equals `throw-on-input', that means quit by throwing
to the tag specified in `throw-on-input'; it's for handling `while-no-input'.
Typing C-g sets `quit-flag' to t, regardless of `inhibit-quit',
but `inhibit-quit' non-nil prevents anything from taking notice of that.  */);
  Vquit_flag = Qnil;

  DEFVAR_LISP ("inhibit-quit", Vinhibit_quit,
	       doc: /* Non-nil inhibits C-g quitting from happening immediately.
Note that `quit-flag' will still be set by typing C-g,
so a quit will be signaled as soon as `inhibit-quit' is nil.
To prevent this happening, set `quit-flag' to nil
before making `inhibit-quit' nil.  */);
  Vinhibit_quit = Qnil;

  DEFSYM (Qinhibit_quit, "inhibit-quit");
  DEFSYM (Qautoload, "autoload");
  DEFSYM (Qdebug_on_error, "debug-on-error");
  DEFSYM (Qmacro, "macro");
  DEFSYM (Qdeclare, "declare");

  /* Note that the process handling also uses Qexit, but we don't want
     to staticpro it twice, so we just do it here.  */
  DEFSYM (Qexit, "exit");

  DEFSYM (Qinteractive, "interactive");
  DEFSYM (Qcommandp, "commandp");
  DEFSYM (Qdefun, "defun");
  DEFSYM (Qand_rest, "&rest");
  DEFSYM (Qand_optional, "&optional");
  DEFSYM (Qclosure, "closure");
  DEFSYM (Qdebug, "debug");

  DEFVAR_LISP ("debug-on-error", Vdebug_on_error,
	       doc: /* *Non-nil means enter debugger if an error is signaled.
Does not apply to errors handled by `condition-case' or those
matched by `debug-ignored-errors'.
If the value is a list, an error only means to enter the debugger
if one of its condition symbols appears in the list.
When you evaluate an expression interactively, this variable
is temporarily non-nil if `eval-expression-debug-on-error' is non-nil.
The command `toggle-debug-on-error' toggles this.
See also the variable `debug-on-quit'.  */);
  Vdebug_on_error = Qnil;

  DEFVAR_LISP ("debug-ignored-errors", Vdebug_ignored_errors,
    doc: /* *List of errors for which the debugger should not be called.
Each element may be a condition-name or a regexp that matches error messages.
If any element applies to a given error, that error skips the debugger
and just returns to top level.
This overrides the variable `debug-on-error'.
It does not apply to errors handled by `condition-case'.  */);
  Vdebug_ignored_errors = Qnil;

  DEFVAR_BOOL ("debug-on-quit", debug_on_quit,
    doc: /* *Non-nil means enter debugger if quit is signaled (C-g, for example).
Does not apply if quit is handled by a `condition-case'.  */);
  debug_on_quit = 0;

  DEFVAR_BOOL ("debug-on-next-call", debug_on_next_call,
	       doc: /* Non-nil means enter debugger before next `eval', `apply' or `funcall'.  */);

  DEFVAR_BOOL ("debugger-may-continue", debugger_may_continue,
	       doc: /* Non-nil means debugger may continue execution.
This is nil when the debugger is called under circumstances where it
might not be safe to continue.  */);
  debugger_may_continue = 1;

  DEFVAR_LISP ("debugger", Vdebugger,
	       doc: /* Function to call to invoke debugger.
If due to frame exit, args are `exit' and the value being returned;
 this function's value will be returned instead of that.
If due to error, args are `error' and a list of the args to `signal'.
If due to `apply' or `funcall' entry, one arg, `lambda'.
If due to `eval' entry, one arg, t.  */);
  Vdebugger = Qnil;

  DEFVAR_LISP ("signal-hook-function", Vsignal_hook_function,
	       doc: /* If non-nil, this is a function for `signal' to call.
It receives the same arguments that `signal' was given.
The Edebug package uses this to regain control.  */);
  Vsignal_hook_function = Qnil;

  DEFVAR_LISP ("debug-on-signal", Vdebug_on_signal,
	       doc: /* *Non-nil means call the debugger regardless of condition handlers.
Note that `debug-on-error', `debug-on-quit' and friends
still determine whether to handle the particular condition.  */);
  Vdebug_on_signal = Qnil;

  DEFVAR_LISP ("macro-declaration-function", Vmacro_declaration_function,
	       doc: /* Function to process declarations in a macro definition.
The function will be called with two args MACRO and DECL.
MACRO is the name of the macro being defined.
DECL is a list `(declare ...)' containing the declarations.
The value the function returns is not used.  */);
  Vmacro_declaration_function = Qnil;

  /* When lexical binding is being used,
   vinternal_interpreter_environment is non-nil, and contains an alist
   of lexically-bound variable, or (t), indicating an empty
   environment.  The lisp name of this variable would be
   `internal-interpreter-environment' if it weren't hidden.
   Every element of this list can be either a cons (VAR . VAL)
   specifying a lexical binding, or a single symbol VAR indicating
   that this variable should use dynamic scoping.  */
  DEFSYM (Qinternal_interpreter_environment, "internal-interpreter-environment");
  DEFVAR_LISP ("internal-interpreter-environment",
		Vinternal_interpreter_environment,
	       doc: /* If non-nil, the current lexical environment of the lisp interpreter.
When lexical binding is not being used, this variable is nil.
A value of `(t)' indicates an empty environment, otherwise it is an
alist of active lexical bindings.  */);
  Vinternal_interpreter_environment = Qnil;
  /* Don't export this variable to Elisp, so no one can mess with it
     (Just imagine if someone makes it buffer-local).  */
  Funintern (Qinternal_interpreter_environment, Qnil);

  DEFSYM (Vrun_hooks, "run-hooks");

  staticpro (&Vautoload_queue);
  Vautoload_queue = Qnil;
  staticpro (&Vsignaling_function);
  Vsignaling_function = Qnil;

  inhibit_lisp_code = Qnil;

  defsubr (&Sor);
  defsubr (&Sand);
  defsubr (&Sif);
  defsubr (&Scond);
  defsubr (&Sprogn);
  defsubr (&Sprog1);
  defsubr (&Sprog2);
  defsubr (&Ssetq);
  defsubr (&Squote);
  defsubr (&Sfunction);
  defsubr (&Sdefun);
  defsubr (&Sdefmacro);
  defsubr (&Sdefvar);
  defsubr (&Sdefvaralias);
  defsubr (&Sdefconst);
  defsubr (&Suser_variable_p);
  defsubr (&Slet);
  defsubr (&SletX);
  defsubr (&Swhile);
  defsubr (&Smacroexpand);
  defsubr (&Scatch);
  defsubr (&Sthrow);
  defsubr (&Sunwind_protect);
  defsubr (&Scondition_case);
  defsubr (&Ssignal);
  defsubr (&Sinteractive_p);
  defsubr (&Scalled_interactively_p);
  defsubr (&Scommandp);
  defsubr (&Sautoload);
  defsubr (&Seval);
  defsubr (&Sapply);
  defsubr (&Sfuncall);
  defsubr (&Srun_hooks);
  defsubr (&Srun_hook_with_args);
  defsubr (&Srun_hook_with_args_until_success);
  defsubr (&Srun_hook_with_args_until_failure);
  defsubr (&Srun_hook_wrapped);
  defsubr (&Sfetch_bytecode);
  defsubr (&Sbacktrace_debug);
  defsubr (&Sbacktrace);
  defsubr (&Sbacktrace_frame);
  defsubr (&Sspecial_variable_p);
  defsubr (&Sfunctionp);
}
