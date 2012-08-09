/* Primitive operations on Lisp data types for GNU Emacs Lisp interpreter.
   Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2012
                 Free Software Foundation, Inc.

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
#include <signal.h>
#include <stdio.h>
#include <setjmp.h>

#include <intprops.h>

#include "lisp.h"
#include "puresize.h"
#include "character.h"
#include "buffer.h"
#include "keyboard.h"
#include "frame.h"
#include "syssignal.h"
#include "termhooks.h"  /* For FRAME_KBOARD reference in y-or-n-p.  */
#include "font.h"

#include <float.h>
/* If IEEE_FLOATING_POINT isn't defined, default it from FLT_*.  */
#ifndef IEEE_FLOATING_POINT
#if (FLT_RADIX == 2 && FLT_MANT_DIG == 24 \
     && FLT_MIN_EXP == -125 && FLT_MAX_EXP == 128)
#define IEEE_FLOATING_POINT 1
#else
#define IEEE_FLOATING_POINT 0
#endif
#endif

#include <math.h>

Lisp_Object Qnil, Qt, Qquote, Qlambda, Qunbound;
static Lisp_Object Qsubr;
Lisp_Object Qerror_conditions, Qerror_message, Qtop_level;
Lisp_Object Qerror, Qquit, Qargs_out_of_range;
static Lisp_Object Qwrong_type_argument;
Lisp_Object Qvoid_variable, Qvoid_function;
static Lisp_Object Qcyclic_function_indirection;
static Lisp_Object Qcyclic_variable_indirection;
Lisp_Object Qcircular_list;
static Lisp_Object Qsetting_constant;
Lisp_Object Qinvalid_read_syntax;
Lisp_Object Qinvalid_function, Qwrong_number_of_arguments, Qno_catch;
Lisp_Object Qend_of_file, Qarith_error, Qmark_inactive;
Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;
Lisp_Object Qtext_read_only;

Lisp_Object Qintegerp, Qwholenump, Qsymbolp, Qlistp, Qconsp;
static Lisp_Object Qnatnump;
Lisp_Object Qstringp, Qarrayp, Qsequencep, Qbufferp;
Lisp_Object Qchar_or_string_p, Qmarkerp, Qinteger_or_marker_p, Qvectorp;
Lisp_Object Qbuffer_or_string_p;
static Lisp_Object Qkeywordp, Qboundp;
Lisp_Object Qfboundp;
Lisp_Object Qchar_table_p, Qvector_or_char_table_p;

Lisp_Object Qcdr;
static Lisp_Object Qad_advice_info, Qad_activate_internal;

Lisp_Object Qrange_error, Qdomain_error, Qsingularity_error;
Lisp_Object Qoverflow_error, Qunderflow_error;

Lisp_Object Qfloatp;
Lisp_Object Qnumberp, Qnumber_or_marker_p;

Lisp_Object Qinteger;
static Lisp_Object Qsymbol, Qstring, Qcons, Qmarker, Qoverlay;
Lisp_Object Qwindow;
static Lisp_Object Qfloat, Qwindow_configuration;
static Lisp_Object Qprocess;
static Lisp_Object Qcompiled_function, Qframe, Qvector;
Lisp_Object Qbuffer;
static Lisp_Object Qchar_table, Qbool_vector, Qhash_table;
static Lisp_Object Qsubrp, Qmany, Qunevalled;
Lisp_Object Qfont_spec, Qfont_entity, Qfont_object;

Lisp_Object Qinteractive_form;

static void swap_in_symval_forwarding (struct Lisp_Symbol *, struct Lisp_Buffer_Local_Value *);


Lisp_Object
wrong_type_argument (register Lisp_Object predicate, register Lisp_Object value)
{
  /* If VALUE is not even a valid Lisp object, we'd want to abort here
     where we can get a backtrace showing where it came from.  We used
     to try and do that by checking the tagbits, but nowadays all
     tagbits are potentially valid.  */
  /* if ((unsigned int) XTYPE (value) >= Lisp_Type_Limit)
   *   abort (); */

  xsignal2 (Qwrong_type_argument, predicate, value);
}

void
pure_write_error (void)
{
  error ("Attempt to modify read-only object");
}

void
args_out_of_range (Lisp_Object a1, Lisp_Object a2)
{
  xsignal2 (Qargs_out_of_range, a1, a2);
}

void
args_out_of_range_3 (Lisp_Object a1, Lisp_Object a2, Lisp_Object a3)
{
  xsignal3 (Qargs_out_of_range, a1, a2, a3);
}


/* Data type predicates */

DEFUN ("eq", Feq, Seq, 2, 2, 0,
       doc: /* Return t if the two args are the same Lisp object.  */)
  (Lisp_Object obj1, Lisp_Object obj2)
{
  if (EQ (obj1, obj2))
    return Qt;
  return Qnil;
}

DEFUN ("null", Fnull, Snull, 1, 1, 0,
       doc: /* Return t if OBJECT is nil.  */)
  (Lisp_Object object)
{
  if (NILP (object))
    return Qt;
  return Qnil;
}

DEFUN ("type-of", Ftype_of, Stype_of, 1, 1, 0,
       doc: /* Return a symbol representing the type of OBJECT.
The symbol returned names the object's basic type;
for example, (type-of 1) returns `integer'.  */)
  (Lisp_Object object)
{
  switch (XTYPE (object))
    {
    case_Lisp_Int:
      return Qinteger;

    case Lisp_Symbol:
      return Qsymbol;

    case Lisp_String:
      return Qstring;

    case Lisp_Cons:
      return Qcons;

    case Lisp_Misc:
      switch (XMISCTYPE (object))
	{
	case Lisp_Misc_Marker:
	  return Qmarker;
	case Lisp_Misc_Overlay:
	  return Qoverlay;
	case Lisp_Misc_Float:
	  return Qfloat;
	}
      abort ();

    case Lisp_Vectorlike:
      if (WINDOW_CONFIGURATIONP (object))
	return Qwindow_configuration;
      if (PROCESSP (object))
	return Qprocess;
      if (WINDOWP (object))
	return Qwindow;
      if (SUBRP (object))
	return Qsubr;
      if (COMPILEDP (object))
	return Qcompiled_function;
      if (BUFFERP (object))
	return Qbuffer;
      if (CHAR_TABLE_P (object))
	return Qchar_table;
      if (BOOL_VECTOR_P (object))
	return Qbool_vector;
      if (FRAMEP (object))
	return Qframe;
      if (HASH_TABLE_P (object))
	return Qhash_table;
      if (FONT_SPEC_P (object))
	return Qfont_spec;
      if (FONT_ENTITY_P (object))
	return Qfont_entity;
      if (FONT_OBJECT_P (object))
	return Qfont_object;
      return Qvector;

    case Lisp_Float:
      return Qfloat;

    default:
      abort ();
    }
}

DEFUN ("consp", Fconsp, Sconsp, 1, 1, 0,
       doc: /* Return t if OBJECT is a cons cell.  */)
  (Lisp_Object object)
{
  if (CONSP (object))
    return Qt;
  return Qnil;
}

DEFUN ("atom", Fatom, Satom, 1, 1, 0,
       doc: /* Return t if OBJECT is not a cons cell.  This includes nil.  */)
  (Lisp_Object object)
{
  if (CONSP (object))
    return Qnil;
  return Qt;
}

DEFUN ("listp", Flistp, Slistp, 1, 1, 0,
       doc: /* Return t if OBJECT is a list, that is, a cons cell or nil.
Otherwise, return nil.  */)
  (Lisp_Object object)
{
  if (CONSP (object) || NILP (object))
    return Qt;
  return Qnil;
}

DEFUN ("nlistp", Fnlistp, Snlistp, 1, 1, 0,
       doc: /* Return t if OBJECT is not a list.  Lists include nil.  */)
  (Lisp_Object object)
{
  if (CONSP (object) || NILP (object))
    return Qnil;
  return Qt;
}

DEFUN ("symbolp", Fsymbolp, Ssymbolp, 1, 1, 0,
       doc: /* Return t if OBJECT is a symbol.  */)
  (Lisp_Object object)
{
  if (SYMBOLP (object))
    return Qt;
  return Qnil;
}

/* Define this in C to avoid unnecessarily consing up the symbol
   name.  */
DEFUN ("keywordp", Fkeywordp, Skeywordp, 1, 1, 0,
       doc: /* Return t if OBJECT is a keyword.
This means that it is a symbol with a print name beginning with `:'
interned in the initial obarray.  */)
  (Lisp_Object object)
{
  if (SYMBOLP (object)
      && SREF (SYMBOL_NAME (object), 0) == ':'
      && SYMBOL_INTERNED_IN_INITIAL_OBARRAY_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("vectorp", Fvectorp, Svectorp, 1, 1, 0,
       doc: /* Return t if OBJECT is a vector.  */)
  (Lisp_Object object)
{
  if (VECTORP (object))
    return Qt;
  return Qnil;
}

DEFUN ("stringp", Fstringp, Sstringp, 1, 1, 0,
       doc: /* Return t if OBJECT is a string.  */)
  (Lisp_Object object)
{
  if (STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("multibyte-string-p", Fmultibyte_string_p, Smultibyte_string_p,
       1, 1, 0,
       doc: /* Return t if OBJECT is a multibyte string.  */)
  (Lisp_Object object)
{
  if (STRINGP (object) && STRING_MULTIBYTE (object))
    return Qt;
  return Qnil;
}

DEFUN ("char-table-p", Fchar_table_p, Schar_table_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a char-table.  */)
  (Lisp_Object object)
{
  if (CHAR_TABLE_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("vector-or-char-table-p", Fvector_or_char_table_p,
       Svector_or_char_table_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a char-table or vector.  */)
  (Lisp_Object object)
{
  if (VECTORP (object) || CHAR_TABLE_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("bool-vector-p", Fbool_vector_p, Sbool_vector_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a bool-vector.  */)
  (Lisp_Object object)
{
  if (BOOL_VECTOR_P (object))
    return Qt;
  return Qnil;
}

DEFUN ("arrayp", Farrayp, Sarrayp, 1, 1, 0,
       doc: /* Return t if OBJECT is an array (string or vector).  */)
  (Lisp_Object object)
{
  if (ARRAYP (object))
    return Qt;
  return Qnil;
}

DEFUN ("sequencep", Fsequencep, Ssequencep, 1, 1, 0,
       doc: /* Return t if OBJECT is a sequence (list or array).  */)
  (register Lisp_Object object)
{
  if (CONSP (object) || NILP (object) || ARRAYP (object))
    return Qt;
  return Qnil;
}

DEFUN ("bufferp", Fbufferp, Sbufferp, 1, 1, 0,
       doc: /* Return t if OBJECT is an editor buffer.  */)
  (Lisp_Object object)
{
  if (BUFFERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("markerp", Fmarkerp, Smarkerp, 1, 1, 0,
       doc: /* Return t if OBJECT is a marker (editor pointer).  */)
  (Lisp_Object object)
{
  if (MARKERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("subrp", Fsubrp, Ssubrp, 1, 1, 0,
       doc: /* Return t if OBJECT is a built-in function.  */)
  (Lisp_Object object)
{
  if (SUBRP (object))
    return Qt;
  return Qnil;
}

DEFUN ("byte-code-function-p", Fbyte_code_function_p, Sbyte_code_function_p,
       1, 1, 0,
       doc: /* Return t if OBJECT is a byte-compiled function object.  */)
  (Lisp_Object object)
{
  if (COMPILEDP (object))
    return Qt;
  return Qnil;
}

DEFUN ("char-or-string-p", Fchar_or_string_p, Schar_or_string_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a character or a string.  */)
  (register Lisp_Object object)
{
  if (CHARACTERP (object) || STRINGP (object))
    return Qt;
  return Qnil;
}

DEFUN ("integerp", Fintegerp, Sintegerp, 1, 1, 0,
       doc: /* Return t if OBJECT is an integer.  */)
  (Lisp_Object object)
{
  if (INTEGERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("integer-or-marker-p", Finteger_or_marker_p, Sinteger_or_marker_p, 1, 1, 0,
       doc: /* Return t if OBJECT is an integer or a marker (editor pointer).  */)
  (register Lisp_Object object)
{
  if (MARKERP (object) || INTEGERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("natnump", Fnatnump, Snatnump, 1, 1, 0,
       doc: /* Return t if OBJECT is a nonnegative integer.  */)
  (Lisp_Object object)
{
  if (NATNUMP (object))
    return Qt;
  return Qnil;
}

DEFUN ("numberp", Fnumberp, Snumberp, 1, 1, 0,
       doc: /* Return t if OBJECT is a number (floating point or integer).  */)
  (Lisp_Object object)
{
  if (NUMBERP (object))
    return Qt;
  else
    return Qnil;
}

DEFUN ("number-or-marker-p", Fnumber_or_marker_p,
       Snumber_or_marker_p, 1, 1, 0,
       doc: /* Return t if OBJECT is a number or a marker.  */)
  (Lisp_Object object)
{
  if (NUMBERP (object) || MARKERP (object))
    return Qt;
  return Qnil;
}

DEFUN ("floatp", Ffloatp, Sfloatp, 1, 1, 0,
       doc: /* Return t if OBJECT is a floating point number.  */)
  (Lisp_Object object)
{
  if (FLOATP (object))
    return Qt;
  return Qnil;
}


/* Extract and set components of lists */

DEFUN ("car", Fcar, Scar, 1, 1, 0,
       doc: /* Return the car of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `car-safe'.

See Info node `(elisp)Cons Cells' for a discussion of related basic
Lisp concepts such as car, cdr, cons cell and list.  */)
  (register Lisp_Object list)
{
  return CAR (list);
}

DEFUN ("car-safe", Fcar_safe, Scar_safe, 1, 1, 0,
       doc: /* Return the car of OBJECT if it is a cons cell, or else nil.  */)
  (Lisp_Object object)
{
  return CAR_SAFE (object);
}

DEFUN ("cdr", Fcdr, Scdr, 1, 1, 0,
       doc: /* Return the cdr of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `cdr-safe'.

See Info node `(elisp)Cons Cells' for a discussion of related basic
Lisp concepts such as cdr, car, cons cell and list.  */)
  (register Lisp_Object list)
{
  return CDR (list);
}

DEFUN ("cdr-safe", Fcdr_safe, Scdr_safe, 1, 1, 0,
       doc: /* Return the cdr of OBJECT if it is a cons cell, or else nil.  */)
  (Lisp_Object object)
{
  return CDR_SAFE (object);
}

DEFUN ("setcar", Fsetcar, Ssetcar, 2, 2, 0,
       doc: /* Set the car of CELL to be NEWCAR.  Returns NEWCAR.  */)
  (register Lisp_Object cell, Lisp_Object newcar)
{
  CHECK_CONS (cell);
  CHECK_IMPURE (cell);
  XSETCAR (cell, newcar);
  return newcar;
}

DEFUN ("setcdr", Fsetcdr, Ssetcdr, 2, 2, 0,
       doc: /* Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.  */)
  (register Lisp_Object cell, Lisp_Object newcdr)
{
  CHECK_CONS (cell);
  CHECK_IMPURE (cell);
  XSETCDR (cell, newcdr);
  return newcdr;
}

/* Extract and set components of symbols */

DEFUN ("boundp", Fboundp, Sboundp, 1, 1, 0,
       doc: /* Return t if SYMBOL's value is not void.  */)
  (register Lisp_Object symbol)
{
  Lisp_Object valcontents;
  struct Lisp_Symbol *sym;
  CHECK_SYMBOL (symbol);
  sym = XSYMBOL (symbol);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_PLAINVAL: valcontents = SYMBOL_VAL (sym); break;
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_LOCALIZED:
      {
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	if (blv->fwd)
	  /* In set_internal, we un-forward vars when their value is
    	     set to Qunbound. */
    	  return Qt;
	else
	  {
	    swap_in_symval_forwarding (sym, blv);
	    valcontents = BLV_VALUE (blv);
	  }
	break;
      }
    case SYMBOL_FORWARDED:
      /* In set_internal, we un-forward vars when their value is
	 set to Qunbound. */
      return Qt;
    default: abort ();
    }

  return (EQ (valcontents, Qunbound) ? Qnil : Qt);
}

DEFUN ("fboundp", Ffboundp, Sfboundp, 1, 1, 0,
       doc: /* Return t if SYMBOL's function definition is not void.  */)
  (register Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  return (EQ (XSYMBOL (symbol)->function, Qunbound) ? Qnil : Qt);
}

DEFUN ("makunbound", Fmakunbound, Smakunbound, 1, 1, 0,
       doc: /* Make SYMBOL's value be void.
Return SYMBOL.  */)
  (register Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  if (SYMBOL_CONSTANT_P (symbol))
    xsignal1 (Qsetting_constant, symbol);
  Fset (symbol, Qunbound);
  return symbol;
}

DEFUN ("fmakunbound", Ffmakunbound, Sfmakunbound, 1, 1, 0,
       doc: /* Make SYMBOL's function definition be void.
Return SYMBOL.  */)
  (register Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  if (NILP (symbol) || EQ (symbol, Qt))
    xsignal1 (Qsetting_constant, symbol);
  XSYMBOL (symbol)->function = Qunbound;
  return symbol;
}

DEFUN ("symbol-function", Fsymbol_function, Ssymbol_function, 1, 1, 0,
       doc: /* Return SYMBOL's function definition.  Error if that is void.  */)
  (register Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  if (!EQ (XSYMBOL (symbol)->function, Qunbound))
    return XSYMBOL (symbol)->function;
  xsignal1 (Qvoid_function, symbol);
}

DEFUN ("symbol-plist", Fsymbol_plist, Ssymbol_plist, 1, 1, 0,
       doc: /* Return SYMBOL's property list.  */)
  (register Lisp_Object symbol)
{
  CHECK_SYMBOL (symbol);
  return XSYMBOL (symbol)->plist;
}

DEFUN ("symbol-name", Fsymbol_name, Ssymbol_name, 1, 1, 0,
       doc: /* Return SYMBOL's name, a string.  */)
  (register Lisp_Object symbol)
{
  register Lisp_Object name;

  CHECK_SYMBOL (symbol);
  name = SYMBOL_NAME (symbol);
  return name;
}

DEFUN ("fset", Ffset, Sfset, 2, 2, 0,
       doc: /* Set SYMBOL's function definition to DEFINITION, and return DEFINITION.  */)
  (register Lisp_Object symbol, Lisp_Object definition)
{
  register Lisp_Object function;

  CHECK_SYMBOL (symbol);
  if (NILP (symbol) || EQ (symbol, Qt))
    xsignal1 (Qsetting_constant, symbol);

  function = XSYMBOL (symbol)->function;

  if (!NILP (Vautoload_queue) && !EQ (function, Qunbound))
    Vautoload_queue = Fcons (Fcons (symbol, function), Vautoload_queue);

  if (CONSP (function) && EQ (XCAR (function), Qautoload))
    Fput (symbol, Qautoload, XCDR (function));

  XSYMBOL (symbol)->function = definition;
  /* Handle automatic advice activation */
  if (CONSP (XSYMBOL (symbol)->plist) && !NILP (Fget (symbol, Qad_advice_info)))
    {
      call2 (Qad_activate_internal, symbol, Qnil);
      definition = XSYMBOL (symbol)->function;
    }
  return definition;
}

DEFUN ("defalias", Fdefalias, Sdefalias, 2, 3, 0,
       doc: /* Set SYMBOL's function definition to DEFINITION, and return DEFINITION.
Associates the function with the current load file, if any.
The optional third argument DOCSTRING specifies the documentation string
for SYMBOL; if it is omitted or nil, SYMBOL uses the documentation string
determined by DEFINITION.  */)
  (register Lisp_Object symbol, Lisp_Object definition, Lisp_Object docstring)
{
  CHECK_SYMBOL (symbol);
  if (CONSP (XSYMBOL (symbol)->function)
      && EQ (XCAR (XSYMBOL (symbol)->function), Qautoload))
    LOADHIST_ATTACH (Fcons (Qt, symbol));
  definition = Ffset (symbol, definition);
  LOADHIST_ATTACH (Fcons (Qdefun, symbol));
  if (!NILP (docstring))
    Fput (symbol, Qfunction_documentation, docstring);
  return definition;
}

DEFUN ("setplist", Fsetplist, Ssetplist, 2, 2, 0,
       doc: /* Set SYMBOL's property list to NEWPLIST, and return NEWPLIST.  */)
  (register Lisp_Object symbol, Lisp_Object newplist)
{
  CHECK_SYMBOL (symbol);
  XSYMBOL (symbol)->plist = newplist;
  return newplist;
}

DEFUN ("subr-arity", Fsubr_arity, Ssubr_arity, 1, 1, 0,
       doc: /* Return minimum and maximum number of args allowed for SUBR.
SUBR must be a built-in function.
The returned value is a pair (MIN . MAX).  MIN is the minimum number
of args.  MAX is the maximum number or the symbol `many', for a
function with `&rest' args, or `unevalled' for a special form.  */)
  (Lisp_Object subr)
{
  short minargs, maxargs;
  CHECK_SUBR (subr);
  minargs = XSUBR (subr)->min_args;
  maxargs = XSUBR (subr)->max_args;
  if (maxargs == MANY)
    return Fcons (make_number (minargs), Qmany);
  else if (maxargs == UNEVALLED)
    return Fcons (make_number (minargs), Qunevalled);
  else
    return Fcons (make_number (minargs), make_number (maxargs));
}

DEFUN ("subr-name", Fsubr_name, Ssubr_name, 1, 1, 0,
       doc: /* Return name of subroutine SUBR.
SUBR must be a built-in function.  */)
  (Lisp_Object subr)
{
  const char *name;
  CHECK_SUBR (subr);
  name = XSUBR (subr)->symbol_name;
  return build_string (name);
}

DEFUN ("interactive-form", Finteractive_form, Sinteractive_form, 1, 1, 0,
       doc: /* Return the interactive form of CMD or nil if none.
If CMD is not a command, the return value is nil.
Value, if non-nil, is a list \(interactive SPEC).  */)
  (Lisp_Object cmd)
{
  Lisp_Object fun = indirect_function (cmd); /* Check cycles.  */

  if (NILP (fun) || EQ (fun, Qunbound))
    return Qnil;

  /* Use an `interactive-form' property if present, analogous to the
     function-documentation property. */
  fun = cmd;
  while (SYMBOLP (fun))
    {
      Lisp_Object tmp = Fget (fun, Qinteractive_form);
      if (!NILP (tmp))
	return tmp;
      else
	fun = Fsymbol_function (fun);
    }

  if (SUBRP (fun))
    {
      const char *spec = XSUBR (fun)->intspec;
      if (spec)
	return list2 (Qinteractive,
		      (*spec != '(') ? build_string (spec) :
		      Fcar (Fread_from_string (build_string (spec), Qnil, Qnil)));
    }
  else if (COMPILEDP (fun))
    {
      if ((ASIZE (fun) & PSEUDOVECTOR_SIZE_MASK) > COMPILED_INTERACTIVE)
	return list2 (Qinteractive, AREF (fun, COMPILED_INTERACTIVE));
    }
  else if (CONSP (fun))
    {
      Lisp_Object funcar = XCAR (fun);
      if (EQ (funcar, Qclosure))
	return Fassq (Qinteractive, Fcdr (Fcdr (XCDR (fun))));
      else if (EQ (funcar, Qlambda))
	return Fassq (Qinteractive, Fcdr (XCDR (fun)));
      else if (EQ (funcar, Qautoload))
	{
	  struct gcpro gcpro1;
	  GCPRO1 (cmd);
	  do_autoload (fun, cmd);
	  UNGCPRO;
	  return Finteractive_form (cmd);
	}
    }
  return Qnil;
}


/***********************************************************************
		Getting and Setting Values of Symbols
 ***********************************************************************/

/* Return the symbol holding SYMBOL's value.  Signal
   `cyclic-variable-indirection' if SYMBOL's chain of variable
   indirections contains a loop.  */

struct Lisp_Symbol *
indirect_variable (struct Lisp_Symbol *symbol)
{
  struct Lisp_Symbol *tortoise, *hare;

  hare = tortoise = symbol;

  while (hare->redirect == SYMBOL_VARALIAS)
    {
      hare = SYMBOL_ALIAS (hare);
      if (hare->redirect != SYMBOL_VARALIAS)
	break;

      hare = SYMBOL_ALIAS (hare);
      tortoise = SYMBOL_ALIAS (tortoise);

      if (hare == tortoise)
	{
	  Lisp_Object tem;
	  XSETSYMBOL (tem, symbol);
	  xsignal1 (Qcyclic_variable_indirection, tem);
	}
    }

  return hare;
}


DEFUN ("indirect-variable", Findirect_variable, Sindirect_variable, 1, 1, 0,
       doc: /* Return the variable at the end of OBJECT's variable chain.
If OBJECT is a symbol, follow all variable indirections and return the final
variable.  If OBJECT is not a symbol, just return it.
Signal a cyclic-variable-indirection error if there is a loop in the
variable chain of symbols.  */)
  (Lisp_Object object)
{
  if (SYMBOLP (object))
    {
      struct Lisp_Symbol *sym = indirect_variable (XSYMBOL (object));
      XSETSYMBOL (object, sym);
    }
  return object;
}


/* Given the raw contents of a symbol value cell,
   return the Lisp value of the symbol.
   This does not handle buffer-local variables; use
   swap_in_symval_forwarding for that.  */

Lisp_Object
do_symval_forwarding (register union Lisp_Fwd *valcontents)
{
  register Lisp_Object val;
  switch (XFWDTYPE (valcontents))
    {
    case Lisp_Fwd_Int:
      XSETINT (val, *XINTFWD (valcontents)->intvar);
      return val;

    case Lisp_Fwd_Bool:
      return (*XBOOLFWD (valcontents)->boolvar ? Qt : Qnil);

    case Lisp_Fwd_Obj:
      return *XOBJFWD (valcontents)->objvar;

    case Lisp_Fwd_Buffer_Obj:
      return PER_BUFFER_VALUE (current_buffer,
			       XBUFFER_OBJFWD (valcontents)->offset);

    case Lisp_Fwd_Kboard_Obj:
      /* We used to simply use current_kboard here, but from Lisp
	 code, its value is often unexpected.  It seems nicer to
	 allow constructions like this to work as intuitively expected:

	 (with-selected-frame frame
	 (define-key local-function-map "\eOP" [f1]))

	 On the other hand, this affects the semantics of
	 last-command and real-last-command, and people may rely on
	 that.  I took a quick look at the Lisp codebase, and I
	 don't think anything will break.  --lorentey  */
      return *(Lisp_Object *)(XKBOARD_OBJFWD (valcontents)->offset
			      + (char *)FRAME_KBOARD (SELECTED_FRAME ()));
    default: abort ();
    }
}

/* Store NEWVAL into SYMBOL, where VALCONTENTS is found in the value cell
   of SYMBOL.  If SYMBOL is buffer-local, VALCONTENTS should be the
   buffer-independent contents of the value cell: forwarded just one
   step past the buffer-localness.

   BUF non-zero means set the value in buffer BUF instead of the
   current buffer.  This only plays a role for per-buffer variables.  */

static void
store_symval_forwarding (union Lisp_Fwd *valcontents, register Lisp_Object newval, struct buffer *buf)
{
  switch (XFWDTYPE (valcontents))
    {
    case Lisp_Fwd_Int:
      CHECK_NUMBER (newval);
      *XINTFWD (valcontents)->intvar = XINT (newval);
      break;

    case Lisp_Fwd_Bool:
      *XBOOLFWD (valcontents)->boolvar = !NILP (newval);
      break;

    case Lisp_Fwd_Obj:
      *XOBJFWD (valcontents)->objvar = newval;

      /* If this variable is a default for something stored
	 in the buffer itself, such as default-fill-column,
	 find the buffers that don't have local values for it
	 and update them.  */
      if (XOBJFWD (valcontents)->objvar > (Lisp_Object *) &buffer_defaults
	  && XOBJFWD (valcontents)->objvar < (Lisp_Object *) (&buffer_defaults + 1))
	{
	  int offset = ((char *) XOBJFWD (valcontents)->objvar
			- (char *) &buffer_defaults);
	  int idx = PER_BUFFER_IDX (offset);

	  Lisp_Object tail;

	  if (idx <= 0)
	    break;

	  for (tail = Vbuffer_alist; CONSP (tail); tail = XCDR (tail))
	    {
	      Lisp_Object lbuf;
	      struct buffer *b;

	      lbuf = Fcdr (XCAR (tail));
	      if (!BUFFERP (lbuf)) continue;
	      b = XBUFFER (lbuf);

	      if (! PER_BUFFER_VALUE_P (b, idx))
		PER_BUFFER_VALUE (b, offset) = newval;
	    }
	}
      break;

    case Lisp_Fwd_Buffer_Obj:
      {
	int offset = XBUFFER_OBJFWD (valcontents)->offset;
	Lisp_Object type = XBUFFER_OBJFWD (valcontents)->slottype;

	if (!(NILP (type) || NILP (newval)
	      || (XINT (type) == LISP_INT_TAG
		  ? INTEGERP (newval)
		  : XTYPE (newval) == XINT (type))))
	  buffer_slot_type_mismatch (newval, XINT (type));

	if (buf == NULL)
	  buf = current_buffer;
	PER_BUFFER_VALUE (buf, offset) = newval;
      }
      break;

    case Lisp_Fwd_Kboard_Obj:
      {
	char *base = (char *) FRAME_KBOARD (SELECTED_FRAME ());
	char *p = base + XKBOARD_OBJFWD (valcontents)->offset;
	*(Lisp_Object *) p = newval;
      }
      break;

    default:
      abort (); /* goto def; */
    }
}

/* Set up SYMBOL to refer to its global binding.
   This makes it safe to alter the status of other bindings.  */

void
swap_in_global_binding (struct Lisp_Symbol *symbol)
{
  struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (symbol);

  /* Unload the previously loaded binding.  */
  if (blv->fwd)
    SET_BLV_VALUE (blv, do_symval_forwarding (blv->fwd));

  /* Select the global binding in the symbol.  */
  blv->valcell = blv->defcell;
  if (blv->fwd)
    store_symval_forwarding (blv->fwd, XCDR (blv->defcell), NULL);

  /* Indicate that the global binding is set up now.  */
  blv->where = Qnil;
  SET_BLV_FOUND (blv, 0);
}

/* Set up the buffer-local symbol SYMBOL for validity in the current buffer.
   VALCONTENTS is the contents of its value cell,
   which points to a struct Lisp_Buffer_Local_Value.

   Return the value forwarded one step past the buffer-local stage.
   This could be another forwarding pointer.  */

static void
swap_in_symval_forwarding (struct Lisp_Symbol *symbol, struct Lisp_Buffer_Local_Value *blv)
{
  register Lisp_Object tem1;

  eassert (blv == SYMBOL_BLV (symbol));

  tem1 = blv->where;

  if (NILP (tem1)
      || (blv->frame_local
	  ? !EQ (selected_frame, tem1)
	  : current_buffer != XBUFFER (tem1)))
    {

      /* Unload the previously loaded binding.  */
      tem1 = blv->valcell;
      if (blv->fwd)
	SET_BLV_VALUE (blv, do_symval_forwarding (blv->fwd));
      /* Choose the new binding.  */
      {
	Lisp_Object var;
	XSETSYMBOL (var, symbol);
	if (blv->frame_local)
	  {
	    tem1 = assq_no_quit (var, XFRAME (selected_frame)->param_alist);
	    blv->where = selected_frame;
	  }
	else
	  {
	    tem1 = assq_no_quit (var, BVAR (current_buffer, local_var_alist));
	    XSETBUFFER (blv->where, current_buffer);
	  }
      }
      if (!(blv->found = !NILP (tem1)))
	tem1 = blv->defcell;

      /* Load the new binding.  */
      blv->valcell = tem1;
      if (blv->fwd)
	store_symval_forwarding (blv->fwd, BLV_VALUE (blv), NULL);
    }
}

/* Find the value of a symbol, returning Qunbound if it's not bound.
   This is helpful for code which just wants to get a variable's value
   if it has one, without signaling an error.
   Note that it must not be possible to quit
   within this function.  Great care is required for this.  */

Lisp_Object
find_symbol_value (Lisp_Object symbol)
{
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (symbol);
  sym = XSYMBOL (symbol);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return SYMBOL_VAL (sym);
    case SYMBOL_LOCALIZED:
      {
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	swap_in_symval_forwarding (sym, blv);
	return blv->fwd ? do_symval_forwarding (blv->fwd) : BLV_VALUE (blv);
      }
      /* FALLTHROUGH */
    case SYMBOL_FORWARDED:
      return do_symval_forwarding (SYMBOL_FWD (sym));
    default: abort ();
    }
}

DEFUN ("symbol-value", Fsymbol_value, Ssymbol_value, 1, 1, 0,
       doc: /* Return SYMBOL's value.  Error if that is void.  */)
  (Lisp_Object symbol)
{
  Lisp_Object val;

  val = find_symbol_value (symbol);
  if (!EQ (val, Qunbound))
    return val;

  xsignal1 (Qvoid_variable, symbol);
}

DEFUN ("set", Fset, Sset, 2, 2, 0,
       doc: /* Set SYMBOL's value to NEWVAL, and return NEWVAL.  */)
  (register Lisp_Object symbol, Lisp_Object newval)
{
  set_internal (symbol, newval, Qnil, 0);
  return newval;
}

/* Return 1 if SYMBOL currently has a let-binding
   which was made in the buffer that is now current.  */

static int
let_shadows_buffer_binding_p (struct Lisp_Symbol *symbol)
{
  struct specbinding *p;

  for (p = specpdl_ptr - 1; p >= specpdl; p--)
    if (p->func == NULL
	&& CONSP (p->symbol))
      {
	struct Lisp_Symbol *let_bound_symbol = XSYMBOL (XCAR (p->symbol));
	eassert (let_bound_symbol->redirect != SYMBOL_VARALIAS);
	if (symbol == let_bound_symbol
	    && XBUFFER (XCDR (XCDR (p->symbol))) == current_buffer)
	  break;
      }

  return p >= specpdl;
}

static int
let_shadows_global_binding_p (Lisp_Object symbol)
{
  struct specbinding *p;

  for (p = specpdl_ptr - 1; p >= specpdl; p--)
    if (p->func == NULL && EQ (p->symbol, symbol))
      break;

  return p >= specpdl;
}

/* Store the value NEWVAL into SYMBOL.
   If buffer/frame-locality is an issue, WHERE specifies which context to use.
   (nil stands for the current buffer/frame).

   If BINDFLAG is zero, then if this symbol is supposed to become
   local in every buffer where it is set, then we make it local.
   If BINDFLAG is nonzero, we don't do that.  */

void
set_internal (register Lisp_Object symbol, register Lisp_Object newval, register Lisp_Object where, int bindflag)
{
  int voide = EQ (newval, Qunbound);
  struct Lisp_Symbol *sym;
  Lisp_Object tem1;

  /* If restoring in a dead buffer, do nothing.  */
  /* if (BUFFERP (where) && NILP (XBUFFER (where)->name))
      return; */

  CHECK_SYMBOL (symbol);
  if (SYMBOL_CONSTANT_P (symbol))
    {
      if (NILP (Fkeywordp (symbol))
	  || !EQ (newval, Fsymbol_value (symbol)))
	xsignal1 (Qsetting_constant, symbol);
      else
	/* Allow setting keywords to their own value.  */
	return;
    }

  sym = XSYMBOL (symbol);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: SET_SYMBOL_VAL (sym , newval); return;
    case SYMBOL_LOCALIZED:
      {
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	if (NILP (where))
	  {
	    if (blv->frame_local)
	      where = selected_frame;
	    else
	      XSETBUFFER (where, current_buffer);
	  }
	/* If the current buffer is not the buffer whose binding is
	   loaded, or if there may be frame-local bindings and the frame
	   isn't the right one, or if it's a Lisp_Buffer_Local_Value and
	   the default binding is loaded, the loaded binding may be the
	   wrong one.  */
	if (!EQ (blv->where, where)
	    /* Also unload a global binding (if the var is local_if_set). */
	    || (EQ (blv->valcell, blv->defcell)))
	  {
	    /* The currently loaded binding is not necessarily valid.
	       We need to unload it, and choose a new binding.  */

	    /* Write out `realvalue' to the old loaded binding.  */
	    if (blv->fwd)
	      SET_BLV_VALUE (blv, do_symval_forwarding (blv->fwd));

	    /* Find the new binding.  */
	    XSETSYMBOL (symbol, sym); /* May have changed via aliasing.  */
	    tem1 = Fassq (symbol,
			  (blv->frame_local
			   ? XFRAME (where)->param_alist
			   : BVAR (XBUFFER (where), local_var_alist)));
	    blv->where = where;
	    blv->found = 1;

	    if (NILP (tem1))
	      {
		/* This buffer still sees the default value.  */

		/* If the variable is a Lisp_Some_Buffer_Local_Value,
		   or if this is `let' rather than `set',
		   make CURRENT-ALIST-ELEMENT point to itself,
		   indicating that we're seeing the default value.
		   Likewise if the variable has been let-bound
		   in the current buffer.  */
		if (bindflag || !blv->local_if_set
		    || let_shadows_buffer_binding_p (sym))
		  {
		    blv->found = 0;
		    tem1 = blv->defcell;
		  }
		/* If it's a local_if_set, being set not bound,
		   and we're not within a let that was made for this buffer,
		   create a new buffer-local binding for the variable.
		   That means, give this buffer a new assoc for a local value
		   and load that binding.  */
		else
		  {
		    /* local_if_set is only supported for buffer-local
		       bindings, not for frame-local bindings.  */
		    eassert (!blv->frame_local);
		    tem1 = Fcons (symbol, XCDR (blv->defcell));
		    BVAR (XBUFFER (where), local_var_alist)
		      = Fcons (tem1, BVAR (XBUFFER (where), local_var_alist));
		  }
	      }

	    /* Record which binding is now loaded.  */
	    blv->valcell = tem1;
	  }

	/* Store the new value in the cons cell.  */
	SET_BLV_VALUE (blv, newval);

	if (blv->fwd)
	  {
	    if (voide)
	      /* If storing void (making the symbol void), forward only through
		 buffer-local indicator, not through Lisp_Objfwd, etc.  */
	      blv->fwd = NULL;
	    else
	      store_symval_forwarding (blv->fwd, newval,
				       BUFFERP (where)
				       ? XBUFFER (where) : current_buffer);
	  }
	break;
      }
    case SYMBOL_FORWARDED:
      {
	struct buffer *buf
	  = BUFFERP (where) ? XBUFFER (where) : current_buffer;
	union Lisp_Fwd *innercontents = SYMBOL_FWD (sym);
	if (BUFFER_OBJFWDP (innercontents))
	  {
	    int offset = XBUFFER_OBJFWD (innercontents)->offset;
	    int idx = PER_BUFFER_IDX (offset);
	    if (idx > 0
		&& !bindflag
		&& !let_shadows_buffer_binding_p (sym))
	      SET_PER_BUFFER_VALUE_P (buf, idx, 1);
	  }

	if (voide)
	  { /* If storing void (making the symbol void), forward only through
	       buffer-local indicator, not through Lisp_Objfwd, etc.  */
	    sym->redirect = SYMBOL_PLAINVAL;
	    SET_SYMBOL_VAL (sym, newval);
	  }
	else
	  store_symval_forwarding (/* sym, */ innercontents, newval, buf);
	break;
      }
    default: abort ();
    }
  return;
}

/* Access or set a buffer-local symbol's default value.  */

/* Return the default value of SYMBOL, but don't check for voidness.
   Return Qunbound if it is void.  */

static Lisp_Object
default_value (Lisp_Object symbol)
{
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (symbol);
  sym = XSYMBOL (symbol);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return SYMBOL_VAL (sym);
    case SYMBOL_LOCALIZED:
      {
	/* If var is set up for a buffer that lacks a local value for it,
	   the current value is nominally the default value.
	   But the `realvalue' slot may be more up to date, since
	   ordinary setq stores just that slot.  So use that.  */
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	if (blv->fwd && EQ (blv->valcell, blv->defcell))
	  return do_symval_forwarding (blv->fwd);
	else
	  return XCDR (blv->defcell);
      }
    case SYMBOL_FORWARDED:
      {
	union Lisp_Fwd *valcontents = SYMBOL_FWD (sym);

	/* For a built-in buffer-local variable, get the default value
	   rather than letting do_symval_forwarding get the current value.  */
	if (BUFFER_OBJFWDP (valcontents))
	  {
	    int offset = XBUFFER_OBJFWD (valcontents)->offset;
	    if (PER_BUFFER_IDX (offset) != 0)
	      return PER_BUFFER_DEFAULT (offset);
	  }

	/* For other variables, get the current value.  */
	return do_symval_forwarding (valcontents);
      }
    default: abort ();
    }
}

DEFUN ("default-boundp", Fdefault_boundp, Sdefault_boundp, 1, 1, 0,
       doc: /* Return t if SYMBOL has a non-void default value.
This is the value that is seen in buffers that do not have their own values
for this variable.  */)
  (Lisp_Object symbol)
{
  register Lisp_Object value;

  value = default_value (symbol);
  return (EQ (value, Qunbound) ? Qnil : Qt);
}

DEFUN ("default-value", Fdefault_value, Sdefault_value, 1, 1, 0,
       doc: /* Return SYMBOL's default value.
This is the value that is seen in buffers that do not have their own values
for this variable.  The default value is meaningful for variables with
local bindings in certain buffers.  */)
  (Lisp_Object symbol)
{
  register Lisp_Object value;

  value = default_value (symbol);
  if (!EQ (value, Qunbound))
    return value;

  xsignal1 (Qvoid_variable, symbol);
}

DEFUN ("set-default", Fset_default, Sset_default, 2, 2, 0,
       doc: /* Set SYMBOL's default value to VALUE.  SYMBOL and VALUE are evaluated.
The default value is seen in buffers that do not have their own values
for this variable.  */)
  (Lisp_Object symbol, Lisp_Object value)
{
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (symbol);
  if (SYMBOL_CONSTANT_P (symbol))
    {
      if (NILP (Fkeywordp (symbol))
	  || !EQ (value, Fdefault_value (symbol)))
	xsignal1 (Qsetting_constant, symbol);
      else
	/* Allow setting keywords to their own value.  */
	return value;
    }
  sym = XSYMBOL (symbol);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return Fset (symbol, value);
    case SYMBOL_LOCALIZED:
      {
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);

	/* Store new value into the DEFAULT-VALUE slot.  */
	XSETCDR (blv->defcell, value);

	/* If the default binding is now loaded, set the REALVALUE slot too.  */
	if (blv->fwd && EQ (blv->defcell, blv->valcell))
	  store_symval_forwarding (blv->fwd, value, NULL);
	return value;
      }
    case SYMBOL_FORWARDED:
      {
	union Lisp_Fwd *valcontents = SYMBOL_FWD (sym);

	/* Handle variables like case-fold-search that have special slots
	   in the buffer.
	   Make them work apparently like Lisp_Buffer_Local_Value variables.  */
	if (BUFFER_OBJFWDP (valcontents))
	  {
	    int offset = XBUFFER_OBJFWD (valcontents)->offset;
	    int idx = PER_BUFFER_IDX (offset);

	    PER_BUFFER_DEFAULT (offset) = value;

	    /* If this variable is not always local in all buffers,
	       set it in the buffers that don't nominally have a local value.  */
	    if (idx > 0)
	      {
		struct buffer *b;

		for (b = all_buffers; b; b = b->header.next.buffer)
		  if (!PER_BUFFER_VALUE_P (b, idx))
		    PER_BUFFER_VALUE (b, offset) = value;
	      }
	    return value;
	  }
	else
	  return Fset (symbol, value);
      }
    default: abort ();
    }
}

DEFUN ("setq-default", Fsetq_default, Ssetq_default, 0, UNEVALLED, 0,
       doc: /* Set the default value of variable VAR to VALUE.
VAR, the variable name, is literal (not evaluated);
VALUE is an expression: it is evaluated and its value returned.
The default value of a variable is seen in buffers
that do not have their own values for the variable.

More generally, you can use multiple variables and values, as in
  (setq-default VAR VALUE VAR VALUE...)
This sets each VAR's default value to the corresponding VALUE.
The VALUE for the Nth VAR can refer to the new default values
of previous VARs.
usage: (setq-default [VAR VALUE]...)  */)
  (Lisp_Object args)
{
  register Lisp_Object args_left;
  register Lisp_Object val, symbol;
  struct gcpro gcpro1;

  if (NILP (args))
    return Qnil;

  args_left = args;
  GCPRO1 (args);

  do
    {
      val = eval_sub (Fcar (Fcdr (args_left)));
      symbol = XCAR (args_left);
      Fset_default (symbol, val);
      args_left = Fcdr (XCDR (args_left));
    }
  while (!NILP (args_left));

  UNGCPRO;
  return val;
}

/* Lisp functions for creating and removing buffer-local variables.  */

union Lisp_Val_Fwd
  {
    Lisp_Object value;
    union Lisp_Fwd *fwd;
  };

static struct Lisp_Buffer_Local_Value *
make_blv (struct Lisp_Symbol *sym, int forwarded, union Lisp_Val_Fwd valcontents)
{
  struct Lisp_Buffer_Local_Value *blv
    = xmalloc (sizeof (struct Lisp_Buffer_Local_Value));
  Lisp_Object symbol;
  Lisp_Object tem;

 XSETSYMBOL (symbol, sym);
 tem = Fcons (symbol, (forwarded
                       ? do_symval_forwarding (valcontents.fwd)
                       : valcontents.value));

  /* Buffer_Local_Values cannot have as realval a buffer-local
     or keyboard-local forwarding.  */
  eassert (!(forwarded && BUFFER_OBJFWDP (valcontents.fwd)));
  eassert (!(forwarded && KBOARD_OBJFWDP (valcontents.fwd)));
  blv->fwd = forwarded ? valcontents.fwd : NULL;
  blv->where = Qnil;
  blv->frame_local = 0;
  blv->local_if_set = 0;
  blv->defcell = tem;
  blv->valcell = tem;
  SET_BLV_FOUND (blv, 0);
  return blv;
}

DEFUN ("make-variable-buffer-local", Fmake_variable_buffer_local,
       Smake_variable_buffer_local, 1, 1, "vMake Variable Buffer Local: ",
       doc: /* Make VARIABLE become buffer-local whenever it is set.
At any time, the value for the current buffer is in effect,
unless the variable has never been set in this buffer,
in which case the default value is in effect.
Note that binding the variable with `let', or setting it while
a `let'-style binding made in this buffer is in effect,
does not make the variable buffer-local.  Return VARIABLE.

In most cases it is better to use `make-local-variable',
which makes a variable local in just one buffer.

The function `default-value' gets the default value and `set-default' sets it.  */)
  (register Lisp_Object variable)
{
  struct Lisp_Symbol *sym;
  struct Lisp_Buffer_Local_Value *blv = NULL;
  union Lisp_Val_Fwd valcontents IF_LINT (= {0});
  int forwarded IF_LINT (= 0);

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL:
      forwarded = 0; valcontents.value = SYMBOL_VAL (sym);
      if (EQ (valcontents.value, Qunbound))
	valcontents.value = Qnil;
      break;
    case SYMBOL_LOCALIZED:
      blv = SYMBOL_BLV (sym);
      if (blv->frame_local)
	error ("Symbol %s may not be buffer-local",
	       SDATA (SYMBOL_NAME (variable)));
      break;
    case SYMBOL_FORWARDED:
      forwarded = 1; valcontents.fwd = SYMBOL_FWD (sym);
      if (KBOARD_OBJFWDP (valcontents.fwd))
	error ("Symbol %s may not be buffer-local",
	       SDATA (SYMBOL_NAME (variable)));
      else if (BUFFER_OBJFWDP (valcontents.fwd))
	return variable;
      break;
    default: abort ();
    }

  if (sym->constant)
    error ("Symbol %s may not be buffer-local", SDATA (SYMBOL_NAME (variable)));

  if (!blv)
    {
      blv = make_blv (sym, forwarded, valcontents);
      sym->redirect = SYMBOL_LOCALIZED;
      SET_SYMBOL_BLV (sym, blv);
      {
	Lisp_Object symbol;
	XSETSYMBOL (symbol, sym); /* In case `variable' is aliased.  */
	if (let_shadows_global_binding_p (symbol))
	  message ("Making %s buffer-local while let-bound!",
		   SDATA (SYMBOL_NAME (variable)));
      }
    }

  blv->local_if_set = 1;
  return variable;
}

DEFUN ("make-local-variable", Fmake_local_variable, Smake_local_variable,
       1, 1, "vMake Local Variable: ",
       doc: /* Make VARIABLE have a separate value in the current buffer.
Other buffers will continue to share a common default value.
\(The buffer-local value of VARIABLE starts out as the same value
VARIABLE previously had.  If VARIABLE was void, it remains void.\)
Return VARIABLE.

If the variable is already arranged to become local when set,
this function causes a local value to exist for this buffer,
just as setting the variable would do.

This function returns VARIABLE, and therefore
  (set (make-local-variable 'VARIABLE) VALUE-EXP)
works.

See also `make-variable-buffer-local'.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument.  */)
  (register Lisp_Object variable)
{
  register Lisp_Object tem;
  int forwarded IF_LINT (= 0);
  union Lisp_Val_Fwd valcontents IF_LINT (= {0});
  struct Lisp_Symbol *sym;
  struct Lisp_Buffer_Local_Value *blv = NULL;

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL:
      forwarded = 0; valcontents.value = SYMBOL_VAL (sym); break;
    case SYMBOL_LOCALIZED:
      blv = SYMBOL_BLV (sym);
      if (blv->frame_local)
	error ("Symbol %s may not be buffer-local",
	       SDATA (SYMBOL_NAME (variable)));
      break;
    case SYMBOL_FORWARDED:
      forwarded = 1; valcontents.fwd = SYMBOL_FWD (sym);
      if (KBOARD_OBJFWDP (valcontents.fwd))
	error ("Symbol %s may not be buffer-local",
	       SDATA (SYMBOL_NAME (variable)));
      break;
    default: abort ();
    }

  if (sym->constant)
    error ("Symbol %s may not be buffer-local",
	   SDATA (SYMBOL_NAME (variable)));

  if (blv ? blv->local_if_set
      : (forwarded && BUFFER_OBJFWDP (valcontents.fwd)))
    {
      tem = Fboundp (variable);
      /* Make sure the symbol has a local value in this particular buffer,
	 by setting it to the same value it already has.  */
      Fset (variable, (EQ (tem, Qt) ? Fsymbol_value (variable) : Qunbound));
      return variable;
    }
  if (!blv)
    {
      blv = make_blv (sym, forwarded, valcontents);
      sym->redirect = SYMBOL_LOCALIZED;
      SET_SYMBOL_BLV (sym, blv);
      {
	Lisp_Object symbol;
	XSETSYMBOL (symbol, sym); /* In case `variable' is aliased.  */
	if (let_shadows_global_binding_p (symbol))
	  message ("Making %s local to %s while let-bound!",
		   SDATA (SYMBOL_NAME (variable)),
		   SDATA (BVAR (current_buffer, name)));
      }
    }

  /* Make sure this buffer has its own value of symbol.  */
  XSETSYMBOL (variable, sym);	/* Update in case of aliasing.  */
  tem = Fassq (variable, BVAR (current_buffer, local_var_alist));
  if (NILP (tem))
    {
      if (let_shadows_buffer_binding_p (sym))
	message ("Making %s buffer-local while locally let-bound!",
		 SDATA (SYMBOL_NAME (variable)));

      /* Swap out any local binding for some other buffer, and make
	 sure the current value is permanently recorded, if it's the
	 default value.  */
      find_symbol_value (variable);

      BVAR (current_buffer, local_var_alist)
        = Fcons (Fcons (variable, XCDR (blv->defcell)),
		 BVAR (current_buffer, local_var_alist));

      /* Make sure symbol does not think it is set up for this buffer;
	 force it to look once again for this buffer's value.  */
      if (current_buffer == XBUFFER (blv->where))
	blv->where = Qnil;
      /* blv->valcell = blv->defcell;
       * SET_BLV_FOUND (blv, 0); */
      blv->found = 0;
    }

  /* If the symbol forwards into a C variable, then load the binding
     for this buffer now.  If C code modifies the variable before we
     load the binding in, then that new value will clobber the default
     binding the next time we unload it.  */
  if (blv->fwd)
    swap_in_symval_forwarding (sym, blv);

  return variable;
}

DEFUN ("kill-local-variable", Fkill_local_variable, Skill_local_variable,
       1, 1, "vKill Local Variable: ",
       doc: /* Make VARIABLE no longer have a separate value in the current buffer.
From now on the default value will apply in this buffer.  Return VARIABLE.  */)
  (register Lisp_Object variable)
{
  register Lisp_Object tem;
  struct Lisp_Buffer_Local_Value *blv;
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return variable;
    case SYMBOL_FORWARDED:
      {
	union Lisp_Fwd *valcontents = SYMBOL_FWD (sym);
	if (BUFFER_OBJFWDP (valcontents))
	  {
	    int offset = XBUFFER_OBJFWD (valcontents)->offset;
	    int idx = PER_BUFFER_IDX (offset);

	    if (idx > 0)
	      {
		SET_PER_BUFFER_VALUE_P (current_buffer, idx, 0);
		PER_BUFFER_VALUE (current_buffer, offset)
		  = PER_BUFFER_DEFAULT (offset);
	      }
	  }
	return variable;
      }
    case SYMBOL_LOCALIZED:
      blv = SYMBOL_BLV (sym);
      if (blv->frame_local)
	return variable;
      break;
    default: abort ();
    }

  /* Get rid of this buffer's alist element, if any.  */
  XSETSYMBOL (variable, sym);	/* Propagate variable indirection.  */
  tem = Fassq (variable, BVAR (current_buffer, local_var_alist));
  if (!NILP (tem))
    BVAR (current_buffer, local_var_alist)
      = Fdelq (tem, BVAR (current_buffer, local_var_alist));

  /* If the symbol is set up with the current buffer's binding
     loaded, recompute its value.  We have to do it now, or else
     forwarded objects won't work right.  */
  {
    Lisp_Object buf; XSETBUFFER (buf, current_buffer);
    if (EQ (buf, blv->where))
      {
	blv->where = Qnil;
	/* blv->valcell = blv->defcell;
	 * SET_BLV_FOUND (blv, 0); */
	blv->found = 0;
	find_symbol_value (variable);
      }
  }

  return variable;
}

/* Lisp functions for creating and removing buffer-local variables.  */

/* Obsolete since 22.2.  NB adjust doc of modify-frame-parameters
   when/if this is removed.  */

DEFUN ("make-variable-frame-local", Fmake_variable_frame_local, Smake_variable_frame_local,
       1, 1, "vMake Variable Frame Local: ",
       doc: /* Enable VARIABLE to have frame-local bindings.
This does not create any frame-local bindings for VARIABLE,
it just makes them possible.

A frame-local binding is actually a frame parameter value.
If a frame F has a value for the frame parameter named VARIABLE,
that also acts as a frame-local binding for VARIABLE in F--
provided this function has been called to enable VARIABLE
to have frame-local bindings at all.

The only way to create a frame-local binding for VARIABLE in a frame
is to set the VARIABLE frame parameter of that frame.  See
`modify-frame-parameters' for how to set frame parameters.

Note that since Emacs 23.1, variables cannot be both buffer-local and
frame-local any more (buffer-local bindings used to take precedence over
frame-local bindings).  */)
  (register Lisp_Object variable)
{
  int forwarded;
  union Lisp_Val_Fwd valcontents;
  struct Lisp_Symbol *sym;
  struct Lisp_Buffer_Local_Value *blv = NULL;

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL:
      forwarded = 0; valcontents.value = SYMBOL_VAL (sym);
      if (EQ (valcontents.value, Qunbound))
	valcontents.value = Qnil;
      break;
    case SYMBOL_LOCALIZED:
      if (SYMBOL_BLV (sym)->frame_local)
	return variable;
      else
	error ("Symbol %s may not be frame-local",
	       SDATA (SYMBOL_NAME (variable)));
    case SYMBOL_FORWARDED:
      forwarded = 1; valcontents.fwd = SYMBOL_FWD (sym);
      if (KBOARD_OBJFWDP (valcontents.fwd) || BUFFER_OBJFWDP (valcontents.fwd))
	error ("Symbol %s may not be frame-local",
	       SDATA (SYMBOL_NAME (variable)));
      break;
    default: abort ();
    }

  if (sym->constant)
    error ("Symbol %s may not be frame-local", SDATA (SYMBOL_NAME (variable)));

  blv = make_blv (sym, forwarded, valcontents);
  blv->frame_local = 1;
  sym->redirect = SYMBOL_LOCALIZED;
  SET_SYMBOL_BLV (sym, blv);
  {
    Lisp_Object symbol;
    XSETSYMBOL (symbol, sym); /* In case `variable' is aliased.  */
    if (let_shadows_global_binding_p (symbol))
      message ("Making %s frame-local while let-bound!",
	       SDATA (SYMBOL_NAME (variable)));
  }
  return variable;
}

DEFUN ("local-variable-p", Flocal_variable_p, Slocal_variable_p,
       1, 2, 0,
       doc: /* Non-nil if VARIABLE has a local binding in buffer BUFFER.
BUFFER defaults to the current buffer.  */)
  (register Lisp_Object variable, Lisp_Object buffer)
{
  register struct buffer *buf;
  struct Lisp_Symbol *sym;

  if (NILP (buffer))
    buf = current_buffer;
  else
    {
      CHECK_BUFFER (buffer);
      buf = XBUFFER (buffer);
    }

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return Qnil;
    case SYMBOL_LOCALIZED:
      {
	Lisp_Object tail, elt, tmp;
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	XSETBUFFER (tmp, buf);
	XSETSYMBOL (variable, sym); /* Update in case of aliasing.  */

	for (tail = BVAR (buf, local_var_alist); CONSP (tail); tail = XCDR (tail))
	  {
	    elt = XCAR (tail);
	    if (EQ (variable, XCAR (elt)))
	      {
		eassert (!blv->frame_local);
		eassert (BLV_FOUND (blv) || !EQ (blv->where, tmp));
		return Qt;
	      }
	  }
	eassert (!BLV_FOUND (blv) || !EQ (blv->where, tmp));
	return Qnil;
      }
    case SYMBOL_FORWARDED:
      {
	union Lisp_Fwd *valcontents = SYMBOL_FWD (sym);
	if (BUFFER_OBJFWDP (valcontents))
	  {
	    int offset = XBUFFER_OBJFWD (valcontents)->offset;
	    int idx = PER_BUFFER_IDX (offset);
	    if (idx == -1 || PER_BUFFER_VALUE_P (buf, idx))
	      return Qt;
	  }
	return Qnil;
      }
    default: abort ();
    }
}

DEFUN ("local-variable-if-set-p", Flocal_variable_if_set_p, Slocal_variable_if_set_p,
       1, 2, 0,
       doc: /* Non-nil if VARIABLE will be local in buffer BUFFER when set there.
More precisely, this means that setting the variable \(with `set' or`setq'),
while it does not have a `let'-style binding that was made in BUFFER,
will produce a buffer local binding.  See Info node
`(elisp)Creating Buffer-Local'.
BUFFER defaults to the current buffer.  */)
  (register Lisp_Object variable, Lisp_Object buffer)
{
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return Qnil;
    case SYMBOL_LOCALIZED:
      {
	struct Lisp_Buffer_Local_Value *blv = SYMBOL_BLV (sym);
	if (blv->local_if_set)
	  return Qt;
	XSETSYMBOL (variable, sym); /* Update in case of aliasing.  */
	return Flocal_variable_p (variable, buffer);
      }
    case SYMBOL_FORWARDED:
      /* All BUFFER_OBJFWD slots become local if they are set.  */
      return (BUFFER_OBJFWDP (SYMBOL_FWD (sym)) ? Qt : Qnil);
    default: abort ();
    }
}

DEFUN ("variable-binding-locus", Fvariable_binding_locus, Svariable_binding_locus,
       1, 1, 0,
       doc: /* Return a value indicating where VARIABLE's current binding comes from.
If the current binding is buffer-local, the value is the current buffer.
If the current binding is frame-local, the value is the selected frame.
If the current binding is global (the default), the value is nil.  */)
  (register Lisp_Object variable)
{
  struct Lisp_Symbol *sym;

  CHECK_SYMBOL (variable);
  sym = XSYMBOL (variable);

  /* Make sure the current binding is actually swapped in.  */
  find_symbol_value (variable);

 start:
  switch (sym->redirect)
    {
    case SYMBOL_VARALIAS: sym = indirect_variable (sym); goto start;
    case SYMBOL_PLAINVAL: return Qnil;
    case SYMBOL_FORWARDED:
      {
	union Lisp_Fwd *valcontents = SYMBOL_FWD (sym);
	if (KBOARD_OBJFWDP (valcontents))
	  return Fframe_terminal (Fselected_frame ());
	else if (!BUFFER_OBJFWDP (valcontents))
	  return Qnil;
      }
      /* FALLTHROUGH */
    case SYMBOL_LOCALIZED:
      /* For a local variable, record both the symbol and which
	 buffer's or frame's value we are saving.  */
      if (!NILP (Flocal_variable_p (variable, Qnil)))
	return Fcurrent_buffer ();
      else if (sym->redirect == SYMBOL_LOCALIZED
	       && BLV_FOUND (SYMBOL_BLV (sym)))
	return SYMBOL_BLV (sym)->where;
      else
	return Qnil;
    default: abort ();
    }
}

/* This code is disabled now that we use the selected frame to return
   keyboard-local-values. */
#if 0
extern struct terminal *get_terminal (Lisp_Object display, int);

DEFUN ("terminal-local-value", Fterminal_local_value,
       Sterminal_local_value, 2, 2, 0,
       doc: /* Return the terminal-local value of SYMBOL on TERMINAL.
If SYMBOL is not a terminal-local variable, then return its normal
value, like `symbol-value'.

TERMINAL may be a terminal object, a frame, or nil (meaning the
selected frame's terminal device).  */)
  (Lisp_Object symbol, Lisp_Object terminal)
{
  Lisp_Object result;
  struct terminal *t = get_terminal (terminal, 1);
  push_kboard (t->kboard);
  result = Fsymbol_value (symbol);
  pop_kboard ();
  return result;
}

DEFUN ("set-terminal-local-value", Fset_terminal_local_value,
       Sset_terminal_local_value, 3, 3, 0,
       doc: /* Set the terminal-local binding of SYMBOL on TERMINAL to VALUE.
If VARIABLE is not a terminal-local variable, then set its normal
binding, like `set'.

TERMINAL may be a terminal object, a frame, or nil (meaning the
selected frame's terminal device).  */)
  (Lisp_Object symbol, Lisp_Object terminal, Lisp_Object value)
{
  Lisp_Object result;
  struct terminal *t = get_terminal (terminal, 1);
  push_kboard (d->kboard);
  result = Fset (symbol, value);
  pop_kboard ();
  return result;
}
#endif

/* Find the function at the end of a chain of symbol function indirections.  */

/* If OBJECT is a symbol, find the end of its function chain and
   return the value found there.  If OBJECT is not a symbol, just
   return it.  If there is a cycle in the function chain, signal a
   cyclic-function-indirection error.

   This is like Findirect_function, except that it doesn't signal an
   error if the chain ends up unbound.  */
Lisp_Object
indirect_function (register Lisp_Object object)
{
  Lisp_Object tortoise, hare;

  hare = tortoise = object;

  for (;;)
    {
      if (!SYMBOLP (hare) || EQ (hare, Qunbound))
	break;
      hare = XSYMBOL (hare)->function;
      if (!SYMBOLP (hare) || EQ (hare, Qunbound))
	break;
      hare = XSYMBOL (hare)->function;

      tortoise = XSYMBOL (tortoise)->function;

      if (EQ (hare, tortoise))
	xsignal1 (Qcyclic_function_indirection, object);
    }

  return hare;
}

DEFUN ("indirect-function", Findirect_function, Sindirect_function, 1, 2, 0,
       doc: /* Return the function at the end of OBJECT's function chain.
If OBJECT is not a symbol, just return it.  Otherwise, follow all
function indirections to find the final function binding and return it.
If the final symbol in the chain is unbound, signal a void-function error.
Optional arg NOERROR non-nil means to return nil instead of signaling.
Signal a cyclic-function-indirection error if there is a loop in the
function chain of symbols.  */)
  (register Lisp_Object object, Lisp_Object noerror)
{
  Lisp_Object result;

  /* Optimize for no indirection.  */
  result = object;
  if (SYMBOLP (result) && !EQ (result, Qunbound)
      && (result = XSYMBOL (result)->function, SYMBOLP (result)))
    result = indirect_function (result);
  if (!EQ (result, Qunbound))
    return result;

  if (NILP (noerror))
    xsignal1 (Qvoid_function, object);

  return Qnil;
}

/* Extract and set vector and string elements */

DEFUN ("aref", Faref, Saref, 2, 2, 0,
       doc: /* Return the element of ARRAY at index IDX.
ARRAY may be a vector, a string, a char-table, a bool-vector,
or a byte-code object.  IDX starts at 0.  */)
  (register Lisp_Object array, Lisp_Object idx)
{
  register EMACS_INT idxval;

  CHECK_NUMBER (idx);
  idxval = XINT (idx);
  if (STRINGP (array))
    {
      int c;
      EMACS_INT idxval_byte;

      if (idxval < 0 || idxval >= SCHARS (array))
	args_out_of_range (array, idx);
      if (! STRING_MULTIBYTE (array))
	return make_number ((unsigned char) SREF (array, idxval));
      idxval_byte = string_char_to_byte (array, idxval);

      c = STRING_CHAR (SDATA (array) + idxval_byte);
      return make_number (c);
    }
  else if (BOOL_VECTOR_P (array))
    {
      int val;

      if (idxval < 0 || idxval >= XBOOL_VECTOR (array)->size)
	args_out_of_range (array, idx);

      val = (unsigned char) XBOOL_VECTOR (array)->data[idxval / BOOL_VECTOR_BITS_PER_CHAR];
      return (val & (1 << (idxval % BOOL_VECTOR_BITS_PER_CHAR)) ? Qt : Qnil);
    }
  else if (CHAR_TABLE_P (array))
    {
      CHECK_CHARACTER (idx);
      return CHAR_TABLE_REF (array, idxval);
    }
  else
    {
      int size = 0;
      if (VECTORP (array))
	size = ASIZE (array);
      else if (COMPILEDP (array))
	size = ASIZE (array) & PSEUDOVECTOR_SIZE_MASK;
      else
	wrong_type_argument (Qarrayp, array);

      if (idxval < 0 || idxval >= size)
	args_out_of_range (array, idx);
      return AREF (array, idxval);
    }
}

DEFUN ("aset", Faset, Saset, 3, 3, 0,
       doc: /* Store into the element of ARRAY at index IDX the value NEWELT.
Return NEWELT.  ARRAY may be a vector, a string, a char-table or a
bool-vector.  IDX starts at 0.  */)
  (register Lisp_Object array, Lisp_Object idx, Lisp_Object newelt)
{
  register EMACS_INT idxval;

  CHECK_NUMBER (idx);
  idxval = XINT (idx);
  CHECK_ARRAY (array, Qarrayp);
  CHECK_IMPURE (array);

  if (VECTORP (array))
    {
      if (idxval < 0 || idxval >= ASIZE (array))
	args_out_of_range (array, idx);
      XVECTOR (array)->contents[idxval] = newelt;
    }
  else if (BOOL_VECTOR_P (array))
    {
      int val;

      if (idxval < 0 || idxval >= XBOOL_VECTOR (array)->size)
	args_out_of_range (array, idx);

      val = (unsigned char) XBOOL_VECTOR (array)->data[idxval / BOOL_VECTOR_BITS_PER_CHAR];

      if (! NILP (newelt))
	val |= 1 << (idxval % BOOL_VECTOR_BITS_PER_CHAR);
      else
	val &= ~(1 << (idxval % BOOL_VECTOR_BITS_PER_CHAR));
      XBOOL_VECTOR (array)->data[idxval / BOOL_VECTOR_BITS_PER_CHAR] = val;
    }
  else if (CHAR_TABLE_P (array))
    {
      CHECK_CHARACTER (idx);
      CHAR_TABLE_SET (array, idxval, newelt);
    }
  else
    {
      int c;

      if (idxval < 0 || idxval >= SCHARS (array))
	args_out_of_range (array, idx);
      CHECK_CHARACTER (newelt);
      c = XFASTINT (newelt);

      if (STRING_MULTIBYTE (array))
	{
	  EMACS_INT idxval_byte, prev_bytes, new_bytes, nbytes;
	  unsigned char workbuf[MAX_MULTIBYTE_LENGTH], *p0 = workbuf, *p1;

	  nbytes = SBYTES (array);
	  idxval_byte = string_char_to_byte (array, idxval);
	  p1 = SDATA (array) + idxval_byte;
	  prev_bytes = BYTES_BY_CHAR_HEAD (*p1);
	  new_bytes = CHAR_STRING (c, p0);
	  if (prev_bytes != new_bytes)
	    {
	      /* We must relocate the string data.  */
	      EMACS_INT nchars = SCHARS (array);
	      unsigned char *str;
	      USE_SAFE_ALLOCA;

	      SAFE_ALLOCA (str, unsigned char *, nbytes);
	      memcpy (str, SDATA (array), nbytes);
	      allocate_string_data (XSTRING (array), nchars,
				    nbytes + new_bytes - prev_bytes);
	      memcpy (SDATA (array), str, idxval_byte);
	      p1 = SDATA (array) + idxval_byte;
	      memcpy (p1 + new_bytes, str + idxval_byte + prev_bytes,
		      nbytes - (idxval_byte + prev_bytes));
	      SAFE_FREE ();
	      clear_string_char_byte_cache ();
	    }
	  while (new_bytes--)
	    *p1++ = *p0++;
	}
      else
	{
	  if (! SINGLE_BYTE_CHAR_P (c))
	    {
	      int i;

	      for (i = SBYTES (array) - 1; i >= 0; i--)
		if (SREF (array, i) >= 0x80)
		  args_out_of_range (array, newelt);
	      /* ARRAY is an ASCII string.  Convert it to a multibyte
		 string, and try `aset' again.  */
	      STRING_SET_MULTIBYTE (array);
	      return Faset (array, idx, newelt);
	    }
	  SSET (array, idxval, c);
	}
    }

  return newelt;
}

/* Arithmetic functions */

enum comparison { equal, notequal, less, grtr, less_or_equal, grtr_or_equal };

static Lisp_Object
arithcompare (Lisp_Object num1, Lisp_Object num2, enum comparison comparison)
{
  double f1 = 0, f2 = 0;
  int floatp = 0;

  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num1);
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (num2);

  if (FLOATP (num1) || FLOATP (num2))
    {
      floatp = 1;
      f1 = (FLOATP (num1)) ? XFLOAT_DATA (num1) : XINT (num1);
      f2 = (FLOATP (num2)) ? XFLOAT_DATA (num2) : XINT (num2);
    }

  switch (comparison)
    {
    case equal:
      if (floatp ? f1 == f2 : XINT (num1) == XINT (num2))
	return Qt;
      return Qnil;

    case notequal:
      if (floatp ? f1 != f2 : XINT (num1) != XINT (num2))
	return Qt;
      return Qnil;

    case less:
      if (floatp ? f1 < f2 : XINT (num1) < XINT (num2))
	return Qt;
      return Qnil;

    case less_or_equal:
      if (floatp ? f1 <= f2 : XINT (num1) <= XINT (num2))
	return Qt;
      return Qnil;

    case grtr:
      if (floatp ? f1 > f2 : XINT (num1) > XINT (num2))
	return Qt;
      return Qnil;

    case grtr_or_equal:
      if (floatp ? f1 >= f2 : XINT (num1) >= XINT (num2))
	return Qt;
      return Qnil;

    default:
      abort ();
    }
}

DEFUN ("=", Feqlsign, Seqlsign, 2, 2, 0,
       doc: /* Return t if two args, both numbers or markers, are equal.  */)
  (register Lisp_Object num1, Lisp_Object num2)
{
  return arithcompare (num1, num2, equal);
}

DEFUN ("<", Flss, Slss, 2, 2, 0,
       doc: /* Return t if first arg is less than second arg.  Both must be numbers or markers.  */)
  (register Lisp_Object num1, Lisp_Object num2)
{
  return arithcompare (num1, num2, less);
}

DEFUN (">", Fgtr, Sgtr, 2, 2, 0,
       doc: /* Return t if first arg is greater than second arg.  Both must be numbers or markers.  */)
  (register Lisp_Object num1, Lisp_Object num2)
{
  return arithcompare (num1, num2, grtr);
}

DEFUN ("<=", Fleq, Sleq, 2, 2, 0,
       doc: /* Return t if first arg is less than or equal to second arg.
Both must be numbers or markers.  */)
  (register Lisp_Object num1, Lisp_Object num2)
{
  return arithcompare (num1, num2, less_or_equal);
}

DEFUN (">=", Fgeq, Sgeq, 2, 2, 0,
       doc: /* Return t if first arg is greater than or equal to second arg.
Both must be numbers or markers.  */)
  (register Lisp_Object num1, Lisp_Object num2)
{
  return arithcompare (num1, num2, grtr_or_equal);
}

DEFUN ("/=", Fneq, Sneq, 2, 2, 0,
       doc: /* Return t if first arg is not equal to second arg.  Both must be numbers or markers.  */)
  (register Lisp_Object num1, Lisp_Object num2)
{
  return arithcompare (num1, num2, notequal);
}

DEFUN ("zerop", Fzerop, Szerop, 1, 1, 0,
       doc: /* Return t if NUMBER is zero.  */)
  (register Lisp_Object number)
{
  CHECK_NUMBER_OR_FLOAT (number);

  if (FLOATP (number))
    {
      if (XFLOAT_DATA (number) == 0.0)
	return Qt;
      return Qnil;
    }

  if (!XINT (number))
    return Qt;
  return Qnil;
}

/* Convert the cons-of-integers, integer, or float value C to an
   unsigned value with maximum value MAX.  Signal an error if C does not
   have a valid format or is out of range.  */
uintmax_t
cons_to_unsigned (Lisp_Object c, uintmax_t max)
{
  int valid = 0;
  uintmax_t val IF_LINT (= 0);
  if (INTEGERP (c))
    {
      valid = 0 <= XINT (c);
      val = XINT (c);
    }
  else if (FLOATP (c))
    {
      double d = XFLOAT_DATA (c);
      if (0 <= d
	  && d < (max == UINTMAX_MAX ? (double) UINTMAX_MAX + 1 : max + 1))
	{
	  val = d;
	  valid = 1;
	}
    }
  else if (CONSP (c) && NATNUMP (XCAR (c)))
    {
      uintmax_t top = XFASTINT (XCAR (c));
      Lisp_Object rest = XCDR (c);
      if (top <= UINTMAX_MAX >> 24 >> 16
	  && CONSP (rest)
	  && NATNUMP (XCAR (rest)) && XFASTINT (XCAR (rest)) < 1 << 24
	  && NATNUMP (XCDR (rest)) && XFASTINT (XCDR (rest)) < 1 << 16)
	{
	  uintmax_t mid = XFASTINT (XCAR (rest));
	  val = top << 24 << 16 | mid << 16 | XFASTINT (XCDR (rest));
	  valid = 1;
	}
      else if (top <= UINTMAX_MAX >> 16)
	{
	  if (CONSP (rest))
	    rest = XCAR (rest);
	  if (NATNUMP (rest) && XFASTINT (rest) < 1 << 16)
	    {
	      val = top << 16 | XFASTINT (rest);
	      valid = 1;
	    }
	}
    }

  if (! (valid && val <= max))
    error ("Not an in-range integer, float, or cons of integers");
  return val;
}

/* Convert the cons-of-integers, integer, or float value C to a signed
   value with extrema MIN and MAX.  Signal an error if C does not have
   a valid format or is out of range.  */
intmax_t
cons_to_signed (Lisp_Object c, intmax_t min, intmax_t max)
{
  int valid = 0;
  intmax_t val IF_LINT (= 0);
  if (INTEGERP (c))
    {
      val = XINT (c);
      valid = 1;
    }
  else if (FLOATP (c))
    {
      double d = XFLOAT_DATA (c);
      if (min <= d
	  && d < (max == INTMAX_MAX ? (double) INTMAX_MAX + 1 : max + 1))
	{
	  val = d;
	  valid = 1;
	}
    }
  else if (CONSP (c) && INTEGERP (XCAR (c)))
    {
      intmax_t top = XINT (XCAR (c));
      Lisp_Object rest = XCDR (c);
      if (INTMAX_MIN >> 24 >> 16 <= top && top <= INTMAX_MAX >> 24 >> 16
	  && CONSP (rest)
	  && NATNUMP (XCAR (rest)) && XFASTINT (XCAR (rest)) < 1 << 24
	  && NATNUMP (XCDR (rest)) && XFASTINT (XCDR (rest)) < 1 << 16)
	{
	  intmax_t mid = XFASTINT (XCAR (rest));
	  val = top << 24 << 16 | mid << 16 | XFASTINT (XCDR (rest));
	  valid = 1;
	}
      else if (INTMAX_MIN >> 16 <= top && top <= INTMAX_MAX >> 16)
	{
	  if (CONSP (rest))
	    rest = XCAR (rest);
	  if (NATNUMP (rest) && XFASTINT (rest) < 1 << 16)
	    {
	      val = top << 16 | XFASTINT (rest);
	      valid = 1;
	    }
	}
    }

  if (! (valid && min <= val && val <= max))
    error ("Not an in-range integer, float, or cons of integers");
  return val;
}

DEFUN ("number-to-string", Fnumber_to_string, Snumber_to_string, 1, 1, 0,
       doc: /* Return the decimal representation of NUMBER as a string.
Uses a minus sign if negative.
NUMBER may be an integer or a floating point number.  */)
  (Lisp_Object number)
{
  char buffer[VALBITS];

  CHECK_NUMBER_OR_FLOAT (number);

  if (FLOATP (number))
    {
      char pigbuf[FLOAT_TO_STRING_BUFSIZE];

      float_to_string (pigbuf, XFLOAT_DATA (number));
      return build_string (pigbuf);
    }

  sprintf (buffer, "%"pI"d", XINT (number));
  return build_string (buffer);
}

DEFUN ("string-to-number", Fstring_to_number, Sstring_to_number, 1, 2, 0,
       doc: /* Parse STRING as a decimal number and return the number.
This parses both integers and floating point numbers.
It ignores leading spaces and tabs, and all trailing chars.

If BASE, interpret STRING as a number in that base.  If BASE isn't
present, base 10 is used.  BASE must be between 2 and 16 (inclusive).
If the base used is not 10, STRING is always parsed as integer.  */)
  (register Lisp_Object string, Lisp_Object base)
{
  register char *p;
  register int b;
  Lisp_Object val;

  CHECK_STRING (string);

  if (NILP (base))
    b = 10;
  else
    {
      CHECK_NUMBER (base);
      b = XINT (base);
      if (b < 2 || b > 16)
	xsignal1 (Qargs_out_of_range, base);
    }

  p = SSDATA (string);
  while (*p == ' ' || *p == '\t')
    p++;

  val = string_to_number (p, b, 1);
  return NILP (val) ? make_number (0) : val;
}

enum arithop
  {
    Aadd,
    Asub,
    Amult,
    Adiv,
    Alogand,
    Alogior,
    Alogxor,
    Amax,
    Amin
  };

static Lisp_Object float_arith_driver (double, ptrdiff_t, enum arithop,
                                       ptrdiff_t, Lisp_Object *);
static Lisp_Object
arith_driver (enum arithop code, ptrdiff_t nargs, Lisp_Object *args)
{
  register Lisp_Object val;
  ptrdiff_t argnum;
  register EMACS_INT accum = 0;
  register EMACS_INT next;

  int overflow = 0;
  ptrdiff_t ok_args;
  EMACS_INT ok_accum;

  switch (SWITCH_ENUM_CAST (code))
    {
    case Alogior:
    case Alogxor:
    case Aadd:
    case Asub:
      accum = 0;
      break;
    case Amult:
      accum = 1;
      break;
    case Alogand:
      accum = -1;
      break;
    default:
      break;
    }

  for (argnum = 0; argnum < nargs; argnum++)
    {
      if (! overflow)
	{
	  ok_args = argnum;
	  ok_accum = accum;
	}

      /* Using args[argnum] as argument to CHECK_NUMBER_... */
      val = args[argnum];
      CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (val);

      if (FLOATP (val))
	return float_arith_driver (ok_accum, ok_args, code,
				   nargs, args);
      args[argnum] = val;
      next = XINT (args[argnum]);
      switch (SWITCH_ENUM_CAST (code))
	{
	case Aadd:
	  if (INT_ADD_OVERFLOW (accum, next))
	    {
	      overflow = 1;
	      accum &= INTMASK;
	    }
	  accum += next;
	  break;
	case Asub:
	  if (INT_SUBTRACT_OVERFLOW (accum, next))
	    {
	      overflow = 1;
	      accum &= INTMASK;
	    }
	  accum = argnum ? accum - next : nargs == 1 ? - next : next;
	  break;
	case Amult:
	  if (INT_MULTIPLY_OVERFLOW (accum, next))
	    {
	      EMACS_UINT a = accum, b = next, ab = a * b;
	      overflow = 1;
	      accum = ab & INTMASK;
	    }
	  else
	    accum *= next;
	  break;
	case Adiv:
	  if (!argnum)
	    accum = next;
	  else
	    {
	      if (next == 0)
		xsignal0 (Qarith_error);
	      accum /= next;
	    }
	  break;
	case Alogand:
	  accum &= next;
	  break;
	case Alogior:
	  accum |= next;
	  break;
	case Alogxor:
	  accum ^= next;
	  break;
	case Amax:
	  if (!argnum || next > accum)
	    accum = next;
	  break;
	case Amin:
	  if (!argnum || next < accum)
	    accum = next;
	  break;
	}
    }

  XSETINT (val, accum);
  return val;
}

#undef isnan
#define isnan(x) ((x) != (x))

static Lisp_Object
float_arith_driver (double accum, ptrdiff_t argnum, enum arithop code,
		    ptrdiff_t nargs, Lisp_Object *args)
{
  register Lisp_Object val;
  double next;

  for (; argnum < nargs; argnum++)
    {
      val = args[argnum];    /* using args[argnum] as argument to CHECK_NUMBER_... */
      CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (val);

      if (FLOATP (val))
	{
	  next = XFLOAT_DATA (val);
	}
      else
	{
	  args[argnum] = val;    /* runs into a compiler bug. */
	  next = XINT (args[argnum]);
	}
      switch (SWITCH_ENUM_CAST (code))
	{
	case Aadd:
	  accum += next;
	  break;
	case Asub:
	  accum = argnum ? accum - next : nargs == 1 ? - next : next;
	  break;
	case Amult:
	  accum *= next;
	  break;
	case Adiv:
	  if (!argnum)
	    accum = next;
	  else
	    {
	      if (! IEEE_FLOATING_POINT && next == 0)
		xsignal0 (Qarith_error);
	      accum /= next;
	    }
	  break;
	case Alogand:
	case Alogior:
	case Alogxor:
	  return wrong_type_argument (Qinteger_or_marker_p, val);
	case Amax:
	  if (!argnum || isnan (next) || next > accum)
	    accum = next;
	  break;
	case Amin:
	  if (!argnum || isnan (next) || next < accum)
	    accum = next;
	  break;
	}
    }

  return make_float (accum);
}


DEFUN ("+", Fplus, Splus, 0, MANY, 0,
       doc: /* Return sum of any number of arguments, which are numbers or markers.
usage: (+ &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Aadd, nargs, args);
}

DEFUN ("-", Fminus, Sminus, 0, MANY, 0,
       doc: /* Negate number or subtract numbers or markers and return the result.
With one arg, negates it.  With more than one arg,
subtracts all but the first from the first.
usage: (- &optional NUMBER-OR-MARKER &rest MORE-NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Asub, nargs, args);
}

DEFUN ("*", Ftimes, Stimes, 0, MANY, 0,
       doc: /* Return product of any number of arguments, which are numbers or markers.
usage: (* &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Amult, nargs, args);
}

DEFUN ("/", Fquo, Squo, 2, MANY, 0,
       doc: /* Return first argument divided by all the remaining arguments.
The arguments must be numbers or markers.
usage: (/ DIVIDEND DIVISOR &rest DIVISORS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  ptrdiff_t argnum;
  for (argnum = 2; argnum < nargs; argnum++)
    if (FLOATP (args[argnum]))
      return float_arith_driver (0, 0, Adiv, nargs, args);
  return arith_driver (Adiv, nargs, args);
}

DEFUN ("%", Frem, Srem, 2, 2, 0,
       doc: /* Return remainder of X divided by Y.
Both must be integers or markers.  */)
  (register Lisp_Object x, Lisp_Object y)
{
  Lisp_Object val;

  CHECK_NUMBER_COERCE_MARKER (x);
  CHECK_NUMBER_COERCE_MARKER (y);

  if (XFASTINT (y) == 0)
    xsignal0 (Qarith_error);

  XSETINT (val, XINT (x) % XINT (y));
  return val;
}

#ifndef HAVE_FMOD
double
fmod (double f1, double f2)
{
  double r = f1;

  if (f2 < 0.0)
    f2 = -f2;

  /* If the magnitude of the result exceeds that of the divisor, or
     the sign of the result does not agree with that of the dividend,
     iterate with the reduced value.  This does not yield a
     particularly accurate result, but at least it will be in the
     range promised by fmod.  */
  do
    r -= f2 * floor (r / f2);
  while (f2 <= (r < 0 ? -r : r) || ((r < 0) != (f1 < 0) && ! isnan (r)));

  return r;
}
#endif /* ! HAVE_FMOD */

DEFUN ("mod", Fmod, Smod, 2, 2, 0,
       doc: /* Return X modulo Y.
The result falls between zero (inclusive) and Y (exclusive).
Both X and Y must be numbers or markers.  */)
  (register Lisp_Object x, Lisp_Object y)
{
  Lisp_Object val;
  EMACS_INT i1, i2;

  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (x);
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (y);

  if (FLOATP (x) || FLOATP (y))
    return fmod_float (x, y);

  i1 = XINT (x);
  i2 = XINT (y);

  if (i2 == 0)
    xsignal0 (Qarith_error);

  i1 %= i2;

  /* If the "remainder" comes out with the wrong sign, fix it.  */
  if (i2 < 0 ? i1 > 0 : i1 < 0)
    i1 += i2;

  XSETINT (val, i1);
  return val;
}

DEFUN ("max", Fmax, Smax, 1, MANY, 0,
       doc: /* Return largest of all the arguments (which must be numbers or markers).
The value is always a number; markers are converted to numbers.
usage: (max NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Amax, nargs, args);
}

DEFUN ("min", Fmin, Smin, 1, MANY, 0,
       doc: /* Return smallest of all the arguments (which must be numbers or markers).
The value is always a number; markers are converted to numbers.
usage: (min NUMBER-OR-MARKER &rest NUMBERS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Amin, nargs, args);
}

DEFUN ("logand", Flogand, Slogand, 0, MANY, 0,
       doc: /* Return bitwise-and of all the arguments.
Arguments may be integers, or markers converted to integers.
usage: (logand &rest INTS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Alogand, nargs, args);
}

DEFUN ("logior", Flogior, Slogior, 0, MANY, 0,
       doc: /* Return bitwise-or of all the arguments.
Arguments may be integers, or markers converted to integers.
usage: (logior &rest INTS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Alogior, nargs, args);
}

DEFUN ("logxor", Flogxor, Slogxor, 0, MANY, 0,
       doc: /* Return bitwise-exclusive-or of all the arguments.
Arguments may be integers, or markers converted to integers.
usage: (logxor &rest INTS-OR-MARKERS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  return arith_driver (Alogxor, nargs, args);
}

DEFUN ("ash", Fash, Sash, 2, 2, 0,
       doc: /* Return VALUE with its bits shifted left by COUNT.
If COUNT is negative, shifting is actually to the right.
In this case, the sign bit is duplicated.  */)
  (register Lisp_Object value, Lisp_Object count)
{
  register Lisp_Object val;

  CHECK_NUMBER (value);
  CHECK_NUMBER (count);

  if (XINT (count) >= BITS_PER_EMACS_INT)
    XSETINT (val, 0);
  else if (XINT (count) > 0)
    XSETINT (val, XINT (value) << XFASTINT (count));
  else if (XINT (count) <= -BITS_PER_EMACS_INT)
    XSETINT (val, XINT (value) < 0 ? -1 : 0);
  else
    XSETINT (val, XINT (value) >> -XINT (count));
  return val;
}

DEFUN ("lsh", Flsh, Slsh, 2, 2, 0,
       doc: /* Return VALUE with its bits shifted left by COUNT.
If COUNT is negative, shifting is actually to the right.
In this case, zeros are shifted in on the left.  */)
  (register Lisp_Object value, Lisp_Object count)
{
  register Lisp_Object val;

  CHECK_NUMBER (value);
  CHECK_NUMBER (count);

  if (XINT (count) >= BITS_PER_EMACS_INT)
    XSETINT (val, 0);
  else if (XINT (count) > 0)
    XSETINT (val, XUINT (value) << XFASTINT (count));
  else if (XINT (count) <= -BITS_PER_EMACS_INT)
    XSETINT (val, 0);
  else
    XSETINT (val, XUINT (value) >> -XINT (count));
  return val;
}

DEFUN ("1+", Fadd1, Sadd1, 1, 1, 0,
       doc: /* Return NUMBER plus one.  NUMBER may be a number or a marker.
Markers are converted to integers.  */)
  (register Lisp_Object number)
{
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (number);

  if (FLOATP (number))
    return (make_float (1.0 + XFLOAT_DATA (number)));

  XSETINT (number, XINT (number) + 1);
  return number;
}

DEFUN ("1-", Fsub1, Ssub1, 1, 1, 0,
       doc: /* Return NUMBER minus one.  NUMBER may be a number or a marker.
Markers are converted to integers.  */)
  (register Lisp_Object number)
{
  CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (number);

  if (FLOATP (number))
    return (make_float (-1.0 + XFLOAT_DATA (number)));

  XSETINT (number, XINT (number) - 1);
  return number;
}

DEFUN ("lognot", Flognot, Slognot, 1, 1, 0,
       doc: /* Return the bitwise complement of NUMBER.  NUMBER must be an integer.  */)
  (register Lisp_Object number)
{
  CHECK_NUMBER (number);
  XSETINT (number, ~XINT (number));
  return number;
}

DEFUN ("byteorder", Fbyteorder, Sbyteorder, 0, 0, 0,
       doc: /* Return the byteorder for the machine.
Returns 66 (ASCII uppercase B) for big endian machines or 108 (ASCII
lowercase l) for small endian machines.  */)
  (void)
{
  unsigned i = 0x04030201;
  int order = *(char *)&i == 1 ? 108 : 66;

  return make_number (order);
}



void
syms_of_data (void)
{
  Lisp_Object error_tail, arith_tail;

  DEFSYM (Qquote, "quote");
  DEFSYM (Qlambda, "lambda");
  DEFSYM (Qsubr, "subr");
  DEFSYM (Qerror_conditions, "error-conditions");
  DEFSYM (Qerror_message, "error-message");
  DEFSYM (Qtop_level, "top-level");

  DEFSYM (Qerror, "error");
  DEFSYM (Qquit, "quit");
  DEFSYM (Qwrong_type_argument, "wrong-type-argument");
  DEFSYM (Qargs_out_of_range, "args-out-of-range");
  DEFSYM (Qvoid_function, "void-function");
  DEFSYM (Qcyclic_function_indirection, "cyclic-function-indirection");
  DEFSYM (Qcyclic_variable_indirection, "cyclic-variable-indirection");
  DEFSYM (Qvoid_variable, "void-variable");
  DEFSYM (Qsetting_constant, "setting-constant");
  DEFSYM (Qinvalid_read_syntax, "invalid-read-syntax");

  DEFSYM (Qinvalid_function, "invalid-function");
  DEFSYM (Qwrong_number_of_arguments, "wrong-number-of-arguments");
  DEFSYM (Qno_catch, "no-catch");
  DEFSYM (Qend_of_file, "end-of-file");
  DEFSYM (Qarith_error, "arith-error");
  DEFSYM (Qbeginning_of_buffer, "beginning-of-buffer");
  DEFSYM (Qend_of_buffer, "end-of-buffer");
  DEFSYM (Qbuffer_read_only, "buffer-read-only");
  DEFSYM (Qtext_read_only, "text-read-only");
  DEFSYM (Qmark_inactive, "mark-inactive");

  DEFSYM (Qlistp, "listp");
  DEFSYM (Qconsp, "consp");
  DEFSYM (Qsymbolp, "symbolp");
  DEFSYM (Qkeywordp, "keywordp");
  DEFSYM (Qintegerp, "integerp");
  DEFSYM (Qnatnump, "natnump");
  DEFSYM (Qwholenump, "wholenump");
  DEFSYM (Qstringp, "stringp");
  DEFSYM (Qarrayp, "arrayp");
  DEFSYM (Qsequencep, "sequencep");
  DEFSYM (Qbufferp, "bufferp");
  DEFSYM (Qvectorp, "vectorp");
  DEFSYM (Qchar_or_string_p, "char-or-string-p");
  DEFSYM (Qmarkerp, "markerp");
  DEFSYM (Qbuffer_or_string_p, "buffer-or-string-p");
  DEFSYM (Qinteger_or_marker_p, "integer-or-marker-p");
  DEFSYM (Qboundp, "boundp");
  DEFSYM (Qfboundp, "fboundp");

  DEFSYM (Qfloatp, "floatp");
  DEFSYM (Qnumberp, "numberp");
  DEFSYM (Qnumber_or_marker_p, "number-or-marker-p");

  DEFSYM (Qchar_table_p, "char-table-p");
  DEFSYM (Qvector_or_char_table_p, "vector-or-char-table-p");

  DEFSYM (Qsubrp, "subrp");
  DEFSYM (Qunevalled, "unevalled");
  DEFSYM (Qmany, "many");

  DEFSYM (Qcdr, "cdr");

  /* Handle automatic advice activation.  */
  DEFSYM (Qad_advice_info, "ad-advice-info");
  DEFSYM (Qad_activate_internal, "ad-activate-internal");

  error_tail = pure_cons (Qerror, Qnil);

  /* ERROR is used as a signaler for random errors for which nothing else is
     right.  */

  Fput (Qerror, Qerror_conditions,
	error_tail);
  Fput (Qerror, Qerror_message,
	make_pure_c_string ("error"));

  Fput (Qquit, Qerror_conditions,
	pure_cons (Qquit, Qnil));
  Fput (Qquit, Qerror_message,
	make_pure_c_string ("Quit"));

  Fput (Qwrong_type_argument, Qerror_conditions,
	pure_cons (Qwrong_type_argument, error_tail));
  Fput (Qwrong_type_argument, Qerror_message,
	make_pure_c_string ("Wrong type argument"));

  Fput (Qargs_out_of_range, Qerror_conditions,
	pure_cons (Qargs_out_of_range, error_tail));
  Fput (Qargs_out_of_range, Qerror_message,
	make_pure_c_string ("Args out of range"));

  Fput (Qvoid_function, Qerror_conditions,
	pure_cons (Qvoid_function, error_tail));
  Fput (Qvoid_function, Qerror_message,
	make_pure_c_string ("Symbol's function definition is void"));

  Fput (Qcyclic_function_indirection, Qerror_conditions,
	pure_cons (Qcyclic_function_indirection, error_tail));
  Fput (Qcyclic_function_indirection, Qerror_message,
	make_pure_c_string ("Symbol's chain of function indirections contains a loop"));

  Fput (Qcyclic_variable_indirection, Qerror_conditions,
	pure_cons (Qcyclic_variable_indirection, error_tail));
  Fput (Qcyclic_variable_indirection, Qerror_message,
	make_pure_c_string ("Symbol's chain of variable indirections contains a loop"));

  DEFSYM (Qcircular_list, "circular-list");
  Fput (Qcircular_list, Qerror_conditions,
	pure_cons (Qcircular_list, error_tail));
  Fput (Qcircular_list, Qerror_message,
	make_pure_c_string ("List contains a loop"));

  Fput (Qvoid_variable, Qerror_conditions,
	pure_cons (Qvoid_variable, error_tail));
  Fput (Qvoid_variable, Qerror_message,
	make_pure_c_string ("Symbol's value as variable is void"));

  Fput (Qsetting_constant, Qerror_conditions,
	pure_cons (Qsetting_constant, error_tail));
  Fput (Qsetting_constant, Qerror_message,
	make_pure_c_string ("Attempt to set a constant symbol"));

  Fput (Qinvalid_read_syntax, Qerror_conditions,
	pure_cons (Qinvalid_read_syntax, error_tail));
  Fput (Qinvalid_read_syntax, Qerror_message,
	make_pure_c_string ("Invalid read syntax"));

  Fput (Qinvalid_function, Qerror_conditions,
	pure_cons (Qinvalid_function, error_tail));
  Fput (Qinvalid_function, Qerror_message,
	make_pure_c_string ("Invalid function"));

  Fput (Qwrong_number_of_arguments, Qerror_conditions,
	pure_cons (Qwrong_number_of_arguments, error_tail));
  Fput (Qwrong_number_of_arguments, Qerror_message,
	make_pure_c_string ("Wrong number of arguments"));

  Fput (Qno_catch, Qerror_conditions,
	pure_cons (Qno_catch, error_tail));
  Fput (Qno_catch, Qerror_message,
	make_pure_c_string ("No catch for tag"));

  Fput (Qend_of_file, Qerror_conditions,
	pure_cons (Qend_of_file, error_tail));
  Fput (Qend_of_file, Qerror_message,
	make_pure_c_string ("End of file during parsing"));

  arith_tail = pure_cons (Qarith_error, error_tail);
  Fput (Qarith_error, Qerror_conditions,
	arith_tail);
  Fput (Qarith_error, Qerror_message,
	make_pure_c_string ("Arithmetic error"));

  Fput (Qbeginning_of_buffer, Qerror_conditions,
	pure_cons (Qbeginning_of_buffer, error_tail));
  Fput (Qbeginning_of_buffer, Qerror_message,
	make_pure_c_string ("Beginning of buffer"));

  Fput (Qend_of_buffer, Qerror_conditions,
	pure_cons (Qend_of_buffer, error_tail));
  Fput (Qend_of_buffer, Qerror_message,
	make_pure_c_string ("End of buffer"));

  Fput (Qbuffer_read_only, Qerror_conditions,
	pure_cons (Qbuffer_read_only, error_tail));
  Fput (Qbuffer_read_only, Qerror_message,
	make_pure_c_string ("Buffer is read-only"));

  Fput (Qtext_read_only, Qerror_conditions,
	pure_cons (Qtext_read_only, error_tail));
  Fput (Qtext_read_only, Qerror_message,
	make_pure_c_string ("Text is read-only"));

  DEFSYM (Qrange_error, "range-error");
  DEFSYM (Qdomain_error, "domain-error");
  DEFSYM (Qsingularity_error, "singularity-error");
  DEFSYM (Qoverflow_error, "overflow-error");
  DEFSYM (Qunderflow_error, "underflow-error");

  Fput (Qdomain_error, Qerror_conditions,
	pure_cons (Qdomain_error, arith_tail));
  Fput (Qdomain_error, Qerror_message,
	make_pure_c_string ("Arithmetic domain error"));

  Fput (Qrange_error, Qerror_conditions,
	pure_cons (Qrange_error, arith_tail));
  Fput (Qrange_error, Qerror_message,
	make_pure_c_string ("Arithmetic range error"));

  Fput (Qsingularity_error, Qerror_conditions,
	pure_cons (Qsingularity_error, Fcons (Qdomain_error, arith_tail)));
  Fput (Qsingularity_error, Qerror_message,
	make_pure_c_string ("Arithmetic singularity error"));

  Fput (Qoverflow_error, Qerror_conditions,
	pure_cons (Qoverflow_error, Fcons (Qdomain_error, arith_tail)));
  Fput (Qoverflow_error, Qerror_message,
	make_pure_c_string ("Arithmetic overflow error"));

  Fput (Qunderflow_error, Qerror_conditions,
	pure_cons (Qunderflow_error, Fcons (Qdomain_error, arith_tail)));
  Fput (Qunderflow_error, Qerror_message,
	make_pure_c_string ("Arithmetic underflow error"));

  staticpro (&Qnil);
  staticpro (&Qt);
  staticpro (&Qunbound);

  /* Types that type-of returns.  */
  DEFSYM (Qinteger, "integer");
  DEFSYM (Qsymbol, "symbol");
  DEFSYM (Qstring, "string");
  DEFSYM (Qcons, "cons");
  DEFSYM (Qmarker, "marker");
  DEFSYM (Qoverlay, "overlay");
  DEFSYM (Qfloat, "float");
  DEFSYM (Qwindow_configuration, "window-configuration");
  DEFSYM (Qprocess, "process");
  DEFSYM (Qwindow, "window");
  /* DEFSYM (Qsubr, "subr"); */
  DEFSYM (Qcompiled_function, "compiled-function");
  DEFSYM (Qbuffer, "buffer");
  DEFSYM (Qframe, "frame");
  DEFSYM (Qvector, "vector");
  DEFSYM (Qchar_table, "char-table");
  DEFSYM (Qbool_vector, "bool-vector");
  DEFSYM (Qhash_table, "hash-table");

  DEFSYM (Qfont_spec, "font-spec");
  DEFSYM (Qfont_entity, "font-entity");
  DEFSYM (Qfont_object, "font-object");

  DEFSYM (Qinteractive_form, "interactive-form");

  defsubr (&Sindirect_variable);
  defsubr (&Sinteractive_form);
  defsubr (&Seq);
  defsubr (&Snull);
  defsubr (&Stype_of);
  defsubr (&Slistp);
  defsubr (&Snlistp);
  defsubr (&Sconsp);
  defsubr (&Satom);
  defsubr (&Sintegerp);
  defsubr (&Sinteger_or_marker_p);
  defsubr (&Snumberp);
  defsubr (&Snumber_or_marker_p);
  defsubr (&Sfloatp);
  defsubr (&Snatnump);
  defsubr (&Ssymbolp);
  defsubr (&Skeywordp);
  defsubr (&Sstringp);
  defsubr (&Smultibyte_string_p);
  defsubr (&Svectorp);
  defsubr (&Schar_table_p);
  defsubr (&Svector_or_char_table_p);
  defsubr (&Sbool_vector_p);
  defsubr (&Sarrayp);
  defsubr (&Ssequencep);
  defsubr (&Sbufferp);
  defsubr (&Smarkerp);
  defsubr (&Ssubrp);
  defsubr (&Sbyte_code_function_p);
  defsubr (&Schar_or_string_p);
  defsubr (&Scar);
  defsubr (&Scdr);
  defsubr (&Scar_safe);
  defsubr (&Scdr_safe);
  defsubr (&Ssetcar);
  defsubr (&Ssetcdr);
  defsubr (&Ssymbol_function);
  defsubr (&Sindirect_function);
  defsubr (&Ssymbol_plist);
  defsubr (&Ssymbol_name);
  defsubr (&Smakunbound);
  defsubr (&Sfmakunbound);
  defsubr (&Sboundp);
  defsubr (&Sfboundp);
  defsubr (&Sfset);
  defsubr (&Sdefalias);
  defsubr (&Ssetplist);
  defsubr (&Ssymbol_value);
  defsubr (&Sset);
  defsubr (&Sdefault_boundp);
  defsubr (&Sdefault_value);
  defsubr (&Sset_default);
  defsubr (&Ssetq_default);
  defsubr (&Smake_variable_buffer_local);
  defsubr (&Smake_local_variable);
  defsubr (&Skill_local_variable);
  defsubr (&Smake_variable_frame_local);
  defsubr (&Slocal_variable_p);
  defsubr (&Slocal_variable_if_set_p);
  defsubr (&Svariable_binding_locus);
#if 0                           /* XXX Remove this. --lorentey */
  defsubr (&Sterminal_local_value);
  defsubr (&Sset_terminal_local_value);
#endif
  defsubr (&Saref);
  defsubr (&Saset);
  defsubr (&Snumber_to_string);
  defsubr (&Sstring_to_number);
  defsubr (&Seqlsign);
  defsubr (&Slss);
  defsubr (&Sgtr);
  defsubr (&Sleq);
  defsubr (&Sgeq);
  defsubr (&Sneq);
  defsubr (&Szerop);
  defsubr (&Splus);
  defsubr (&Sminus);
  defsubr (&Stimes);
  defsubr (&Squo);
  defsubr (&Srem);
  defsubr (&Smod);
  defsubr (&Smax);
  defsubr (&Smin);
  defsubr (&Slogand);
  defsubr (&Slogior);
  defsubr (&Slogxor);
  defsubr (&Slsh);
  defsubr (&Sash);
  defsubr (&Sadd1);
  defsubr (&Ssub1);
  defsubr (&Slognot);
  defsubr (&Sbyteorder);
  defsubr (&Ssubr_arity);
  defsubr (&Ssubr_name);

  XSYMBOL (Qwholenump)->function = XSYMBOL (Qnatnump)->function;

  DEFVAR_LISP ("most-positive-fixnum", Vmost_positive_fixnum,
	       doc: /* The largest value that is representable in a Lisp integer.  */);
  Vmost_positive_fixnum = make_number (MOST_POSITIVE_FIXNUM);
  XSYMBOL (intern_c_string ("most-positive-fixnum"))->constant = 1;

  DEFVAR_LISP ("most-negative-fixnum", Vmost_negative_fixnum,
	       doc: /* The smallest value that is representable in a Lisp integer.  */);
  Vmost_negative_fixnum = make_number (MOST_NEGATIVE_FIXNUM);
  XSYMBOL (intern_c_string ("most-negative-fixnum"))->constant = 1;
}

#ifndef FORWARD_SIGNAL_TO_MAIN_THREAD
static void arith_error (int) NO_RETURN;
#endif

static void
arith_error (int signo)
{
  sigsetmask (SIGEMPTYMASK);

  SIGNAL_THREAD_CHECK (signo);
  xsignal0 (Qarith_error);
}

void
init_data (void)
{
  /* Don't do this if just dumping out.
     We don't want to call `signal' in this case
     so that we don't have trouble with dumping
     signal-delivering routines in an inconsistent state.  */
#ifndef CANNOT_DUMP
  if (!initialized)
    return;
#endif /* CANNOT_DUMP */
  signal (SIGFPE, arith_error);
}
