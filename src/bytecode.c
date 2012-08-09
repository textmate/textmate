/* Execution of byte code produced by bytecomp.el.
   Copyright (C) 1985-1988, 1993, 2000-2012 Free Software Foundation, Inc.

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

/*
hacked on by jwz@lucid.com 17-jun-91
  o  added a compile-time switch to turn on simple sanity checking;
  o  put back the obsolete byte-codes for error-detection;
  o  added a new instruction, unbind_all, which I will use for
     tail-recursion elimination;
  o  made temp_output_buffer_show be called with the right number
     of args;
  o  made the new bytecodes be called with args in the right order;
  o  added metering support.

by Hallvard:
  o  added relative jump instructions;
  o  all conditionals now only do QUIT if they jump.
 */

#include <config.h>
#include <setjmp.h>
#include "lisp.h"
#include "buffer.h"
#include "character.h"
#include "syntax.h"
#include "window.h"

#ifdef CHECK_FRAME_FONT
#include "frame.h"
#include "xterm.h"
#endif

/*
 * define BYTE_CODE_SAFE to enable some minor sanity checking (useful for
 * debugging the byte compiler...)
 *
 * define BYTE_CODE_METER to enable generation of a byte-op usage histogram.
 */
/* #define BYTE_CODE_SAFE */
/* #define BYTE_CODE_METER */


#ifdef BYTE_CODE_METER

Lisp_Object Qbyte_code_meter;
#define METER_2(code1, code2) \
  XFASTINT (XVECTOR (XVECTOR (Vbyte_code_meter)->contents[(code1)]) \
	    ->contents[(code2)])

#define METER_1(code) METER_2 (0, (code))

#define METER_CODE(last_code, this_code)				\
{									\
  if (byte_metering_on)							\
    {									\
      if (METER_1 (this_code) < MOST_POSITIVE_FIXNUM)			\
        METER_1 (this_code)++;						\
      if (last_code							\
	  && METER_2 (last_code, this_code) < MOST_POSITIVE_FIXNUM)	\
        METER_2 (last_code, this_code)++;				\
    }									\
}

#endif /* BYTE_CODE_METER */


Lisp_Object Qbytecode;

/*  Byte codes: */

#define Bstack_ref 0 /* Actually, Bstack_ref+0 is not implemented: use dup.  */
#define Bvarref 010
#define Bvarset 020
#define Bvarbind 030
#define Bcall 040
#define Bunbind 050

#define Bnth 070
#define Bsymbolp 071
#define Bconsp 072
#define Bstringp 073
#define Blistp 074
#define Beq 075
#define Bmemq 076
#define Bnot 077
#define Bcar 0100
#define Bcdr 0101
#define Bcons 0102
#define Blist1 0103
#define Blist2 0104
#define Blist3 0105
#define Blist4 0106
#define Blength 0107
#define Baref 0110
#define Baset 0111
#define Bsymbol_value 0112
#define Bsymbol_function 0113
#define Bset 0114
#define Bfset 0115
#define Bget 0116
#define Bsubstring 0117
#define Bconcat2 0120
#define Bconcat3 0121
#define Bconcat4 0122
#define Bsub1 0123
#define Badd1 0124
#define Beqlsign 0125
#define Bgtr 0126
#define Blss 0127
#define Bleq 0130
#define Bgeq 0131
#define Bdiff 0132
#define Bnegate 0133
#define Bplus 0134
#define Bmax 0135
#define Bmin 0136
#define Bmult 0137

#define Bpoint 0140
/* Was Bmark in v17.  */
#define Bsave_current_buffer 0141 /* Obsolete.  */
#define Bgoto_char 0142
#define Binsert 0143
#define Bpoint_max 0144
#define Bpoint_min 0145
#define Bchar_after 0146
#define Bfollowing_char 0147
#define Bpreceding_char 0150
#define Bcurrent_column 0151
#define Bindent_to 0152
#ifdef BYTE_CODE_SAFE
#define Bscan_buffer 0153 /* No longer generated as of v18.  */
#endif
#define Beolp 0154
#define Beobp 0155
#define Bbolp 0156
#define Bbobp 0157
#define Bcurrent_buffer 0160
#define Bset_buffer 0161
#define Bsave_current_buffer_1 0162 /* Replacing Bsave_current_buffer.  */
#if 0
#define Bread_char 0162 /* No longer generated as of v19 */
#endif
#ifdef BYTE_CODE_SAFE
#define Bset_mark 0163 /* this loser is no longer generated as of v18 */
#endif
#define Binteractive_p 0164 /* Obsolete since Emacs-24.1.  */

#define Bforward_char 0165
#define Bforward_word 0166
#define Bskip_chars_forward 0167
#define Bskip_chars_backward 0170
#define Bforward_line 0171
#define Bchar_syntax 0172
#define Bbuffer_substring 0173
#define Bdelete_region 0174
#define Bnarrow_to_region 0175
#define Bwiden 0176
#define Bend_of_line 0177

#define Bconstant2 0201
#define Bgoto 0202
#define Bgotoifnil 0203
#define Bgotoifnonnil 0204
#define Bgotoifnilelsepop 0205
#define Bgotoifnonnilelsepop 0206
#define Breturn 0207
#define Bdiscard 0210
#define Bdup 0211

#define Bsave_excursion 0212
#define Bsave_window_excursion 0213 /* Obsolete since Emacs-24.1.  */
#define Bsave_restriction 0214
#define Bcatch 0215

#define Bunwind_protect 0216
#define Bcondition_case 0217
#define Btemp_output_buffer_setup 0220 /* Obsolete since Emacs-24.1.  */
#define Btemp_output_buffer_show 0221  /* Obsolete since Emacs-24.1.  */

#define Bunbind_all 0222	/* Obsolete.  Never used.  */

#define Bset_marker 0223
#define Bmatch_beginning 0224
#define Bmatch_end 0225
#define Bupcase 0226
#define Bdowncase 0227

#define Bstringeqlsign 0230
#define Bstringlss 0231
#define Bequal 0232
#define Bnthcdr 0233
#define Belt 0234
#define Bmember 0235
#define Bassq 0236
#define Bnreverse 0237
#define Bsetcar 0240
#define Bsetcdr 0241
#define Bcar_safe 0242
#define Bcdr_safe 0243
#define Bnconc 0244
#define Bquo 0245
#define Brem 0246
#define Bnumberp 0247
#define Bintegerp 0250

#define BRgoto 0252
#define BRgotoifnil 0253
#define BRgotoifnonnil 0254
#define BRgotoifnilelsepop 0255
#define BRgotoifnonnilelsepop 0256

#define BlistN 0257
#define BconcatN 0260
#define BinsertN 0261

/* Bstack_ref is code 0.  */
#define Bstack_set  0262
#define Bstack_set2 0263
#define BdiscardN   0266

#define Bconstant 0300

/* Whether to maintain a `top' and `bottom' field in the stack frame.  */
#define BYTE_MAINTAIN_TOP (BYTE_CODE_SAFE || BYTE_MARK_STACK)

/* Structure describing a value stack used during byte-code execution
   in Fbyte_code.  */

struct byte_stack
{
  /* Program counter.  This points into the byte_string below
     and is relocated when that string is relocated.  */
  const unsigned char *pc;

  /* Top and bottom of stack.  The bottom points to an area of memory
     allocated with alloca in Fbyte_code.  */
#if BYTE_MAINTAIN_TOP
  Lisp_Object *top, *bottom;
#endif

  /* The string containing the byte-code, and its current address.
     Storing this here protects it from GC because mark_byte_stack
     marks it.  */
  Lisp_Object byte_string;
  const unsigned char *byte_string_start;

  /* The vector of constants used during byte-code execution.  Storing
     this here protects it from GC because mark_byte_stack marks it.  */
  Lisp_Object constants;

  /* Next entry in byte_stack_list.  */
  struct byte_stack *next;
};

/* A list of currently active byte-code execution value stacks.
   Fbyte_code adds an entry to the head of this list before it starts
   processing byte-code, and it removed the entry again when it is
   done.  Signaling an error truncates the list analogous to
   gcprolist.  */

struct byte_stack *byte_stack_list;


/* Mark objects on byte_stack_list.  Called during GC.  */

#if BYTE_MARK_STACK
void
mark_byte_stack (void)
{
  struct byte_stack *stack;
  Lisp_Object *obj;

  for (stack = byte_stack_list; stack; stack = stack->next)
    {
      /* If STACK->top is null here, this means there's an opcode in
	 Fbyte_code that wasn't expected to GC, but did.  To find out
	 which opcode this is, record the value of `stack', and walk
	 up the stack in a debugger, stopping in frames of Fbyte_code.
	 The culprit is found in the frame of Fbyte_code where the
	 address of its local variable `stack' is equal to the
	 recorded value of `stack' here.  */
      eassert (stack->top);

      for (obj = stack->bottom; obj <= stack->top; ++obj)
	mark_object (*obj);

      mark_object (stack->byte_string);
      mark_object (stack->constants);
    }
}
#endif

/* Unmark objects in the stacks on byte_stack_list.  Relocate program
   counters.  Called when GC has completed.  */

void
unmark_byte_stack (void)
{
  struct byte_stack *stack;

  for (stack = byte_stack_list; stack; stack = stack->next)
    {
      if (stack->byte_string_start != SDATA (stack->byte_string))
	{
	  int offset = stack->pc - stack->byte_string_start;
	  stack->byte_string_start = SDATA (stack->byte_string);
	  stack->pc = stack->byte_string_start + offset;
	}
    }
}


/* Fetch the next byte from the bytecode stream */

#define FETCH *stack.pc++

/* Fetch two bytes from the bytecode stream and make a 16-bit number
   out of them */

#define FETCH2 (op = FETCH, op + (FETCH << 8))

/* Push x onto the execution stack.  This used to be #define PUSH(x)
   (*++stackp = (x)) This oddity is necessary because Alliant can't be
   bothered to compile the preincrement operator properly, as of 4/91.
   -JimB */

#define PUSH(x) (top++, *top = (x))

/* Pop a value off the execution stack.  */

#define POP (*top--)

/* Discard n values from the execution stack.  */

#define DISCARD(n) (top -= (n))

/* Get the value which is at the top of the execution stack, but don't
   pop it. */

#define TOP (*top)

/* Actions that must be performed before and after calling a function
   that might GC.  */

#if !BYTE_MAINTAIN_TOP
#define BEFORE_POTENTIAL_GC()	((void)0)
#define AFTER_POTENTIAL_GC()	((void)0)
#else
#define BEFORE_POTENTIAL_GC()	stack.top = top
#define AFTER_POTENTIAL_GC()	stack.top = NULL
#endif

/* Garbage collect if we have consed enough since the last time.
   We do this at every branch, to avoid loops that never GC.  */

#define MAYBE_GC()					\
 do {							\
  if (consing_since_gc > gc_cons_threshold		\
      && consing_since_gc > gc_relative_threshold)	\
    {							\
      BEFORE_POTENTIAL_GC ();				\
      Fgarbage_collect ();				\
      AFTER_POTENTIAL_GC ();				\
    }							\
 } while (0)

/* Check for jumping out of range.  */

#ifdef BYTE_CODE_SAFE

#define CHECK_RANGE(ARG) \
  if (ARG >= bytestr_length) abort ()

#else /* not BYTE_CODE_SAFE */

#define CHECK_RANGE(ARG)

#endif /* not BYTE_CODE_SAFE */

/* A version of the QUIT macro which makes sure that the stack top is
   set before signaling `quit'.  */

#define BYTE_CODE_QUIT					\
  do {							\
    if (!NILP (Vquit_flag) && NILP (Vinhibit_quit))	\
      {							\
        Lisp_Object flag = Vquit_flag;			\
	Vquit_flag = Qnil;				\
        BEFORE_POTENTIAL_GC ();				\
	if (EQ (Vthrow_on_input, flag))			\
	  Fthrow (Vthrow_on_input, Qt);			\
	Fsignal (Qquit, Qnil);				\
	AFTER_POTENTIAL_GC ();				\
      }							\
    ELSE_PENDING_SIGNALS				\
  } while (0)


DEFUN ("byte-code", Fbyte_code, Sbyte_code, 3, 3, 0,
       doc: /* Function used internally in byte-compiled code.
The first argument, BYTESTR, is a string of byte code;
the second, VECTOR, a vector of constants;
the third, MAXDEPTH, the maximum stack depth used in this function.
If the third argument is incorrect, Emacs may crash.  */)
  (Lisp_Object bytestr, Lisp_Object vector, Lisp_Object maxdepth)
{
  return exec_byte_code (bytestr, vector, maxdepth, Qnil, 0, NULL);
}

/* Execute the byte-code in BYTESTR.  VECTOR is the constant vector, and
   MAXDEPTH is the maximum stack depth used (if MAXDEPTH is incorrect,
   emacs may crash!).  If ARGS_TEMPLATE is non-nil, it should be a lisp
   argument list (including &rest, &optional, etc.), and ARGS, of size
   NARGS, should be a vector of the actual arguments.  The arguments in
   ARGS are pushed on the stack according to ARGS_TEMPLATE before
   executing BYTESTR.  */

Lisp_Object
exec_byte_code (Lisp_Object bytestr, Lisp_Object vector, Lisp_Object maxdepth,
		Lisp_Object args_template, ptrdiff_t nargs, Lisp_Object *args)
{
  int count = SPECPDL_INDEX ();
#ifdef BYTE_CODE_METER
  int this_op = 0;
  int prev_op;
#endif
  int op;
  /* Lisp_Object v1, v2; */
  Lisp_Object *vectorp;
#ifdef BYTE_CODE_SAFE
  ptrdiff_t const_length;
  Lisp_Object *stacke;
  int bytestr_length;
#endif
  struct byte_stack stack;
  Lisp_Object *top;
  Lisp_Object result;

#if 0 /* CHECK_FRAME_FONT */
 {
   struct frame *f = SELECTED_FRAME ();
   if (FRAME_X_P (f)
       && FRAME_FONT (f)->direction != 0
       && FRAME_FONT (f)->direction != 1)
     abort ();
 }
#endif

  CHECK_STRING (bytestr);
  CHECK_VECTOR (vector);
  CHECK_NATNUM (maxdepth);

#ifdef BYTE_CODE_SAFE
  const_length = ASIZE (vector);
#endif

  if (STRING_MULTIBYTE (bytestr))
    /* BYTESTR must have been produced by Emacs 20.2 or the earlier
       because they produced a raw 8-bit string for byte-code and now
       such a byte-code string is loaded as multibyte while raw 8-bit
       characters converted to multibyte form.  Thus, now we must
       convert them back to the originally intended unibyte form.  */
    bytestr = Fstring_as_unibyte (bytestr);

#ifdef BYTE_CODE_SAFE
  bytestr_length = SBYTES (bytestr);
#endif
  vectorp = XVECTOR (vector)->contents;

  stack.byte_string = bytestr;
  stack.pc = stack.byte_string_start = SDATA (bytestr);
  stack.constants = vector;
  if (min (PTRDIFF_MAX, SIZE_MAX) / sizeof (Lisp_Object) < XFASTINT (maxdepth))
    memory_full (SIZE_MAX);
  top = (Lisp_Object *) alloca (XFASTINT (maxdepth)
                                         * sizeof (Lisp_Object));
#if BYTE_MAINTAIN_TOP
  stack.bottom = top;
  stack.top = NULL;
#endif
  top -= 1;
  stack.next = byte_stack_list;
  byte_stack_list = &stack;

#ifdef BYTE_CODE_SAFE
  stacke = stack.bottom - 1 + XFASTINT (maxdepth);
#endif

  if (INTEGERP (args_template))
    {
      ptrdiff_t at = XINT (args_template);
      int rest = at & 128;
      int mandatory = at & 127;
      ptrdiff_t nonrest = at >> 8;
      eassert (mandatory <= nonrest);
      if (nargs <= nonrest)
	{
	  ptrdiff_t i;
	  for (i = 0 ; i < nargs; i++, args++)
	    PUSH (*args);
	  if (nargs < mandatory)
	    /* Too few arguments.  */
	    Fsignal (Qwrong_number_of_arguments,
		     Fcons (Fcons (make_number (mandatory),
				   rest ? Qand_rest : make_number (nonrest)),
			    Fcons (make_number (nargs), Qnil)));
	  else
	    {
	      for (; i < nonrest; i++)
		PUSH (Qnil);
	      if (rest)
		PUSH (Qnil);
	    }
	}
      else if (rest)
	{
	  ptrdiff_t i;
	  for (i = 0 ; i < nonrest; i++, args++)
	    PUSH (*args);
	  PUSH (Flist (nargs - nonrest, args));
	}
      else
	/* Too many arguments.  */
	Fsignal (Qwrong_number_of_arguments,
		 Fcons (Fcons (make_number (mandatory),
			       make_number (nonrest)),
			Fcons (make_number (nargs), Qnil)));
    }
  else if (! NILP (args_template))
    /* We should push some arguments on the stack.  */
    {
      error ("Unknown args template!");
    }

  while (1)
    {
#ifdef BYTE_CODE_SAFE
      if (top > stacke)
	abort ();
      else if (top < stack.bottom - 1)
	abort ();
#endif

#ifdef BYTE_CODE_METER
      prev_op = this_op;
      this_op = op = FETCH;
      METER_CODE (prev_op, op);
#else
      op = FETCH;
#endif

      switch (op)
	{
	case Bvarref + 7:
	  op = FETCH2;
	  goto varref;

	case Bvarref:
	case Bvarref + 1:
	case Bvarref + 2:
	case Bvarref + 3:
	case Bvarref + 4:
	case Bvarref + 5:
	  op = op - Bvarref;
	  goto varref;

	/* This seems to be the most frequently executed byte-code
	   among the Bvarref's, so avoid a goto here.  */
	case Bvarref+6:
	  op = FETCH;
	varref:
	  {
	    Lisp_Object v1, v2;

	    v1 = vectorp[op];
	    if (SYMBOLP (v1))
	      {
		if (XSYMBOL (v1)->redirect != SYMBOL_PLAINVAL
		    || (v2 = SYMBOL_VAL (XSYMBOL (v1)),
			EQ (v2, Qunbound)))
		  {
		    BEFORE_POTENTIAL_GC ();
		    v2 = Fsymbol_value (v1);
		    AFTER_POTENTIAL_GC ();
		  }
	      }
	    else
	      {
		BEFORE_POTENTIAL_GC ();
		v2 = Fsymbol_value (v1);
		AFTER_POTENTIAL_GC ();
	      }
	    PUSH (v2);
	    break;
	  }

	case Bgotoifnil:
	  {
	    Lisp_Object v1;
	    MAYBE_GC ();
	    op = FETCH2;
	    v1 = POP;
	    if (NILP (v1))
	      {
		BYTE_CODE_QUIT;
		CHECK_RANGE (op);
		stack.pc = stack.byte_string_start + op;
	      }
	    break;
	  }

	case Bcar:
	  {
	    Lisp_Object v1;
	    v1 = TOP;
	    if (CONSP (v1))
	      TOP = XCAR (v1);
	    else if (NILP (v1))
	      TOP = Qnil;
	    else
	      {
		BEFORE_POTENTIAL_GC ();
		wrong_type_argument (Qlistp, v1);
		AFTER_POTENTIAL_GC ();
	      }
	    break;
	  }

	case Beq:
	  {
	    Lisp_Object v1;
	    v1 = POP;
	    TOP = EQ (v1, TOP) ? Qt : Qnil;
	    break;
	  }

	case Bmemq:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fmemq (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bcdr:
	  {
	    Lisp_Object v1;
	    v1 = TOP;
	    if (CONSP (v1))
	      TOP = XCDR (v1);
	    else if (NILP (v1))
	      TOP = Qnil;
	    else
	      {
		BEFORE_POTENTIAL_GC ();
		wrong_type_argument (Qlistp, v1);
		AFTER_POTENTIAL_GC ();
	      }
	    break;
	    break;
	  }

	case Bvarset:
	case Bvarset+1:
	case Bvarset+2:
	case Bvarset+3:
	case Bvarset+4:
	case Bvarset+5:
	  op -= Bvarset;
	  goto varset;

	case Bvarset+7:
	  op = FETCH2;
	  goto varset;

	case Bvarset+6:
	  op = FETCH;
	varset:
	  {
	    Lisp_Object sym, val;

	    sym = vectorp[op];
	    val = TOP;

	    /* Inline the most common case.  */
	    if (SYMBOLP (sym)
		&& !EQ (val, Qunbound)
		&& !XSYMBOL (sym)->redirect
		&& !SYMBOL_CONSTANT_P (sym))
	      XSYMBOL (sym)->val.value = val;
	    else
	      {
		BEFORE_POTENTIAL_GC ();
		set_internal (sym, val, Qnil, 0);
		AFTER_POTENTIAL_GC ();
	      }
	  }
	  (void) POP;
	  break;

	case Bdup:
	  {
	    Lisp_Object v1;
	    v1 = TOP;
	    PUSH (v1);
	    break;
	  }

	/* ------------------ */

	case Bvarbind+6:
	  op = FETCH;
	  goto varbind;

	case Bvarbind+7:
	  op = FETCH2;
	  goto varbind;

	case Bvarbind:
	case Bvarbind+1:
	case Bvarbind+2:
	case Bvarbind+3:
	case Bvarbind+4:
	case Bvarbind+5:
	  op -= Bvarbind;
	varbind:
	  /* Specbind can signal and thus GC.  */
	  BEFORE_POTENTIAL_GC ();
	  specbind (vectorp[op], POP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bcall+6:
	  op = FETCH;
	  goto docall;

	case Bcall+7:
	  op = FETCH2;
	  goto docall;

	case Bcall:
	case Bcall+1:
	case Bcall+2:
	case Bcall+3:
	case Bcall+4:
	case Bcall+5:
	  op -= Bcall;
	docall:
	  {
	    BEFORE_POTENTIAL_GC ();
	    DISCARD (op);
#ifdef BYTE_CODE_METER
	    if (byte_metering_on && SYMBOLP (TOP))
	      {
		Lisp_Object v1, v2;

		v1 = TOP;
		v2 = Fget (v1, Qbyte_code_meter);
		if (INTEGERP (v2)
		    && XINT (v2) < MOST_POSITIVE_FIXNUM)
		  {
		    XSETINT (v2, XINT (v2) + 1);
		    Fput (v1, Qbyte_code_meter, v2);
		  }
	      }
#endif
	    TOP = Ffuncall (op + 1, &TOP);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bunbind+6:
	  op = FETCH;
	  goto dounbind;

	case Bunbind+7:
	  op = FETCH2;
	  goto dounbind;

	case Bunbind:
	case Bunbind+1:
	case Bunbind+2:
	case Bunbind+3:
	case Bunbind+4:
	case Bunbind+5:
	  op -= Bunbind;
	dounbind:
	  BEFORE_POTENTIAL_GC ();
	  unbind_to (SPECPDL_INDEX () - op, Qnil);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bunbind_all:	/* Obsolete.  Never used.  */
	  /* To unbind back to the beginning of this frame.  Not used yet,
	     but will be needed for tail-recursion elimination.  */
	  BEFORE_POTENTIAL_GC ();
	  unbind_to (count, Qnil);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bgoto:
	  MAYBE_GC ();
	  BYTE_CODE_QUIT;
	  op = FETCH2;    /* pc = FETCH2 loses since FETCH2 contains pc++ */
	  CHECK_RANGE (op);
	  stack.pc = stack.byte_string_start + op;
	  break;

	case Bgotoifnonnil:
	  {
	    Lisp_Object v1;
	    MAYBE_GC ();
	    op = FETCH2;
	    v1 = POP;
	    if (!NILP (v1))
	      {
		BYTE_CODE_QUIT;
		CHECK_RANGE (op);
		stack.pc = stack.byte_string_start + op;
	      }
	    break;
	  }

	case Bgotoifnilelsepop:
	  MAYBE_GC ();
	  op = FETCH2;
	  if (NILP (TOP))
	    {
	      BYTE_CODE_QUIT;
	      CHECK_RANGE (op);
	      stack.pc = stack.byte_string_start + op;
	    }
	  else DISCARD (1);
	  break;

	case Bgotoifnonnilelsepop:
	  MAYBE_GC ();
	  op = FETCH2;
	  if (!NILP (TOP))
	    {
	      BYTE_CODE_QUIT;
	      CHECK_RANGE (op);
	      stack.pc = stack.byte_string_start + op;
	    }
	  else DISCARD (1);
	  break;

	case BRgoto:
	  MAYBE_GC ();
	  BYTE_CODE_QUIT;
	  stack.pc += (int) *stack.pc - 127;
	  break;

	case BRgotoifnil:
	  {
	    Lisp_Object v1;
	    MAYBE_GC ();
	    v1 = POP;
	    if (NILP (v1))
	      {
		BYTE_CODE_QUIT;
		stack.pc += (int) *stack.pc - 128;
	      }
	    stack.pc++;
	    break;
	  }

	case BRgotoifnonnil:
	  {
	    Lisp_Object v1;
	    MAYBE_GC ();
	    v1 = POP;
	    if (!NILP (v1))
	      {
		BYTE_CODE_QUIT;
		stack.pc += (int) *stack.pc - 128;
	      }
	    stack.pc++;
	    break;
	  }

	case BRgotoifnilelsepop:
	  MAYBE_GC ();
	  op = *stack.pc++;
	  if (NILP (TOP))
	    {
	      BYTE_CODE_QUIT;
	      stack.pc += op - 128;
	    }
	  else DISCARD (1);
	  break;

	case BRgotoifnonnilelsepop:
	  MAYBE_GC ();
	  op = *stack.pc++;
	  if (!NILP (TOP))
	    {
	      BYTE_CODE_QUIT;
	      stack.pc += op - 128;
	    }
	  else DISCARD (1);
	  break;

	case Breturn:
	  result = POP;
	  goto exit;

	case Bdiscard:
	  DISCARD (1);
	  break;

	case Bconstant2:
	  PUSH (vectorp[FETCH2]);
	  break;

	case Bsave_excursion:
	  record_unwind_protect (save_excursion_restore,
				 save_excursion_save ());
	  break;

	case Bsave_current_buffer: /* Obsolete since ??.  */
	case Bsave_current_buffer_1:
	  record_unwind_protect (set_buffer_if_live, Fcurrent_buffer ());
	  break;

	case Bsave_window_excursion: /* Obsolete since 24.1.  */
	  {
	    register int count1 = SPECPDL_INDEX ();
	    record_unwind_protect (Fset_window_configuration,
				   Fcurrent_window_configuration (Qnil));
	    BEFORE_POTENTIAL_GC ();
	    TOP = Fprogn (TOP);
	    unbind_to (count1, TOP);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bsave_restriction:
	  record_unwind_protect (save_restriction_restore,
				 save_restriction_save ());
	  break;

	case Bcatch:		/* FIXME: ill-suited for lexbind.  */
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = internal_catch (TOP, eval_sub, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bunwind_protect:	/* FIXME: avoid closure for lexbind.  */
	  record_unwind_protect (Fprogn, POP);
	  break;

	case Bcondition_case:	/* FIXME: ill-suited for lexbind.  */
	  {
	    Lisp_Object handlers, body;
	    handlers = POP;
	    body = POP;
	    BEFORE_POTENTIAL_GC ();
	    TOP = internal_lisp_condition_case (TOP, body, handlers);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Btemp_output_buffer_setup: /* Obsolete since 24.1.  */
	  BEFORE_POTENTIAL_GC ();
	  CHECK_STRING (TOP);
	  temp_output_buffer_setup (SSDATA (TOP));
	  AFTER_POTENTIAL_GC ();
	  TOP = Vstandard_output;
	  break;

	case Btemp_output_buffer_show: /* Obsolete since 24.1.  */
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    temp_output_buffer_show (TOP);
	    TOP = v1;
	    /* pop binding of standard-output */
	    unbind_to (SPECPDL_INDEX () - 1, Qnil);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bnth:
	  {
	    Lisp_Object v1, v2;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    v2 = TOP;
	    CHECK_NUMBER (v2);
	    op = XINT (v2);
	    immediate_quit = 1;
	    while (--op >= 0 && CONSP (v1))
	      v1 = XCDR (v1);
	    immediate_quit = 0;
	    TOP = CAR (v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bsymbolp:
	  TOP = SYMBOLP (TOP) ? Qt : Qnil;
	  break;

	case Bconsp:
	  TOP = CONSP (TOP) ? Qt : Qnil;
	  break;

	case Bstringp:
	  TOP = STRINGP (TOP) ? Qt : Qnil;
	  break;

	case Blistp:
	  TOP = CONSP (TOP) || NILP (TOP) ? Qt : Qnil;
	  break;

	case Bnot:
	  TOP = NILP (TOP) ? Qt : Qnil;
	  break;

	case Bcons:
	  {
	    Lisp_Object v1;
	    v1 = POP;
	    TOP = Fcons (TOP, v1);
	    break;
	  }

	case Blist1:
	  TOP = Fcons (TOP, Qnil);
	  break;

	case Blist2:
	  {
	    Lisp_Object v1;
	    v1 = POP;
	    TOP = Fcons (TOP, Fcons (v1, Qnil));
	    break;
	  }

	case Blist3:
	  DISCARD (2);
	  TOP = Flist (3, &TOP);
	  break;

	case Blist4:
	  DISCARD (3);
	  TOP = Flist (4, &TOP);
	  break;

	case BlistN:
	  op = FETCH;
	  DISCARD (op - 1);
	  TOP = Flist (op, &TOP);
	  break;

	case Blength:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Flength (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Baref:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Faref (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Baset:
	  {
	    Lisp_Object v1, v2;
	    BEFORE_POTENTIAL_GC ();
	    v2 = POP; v1 = POP;
	    TOP = Faset (TOP, v1, v2);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bsymbol_value:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fsymbol_value (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bsymbol_function:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fsymbol_function (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bset:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fset (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bfset:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Ffset (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bget:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fget (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bsubstring:
	  {
	    Lisp_Object v1, v2;
	    BEFORE_POTENTIAL_GC ();
	    v2 = POP; v1 = POP;
	    TOP = Fsubstring (TOP, v1, v2);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bconcat2:
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (1);
	  TOP = Fconcat (2, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bconcat3:
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (2);
	  TOP = Fconcat (3, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bconcat4:
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (3);
	  TOP = Fconcat (4, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case BconcatN:
	  op = FETCH;
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (op - 1);
	  TOP = Fconcat (op, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bsub1:
	  {
	    Lisp_Object v1;
	    v1 = TOP;
	    if (INTEGERP (v1))
	      {
		XSETINT (v1, XINT (v1) - 1);
		TOP = v1;
	      }
	    else
	      {
		BEFORE_POTENTIAL_GC ();
		TOP = Fsub1 (v1);
		AFTER_POTENTIAL_GC ();
	      }
	    break;
	  }

	case Badd1:
	  {
	    Lisp_Object v1;
	    v1 = TOP;
	    if (INTEGERP (v1))
	      {
		XSETINT (v1, XINT (v1) + 1);
		TOP = v1;
	      }
	    else
	      {
		BEFORE_POTENTIAL_GC ();
		TOP = Fadd1 (v1);
		AFTER_POTENTIAL_GC ();
	      }
	    break;
	  }

	case Beqlsign:
	  {
	    Lisp_Object v1, v2;
	    BEFORE_POTENTIAL_GC ();
	    v2 = POP; v1 = TOP;
	    CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (v1);
	    CHECK_NUMBER_OR_FLOAT_COERCE_MARKER (v2);
	    AFTER_POTENTIAL_GC ();
	    if (FLOATP (v1) || FLOATP (v2))
	      {
		double f1, f2;

		f1 = (FLOATP (v1) ? XFLOAT_DATA (v1) : XINT (v1));
		f2 = (FLOATP (v2) ? XFLOAT_DATA (v2) : XINT (v2));
		TOP = (f1 == f2 ? Qt : Qnil);
	      }
	    else
	      TOP = (XINT (v1) == XINT (v2) ? Qt : Qnil);
	    break;
	  }

	case Bgtr:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fgtr (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Blss:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Flss (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bleq:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fleq (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bgeq:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fgeq (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bdiff:
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (1);
	  TOP = Fminus (2, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bnegate:
	  {
	    Lisp_Object v1;
	    v1 = TOP;
	    if (INTEGERP (v1))
	      {
		XSETINT (v1, - XINT (v1));
		TOP = v1;
	      }
	    else
	      {
		BEFORE_POTENTIAL_GC ();
		TOP = Fminus (1, &TOP);
		AFTER_POTENTIAL_GC ();
	      }
	    break;
	  }

	case Bplus:
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (1);
	  TOP = Fplus (2, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bmax:
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (1);
	  TOP = Fmax (2, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bmin:
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (1);
	  TOP = Fmin (2, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bmult:
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (1);
	  TOP = Ftimes (2, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bquo:
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (1);
	  TOP = Fquo (2, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Brem:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Frem (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bpoint:
	  {
	    Lisp_Object v1;
	    XSETFASTINT (v1, PT);
	    PUSH (v1);
	    break;
	  }

	case Bgoto_char:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fgoto_char (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Binsert:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Finsert (1, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case BinsertN:
	  op = FETCH;
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (op - 1);
	  TOP = Finsert (op, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bpoint_max:
	  {
	    Lisp_Object v1;
	    XSETFASTINT (v1, ZV);
	    PUSH (v1);
	    break;
	  }

	case Bpoint_min:
	  {
	    Lisp_Object v1;
	    XSETFASTINT (v1, BEGV);
	    PUSH (v1);
	    break;
	  }

	case Bchar_after:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fchar_after (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bfollowing_char:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = Ffollowing_char ();
	    AFTER_POTENTIAL_GC ();
	    PUSH (v1);
	    break;
	  }

	case Bpreceding_char:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = Fprevious_char ();
	    AFTER_POTENTIAL_GC ();
	    PUSH (v1);
	    break;
	  }

	case Bcurrent_column:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    XSETFASTINT (v1, current_column ());
	    AFTER_POTENTIAL_GC ();
	    PUSH (v1);
	    break;
	  }

	case Bindent_to:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Findent_to (TOP, Qnil);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Beolp:
	  PUSH (Feolp ());
	  break;

	case Beobp:
	  PUSH (Feobp ());
	  break;

	case Bbolp:
	  PUSH (Fbolp ());
	  break;

	case Bbobp:
	  PUSH (Fbobp ());
	  break;

	case Bcurrent_buffer:
	  PUSH (Fcurrent_buffer ());
	  break;

	case Bset_buffer:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fset_buffer (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Binteractive_p:	/* Obsolete since 24.1.  */
	  PUSH (Finteractive_p ());
	  break;

	case Bforward_char:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fforward_char (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bforward_word:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fforward_word (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bskip_chars_forward:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fskip_chars_forward (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bskip_chars_backward:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fskip_chars_backward (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bforward_line:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fforward_line (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bchar_syntax:
	  {
	    int c;

	    BEFORE_POTENTIAL_GC ();
	    CHECK_CHARACTER (TOP);
	    AFTER_POTENTIAL_GC ();
	    c = XFASTINT (TOP);
	    if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	      MAKE_CHAR_MULTIBYTE (c);
	    XSETFASTINT (TOP, syntax_code_spec[(int) SYNTAX (c)]);
	  }
	  break;

	case Bbuffer_substring:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fbuffer_substring (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bdelete_region:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fdelete_region (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bnarrow_to_region:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fnarrow_to_region (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bwiden:
	  BEFORE_POTENTIAL_GC ();
	  PUSH (Fwiden ());
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bend_of_line:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fend_of_line (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bset_marker:
	  {
	    Lisp_Object v1, v2;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    v2 = POP;
	    TOP = Fset_marker (TOP, v2, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bmatch_beginning:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fmatch_beginning (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bmatch_end:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fmatch_end (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bupcase:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fupcase (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bdowncase:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fdowncase (TOP);
	  AFTER_POTENTIAL_GC ();
	break;

	case Bstringeqlsign:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fstring_equal (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bstringlss:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fstring_lessp (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bequal:
	  {
	    Lisp_Object v1;
	    v1 = POP;
	    TOP = Fequal (TOP, v1);
	    break;
	  }

	case Bnthcdr:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fnthcdr (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Belt:
	  {
	    Lisp_Object v1, v2;
	    if (CONSP (TOP))
	      {
		/* Exchange args and then do nth.  */
		BEFORE_POTENTIAL_GC ();
		v2 = POP;
		v1 = TOP;
		CHECK_NUMBER (v2);
		AFTER_POTENTIAL_GC ();
		op = XINT (v2);
		immediate_quit = 1;
		while (--op >= 0 && CONSP (v1))
		  v1 = XCDR (v1);
		immediate_quit = 0;
		TOP = CAR (v1);
	      }
	    else
	      {
		BEFORE_POTENTIAL_GC ();
		v1 = POP;
		TOP = Felt (TOP, v1);
		AFTER_POTENTIAL_GC ();
	      }
	    break;
	  }

	case Bmember:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fmember (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bassq:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fassq (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bnreverse:
	  BEFORE_POTENTIAL_GC ();
	  TOP = Fnreverse (TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bsetcar:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fsetcar (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bsetcdr:
	  {
	    Lisp_Object v1;
	    BEFORE_POTENTIAL_GC ();
	    v1 = POP;
	    TOP = Fsetcdr (TOP, v1);
	    AFTER_POTENTIAL_GC ();
	    break;
	  }

	case Bcar_safe:
	  {
	    Lisp_Object v1;
	    v1 = TOP;
	    TOP = CAR_SAFE (v1);
	    break;
	  }

	case Bcdr_safe:
	  {
	    Lisp_Object v1;
	    v1 = TOP;
	    TOP = CDR_SAFE (v1);
	    break;
	  }

	case Bnconc:
	  BEFORE_POTENTIAL_GC ();
	  DISCARD (1);
	  TOP = Fnconc (2, &TOP);
	  AFTER_POTENTIAL_GC ();
	  break;

	case Bnumberp:
	  TOP = (NUMBERP (TOP) ? Qt : Qnil);
	  break;

	case Bintegerp:
	  TOP = INTEGERP (TOP) ? Qt : Qnil;
	  break;

#ifdef BYTE_CODE_SAFE
	case Bset_mark:
	  BEFORE_POTENTIAL_GC ();
	  error ("set-mark is an obsolete bytecode");
	  AFTER_POTENTIAL_GC ();
	  break;
	case Bscan_buffer:
	  BEFORE_POTENTIAL_GC ();
	  error ("scan-buffer is an obsolete bytecode");
	  AFTER_POTENTIAL_GC ();
	  break;
#endif

	case 0:
	  /* Actually this is Bstack_ref with offset 0, but we use Bdup
	     for that instead.  */
	  /* case Bstack_ref: */
	  abort ();

	  /* Handy byte-codes for lexical binding.  */
	case Bstack_ref+1:
	case Bstack_ref+2:
	case Bstack_ref+3:
	case Bstack_ref+4:
	case Bstack_ref+5:
	  {
	    Lisp_Object *ptr = top - (op - Bstack_ref);
	    PUSH (*ptr);
	    break;
	  }
	case Bstack_ref+6:
	  {
	    Lisp_Object *ptr = top - (FETCH);
	    PUSH (*ptr);
	    break;
	  }
	case Bstack_ref+7:
	  {
	    Lisp_Object *ptr = top - (FETCH2);
	    PUSH (*ptr);
	    break;
	  }
	case Bstack_set:
	  /* stack-set-0 = discard; stack-set-1 = discard-1-preserve-tos.  */
	  {
	    Lisp_Object *ptr = top - (FETCH);
	    *ptr = POP;
	    break;
	  }
	case Bstack_set2:
	  {
	    Lisp_Object *ptr = top - (FETCH2);
	    *ptr = POP;
	    break;
	  }
	case BdiscardN:
	  op = FETCH;
	  if (op & 0x80)
	    {
	      op &= 0x7F;
	      top[-op] = TOP;
	    }
	  DISCARD (op);
	  break;

	case 255:
	default:
#ifdef BYTE_CODE_SAFE
	  if (op < Bconstant)
	    {
	      abort ();
	    }
	  if ((op -= Bconstant) >= const_length)
	    {
	      abort ();
	    }
	  PUSH (vectorp[op]);
#else
	  PUSH (vectorp[op - Bconstant]);
#endif
	}
    }

 exit:

  byte_stack_list = byte_stack_list->next;

  /* Binds and unbinds are supposed to be compiled balanced.  */
  if (SPECPDL_INDEX () != count)
#ifdef BYTE_CODE_SAFE
    error ("binding stack not balanced (serious byte compiler bug)");
#else
    abort ();
#endif

  return result;
}

void
syms_of_bytecode (void)
{
  DEFSYM (Qbytecode, "byte-code");

  defsubr (&Sbyte_code);

#ifdef BYTE_CODE_METER

  DEFVAR_LISP ("byte-code-meter", Vbyte_code_meter,
	       doc: /* A vector of vectors which holds a histogram of byte-code usage.
\(aref (aref byte-code-meter 0) CODE) indicates how many times the byte
opcode CODE has been executed.
\(aref (aref byte-code-meter CODE1) CODE2), where CODE1 is not 0,
indicates how many times the byte opcodes CODE1 and CODE2 have been
executed in succession.  */);

  DEFVAR_BOOL ("byte-metering-on", byte_metering_on,
	       doc: /* If non-nil, keep profiling information on byte code usage.
The variable byte-code-meter indicates how often each byte opcode is used.
If a symbol has a property named `byte-code-meter' whose value is an
integer, it is incremented each time that symbol's function is called.  */);

  byte_metering_on = 0;
  Vbyte_code_meter = Fmake_vector (make_number (256), make_number (0));
  DEFSYM (Qbyte_code_meter, "byte-code-meter");
  {
    int i = 256;
    while (i--)
      XVECTOR (Vbyte_code_meter)->contents[i] =
	Fmake_vector (make_number (256), make_number (0));
  }
#endif
}
