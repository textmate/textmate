/* Header for CCL (Code Conversion Language) interpreter.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
     2005, 2006, 2007, 2008, 2009, 2010, 2011
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H14PRO021
   Copyright (C) 2003
     National Institute of Advanced Industrial Science and Technology (AIST)
     Registration Number H13PRO009

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


#ifndef EMACS_CCL_H
#define EMACS_CCL_H

/* Macros for exit status of CCL program.  */
#define CCL_STAT_SUCCESS	0 /* Terminated successfully.  */
#define CCL_STAT_SUSPEND_BY_SRC	1 /* Terminated by empty input.  */
#define CCL_STAT_SUSPEND_BY_DST	2 /* Terminated by output buffer full.  */
#define CCL_STAT_INVALID_CMD	3 /* Terminated because of invalid
				     command.  */
#define CCL_STAT_QUIT		4 /* Terminated because of quit.  */

/* Structure to hold information about running CCL code.  Read
   comments in the file ccl.c for the detail of each field.  */
struct ccl_program {
  int idx;			/* Index number of the CCL program.
				   -1 means that the program was given
				   by a vector, not by a program
				   name.  */
  int size;			/* Size of the compiled code.  */
  Lisp_Object *prog;		/* Pointer into the compiled code.  */
  int ic;			/* Instruction Counter (index for PROG).  */
  int eof_ic;			/* Instruction Counter for end-of-file
				   processing code.  */
  int reg[8];			/* CCL registers, reg[7] is used for
				   condition flag of relational
				   operations.  */
  int private_state;            /* CCL instruction may use this
				   for private use, mainly for saving
				   internal states on suspending.
				   This variable is set to 0 when ccl is
				   set up.  */
  int last_block;		/* Set to 1 while processing the last
				   block. */
  int status;			/* Exit status of the CCL program.  */
  int buf_magnification;	/* Output buffer magnification.  How
				   many times bigger the output buffer
				   should be than the input buffer.  */
  int stack_idx;		/* How deep the call of CCL_Call is nested.  */
  int src_multibyte;		/* 1 if the input buffer is multibyte.  */
  int dst_multibyte;		/* 1 if the output buffer is multibyte.  */
  int cr_consumed;		/* Flag for encoding DOS-like EOL
				   format when the CCL program is used
				   for encoding by a coding
				   system.  */
  int consumed;
  int produced;
  int suppress_error;		/* If nonzero, don't insert error
				   message in the output.  */
  int eight_bit_control;	/* If nonzero, ccl_driver counts all
				   eight-bit-control bytes written by
				   CCL_WRITE_CHAR.  After execution,
				   if no such byte is written, set
				   this value to zero.  */
  int quit_silently;		/* If nonzero, don't append "CCL:
				   Quitted" to the generated text when
				   CCL program is quitted. */
};

/* This data type is used for the spec field of the structure
   coding_system.  */

struct ccl_spec {
  struct ccl_program ccl;
  int cr_carryover;		/* CR carryover flag.  */
  unsigned char eight_bit_carryover[MAX_MULTIBYTE_LENGTH];
};

#define CODING_SPEC_CCL_PROGRAM(coding) ((coding)->spec.ccl.ccl)

/* Setup fields of the structure pointed by CCL appropriately for the
   execution of ccl program CCL_PROG (symbol or vector).  */
extern int setup_ccl_program (struct ccl_program *, Lisp_Object);

extern void ccl_driver (struct ccl_program *, int *, int *, int, int,
                        Lisp_Object);

extern Lisp_Object Qccl, Qcclp;

EXFUN (Fccl_program_p, 1);

#define CHECK_CCL_PROGRAM(x)			\
  do {						\
    if (NILP (Fccl_program_p (x)))		\
      wrong_type_argument (Qcclp, (x));	\
  } while (0);

#endif /* EMACS_CCL_H */
