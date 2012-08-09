/* blockinput.h - interface to blocking complicated interrupt-driven input.
   Copyright (C) 1989, 1993, 2001-2012  Free Software Foundation, Inc.

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

#ifndef EMACS_BLOCKINPUT_H
#define EMACS_BLOCKINPUT_H

#include "atimer.h"

/* When Emacs is using signal-driven input, the processing of those
   input signals can get pretty hairy.  For example, when Emacs is
   running under X windows, handling an input signal can entail
   retrieving events from the X event queue, or making other X calls.

   If an input signal occurs while Emacs is in the midst of some
   non-reentrant code, and the signal processing invokes that same
   code, we lose.  For example, malloc and the Xlib functions aren't
   usually re-entrant, and both are used by the X input signal handler
   - if we try to process an input signal in the midst of executing
   any of these functions, we'll lose.

   To avoid this, we make the following requirements:

   * Everyone must evaluate BLOCK_INPUT before entering these functions,
   and then call UNBLOCK_INPUT after performing them.  Calls
   BLOCK_INPUT and UNBLOCK_INPUT may be nested.

   * Any complicated interrupt handling code should test
   interrupt_input_blocked, and put off its work until later.

   * If the interrupt handling code wishes, it may set
   interrupt_input_pending to a non-zero value.  If that flag is set
   when input becomes unblocked, UNBLOCK_INPUT will send a new SIGIO.  */

extern volatile int interrupt_input_blocked;

/* Nonzero means an input interrupt has arrived
   during the current critical section.  */
extern int interrupt_input_pending;


/* Non-zero means asynchronous timers should be run when input is
   unblocked.  */

extern int pending_atimers;


/* Begin critical section. */
#define BLOCK_INPUT (interrupt_input_blocked++)

/* End critical section.

   If doing signal-driven input, and a signal came in when input was
   blocked, reinvoke the signal handler now to deal with it.

   We used to have two possible definitions of this macro - one for
   when SIGIO was #defined, and one for when it wasn't; when SIGIO
   wasn't #defined, we wouldn't bother to check if we should re-invoke
   the signal handler.  But that doesn't work very well; some of the
   files which use this macro don't #include the right files to get
   SIGIO.

   So, we always test interrupt_input_pending now; that's not too
   expensive, and it'll never get set if we don't need to resignal.  */

#define UNBLOCK_INPUT 				\
  do						\
    {						\
      --interrupt_input_blocked;		\
      if (interrupt_input_blocked == 0)		\
	{					\
	  if (interrupt_input_pending)		\
	    reinvoke_input_signal ();		\
	  if (pending_atimers)			\
	    do_pending_atimers ();		\
	}					\
      else if (interrupt_input_blocked < 0)	\
	abort ();				\
    }						\
  while (0)

/* Undo any number of BLOCK_INPUT calls,
   and also reinvoke any pending signal.  */

#define TOTALLY_UNBLOCK_INPUT			\
  do if (interrupt_input_blocked != 0)		\
    {						\
      interrupt_input_blocked = 1;		\
      UNBLOCK_INPUT;				\
    }						\
  while (0)

/* Undo any number of BLOCK_INPUT calls down to level LEVEL,
   and also (if the level is now 0) reinvoke any pending signal.  */

#define UNBLOCK_INPUT_TO(LEVEL)				\
  do							\
    {							\
      interrupt_input_blocked = (LEVEL) + 1;		\
      UNBLOCK_INPUT;					\
    }							\
  while (0)

#define UNBLOCK_INPUT_RESIGNAL UNBLOCK_INPUT

/* In critical section ? */
#define INPUT_BLOCKED_P (interrupt_input_blocked > 0)

/* Defined in keyboard.c */
extern void reinvoke_input_signal (void);

#endif /* EMACS_BLOCKINPUT_H */

