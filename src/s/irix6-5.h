/* Definitions file for GNU Emacs running on Silicon Graphics Irix system 6.5.

Copyright (C) 1999-2012  Free Software Foundation, Inc.

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


#define IRIX6_5			/* used in m/iris4d */
#include "usg5-4-common.h"

#undef _longjmp /* use system versions, not conservative aliases */
#undef _setjmp

#define SETPGRP_RELEASES_CTTY

#ifdef SYSTEM_TYPE
#undef SYSTEM_TYPE
#endif
#define SYSTEM_TYPE "irix"

#ifdef SETUP_SLAVE_PTY
#undef SETUP_SLAVE_PTY
#endif

/* thomas@mathematik.uni-bremen.de says this is needed.  */
/* Make process_send_signal work by "typing" a signal character on the pty.  */
#define SIGNALS_VIA_CHARACTERS

/* Letter to use in finding device name of first pty,
   if system supports pty's.  'a' means it is /dev/ptya0  */
#undef FIRST_PTY_LETTER
#define FIRST_PTY_LETTER 'q'

/* No need to use sprintf to get the tty name--we get that from _getpty.  */
#define PTY_TTY_NAME_SPRINTF
/* No need to get the pty name at all.  */
#ifdef PTY_NAME_SPRINTF
#undef PTY_NAME_SPRINTF
#endif
#define PTY_NAME_SPRINTF
#ifdef emacs
char *_getpty();
#endif
/* We need only try once to open a pty.  */
#define PTY_ITERATION
/* Here is how to do it.  */
#define PTY_OPEN					    \
{							    \
  struct sigaction ocstat, cstat;			    \
  struct stat stb;					    \
  char * name;						    \
  sigemptyset(&cstat.sa_mask);				    \
  cstat.sa_handler = SIG_DFL;				    \
  cstat.sa_flags = 0;					    \
  sigaction(SIGCLD, &cstat, &ocstat);			    \
  name = _getpty (&fd, O_RDWR | O_NDELAY, 0600, 0);	    \
  sigaction(SIGCLD, &ocstat, (struct sigaction *)0);	    \
  if (name == 0)					    \
    return -1;						    \
  if (fd < 0)						    \
    return -1;						    \
  if (fstat (fd, &stb) < 0)				    \
    return -1;						    \
  strcpy (pty_name, name);				    \
}

/* Ulimit(UL_GMEMLIM) is busted...  */
#define ULIMIT_BREAK_VALUE 0x14000000

/* Tell process_send_signal to use VSUSP instead of VSWTCH.  */
#define PREFER_VSUSP

#define NARROWPROTO 1

#if _MIPS_SZLONG == 64		/* -mabi=64 (gcc) or -64 (MIPSpro) */
#define _LP64			/* lisp.h takes care of the rest */
#endif /* _MIPS_SZLONG */

#undef SA_RESTART

#undef TIOCSIGSEND		/* defined in usg5-4-common.h */

/* Tested on Irix 6.5.  SCM worked on earlier versions.  */
#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS


/* DATA_SEG_BITS forces extra bits to be or'd in with any pointers which
   were stored in a Lisp_Object (as Emacs uses fewer than 32 bits for
   the value field of a LISP_OBJECT).  */
#define DATA_START 0x10000000
#define DATA_SEG_BITS	0x10000000
