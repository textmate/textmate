/* systty.h - System-dependent definitions for terminals.
   Copyright (C) 1993-1994, 2001-2012  Free Software Foundation, Inc.

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

/* Include the proper files.  */
#ifndef DOS_NT
#ifndef NO_TERMIO
#include <termio.h>
#endif /* not NO_TERMIO */
#include <termios.h>
#include <fcntl.h>
#endif /* not DOS_NT */

#include <sys/ioctl.h>

#ifdef HPUX
#include <sys/bsdtty.h>
#include <sys/ptyio.h>
#endif

#ifdef AIX
#include <sys/pty.h>
#endif /* AIX */

#include <unistd.h>


/* Special cases - inhibiting the use of certain features.  */

/* Allow m- file to inhibit use of FIONREAD.  */
#ifdef BROKEN_FIONREAD
#undef FIONREAD
#undef ASYNC
#endif

/* Interrupt input is not used if there is no FIONREAD.  */
#ifndef FIONREAD
#undef SIGIO
#endif


/* Try to establish the correct character to disable terminal functions
   in a system-independent manner.  Note that USG (at least) define
   _POSIX_VDISABLE as 0!  */

#ifdef _POSIX_VDISABLE
#define CDISABLE _POSIX_VDISABLE
#else /* not _POSIX_VDISABLE */
#ifdef CDEL
#undef CDISABLE
#define CDISABLE CDEL
#else /* not CDEL */
#define CDISABLE 255
#endif /* not CDEL */
#endif /* not _POSIX_VDISABLE */

/* Get the number of characters queued for output.  */

/* EMACS_OUTQSIZE(FD, int *SIZE) stores the number of characters
   queued for output to the terminal FD in *SIZE, if FD is a tty.
   Returns -1 if there was an error (i.e. FD is not a tty), 0
   otherwise.  */
#ifdef TIOCOUTQ
#define EMACS_OUTQSIZE(fd, size) (ioctl ((fd), TIOCOUTQ, (size)))
#endif


/* Manipulate a terminal's current process group.  */

/* EMACS_GETPGRP (arg) returns the process group of the process.  */

#if defined (GETPGRP_VOID)
#  define EMACS_GETPGRP(x) getpgrp()
#else /* !GETPGRP_VOID */
#  define EMACS_GETPGRP(x) getpgrp(x)
#endif /* !GETPGRP_VOID */

/* Manipulate a TTY's input/output processing parameters.  */

/* struct emacs_tty is a structure used to hold the current tty
   parameters.  If the terminal has several structures describing its
   state, for example a struct tchars, a struct sgttyb, a struct
   tchars, a struct ltchars, and a struct pagechars, struct
   emacs_tty should contain an element for each parameter struct
   that Emacs may change.  */


/* For each tty parameter structure that Emacs might want to save and restore,
   - include an element for it in this structure, and
   - extend the emacs_{get,set}_tty functions in sysdep.c to deal with the
     new members.  */

struct emacs_tty {

/* There is always one of the following elements, so there is no need
   for dummy get and set definitions.  */
#ifndef DOS_NT
  struct termios main;
#else /* DOS_NT */
  int main;
#endif /* DOS_NT */
};

/* From sysdep.c or w32.c  */
extern int serial_open (char *);
extern void serial_configure (struct Lisp_Process *, Lisp_Object);
