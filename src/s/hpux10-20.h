/* System description file for hpux version 10.20.

Copyright (C) 1999, 2001-2012  Free Software Foundation, Inc.

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


#define RUN_TIME_REMAP

/* Define symbols to identify the version of Unix this is.
   Define all the symbols that apply correctly.  */
#define USG				/* System III, System V, etc */
#define USG5
#define HPUX

/* SYSTEM_TYPE should indicate the kind of system you are using.
   It sets the Lisp variable system-type.  */
#define SYSTEM_TYPE "hpux"

/* Letter to use in finding device name of first pty,
   if system supports pty's.  'p' means it is /dev/ptym/ptyp0  */
#define FIRST_PTY_LETTER 'p'

#define NO_TERMIO

/* Define HAVE_PTYS if the system supports pty devices.  */
#define HAVE_PTYS

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */
#define HAVE_SOCKETS

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */
#define CLASH_DETECTION

/* Special hacks needed to make Emacs run on this system.  */

/* In hpux, the symbol SIGIO is defined, but the feature
   doesn't work in the way Emacs needs it to.  */
#define BROKEN_SIGIO

/* Some additional system facilities exist.  */
#define HAVE_PERROR  /* Delete this line for version 6.  */

/* This is how to get the device name of the tty end of a pty.  */
#define PTY_TTY_NAME_SPRINTF \
            sprintf (pty_name, "/dev/pty/tty%c%x", c, i);

/* This is how to get the device name of the control end of a pty.  */
#define PTY_NAME_SPRINTF \
	sprintf (pty_name, "/dev/ptym/pty%c%x", c, i);

/* This triggers a conditional in xfaces.c.  */
#define XOS_NEEDS_TIME_H

/* Assar Westerlund <assar@sics.se> says this is necessary for
   HP-UX 10.20, and that it works for HP-UX 0 as well.  */
#define NO_EDITRES

/* Eric Backus <ericb@lsid.hp.com> says, HP-UX 9.x on HP 700 machines
   has a broken `rint' in some library versions including math library
   version number A.09.05.

   You can fix the math library by installing patch number PHSS_4630.
   But we can fix it more reliably for Emacs like this.  */
#undef HAVE_RINT

/* We have to go this route, rather than hpux9's approach of renaming the
   functions via macros.  The system's stdlib.h has fully prototyped
   declarations, which yields a conflicting definition of srand48; it
   tries to redeclare what was once srandom to be srand48.  So we go
   with HAVE_LRAND48 being defined.  */
#undef srandom
#undef random
#undef HAVE_RANDOM


/* Rainer Malzbender <rainer@displaytech.com> says defining
   HAVE_XRMSETDATABASE allows Emacs to compile on HP-UX 10.20 using GCC.  */
#ifndef HAVE_XRMSETDATABASE
#define HAVE_XRMSETDATABASE
#endif

/* 2000-11-21: Temporarily disable Unix 98 large file support found by
   configure.  It fails on HPUX 11, at least, because it enables
   header sections which lose when `static' is defined away, as it is
   on HP-UX.  (You get duplicate symbol errors on linking). */
#undef _FILE_OFFSET_BITS

/* The data segment on this machine always starts at address 0x40000000.  */
#define DATA_SEG_BITS 0x40000000

#define DATA_START    0x40000000
