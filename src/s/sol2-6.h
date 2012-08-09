/* Definitions file for GNU Emacs running on Solaris 2.6.

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

#include "usg5-4-common.h"

#define SOLARIS2

/* This triggers a conditional in xfaces.c.  */
#define XOS_NEEDS_TIME_H

#define POSIX

/* Prefer kstat over kvm in getloadavg.c, kstat doesn't require root.
   ghazi@caip.rutgers.edu, 7/21/97.  Don't redefine if already defined
   (e.g., by config.h). */
#ifndef HAVE_LIBKSTAT
#define HAVE_LIBKSTAT
#endif

/* This is the same definition as in usg5-4-common.h, but with sigblock/sigunblock
   rather than sighold/sigrelse, which appear to be BSD4.1 specific.
   It may also be appropriate for SVR4.x
   (x<2) but I'm not sure.   fnf@cygnus.com */
/* This sets the name of the slave side of the PTY.  On SysVr4,
   grantpt(3) forks a subprocess, so keep sigchld_handler() from
   intercepting that death.  If any child but grantpt's should die
   within, it should be caught after sigrelse(2). */

#define PTY_TTY_NAME_SPRINTF			\
  {						\
    char *ptsname (int), *ptyname;		\
						\
    sigblock (sigmask (SIGCLD));		\
    if (grantpt (fd) == -1)			\
      { emacs_close (fd); return -1; }		\
    sigunblock (sigmask (SIGCLD));		\
    if (unlockpt (fd) == -1)			\
      { emacs_close (fd); return -1; }		\
    if (!(ptyname = ptsname (fd)))		\
      { emacs_close (fd); return -1; }		\
    strncpy (pty_name, ptyname, sizeof (pty_name)); \
    pty_name[sizeof (pty_name) - 1] = 0;	\
  }

#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS
