/* sysselect.h - System-dependent definitions for the select function.
   Copyright (C) 1995, 2001-2012  Free Software Foundation, Inc.

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

#ifdef HAVE_SYS_SELECT_H
#if defined (DARWIN_OS)
#undef init_process
#endif
#include <sys/select.h>
#if defined (DARWIN_OS)
#define init_process emacs_init_process
#endif
#endif

/* The w32 build defines select stuff in w32.h, which is included
   where w32 needs it, but not where sysselect.h is included.  The w32
   definitions in w32.h are incompatible with the below.  */
#ifndef WINDOWSNT
#ifdef FD_SET
#ifdef FD_SETSIZE
#define MAXDESC FD_SETSIZE
#else
#define MAXDESC 64
#endif
#define SELECT_TYPE fd_set
#else /* no FD_SET */
#define MAXDESC 32
#define SELECT_TYPE int

/* Define the macros to access a single-int bitmap of descriptors.  */
#define FD_SET(n, p) (*(p) |= (1 << (n)))
#define FD_CLR(n, p) (*(p) &= ~(1 << (n)))
#define FD_ISSET(n, p) (*(p) & (1 << (n)))
#define FD_ZERO(p) (*(p) = 0)
#endif /* no FD_SET */
#endif /* not WINDOWSNT */

#if !defined (HAVE_SELECT)
#define select sys_select
#endif

