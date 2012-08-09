/* Definitions file for GNU Emacs running on the GNU Hurd.

Copyright (C) 1994-1996, 2001-2012  Free Software Foundation, Inc.

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


/* Get most of the stuff from bsd-common */
#include "bsd-common.h"

#undef SYSTEM_TYPE
#define SYSTEM_TYPE "gnu"

#undef NLIST_STRUCT

#define SIGNALS_VIA_CHARACTERS

/* libc defines data_start.  */
#define DATA_START ({ extern int data_start; (char *) &data_start; })

/* Some losing code fails to include this and then assumes
   that because it is braindead that O_RDONLY==0.  */
#include <fcntl.h>

#ifdef emacs
#include <stdio.h>  /* Get the definition of _IO_STDIO_H.  */
#if defined (_IO_STDIO_H) || defined (_STDIO_USES_IOSTREAM)
/* new C libio names */
#define GNU_LIBRARY_PENDING_OUTPUT_COUNT(FILE) \
  ((FILE)->_IO_write_ptr - (FILE)->_IO_write_base)
#endif /* !_IO_STDIO_H */
#endif /* emacs */

#define POSIX_SIGNALS 1

/* Use the GC_MAKE_GCPROS_NOOPS (see lisp.h) method for marking the stack.  */
#define GC_MARK_STACK 	GC_MAKE_GCPROS_NOOPS
