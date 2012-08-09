/* Includes for memory limit warnings.
   Copyright (C) 1990, 1993-1996, 2001-2012  Free Software Foundation, Inc.

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

#ifdef MSDOS
#include <dpmi.h>
extern int etext;
#endif

/* Some systems need this before <sys/resource.h>.  */
#include <sys/types.h>

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/time.h>
# include <sys/resource.h>
#else
# if HAVE_SYS_VLIMIT_H
#  include <sys/vlimit.h>	/* Obsolete, says glibc */
# endif
#endif

extern char *start_of_data (void);
#if defined USE_LSB_TAG || UINTPTR_MAX >> VALBITS == 0
#define EXCEEDS_LISP_PTR(ptr) 0
#elif defined DATA_SEG_BITS
#define EXCEEDS_LISP_PTR(ptr) \
  (((uintptr_t) (ptr) & ~DATA_SEG_BITS) >> VALBITS)
#else
#define EXCEEDS_LISP_PTR(ptr) ((uintptr_t) (ptr) >> VALBITS)
#endif
