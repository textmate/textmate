/* machine description file for the IA-64 architecture.

Copyright (C) 2000-2012  Free Software Foundation, Inc.

     Contributed by David Mosberger <davidm@hpl.hp.com>

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

#define BITS_PER_LONG		64
#define BITS_PER_EMACS_INT	64

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */
/* __ia64__ defined automatically */

/* Define the type to use.  */
#define EMACS_INT		long
#define pI			"l"
#define EMACS_UINT		unsigned long

#ifdef REL_ALLOC
#ifndef _MALLOC_INTERNAL
/* "char *" because ralloc.c defines it that way.  gmalloc.c thinks it
   is allowed to prototype these as "void *" so we don't prototype in
   that case.  You're right: it stinks!  */
extern char *r_alloc (), *r_re_alloc ();
extern void r_alloc_free ();
#endif /* not _MALLOC_INTERNAL */
#endif /* REL_ALLOC */
