/* machine description file for Sun 4 SPARC.

Copyright (C) 1987, 2001-2012  Free Software Foundation, Inc.

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

/* __sparc__ is defined by the compiler by default.  */

#ifdef __arch64__		/* GCC, 64-bit ABI.  */

#define BITS_PER_LONG 64

#ifndef _LP64
#define _LP64 /* Done on Alpha -- not sure if it should be here.  -- fx */
#endif

#endif  /* __arch64__ */
