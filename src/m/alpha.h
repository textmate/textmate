/* Machine description file for the alpha chip.

Copyright (C) 1994, 1997, 1999, 2001-2012  Free Software Foundation, Inc.

Author: Rainer Schoepf
(according to authors.el)

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

#ifndef _LP64
#define _LP64 /* This doesn't appear to be necessary on OSF 4/5  -- fx.  */
#endif

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */
/* __alpha defined automatically */


#ifdef __ELF__

#if !defined (GNU_LINUX) && !defined (__NetBSD__)
#define DATA_START    0x140000000
#endif

#else  /* not __ELF__ */

/* Describe layout of the address space in an executing process.  */
#define DATA_START    0x140000000

#endif /* __ELF__ */

/* On the Alpha it's best to avoid including TERMIO since struct
   termio and struct termios are mutually incompatible.  */
#define NO_TERMIO

/* Many Alpha implementations (e.g. gas 2.8) can't handle DBL_MIN:
   they generate code that uses a signaling NaN instead of DBL_MIN.
   Define DBL_MIN_REPLACEMENT to be the next value larger than DBL_MIN:
   this avoids the assembler bug.  */
#define DBL_MIN_REPLACEMENT 2.2250738585072019e-308
