/* MS-DOS specific Lisp utilities interface.
   Coded by Manabu Higashida, 1991.
   Modified by Morten Welinder, 1993-1994.

Copyright (C) 1991, 1994-1995, 1997, 1999, 2001-2012
  Free Software Foundation, Inc.

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

#define DOS_COUNTRY_INFO 34	/* no of bytes returned by dos int 38h */
extern unsigned char dos_country_info[DOS_COUNTRY_INFO];

#ifndef HAVE_X_WINDOWS
extern int         msdos_stdcolor_idx  (const char *);
extern Lisp_Object msdos_stdcolor_name (int);
extern void        x_set_title (struct frame *, Lisp_Object);
#endif

