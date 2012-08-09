/* Substitute for and wrapper around <stdarg.h>.
   Copyright (C) 2008-2011 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#ifndef _@GUARD_PREFIX@_STDARG_H

#if __GNUC__ >= 3
@PRAGMA_SYSTEM_HEADER@
#endif
@PRAGMA_COLUMNS@

/* The include_next requires a split double-inclusion guard.  */
#@INCLUDE_NEXT@ @NEXT_STDARG_H@

#ifndef _@GUARD_PREFIX@_STDARG_H
#define _@GUARD_PREFIX@_STDARG_H

#ifndef va_copy
# define va_copy(a,b) ((a) = (b))
#endif

#endif /* _@GUARD_PREFIX@_STDARG_H */
#endif /* _@GUARD_PREFIX@_STDARG_H */
