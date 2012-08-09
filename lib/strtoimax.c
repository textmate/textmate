/* Convert string representation of a number into an intmax_t value.

   Copyright (C) 1999, 2001-2004, 2006, 2009-2011 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* Written by Paul Eggert. */

#include <config.h>

/* Verify interface.  */
#include <inttypes.h>

#include <stdlib.h>

#include "verify.h"

#ifdef UNSIGNED
# ifndef HAVE_DECL_STRTOULL
"this configure-time declaration test was not run"
# endif
# if !HAVE_DECL_STRTOULL && HAVE_UNSIGNED_LONG_LONG_INT
unsigned long long int strtoull (char const *, char **, int);
# endif

#else

# ifndef HAVE_DECL_STRTOLL
"this configure-time declaration test was not run"
# endif
# if !HAVE_DECL_STRTOLL && HAVE_LONG_LONG_INT
long long int strtoll (char const *, char **, int);
# endif
#endif

#ifdef UNSIGNED
# define Have_long_long HAVE_UNSIGNED_LONG_LONG_INT
# define Int uintmax_t
# define Unsigned unsigned
# define strtoimax strtoumax
# define strtol strtoul
# define strtoll strtoull
#else
# define Have_long_long HAVE_LONG_LONG_INT
# define Int intmax_t
# define Unsigned
#endif

Int
strtoimax (char const *ptr, char **endptr, int base)
{
#if Have_long_long
  verify (sizeof (Int) == sizeof (Unsigned long int)
          || sizeof (Int) == sizeof (Unsigned long long int));

  if (sizeof (Int) != sizeof (Unsigned long int))
    return strtoll (ptr, endptr, base);
#else
  verify (sizeof (Int) == sizeof (Unsigned long int));
#endif

  return strtol (ptr, endptr, base);
}
