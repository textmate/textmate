/* Reentrant time functions like localtime_r.

   Copyright (C) 2003, 2006-2007, 2010-2011 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

/* Written by Paul Eggert.  */

#include <config.h>

#include <time.h>

static struct tm *
copy_tm_result (struct tm *dest, struct tm const *src)
{
  if (! src)
    return 0;
  *dest = *src;
  return dest;
}


struct tm *
gmtime_r (time_t const * restrict t, struct tm * restrict tp)
{
  return copy_tm_result (tp, gmtime (t));
}

struct tm *
localtime_r (time_t const * restrict t, struct tm * restrict tp)
{
  return copy_tm_result (tp, localtime (t));
}
