/* ignore a function return without a compiler warning

   Copyright (C) 2008-2011 Free Software Foundation, Inc.

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

/* Written by Jim Meyering, Eric Blake and Pádraig Brady.  */

/* Use "ignore_value" to avoid a warning when using a function declared with
   gcc's warn_unused_result attribute, but for which you really do want to
   ignore the result.  Traditionally, people have used a "(void)" cast to
   indicate that a function's return value is deliberately unused.  However,
   if the function is declared with __attribute__((warn_unused_result)),
   gcc issues a warning even with the cast.

   Caution: most of the time, you really should heed gcc's warning, and
   check the return value.  However, in those exceptional cases in which
   you're sure you know what you're doing, use this function.

   For the record, here's one of the ignorable warnings:
   "copy.c:233: warning: ignoring return value of 'fchown',
   declared with attribute warn_unused_result".  */

#ifndef _GL_IGNORE_VALUE_H
# define _GL_IGNORE_VALUE_H

# ifndef _GL_ATTRIBUTE_DEPRECATED
/* The __attribute__((__deprecated__)) feature
   is available in gcc versions 3.1 and newer.  */
#  if __GNUC__ < 3 || (__GNUC__ == 3 && __GNUC_MINOR__ < 1)
#   define _GL_ATTRIBUTE_DEPRECATED /* empty */
#  else
#   define _GL_ATTRIBUTE_DEPRECATED __attribute__ ((__deprecated__))
#  endif
# endif

/* The __attribute__((__warn_unused_result__)) feature
   is available in gcc versions 3.4 and newer,
   while the typeof feature has been available since 2.7 at least.  */
# if __GNUC__ < 3 || (__GNUC__ == 3 && __GNUC_MINOR__ < 4)
#  define ignore_value(x) ((void) (x))
# else
#  define ignore_value(x) (({ __typeof__ (x) __x = (x); (void) __x; }))
# endif

/* ignore_value works for scalars, pointers and aggregates;
   deprecate ignore_ptr.  */
static inline void _GL_ATTRIBUTE_DEPRECATED
ignore_ptr (void *p) { (void) p; } /* deprecated: use ignore_value */

#endif
