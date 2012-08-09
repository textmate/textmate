/* floating point to accurate string

   Copyright (C) 2010-2011 Free Software Foundation, Inc.

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

/* Written by Paul Eggert.  */

#ifndef _GL_FTOASTR_H

#include "intprops.h"
#include <float.h>
#include <stddef.h>

/* Store into BUF (of size BUFSIZE) an accurate minimal-precision
   string representation of a floating point number.  FLAGS affect the
   formatting of the number.  Pad the output string with spaces as
   necessary to width WIDTH bytes, in the style of printf.  WIDTH must
   be nonnegative.  X is the floating-point number to be converted.

   Return the number of bytes stored into BUF, not counting the
   terminating null.  However, do not overrun BUF: if BUF is too
   small, return a fairly tight (but not necessarily exact) upper
   bound on the value that would have been returned if BUF had been
   big enough.  If SIZE is zero, BUF may be a null pointer.  On error
   (e.g., returned value would exceed INT_MAX), return -1 and set
   errno.

   Example:

     char buf[DBL_BUFSIZE_BOUND];
     int r = dtoastr (buf, sizeof buf, 0, 0, 0.1);

   In the C locale, this sets R to 3 and stores "0.1" into BUF.  */

int  ftoastr (char *buf, size_t bufsize, int flags, int width,       float x);
int  dtoastr (char *buf, size_t bufsize, int flags, int width,      double x);
int ldtoastr (char *buf, size_t bufsize, int flags, int width, long double x);

/* Flag values for ftoastr etc.  These can be ORed together.  */
enum
  {
    /* Left justify within the width; the default is right justification.  */
    FTOASTR_LEFT_JUSTIFY = 1,

    /* Output "+" before positive numbers; the default outputs nothing.  */
    FTOASTR_ALWAYS_SIGNED = 2,

    /* Output " " before positive numbers; ignored if
       FTOASTER_ALWAYS_SIGNED is also given.  */
    FTOASTR_SPACE_POSITIVE = 4,

    /* Pad with zeros instead of spaces; ignored if FTOASTR_LEFT_JUSTIFY
       is also given.  */
    FTOASTR_ZERO_PAD = 8,

    /* Use 'E' instead of 'e' before the exponent.  */
    FTOASTR_UPPER_E = 16
  };


/* _GL_FLT_PREC_BOUND is an upper bound on the precision needed to
   represent a float value without losing information.  Likewise for
   _GL_DBL_PREC_BOUND and double, and _GL_LDBL_PREC_BOUND and long double.  */

#if FLT_RADIX == 10 /* decimal floating point */
 enum {  _GL_FLT_PREC_BOUND =  FLT_MANT_DIG };
 enum {  _GL_DBL_PREC_BOUND =  DBL_MANT_DIG };
 enum { _GL_LDBL_PREC_BOUND = LDBL_MANT_DIG };
#else

/* An upper bound on the number of bits needed to represent a single
   digit in a floating-point fraction.  */
# if FLT_RADIX == 2 /* IEEE 754 floating point, VAX floating point, etc. */
#  define _GL_FLOAT_DIG_BITS_BOUND 1
# elif FLT_RADIX <= 16 /* IBM hex floating point has FLT_RADIX == 16.  */
#  define _GL_FLOAT_DIG_BITS_BOUND 4
# else /* no machine is this bad, but let's be complete */
#  define _GL_FLOAT_DIG_BITS_BOUND (CHAR_BIT * (int) sizeof (int) - 1)
# endif

/* An upper bound on the number of decimal digits needed to represent
   a floating point number accurately, assuming a fraction contains
   DIG digits.  For why the "+ 1" is needed, see "Binary to Decimal
   Conversion" in David Goldberg's paper "What Every Computer
   Scientist Should Know About Floating-Point Arithmetic"
   <http://docs.sun.com/source/806-3568/ncg_goldberg.html>.  */
# define _GL_FLOAT_PREC_BOUND(dig) \
   (INT_BITS_STRLEN_BOUND ((dig) * _GL_FLOAT_DIG_BITS_BOUND) + 1)

 enum {  _GL_FLT_PREC_BOUND = _GL_FLOAT_PREC_BOUND ( FLT_MANT_DIG) };
 enum {  _GL_DBL_PREC_BOUND = _GL_FLOAT_PREC_BOUND ( DBL_MANT_DIG) };
 enum { _GL_LDBL_PREC_BOUND = _GL_FLOAT_PREC_BOUND (LDBL_MANT_DIG) };
#endif


/* Bound on the number of bytes printed for an exponent in the range
   MIN..MAX, where MIN < 0 < MAX; printf always prints a sign and at
   least 2 digits.  Although the maximum known exponent is 4932 for
   IEEE 754 binary128, support tight bounds for exponents up to a
   million, just in case.  */
#define _GL_FLOAT_EXPONENT_STRLEN_BOUND(min, max)  \
  (      -100 < (min) && (max) <     100 ? 3       \
   :    -1000 < (min) && (max) <    1000 ? 4       \
   :   -10000 < (min) && (max) <   10000 ? 5       \
   :  -100000 < (min) && (max) <  100000 ? 6       \
   : -1000000 < (min) && (max) < 1000000 ? 7       \
   : INT_STRLEN_BOUND (int) /* not a tight bound */)

/* A reasonably tight bound on the length of a type-T floating value
   formatted with ftoastr etc.  Room is needed for sign, fraction
   digits, decimal point, "e", and exponent.  POINTLEN should be a
   reasonably tight bound on the string length of the decimal
   point.  */
#define _GL_FLOAT_STRLEN_BOUND_L(t, pointlen)                          \
  (1 + _GL_##t##_PREC_BOUND + pointlen + 1                             \
   + _GL_FLOAT_EXPONENT_STRLEN_BOUND (t##_MIN_10_EXP, t##_MAX_10_EXP))
#define  FLT_STRLEN_BOUND_L(pointlen) _GL_FLOAT_STRLEN_BOUND_L ( FLT, pointlen)
#define  DBL_STRLEN_BOUND_L(pointlen) _GL_FLOAT_STRLEN_BOUND_L ( DBL, pointlen)
#define LDBL_STRLEN_BOUND_L(pointlen) _GL_FLOAT_STRLEN_BOUND_L (LDBL, pointlen)

/* Looser bounds that are locale-independent and are integral constant
   expressions.  */
#define  FLT_STRLEN_BOUND  FLT_STRLEN_BOUND_L (MB_LEN_MAX)
#define  DBL_STRLEN_BOUND  DBL_STRLEN_BOUND_L (MB_LEN_MAX)
#define LDBL_STRLEN_BOUND LDBL_STRLEN_BOUND_L (MB_LEN_MAX)

/* Looser, locale-independent bounds that include the trailing null byte.  */
#define  FLT_BUFSIZE_BOUND ( FLT_STRLEN_BOUND + 1)
#define  DBL_BUFSIZE_BOUND ( DBL_STRLEN_BOUND + 1)
#define LDBL_BUFSIZE_BOUND (LDBL_STRLEN_BOUND + 1)

#endif /* _GL_FTOASTR_H */
