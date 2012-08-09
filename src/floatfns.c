/* Primitive operations on floating point for GNU Emacs Lisp interpreter.

Copyright (C) 1988, 1993-1994, 1999, 2001-2012
  Free Software Foundation, Inc.

Author: Wolfgang Rupprecht
(according to ack.texi)

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


/* ANSI C requires only these float functions:
   acos, asin, atan, atan2, ceil, cos, cosh, exp, fabs, floor, fmod,
   frexp, ldexp, log, log10, modf, pow, sin, sinh, sqrt, tan, tanh.

   Define HAVE_INVERSE_HYPERBOLIC if you have acosh, asinh, and atanh.
   Define HAVE_CBRT if you have cbrt.
   Define HAVE_RINT if you have a working rint.
   If you don't define these, then the appropriate routines will be simulated.

   Define HAVE_MATHERR if on a system supporting the SysV matherr callback.
   (This should happen automatically.)

   Define FLOAT_CHECK_ERRNO if the float library routines set errno.
   This has no effect if HAVE_MATHERR is defined.

   Define FLOAT_CATCH_SIGILL if the float library routines signal SIGILL.
   (What systems actually do this?  Please let us know.)

   Define FLOAT_CHECK_DOMAIN if the float library doesn't handle errors by
   either setting errno, or signaling SIGFPE/SIGILL.  Otherwise, domain and
   range checking will happen before calling the float routines.  This has
   no effect if HAVE_MATHERR is defined (since matherr will be called when
   a domain error occurs.)
 */

#include <config.h>
#include <signal.h>
#include <setjmp.h>
#include "lisp.h"
#include "syssignal.h"

#include <float.h>
/* If IEEE_FLOATING_POINT isn't defined, default it from FLT_*. */
#ifndef IEEE_FLOATING_POINT
#if (FLT_RADIX == 2 && FLT_MANT_DIG == 24 \
     && FLT_MIN_EXP == -125 && FLT_MAX_EXP == 128)
#define IEEE_FLOATING_POINT 1
#else
#define IEEE_FLOATING_POINT 0
#endif
#endif

#include <math.h>

/* This declaration is omitted on some systems, like Ultrix.  */
#if !defined (HPUX) && defined (HAVE_LOGB) && !defined (logb)
extern double logb (double);
#endif /* not HPUX and HAVE_LOGB and no logb macro */

#if defined (DOMAIN) && defined (SING) && defined (OVERFLOW)
    /* If those are defined, then this is probably a `matherr' machine. */
# ifndef HAVE_MATHERR
#  define HAVE_MATHERR
# endif
#endif

#ifdef NO_MATHERR
#undef HAVE_MATHERR
#endif

#ifdef HAVE_MATHERR
# ifdef FLOAT_CHECK_ERRNO
#  undef FLOAT_CHECK_ERRNO
# endif
# ifdef FLOAT_CHECK_DOMAIN
#  undef FLOAT_CHECK_DOMAIN
# endif
#endif

#ifndef NO_FLOAT_CHECK_ERRNO
#define FLOAT_CHECK_ERRNO
#endif

#ifdef FLOAT_CHECK_ERRNO
# include <errno.h>
#endif

#ifdef FLOAT_CATCH_SIGILL
static void float_error ();
#endif

/* Nonzero while executing in floating point.
   This tells float_error what to do.  */

static int in_float;

/* If an argument is out of range for a mathematical function,
   here is the actual argument value to use in the error message.
   These variables are used only across the floating point library call
   so there is no need to staticpro them.  */

static Lisp_Object float_error_arg, float_error_arg2;

static const char *float_error_fn_name;

/* Evaluate the floating point expression D, recording NUM
   as the original argument for error messages.
   D is normally an assignment expression.
   Handle errors which may result in signals or may set errno.

   Note that float_error may be declared to return void, so you can't
   just cast the zero after the colon to (void) to make the types
   check properly.  */

#ifdef FLOAT_CHECK_ERRNO
#define IN_FLOAT(d, name, num)				\
  do {							\
    float_error_arg = num;				\
    float_error_fn_name = name;				\
    in_float = 1; errno = 0; (d); in_float = 0;		\
    switch (errno) {					\
    case 0: break;					\
    case EDOM:	 domain_error (float_error_fn_name, float_error_arg);	\
    case ERANGE: range_error (float_error_fn_name, float_error_arg);	\
    default:	 arith_error (float_error_fn_name, float_error_arg);	\
    }							\
  } while (0)
#define IN_FLOAT2(d, name, num, num2)			\
  do {							\
    float_error_arg = num;				\
    float_error_arg2 = num2;				\
    float_error_fn_name = name;				\
    in_float = 1; errno = 0; (d); in_float = 0;		\
    switch (errno) {					\
    case 0: break;					\
    case EDOM:	 domain_error (float_error_fn_name, float_error_arg);	\
    case ERANGE: range_error (float_error_fn_name, float_error_arg);	\
    default:	 arith_error (float_error_fn_name, float_error_arg);	\
    }							\
  } while (0)
#else
#define IN_FLOAT(d, name, num) (in_float = 1, (d), in_float = 0)
#define IN_FLOAT2(d, name, num, num2) (in_float = 1, (d), in_float = 0)
#endif

/* Convert float to Lisp_Int if it fits, else signal a range error
   using the given arguments.  */
#define FLOAT_TO_INT(x, i, name, num)					\
  do									\
    {									\
      if (FIXNUM_OVERFLOW_P (x))					\
	range_error (name, num);					\
      XSETINT (i,  (EMACS_INT)(x));					\
    }									\
  while (0)
#define FLOAT_TO_INT2(x, i, name, num1, num2)				\
  do									\
    {									\
      if (FIXNUM_OVERFLOW_P (x))					\
	range_error2 (name, num1, num2);				\
      XSETINT (i,  (EMACS_INT)(x));					\
    }									\
  while (0)

#define arith_error(op,arg) \
  xsignal2 (Qarith_error, build_string ((op)), (arg))
#define range_error(op,arg) \
  xsignal2 (Qrange_error, build_string ((op)), (arg))
#define range_error2(op,a1,a2) \
  xsignal3 (Qrange_error, build_string ((op)), (a1), (a2))
#define domain_error(op,arg) \
  xsignal2 (Qdomain_error, build_string ((op)), (arg))
#ifdef FLOAT_CHECK_DOMAIN
#define domain_error2(op,a1,a2) \
  xsignal3 (Qdomain_error, build_string ((op)), (a1), (a2))
#endif

/* Extract a Lisp number as a `double', or signal an error.  */

double
extract_float (Lisp_Object num)
{
  CHECK_NUMBER_OR_FLOAT (num);

  if (FLOATP (num))
    return XFLOAT_DATA (num);
  return (double) XINT (num);
}

/* Trig functions.  */

DEFUN ("acos", Facos, Sacos, 1, 1, 0,
       doc: /* Return the inverse cosine of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 1.0 || d < -1.0)
    domain_error ("acos", arg);
#endif
  IN_FLOAT (d = acos (d), "acos", arg);
  return make_float (d);
}

DEFUN ("asin", Fasin, Sasin, 1, 1, 0,
       doc: /* Return the inverse sine of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 1.0 || d < -1.0)
    domain_error ("asin", arg);
#endif
  IN_FLOAT (d = asin (d), "asin", arg);
  return make_float (d);
}

DEFUN ("atan", Fatan, Satan, 1, 2, 0,
       doc: /* Return the inverse tangent of the arguments.
If only one argument Y is given, return the inverse tangent of Y.
If two arguments Y and X are given, return the inverse tangent of Y
divided by X, i.e. the angle in radians between the vector (X, Y)
and the x-axis.  */)
  (register Lisp_Object y, Lisp_Object x)
{
  double d = extract_float (y);

  if (NILP (x))
    IN_FLOAT (d = atan (d), "atan", y);
  else
    {
      double d2 = extract_float (x);

      IN_FLOAT2 (d = atan2 (d, d2), "atan", y, x);
    }
  return make_float (d);
}

DEFUN ("cos", Fcos, Scos, 1, 1, 0,
       doc: /* Return the cosine of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = cos (d), "cos", arg);
  return make_float (d);
}

DEFUN ("sin", Fsin, Ssin, 1, 1, 0,
       doc: /* Return the sine of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = sin (d), "sin", arg);
  return make_float (d);
}

DEFUN ("tan", Ftan, Stan, 1, 1, 0,
       doc: /* Return the tangent of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  double c = cos (d);
#ifdef FLOAT_CHECK_DOMAIN
  if (c == 0.0)
    domain_error ("tan", arg);
#endif
  IN_FLOAT (d = sin (d) / c, "tan", arg);
  return make_float (d);
}

#undef isnan
#define isnan(x) ((x) != (x))

DEFUN ("isnan", Fisnan, Sisnan, 1, 1, 0,
       doc: /* Return non nil iff argument X is a NaN.  */)
  (Lisp_Object x)
{
  CHECK_FLOAT (x);
  return isnan (XFLOAT_DATA (x)) ? Qt : Qnil;
}

#ifdef HAVE_COPYSIGN
DEFUN ("copysign", Fcopysign, Scopysign, 2, 2, 0,
       doc: /* Copy sign of X2 to value of X1, and return the result.
Cause an error if X1 or X2 is not a float.  */)
  (Lisp_Object x1, Lisp_Object x2)
{
  double f1, f2;

  CHECK_FLOAT (x1);
  CHECK_FLOAT (x2);

  f1 = XFLOAT_DATA (x1);
  f2 = XFLOAT_DATA (x2);

  return make_float (copysign (f1, f2));
}

DEFUN ("frexp", Ffrexp, Sfrexp, 1, 1, 0,
       doc: /* Get significand and exponent of a floating point number.
Breaks the floating point number X into its binary significand SGNFCAND
\(a floating point value between 0.5 (included) and 1.0 (excluded))
and an integral exponent EXP for 2, such that:

  X = SGNFCAND * 2^EXP

The function returns the cons cell (SGNFCAND . EXP).
If X is zero, both parts (SGNFCAND and EXP) are zero.  */)
  (Lisp_Object x)
{
  double f = XFLOATINT (x);

  if (f == 0.0)
    return Fcons (make_float (0.0), make_number (0));
  else
    {
      int exponent;
      double sgnfcand = frexp (f, &exponent);
      return Fcons (make_float (sgnfcand), make_number (exponent));
    }
}

DEFUN ("ldexp", Fldexp, Sldexp, 1, 2, 0,
       doc: /* Construct number X from significand SGNFCAND and exponent EXP.
Returns the floating point value resulting from multiplying SGNFCAND
(the significand) by 2 raised to the power of EXP (the exponent).   */)
  (Lisp_Object sgnfcand, Lisp_Object exponent)
{
  CHECK_NUMBER (exponent);
  return make_float (ldexp (XFLOATINT (sgnfcand), XINT (exponent)));
}
#endif

#if 0 /* Leave these out unless we find there's a reason for them.  */

DEFUN ("bessel-j0", Fbessel_j0, Sbessel_j0, 1, 1, 0,
       doc: /* Return the bessel function j0 of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = j0 (d), "bessel-j0", arg);
  return make_float (d);
}

DEFUN ("bessel-j1", Fbessel_j1, Sbessel_j1, 1, 1, 0,
       doc: /* Return the bessel function j1 of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = j1 (d), "bessel-j1", arg);
  return make_float (d);
}

DEFUN ("bessel-jn", Fbessel_jn, Sbessel_jn, 2, 2, 0,
       doc: /* Return the order N bessel function output jn of ARG.
The first arg (the order) is truncated to an integer.  */)
  (register Lisp_Object n, Lisp_Object arg)
{
  int i1 = extract_float (n);
  double f2 = extract_float (arg);

  IN_FLOAT (f2 = jn (i1, f2), "bessel-jn", n);
  return make_float (f2);
}

DEFUN ("bessel-y0", Fbessel_y0, Sbessel_y0, 1, 1, 0,
       doc: /* Return the bessel function y0 of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = y0 (d), "bessel-y0", arg);
  return make_float (d);
}

DEFUN ("bessel-y1", Fbessel_y1, Sbessel_y1, 1, 1, 0,
       doc: /* Return the bessel function y1 of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = y1 (d), "bessel-y0", arg);
  return make_float (d);
}

DEFUN ("bessel-yn", Fbessel_yn, Sbessel_yn, 2, 2, 0,
       doc: /* Return the order N bessel function output yn of ARG.
The first arg (the order) is truncated to an integer.  */)
  (register Lisp_Object n, Lisp_Object arg)
{
  int i1 = extract_float (n);
  double f2 = extract_float (arg);

  IN_FLOAT (f2 = yn (i1, f2), "bessel-yn", n);
  return make_float (f2);
}

#endif

#if 0 /* Leave these out unless we see they are worth having.  */

DEFUN ("erf", Ferf, Serf, 1, 1, 0,
       doc: /* Return the mathematical error function of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = erf (d), "erf", arg);
  return make_float (d);
}

DEFUN ("erfc", Ferfc, Serfc, 1, 1, 0,
       doc: /* Return the complementary error function of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = erfc (d), "erfc", arg);
  return make_float (d);
}

DEFUN ("log-gamma", Flog_gamma, Slog_gamma, 1, 1, 0,
       doc: /* Return the log gamma of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = lgamma (d), "log-gamma", arg);
  return make_float (d);
}

DEFUN ("cube-root", Fcube_root, Scube_root, 1, 1, 0,
       doc: /* Return the cube root of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
#ifdef HAVE_CBRT
  IN_FLOAT (d = cbrt (d), "cube-root", arg);
#else
  if (d >= 0.0)
    IN_FLOAT (d = pow (d, 1.0/3.0), "cube-root", arg);
  else
    IN_FLOAT (d = -pow (-d, 1.0/3.0), "cube-root", arg);
#endif
  return make_float (d);
}

#endif

DEFUN ("exp", Fexp, Sexp, 1, 1, 0,
       doc: /* Return the exponential base e of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 709.7827)   /* Assume IEEE doubles here */
    range_error ("exp", arg);
  else if (d < -709.0)
    return make_float (0.0);
  else
#endif
    IN_FLOAT (d = exp (d), "exp", arg);
  return make_float (d);
}

DEFUN ("expt", Fexpt, Sexpt, 2, 2, 0,
       doc: /* Return the exponential ARG1 ** ARG2.  */)
  (register Lisp_Object arg1, Lisp_Object arg2)
{
  double f1, f2, f3;

  CHECK_NUMBER_OR_FLOAT (arg1);
  CHECK_NUMBER_OR_FLOAT (arg2);
  if (INTEGERP (arg1)     /* common lisp spec */
      && INTEGERP (arg2)   /* don't promote, if both are ints, and */
      && 0 <= XINT (arg2)) /* we are sure the result is not fractional */
    {				/* this can be improved by pre-calculating */
      EMACS_INT acc, x, y;	/* some binary powers of x then accumulating */
      Lisp_Object val;

      x = XINT (arg1);
      y = XINT (arg2);
      acc = 1;

      if (y < 0)
	{
	  if (x == 1)
	    acc = 1;
	  else if (x == -1)
	    acc = (y & 1) ? -1 : 1;
	  else
	    acc = 0;
	}
      else
	{
	  while (y > 0)
	    {
	      if (y & 1)
		acc *= x;
	      x *= x;
	      y >>= 1;
	    }
	}
      XSETINT (val, acc);
      return val;
    }
  f1 = FLOATP (arg1) ? XFLOAT_DATA (arg1) : XINT (arg1);
  f2 = FLOATP (arg2) ? XFLOAT_DATA (arg2) : XINT (arg2);
  /* Really should check for overflow, too */
  if (f1 == 0.0 && f2 == 0.0)
    f1 = 1.0;
#ifdef FLOAT_CHECK_DOMAIN
  else if ((f1 == 0.0 && f2 < 0.0) || (f1 < 0 && f2 != floor (f2)))
    domain_error2 ("expt", arg1, arg2);
#endif
  IN_FLOAT2 (f3 = pow (f1, f2), "expt", arg1, arg2);
  /* Check for overflow in the result.  */
  if (f1 != 0.0 && f3 == 0.0)
    range_error ("expt", arg1);
  return make_float (f3);
}

DEFUN ("log", Flog, Slog, 1, 2, 0,
       doc: /* Return the natural logarithm of ARG.
If the optional argument BASE is given, return log ARG using that base.  */)
  (register Lisp_Object arg, Lisp_Object base)
{
  double d = extract_float (arg);

#ifdef FLOAT_CHECK_DOMAIN
  if (d <= 0.0)
    domain_error2 ("log", arg, base);
#endif
  if (NILP (base))
    IN_FLOAT (d = log (d), "log", arg);
  else
    {
      double b = extract_float (base);

#ifdef FLOAT_CHECK_DOMAIN
      if (b <= 0.0 || b == 1.0)
	domain_error2 ("log", arg, base);
#endif
      if (b == 10.0)
	IN_FLOAT2 (d = log10 (d), "log", arg, base);
      else
	IN_FLOAT2 (d = log (d) / log (b), "log", arg, base);
    }
  return make_float (d);
}

DEFUN ("log10", Flog10, Slog10, 1, 1, 0,
       doc: /* Return the logarithm base 10 of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d <= 0.0)
    domain_error ("log10", arg);
#endif
  IN_FLOAT (d = log10 (d), "log10", arg);
  return make_float (d);
}

DEFUN ("sqrt", Fsqrt, Ssqrt, 1, 1, 0,
       doc: /* Return the square root of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d < 0.0)
    domain_error ("sqrt", arg);
#endif
  IN_FLOAT (d = sqrt (d), "sqrt", arg);
  return make_float (d);
}

#if 0 /* Not clearly worth adding.  */

DEFUN ("acosh", Facosh, Sacosh, 1, 1, 0,
       doc: /* Return the inverse hyperbolic cosine of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d < 1.0)
    domain_error ("acosh", arg);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = acosh (d), "acosh", arg);
#else
  IN_FLOAT (d = log (d + sqrt (d*d - 1.0)), "acosh", arg);
#endif
  return make_float (d);
}

DEFUN ("asinh", Fasinh, Sasinh, 1, 1, 0,
       doc: /* Return the inverse hyperbolic sine of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = asinh (d), "asinh", arg);
#else
  IN_FLOAT (d = log (d + sqrt (d*d + 1.0)), "asinh", arg);
#endif
  return make_float (d);
}

DEFUN ("atanh", Fatanh, Satanh, 1, 1, 0,
       doc: /* Return the inverse hyperbolic tangent of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d >= 1.0 || d <= -1.0)
    domain_error ("atanh", arg);
#endif
#ifdef HAVE_INVERSE_HYPERBOLIC
  IN_FLOAT (d = atanh (d), "atanh", arg);
#else
  IN_FLOAT (d = 0.5 * log ((1.0 + d) / (1.0 - d)), "atanh", arg);
#endif
  return make_float (d);
}

DEFUN ("cosh", Fcosh, Scosh, 1, 1, 0,
       doc: /* Return the hyperbolic cosine of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 710.0 || d < -710.0)
    range_error ("cosh", arg);
#endif
  IN_FLOAT (d = cosh (d), "cosh", arg);
  return make_float (d);
}

DEFUN ("sinh", Fsinh, Ssinh, 1, 1, 0,
       doc: /* Return the hyperbolic sine of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
#ifdef FLOAT_CHECK_DOMAIN
  if (d > 710.0 || d < -710.0)
    range_error ("sinh", arg);
#endif
  IN_FLOAT (d = sinh (d), "sinh", arg);
  return make_float (d);
}

DEFUN ("tanh", Ftanh, Stanh, 1, 1, 0,
       doc: /* Return the hyperbolic tangent of ARG.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = tanh (d), "tanh", arg);
  return make_float (d);
}
#endif

DEFUN ("abs", Fabs, Sabs, 1, 1, 0,
       doc: /* Return the absolute value of ARG.  */)
  (register Lisp_Object arg)
{
  CHECK_NUMBER_OR_FLOAT (arg);

  if (FLOATP (arg))
    IN_FLOAT (arg = make_float (fabs (XFLOAT_DATA (arg))), "abs", arg);
  else if (XINT (arg) < 0)
    XSETINT (arg, - XINT (arg));

  return arg;
}

DEFUN ("float", Ffloat, Sfloat, 1, 1, 0,
       doc: /* Return the floating point number equal to ARG.  */)
  (register Lisp_Object arg)
{
  CHECK_NUMBER_OR_FLOAT (arg);

  if (INTEGERP (arg))
    return make_float ((double) XINT (arg));
  else				/* give 'em the same float back */
    return arg;
}

DEFUN ("logb", Flogb, Slogb, 1, 1, 0,
       doc: /* Returns largest integer <= the base 2 log of the magnitude of ARG.
This is the same as the exponent of a float.  */)
  (Lisp_Object arg)
{
  Lisp_Object val;
  EMACS_INT value;
  double f = extract_float (arg);

  if (f == 0.0)
    value = MOST_NEGATIVE_FIXNUM;
  else
    {
#ifdef HAVE_LOGB
      IN_FLOAT (value = logb (f), "logb", arg);
#else
#ifdef HAVE_FREXP
      int ivalue;
      IN_FLOAT (frexp (f, &ivalue), "logb", arg);
      value = ivalue - 1;
#else
      int i;
      double d;
      if (f < 0.0)
	f = -f;
      value = -1;
      while (f < 0.5)
	{
	  for (i = 1, d = 0.5; d * d >= f; i += i)
	    d *= d;
	  f /= d;
	  value -= i;
	}
      while (f >= 1.0)
	{
	  for (i = 1, d = 2.0; d * d <= f; i += i)
	    d *= d;
	  f /= d;
	  value += i;
	}
#endif
#endif
    }
  XSETINT (val, value);
  return val;
}


/* the rounding functions  */

static Lisp_Object
rounding_driver (Lisp_Object arg, Lisp_Object divisor,
		 double (*double_round) (double),
		 EMACS_INT (*int_round2) (EMACS_INT, EMACS_INT),
		 const char *name)
{
  CHECK_NUMBER_OR_FLOAT (arg);

  if (! NILP (divisor))
    {
      EMACS_INT i1, i2;

      CHECK_NUMBER_OR_FLOAT (divisor);

      if (FLOATP (arg) || FLOATP (divisor))
	{
	  double f1, f2;

	  f1 = FLOATP (arg) ? XFLOAT_DATA (arg) : XINT (arg);
	  f2 = (FLOATP (divisor) ? XFLOAT_DATA (divisor) : XINT (divisor));
	  if (! IEEE_FLOATING_POINT && f2 == 0)
	    xsignal0 (Qarith_error);

	  IN_FLOAT2 (f1 = (*double_round) (f1 / f2), name, arg, divisor);
	  FLOAT_TO_INT2 (f1, arg, name, arg, divisor);
	  return arg;
	}

      i1 = XINT (arg);
      i2 = XINT (divisor);

      if (i2 == 0)
	xsignal0 (Qarith_error);

      XSETINT (arg, (*int_round2) (i1, i2));
      return arg;
    }

  if (FLOATP (arg))
    {
      double d;

      IN_FLOAT (d = (*double_round) (XFLOAT_DATA (arg)), name, arg);
      FLOAT_TO_INT (d, arg, name, arg);
    }

  return arg;
}

/* With C's /, the result is implementation-defined if either operand
   is negative, so take care with negative operands in the following
   integer functions.  */

static EMACS_INT
ceiling2 (EMACS_INT i1, EMACS_INT i2)
{
  return (i2 < 0
	  ? (i1 < 0  ?  ((-1 - i1) / -i2) + 1  :  - (i1 / -i2))
	  : (i1 <= 0  ?  - (-i1 / i2)  :  ((i1 - 1) / i2) + 1));
}

static EMACS_INT
floor2 (EMACS_INT i1, EMACS_INT i2)
{
  return (i2 < 0
	  ? (i1 <= 0  ?  -i1 / -i2  :  -1 - ((i1 - 1) / -i2))
	  : (i1 < 0  ?  -1 - ((-1 - i1) / i2)  :  i1 / i2));
}

static EMACS_INT
truncate2 (EMACS_INT i1, EMACS_INT i2)
{
  return (i2 < 0
	  ? (i1 < 0  ?  -i1 / -i2  :  - (i1 / -i2))
	  : (i1 < 0  ?  - (-i1 / i2)  :  i1 / i2));
}

static EMACS_INT
round2 (EMACS_INT i1, EMACS_INT i2)
{
  /* The C language's division operator gives us one remainder R, but
     we want the remainder R1 on the other side of 0 if R1 is closer
     to 0 than R is; because we want to round to even, we also want R1
     if R and R1 are the same distance from 0 and if C's quotient is
     odd.  */
  EMACS_INT q = i1 / i2;
  EMACS_INT r = i1 % i2;
  EMACS_INT abs_r = r < 0 ? -r : r;
  EMACS_INT abs_r1 = (i2 < 0 ? -i2 : i2) - abs_r;
  return q + (abs_r + (q & 1) <= abs_r1 ? 0 : (i2 ^ r) < 0 ? -1 : 1);
}

/* The code uses emacs_rint, so that it works to undefine HAVE_RINT
   if `rint' exists but does not work right.  */
#ifdef HAVE_RINT
#define emacs_rint rint
#else
static double
emacs_rint (double d)
{
  return floor (d + 0.5);
}
#endif

static double
double_identity (double d)
{
  return d;
}

DEFUN ("ceiling", Fceiling, Sceiling, 1, 2, 0,
       doc: /* Return the smallest integer no less than ARG.
This rounds the value towards +inf.
With optional DIVISOR, return the smallest integer no less than ARG/DIVISOR.  */)
  (Lisp_Object arg, Lisp_Object divisor)
{
  return rounding_driver (arg, divisor, ceil, ceiling2, "ceiling");
}

DEFUN ("floor", Ffloor, Sfloor, 1, 2, 0,
       doc: /* Return the largest integer no greater than ARG.
This rounds the value towards -inf.
With optional DIVISOR, return the largest integer no greater than ARG/DIVISOR.  */)
  (Lisp_Object arg, Lisp_Object divisor)
{
  return rounding_driver (arg, divisor, floor, floor2, "floor");
}

DEFUN ("round", Fround, Sround, 1, 2, 0,
       doc: /* Return the nearest integer to ARG.
With optional DIVISOR, return the nearest integer to ARG/DIVISOR.

Rounding a value equidistant between two integers may choose the
integer closer to zero, or it may prefer an even integer, depending on
your machine.  For example, \(round 2.5\) can return 3 on some
systems, but 2 on others.  */)
  (Lisp_Object arg, Lisp_Object divisor)
{
  return rounding_driver (arg, divisor, emacs_rint, round2, "round");
}

DEFUN ("truncate", Ftruncate, Struncate, 1, 2, 0,
       doc: /* Truncate a floating point number to an int.
Rounds ARG toward zero.
With optional DIVISOR, truncate ARG/DIVISOR.  */)
  (Lisp_Object arg, Lisp_Object divisor)
{
  return rounding_driver (arg, divisor, double_identity, truncate2,
			  "truncate");
}


Lisp_Object
fmod_float (Lisp_Object x, Lisp_Object y)
{
  double f1, f2;

  f1 = FLOATP (x) ? XFLOAT_DATA (x) : XINT (x);
  f2 = FLOATP (y) ? XFLOAT_DATA (y) : XINT (y);

  if (! IEEE_FLOATING_POINT && f2 == 0)
    xsignal0 (Qarith_error);

  /* If the "remainder" comes out with the wrong sign, fix it.  */
  IN_FLOAT2 ((f1 = fmod (f1, f2),
	      f1 = (f2 < 0 ? f1 > 0 : f1 < 0) ? f1 + f2 : f1),
	     "mod", x, y);
  return make_float (f1);
}

/* It's not clear these are worth adding.  */

DEFUN ("fceiling", Ffceiling, Sfceiling, 1, 1, 0,
       doc: /* Return the smallest integer no less than ARG, as a float.
\(Round toward +inf.\)  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = ceil (d), "fceiling", arg);
  return make_float (d);
}

DEFUN ("ffloor", Fffloor, Sffloor, 1, 1, 0,
       doc: /* Return the largest integer no greater than ARG, as a float.
\(Round towards -inf.\)  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = floor (d), "ffloor", arg);
  return make_float (d);
}

DEFUN ("fround", Ffround, Sfround, 1, 1, 0,
       doc: /* Return the nearest integer to ARG, as a float.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  IN_FLOAT (d = emacs_rint (d), "fround", arg);
  return make_float (d);
}

DEFUN ("ftruncate", Fftruncate, Sftruncate, 1, 1, 0,
       doc: /* Truncate a floating point number to an integral float value.
Rounds the value toward zero.  */)
  (register Lisp_Object arg)
{
  double d = extract_float (arg);
  if (d >= 0.0)
    IN_FLOAT (d = floor (d), "ftruncate", arg);
  else
    IN_FLOAT (d = ceil (d), "ftruncate", arg);
  return make_float (d);
}

#ifdef FLOAT_CATCH_SIGILL
static void
float_error (int signo)
{
  if (! in_float)
    fatal_error_signal (signo);

#ifdef BSD_SYSTEM
  sigsetmask (SIGEMPTYMASK);
#else
  /* Must reestablish handler each time it is called.  */
  signal (SIGILL, float_error);
#endif /* BSD_SYSTEM */

  SIGNAL_THREAD_CHECK (signo);
  in_float = 0;

  xsignal1 (Qarith_error, float_error_arg);
}

/* Another idea was to replace the library function `infnan'
   where SIGILL is signaled.  */

#endif /* FLOAT_CATCH_SIGILL */

#ifdef HAVE_MATHERR
int
matherr (struct exception *x)
{
  Lisp_Object args;
  const char *name = x->name;

  if (! in_float)
    /* Not called from emacs-lisp float routines; do the default thing. */
    return 0;
  if (!strcmp (x->name, "pow"))
    name = "expt";

  args
    = Fcons (build_string (name),
	     Fcons (make_float (x->arg1),
		    ((!strcmp (name, "log") || !strcmp (name, "pow"))
		     ? Fcons (make_float (x->arg2), Qnil)
		     : Qnil)));
  switch (x->type)
    {
    case DOMAIN:	xsignal (Qdomain_error, args);		break;
    case SING:		xsignal (Qsingularity_error, args);	break;
    case OVERFLOW:	xsignal (Qoverflow_error, args);	break;
    case UNDERFLOW:	xsignal (Qunderflow_error, args);	break;
    default:		xsignal (Qarith_error, args);		break;
    }
  return (1);	/* don't set errno or print a message */
}
#endif /* HAVE_MATHERR */

void
init_floatfns (void)
{
#ifdef FLOAT_CATCH_SIGILL
  signal (SIGILL, float_error);
#endif
  in_float = 0;
}

void
syms_of_floatfns (void)
{
  defsubr (&Sacos);
  defsubr (&Sasin);
  defsubr (&Satan);
  defsubr (&Scos);
  defsubr (&Ssin);
  defsubr (&Stan);
  defsubr (&Sisnan);
#ifdef HAVE_COPYSIGN
  defsubr (&Scopysign);
  defsubr (&Sfrexp);
  defsubr (&Sldexp);
#endif
#if 0
  defsubr (&Sacosh);
  defsubr (&Sasinh);
  defsubr (&Satanh);
  defsubr (&Scosh);
  defsubr (&Ssinh);
  defsubr (&Stanh);
  defsubr (&Sbessel_y0);
  defsubr (&Sbessel_y1);
  defsubr (&Sbessel_yn);
  defsubr (&Sbessel_j0);
  defsubr (&Sbessel_j1);
  defsubr (&Sbessel_jn);
  defsubr (&Serf);
  defsubr (&Serfc);
  defsubr (&Slog_gamma);
  defsubr (&Scube_root);
#endif
  defsubr (&Sfceiling);
  defsubr (&Sffloor);
  defsubr (&Sfround);
  defsubr (&Sftruncate);
  defsubr (&Sexp);
  defsubr (&Sexpt);
  defsubr (&Slog);
  defsubr (&Slog10);
  defsubr (&Ssqrt);

  defsubr (&Sabs);
  defsubr (&Sfloat);
  defsubr (&Slogb);
  defsubr (&Sceiling);
  defsubr (&Sfloor);
  defsubr (&Sround);
  defsubr (&Struncate);
}
