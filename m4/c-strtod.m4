# c-strtod.m4 serial 14

# Copyright (C) 2004-2006, 2009-2011 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# Written by Paul Eggert.

AC_DEFUN([gl_C99_STRTOLD],
[
  AC_CACHE_CHECK([whether strtold conforms to C99],
    [gl_cv_func_c99_strtold],
    [AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
          [[/* On HP-UX before 11.23, strtold returns a struct instead of
                long double.  Reject implementations like that, by requiring
                compatibility with the C99 prototype.  */
             #include <stdlib.h>
             static long double (*p) (char const *, char **) = strtold;
             static long double
             test (char const *nptr, char **endptr)
             {
               long double r;
               r = strtold (nptr, endptr);
               return r;
             }]],
           [[return test ("1.0", NULL) != 1 || p ("1.0", NULL) != 1;]])],
       [gl_cv_func_c99_strtold=yes],
       [gl_cv_func_c99_strtold=no])])
  if test $gl_cv_func_c99_strtold = yes; then
    AC_DEFINE([HAVE_C99_STRTOLD], [1], [Define to 1 if strtold conforms to C99.])
  fi
])

dnl Prerequisites of lib/c-strtod.c.
AC_DEFUN([gl_C_STRTOD],
[
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  AC_CHECK_FUNCS([strtod_l])

  AC_REQUIRE([AC_C_INLINE])
  :
])

dnl Prerequisites of lib/c-strtold.c.
AC_DEFUN([gl_C_STRTOLD],
[
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  AC_REQUIRE([gl_C99_STRTOLD])
  AC_CHECK_FUNCS([strtold_l])

  AC_REQUIRE([AC_C_INLINE])
  :
])
