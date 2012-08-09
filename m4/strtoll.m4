# strtoll.m4 serial 7
dnl Copyright (C) 2002, 2004, 2006, 2008-2011 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_STRTOLL],
[
  AC_REQUIRE([gl_STDLIB_H_DEFAULTS])
  dnl We don't need (and can't compile) the replacement strtoll
  dnl unless the type 'long long int' exists.
  AC_REQUIRE([AC_TYPE_LONG_LONG_INT])
  if test "$ac_cv_type_long_long_int" = yes; then
    AC_CHECK_FUNCS([strtoll])
    if test $ac_cv_func_strtoll = no; then
      HAVE_STRTOLL=0
    fi
  fi
])

# Prerequisites of lib/strtoll.c.
AC_DEFUN([gl_PREREQ_STRTOLL], [
  :
])
