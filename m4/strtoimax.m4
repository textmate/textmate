# strtoimax.m4 serial 11
dnl Copyright (C) 2002-2004, 2006, 2009-2011 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_STRTOIMAX],
[
  AC_REQUIRE([gl_INTTYPES_H_DEFAULTS])

  dnl On OSF/1 5.1 with cc, this function is declared but not defined.
  AC_CHECK_FUNCS_ONCE([strtoimax])
  AC_CHECK_DECLS_ONCE([strtoimax])
  if test "$ac_cv_have_decl_strtoimax" != yes; then
    HAVE_DECL_STRTOIMAX=0
  fi
])

# Prerequisites of lib/strtoimax.c.
AC_DEFUN([gl_PREREQ_STRTOIMAX], [
  AC_CHECK_DECLS([strtoll])
  AC_REQUIRE([AC_TYPE_LONG_LONG_INT])
])
