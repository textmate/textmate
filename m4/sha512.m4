# sha512.m4 serial 6
dnl Copyright (C) 2005-2006, 2008-2011 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_SHA512],
[
  dnl Prerequisites of lib/sha512.c.
  AC_REQUIRE([gl_BIGENDIAN])
  AC_REQUIRE([AC_C_INLINE])
])
