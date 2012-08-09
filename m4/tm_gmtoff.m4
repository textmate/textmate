# tm_gmtoff.m4 serial 3
dnl Copyright (C) 2002, 2009-2011 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_TM_GMTOFF],
[
 AC_CHECK_MEMBER([struct tm.tm_gmtoff],
                 [AC_DEFINE([HAVE_TM_GMTOFF], [1],
                            [Define if struct tm has the tm_gmtoff member.])],
                 ,
                 [#include <time.h>])
])
