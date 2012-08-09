# serial 33

# Copyright (C) 1996-1997, 1999-2007, 2009-2011 Free Software Foundation, Inc.
#
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# Written by Jim Meyering and Paul Eggert.

AC_DEFUN([gl_FUNC_GNU_STRFTIME],
[
  gl_FUNC_STRFTIME
])

# These are the prerequisite macros for GNU's strftime.c replacement.
AC_DEFUN([gl_FUNC_STRFTIME],
[
 # This defines (or not) HAVE_TZNAME and HAVE_TM_ZONE.
 AC_REQUIRE([AC_STRUCT_TIMEZONE])

 AC_REQUIRE([gl_TM_GMTOFF])

 AC_CHECK_FUNCS_ONCE([tzset])

 AC_DEFINE([my_strftime], [nstrftime],
   [Define to the name of the strftime replacement function.])
])
