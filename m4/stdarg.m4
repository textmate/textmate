# stdarg.m4 serial 6
dnl Copyright (C) 2006, 2008-2011 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Bruno Haible.
dnl Provide a working va_copy in combination with <stdarg.h>.

AC_DEFUN([gl_STDARG_H],
[
  STDARG_H=''
  NEXT_STDARG_H='<stdarg.h>'
  AC_MSG_CHECKING([for va_copy])
  AC_CACHE_VAL([gl_cv_func_va_copy], [
    AC_COMPILE_IFELSE(
      [AC_LANG_PROGRAM(
         [[#include <stdarg.h>]],
         [[
#ifndef va_copy
void (*func) (va_list, va_list) = va_copy;
#endif
         ]])],
      [gl_cv_func_va_copy=yes],
      [gl_cv_func_va_copy=no])])
  AC_MSG_RESULT([$gl_cv_func_va_copy])
  if test $gl_cv_func_va_copy = no; then
    dnl Provide a substitute.
    dnl Usually a simple definition in <config.h> is enough. Not so on AIX 5
    dnl with some versions of the /usr/vac/bin/cc compiler. It has an <stdarg.h>
    dnl which does '#undef va_copy', leading to a missing va_copy symbol. For
    dnl this platform, we use an <stdarg.h> substitute. But we cannot use this
    dnl approach on other platforms, because <stdarg.h> often defines only
    dnl preprocessor macros and gl_ABSOLUTE_HEADER, gl_CHECK_NEXT_HEADERS do
    dnl not work in this situation.
    AC_EGREP_CPP([vaccine],
      [#if defined _AIX && !defined __GNUC__
        AIX vaccine
       #endif
      ], [gl_aixcc=yes], [gl_aixcc=no])
    if test $gl_aixcc = yes; then
      dnl Provide a substitute <stdarg.h> file.
      STDARG_H=stdarg.h
      gl_NEXT_HEADERS([stdarg.h])
      dnl Fallback for the case when <stdarg.h> contains only macro definitions.
      if test "$gl_cv_next_stdarg_h" = '""'; then
        gl_cv_next_stdarg_h='"///usr/include/stdarg.h"'
        NEXT_STDARG_H="$gl_cv_next_stdarg_h"
      fi
    else
      dnl Provide a substitute in <config.h>, either __va_copy or as a simple
      dnl assignment.
      gl_CACHE_VAL_SILENT([gl_cv_func___va_copy], [
        AC_COMPILE_IFELSE(
          [AC_LANG_PROGRAM(
             [[#include <stdarg.h>]],
             [[
#ifndef __va_copy
error, bail out
#endif
             ]])],
          [gl_cv_func___va_copy=yes],
          [gl_cv_func___va_copy=no])])
      if test $gl_cv_func___va_copy = yes; then
        AC_DEFINE([va_copy], [__va_copy],
          [Define as a macro for copying va_list variables.])
      else
        AH_VERBATIM([gl_VA_COPY], [/* A replacement for va_copy, if needed.  */
#define gl_va_copy(a,b) ((a) = (b))])
        AC_DEFINE([va_copy], [gl_va_copy],
          [Define as a macro for copying va_list variables.])
      fi
    fi
  fi
  AC_SUBST([STDARG_H])
  AM_CONDITIONAL([GL_GENERATE_STDARG_H], [test -n "$STDARG_H"])
  AC_SUBST([NEXT_STDARG_H])
])
