# socklen.m4 serial 10
dnl Copyright (C) 2005-2007, 2009-2011 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl From Albert Chin, Windows fixes from Simon Josefsson.

dnl Check for socklen_t: historically on BSD it is an int, and in
dnl POSIX 1g it is a type of its own, but some platforms use different
dnl types for the argument to getsockopt, getpeername, etc.:
dnl HP-UX 10.20, IRIX 6.5, OSF/1 4.0, Interix 3.5, BeOS.
dnl So we have to test to find something that will work.

AC_DEFUN([gl_TYPE_SOCKLEN_T],
  [AC_REQUIRE([gl_CHECK_SOCKET_HEADERS])dnl
   AC_CHECK_TYPE([socklen_t], ,
     [AC_MSG_CHECKING([for socklen_t equivalent])
      AC_CACHE_VAL([gl_cv_socklen_t_equiv],
        [# Systems have either "struct sockaddr *" or
         # "void *" as the second argument to getpeername
         gl_cv_socklen_t_equiv=
         for arg2 in "struct sockaddr" void; do
           for t in int size_t "unsigned int" "long int" "unsigned long int"; do
             AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
                 [[#include <sys/types.h>
                   #include <sys/socket.h>

                   int getpeername (int, $arg2 *, $t *);]],
                 [[$t len;
                  getpeername (0, 0, &len);]])],
               [gl_cv_socklen_t_equiv="$t"])
             test "$gl_cv_socklen_t_equiv" != "" && break
           done
           test "$gl_cv_socklen_t_equiv" != "" && break
         done
      ])
      if test "$gl_cv_socklen_t_equiv" = ""; then
        AC_MSG_ERROR([Cannot find a type to use in place of socklen_t])
      fi
      AC_MSG_RESULT([$gl_cv_socklen_t_equiv])
      AC_DEFINE_UNQUOTED([socklen_t], [$gl_cv_socklen_t_equiv],
        [type to use in place of socklen_t if not defined])],
     [gl_SOCKET_HEADERS])])

dnl On mingw32, socklen_t is in ws2tcpip.h ('int'), so we try to find
dnl it there too.  But on Cygwin, wc2tcpip.h must not be included.  Users
dnl of this module should use the same include pattern as gl_SOCKET_HEADERS.
dnl When you change this macro, keep also in sync:
dnl   - gl_CHECK_SOCKET_HEADERS,
dnl   - the Include section of modules/socklen.
AC_DEFUN([gl_SOCKET_HEADERS],
[
/* <sys/types.h> is not needed according to POSIX, but the
   <sys/socket.h> in i386-unknown-freebsd4.10 and
   powerpc-apple-darwin5.5 required it. */
#include <sys/types.h>
#if HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#elif HAVE_WS2TCPIP_H
# include <ws2tcpip.h>
#endif
])

dnl Tests for the existence of the header for socket facilities.
dnl Defines the C macros HAVE_SYS_SOCKET_H, HAVE_WS2TCPIP_H.
dnl This macro must match gl_SOCKET_HEADERS.
AC_DEFUN([gl_CHECK_SOCKET_HEADERS],
  [AC_CHECK_HEADERS_ONCE([sys/socket.h])
   if test $ac_cv_header_sys_socket_h = no; then
     dnl We cannot use AC_CHECK_HEADERS_ONCE here, because that would make
     dnl the check for those headers unconditional; yet cygwin reports
     dnl that the headers are present but cannot be compiled (since on
     dnl cygwin, all socket information should come from sys/socket.h).
     AC_CHECK_HEADERS([ws2tcpip.h])
   fi
  ])
