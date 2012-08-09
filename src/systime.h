/* systime.h - System-dependent definitions for time manipulations.
   Copyright (C) 1993-1994, 2002-2012 Free Software Foundation, Inc.

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

#ifndef EMACS_SYSTIME_H
#define EMACS_SYSTIME_H

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#ifdef emacs
# ifdef HAVE_X_WINDOWS
#  include <X11/X.h>
# else
typedef unsigned long Time;
# endif
#endif

/* On some configurations (hpux8.0, X11R4), sys/time.h and X11/Xos.h
   disagree about the name of the guard symbol.  */
#ifdef HPUX
#ifdef _STRUCT_TIMEVAL
#ifndef __TIMEVAL__
#define __TIMEVAL__
#endif
#endif
#endif

/* EMACS_TIME is the type to use to represent temporal intervals -
   struct timeval on some systems, int on others.  It can be passed as
   the timeout argument to the select  system call.

   EMACS_SECS (TIME) is an rvalue for the seconds component of TIME.
   EMACS_SET_SECS (TIME, SECONDS) sets that to SECONDS.

   EMACS_HAS_USECS is defined if EMACS_TIME has a usecs component.
   EMACS_USECS (TIME) is an rvalue for the microseconds component of TIME.
   	This returns zero if EMACS_TIME doesn't have a microseconds component.
   EMACS_SET_USECS (TIME, MICROSECONDS) sets that to MICROSECONDS.
	This does nothing if EMACS_TIME doesn't have a microseconds component.

   EMACS_SET_SECS_USECS (TIME, SECS, USECS) sets both components of TIME.

   EMACS_GET_TIME (TIME) stores the current system time in TIME, which
	should be an lvalue.

   EMACS_ADD_TIME (DEST, SRC1, SRC2) adds SRC1 to SRC2 and stores the
	result in DEST.  SRC should not be negative.

   EMACS_SUB_TIME (DEST, SRC1, SRC2) subtracts SRC2 from SRC1 and
	stores the result in DEST.  SRC should not be negative.
   EMACS_TIME_NEG_P (TIME) is true if TIME is negative.

*/

#ifdef HAVE_TIMEVAL

#define EMACS_HAS_USECS

#define EMACS_TIME struct timeval
#define EMACS_SECS(time)		    ((time).tv_sec  + 0)
#define EMACS_USECS(time)		    ((time).tv_usec + 0)
#define EMACS_SET_SECS(time, seconds)	    ((time).tv_sec  = (seconds))
#define EMACS_SET_USECS(time, microseconds) ((time).tv_usec = (microseconds))

/* On SVR4, the compiler may complain if given this extra BSD arg.  */
#ifdef GETTIMEOFDAY_ONE_ARGUMENT
#define EMACS_GET_TIME(time) gettimeofday (&(time))
#else /* not GETTIMEOFDAY_ONE_ARGUMENT */
/* Presumably the second arg is ignored.  */
#define EMACS_GET_TIME(time) gettimeofday (&(time), NULL)
#endif /* not GETTIMEOFDAY_ONE_ARGUMENT */

#define EMACS_ADD_TIME(dest, src1, src2)		\
  do {							\
    (dest).tv_sec  = (src1).tv_sec  + (src2).tv_sec;	\
    (dest).tv_usec = (src1).tv_usec + (src2).tv_usec;	\
    if ((dest).tv_usec > 1000000)			\
      (dest).tv_usec -= 1000000, (dest).tv_sec++;	\
  } while (0)

#define EMACS_SUB_TIME(dest, src1, src2)		\
  do {							\
    (dest).tv_sec  = (src1).tv_sec  - (src2).tv_sec;	\
    (dest).tv_usec = (src1).tv_usec - (src2).tv_usec;	\
    if ((dest).tv_usec < 0)				\
      (dest).tv_usec += 1000000, (dest).tv_sec--;	\
  } while (0)

#define EMACS_TIME_NEG_P(time)					\
  ((long)(time).tv_sec < 0					\
   || ((time).tv_sec == 0					\
       && (long)(time).tv_usec < 0))

#else /* ! defined (HAVE_TIMEVAL) */

#define EMACS_TIME int
#define EMACS_SECS(time)		    (time)
#define EMACS_USECS(time)		    0
#define EMACS_SET_SECS(time, seconds)	    ((time) = (seconds))
#define EMACS_SET_USECS(time, usecs)	    0

#define EMACS_GET_TIME(t) ((t) = time ((long *) 0))
#define EMACS_ADD_TIME(dest, src1, src2) ((dest) = (src1) + (src2))
#define EMACS_SUB_TIME(dest, src1, src2) ((dest) = (src1) - (src2))
#define EMACS_TIME_NEG_P(t) ((t) < 0)

#endif /* ! defined (HAVE_TIMEVAL) */

#define EMACS_SET_SECS_USECS(time, secs, usecs) 		\
  (EMACS_SET_SECS (time, secs), EMACS_SET_USECS (time, usecs))

extern int set_file_times (const char *, EMACS_TIME, EMACS_TIME);

/* defined in keyboard.c */
extern void set_waiting_for_input (EMACS_TIME *);

/* When lisp.h is not included Lisp_Object is not defined (this can
   happen when this files is used outside the src directory).
   Use GCPRO1 to determine if lisp.h was included.  */
#ifdef GCPRO1
/* defined in editfns.c*/
extern Lisp_Object make_time (time_t);
extern int lisp_time_argument (Lisp_Object, time_t *, int *);
#endif

/* Compare times T1 and T2.  Value is 0 if T1 and T2 are the same.
   Value is < 0 if T1 is less than T2.  Value is > 0 otherwise.  (Cast
   to long is for those platforms where time_t is an unsigned
   type, and where otherwise T1 will always be grater than T2.)  */

#define EMACS_TIME_CMP(T1, T2)				\
  ((long)EMACS_SECS (T1) - (long)EMACS_SECS (T2)	\
   + (EMACS_SECS (T1) == EMACS_SECS (T2)		\
      ? EMACS_USECS (T1) - EMACS_USECS (T2)		\
      : 0))

/* Compare times T1 and T2 for equality, inequality etc.  */

#define EMACS_TIME_EQ(T1, T2) (EMACS_TIME_CMP (T1, T2) == 0)
#define EMACS_TIME_NE(T1, T2) (EMACS_TIME_CMP (T1, T2) != 0)
#define EMACS_TIME_GT(T1, T2) (EMACS_TIME_CMP (T1, T2) > 0)
#define EMACS_TIME_GE(T1, T2) (EMACS_TIME_CMP (T1, T2) >= 0)
#define EMACS_TIME_LT(T1, T2) (EMACS_TIME_CMP (T1, T2) < 0)
#define EMACS_TIME_LE(T1, T2) (EMACS_TIME_CMP (T1, T2) <= 0)

#endif /* EMACS_SYSTIME_H */
