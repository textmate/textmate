/* profile.c --- generate periodic events for profiling of Emacs Lisp code.
   Copyright (C) 1992, 1994, 1999, 2001-2012  Free Software Foundation, Inc.

Author: Boaz Ben-Zvi <boaz@lcs.mit.edu>

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


/**
 **  To be run as an emacs process. Input string that starts with:
 **    'z' -- resets the watch (to zero).
 **    'p' -- return time (on stdout) as string with format <sec>.<micro-sec>
 **    'q' -- exit.
 **
 **  abstraction : a stopwatch
 **  operations: reset_watch, get_time
 */
#include <config.h>
#include <stdio.h>
#include <systime.h>

static EMACS_TIME TV1, TV2;
static int watch_not_started = 1; /* flag */
static char time_string[30];

/* Reset the stopwatch to zero.  */

static void
reset_watch (void)
{
  EMACS_GET_TIME (TV1);
  watch_not_started = 0;
}

/* This call returns the time since the last reset_watch call.  The time
   is returned as a string with the format  <seconds>.<micro-seconds>
   If reset_watch was not called yet, exit.  */

static char *
get_time (void)
{
  if (watch_not_started)
    exit (EXIT_FAILURE);  /* call reset_watch first ! */
  EMACS_GET_TIME (TV2);
  EMACS_SUB_TIME (TV2, TV2, TV1);
  sprintf (time_string, "%lu.%06lu", (unsigned long)EMACS_SECS (TV2), (unsigned long)EMACS_USECS (TV2));
  return time_string;
}

#if ! defined (HAVE_GETTIMEOFDAY) && defined (HAVE_TIMEVAL)

/* ARGSUSED */
gettimeofday (tp, tzp)
     struct timeval *tp;
     struct timezone *tzp;
{
  extern long time ();

  tp->tv_sec = time ((long *)0);
  tp->tv_usec = 0;
  if (tzp != 0)
    tzp->tz_minuteswest = -1;
}

#endif

int
main (void)
{
  int c;
  while ((c = getchar ()) != EOF)
    {
      switch (c)
	{
	case 'z':
	  reset_watch ();
	  break;
	case 'p':
	  puts (get_time ());
	  break;
	case 'q':
	  exit (EXIT_SUCCESS);
	}
      /* Anything remaining on the line is ignored.  */
      while (c != '\n' && c != EOF)
	c = getchar ();
    }
  exit (EXIT_FAILURE);
}


/* profile.c ends here */
