/* Asynchronous timers.
   Copyright (C) 2000-2012  Free Software Foundation, Inc.

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

#include <config.h>
#include <signal.h>
#include <stdio.h>
#include <setjmp.h>
#include "lisp.h"
#include "syssignal.h"
#include "systime.h"
#include "blockinput.h"
#include "atimer.h"
#include <unistd.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

/* Free-list of atimer structures.  */

static struct atimer *free_atimers;

/* List of currently not running timers due to a call to
   lock_atimer.  */

static struct atimer *stopped_atimers;

/* List of active atimers, sorted by expiration time.  The timer that
   will become ripe next is always at the front of this list.  */

static struct atimer *atimers;

/* Non-zero means alarm_signal_handler has found ripe timers but
   interrupt_input_blocked was non-zero.  In this case, timer
   functions are not called until the next UNBLOCK_INPUT because timer
   functions are expected to call X, and X cannot be assumed to be
   reentrant.  */

int pending_atimers;

/* Block/unblock SIGALRM.  */

#define BLOCK_ATIMERS   sigblock (sigmask (SIGALRM))
#define UNBLOCK_ATIMERS sigunblock (sigmask (SIGALRM))

/* Function prototypes.  */

static void set_alarm (void);
static void schedule_atimer (struct atimer *);
static struct atimer *append_atimer_lists (struct atimer *,
                                           struct atimer *);
static void alarm_signal_handler (int signo);


/* Start a new atimer of type TYPE.  TIME specifies when the timer is
   ripe.  FN is the function to call when the timer fires.
   CLIENT_DATA is stored in the client_data member of the atimer
   structure returned and so made available to FN when it is called.

   If TYPE is ATIMER_ABSOLUTE, TIME is the absolute time at which the
   timer fires.

   If TYPE is ATIMER_RELATIVE, the timer is ripe TIME s/us in the
   future.

   In both cases, the timer is automatically freed after it has fired.

   If TYPE is ATIMER_CONTINUOUS, the timer fires every TIME s/us.

   Value is a pointer to the atimer started.  It can be used in calls
   to cancel_atimer; don't free it yourself.  */

struct atimer *
start_atimer (enum atimer_type type, EMACS_TIME timestamp, atimer_callback fn,
	      void *client_data)
{
  struct atimer *t;

  /* Round TIME up to the next full second if we don't have
     itimers.  */
#ifndef HAVE_SETITIMER
  if (EMACS_USECS (timestamp) != 0)
    {
      EMACS_SET_USECS (timestamp, 0);
      EMACS_SET_SECS (timestamp, EMACS_SECS (timestamp) + 1);
    }
#endif /* not HAVE_SETITIMER */

  /* Get an atimer structure from the free-list, or allocate
     a new one.  */
  if (free_atimers)
    {
      t = free_atimers;
      free_atimers = t->next;
    }
  else
    t = (struct atimer *) xmalloc (sizeof *t);

  /* Fill the atimer structure.  */
  memset (t, 0, sizeof *t);
  t->type = type;
  t->fn = fn;
  t->client_data = client_data;

  BLOCK_ATIMERS;

  /* Compute the timer's expiration time.  */
  switch (type)
    {
    case ATIMER_ABSOLUTE:
      t->expiration = timestamp;
      break;

    case ATIMER_RELATIVE:
      EMACS_GET_TIME (t->expiration);
      EMACS_ADD_TIME (t->expiration, t->expiration, timestamp);
      break;

    case ATIMER_CONTINUOUS:
      EMACS_GET_TIME (t->expiration);
      EMACS_ADD_TIME (t->expiration, t->expiration, timestamp);
      t->interval = timestamp;
      break;
    }

  /* Insert the timer in the list of active atimers.  */
  schedule_atimer (t);
  UNBLOCK_ATIMERS;

  /* Arrange for a SIGALRM at the time the next atimer is ripe.  */
  set_alarm ();

  return t;
}


/* Cancel and free atimer TIMER.  */

void
cancel_atimer (struct atimer *timer)
{
  int i;

  BLOCK_ATIMERS;

  for (i = 0; i < 2; ++i)
    {
      struct atimer *t, *prev;
      struct atimer **list = i ? &stopped_atimers : &atimers;

      /* See if TIMER is active or stopped.  */
      for (t = *list, prev = NULL; t && t != timer; prev = t, t = t->next)
	;

      /* If it is, take it off its list, and put in on the free-list.
	 We don't bother to arrange for setting a different alarm time,
	 since a too early one doesn't hurt.  */
      if (t)
	{
	  if (prev)
	    prev->next = t->next;
	  else
	    *list = t->next;

	  t->next = free_atimers;
	  free_atimers = t;
	  break;
	}
    }

  UNBLOCK_ATIMERS;
}


/* Append two lists of atimers LIST_1 and LIST_2 and return the
   result list.  */

static struct atimer *
append_atimer_lists (struct atimer *list_1, struct atimer *list_2)
{
  if (list_1 == NULL)
    return list_2;
  else if (list_2 == NULL)
    return list_1;
  else
    {
      struct atimer *p;

      for (p = list_1; p->next; p = p->next)
	;
      p->next = list_2;
      return list_1;
    }
}


/* Stop all timers except timer T.  T null means stop all timers.  */

void
stop_other_atimers (struct atimer *t)
{
  BLOCK_ATIMERS;

  if (t)
    {
      struct atimer *p, *prev;

      /* See if T is active.  */
      for (p = atimers, prev = NULL; p && p != t; prev = p, p = p->next)
	;

      if (p == t)
	{
	  if (prev)
	    prev->next = t->next;
	  else
	    atimers = t->next;
	  t->next = NULL;
	}
      else
	/* T is not active.  Let's handle this like T == 0.  */
	t = NULL;
    }

  stopped_atimers = append_atimer_lists (atimers, stopped_atimers);
  atimers = t;
  UNBLOCK_ATIMERS;
}


/* Run all timers again, if some have been stopped with a call to
   stop_other_atimers.  */

static void
run_all_atimers (void)
{
  if (stopped_atimers)
    {
      struct atimer *t = atimers;
      struct atimer *next;

      BLOCK_ATIMERS;
      atimers = stopped_atimers;
      stopped_atimers = NULL;

      while (t)
	{
	  next = t->next;
	  schedule_atimer (t);
	  t = next;
	}

      UNBLOCK_ATIMERS;
    }
}


/* A version of run_all_atimers suitable for a record_unwind_protect.  */

Lisp_Object
unwind_stop_other_atimers (Lisp_Object dummy)
{
  run_all_atimers ();
  return Qnil;
}


/* Arrange for a SIGALRM to arrive when the next timer is ripe.  */

static void
set_alarm (void)
{
  if (atimers)
    {
      EMACS_TIME now, timestamp;
#ifdef HAVE_SETITIMER
      struct itimerval it;
#endif

      /* Determine s/us till the next timer is ripe.  */
      EMACS_GET_TIME (now);
      EMACS_SUB_TIME (timestamp, atimers->expiration, now);

#ifdef HAVE_SETITIMER
      /* Don't set the interval to 0; this disables the timer.  */
      if (EMACS_TIME_LE (atimers->expiration, now))
	{
	  EMACS_SET_SECS (timestamp, 0);
	  EMACS_SET_USECS (timestamp, 1000);
	}

      memset (&it, 0, sizeof it);
      it.it_value = timestamp;
      setitimer (ITIMER_REAL, &it, 0);
#else /* not HAVE_SETITIMER */
      alarm (max (EMACS_SECS (timestamp), 1));
#endif /* not HAVE_SETITIMER */
    }
}


/* Insert timer T into the list of active atimers `atimers', keeping
   the list sorted by expiration time.  T must not be in this list
   already.  */

static void
schedule_atimer (struct atimer *t)
{
  struct atimer *a = atimers, *prev = NULL;

  /* Look for the first atimer that is ripe after T.  */
  while (a && EMACS_TIME_GT (t->expiration, a->expiration))
    prev = a, a = a->next;

  /* Insert T in front of the atimer found, if any.  */
  if (prev)
    prev->next = t;
  else
    atimers = t;

  t->next = a;
}

static void
run_timers (void)
{
  EMACS_TIME now;

  EMACS_GET_TIME (now);

  while (atimers
	 && (pending_atimers = interrupt_input_blocked) == 0
	 && EMACS_TIME_LE (atimers->expiration, now))
    {
      struct atimer *t;

      t = atimers;
      atimers = atimers->next;
      t->fn (t);

      if (t->type == ATIMER_CONTINUOUS)
	{
	  EMACS_ADD_TIME (t->expiration, now, t->interval);
	  schedule_atimer (t);
	}
      else
	{
	  t->next = free_atimers;
	  free_atimers = t;
	}

      EMACS_GET_TIME (now);
    }

  if (! atimers)
    pending_atimers = 0;

#ifdef SYNC_INPUT
  if (pending_atimers)
    pending_signals = 1;
  else
    {
      pending_signals = interrupt_input_pending;
      set_alarm ();
    }
#else
  if (! pending_atimers)
    set_alarm ();
#endif
}


/* Signal handler for SIGALRM.  SIGNO is the signal number, i.e.
   SIGALRM.  */

void
alarm_signal_handler (int signo)
{
#ifndef SYNC_INPUT
  SIGNAL_THREAD_CHECK (signo);
#endif

  pending_atimers = 1;
#ifdef SYNC_INPUT
  pending_signals = 1;
#else
  run_timers ();
#endif
}


/* Call alarm_signal_handler for pending timers.  */

void
do_pending_atimers (void)
{
  if (pending_atimers)
    {
      BLOCK_ATIMERS;
      run_timers ();
      UNBLOCK_ATIMERS;
    }
}


/* Turn alarms on/off.  This seems to be temporarily necessary on
   some systems like HPUX (see process.c).  */

void
turn_on_atimers (int on)
{
  if (on)
    {
      signal (SIGALRM, alarm_signal_handler);
      set_alarm ();
    }
  else
    alarm (0);
}


void
init_atimer (void)
{
  free_atimers = stopped_atimers = atimers = NULL;
  pending_atimers = 0;
  /* pending_signals is initialized in init_keyboard.*/
  signal (SIGALRM, alarm_signal_handler);
}
