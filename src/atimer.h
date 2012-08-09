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

#ifndef EMACS_ATIMER_H
#define EMACS_ATIMER_H

#include "systime.h"		/* for EMACS_TIME */

/* Forward declaration.  */

struct atimer;

/* Types of timers.  */

enum atimer_type
{
  /* Timer is ripe at some absolute time.  */
  ATIMER_ABSOLUTE,

  /* Timer is ripe at now plus an offset.  */
  ATIMER_RELATIVE,

  /* Timer runs continuously.  */
  ATIMER_CONTINUOUS
};

/* Type of timer callback functions.  */

typedef void (* atimer_callback) (struct atimer *timer);

/* Structure describing an asynchronous timer.  */

struct atimer
{
  /* The type of this timer.  */
  enum atimer_type type;

  /* Time when this timer is ripe.  */
  EMACS_TIME expiration;

  /* Interval of this timer.  */
  EMACS_TIME interval;

  /* Function to call when timer is ripe.  Interrupt input is
     guaranteed to not be blocked when this function is called.  */
  atimer_callback fn;

  /* Additional user-specified data to pass to FN.  */
  void *client_data;

  /* Next in list of active or free atimers.  */
  struct atimer *next;
};

/* Function prototypes.  */

struct atimer *start_atimer (enum atimer_type, EMACS_TIME,
                             atimer_callback, void *);
void cancel_atimer (struct atimer *);
void do_pending_atimers (void);
void init_atimer (void);
void turn_on_atimers (int);
void stop_other_atimers (struct atimer *);
Lisp_Object unwind_stop_other_atimers (Lisp_Object);

#endif /* EMACS_ATIMER_H */
