/*
Copyright (C) 1993-1998, 2001-2012  Free Software Foundation, Inc.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* This file implements the emacs_insque and emacs_remque functions,
   clones of the insque and remque functions of BSD.  They and all
   their callers have been renamed to emacs_mumble to allow us to
   include this file in the menu library on all systems.  */

#include "XMenuInt.h"

struct qelem {
  struct    qelem *q_forw;
  struct    qelem *q_back;
  char q_data[1];
};

/* Insert ELEM into a doubly-linked list, after PREV.  */

void
emacs_insque (void *velem, void *vprev)
{
  struct qelem *elem = velem;
  struct qelem *prev = vprev;
  struct qelem *next = prev->q_forw;
  prev->q_forw = elem;
  if (next)
    next->q_back = elem;
  elem->q_forw = next;
  elem->q_back = prev;
}

/* Unlink ELEM from the doubly-linked list that it is in.  */

void
emacs_remque (void *velem)
{
  struct qelem *elem = velem;
  struct qelem *next = elem->q_forw;
  struct qelem *prev = elem->q_back;
  if (next)
    next->q_back = prev;
  if (prev)
    prev->q_forw = next;
}
