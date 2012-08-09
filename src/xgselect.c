/* Function for handling the GLib event loop.

Copyright (C) 2009-2012  Free Software Foundation, Inc.

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
along with GNU Emacs.  If not, see <http§://www.gnu.org/licenses/>.  */

#include <config.h>

#include <setjmp.h>
#include "xgselect.h"

#if defined (USE_GTK) || defined (HAVE_GCONF) || defined (HAVE_GSETTINGS)

#include <glib.h>
#include <errno.h>
#include <setjmp.h>

static GPollFD *gfds;
static ptrdiff_t gfds_size;

int
xg_select (int max_fds, SELECT_TYPE *rfds, SELECT_TYPE *wfds, SELECT_TYPE *efds,
	   EMACS_TIME *timeout)
{
  SELECT_TYPE all_rfds, all_wfds;
  EMACS_TIME tmo, *tmop = timeout;

  GMainContext *context = g_main_context_default ();
  int have_wfds = wfds != NULL;
  int n_gfds = 0, our_tmo = 0, retval = 0, our_fds = 0;
  int i, nfds, tmo_in_millisec;

  if (rfds) memcpy (&all_rfds, rfds, sizeof (all_rfds));
  else FD_ZERO (&all_rfds);
  if (wfds) memcpy (&all_wfds, wfds, sizeof (all_rfds));
  else FD_ZERO (&all_wfds);

  /* Update event sources in GLib. */
  g_main_context_pending (context);

  do {
    if (n_gfds > gfds_size)
      {
        xfree (gfds);
	gfds = xpalloc (0, &gfds_size, n_gfds - gfds_size, INT_MAX,
			sizeof *gfds);
      }

    n_gfds = g_main_context_query (context,
                                   G_PRIORITY_LOW,
                                   &tmo_in_millisec,
                                   gfds,
                                   gfds_size);
  } while (n_gfds > gfds_size);

  for (i = 0; i < n_gfds; ++i)
    {
      if (gfds[i].events & G_IO_IN)
        {
          FD_SET (gfds[i].fd, &all_rfds);
          if (gfds[i].fd > max_fds) max_fds = gfds[i].fd;
        }
      if (gfds[i].events & G_IO_OUT)
        {
          FD_SET (gfds[i].fd, &all_wfds);
          if (gfds[i].fd > max_fds) max_fds = gfds[i].fd;
          have_wfds = 1;
        }
    }

  if (tmo_in_millisec >= 0)
    {
      EMACS_SET_SECS_USECS (tmo, tmo_in_millisec/1000,
                            1000 * (tmo_in_millisec % 1000));
      if (!timeout) our_tmo = 1;
      else
        {
          EMACS_TIME difference;

          EMACS_SUB_TIME (difference, tmo, *timeout);
          if (EMACS_TIME_NEG_P (difference)) our_tmo = 1;
        }

      if (our_tmo) tmop = &tmo;
    }

  nfds = select (max_fds+1, &all_rfds, have_wfds ? &all_wfds : NULL,
                 efds, tmop);

  if (nfds < 0)
    retval = nfds;
  else if (nfds > 0)
    {
      for (i = 0; i < max_fds+1; ++i)
        {
          if (FD_ISSET (i, &all_rfds))
            {
              if (rfds && FD_ISSET (i, rfds)) ++retval;
              else ++our_fds;
            }
          else if (rfds)
            FD_CLR (i, rfds);

          if (have_wfds && FD_ISSET (i, &all_wfds))
            {
              if (wfds && FD_ISSET (i, wfds)) ++retval;
              else ++our_fds;
            }
          else if (wfds)
            FD_CLR (i, wfds);

          if (efds && FD_ISSET (i, efds))
            ++retval;
        }
    }

  if (our_fds > 0 || (nfds == 0 && our_tmo))
    {

      /* If Gtk+ is in use eventually gtk_main_iteration will be called,
         unless retval is zero.  */
#ifdef USE_GTK
      if (retval == 0)
#endif
        while (g_main_context_pending (context))
          g_main_context_dispatch (context);

      /* To not have to recalculate timeout, return like this.  */
      if (retval == 0)
        {
          retval = -1;
          errno = EINTR;
        }
    }

  return retval;
}
#endif /* USE_GTK || HAVE_GCONF || HAVE_GSETTINGS */

void
xgselect_initialize (void)
{
#if defined (USE_GTK) || defined (HAVE_GCONF) || defined (HAVE_GSETTINGS)
  gfds_size = 128;
  gfds = xmalloc (sizeof (*gfds)*gfds_size);
#endif
}
