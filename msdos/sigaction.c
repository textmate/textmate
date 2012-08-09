/* sigaction.c
 *
 * Copyright (C) 1995 DJ Delorie
 *
 * (See the README file in this directory for the copyright and license
 * history of this file.)
 *
 * This file is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This file is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this file.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <signal.h>
#include <errno.h>

int
sigaction(int _sig, const struct sigaction *_act, struct sigaction *_oact)
{
  int retval = 0;

  if (_oact)
  {
    void (*installed_sig)(int) = signal (_sig, SIG_IGN);

    /* FIXME */
    if (installed_sig == SIG_ERR)
    {
      retval = -1;
      errno = EINVAL;
    }
    else
      signal (_sig, installed_sig);
    _oact->sa_handler = installed_sig;
    retval = sigemptyset (&_oact->sa_mask);
    _oact->sa_flags = 0;
  }
  if (_act)
  {
    if (signal (_sig, _act->sa_handler) == SIG_ERR)
    {
      retval = -1;
      errno = EINVAL;
    }
  }
  return 0;
}



