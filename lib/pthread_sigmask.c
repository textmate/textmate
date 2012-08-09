/* POSIX compatible signal blocking for threads.
   Copyright (C) 2011 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

/* Specification.  */
#include <signal.h>

#include <errno.h>
#include <stddef.h>

#if PTHREAD_SIGMASK_UNBLOCK_BUG
# include <unistd.h>
#endif

int
pthread_sigmask (int how, const sigset_t *new_mask, sigset_t *old_mask)
#undef pthread_sigmask
{
#if HAVE_PTHREAD_SIGMASK
  int ret = pthread_sigmask (how, new_mask, old_mask);
# if PTHREAD_SIGMASK_INEFFECTIVE
  if (ret == 0)
    {
      /* Detect whether pthread_sigmask is currently ineffective.
         Don't cache the information: libpthread.so could be dynamically
         loaded after the program started and after pthread_sigmask was
         called for the first time.  */
      if (pthread_sigmask (1729, NULL, NULL) == 0)
        {
          /* pthread_sigmask is currently ineffective.  The program is not
             linked to -lpthread.  So use sigprocmask instead.  */
          return (sigprocmask (how, new_mask, old_mask) < 0 ? errno : 0);
        }
    }
# endif
# if PTHREAD_SIGMASK_FAILS_WITH_ERRNO
  if (ret == -1)
    return errno;
# endif
# if PTHREAD_SIGMASK_UNBLOCK_BUG
  if (ret == 0
      && new_mask != NULL
      && (how == SIG_UNBLOCK || how == SIG_SETMASK))
    {
      /* Give the OS the opportunity to raise signals that were pending before
         the pthread_sigmask call and have now been unblocked.  */
      usleep (1);
    }
# endif
  return ret;
#else
  int ret = sigprocmask (how, new_mask, old_mask);
  return (ret < 0 ? errno : 0);
#endif
}
