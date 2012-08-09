/* Define wait system call interface for Emacs.
   Copyright (C) 1993-1995, 2000-2012  Free Software Foundation, Inc.

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

/* Define the structure that the wait system call stores.
   On many systems, there is a structure defined for this.
   But on vanilla-ish USG systems there is not.  */

#ifndef EMACS_SYSWAIT_H
#define EMACS_SYSWAIT_H

#include <sys/types.h>

#ifdef HAVE_SYS_WAIT_H	/* We have sys/wait.h with POSIXoid definitions. */
#include <sys/wait.h>
#endif  /* !HAVE_SYS_WAIT_H */

#ifndef WCOREDUMP		/* not POSIX */
#define WCOREDUMP(status) ((status) & 0x80)
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(status) (((status)  & 0xff00) >> 8)
#endif
#ifndef WIFEXITED
#define WIFEXITED(status) (WTERMSIG(status) == 0)
#endif
#ifndef WIFSTOPPED
#define WIFSTOPPED(status) (((status) & 0xff) == 0x7f)
#endif
#ifndef WIFSIGNALED
#define WIFSIGNALED(status) (!WIFSTOPPED(status) && !WIFEXITED(status))
#endif
#ifndef WSTOPSIG
#define WSTOPSIG(status) WEXITSTATUS(status)
#endif
#ifndef WTERMSIG
#define WTERMSIG(status) ((status) & 0x7f)
#endif

#undef WRETCODE
#define WRETCODE(status) WEXITSTATUS (status)


#endif /* EMACS_SYSWAIT_H */

