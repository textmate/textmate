/* System description header file for Darwin (Mac OS X).

Copyright (C) 2001-2012  Free Software Foundation, Inc.

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


/* Define symbols to identify the version of Unix this is.
   Define all the symbols that apply correctly.  */
#define BSD4_2
/* BSD4_3 and BSD4_4 are already defined in sys/param.h */
#define BSD_SYSTEM

/* More specific than the above two.  We cannot use __APPLE__ as this
   may not be defined on non-OSX Darwin, and we cannot define DARWIN
   here because Panther and lower CoreFoundation.h uses DARWIN to
   distinguish OS X from pure Darwin.  */
#define DARWIN_OS


/* SYSTEM_TYPE should indicate the kind of system you are using.
   It sets the Lisp variable system-type.  */
#define SYSTEM_TYPE "darwin"

/* Emacs can read input using SIGIO and buffering characters itself,
   or using CBREAK mode and making C-g cause SIGINT.
   The choice is controlled by the variable interrupt_input.

   Define INTERRUPT_INPUT to make interrupt_input = 1 the default (use SIGIO)

   Emacs uses the presence or absence of the SIGIO and BROKEN_SIGIO macros
   to indicate whether or not signal-driven I/O is possible.  It uses
   INTERRUPT_INPUT to decide whether to use it by default.

   SIGIO can be used only on systems that implement it (4.2 and 4.3).
   CBREAK mode has two disadvantages
     1) At least in 4.2, it is impossible to handle the Meta key properly.
        I hear that in system V this problem does not exist.
     2) Control-G causes output to be discarded.
        I do not know whether this can be fixed in system V.

   Another method of doing input is planned but not implemented.
   It would have Emacs fork off a separate process
   to read the input and send it to the true Emacs process
   through a pipe. */
#define INTERRUPT_INPUT

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */
#define FIRST_PTY_LETTER 'p'

#define NO_TERMIO

/* Define HAVE_PTYS if the system supports pty devices.
   Note: PTYs are broken on darwin <6.  Use at your own risk.  */
#define HAVE_PTYS
/* Run only once.  We need a `for'-loop because the code uses `continue'.  */
#define PTY_ITERATION	int i; for (i = 0; i < 1; i++)
#define PTY_NAME_SPRINTF	/* none */
#define PTY_TTY_NAME_SPRINTF	/* none */
/* Note that openpty may fork via grantpt on Mac OS X 10.4/Darwin 8.
   But we don't have to block SIGCHLD because it is blocked in the
   implementation of grantpt.  */
#define PTY_OPEN						\
  do								\
    {								\
      int slave;						\
      if (openpty (&fd, &slave, pty_name, NULL, NULL) == -1)	\
	fd = -1;						\
      else							\
	emacs_close (slave);					\
    }								\
  while (0)

/* PTYs only work correctly on Darwin 7 or higher.  So make the default
   for process-connection-type dependent on the kernel version.  */
#define MIN_PTY_KERNEL_VERSION '7'

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */
#define CLASH_DETECTION

/* Avoid the use of the name init_process (process.c) because it is
   also the name of a Mach system call.  */
#define init_process emacs_init_process

/* Used in dispnew.c.  Copied from freebsd.h. */
#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

/* System uses OXTABS instead of the expected TAB3.  (Copied from bsd386.h.)  */
#define TAB3 OXTABS

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */
#define HAVE_SOCKETS

/* Definitions for how to compile & link.  */
#ifdef HAVE_NS
#define SYSTEM_PURESIZE_EXTRA 200000
#endif

/* On Darwin, res_init appears not to be useful: see bug#562 and
   http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01467.html  */
#undef HAVE_RES_INIT
#undef HAVE_LIBRESOLV

#ifdef emacs
#define malloc unexec_malloc
#define realloc unexec_realloc
#define free unexec_free
/* Don't use posix_memalign because it is not compatible with unexmacosx.c.  */
#undef HAVE_POSIX_MEMALIGN
#endif

/* Define the following so emacs symbols will not conflict with those
   in the System framework.  Otherwise -prebind will not work.  */

/* Do not define abort in emacs.c.  */
#define NO_ABORT

/* Do not define matherr in floatfns.c.  */
#define NO_MATHERR

/* The following solves the problem that Emacs hangs when evaluating
   (make-comint "test0" "/nodir/nofile" nil "") when /nodir/nofile
   does not exist.  Also, setsid is not allowed in the vfork child's
   context as of Darwin 9/Mac OS X 10.5.  */
#undef HAVE_WORKING_VFORK
#define vfork fork

/* Don't close pty in process.c to make it as controlling terminal.
   It is already a controlling terminal of subprocess, because we did
   ioctl TIOCSCTTY.  */
#define DONT_REOPEN_PTY

/* Use the GC_MAKE_GCPROS_NOOPS (see lisp.h) method for marking the stack.  */
#define GC_MARK_STACK   GC_MAKE_GCPROS_NOOPS
