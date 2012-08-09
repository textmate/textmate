/* This file is the configuration file for Linux-based GNU systems

Copyright (C) 1985-1986, 1992, 1994, 1996, 1999, 2001-2012
  Free Software Foundation, Inc.

This file was put together by Michael K. Johnson and Rik Faith.

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
#define USG
#define GNU_LINUX

/* SYSTEM_TYPE should indicate the kind of system you are using.
   It sets the Lisp variable system-type.  */
#define SYSTEM_TYPE "gnu/linux"		/* All the best software is free. */

#ifdef emacs
#ifdef HAVE_LINUX_VERSION_H
#include <linux/version.h>

#if LINUX_VERSION_CODE >= 0x20400
/* 21 Jun 06: Eric Hanchrow <offby1@blarg.net> says this works.  */
#define SIGNALS_VIA_CHARACTERS
#endif /* LINUX_VERSION_CODE >= 0x20400 */
#endif /* HAVE_LINUX_VERSION_H */
#endif /* emacs */

#if defined HAVE_GRANTPT
#define UNIX98_PTYS

/* Run only once.  We need a `for'-loop because the code uses `continue'.  */
#define PTY_ITERATION	int i; for (i = 0; i < 1; i++)

#ifdef HAVE_GETPT
#define PTY_NAME_SPRINTF
#define PTY_OPEN fd = getpt ()
#else /* not HAVE_GETPT */
#define PTY_NAME_SPRINTF strcpy (pty_name, "/dev/ptmx");
#endif /* not HAVE_GETPT */

/* Note that grantpt and unlockpt may fork.  We must block SIGCHLD to
   prevent sigchld_handler from intercepting the child's death.  */
#define PTY_TTY_NAME_SPRINTF				\
  {							\
    char *ptyname;					\
							\
    sigblock (sigmask (SIGCHLD));			\
    if (grantpt (fd) == -1 || unlockpt (fd) == -1	\
        || !(ptyname = ptsname(fd)))			\
      {							\
	sigunblock (sigmask (SIGCHLD));			\
	close (fd);					\
	return -1;					\
      }							\
    strncpy (pty_name, ptyname, sizeof (pty_name));	\
    pty_name[sizeof (pty_name) - 1] = 0;		\
    sigunblock (sigmask (SIGCHLD));			\
  }

#else /* not HAVE_GRANTPT */

/* Letter to use in finding device name of first pty,
   if system supports pty's.  'p' means it is /dev/ptyp0  */
#define FIRST_PTY_LETTER 'p'

#endif  /* not HAVE_GRANTPT */

/* Define HAVE_PTYS if the system supports pty devices.  */
#define HAVE_PTYS

#define HAVE_SOCKETS

/* This is used in list_system_processes.  */
#define HAVE_PROCFS 1

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */
#define CLASH_DETECTION

/* Here, on a separate page, add any special hacks needed
   to make Emacs work on this system.  For example,
   you might define certain system call names that don't
   exist on your system, or that do different things on
   your system and must be used only through an encapsulation
   (Which you should place, by convention, in sysdep.c).  */

/* This is needed for dispnew.c:update_frame.  */
#ifdef emacs
#include <stdio.h>  /* Get the definition of _IO_STDIO_H.  */
#if defined (_IO_STDIO_H) || defined (_STDIO_USES_IOSTREAM)
/* New C libio names.  */
#define GNU_LIBRARY_PENDING_OUTPUT_COUNT(FILE) \
  ((FILE)->_IO_write_ptr - (FILE)->_IO_write_base)
#elif defined (__UCLIBC__)
/* Using the uClibc library.  */
#define GNU_LIBRARY_PENDING_OUTPUT_COUNT(FILE) \
  ((FILE)->__bufpos - (FILE)->__bufstart)
#else /* !_IO_STDIO_H && ! __UCLIBC__ */
/* Old C++ iostream names.  */
#define GNU_LIBRARY_PENDING_OUTPUT_COUNT(FILE) \
  ((FILE)->_pptr - (FILE)->_pbase)
#endif /* !_IO_STDIO_H && ! __UCLIBC__ */

#define INTERRUPT_INPUT
#endif /* emacs */

#define POSIX                 /* affects getpagesize.h and systty.h */

/* This is to work around mysterious gcc failures in some system versions.
   It is unlikely that Emacs changes will work around this problem;
   therefore, this should remain permanently.  */
#ifndef HAVE_XRMSETDATABASE
#define HAVE_XRMSETDATABASE
#endif

#define NARROWPROTO 1

/* Tell that garbage collector that setjmp is known to save all
   registers relevant for conservative garbage collection in the jmp_buf.  */
/* Not all the architectures are tested, but there are Debian packages
   for SCM and/or Guile on them, so the technique must work.  See also
   comments in alloc.c concerning setjmp and gcc.  Fixme:  it's
   probably safe to make this conditional just on GCC, except for ia64
   register window-flushing.  */
/* Don't use #cpu here since in newest development versions of GCC,
   we must call cpp with -traditional, and that disables #cpu.  */
#if defined __i386__ || defined __sparc__ || defined __mc68000__ \
    || defined __alpha__ || defined __mips__ || defined __s390__ \
    || defined __arm__ || defined __powerpc__ || defined __amd64__ \
    || defined __ia64__ || defined __sh__
#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS
#ifdef __mc68000__
#define GC_LISP_OBJECT_ALIGNMENT 2
#endif
#ifdef __ia64__
#define GC_MARK_SECONDARY_STACK()				\
  do {								\
    extern void *__libc_ia64_register_backing_store_base;	\
    __builtin_ia64_flushrs ();					\
    mark_memory (__libc_ia64_register_backing_store_base,	\
		 __builtin_ia64_bsp ());			\
  } while (0)
#endif
#endif
