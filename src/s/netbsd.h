/* s/ file for netbsd system.

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


/* Get most of the stuff from bsd-common.  */
#include "bsd-common.h"

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)

#define DEFAULT_SOUND_DEVICE "/dev/audio"

/* Greg A. Woods <woods@weird.com> says we must include signal.h
   before syssignal.h is included, to work around interface conflicts
   that are handled with CPP __RENAME() macro in signal.h.  */
#include <signal.h>

/* Don't close pty in process.c to make it as controlling terminal.
   It is already a controlling terminal of subprocess, because we did
   ioctl TIOCSCTTY.  */
#define DONT_REOPEN_PTY

/* Tell that garbage collector that setjmp is known to save all
   registers relevant for conservative garbage collection in the jmp_buf.  */
#define GC_SETJMP_WORKS 1

/* Use the GC_MAKE_GCPROS_NOOPS (see lisp.h) method.  */
#define GC_MARK_STACK	GC_MAKE_GCPROS_NOOPS
