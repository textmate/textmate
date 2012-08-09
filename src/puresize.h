/* How much read-only Lisp storage a dumped Emacs needs.
   Copyright (C) 1993, 2001-2012  Free Software Foundation, Inc.

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

/* Define PURESIZE, the number of bytes of pure Lisp code to leave space for.

   At one point, this was defined in config.h, meaning that changing
   PURESIZE would make Make recompile all of Emacs.  But only a few
   files actually use PURESIZE, so we split it out to its own .h file.

   Make sure to include this file after config.h, since that tells us
   whether we are running X windows, which tells us how much pure
   storage to allocate.  */

/* First define a measure of the amount of data we have.  */

/* A system configuration file may set this to request a certain extra
   amount of storage.  This is a lot more update-robust that defining
   BASE_PURESIZE or even PURESIZE directly.  */
#ifndef SYSTEM_PURESIZE_EXTRA
#define SYSTEM_PURESIZE_EXTRA 0
#endif

#ifndef SITELOAD_PURESIZE_EXTRA
#define SITELOAD_PURESIZE_EXTRA 0
#endif

#ifndef BASE_PURESIZE
#define BASE_PURESIZE (1620000 + SYSTEM_PURESIZE_EXTRA + SITELOAD_PURESIZE_EXTRA)
#endif

/* Increase BASE_PURESIZE by a ratio depending on the machine's word size.  */
#ifndef PURESIZE_RATIO
#if BITS_PER_EMACS_INT > 32
#define PURESIZE_RATIO 10/6	/* Don't surround with `()'. */
#else
#define PURESIZE_RATIO 1
#endif
#endif

#ifdef ENABLE_CHECKING
/* ENABLE_CHECKING somehow increases the purespace used, probably because
   it tends to cause some macro arguments to be evaluated twice.  This is
   a bug, but it's difficult to track it down.  */
#define PURESIZE_CHECKING_RATIO 12/10	/* Don't surround with `()'. */
#else
#define PURESIZE_CHECKING_RATIO 1
#endif

/* This is the actual size in bytes to allocate.  */
#ifndef PURESIZE
#define PURESIZE  (BASE_PURESIZE * PURESIZE_RATIO * PURESIZE_CHECKING_RATIO)
#endif

/* Signal an error if OBJ is pure.  */
#define CHECK_IMPURE(obj) \
  { if (PURE_P (obj))	  \
      pure_write_error (); }

extern void pure_write_error (void) NO_RETURN;

/* Define PURE_P.  */

extern EMACS_INT pure[];

#define PURE_P(obj) \
  ((uintptr_t) XPNTR (obj) - (uintptr_t) pure <= PURESIZE)
