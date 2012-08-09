/* simulate `sbrk' with an array in .bss, for `unexec' support for Cygwin;
   complete rewrite of xemacs Cygwin `unexec' code

   Copyright (C) 2004-2012 Free Software Foundation, Inc.

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

#include <config.h>
#include <stdio.h>
#include <setjmp.h>
#include "lisp.h"

#include <unistd.h>

#define STATIC_HEAP_SIZE	(13 * 1024 * 1024)

int debug_sheap = 0;

#define BLOCKSIZE 4096

char bss_sbrk_buffer[STATIC_HEAP_SIZE];
char *bss_sbrk_ptr;
int bss_sbrk_did_unexec;

void *
bss_sbrk (ptrdiff_t request_size)
{
  if (!bss_sbrk_ptr)
    {
      bss_sbrk_ptr = bss_sbrk_buffer;
#ifdef CYGWIN
      sbrk (BLOCKSIZE);		/* force space for fork to work */
#endif
    }

  if (!(int) request_size)
    {
      return (bss_sbrk_ptr);
    }
  else if (bss_sbrk_ptr + (int) request_size < bss_sbrk_buffer)
    {
      printf
	("attempt to free too much: avail %d used %d failed request %d\n",
	 STATIC_HEAP_SIZE, bss_sbrk_ptr - bss_sbrk_buffer,
	 (int) request_size);
      exit (-1);
      return 0;
    }
  else if (bss_sbrk_ptr + (int) request_size >
	   bss_sbrk_buffer + STATIC_HEAP_SIZE)
    {
      printf ("static heap exhausted: avail %d used %d failed request %d\n",
	      STATIC_HEAP_SIZE,
	      bss_sbrk_ptr - bss_sbrk_buffer, (int) request_size);
      exit (-1);
      return 0;
    }
  else if ((int) request_size < 0)
    {
      bss_sbrk_ptr += (int) request_size;
      if (debug_sheap)
	printf ("freed size %d\n", request_size);
      return bss_sbrk_ptr;
    }
  else
    {
      char *ret = bss_sbrk_ptr;
      if (debug_sheap)
	printf ("allocated 0x%08x size %d\n", ret, request_size);
      bss_sbrk_ptr += (int) request_size;
      return ret;
    }
}

void
report_sheap_usage (int die_if_pure_storage_exceeded)
{
  char buf[200];
  sprintf (buf, "Static heap usage: %d of %d bytes",
	   bss_sbrk_ptr - bss_sbrk_buffer, STATIC_HEAP_SIZE);
  message ("%s", buf);
}

