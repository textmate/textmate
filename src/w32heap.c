/* Heap management routines for GNU Emacs on the Microsoft W32 API.
   Copyright (C) 1994, 2001-2012  Free Software Foundation, Inc.

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

/*
   Geoff Voelker (voelker@cs.washington.edu)			     7-29-94
*/

#include <config.h>
#include <stdio.h>
#include <setjmp.h>

#include "w32heap.h"
#include "lisp.h"  /* for VALMASK */

#define RVA_TO_PTR(rva) ((unsigned char *)((DWORD)(rva) + (DWORD)GetModuleHandle (NULL)))

/* This gives us the page size and the size of the allocation unit on NT.  */
SYSTEM_INFO sysinfo_cache;

/* This gives us version, build, and platform identification.  */
OSVERSIONINFO osinfo_cache;

unsigned long syspage_mask = 0;

/* The major and minor versions of NT.  */
int w32_major_version;
int w32_minor_version;
int w32_build_number;

/* Distinguish between Windows NT and Windows 95.  */
int os_subtype;

/* Cache information describing the NT system for later use.  */
void
cache_system_info (void)
{
  union
    {
      struct info
	{
	  char  major;
	  char  minor;
	  short platform;
	} info;
      DWORD data;
    } version;

  /* Cache the version of the operating system.  */
  version.data = GetVersion ();
  w32_major_version = version.info.major;
  w32_minor_version = version.info.minor;

  if (version.info.platform & 0x8000)
    os_subtype = OS_WIN95;
  else
    os_subtype = OS_NT;

  /* Cache page size, allocation unit, processor type, etc.  */
  GetSystemInfo (&sysinfo_cache);
  syspage_mask = sysinfo_cache.dwPageSize - 1;

  /* Cache os info.  */
  osinfo_cache.dwOSVersionInfoSize = sizeof (OSVERSIONINFO);
  GetVersionEx (&osinfo_cache);

  w32_build_number = osinfo_cache.dwBuildNumber;
  if (os_subtype == OS_WIN95)
    w32_build_number &= 0xffff;
}

/* Emulate getpagesize.  */
int
getpagesize (void)
{
  return sysinfo_cache.dwPageSize;
}

/* Info for managing our preload heap, which is essentially a fixed size
   data area in the executable.  */
PIMAGE_SECTION_HEADER preload_heap_section;

/* Info for keeping track of our heap.  */
unsigned char *data_region_base = NULL;
unsigned char *data_region_end = NULL;
unsigned char *real_data_region_end = NULL;
unsigned long  reserved_heap_size = 0;

/* The start of the data segment.  */
unsigned char *
get_data_start (void)
{
  return data_region_base;
}

/* The end of the data segment.  */
unsigned char *
get_data_end (void)
{
  return data_region_end;
}

#if !defined (USE_LISP_UNION_TYPE) && !defined (USE_LSB_TAG)
static char *
allocate_heap (void)
{
  /* Try to get as much as possible of the address range from the end of
     the preload heap section up to the usable address limit.  Since GNU
     malloc can handle gaps in the memory it gets from sbrk, we can
     simply set the sbrk pointer to the base of the new heap region.  */
  unsigned long base =
    ROUND_UP ((RVA_TO_PTR (preload_heap_section->VirtualAddress)
	       + preload_heap_section->Misc.VirtualSize),
	      get_allocation_unit ());
  unsigned long end  = 1 << VALBITS; /* 256MB */
  void *ptr = NULL;

  while (!ptr && (base < end))
    {
      reserved_heap_size = end - base;
      ptr = VirtualAlloc ((void *) base,
			  get_reserved_heap_size (),
			  MEM_RESERVE,
			  PAGE_NOACCESS);
      base += 0x00100000;  /* 1MB increment */
    }

  return ptr;
}
#else  /* USE_LISP_UNION_TYPE || USE_LSB_TAG */
static char *
allocate_heap (void)
{
  unsigned long size = 0x80000000; /* start by asking for 2GB */
  void *ptr = NULL;

  while (!ptr && size > 0x00100000)
    {
      reserved_heap_size = size;
      ptr = VirtualAlloc (NULL,
			  get_reserved_heap_size (),
			  MEM_RESERVE,
			  PAGE_NOACCESS);
      size -= 0x00800000; /* if failed, decrease request by 8MB */
    }

  return ptr;
}
#endif /* USE_LISP_UNION_TYPE || USE_LSB_TAG */


/* Emulate Unix sbrk.  Note that ralloc.c expects the return value to
   be the address of the _start_ (not end) of the new block in case of
   success, and zero (not -1) in case of failure.  */
void *
sbrk (unsigned long increment)
{
  void *result;
  long size = (long) increment;

  result = data_region_end;

  /* If size is negative, shrink the heap by decommitting pages.  */
  if (size < 0)
    {
      int new_size;
      unsigned char *new_data_region_end;

      size = -size;

      /* Sanity checks.  */
      if ((data_region_end - size) < data_region_base)
	return NULL;

      /* We can only decommit full pages, so allow for
	 partial deallocation [cga].  */
      new_data_region_end = (data_region_end - size);
      new_data_region_end = (unsigned char *)
	((long) (new_data_region_end + syspage_mask) & ~syspage_mask);
      new_size = real_data_region_end - new_data_region_end;
      real_data_region_end = new_data_region_end;
      if (new_size > 0)
	{
	  /* Decommit size bytes from the end of the heap.  */
	  if (using_dynamic_heap
	      && !VirtualFree (real_data_region_end, new_size, MEM_DECOMMIT))
	    return NULL;
 	}

      data_region_end -= size;
    }
  /* If size is positive, grow the heap by committing reserved pages.  */
  else if (size > 0)
    {
      /* Sanity checks.  */
      if ((data_region_end + size) >
	  (data_region_base + get_reserved_heap_size ()))
	return NULL;

      /* Commit more of our heap. */
      if (using_dynamic_heap
	  && VirtualAlloc (data_region_end, size, MEM_COMMIT,
			   PAGE_READWRITE) == NULL)
	return NULL;
      data_region_end += size;

      /* We really only commit full pages, so record where
	 the real end of committed memory is [cga].  */
      real_data_region_end = (unsigned char *)
	  ((long) (data_region_end + syspage_mask) & ~syspage_mask);
    }

  return result;
}

/* Initialize the internal heap variables used by sbrk.  When running in
   preload phase (ie. in the undumped executable), we rely entirely on a
   fixed size heap section included in the .exe itself; this is
   preserved during dumping, and truncated to the size actually used.

   When running in the dumped executable, we reserve as much as possible
   of the address range that is addressable by Lisp object pointers, to
   supplement what is left of the preload heap.  Although we cannot rely
   on the dynamically allocated arena being contiguous with the static
   heap area, it is not a problem because sbrk can pretend that the gap
   was allocated by something else; GNU malloc detects when there is a
   jump in the sbrk values, and starts a new heap block.  */
void
init_heap (void)
{
  PIMAGE_DOS_HEADER dos_header;
  PIMAGE_NT_HEADERS nt_header;

  dos_header = (PIMAGE_DOS_HEADER) RVA_TO_PTR (0);
  nt_header = (PIMAGE_NT_HEADERS) (((unsigned long) dos_header) +
				   dos_header->e_lfanew);
  preload_heap_section = find_section ("EMHEAP", nt_header);

  if (using_dynamic_heap)
    {
      data_region_base = allocate_heap ();
      if (!data_region_base)
	{
	  printf ("Error: Could not reserve dynamic heap area.\n");
	  exit (1);
	}

#if !defined (USE_LISP_UNION_TYPE) && !defined (USE_LSB_TAG)
      /* Ensure that the addresses don't use the upper tag bits since
	 the Lisp type goes there.  */
      if (((unsigned long) data_region_base & ~VALMASK) != 0)
	{
	  printf ("Error: The heap was allocated in upper memory.\n");
	  exit (1);
	}
#endif
      data_region_end = data_region_base;
      real_data_region_end = data_region_end;
    }
  else
    {
      data_region_base = RVA_TO_PTR (preload_heap_section->VirtualAddress);
      data_region_end = data_region_base;
      real_data_region_end = data_region_end;
      reserved_heap_size = preload_heap_section->Misc.VirtualSize;
    }

  /* Update system version information to match current system.  */
  cache_system_info ();
}

/* Round the heap up to the given alignment.  */
void
round_heap (unsigned long align)
{
  unsigned long needs_to_be;
  unsigned long need_to_alloc;

  needs_to_be = (unsigned long) ROUND_UP (get_heap_end (), align);
  need_to_alloc = needs_to_be - (unsigned long) get_heap_end ();

  if (need_to_alloc)
    sbrk (need_to_alloc);
}
