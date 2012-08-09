/* Heap management routines (including unexec) for GNU Emacs on Windows NT.
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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

   Geoff Voelker (voelker@cs.washington.edu)			     7-29-94
*/

#ifndef NTHEAP_H_
#define NTHEAP_H_

#include <windows.h>

#define ROUND_UP(p, align)   (((DWORD)(p) + (align)-1) & ~((align)-1))
#define ROUND_DOWN(p, align) ((DWORD)(p) & ~((align)-1))

/*
 * Heap related stuff.
 */
#define get_reserved_heap_size()	reserved_heap_size
#define get_committed_heap_size()	(get_data_end () - get_data_start ())
#define get_heap_start()		get_data_start ()
#define get_heap_end()			get_data_end ()
#define get_page_size()			sysinfo_cache.dwPageSize
#define get_allocation_unit()		sysinfo_cache.dwAllocationGranularity
#define get_processor_type()		sysinfo_cache.dwProcessorType
#define get_w32_major_version()  	w32_major_version
#define get_w32_minor_version()  	w32_minor_version

extern unsigned char *get_data_start (void);
extern unsigned char *get_data_end (void);
extern unsigned long  reserved_heap_size;
extern SYSTEM_INFO    sysinfo_cache;
extern OSVERSIONINFO  osinfo_cache;
extern BOOL   	      using_dynamic_heap;
extern int    	      w32_major_version;
extern int    	      w32_minor_version;
extern int    	      w32_build_number;

enum {
  OS_WIN95 = 1,
  OS_NT
};

extern int os_subtype;

/* Emulation of Unix sbrk().  */
extern void *sbrk (unsigned long size);

/* Initialize heap structures for sbrk on startup.  */
extern void init_heap (void);

/* Round the heap to this size.  */
extern void round_heap (unsigned long size);

/* Cache system info, e.g., the NT page size.  */
extern void cache_system_info (void);

/* ----------------------------------------------------------------- */
/* Useful routines for manipulating memory-mapped files. */

typedef struct file_data {
    char          *name;
    unsigned long  size;
    HANDLE         file;
    HANDLE         file_mapping;
    unsigned char *file_base;
} file_data;

int open_input_file (file_data *p_file, char *name);
int open_output_file (file_data *p_file, char *name, unsigned long size);
void close_file_data (file_data *p_file);

/* Return pointer to section header for named section. */
IMAGE_SECTION_HEADER * find_section (char * name, IMAGE_NT_HEADERS * nt_header);

/* Return pointer to section header for section containing the given
   relative virtual address. */
IMAGE_SECTION_HEADER * rva_to_section (DWORD rva, IMAGE_NT_HEADERS * nt_header);

#endif /* NTHEAP_H_ */

