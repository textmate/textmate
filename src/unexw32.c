/* unexec for GNU Emacs on Windows NT.
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
   Geoff Voelker (voelker@cs.washington.edu)                         8-12-94
*/

#include <config.h>
#include "unexec.h"

#include <stdio.h>
#include <fcntl.h>
#include <time.h>
#include <windows.h>

/* Include relevant definitions from IMAGEHLP.H, which can be found
   in \\win32sdk\mstools\samples\image\include\imagehlp.h. */

PIMAGE_NT_HEADERS
(__stdcall * pfnCheckSumMappedFile) (LPVOID BaseAddress,
				    DWORD FileLength,
				    LPDWORD HeaderSum,
				    LPDWORD CheckSum);

extern BOOL ctrl_c_handler (unsigned long type);

extern char my_begdata[];
extern char my_edata[];
extern char my_begbss[];
extern char my_endbss[];
extern char *my_begbss_static;
extern char *my_endbss_static;

#include "w32heap.h"

#undef min
#undef max
#define min(x, y) (((x) < (y)) ? (x) : (y))
#define max(x, y) (((x) > (y)) ? (x) : (y))

/* Basically, our "initialized" flag.  */
BOOL using_dynamic_heap = FALSE;

int open_input_file (file_data *p_file, char *name);
int open_output_file (file_data *p_file, char *name, unsigned long size);
void close_file_data (file_data *p_file);

void get_section_info (file_data *p_file);
void copy_executable_and_dump_data (file_data *, file_data *);
void dump_bss_and_heap (file_data *p_infile, file_data *p_outfile);

/* Cached info about the .data section in the executable.  */
PIMAGE_SECTION_HEADER data_section;
PCHAR  data_start = 0;
DWORD  data_size = 0;

/* Cached info about the .bss section in the executable.  */
PIMAGE_SECTION_HEADER bss_section;
PCHAR  bss_start = 0;
DWORD  bss_size = 0;
DWORD  extra_bss_size = 0;
/* bss data that is static might be discontiguous from non-static.  */
PIMAGE_SECTION_HEADER bss_section_static;
PCHAR  bss_start_static = 0;
DWORD  bss_size_static = 0;
DWORD  extra_bss_size_static = 0;

PIMAGE_SECTION_HEADER heap_section;

#ifdef HAVE_NTGUI
HINSTANCE hinst = NULL;
HINSTANCE hprevinst = NULL;
LPSTR lpCmdLine = "";
int nCmdShow = 0;
#endif /* HAVE_NTGUI */

/* Startup code for running on NT.  When we are running as the dumped
   version, we need to bootstrap our heap and .bss section into our
   address space before we can actually hand off control to the startup
   code supplied by NT (primarily because that code relies upon malloc ()).  */
void
_start (void)
{
  extern void mainCRTStartup (void);

#if 1
  /* Give us a way to debug problems with crashes on startup when
     running under the MSVC profiler. */
  if (GetEnvironmentVariable ("EMACS_DEBUG", NULL, 0) > 0)
    DebugBreak ();
#endif

  /* Cache system info, e.g., the NT page size.  */
  cache_system_info ();

  /* Grab our malloc arena space now, before CRT starts up. */
  init_heap ();

  /* This prevents ctrl-c's in shells running while we're suspended from
     having us exit.  */
  SetConsoleCtrlHandler ((PHANDLER_ROUTINE) ctrl_c_handler, TRUE);

  /* Prevent Emacs from being locked up (eg. in batch mode) when
     accessing devices that aren't mounted (eg. removable media drives).  */
  SetErrorMode (SEM_FAILCRITICALERRORS);

  /* Invoke the NT CRT startup routine now that our housecleaning
     is finished.  */
#ifdef HAVE_NTGUI
  /* determine WinMain args like crt0.c does */
  hinst = GetModuleHandle (NULL);
  lpCmdLine = GetCommandLine ();
  nCmdShow = SW_SHOWDEFAULT;
#endif
  mainCRTStartup ();
}


/* File handling.  */

int
open_input_file (file_data *p_file, char *filename)
{
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;
  unsigned long size, upper_size;

  file = CreateFile (filename, GENERIC_READ, FILE_SHARE_READ, NULL,
		     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE)
    return FALSE;

  size = GetFileSize (file, &upper_size);
  file_mapping = CreateFileMapping (file, NULL, PAGE_READONLY,
				    0, size, NULL);
  if (!file_mapping)
    return FALSE;

  file_base = MapViewOfFile (file_mapping, FILE_MAP_READ, 0, 0, size);
  if (file_base == 0)
    return FALSE;

  p_file->name = filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = file_base;

  return TRUE;
}

int
open_output_file (file_data *p_file, char *filename, unsigned long size)
{
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;

  file = CreateFile (filename, GENERIC_READ | GENERIC_WRITE, 0, NULL,
		     CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE)
    return FALSE;

  file_mapping = CreateFileMapping (file, NULL, PAGE_READWRITE,
				    0, size, NULL);
  if (!file_mapping)
    return FALSE;

  file_base = MapViewOfFile (file_mapping, FILE_MAP_WRITE, 0, 0, size);
  if (file_base == 0)
    return FALSE;

  p_file->name = filename;
  p_file->size = size;
  p_file->file = file;
  p_file->file_mapping = file_mapping;
  p_file->file_base = file_base;

  return TRUE;
}

/* Close the system structures associated with the given file.  */
void
close_file_data (file_data *p_file)
{
  UnmapViewOfFile (p_file->file_base);
  CloseHandle (p_file->file_mapping);
  /* For the case of output files, set final size.  */
  SetFilePointer (p_file->file, p_file->size, NULL, FILE_BEGIN);
  SetEndOfFile (p_file->file);
  CloseHandle (p_file->file);
}


/* Routines to manipulate NT executable file sections.  */

/* Return pointer to section header for named section. */
IMAGE_SECTION_HEADER *
find_section (char * name, IMAGE_NT_HEADERS * nt_header)
{
  PIMAGE_SECTION_HEADER section;
  int i;

  section = IMAGE_FIRST_SECTION (nt_header);

  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++)
    {
      if (strcmp (section->Name, name) == 0)
	return section;
      section++;
    }
  return NULL;
}

/* Return pointer to section header for section containing the given
   relative virtual address. */
IMAGE_SECTION_HEADER *
rva_to_section (DWORD rva, IMAGE_NT_HEADERS * nt_header)
{
  PIMAGE_SECTION_HEADER section;
  int i;

  section = IMAGE_FIRST_SECTION (nt_header);

  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++)
    {
      /* Some linkers (eg. the NT SDK linker I believe) swapped the
	 meaning of these two values - or rather, they ignored
	 VirtualSize entirely and always set it to zero.  This affects
	 some very old exes (eg. gzip dated Dec 1993).  Since
	 w32_executable_type relies on this function to work reliably,
	 we need to cope with this.  */
      DWORD real_size = max (section->SizeOfRawData,
			     section->Misc.VirtualSize);
      if (rva >= section->VirtualAddress
	  && rva < section->VirtualAddress + real_size)
	return section;
      section++;
    }
  return NULL;
}

/* Return pointer to section header for section containing the given
   offset in its raw data area. */
IMAGE_SECTION_HEADER *
offset_to_section (DWORD offset, IMAGE_NT_HEADERS * nt_header)
{
  PIMAGE_SECTION_HEADER section;
  int i;

  section = IMAGE_FIRST_SECTION (nt_header);

  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++)
    {
      if (offset >= section->PointerToRawData
	  && offset < section->PointerToRawData + section->SizeOfRawData)
	return section;
      section++;
    }
  return NULL;
}

/* Return offset to an object in dst, given offset in src.  We assume
   there is at least one section in both src and dst images, and that
   the some sections may have been added to dst (after sections in src).  */
DWORD
relocate_offset (DWORD offset,
		 IMAGE_NT_HEADERS * src_nt_header,
		 IMAGE_NT_HEADERS * dst_nt_header)
{
  PIMAGE_SECTION_HEADER src_section = IMAGE_FIRST_SECTION (src_nt_header);
  PIMAGE_SECTION_HEADER dst_section = IMAGE_FIRST_SECTION (dst_nt_header);
  int i = 0;

  while (offset >= src_section->PointerToRawData)
    {
      if (offset < src_section->PointerToRawData + src_section->SizeOfRawData)
	break;
      i++;
      if (i == src_nt_header->FileHeader.NumberOfSections)
	{
	  /* Handle offsets after the last section.  */
	  dst_section = IMAGE_FIRST_SECTION (dst_nt_header);
	  dst_section += dst_nt_header->FileHeader.NumberOfSections - 1;
	  while (dst_section->PointerToRawData == 0)
	    dst_section--;
	  while (src_section->PointerToRawData == 0)
	    src_section--;
	  return offset
	    + (dst_section->PointerToRawData + dst_section->SizeOfRawData)
	    - (src_section->PointerToRawData + src_section->SizeOfRawData);
	}
      src_section++;
      dst_section++;
    }
  return offset +
    (dst_section->PointerToRawData - src_section->PointerToRawData);
}

#define OFFSET_TO_RVA(offset, section) \
	  (section->VirtualAddress + ((DWORD)(offset) - section->PointerToRawData))

#define RVA_TO_OFFSET(rva, section) \
	  (section->PointerToRawData + ((DWORD)(rva) - section->VirtualAddress))

#define RVA_TO_SECTION_OFFSET(rva, section) \
	  ((DWORD)(rva) - section->VirtualAddress)

/* Convert address in executing image to RVA.  */
#define PTR_TO_RVA(ptr) ((DWORD)(ptr) - (DWORD) GetModuleHandle (NULL))

#define RVA_TO_PTR(var,section,filedata) \
	  ((void *)(RVA_TO_OFFSET (var,section) + (filedata).file_base))

#define PTR_TO_OFFSET(ptr, pfile_data) \
          ((unsigned char *)(ptr) - (pfile_data)->file_base)

#define OFFSET_TO_PTR(offset, pfile_data) \
          ((pfile_data)->file_base + (DWORD)(offset))


/* Flip through the executable and cache the info necessary for dumping.  */
void
get_section_info (file_data *p_infile)
{
  PIMAGE_DOS_HEADER dos_header;
  PIMAGE_NT_HEADERS nt_header;
  int overlap;

  dos_header = (PIMAGE_DOS_HEADER) p_infile->file_base;
  if (dos_header->e_magic != IMAGE_DOS_SIGNATURE)
    {
      printf ("Unknown EXE header in %s...bailing.\n", p_infile->name);
      exit (1);
    }
  nt_header = (PIMAGE_NT_HEADERS) (((unsigned long) dos_header) +
				   dos_header->e_lfanew);
  if (nt_header == NULL)
    {
      printf ("Failed to find IMAGE_NT_HEADER in %s...bailing.\n",
	     p_infile->name);
      exit (1);
    }

  /* Check the NT header signature ...  */
  if (nt_header->Signature != IMAGE_NT_SIGNATURE)
    {
      printf ("Invalid IMAGE_NT_SIGNATURE 0x%x in %s...bailing.\n",
	      nt_header->Signature, p_infile->name);
      exit (1);
    }

  /* Locate the ".data" and ".bss" sections for Emacs.  (Note that the
     actual section names are probably different from these, and might
     actually be the same section.)

     We do this as follows: first we determine the virtual address
     ranges in this process for the data and bss variables that we wish
     to preserve.  Then we map these VAs to the section entries in the
     source image.  Finally, we determine the new size of the raw data
     area for the bss section, so we can make the new image the correct
     size.  */

  /* We arrange for the Emacs initialized data to be in a separate
     section if possible, because we cannot rely on my_begdata and
     my_edata marking out the full extent of the initialized data, at
     least on the Alpha where the linker freely reorders variables
     across libraries.  If we can arrange for this, all we need to do is
     find the start and size of the EMDATA section.  */
  data_section = find_section ("EMDATA", nt_header);
  if (data_section)
    {
      data_start = (char *) nt_header->OptionalHeader.ImageBase +
	data_section->VirtualAddress;
      data_size = data_section->Misc.VirtualSize;
    }
  else
    {
      /* Fallback on the old method if compiler doesn't support the
         data_set #pragma (or its equivalent).  */
      data_start = my_begdata;
      data_size = my_edata - my_begdata;
      data_section = rva_to_section (PTR_TO_RVA (my_begdata), nt_header);
      if (data_section != rva_to_section (PTR_TO_RVA (my_edata), nt_header))
	{
	  printf ("Initialized data is not in a single section...bailing\n");
	  exit (1);
	}
    }

  /* As noted in lastfile.c, the Alpha (but not the Intel) MSVC linker
     globally segregates all static and public bss data (ie. across all
     linked modules, not just per module), so we must take both static
     and public bss areas into account to determine the true extent of
     the bss area used by Emacs.

     To be strictly correct, we dump the static and public bss areas
     used by Emacs separately if non-overlapping (since otherwise we are
     dumping bss data belonging to system libraries, eg. the static bss
     system data on the Alpha).  */

  bss_start = my_begbss;
  bss_size = my_endbss - my_begbss;
  bss_section = rva_to_section (PTR_TO_RVA (my_begbss), nt_header);
  if (bss_section != rva_to_section (PTR_TO_RVA (my_endbss), nt_header))
    {
      printf ("Uninitialized data is not in a single section...bailing\n");
      exit (1);
    }
  /* Compute how much the .bss section's raw data will grow.  */
  extra_bss_size =
    ROUND_UP (RVA_TO_SECTION_OFFSET (PTR_TO_RVA (my_endbss), bss_section),
	      nt_header->OptionalHeader.FileAlignment)
    - bss_section->SizeOfRawData;

  bss_start_static = my_begbss_static;
  bss_size_static = my_endbss_static - my_begbss_static;
  bss_section_static = rva_to_section (PTR_TO_RVA (my_begbss_static), nt_header);
  if (bss_section_static != rva_to_section (PTR_TO_RVA (my_endbss_static), nt_header))
    {
      printf ("Uninitialized static data is not in a single section...bailing\n");
      exit (1);
    }
  /* Compute how much the static .bss section's raw data will grow.  */
  extra_bss_size_static =
    ROUND_UP (RVA_TO_SECTION_OFFSET (PTR_TO_RVA (my_endbss_static), bss_section_static),
	      nt_header->OptionalHeader.FileAlignment)
    - bss_section_static->SizeOfRawData;

  /* Combine the bss sections into one if they overlap.  */
#ifdef _ALPHA_
  overlap = 1;			/* force all bss data to be dumped */
#else
  overlap = 0;
#endif
  if (bss_start < bss_start_static)
    {
      if (bss_start_static < bss_start + bss_size)
	overlap = 1;
    }
  else
    {
      if (bss_start < bss_start_static + bss_size_static)
	overlap = 1;
    }
  if (overlap)
    {
      if (bss_section != bss_section_static)
	{
	  printf ("BSS data not in a single section...bailing\n");
	  exit (1);
	}
      bss_start = min (bss_start, bss_start_static);
      bss_size = max (my_endbss, my_endbss_static) - bss_start;
      bss_section_static = 0;
      extra_bss_size_static = 0;
    }

  heap_section = rva_to_section (PTR_TO_RVA (get_heap_start ()), nt_header);
}


/* The dump routines.  */

void
copy_executable_and_dump_data (file_data *p_infile,
			       file_data *p_outfile)
{
  unsigned char *dst, *dst_save;
  PIMAGE_DOS_HEADER dos_header;
  PIMAGE_NT_HEADERS nt_header;
  PIMAGE_NT_HEADERS dst_nt_header;
  PIMAGE_SECTION_HEADER section;
  PIMAGE_SECTION_HEADER dst_section;
  DWORD offset;
  int i;
  int be_verbose = GetEnvironmentVariable ("DEBUG_DUMP", NULL, 0) > 0;

#define COPY_CHUNK(message, src, size, verbose)					\
  do {										\
    unsigned char *s = (void *)(src);						\
    unsigned long count = (size);						\
    if (verbose)								\
      {										\
	printf ("%s\n", (message));						\
	printf ("\t0x%08x Offset in input file.\n", s - p_infile->file_base); 	\
	printf ("\t0x%08x Offset in output file.\n", dst - p_outfile->file_base); \
	printf ("\t0x%08x Size in bytes.\n", count);				\
      }										\
    memcpy (dst, s, count);							\
    dst += count;								\
  } while (0)

#define COPY_PROC_CHUNK(message, src, size, verbose)				\
  do {										\
    unsigned char *s = (void *)(src);						\
    unsigned long count = (size);						\
    if (verbose)								\
      {										\
	printf ("%s\n", (message));						\
	printf ("\t0x%08x Address in process.\n", s);				\
	printf ("\t0x%08x Offset in output file.\n", dst - p_outfile->file_base); \
	printf ("\t0x%08x Size in bytes.\n", count);				\
      }										\
    memcpy (dst, s, count);							\
    dst += count;								\
  } while (0)

#define DST_TO_OFFSET()  PTR_TO_OFFSET (dst, p_outfile)
#define ROUND_UP_DST(align) \
  (dst = p_outfile->file_base + ROUND_UP (DST_TO_OFFSET (), (align)))
#define ROUND_UP_DST_AND_ZERO(align)						\
  do {										\
    unsigned char *newdst = p_outfile->file_base				\
      + ROUND_UP (DST_TO_OFFSET (), (align));					\
    /* Zero the alignment slop; it may actually initialize real data.  */	\
    memset (dst, 0, newdst - dst);						\
    dst = newdst;								\
  } while (0)

  /* Copy the source image sequentially, ie. section by section after
     copying the headers and section table, to simplify the process of
     dumping the raw data for the bss and heap sections.

     Note that dst is updated implicitly by each COPY_CHUNK.  */

  dos_header = (PIMAGE_DOS_HEADER) p_infile->file_base;
  nt_header = (PIMAGE_NT_HEADERS) (((unsigned long) dos_header) +
				   dos_header->e_lfanew);
  section = IMAGE_FIRST_SECTION (nt_header);

  dst = (unsigned char *) p_outfile->file_base;

  COPY_CHUNK ("Copying DOS header...", dos_header,
	      (DWORD) nt_header - (DWORD) dos_header, be_verbose);
  dst_nt_header = (PIMAGE_NT_HEADERS) dst;
  COPY_CHUNK ("Copying NT header...", nt_header,
	      (DWORD) section - (DWORD) nt_header, be_verbose);
  dst_section = (PIMAGE_SECTION_HEADER) dst;
  COPY_CHUNK ("Copying section table...", section,
	      nt_header->FileHeader.NumberOfSections * sizeof (*section),
	      be_verbose);

  /* Align the first section's raw data area, and set the header size
     field accordingly.  */
  ROUND_UP_DST_AND_ZERO (dst_nt_header->OptionalHeader.FileAlignment);
  dst_nt_header->OptionalHeader.SizeOfHeaders = DST_TO_OFFSET ();

  for (i = 0; i < nt_header->FileHeader.NumberOfSections; i++)
    {
      char msg[100];
      /* Windows section names are fixed 8-char strings, only
	 zero-terminated if the name is shorter than 8 characters.  */
      sprintf (msg, "Copying raw data for %.8s...", section->Name);

      dst_save = dst;

      /* Update the file-relative offset for this section's raw data (if
         it has any) in case things have been relocated; we will update
         the other offsets below once we know where everything is.  */
      if (dst_section->PointerToRawData)
	dst_section->PointerToRawData = DST_TO_OFFSET ();

      /* Can always copy the original raw data.  */
      COPY_CHUNK
	(msg, OFFSET_TO_PTR (section->PointerToRawData, p_infile),
	 section->SizeOfRawData, be_verbose);
      /* Ensure alignment slop is zeroed.  */
      ROUND_UP_DST_AND_ZERO (dst_nt_header->OptionalHeader.FileAlignment);

      /* Note that various sections below may be aliases.  */
      if (section == data_section)
	{
	  dst = dst_save
	    + RVA_TO_SECTION_OFFSET (PTR_TO_RVA (data_start), dst_section);
	  COPY_PROC_CHUNK ("Dumping initialized data...",
			   data_start, data_size, be_verbose);
	  dst = dst_save + dst_section->SizeOfRawData;
	}
      if (section == bss_section)
	{
	  /* Dump contents of bss variables, adjusting the section's raw
             data size as necessary.  */
	  dst = dst_save
	    + RVA_TO_SECTION_OFFSET (PTR_TO_RVA (bss_start), dst_section);
	  COPY_PROC_CHUNK ("Dumping bss data...", bss_start,
			   bss_size, be_verbose);
	  ROUND_UP_DST (dst_nt_header->OptionalHeader.FileAlignment);
	  dst_section->PointerToRawData = PTR_TO_OFFSET (dst_save, p_outfile);
	  /* Determine new size of raw data area.  */
	  dst = max (dst, dst_save + dst_section->SizeOfRawData);
	  dst_section->SizeOfRawData = dst - dst_save;
	  dst_section->Characteristics &= ~IMAGE_SCN_CNT_UNINITIALIZED_DATA;
	  dst_section->Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA;
	}
      if (section == bss_section_static)
	{
	  /* Dump contents of static bss variables, adjusting the
             section's raw data size as necessary.  */
	  dst = dst_save
	    + RVA_TO_SECTION_OFFSET (PTR_TO_RVA (bss_start_static), dst_section);
	  COPY_PROC_CHUNK ("Dumping static bss data...", bss_start_static,
			   bss_size_static, be_verbose);
	  ROUND_UP_DST (dst_nt_header->OptionalHeader.FileAlignment);
	  dst_section->PointerToRawData = PTR_TO_OFFSET (dst_save, p_outfile);
	  /* Determine new size of raw data area.  */
	  dst = max (dst, dst_save + dst_section->SizeOfRawData);
	  dst_section->SizeOfRawData = dst - dst_save;
	  dst_section->Characteristics &= ~IMAGE_SCN_CNT_UNINITIALIZED_DATA;
	  dst_section->Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA;
	}
      if (section == heap_section)
	{
	  DWORD heap_start = (DWORD) get_heap_start ();
	  DWORD heap_size = get_committed_heap_size ();

	  /* Dump the used portion of the predump heap, adjusting the
             section's size to the appropriate size.  */
	  dst = dst_save
	    + RVA_TO_SECTION_OFFSET (PTR_TO_RVA (heap_start), dst_section);
	  COPY_PROC_CHUNK ("Dumping heap...", heap_start, heap_size,
			   be_verbose);
	  ROUND_UP_DST (dst_nt_header->OptionalHeader.FileAlignment);
	  dst_section->PointerToRawData = PTR_TO_OFFSET (dst_save, p_outfile);
	  /* Determine new size of raw data area.  */
	  dst = max (dst, dst_save + dst_section->SizeOfRawData);
	  dst_section->SizeOfRawData = dst - dst_save;
	  /* Reduce the size of the heap section to fit (must be last
             section).  */
	  dst_nt_header->OptionalHeader.SizeOfImage -=
	    dst_section->Misc.VirtualSize
	    - ROUND_UP (dst_section->SizeOfRawData,
			dst_nt_header->OptionalHeader.SectionAlignment);
	  dst_section->Misc.VirtualSize =
	    ROUND_UP (dst_section->SizeOfRawData,
		      dst_nt_header->OptionalHeader.SectionAlignment);
	  dst_section->Characteristics &= ~IMAGE_SCN_CNT_UNINITIALIZED_DATA;
	  dst_section->Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA;
	}

      /* Align the section's raw data area.  */
      ROUND_UP_DST (dst_nt_header->OptionalHeader.FileAlignment);

      section++;
      dst_section++;
    }

  /* Copy remainder of source image.  */
  do
    section--;
  while (section->PointerToRawData == 0);
  offset = ROUND_UP (section->PointerToRawData + section->SizeOfRawData,
		     nt_header->OptionalHeader.FileAlignment);
  COPY_CHUNK
    ("Copying remainder of executable...",
     OFFSET_TO_PTR (offset, p_infile),
     p_infile->size - offset, be_verbose);

  /* Final size for new image.  */
  p_outfile->size = DST_TO_OFFSET ();

  /* Now patch up remaining file-relative offsets.  */
  section = IMAGE_FIRST_SECTION (nt_header);
  dst_section = IMAGE_FIRST_SECTION (dst_nt_header);

#define ADJUST_OFFSET(var)						\
  do {									\
    if ((var) != 0)							\
      (var) = relocate_offset ((var), nt_header, dst_nt_header);	\
  } while (0)

  dst_nt_header->OptionalHeader.SizeOfInitializedData = 0;
  dst_nt_header->OptionalHeader.SizeOfUninitializedData = 0;
  for (i = 0; i < dst_nt_header->FileHeader.NumberOfSections; i++)
    {
      /* Recompute data sizes for completeness.  */
      if (dst_section[i].Characteristics & IMAGE_SCN_CNT_INITIALIZED_DATA)
	dst_nt_header->OptionalHeader.SizeOfInitializedData +=
	  ROUND_UP (dst_section[i].Misc.VirtualSize, dst_nt_header->OptionalHeader.FileAlignment);
      else if (dst_section[i].Characteristics & IMAGE_SCN_CNT_UNINITIALIZED_DATA)
	dst_nt_header->OptionalHeader.SizeOfUninitializedData +=
	  ROUND_UP (dst_section[i].Misc.VirtualSize, dst_nt_header->OptionalHeader.FileAlignment);

      ADJUST_OFFSET (dst_section[i].PointerToLinenumbers);
    }

  ADJUST_OFFSET (dst_nt_header->FileHeader.PointerToSymbolTable);

  /* Update offsets in debug directory entries. */
  {
    IMAGE_DATA_DIRECTORY debug_dir =
      dst_nt_header->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG];
    PIMAGE_DEBUG_DIRECTORY debug_entry;

    section = rva_to_section (debug_dir.VirtualAddress, dst_nt_header);
    if (section)
      {
	debug_entry = (PIMAGE_DEBUG_DIRECTORY)
	  (RVA_TO_OFFSET (debug_dir.VirtualAddress, section) + p_outfile->file_base);
	debug_dir.Size /= sizeof (IMAGE_DEBUG_DIRECTORY);

	for (i = 0; i < debug_dir.Size; i++, debug_entry++)
	  ADJUST_OFFSET (debug_entry->PointerToRawData);
      }
  }
}


/* Dump out .data and .bss sections into a new executable.  */
void
unexec (const char *new_name, const char *old_name)
{
  file_data in_file, out_file;
  char out_filename[MAX_PATH], in_filename[MAX_PATH];
  unsigned long size;
  char *p;
  char *q;

  /* Ignore old_name, and get our actual location from the OS.  */
  if (!GetModuleFileName (NULL, in_filename, MAX_PATH))
    abort ();
  dostounix_filename (in_filename);
  strcpy (out_filename, in_filename);

  /* Change the base of the output filename to match the requested name.  */
  if ((p = strrchr (out_filename, '/')) == NULL)
    abort ();
  /* The filenames have already been expanded, and will be in Unix
     format, so it is safe to expect an absolute name.  */
  if ((q = strrchr (new_name, '/')) == NULL)
    abort ();
  strcpy (p, q);

  /* Make sure that the output filename has the ".exe" extension...patch
     it up if not.  */
  p = out_filename + strlen (out_filename) - 4;
  if (strcmp (p, ".exe"))
    strcat (out_filename, ".exe");

  printf ("Dumping from %s\n", in_filename);
  printf ("          to %s\n", out_filename);

  /* We need to round off our heap to NT's page size.  */
  round_heap (get_page_size ());

  /* Open the undumped executable file.  */
  if (!open_input_file (&in_file, in_filename))
    {
      printf ("Failed to open %s (%d)...bailing.\n",
	      in_filename, GetLastError ());
      exit (1);
    }

  /* Get the interesting section info, like start and size of .bss...  */
  get_section_info (&in_file);

  /* The size of the dumped executable is the size of the original
     executable plus the size of the heap and the size of the .bss section.  */
  size = in_file.size +
    get_committed_heap_size () +
    extra_bss_size +
    extra_bss_size_static;
  if (!open_output_file (&out_file, out_filename, size))
    {
      printf ("Failed to open %s (%d)...bailing.\n",
	      out_filename, GetLastError ());
      exit (1);
    }

  /* Set the flag (before dumping).  */
  using_dynamic_heap = TRUE;

  copy_executable_and_dump_data (&in_file, &out_file);

  /* Patch up header fields; profiler is picky about this. */
  {
    PIMAGE_DOS_HEADER dos_header;
    PIMAGE_NT_HEADERS nt_header;
    HANDLE hImagehelp = LoadLibrary ("imagehlp.dll");
    DWORD  headersum;
    DWORD  checksum;

    dos_header = (PIMAGE_DOS_HEADER) out_file.file_base;
    nt_header = (PIMAGE_NT_HEADERS) ((char *) dos_header + dos_header->e_lfanew);

    nt_header->OptionalHeader.CheckSum = 0;
//    nt_header->FileHeader.TimeDateStamp = time (NULL);
//    dos_header->e_cp = size / 512;
//    nt_header->OptionalHeader.SizeOfImage = size;

    pfnCheckSumMappedFile = (void *) GetProcAddress (hImagehelp, "CheckSumMappedFile");
    if (pfnCheckSumMappedFile)
      {
//	nt_header->FileHeader.TimeDateStamp = time (NULL);
	pfnCheckSumMappedFile (out_file.file_base,
			       out_file.size,
			       &headersum,
			       &checksum);
	nt_header->OptionalHeader.CheckSum = checksum;
      }
    FreeLibrary (hImagehelp);
  }

  close_file_data (&in_file);
  close_file_data (&out_file);
}

/* eof */
