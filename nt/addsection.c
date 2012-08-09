/* Add an uninitialized data section to an executable.
   Copyright (C) 1999, 2001-2012  Free Software Foundation, Inc.

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


   Andrew Innes <andrewi@harlequin.co.uk>       04-Jan-1999
     based on code from unexw32.c
*/

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <time.h>
#ifdef __GNUC__
#define _ANONYMOUS_UNION
#define _ANONYMOUS_STRUCT
#endif
#include <windows.h>

/* Include relevant definitions from IMAGEHLP.H, which can be found
   in \\win32sdk\mstools\samples\image\include\imagehlp.h. */

PIMAGE_NT_HEADERS
(__stdcall * pfnCheckSumMappedFile) (LPVOID BaseAddress,
				     DWORD FileLength,
				     LPDWORD HeaderSum,
				     LPDWORD CheckSum);

#undef min
#undef max
#define min(x, y) (((x) < (y)) ? (x) : (y))
#define max(x, y) (((x) > (y)) ? (x) : (y))


/* File handling.  */

typedef struct file_data {
  const char    *name;
  unsigned long  size;
  HANDLE         file;
  HANDLE         file_mapping;
  unsigned char *file_base;
} file_data;

int
open_input_file (file_data *p_file, const char *filename)
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
open_output_file (file_data *p_file, const char *filename, unsigned long size)
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

unsigned long
get_unrounded_section_size (PIMAGE_SECTION_HEADER p_section)
{
  /* The true section size, before rounding, for an initialized data or
     code section.  (Supposedly some linkers swap the meaning of these
     two values.)  */
  return min (p_section->SizeOfRawData,
	      p_section->Misc.VirtualSize);
}

/* Return pointer to section header for named section. */
IMAGE_SECTION_HEADER *
find_section (const char *name, IMAGE_NT_HEADERS *nt_header)
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
static DWORD
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

#define PTR_TO_OFFSET(ptr, pfile_data) \
          ((unsigned const char *)(ptr) - (pfile_data)->file_base)

#define OFFSET_TO_PTR(offset, pfile_data) \
          ((pfile_data)->file_base + (DWORD)(offset))

#define ROUND_UP(p, align)   (((DWORD)(p) + (align)-1) & ~((align)-1))
#define ROUND_DOWN(p, align) ((DWORD)(p) & ~((align)-1))


static void
copy_executable_and_add_section (file_data *p_infile,
				 file_data *p_outfile,
				 const char *new_section_name,
				 DWORD new_section_size)
{
  unsigned char *dst;
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
    unsigned const char *s = (void *)(src);						\
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

#define DST_TO_OFFSET()  PTR_TO_OFFSET (dst, p_outfile)
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
     adding an extra section table entry (which might force the raw
     section data to be relocated).

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

  /* To improve the efficiency of demand loading, make the file
     alignment match the section alignment (VC++ 6.0 does this by
     default anyway).  */
  dst_nt_header->OptionalHeader.FileAlignment =
    dst_nt_header->OptionalHeader.SectionAlignment;

  /* Add an uninitialized data section at the end, of the specified name
     and virtual size.  */
  if (find_section (new_section_name, nt_header) == NULL)
    /* Leave room for extra section table entry; filled in below.  */
    dst += sizeof (*section);
  else
    new_section_name = NULL;

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

      /* Update the file-relative offset for this section's raw data (if
         it has any) in case things have been relocated; we will update
         the other offsets below once we know where everything is.  */
      if (dst_section->PointerToRawData)
	dst_section->PointerToRawData = DST_TO_OFFSET ();

      /* Can always copy the original raw data.  */
      COPY_CHUNK
	(msg, OFFSET_TO_PTR (section->PointerToRawData, p_infile),
	 section->SizeOfRawData, be_verbose);

      /* Round up the raw data size to the new alignment.  */
      dst_section->SizeOfRawData =
	ROUND_UP (dst_section->SizeOfRawData,
		  dst_nt_header->OptionalHeader.FileAlignment);

      /* Align the next section's raw data area.  */
      ROUND_UP_DST_AND_ZERO (dst_nt_header->OptionalHeader.FileAlignment);

      section++;
      dst_section++;
    }

  /* Add the extra section entry (which adds no raw data).  */
  if (new_section_name != NULL)
    {
      dst_nt_header->FileHeader.NumberOfSections++;
      dst_nt_header->OptionalHeader.SizeOfImage += new_section_size;
      strncpy (dst_section->Name, new_section_name, sizeof (dst_section->Name));
      dst_section->VirtualAddress =
	section[-1].VirtualAddress
	+ ROUND_UP (section[-1].Misc.VirtualSize,
		    dst_nt_header->OptionalHeader.SectionAlignment);
      dst_section->Misc.VirtualSize = new_section_size;
      dst_section->PointerToRawData = 0;
      dst_section->SizeOfRawData = 0;
      dst_section->Characteristics =
	IMAGE_SCN_CNT_UNINITIALIZED_DATA
	| IMAGE_SCN_MEM_READ
	| IMAGE_SCN_MEM_WRITE;
    }

  /* Copy remainder of source image.  */
  section--;
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


int
main (int argc, char **argv)
{
  file_data in_file, out_file;
  char out_filename[MAX_PATH], in_filename[MAX_PATH];
  unsigned long size;
  PIMAGE_DOS_HEADER dos_header;
  PIMAGE_NT_HEADERS nt_header;

#define OLD_NAME        argv[1]
#define NEW_NAME        argv[2]
#define SECTION_NAME    argv[3]
#define SECTION_SIZE    argv[4]

  strcpy (in_filename, OLD_NAME);
  strcpy (out_filename, NEW_NAME);

  printf ("Dumping from %s\n", in_filename);
  printf ("          to %s\n", out_filename);

  /* Open the undumped executable file.  */
  if (!open_input_file (&in_file, in_filename))
    {
      printf ("Failed to open %s (%d)...bailing.\n",
	      in_filename, GetLastError ());
      exit (1);
    }
  dos_header = (PIMAGE_DOS_HEADER) in_file.file_base;
  nt_header = (PIMAGE_NT_HEADERS) ((char *) dos_header + dos_header->e_lfanew);
  /* Allow for expansion due to increasing file align to section align.
     We can overestimate here, since close_file_data will update the
     size exactly.  */
  size = in_file.size
    + nt_header->OptionalHeader.SectionAlignment
    * nt_header->FileHeader.NumberOfSections;
  if (!open_output_file (&out_file, out_filename, size))
    {
      printf ("Failed to open %s (%d)...bailing.\n",
	      out_filename, GetLastError ());
      exit (1);
    }

  copy_executable_and_add_section (&in_file, &out_file,
				   SECTION_NAME,
				   atoi (SECTION_SIZE) * 1024 * 1024);

  /* Patch up header fields; profiler is picky about this. */
  {
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

  return 0;
}

/* eof */

