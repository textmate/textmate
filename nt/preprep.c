/* Pre-process emacs.exe for profiling by MSVC.
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


   Andrew Innes <andrewi@harlequin.co.uk>       16-Jan-1999
     based on code from addsection.c
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

int
open_inout_file (file_data *p_file, const char *filename)
{
  HANDLE file;
  HANDLE file_mapping;
  void  *file_base;
  unsigned long size, upper_size;

  file = CreateFile (filename, GENERIC_READ | GENERIC_WRITE, 0, NULL,
		     OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if (file == INVALID_HANDLE_VALUE)
    return FALSE;

  size = GetFileSize (file, &upper_size);
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

#define RVA_TO_PTR(var,section,filedata) \
	  ((void *)(RVA_TO_OFFSET(var,section) + (filedata)->file_base))

/* Convert address in executing image to RVA.  */
#define PTR_TO_RVA(ptr) ((DWORD)(ptr) - (DWORD) GetModuleHandle (NULL))

#define PTR_TO_OFFSET(ptr, pfile_data) \
          ((unsigned const char *)(ptr) - (pfile_data)->file_base)

#define OFFSET_TO_PTR(offset, pfile_data) \
          ((pfile_data)->file_base + (DWORD)(offset))

#define ROUND_UP(p, align)   (((DWORD)(p) + (align)-1) & ~((align)-1))
#define ROUND_DOWN(p, align) ((DWORD)(p) & ~((align)-1))


/* The MSVC prep program generates a ._xe file from .exe, where relevant
   function calls etc have been patched to go through thunks (generated
   by prep) that record timing/call information.  Because the thunks
   need to make references to functions imported from profile.dll, the
   import table must be expanded; the end result is that all the
   sections following .rdata are relocated to higher RVAs (add a final
   code section is added holding all the thunks).  The .reloc section is
   also expanded, so that the thunks themselves are relocatable.

   It is this relocation which kills emacs._xe, because the dumped heap
   pointers aren't relocated, because there is no relocation data for
   either the relevant global/static variables or the heap section
   itself, both of which contain pointers into the heap.  [Note that
   static variables which aren't initialized during linking may become
   initialized with heap pointers, or even pointers to other static
   variables, because of dumping.]

   We could potentially generate the relocation data ourselves by making
   two versions of temacs, one with an extra dummy section before
   EMHEAP to offset it, and then compare the dumped executables from
   both.  That is a lot of work though, and it doesn't solve the problem
   of dumped pointers to static variables, which also can be relocated.

   A better solution is to pre-process emacs.exe so that the .rdata and
   .reloc sections are moved to the end of the section table, and thus
   prep won't relocate anything else.  (Of course, we leave "dead"
   copies of these two sections in place, so that the virtual address of
   everything else is unaffected.)  Relocating the .reloc data is
   trivial - we just update the IMAGE_BASE_RELOCATION address in the
   header (the data itself doesn't change).  Relocating the import table
   is more complicated though, because the calls to imported functions
   must be patched up.  That requires us to selectively apply the base
   relocations when we encounter references to imported functions (or
   variables) in other sections, but at least the base relocations are
   easy to parse.  */

static void
copy_executable_and_move_sections (file_data *p_infile,
				   file_data *p_outfile)
{
  unsigned char *dst;
  PIMAGE_DOS_HEADER dos_header;
  PIMAGE_NT_HEADERS nt_header;
  PIMAGE_NT_HEADERS dst_nt_header;
  PIMAGE_SECTION_HEADER section;
  PIMAGE_SECTION_HEADER dst_section;
  PIMAGE_SECTION_HEADER import_section;
  PIMAGE_SECTION_HEADER reloc_section;
  PIMAGE_DATA_DIRECTORY import_dir;
  PIMAGE_DATA_DIRECTORY reloc_dir;
  DWORD import_delta_rva;
  DWORD reloc_delta_rva;
  DWORD offset;
  int i;

#define COPY_CHUNK(message, src, size)						\
  do {										\
    unsigned const char *s = (void *)(src);					\
    unsigned long count = (size);						\
    printf ("%s\n", (message));							\
    printf ("\t0x%08x Offset in input file.\n", s - p_infile->file_base);	\
    printf ("\t0x%08x Offset in output file.\n", dst - p_outfile->file_base);	\
    printf ("\t0x%08x Size in bytes.\n", count);				\
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
     relocating the .rdata and .reloc section table entries (which might
     force the raw section data to be relocated).

     Note that dst is updated implicitly by each COPY_CHUNK.  */

  dos_header = (PIMAGE_DOS_HEADER) p_infile->file_base;
  nt_header = (PIMAGE_NT_HEADERS) (((unsigned long) dos_header) +
				   dos_header->e_lfanew);
  section = IMAGE_FIRST_SECTION (nt_header);

  import_dir = &nt_header->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
  import_section = rva_to_section (import_dir->VirtualAddress, nt_header);

  reloc_dir = &nt_header->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC];
  reloc_section = rva_to_section (reloc_dir->VirtualAddress, nt_header);
  if (!reloc_section)
    {
      printf ("No relocation data, cannot prepare for profile prepping.\n");
      exit (1);
    }

  dst = (unsigned char *) p_outfile->file_base;

  COPY_CHUNK ("Copying DOS header...", dos_header,
	      (DWORD) nt_header - (DWORD) dos_header);
  dst_nt_header = (PIMAGE_NT_HEADERS) dst;
  COPY_CHUNK ("Copying NT header...", nt_header,
	      (DWORD) section - (DWORD) nt_header);
  dst_section = (PIMAGE_SECTION_HEADER) dst;
  COPY_CHUNK ("Copying section table...", section,
	      nt_header->FileHeader.NumberOfSections * sizeof (*section));

  /* Leave room for extra section table entries; filled in below.  */
  dst += 2 * sizeof (*section);

  /* Align the first section's raw data area, and set the header size
     field accordingly.  */
  ROUND_UP_DST_AND_ZERO (dst_nt_header->OptionalHeader.FileAlignment);
  dst_nt_header->OptionalHeader.SizeOfHeaders = DST_TO_OFFSET ();

  for (i = 0; i < nt_header->FileHeader.NumberOfSections;
       i++, section++, dst_section++)
    {
      char msg[100];
      sprintf (msg, "Copying raw data for %s...", section->Name);

      /* "Blank out" the two sections being relocated.  */
      if (section == import_section || section == reloc_section)
	{
	  dst_section->Name[0] = 'X';
	  dst_section->Misc.VirtualSize =
	    ROUND_UP (dst_section->Misc.VirtualSize,
		      dst_nt_header->OptionalHeader.SectionAlignment);
	  dst_section->PointerToRawData = 0;
	  dst_section->SizeOfRawData = 0;
	  dst_section->Characteristics &= ~IMAGE_SCN_CNT_INITIALIZED_DATA;
	  dst_section->Characteristics |= IMAGE_SCN_CNT_UNINITIALIZED_DATA;
	  dst_section->Characteristics &= ~IMAGE_SCN_MEM_WRITE;
	  continue;
	}

      /* Update the file-relative offset for this section's raw data (if
         it has any) in case things have been relocated; we will update
         the other offsets below once we know where everything is.  */
      if (dst_section->PointerToRawData)
	dst_section->PointerToRawData = DST_TO_OFFSET ();

      /* Copy the original raw data.  */
      COPY_CHUNK
	(msg, OFFSET_TO_PTR (section->PointerToRawData, p_infile),
	 section->SizeOfRawData);

      /* Round up the raw data size to the new alignment.  */
      dst_section->SizeOfRawData =
	ROUND_UP (dst_section->SizeOfRawData,
		  dst_nt_header->OptionalHeader.FileAlignment);

      /* Align the next section's raw data area.  */
      ROUND_UP_DST_AND_ZERO (dst_nt_header->OptionalHeader.FileAlignment);
    }

  /* Add the extra section entries, copying the raw data we skipped
     earlier.  We'll patch up the data itself below.  */
  if (import_section != NULL)
    {
      dst_nt_header->FileHeader.NumberOfSections++;
      dst_nt_header->OptionalHeader.SizeOfImage +=
	ROUND_UP (import_section->Misc.VirtualSize,
		  dst_nt_header->OptionalHeader.SectionAlignment);
      *dst_section = *import_section;
      dst_section->VirtualAddress =
	dst_section[-1].VirtualAddress
	+ ROUND_UP (dst_section[-1].Misc.VirtualSize,
		    dst_nt_header->OptionalHeader.SectionAlignment);
      dst_section->PointerToRawData = DST_TO_OFFSET ();
      /* Remember delta applied to import section.  */
      import_delta_rva = dst_section->VirtualAddress - import_section->VirtualAddress;
      COPY_CHUNK
	("Relocating import directory",
	 OFFSET_TO_PTR (import_section->PointerToRawData, p_infile),
	 import_section->SizeOfRawData);
      ROUND_UP_DST_AND_ZERO (dst_nt_header->OptionalHeader.FileAlignment);
      dst_section++;
    }
  if (reloc_section != NULL)
    {
      dst_nt_header->FileHeader.NumberOfSections++;
      dst_nt_header->OptionalHeader.SizeOfImage +=
	ROUND_UP (reloc_section->Misc.VirtualSize,
		  dst_nt_header->OptionalHeader.SectionAlignment);
      *dst_section = *reloc_section;
      dst_section->VirtualAddress =
	dst_section[-1].VirtualAddress
	+ ROUND_UP (dst_section[-1].Misc.VirtualSize,
		    dst_nt_header->OptionalHeader.SectionAlignment);
      dst_section->PointerToRawData = DST_TO_OFFSET ();
      /* Remember delta applied to reloc section.  */
      reloc_delta_rva = dst_section->VirtualAddress - reloc_section->VirtualAddress;
      COPY_CHUNK
	("Relocating base relocations directory",
	 OFFSET_TO_PTR (reloc_section->PointerToRawData, p_infile),
	 reloc_section->SizeOfRawData);
      ROUND_UP_DST_AND_ZERO (dst_nt_header->OptionalHeader.FileAlignment);
      reloc_dir = &dst_nt_header->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC];
      reloc_dir->VirtualAddress += reloc_delta_rva;
      dst_section++;
    }

  /* Copy remainder of source image.  */
  section--;
  offset = ROUND_UP (section->PointerToRawData + section->SizeOfRawData,
		     nt_header->OptionalHeader.FileAlignment);
  COPY_CHUNK
    ("Copying remainder of executable...",
     OFFSET_TO_PTR (offset, p_infile),
     p_infile->size - offset);

  /* Final size for new image.  */
  p_outfile->size = DST_TO_OFFSET ();

  /* Now patch up remaining file-relative offsets.  */
  printf ("Patching up raw data offsets...\n");

  section = IMAGE_FIRST_SECTION (nt_header);
  dst_section = IMAGE_FIRST_SECTION (dst_nt_header);

#define ADJUST_OFFSET(var)						\
  do {									\
    if ((var) != 0)							\
      (var) = relocate_offset ((var), nt_header, dst_nt_header);	\
  } while (0)

#define ADJUST_IMPORT_RVA(var)			\
  do {						\
    if ((var) != 0)				\
      *((DWORD *)&(var)) += import_delta_rva;	\
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

  /* Update offsets in debug directory entries.  Note that the debug
     directory may be in the same section as the import table, so its
     RVA may need to be adjusted too.  */
  {
    PIMAGE_DATA_DIRECTORY debug_dir =
      &dst_nt_header->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG];
    PIMAGE_DEBUG_DIRECTORY debug_entry;

    /* Update debug_dir if part of import_section.  */
    if (rva_to_section (debug_dir->VirtualAddress, nt_header) == import_section)
      debug_dir->VirtualAddress += import_delta_rva;

    section = rva_to_section (debug_dir->VirtualAddress, dst_nt_header);
    if (section)
      {
	int size;

	debug_entry = RVA_TO_PTR (debug_dir->VirtualAddress, section, p_outfile);
	size = debug_dir->Size / sizeof (IMAGE_DEBUG_DIRECTORY);

	for (i = 0; i < size; i++, debug_entry++)
	  {
	    /* The debug data itself is normally not part of any
	       section, but stored after all the raw section data.  So
	       let relocate_offset do the work.  */
	    ADJUST_OFFSET (debug_entry->PointerToRawData);
	    ADJUST_IMPORT_RVA (debug_entry->AddressOfRawData);
	  }
      }
  }

  /* Update RVAs in import directory entries.  */
  {
    PIMAGE_IMPORT_DESCRIPTOR imports;
    PIMAGE_THUNK_DATA import_thunks;

    import_dir = &dst_nt_header->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
    import_dir->VirtualAddress += import_delta_rva;

    section = rva_to_section (import_dir->VirtualAddress, dst_nt_header);
    imports = RVA_TO_PTR (import_dir->VirtualAddress, section, p_outfile);

    for ( ; imports->Name != 0; imports++)
      {
	ADJUST_IMPORT_RVA (imports->OriginalFirstThunk);
	ADJUST_IMPORT_RVA (imports->FirstThunk);
	ADJUST_IMPORT_RVA (imports->Name);

	for (import_thunks = RVA_TO_PTR (imports->OriginalFirstThunk, section, p_outfile);
	     import_thunks->u1.Function != 0;
	     import_thunks++)
	  if ((import_thunks->u1.Ordinal >> 31) == 0)
	    ADJUST_IMPORT_RVA (import_thunks->u1.Ordinal);

	for (import_thunks = RVA_TO_PTR (imports->FirstThunk, section, p_outfile);
	     import_thunks->u1.Function != 0;
	     import_thunks++)
	  if ((import_thunks->u1.Ordinal >> 31) == 0)
	    ADJUST_IMPORT_RVA (import_thunks->u1.Ordinal);
      }

    import_dir = &dst_nt_header->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IAT];
    import_dir->VirtualAddress += import_delta_rva;
  }

  /* Fix up references to the import section.  */
  printf ("Applying fixups to import references...\n");

  {
    IMAGE_BASE_RELOCATION *relocs, *block, *start_block, *end_block;
    DWORD import_start = import_section->VirtualAddress + dst_nt_header->OptionalHeader.ImageBase;
    DWORD import_end = import_start + import_section->Misc.VirtualSize;
    DWORD len_import_relocs;
    DWORD len_remaining_relocs;
    int seen_high = 0;
    WORD * high_word;
    void * holder;

    reloc_dir = &dst_nt_header->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC];
    reloc_section = rva_to_section (reloc_dir->VirtualAddress, dst_nt_header);
    relocs = RVA_TO_PTR (reloc_dir->VirtualAddress, reloc_section, p_outfile);

    /* Move the base relocations for the import section, if there are
       any; the profiler needs to be able to patch RVAs in the import
       section itself.  */
    for (block = relocs, start_block = 0;
	 (DWORD) block - (DWORD) relocs < reloc_dir->Size;
	 block = (void *)((DWORD) block + block->SizeOfBlock))
      {
	if (block->VirtualAddress >= import_section->VirtualAddress + import_section->Misc.VirtualSize)
	  {
	    end_block = block;
	    break;
	  }
	if (block->VirtualAddress >= import_section->VirtualAddress)
	  {
	    if (start_block == 0)
	      start_block = block;
	    block->VirtualAddress += import_delta_rva;
	  }
      }
    if (start_block)
      {
	len_import_relocs = (DWORD) end_block - (DWORD) start_block;
	len_remaining_relocs = (DWORD) relocs + reloc_dir->Size - (DWORD) end_block;
	holder = malloc (len_import_relocs);
	if (holder == 0)
	  abort ();
	memcpy (holder, start_block, len_import_relocs);
	memcpy (start_block, end_block, len_remaining_relocs);
	memcpy ((char *) start_block + len_remaining_relocs, holder, len_import_relocs);
	free (holder);
      }

    /* Walk up the list of base relocations, checking for references
       to the old import section location, and patching them to
       reference the new location.  */
    for (block = relocs;
	 (DWORD) block - (DWORD) relocs < reloc_dir->Size;
	 block = (void *)((DWORD) block + block->SizeOfBlock))
      {
	DWORD page_rva = block->VirtualAddress;
	DWORD page_offset;
	union {
	  WORD word;
	  DWORD dword;
	} * ploc;
	WORD *fixup;

	section = rva_to_section (page_rva, dst_nt_header);
	/* Don't apply fixups to the blanked sections.  */
	if (section->Name[0] == 'X')
	  continue;

	for (fixup = (WORD *) &block[1];
	     (DWORD) fixup - (DWORD) block < block->SizeOfBlock;
	     fixup++)
	  {
	    page_offset = (*fixup) & 0xfff;
	    ploc = RVA_TO_PTR (page_rva + page_offset, section, p_outfile);

	    /* Unless our assumption is wrong, all low word fixups
	       should immediately follow a high fixup.  */
	    if (seen_high && ((*fixup) >> 12) != IMAGE_REL_BASED_LOW)
	      abort ();

	    switch ((*fixup) >> 12)
	      {
	      case IMAGE_REL_BASED_ABSOLUTE:
		break;
	      case IMAGE_REL_BASED_HIGH:
		/* We must assume that high and low fixups occur in
                   pairs, specifically a low fixup immediately follows a
                   high fixup (normally separated by two bytes).  We
                   have to process the two fixups together, to find out
                   the full pointer value and decide whether to apply
                   the fixup.  */
		seen_high = 1;
		high_word = &ploc->word;
		break;
	      case IMAGE_REL_BASED_LOW:
		offset = (*high_word << 16) + ploc->word;
		if (offset >= import_start && offset < import_end)
		  {
		    (*high_word) += import_delta_rva >> 16;
		    ploc->dword += import_delta_rva & 0xffff;
		  }
		seen_high = 0;
		break;
	      case IMAGE_REL_BASED_HIGHLOW:
		/* Docs imply two words in big-endian order, so perhaps
                   this is only used on big-endian platforms, in which
                   case the obvious code will work.  */
		if (ploc->dword >= import_start && ploc->dword < import_end)
		  ploc->dword += import_delta_rva;
		break;
	      case IMAGE_REL_BASED_HIGHADJ:
		/* Docs don't say, but I guess this is the equivalent
                   for little-endian platforms.  */
		if (ploc->dword >= import_start && ploc->dword < import_end)
		  ploc->dword += import_delta_rva;
		break;
	      case IMAGE_REL_BASED_MIPS_JMPADDR:
		/* Don't know how to handle this; MIPS support has been
                   dropped from NT4 anyway.  */
		abort ();
		break;
#ifdef IMAGE_REL_BASED_SECTION
	      case IMAGE_REL_BASED_SECTION:
	      case IMAGE_REL_BASED_REL32:
		/* Docs don't say what these values mean.  */
#endif
	      default:
		abort ();
	      }
	  }
      }
  }
}


int
main (int argc, char **argv)
{
  PIMAGE_DOS_HEADER dos_header;
  PIMAGE_NT_HEADERS nt_header;
  file_data in_file, out_file;
  char out_filename[MAX_PATH], in_filename[MAX_PATH];

  strcpy (in_filename, argv[1]);
  strcpy (out_filename, argv[2]);

  printf ("Preparing %s for profile prepping\n", out_filename);

  /* Open the original (dumped) executable file for reference.  */
  if (!open_input_file (&in_file, in_filename))
    {
      printf ("Failed to open %s (%d)...bailing.\n",
	      in_filename, GetLastError ());
      exit (1);
    }

  /* Create a new image that can be prepped; we don't expect the size to
     change because we are only adding two new section table entries,
     which should fit in the alignment slop.  */
  if (!open_output_file (&out_file, out_filename, in_file.size))
    {
      printf ("Failed to open %s (%d)...bailing.\n",
	      out_filename, GetLastError ());
      exit (1);
    }

  copy_executable_and_move_sections (&in_file, &out_file);

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

  close_file_data (&out_file);
  close_file_data (&in_file);

  return 0;
}

/* eof */
