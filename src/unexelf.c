/* Copyright (C) 1985-1988, 1990, 1992, 1999-2012
                 Free Software Foundation, Inc.

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
In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */


/*
 * unexec.c - Convert a running program into an a.out file.
 *
 * Author:	Spencer W. Thomas
 *		Computer Science Dept.
 *		University of Utah
 * Date:	Tue Mar  2 1982
 * Modified heavily since then.
 *
 * Synopsis:
 *	unexec (const char *new_name, const char *old_name);
 *
 * Takes a snapshot of the program and makes an a.out format file in the
 * file named by the string argument new_name.
 * If old_name is non-NULL, the symbol table will be taken from the given file.
 * On some machines, an existing old_name file is required.
 *
 */

/* Even more heavily modified by james@bigtex.cactus.org of Dell Computer Co.
 * ELF support added.
 *
 * Basic theory: the data space of the running process needs to be
 * dumped to the output file.  Normally we would just enlarge the size
 * of .data, scooting everything down.  But we can't do that in ELF,
 * because there is often something between the .data space and the
 * .bss space.
 *
 * In the temacs dump below, notice that the Global Offset Table
 * (.got) and the Dynamic link data (.dynamic) come between .data1 and
 * .bss.  It does not work to overlap .data with these fields.
 *
 * The solution is to create a new .data segment.  This segment is
 * filled with data from the current process.  Since the contents of
 * various sections refer to sections by index, the new .data segment
 * is made the last in the table to avoid changing any existing index.

 * This is an example of how the section headers are changed.  "Addr"
 * is a process virtual address.  "Offset" is a file offset.

raid:/nfs/raid/src/dist-18.56/src> dump -h temacs

temacs:

           **** SECTION HEADER TABLE ****
 [No]    Type    Flags   Addr         Offset       Size          Name
         Link    Info    Adralgn      Entsize

 [1]     1       2       0x80480d4    0xd4         0x13          .interp
         0       0       0x1          0

 [2]     5       2       0x80480e8    0xe8         0x388         .hash
         3       0       0x4          0x4

 [3]     11      2       0x8048470    0x470        0x7f0         .dynsym
         4       1       0x4          0x10

 [4]     3       2       0x8048c60    0xc60        0x3ad         .dynstr
         0       0       0x1          0

 [5]     9       2       0x8049010    0x1010       0x338         .rel.plt
         3       7       0x4          0x8

 [6]     1       6       0x8049348    0x1348       0x3           .init
         0       0       0x4          0

 [7]     1       6       0x804934c    0x134c       0x680         .plt
         0       0       0x4          0x4

 [8]     1       6       0x80499cc    0x19cc       0x3c56f       .text
         0       0       0x4          0

 [9]     1       6       0x8085f3c    0x3df3c      0x3           .fini
         0       0       0x4          0

 [10]    1       2       0x8085f40    0x3df40      0x69c         .rodata
         0       0       0x4          0

 [11]    1       2       0x80865dc    0x3e5dc      0xd51         .rodata1
         0       0       0x4          0

 [12]    1       3       0x8088330    0x3f330      0x20afc       .data
         0       0       0x4          0

 [13]    1       3       0x80a8e2c    0x5fe2c      0x89d         .data1
         0       0       0x4          0

 [14]    1       3       0x80a96cc    0x606cc      0x1a8         .got
         0       0       0x4          0x4

 [15]    6       3       0x80a9874    0x60874      0x80          .dynamic
         4       0       0x4          0x8

 [16]    8       3       0x80a98f4    0x608f4      0x449c        .bss
         0       0       0x4          0

 [17]    2       0       0            0x608f4      0x9b90        .symtab
         18      371     0x4          0x10

 [18]    3       0       0            0x6a484      0x8526        .strtab
         0       0       0x1          0

 [19]    3       0       0            0x729aa      0x93          .shstrtab
         0       0       0x1          0

 [20]    1       0       0            0x72a3d      0x68b7        .comment
         0       0       0x1          0

 raid:/nfs/raid/src/dist-18.56/src> dump -h xemacs

 xemacs:

            **** SECTION HEADER TABLE ****
 [No]    Type    Flags   Addr         Offset       Size          Name
         Link    Info    Adralgn      Entsize

 [1]     1       2       0x80480d4    0xd4         0x13          .interp
         0       0       0x1          0

 [2]     5       2       0x80480e8    0xe8         0x388         .hash
         3       0       0x4          0x4

 [3]     11      2       0x8048470    0x470        0x7f0         .dynsym
         4       1       0x4          0x10

 [4]     3       2       0x8048c60    0xc60        0x3ad         .dynstr
         0       0       0x1          0

 [5]     9       2       0x8049010    0x1010       0x338         .rel.plt
         3       7       0x4          0x8

 [6]     1       6       0x8049348    0x1348       0x3           .init
         0       0       0x4          0

 [7]     1       6       0x804934c    0x134c       0x680         .plt
         0       0       0x4          0x4

 [8]     1       6       0x80499cc    0x19cc       0x3c56f       .text
         0       0       0x4          0

 [9]     1       6       0x8085f3c    0x3df3c      0x3           .fini
         0       0       0x4          0

 [10]    1       2       0x8085f40    0x3df40      0x69c         .rodata
         0       0       0x4          0

 [11]    1       2       0x80865dc    0x3e5dc      0xd51         .rodata1
         0       0       0x4          0

 [12]    1       3       0x8088330    0x3f330      0x20afc       .data
         0       0       0x4          0

 [13]    1       3       0x80a8e2c    0x5fe2c      0x89d         .data1
         0       0       0x4          0

 [14]    1       3       0x80a96cc    0x606cc      0x1a8         .got
         0       0       0x4          0x4

 [15]    6       3       0x80a9874    0x60874      0x80          .dynamic
         4       0       0x4          0x8

 [16]    8       3       0x80c6800    0x7d800      0             .bss
         0       0       0x4          0

 [17]    2       0       0            0x7d800      0x9b90        .symtab
         18      371     0x4          0x10

 [18]    3       0       0            0x87390      0x8526        .strtab
         0       0       0x1          0

 [19]    3       0       0            0x8f8b6      0x93          .shstrtab
         0       0       0x1          0

 [20]    1       0       0            0x8f949      0x68b7        .comment
         0       0       0x1          0

 [21]    1       3       0x80a98f4    0x608f4      0x1cf0c       .data
         0       0       0x4          0

  * This is an example of how the file header is changed.  "Shoff" is
  * the section header offset within the file.  Since that table is
  * after the new .data section, it is moved.  "Shnum" is the number of
  * sections, which we increment.
  *
  * "Phoff" is the file offset to the program header.  "Phentsize" and
  * "Shentsz" are the program and section header entries sizes respectively.
  * These can be larger than the apparent struct sizes.

 raid:/nfs/raid/src/dist-18.56/src> dump -f temacs

 temacs:

                     **** ELF HEADER ****
 Class        Data       Type         Machine     Version
 Entry        Phoff      Shoff        Flags       Ehsize
 Phentsize    Phnum      Shentsz      Shnum       Shstrndx

 1            1          2            3           1
 0x80499cc    0x34       0x792f4      0           0x34
 0x20         5          0x28         21          19

 raid:/nfs/raid/src/dist-18.56/src> dump -f xemacs

 xemacs:

                     **** ELF HEADER ****
 Class        Data       Type         Machine     Version
 Entry        Phoff      Shoff        Flags       Ehsize
 Phentsize    Phnum      Shentsz      Shnum       Shstrndx

 1            1          2            3           1
 0x80499cc    0x34       0x96200      0           0x34
 0x20         5          0x28         22          19

  * These are the program headers.  "Offset" is the file offset to the
  * segment.  "Vaddr" is the memory load address.  "Filesz" is the
  * segment size as it appears in the file, and "Memsz" is the size in
  * memory.  Below, the third segment is the code and the fourth is the
  * data: the difference between Filesz and Memsz is .bss

 raid:/nfs/raid/src/dist-18.56/src> dump -o temacs

 temacs:
  ***** PROGRAM EXECUTION HEADER *****
 Type        Offset      Vaddr       Paddr
 Filesz      Memsz       Flags       Align

 6           0x34        0x8048034   0
 0xa0        0xa0        5           0

 3           0xd4        0           0
 0x13        0           4           0

 1           0x34        0x8048034   0
 0x3f2f9     0x3f2f9     5           0x1000

 1           0x3f330     0x8088330   0
 0x215c4     0x25a60     7           0x1000

 2           0x60874     0x80a9874   0
 0x80        0           7           0

 raid:/nfs/raid/src/dist-18.56/src> dump -o xemacs

 xemacs:
  ***** PROGRAM EXECUTION HEADER *****
 Type        Offset      Vaddr       Paddr
 Filesz      Memsz       Flags       Align

 6           0x34        0x8048034   0
 0xa0        0xa0        5           0

 3           0xd4        0           0
 0x13        0           4           0

 1           0x34        0x8048034   0
 0x3f2f9     0x3f2f9     5           0x1000

 1           0x3f330     0x8088330   0
 0x3e4d0     0x3e4d0     7           0x1000

 2           0x60874     0x80a9874   0
 0x80        0           7           0


 */

/* Modified by wtien@urbana.mcd.mot.com of Motorola Inc.
 *
 * The above mechanism does not work if the unexeced ELF file is being
 * re-layout by other applications (such as `strip'). All the applications
 * that re-layout the internal of ELF will layout all sections in ascending
 * order of their file offsets. After the re-layout, the data2 section will
 * still be the LAST section in the section header vector, but its file offset
 * is now being pushed far away down, and causes part of it not to be mapped
 * in (ie. not covered by the load segment entry in PHDR vector), therefore
 * causes the new binary to fail.
 *
 * The solution is to modify the unexec algorithm to insert the new data2
 * section header right before the new bss section header, so their file
 * offsets will be in the ascending order. Since some of the section's (all
 * sections AFTER the bss section) indexes are now changed, we also need to
 * modify some fields to make them point to the right sections. This is done
 * by macro PATCH_INDEX. All the fields that need to be patched are:
 *
 * 1. ELF header e_shstrndx field.
 * 2. section header sh_link and sh_info field.
 * 3. symbol table entry st_shndx field.
 *
 * The above example now should look like:

           **** SECTION HEADER TABLE ****
 [No]    Type    Flags   Addr         Offset       Size          Name
         Link    Info    Adralgn      Entsize

 [1]     1       2       0x80480d4    0xd4         0x13          .interp
         0       0       0x1          0

 [2]     5       2       0x80480e8    0xe8         0x388         .hash
         3       0       0x4          0x4

 [3]     11      2       0x8048470    0x470        0x7f0         .dynsym
         4       1       0x4          0x10

 [4]     3       2       0x8048c60    0xc60        0x3ad         .dynstr
         0       0       0x1          0

 [5]     9       2       0x8049010    0x1010       0x338         .rel.plt
         3       7       0x4          0x8

 [6]     1       6       0x8049348    0x1348       0x3           .init
         0       0       0x4          0

 [7]     1       6       0x804934c    0x134c       0x680         .plt
         0       0       0x4          0x4

 [8]     1       6       0x80499cc    0x19cc       0x3c56f       .text
         0       0       0x4          0

 [9]     1       6       0x8085f3c    0x3df3c      0x3           .fini
         0       0       0x4          0

 [10]    1       2       0x8085f40    0x3df40      0x69c         .rodata
         0       0       0x4          0

 [11]    1       2       0x80865dc    0x3e5dc      0xd51         .rodata1
         0       0       0x4          0

 [12]    1       3       0x8088330    0x3f330      0x20afc       .data
         0       0       0x4          0

 [13]    1       3       0x80a8e2c    0x5fe2c      0x89d         .data1
         0       0       0x4          0

 [14]    1       3       0x80a96cc    0x606cc      0x1a8         .got
         0       0       0x4          0x4

 [15]    6       3       0x80a9874    0x60874      0x80          .dynamic
         4       0       0x4          0x8

 [16]    1       3       0x80a98f4    0x608f4      0x1cf0c       .data
         0       0       0x4          0

 [17]    8       3       0x80c6800    0x7d800      0             .bss
         0       0       0x4          0

 [18]    2       0       0            0x7d800      0x9b90        .symtab
         19      371     0x4          0x10

 [19]    3       0       0            0x87390      0x8526        .strtab
         0       0       0x1          0

 [20]    3       0       0            0x8f8b6      0x93          .shstrtab
         0       0       0x1          0

 [21]    1       0       0            0x8f949      0x68b7        .comment
         0       0       0x1          0

 */

/* We do not use mmap because that fails with NFS.
   Instead we read the whole file, modify it, and write it out.  */

#include <config.h>
#include <unexec.h>

extern void fatal (const char *msgid, ...);

#include <sys/types.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/stat.h>
#include <memory.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#if !defined (__NetBSD__) && !defined (__OpenBSD__)
#include <elf.h>
#endif /* not __NetBSD__ and not __OpenBSD__ */
#include <sys/mman.h>
#if defined (_SYSTYPE_SYSV)
#include <sys/elf_mips.h>
#include <sym.h>
#endif /* _SYSTYPE_SYSV */
#if __sgi
#include <syms.h> /* for HDRR declaration */
#endif /* __sgi */

#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS
#else
#define MAP_ANON 0
#endif
#endif

#ifndef MAP_FAILED
#define MAP_FAILED ((void *) -1)
#endif

#if defined (__alpha__) && !defined (__NetBSD__) && !defined (__OpenBSD__)
/* Declare COFF debugging symbol table.  This used to be in
   /usr/include/sym.h, but this file is no longer included in Red Hat
   5.0 and presumably in any other glibc 2.x based distribution.  */
typedef struct {
	short magic;
	short vstamp;
	int ilineMax;
	int idnMax;
	int ipdMax;
	int isymMax;
	int ioptMax;
	int iauxMax;
	int issMax;
	int issExtMax;
	int ifdMax;
	int crfd;
	int iextMax;
	long cbLine;
	long cbLineOffset;
	long cbDnOffset;
	long cbPdOffset;
	long cbSymOffset;
	long cbOptOffset;
	long cbAuxOffset;
	long cbSsOffset;
	long cbSsExtOffset;
	long cbFdOffset;
	long cbRfdOffset;
	long cbExtOffset;
} HDRR, *pHDRR;
#define cbHDRR sizeof (HDRR)
#define hdrNil ((pHDRR)0)
#endif

#ifdef __NetBSD__
/*
 * NetBSD does not have normal-looking user-land ELF support.
 */
# if defined __alpha__ || defined __sparc_v9__
#  define ELFSIZE	64
# else
#  define ELFSIZE	32
# endif
# include <sys/exec_elf.h>

# ifndef PT_LOAD
#  define PT_LOAD	Elf_pt_load
#  if 0						/* was in pkgsrc patches for 20.7 */
#   define SHT_PROGBITS Elf_sht_progbits
#  endif
#  define SHT_SYMTAB	Elf_sht_symtab
#  define SHT_DYNSYM	Elf_sht_dynsym
#  define SHT_NULL	Elf_sht_null
#  define SHT_NOBITS	Elf_sht_nobits
#  define SHT_REL	Elf_sht_rel
#  define SHT_RELA	Elf_sht_rela

#  define SHN_UNDEF	Elf_eshn_undefined
#  define SHN_ABS	Elf_eshn_absolute
#  define SHN_COMMON	Elf_eshn_common
# endif /* !PT_LOAD */

# ifdef __alpha__
#  include <sys/exec_ecoff.h>
#  define HDRR		struct ecoff_symhdr
#  define pHDRR		HDRR *
# endif /* __alpha__ */

#ifdef __mips__			/* was in pkgsrc patches for 20.7 */
# define SHT_MIPS_DEBUG	DT_MIPS_FLAGS
# define HDRR		struct Elf_Shdr
#endif /* __mips__ */
#endif /* __NetBSD__ */

#ifdef __OpenBSD__
# include <sys/exec_elf.h>
#endif

#if __GNU_LIBRARY__ - 0 >= 6
# include <link.h>	/* get ElfW etc */
#endif

#ifndef ElfW
# ifdef __STDC__
#  define ElfBitsW(bits, type) Elf##bits##_##type
# else
#  define ElfBitsW(bits, type) Elf/**/bits/**/_/**/type
# endif
# ifdef _LP64
#  define ELFSIZE 64
# else
#  define ELFSIZE 32
# endif
  /* This macro expands `bits' before invoking ElfBitsW.  */
# define ElfExpandBitsW(bits, type) ElfBitsW (bits, type)
# define ElfW(type) ElfExpandBitsW (ELFSIZE, type)
#endif

/* Get the address of a particular section or program header entry,
 * accounting for the size of the entries.
 */
/*
   On PPC Reference Platform running Solaris 2.5.1
   the plt section is also of type NOBI like the bss section.
   (not really stored) and therefore sections after the bss
   section start at the plt offset. The plt section is always
   the one just before the bss section.
   Thus, we modify the test from
      if (NEW_SECTION_H (nn).sh_offset >= new_data2_offset)
   to
      if (NEW_SECTION_H (nn).sh_offset >=
               OLD_SECTION_H (old_bss_index-1).sh_offset)
   This is just a hack. We should put the new data section
   before the .plt section.
   And we should not have this routine at all but use
   the libelf library to read the old file and create the new
   file.
   The changed code is minimal and depends on prep set in m/prep.h
   Erik Deumens
   Quantum Theory Project
   University of Florida
   deumens@qtp.ufl.edu
   Apr 23, 1996
   */

#define OLD_SECTION_H(n) \
     (*(ElfW (Shdr) *) ((byte *) old_section_h + old_file_h->e_shentsize * (n)))
#define NEW_SECTION_H(n) \
     (*(ElfW (Shdr) *) ((byte *) new_section_h + new_file_h->e_shentsize * (n)))
#define NEW_PROGRAM_H(n) \
     (*(ElfW (Phdr) *) ((byte *) new_program_h + new_file_h->e_phentsize * (n)))

#define PATCH_INDEX(n) \
  do { \
	 if ((int) (n) >= old_bss_index) \
	   (n)++; } while (0)
typedef unsigned char byte;

/* Round X up to a multiple of Y.  */

static ElfW (Addr)
round_up (ElfW (Addr) x, ElfW (Addr) y)
{
  int rem = x % y;
  if (rem == 0)
    return x;
  return x - rem + y;
}

/* Return the index of the section named NAME.
   SECTION_NAMES, FILE_NAME and FILE_H give information
   about the file we are looking in.

   If we don't find the section NAME, that is a fatal error
   if NOERROR is 0; we return -1 if NOERROR is nonzero.  */

static int
find_section (const char *name, const char *section_names, const char *file_name,
	      ElfW (Ehdr) *old_file_h, ElfW (Shdr) *old_section_h, int noerror)
{
  int idx;

  for (idx = 1; idx < old_file_h->e_shnum; idx++)
    {
#ifdef DEBUG
      fprintf (stderr, "Looking for %s - found %s\n", name,
	       section_names + OLD_SECTION_H (idx).sh_name);
#endif
      if (!strcmp (section_names + OLD_SECTION_H (idx).sh_name,
		   name))
	break;
    }
  if (idx == old_file_h->e_shnum)
    {
      if (noerror)
	return -1;
      else
	fatal ("Can't find %s in %s.\n", name, file_name);
    }

  return idx;
}

/* ****************************************************************
 * unexec
 *
 * driving logic.
 *
 * In ELF, this works by replacing the old .bss section with a new
 * .data section, and inserting an empty .bss immediately afterwards.
 *
 */
void
unexec (const char *new_name, const char *old_name)
{
  int new_file, old_file, new_file_size;

#if defined (emacs) || !defined (DEBUG)
  void *new_break;
#endif

  /* Pointers to the base of the image of the two files.  */
  caddr_t old_base, new_base;

#if MAP_ANON == 0
  int mmap_fd;
#else
# define mmap_fd -1
#endif

  /* Pointers to the file, program and section headers for the old and
     new files.  */
  ElfW (Ehdr) *old_file_h, *new_file_h;
  ElfW (Phdr) *old_program_h, *new_program_h;
  ElfW (Shdr) *old_section_h, *new_section_h;

  /* Point to the section name table in the old file.  */
  char *old_section_names;

  ElfW (Addr) old_bss_addr, new_bss_addr;
  ElfW (Word) old_bss_size, new_data2_size;
  ElfW (Off)  new_data2_offset;
  ElfW (Addr) new_data2_addr;
  ElfW (Off)  old_bss_offset;
  ElfW (Word) new_data2_incr;

  int n, nn;
  int old_bss_index, old_sbss_index, old_plt_index;
  int old_data_index, new_data2_index;
#if defined _SYSTYPE_SYSV || defined __sgi
  int old_mdebug_index;
#endif
  struct stat stat_buf;
  int old_file_size;

  /* Open the old file, allocate a buffer of the right size, and read
     in the file contents.  */

  old_file = open (old_name, O_RDONLY);

  if (old_file < 0)
    fatal ("Can't open %s for reading: errno %d\n", old_name, errno);

  if (fstat (old_file, &stat_buf) == -1)
    fatal ("Can't fstat (%s): errno %d\n", old_name, errno);

#if MAP_ANON == 0
  mmap_fd = open ("/dev/zero", O_RDONLY);
  if (mmap_fd < 0)
    fatal ("Can't open /dev/zero for reading: errno %d\n", errno, 0);
#endif

  /* We cannot use malloc here because that may use sbrk.  If it does,
     we'd dump our temporary buffers with Emacs, and we'd have to be
     extra careful to use the correct value of sbrk(0) after
     allocating all buffers in the code below, which we aren't.  */
  old_file_size = stat_buf.st_size;
  old_base = mmap (NULL, old_file_size, PROT_READ | PROT_WRITE,
		   MAP_ANON | MAP_PRIVATE, mmap_fd, 0);
  if (old_base == MAP_FAILED)
    fatal ("Can't allocate buffer for %s\n", old_name, 0);

  if (read (old_file, old_base, stat_buf.st_size) != stat_buf.st_size)
    fatal ("Didn't read all of %s: errno %d\n", old_name, errno);

  /* Get pointers to headers & section names */

  old_file_h = (ElfW (Ehdr) *) old_base;
  old_program_h = (ElfW (Phdr) *) ((byte *) old_base + old_file_h->e_phoff);
  old_section_h = (ElfW (Shdr) *) ((byte *) old_base + old_file_h->e_shoff);
  old_section_names = (char *) old_base
    + OLD_SECTION_H (old_file_h->e_shstrndx).sh_offset;

  /* Find the mdebug section, if any.  */

#if defined _SYSTYPE_SYSV || defined __sgi
  old_mdebug_index = find_section (".mdebug", old_section_names,
				   old_name, old_file_h, old_section_h, 1);
#endif

  /* Find the old .bss section.  Figure out parameters of the new
     data2 and bss sections.  */

  old_bss_index = find_section (".bss", old_section_names,
				old_name, old_file_h, old_section_h, 0);

  old_sbss_index = find_section (".sbss", old_section_names,
				 old_name, old_file_h, old_section_h, 1);
  if (old_sbss_index != -1)
    if (OLD_SECTION_H (old_sbss_index).sh_type != SHT_NOBITS)
      old_sbss_index = -1;

  /* PowerPC64 has .plt in the BSS section.  */
  old_plt_index = find_section (".plt", old_section_names,
				old_name, old_file_h, old_section_h, 1);
  if (old_plt_index != -1)
    if (OLD_SECTION_H (old_plt_index).sh_type != SHT_NOBITS)
      old_plt_index = -1;

  if (old_sbss_index == -1 && old_plt_index == -1)
    {
      old_bss_addr = OLD_SECTION_H (old_bss_index).sh_addr;
      old_bss_size = OLD_SECTION_H (old_bss_index).sh_size;
      old_bss_offset = OLD_SECTION_H (old_bss_index).sh_offset;
      new_data2_index = old_bss_index;
    }
  else if (old_plt_index != -1
	   && (old_sbss_index == -1
	       || (OLD_SECTION_H (old_sbss_index).sh_addr
		   > OLD_SECTION_H (old_plt_index).sh_addr)))
    {
      old_bss_addr = OLD_SECTION_H (old_plt_index).sh_addr;
      old_bss_size = OLD_SECTION_H (old_bss_index).sh_size
	+ OLD_SECTION_H (old_plt_index).sh_size;
      if (old_sbss_index != -1)
	old_bss_size += OLD_SECTION_H (old_sbss_index).sh_size;
      old_bss_offset = OLD_SECTION_H (old_plt_index).sh_offset;
      new_data2_index = old_plt_index;
    }
  else
    {
      old_bss_addr = OLD_SECTION_H (old_sbss_index).sh_addr;
      old_bss_size = OLD_SECTION_H (old_bss_index).sh_size
	+ OLD_SECTION_H (old_sbss_index).sh_size;
      old_bss_offset = OLD_SECTION_H (old_sbss_index).sh_offset;
      new_data2_index = old_sbss_index;
    }

  /* Find the old .data section.  Figure out parameters of
     the new data2 and bss sections.  */

  old_data_index = find_section (".data", old_section_names,
				 old_name, old_file_h, old_section_h, 0);

#if defined (emacs) || !defined (DEBUG)
  new_break = sbrk (0);
  new_bss_addr = (ElfW (Addr)) new_break;
#else
  new_bss_addr = old_bss_addr + old_bss_size + 0x1234;
#endif
  new_data2_addr = old_bss_addr;
  new_data2_size = new_bss_addr - old_bss_addr;
  new_data2_offset = OLD_SECTION_H (old_data_index).sh_offset
    + (new_data2_addr - OLD_SECTION_H (old_data_index).sh_addr);
  /* This is the amount by which the sections following the bss sections
     must be shifted in the image.  It can differ from new_data2_size if
     the end of the old .data section (and thus the offset of the .bss
     section) was unaligned.  */
  new_data2_incr = new_data2_size + (new_data2_offset - old_bss_offset);

#ifdef DEBUG
  fprintf (stderr, "old_bss_index %d\n", old_bss_index);
  fprintf (stderr, "old_bss_addr %x\n", old_bss_addr);
  fprintf (stderr, "old_bss_size %x\n", old_bss_size);
  fprintf (stderr, "old_bss_offset %x\n", old_bss_offset);
  fprintf (stderr, "new_bss_addr %x\n", new_bss_addr);
  fprintf (stderr, "new_data2_addr %x\n", new_data2_addr);
  fprintf (stderr, "new_data2_size %x\n", new_data2_size);
  fprintf (stderr, "new_data2_offset %x\n", new_data2_offset);
  fprintf (stderr, "new_data2_incr %x\n", new_data2_incr);
#endif

  if ((uintptr_t) new_bss_addr < (uintptr_t) old_bss_addr + old_bss_size)
    fatal (".bss shrank when undumping???\n", 0, 0);

  /* Set the output file to the right size.  Allocate a buffer to hold
     the image of the new file.  Set pointers to various interesting
     objects.  stat_buf still has old_file data.  */

  new_file = open (new_name, O_RDWR | O_CREAT, 0666);
  if (new_file < 0)
    fatal ("Can't creat (%s): errno %d\n", new_name, errno);

  new_file_size = stat_buf.st_size + old_file_h->e_shentsize + new_data2_incr;

  if (ftruncate (new_file, new_file_size))
    fatal ("Can't ftruncate (%s): errno %d\n", new_name, errno);

  new_base = mmap (NULL, new_file_size, PROT_READ | PROT_WRITE,
		   MAP_ANON | MAP_PRIVATE, mmap_fd, 0);
  if (new_base == MAP_FAILED)
    fatal ("Can't allocate buffer for %s\n", old_name, 0);

  new_file_h = (ElfW (Ehdr) *) new_base;
  new_program_h = (ElfW (Phdr) *) ((byte *) new_base + old_file_h->e_phoff);
  new_section_h = (ElfW (Shdr) *)
    ((byte *) new_base + old_file_h->e_shoff + new_data2_incr);

  /* Make our new file, program and section headers as copies of the
     originals.  */

  memcpy (new_file_h, old_file_h, old_file_h->e_ehsize);
  memcpy (new_program_h, old_program_h,
	  old_file_h->e_phnum * old_file_h->e_phentsize);

  /* Modify the e_shstrndx if necessary. */
  PATCH_INDEX (new_file_h->e_shstrndx);

  /* Fix up file header.  We'll add one section.  Section header is
     further away now.  */

  new_file_h->e_shoff += new_data2_incr;
  new_file_h->e_shnum += 1;

#ifdef DEBUG
  fprintf (stderr, "Old section offset %x\n", old_file_h->e_shoff);
  fprintf (stderr, "Old section count %d\n", old_file_h->e_shnum);
  fprintf (stderr, "New section offset %x\n", new_file_h->e_shoff);
  fprintf (stderr, "New section count %d\n", new_file_h->e_shnum);
#endif

  /* Fix up a new program header.  Extend the writable data segment so
     that the bss area is covered too. Find that segment by looking
     for a segment that ends just before the .bss area.  Make sure
     that no segments are above the new .data2.  Put a loop at the end
     to adjust the offset and address of any segment that is above
     data2, just in case we decide to allow this later.  */

  for (n = new_file_h->e_phnum - 1; n >= 0; n--)
    {
      /* Compute maximum of all requirements for alignment of section.  */
      ElfW (Word) alignment = (NEW_PROGRAM_H (n)).p_align;
      if ((OLD_SECTION_H (old_bss_index)).sh_addralign > alignment)
	alignment = OLD_SECTION_H (old_bss_index).sh_addralign;

#ifdef __sgi
	  /* According to r02kar@x4u2.desy.de (Karsten Kuenne)
	     and oliva@gnu.org (Alexandre Oliva), on IRIX 5.2, we
	     always get "Program segment above .bss" when dumping
	     when the executable doesn't have an sbss section.  */
      if (old_sbss_index != -1)
#endif /* __sgi */
      if (NEW_PROGRAM_H (n).p_vaddr + NEW_PROGRAM_H (n).p_filesz
	  > (old_sbss_index == -1
	     ? old_bss_addr
	     : round_up (old_bss_addr, alignment)))
	  fatal ("Program segment above .bss in %s\n", old_name, 0);

      if (NEW_PROGRAM_H (n).p_type == PT_LOAD
	  && (round_up ((NEW_PROGRAM_H (n)).p_vaddr
			+ (NEW_PROGRAM_H (n)).p_filesz,
			alignment)
	      == round_up (old_bss_addr, alignment)))
	break;
    }
  if (n < 0)
    fatal ("Couldn't find segment next to .bss in %s\n", old_name, 0);

  /* Make sure that the size includes any padding before the old .bss
     section.  */
  NEW_PROGRAM_H (n).p_filesz = new_bss_addr - NEW_PROGRAM_H (n).p_vaddr;
  NEW_PROGRAM_H (n).p_memsz = NEW_PROGRAM_H (n).p_filesz;

#if 0 /* Maybe allow section after data2 - does this ever happen? */
  for (n = new_file_h->e_phnum - 1; n >= 0; n--)
    {
      if (NEW_PROGRAM_H (n).p_vaddr
	  && NEW_PROGRAM_H (n).p_vaddr >= new_data2_addr)
	NEW_PROGRAM_H (n).p_vaddr += new_data2_size - old_bss_size;

      if (NEW_PROGRAM_H (n).p_offset >= new_data2_offset)
	NEW_PROGRAM_H (n).p_offset += new_data2_incr;
    }
#endif

  /* Fix up section headers based on new .data2 section.  Any section
     whose offset or virtual address is after the new .data2 section
     gets its value adjusted.  .bss size becomes zero and new address
     is set.  data2 section header gets added by copying the existing
     .data header and modifying the offset, address and size.  */

  /* Walk through all section headers, insert the new data2 section right
     before the new bss section. */
  for (n = 1, nn = 1; n < (int) old_file_h->e_shnum; n++, nn++)
    {
      caddr_t src;
      /* If it is (s)bss section, insert the new data2 section before it.  */
      /* new_data2_index is the index of either old_sbss or old_bss, that was
	 chosen as a section for new_data2.   */
      if (n == new_data2_index)
	{
	  /* Steal the data section header for this data2 section. */
	  memcpy (&NEW_SECTION_H (nn), &OLD_SECTION_H (old_data_index),
		  new_file_h->e_shentsize);

	  NEW_SECTION_H (nn).sh_addr = new_data2_addr;
	  NEW_SECTION_H (nn).sh_offset = new_data2_offset;
	  NEW_SECTION_H (nn).sh_size = new_data2_size;
	  /* Use the bss section's alignment. This will assure that the
	     new data2 section always be placed in the same spot as the old
	     bss section by any other application. */
	  NEW_SECTION_H (nn).sh_addralign = OLD_SECTION_H (n).sh_addralign;

	  /* Now copy over what we have in the memory now. */
	  memcpy (NEW_SECTION_H (nn).sh_offset + new_base,
		  (caddr_t) OLD_SECTION_H (n).sh_addr,
		  new_data2_size);
	  nn++;
	}

      memcpy (&NEW_SECTION_H (nn), &OLD_SECTION_H (n),
	      old_file_h->e_shentsize);

      if (n == old_bss_index
	  /* The new bss and sbss section's size is zero, and its file offset
	     and virtual address should be off by NEW_DATA2_SIZE.  */
	  || n == old_sbss_index || n == old_plt_index
	  )
	{
	  /* NN should be `old_s?bss_index + 1' at this point. */
	  NEW_SECTION_H (nn).sh_offset = new_data2_offset + new_data2_size;
	  NEW_SECTION_H (nn).sh_addr = new_data2_addr + new_data2_size;
	  /* Let the new bss section address alignment be the same as the
	     section address alignment followed the old bss section, so
	     this section will be placed in exactly the same place. */
	  NEW_SECTION_H (nn).sh_addralign = OLD_SECTION_H (nn).sh_addralign;
	  NEW_SECTION_H (nn).sh_size = 0;
	}
      else
	{
	  /* Any section that was originally placed after the .bss
	     section should now be off by NEW_DATA2_INCR.  If a
	     section overlaps the .bss section, consider it to be
	     placed after the .bss section.  Overlap can occur if the
	     section just before .bss has less-strict alignment; this
	     was observed between .symtab and .bss on Solaris 2.5.1
	     (sparc) with GCC snapshot 960602.

> dump -h temacs

temacs:

	   **** SECTION HEADER TABLE ****
[No]	Type	Flags	Addr         Offset       Size        	Name
	Link	Info	Adralgn      Entsize

[22]	1	3	0x335150     0x315150     0x4          	.data.rel.local
	0	0	0x4          0

[23]	8	3	0x335158     0x315158     0x42720      	.bss
	0	0	0x8          0

[24]	2	0	0            0x315154     0x1c9d0      	.symtab
	25	1709	0x4          0x10
	  */

	  if (NEW_SECTION_H (nn).sh_offset >= old_bss_offset
	      || (NEW_SECTION_H (nn).sh_offset + NEW_SECTION_H (nn).sh_size
		  > new_data2_offset))
	    NEW_SECTION_H (nn).sh_offset += new_data2_incr;

	  /* Any section that was originally placed after the section
	     header table should now be off by the size of one section
	     header table entry.  */
	  if (NEW_SECTION_H (nn).sh_offset > new_file_h->e_shoff)
	    NEW_SECTION_H (nn).sh_offset += new_file_h->e_shentsize;
	}

      /* If any section hdr refers to the section after the new .data
	 section, make it refer to next one because we have inserted
	 a new section in between.  */

      PATCH_INDEX (NEW_SECTION_H (nn).sh_link);
      /* For symbol tables, info is a symbol table index,
	 so don't change it.  */
      if (NEW_SECTION_H (nn).sh_type != SHT_SYMTAB
	  && NEW_SECTION_H (nn).sh_type != SHT_DYNSYM)
	PATCH_INDEX (NEW_SECTION_H (nn).sh_info);

      if (old_sbss_index != -1)
	if (!strcmp (old_section_names + NEW_SECTION_H (nn).sh_name, ".sbss"))
	  {
	    NEW_SECTION_H (nn).sh_offset =
	      round_up (NEW_SECTION_H (nn).sh_offset,
			NEW_SECTION_H (nn).sh_addralign);
	    NEW_SECTION_H (nn).sh_type = SHT_PROGBITS;
	  }

      /* Now, start to copy the content of sections.  */
      if (NEW_SECTION_H (nn).sh_type == SHT_NULL
	  || NEW_SECTION_H (nn).sh_type == SHT_NOBITS)
	continue;

      /* Write out the sections. .data and .data1 (and data2, called
	 ".data" in the strings table) get copied from the current process
	 instead of the old file.  */
      if (!strcmp (old_section_names + NEW_SECTION_H (n).sh_name, ".data")
	  || !strcmp ((old_section_names + NEW_SECTION_H (n).sh_name),
		      ".sdata")
	  || !strcmp ((old_section_names + NEW_SECTION_H (n).sh_name),
		      ".lit4")
	  || !strcmp ((old_section_names + NEW_SECTION_H (n).sh_name),
		      ".lit8")
	  /* The conditional bit below was in Oliva's original code
	     (1999-08-25) and seems to have been dropped by mistake
	     subsequently.  It prevents a crash at startup under X in
	     `IRIX64 6.5 6.5.17m', whether compiled on that release or
	     an earlier one.  It causes no trouble on the other ELF
	     platforms I could test (Irix 6.5.15m, Solaris 8, Debian
	     Potato x86, Debian Woody SPARC); however, it's reported
	     to cause crashes under some version of GNU/Linux.  It's
	     not yet clear what's changed in that Irix version to
	     cause the problem, or why the fix sometimes fails under
	     GNU/Linux.  There's probably no good reason to have
	     something Irix-specific here, but this will have to do
	     for now.  IRIX6_5 is the most specific macro we have to
	     test.  -- fx 2002-10-01

	     The issue _looks_ as though it's gone away on 6.5.18m,
	     but maybe it's still lurking, to be triggered by some
	     change in the binary.  It appears to concern the dynamic
	     loader, but I never got anywhere with an SGI support call
	     seeking clues.  -- fx 2002-11-29.  */
#ifdef IRIX6_5
	  || !strcmp ((old_section_names + NEW_SECTION_H (n).sh_name),
		      ".got")
#endif
	  || !strcmp ((old_section_names + NEW_SECTION_H (n).sh_name),
		      ".sdata1")
	  || !strcmp ((old_section_names + NEW_SECTION_H (n).sh_name),
		      ".data1")
	  || !strcmp ((old_section_names + NEW_SECTION_H (n).sh_name),
		      ".sbss"))
	src = (caddr_t) OLD_SECTION_H (n).sh_addr;
      else
	src = old_base + OLD_SECTION_H (n).sh_offset;

      memcpy (NEW_SECTION_H (nn).sh_offset + new_base, src,
	      NEW_SECTION_H (nn).sh_size);

#if defined __alpha__ && !defined __OpenBSD__
      /* Update Alpha COFF symbol table: */
      if (strcmp (old_section_names + OLD_SECTION_H (n).sh_name, ".mdebug")
	  == 0)
	{
	  pHDRR symhdr = (pHDRR) (NEW_SECTION_H (nn).sh_offset + new_base);

	  symhdr->cbLineOffset += new_data2_size;
	  symhdr->cbDnOffset += new_data2_size;
	  symhdr->cbPdOffset += new_data2_size;
	  symhdr->cbSymOffset += new_data2_size;
	  symhdr->cbOptOffset += new_data2_size;
	  symhdr->cbAuxOffset += new_data2_size;
	  symhdr->cbSsOffset += new_data2_size;
	  symhdr->cbSsExtOffset += new_data2_size;
	  symhdr->cbFdOffset += new_data2_size;
	  symhdr->cbRfdOffset += new_data2_size;
	  symhdr->cbExtOffset += new_data2_size;
	}
#endif /* __alpha__ && !__OpenBSD__ */

#if defined (_SYSTYPE_SYSV)
      if (NEW_SECTION_H (nn).sh_type == SHT_MIPS_DEBUG
	  && old_mdebug_index != -1)
	{
	  int diff = NEW_SECTION_H (nn).sh_offset
		- OLD_SECTION_H (old_mdebug_index).sh_offset;
	  HDRR *phdr = (HDRR *)(NEW_SECTION_H (nn).sh_offset + new_base);

	  if (diff)
	    {
	      phdr->cbLineOffset += diff;
	      phdr->cbDnOffset   += diff;
	      phdr->cbPdOffset   += diff;
	      phdr->cbSymOffset  += diff;
	      phdr->cbOptOffset  += diff;
	      phdr->cbAuxOffset  += diff;
	      phdr->cbSsOffset   += diff;
	      phdr->cbSsExtOffset += diff;
	      phdr->cbFdOffset   += diff;
	      phdr->cbRfdOffset  += diff;
	      phdr->cbExtOffset  += diff;
	    }
	}
#endif /* _SYSTYPE_SYSV */

#if __sgi
      /* Adjust  the HDRR offsets in .mdebug and copy the
	 line data if it's in its usual 'hole' in the object.
	 Makes the new file debuggable with dbx.
	 patches up two problems: the absolute file offsets
	 in the HDRR record of .mdebug (see /usr/include/syms.h), and
	 the ld bug that gets the line table in a hole in the
	 elf file rather than in the .mdebug section proper.
	 David Anderson. davea@sgi.com  Jan 16,1994.  */
      if (n == old_mdebug_index)
	{
#define MDEBUGADJUST(__ct,__fileaddr)		\
  if (n_phdrr->__ct > 0)			\
    {						\
      n_phdrr->__fileaddr += movement;		\
    }

	  HDRR * o_phdrr = (HDRR *)((byte *)old_base + OLD_SECTION_H (n).sh_offset);
	  HDRR * n_phdrr = (HDRR *)((byte *)new_base + NEW_SECTION_H (nn).sh_offset);
	  unsigned movement = new_data2_size;

	  MDEBUGADJUST (idnMax, cbDnOffset);
	  MDEBUGADJUST (ipdMax, cbPdOffset);
	  MDEBUGADJUST (isymMax, cbSymOffset);
	  MDEBUGADJUST (ioptMax, cbOptOffset);
	  MDEBUGADJUST (iauxMax, cbAuxOffset);
	  MDEBUGADJUST (issMax, cbSsOffset);
	  MDEBUGADJUST (issExtMax, cbSsExtOffset);
	  MDEBUGADJUST (ifdMax, cbFdOffset);
	  MDEBUGADJUST (crfd, cbRfdOffset);
	  MDEBUGADJUST (iextMax, cbExtOffset);
	  /* The Line Section, being possible off in a hole of the object,
	     requires special handling.  */
	  if (n_phdrr->cbLine > 0)
	    {
	      if (o_phdrr->cbLineOffset > (OLD_SECTION_H (n).sh_offset
					   + OLD_SECTION_H (n).sh_size))
		{
		  /* line data is in a hole in elf. do special copy and adjust
		     for this ld mistake.
		     */
		  n_phdrr->cbLineOffset += movement;

		  memcpy (n_phdrr->cbLineOffset + new_base,
			  o_phdrr->cbLineOffset + old_base, n_phdrr->cbLine);
		}
	      else
		{
		  /* somehow line data is in .mdebug as it is supposed to be.  */
		  MDEBUGADJUST (cbLine, cbLineOffset);
		}
	    }
	}
#endif /* __sgi */

      /* If it is the symbol table, its st_shndx field needs to be patched.  */
      if (NEW_SECTION_H (nn).sh_type == SHT_SYMTAB
	  || NEW_SECTION_H (nn).sh_type == SHT_DYNSYM)
	{
	  ElfW (Shdr) *spt = &NEW_SECTION_H (nn);
	  unsigned int num = spt->sh_size / spt->sh_entsize;
	  ElfW (Sym) * sym = (ElfW (Sym) *) (NEW_SECTION_H (nn).sh_offset +
					   new_base);
	  for (; num--; sym++)
	    {
	      if ((sym->st_shndx == SHN_UNDEF)
		  || (sym->st_shndx == SHN_ABS)
		  || (sym->st_shndx == SHN_COMMON))
		continue;

	      PATCH_INDEX (sym->st_shndx);
	    }
	}
    }

  /* Update the symbol values of _edata and _end.  */
  for (n = new_file_h->e_shnum - 1; n; n--)
    {
      byte *symnames;
      ElfW (Sym) *symp, *symendp;

      if (NEW_SECTION_H (n).sh_type != SHT_DYNSYM
	  && NEW_SECTION_H (n).sh_type != SHT_SYMTAB)
	continue;

      symnames = ((byte *) new_base
		  + NEW_SECTION_H (NEW_SECTION_H (n).sh_link).sh_offset);
      symp = (ElfW (Sym) *) (NEW_SECTION_H (n).sh_offset + new_base);
      symendp = (ElfW (Sym) *) ((byte *)symp + NEW_SECTION_H (n).sh_size);

      for (; symp < symendp; symp ++)
	{
	  if (strcmp ((char *) (symnames + symp->st_name), "_end") == 0
	      || strcmp ((char *) (symnames + symp->st_name), "end") == 0
	      || strcmp ((char *) (symnames + symp->st_name), "_edata") == 0
	      || strcmp ((char *) (symnames + symp->st_name), "edata") == 0)
	    memcpy (&symp->st_value, &new_bss_addr, sizeof (new_bss_addr));

	  /* Strictly speaking, #ifdef below is not necessary.  But we
	     keep it to indicate that this kind of change may also be
	     necessary for other unexecs to support GNUstep.  */
#ifdef NS_IMPL_GNUSTEP
	  /* ObjC runtime modifies the values of some data structures
	     such as classes and selectors in the .data section after
	     loading.  As the dump process copies the .data section
	     from the current process, that causes problems when the
	     modified classes are reinitialized in the dumped
	     executable.  We copy such data from the old file, not
	     from the current process.  */
	  if (strncmp ((char *) (symnames + symp->st_name),
		       "_OBJC_", sizeof ("_OBJC_") - 1) == 0)
	    {
	      caddr_t old, new;

	      new = ((symp->st_value - NEW_SECTION_H (symp->st_shndx).sh_addr)
		     + NEW_SECTION_H (symp->st_shndx).sh_offset + new_base);
	      /* "Unpatch" index.  */
	      nn = symp->st_shndx;
	      if (nn > old_bss_index)
		nn--;
	      if (nn == old_bss_index)
		memset (new, 0, symp->st_size);
	      else
		{
		  old = ((symp->st_value
			  - NEW_SECTION_H (symp->st_shndx).sh_addr)
			 + OLD_SECTION_H (nn).sh_offset + old_base);
		  memcpy (new, old, symp->st_size);
		}
	    }
#endif
	}
    }

  /* This loop seeks out relocation sections for the data section, so
     that it can undo relocations performed by the runtime linker.  */
  for (n = new_file_h->e_shnum - 1; n; n--)
    {
      ElfW (Shdr) section = NEW_SECTION_H (n);

      /* Cause a compilation error if anyone uses n instead of nn below.  */
      #define n ((void) 0);
      n /* Prevent 'macro "n" is not used' warnings.  */

      switch (section.sh_type)
	{
	default:
	  break;
	case SHT_REL:
	case SHT_RELA:
	  /* This code handles two different size structs, but there should
	     be no harm in that provided that r_offset is always the first
	     member.  */
	  nn = section.sh_info;
	  if (!strcmp (old_section_names + NEW_SECTION_H (nn).sh_name, ".data")
	      || !strcmp ((old_section_names + NEW_SECTION_H (nn).sh_name),
			  ".sdata")
	      || !strcmp ((old_section_names + NEW_SECTION_H (nn).sh_name),
			  ".lit4")
	      || !strcmp ((old_section_names + NEW_SECTION_H (nn).sh_name),
			  ".lit8")
#ifdef IRIX6_5			/* see above */
	      || !strcmp ((old_section_names + NEW_SECTION_H (nn).sh_name),
			  ".got")
#endif
	      || !strcmp ((old_section_names + NEW_SECTION_H (nn).sh_name),
			  ".sdata1")
	      || !strcmp ((old_section_names + NEW_SECTION_H (nn).sh_name),
			  ".data1"))
	    {
	      ElfW (Addr) offset = (NEW_SECTION_H (nn).sh_addr
				   - NEW_SECTION_H (nn).sh_offset);
	      caddr_t reloc = old_base + section.sh_offset, end;
	      for (end = reloc + section.sh_size; reloc < end;
		   reloc += section.sh_entsize)
		{
		  ElfW (Addr) addr = ((ElfW (Rel) *) reloc)->r_offset - offset;
#ifdef __alpha__
		  /* The Alpha ELF binutils currently have a bug that
		     sometimes results in relocs that contain all
		     zeroes.  Work around this for now...  */
		  if (((ElfW (Rel) *) reloc)->r_offset == 0)
		    continue;
#endif
		  memcpy (new_base + addr, old_base + addr, sizeof (ElfW (Addr)));
		}
	    }
	  break;
	}

      #undef n
    }

  /* Write out new_file, and free the buffers.  */

  if (write (new_file, new_base, new_file_size) != new_file_size)
    fatal ("Didn't write %d bytes to %s: errno %d\n",
	   new_file_size, new_name, errno);
  munmap (old_base, old_file_size);
  munmap (new_base, new_file_size);

  /* Close the files and make the new file executable.  */

#if MAP_ANON == 0
  close (mmap_fd);
#endif

  if (close (old_file))
    fatal ("Can't close (%s): errno %d\n", old_name, errno);

  if (close (new_file))
    fatal ("Can't close (%s): errno %d\n", new_name, errno);

  if (stat (new_name, &stat_buf) == -1)
    fatal ("Can't stat (%s): errno %d\n", new_name, errno);

  n = umask (777);
  umask (n);
  stat_buf.st_mode |= 0111 & ~n;
  if (chmod (new_name, stat_buf.st_mode) == -1)
    fatal ("Can't chmod (%s): errno %d\n", new_name, errno);
}
