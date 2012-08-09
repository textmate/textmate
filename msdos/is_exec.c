/* IS_EXEC.C
 *
 * Copyright (C) 1995 DJ Delorie
 * Copyright (C) 1994 Eli Zaretskii <eliz@is.elta.co.il>
 *
 * (See the README file in this directory for the copyright and license
 * history of this file.)
 *
 * This file is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This file is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this file.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Commentary:
 *
 * Given a filename or a file handle, and the extension of the file,
 * determine if the file is executable.
 * First, the file extension is checked in case it uniquely identifies
 * the file as either an executable or not.  Failing this, the first
 * two bytes of the file are tested for known signatures of executable
 * files.
 *
 */

#include <libc/stubs.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <dpmi.h>
#include <go32.h>
#include <io.h>
#include <libc/farptrgs.h>
#include <libc/dosio.h>

extern unsigned short _djstat_flags;
unsigned short        _get_magic(const char *, int);
int                   _is_executable(const char *, int, const char *);

/*
 * Read a MAGIC NUMBER from a given file.  These are the first
 * two bytes of the file, if we look at them as an unsigned short. */

#define _STAT_EXEC_EXT      2   /* get execute bits from file extension? */
#define _STAT_EXEC_MAGIC    4   /* get execute bits from magic signature? */

unsigned short
_get_magic(const char *s, int fh)
{
  __dpmi_regs          regs;
  unsigned short       retval;
  unsigned short       fpos_high = 0, fpos_low = 0;
  int                  read_fail = 0;

  /* If given a pathname, open the file. */
  if (s)
  {
    int handle;
    if((handle = _open(s,0)) == -1)
      return 0;
    regs.x.bx = handle;
  }
  /* Else file already open.  Remember its current file position
     and move to beginning of file. */
  else
  {
    regs.x.ax = 0x4201;		/* set pointer from current position */
    regs.x.bx = fh;
    regs.x.cx = regs.x.dx = 0;	/* move 0 bytes (i.e., stay put) */
    __dpmi_int(0x21, &regs);
    if (regs.x.flags & 1)
    {
      errno = __doserr_to_errno(regs.x.ax);
      return 0;
    }
    fpos_high = regs.x.dx;	/* got current position */
    fpos_low  = regs.x.ax;

    regs.x.ax = 0x4200;		/* set pointer from the beginning of file */
    regs.x.cx = regs.x.dx = 0;	/* move to beginning of file */
    __dpmi_int(0x21, &regs);
    if (regs.x.flags & 1)
    {
      errno = __doserr_to_errno(regs.x.ax);
      return 0;
    }
  }
  regs.x.ds = __tb_segment;
  regs.x.dx = __tb_offset;

  /* Read 2 bytes from the file. */
  regs.x.ax = 0x3f00;
  regs.x.cx = 2;
  __dpmi_int(0x21, &regs);

  /* We can either (1) succeed, (2) read less than 2 bytes,
     or (3) fail to read at all.  */
  if (regs.x.ax != 2)
    read_fail = (regs.x.flags & 1) ? regs.x.ax : -1;

  /* If called with filename, close the file. */
  if (s)
  {
    regs.x.ax = 0x3e00;
    __dpmi_int(0x21, &regs);
    if (regs.x.flags & 1)
      errno = __doserr_to_errno(regs.x.ax);
  }
  /* Else leave file pointer where we found it. */
  else
  {
    regs.x.ax = 0x4200;		/* set pointer from the beginning of file */
    regs.x.bx = fh;
    regs.x.cx = fpos_high;
    regs.x.dx = fpos_low;
    __dpmi_int(0x21, &regs);
    if (regs.x.flags & 1)
    {
      errno = __doserr_to_errno(regs.x.ax);
      return 0;
    }
  }

  if (read_fail == 0)
    retval = _farpeekw(_dos_ds, __tb);
  else
  {
    /* The file couldn't be read: assume non-executable.  If the file
       *is* executable, but was passed as a file-handle, and the user
       opened it in write-only mode, they lose...  */
    retval = 0;
    if (read_fail != -1)
      errno = __doserr_to_errno(read_fail);
  }

  return retval;
}

/* A list of extensions which designate executable files.  These
   are NOT tested for the magic number.  */
static char executables[] = "|EXE|COM|BAT|BTM|DLL|VXD|";

/* A list of extensions which belong to files known to NEVER be
   executables.  These exist to minimize read()'ing files while
   detecting executables by magic number.  You are welcome to
   add to this list, but remember: only extensions which could
   NEVER be present in executables should go here.  */
static char non_executables[] = "\
|A|A01|A02|A03|A04|A05|ADL|ARC|ARJ|ASC|ASM|AUX|AWK\
|BAS|BIB|BGI|BMP\
|C|CC|CFG|CGZ|CH3|CHR|CI|CLP|CMF|CPI|CPP|CXX\
|DAT|DBF|DIZ|DOC|DVI\
|E|EL|ELC\
|F77|FN3\
|GIF|GZ\
|H|HLP|HPP|HXX\
|ICO|IN|INC|INF|INI\
|JPG\
|L|LEX|LF|LIB|LOG|LST|LZH\
|M|MAK|MAP|MF|MID|MPG\
|O|OBJ\
|PAK|PAS|PBM|PCD|PCX|PDS|PIC|PIF|PN3|PRJ|PS\
|RAS|RGB|RLE\
|S|SND|SY3\
|TAR|TAZ|TEX|TGA|TGZ|TIF|TXH|TXI|TXT\
|VOC\
|WAV|WK1|WK3|WKB|WQ1|WQ3|WQ4|WQ5|WQ6|WQ!\
|XBM\
|Y\
|ZIP|ZOO|";

int
_is_executable(const char *filename, int fhandle, const char *extension)
{
  if (!extension && filename)
  {
    const char *cp, *ep=0;
    for (cp=filename; *cp; cp++)
    {
      if (*cp == '.')
	ep = cp;
      if (*cp == '/' || *cp == '\\' || *cp == ':')
	ep = 0;
    }
    extension = ep;
  }
  if ((_djstat_flags & _STAT_EXEC_EXT) == 0
      && extension
      && *extension
      && strlen(extension) <= ((extension[0]=='.') ? 4 : 3))
    {
      /* Search the list of extensions in executables[]. */
      char tmp_buf[6], *tp = tmp_buf;

      *tp++ = '|';
      if (*extension == '.')
	extension++;
      while (*extension)
	*tp++ = toupper (*extension++);
      *tp++ = '|';
      *tp = '\0';
      if (strstr(non_executables, tmp_buf))
        return 0;
      else if (strstr(executables, tmp_buf))
        return 1;
    }

  /* No extension, or extension doesn't define execute
     bits unambiguously.  We are in for some dirty work.
     Read the first two bytes of the file and see if they
     are any of the known magic numbers which designate
     executable files.
     Unix-like shells, which have executable shell scripts
     without extensions and DON'T have "#!" as their FIRST
     TWO CHARACTERS, lose here.  Sorry, folks.  */
  if ( (_djstat_flags & _STAT_EXEC_MAGIC) == 0 )
    {
      switch (_get_magic(filename, fhandle))
        {
          case 0x5a4d:      /* "MZ" */
          case 0x010b:
          case 0x014c:
          case 0x2123:      /* "#!" */
              return 1;
        }
    }

  return 0;
}

