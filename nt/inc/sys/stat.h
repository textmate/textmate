/* sys/stat.h supplied with MSVCRT uses too narrow data types for
   inode and user/group id, so we replace them with our own.

Copyright (C) 2008-2012  Free Software Foundation, Inc.

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

#ifndef INC_SYS_STAT_H_
#define INC_SYS_STAT_H_

#ifdef __MINGW32__
# include <_mingw.h>
#endif

/* Only MinGW 3.13 and later has __MINGW_NOTHROW.  */
#ifndef __MINGW_NOTHROW
# define __MINGW_NOTHROW
#endif

#include <sys/types.h>
#include <time.h>

#define	S_IFMT	0xF000

#define	S_IFREG	0x8000
#define	S_IFDIR	0x4000
#define	S_IFBLK	0x3000
#define	S_IFCHR	0x2000
#define	S_IFIFO	0x1000

#define	S_IREAD	 0x0100
#define	S_IWRITE 0x0080
#define	S_IEXEC	 0x0040

#define	S_IRUSR	S_IREAD
#define	S_IWUSR	S_IWRITE
#define	S_IXUSR	S_IEXEC
#define	S_IRWXU	(S_IREAD | S_IWRITE | S_IEXEC)

#define	S_ISREG(m)	(((m) & S_IFMT) == S_IFREG)
#define	S_ISDIR(m)	(((m) & S_IFMT) == S_IFDIR)
#define	S_ISBLK(m)	(((m) & S_IFMT) == S_IFBLK)
#define	S_ISCHR(m)	(((m) & S_IFMT) == S_IFCHR)
#define	S_ISFIFO(m)	(((m) & S_IFMT) == S_IFIFO)

/* These don't exist on Windows, but lib/filemode.c wants them.  */
#define S_ISUID 0
#define S_ISGID 0
#define S_ISVTX 0
#define S_IRGRP (S_IRUSR >> 3)
#define S_IROTH (S_IRUSR >> 6)
#define S_IWGRP (S_IWUSR >> 3)
#define S_IWOTH (S_IWUSR >> 6)
#define S_IXGRP (S_IXUSR >> 3)
#define S_IXOTH (S_IXUSR >> 6)

#define S_ISSOCK(m)    0
#define S_ISLNK(m)     0
#define S_ISCTG(p)     0
#define S_ISDOOR(m)    0
#define S_ISMPB(m)     0
#define S_ISMPC(m)     0
#define S_ISNWK(m)     0
#define S_ISPORT(m)    0
#define S_ISWHT(m)     0
#define S_TYPEISMQ(p)  0
#define S_TYPEISSEM(p) 0
#define S_TYPEISSHM(p) 0
#define S_TYPEISTMO(p) 0

struct stat {
  unsigned __int64 st_ino;	/* ino_t in sys/types.h is too narrow */
  dev_t st_dev;
  unsigned short   st_mode;
  short		   st_nlink;
  unsigned	   st_uid; /* Vista's TrustedInstaller has a very large RID */
  unsigned	   st_gid;
  unsigned __int64 st_size;
  dev_t		   st_rdev;
  time_t	   st_atime;
  time_t	   st_mtime;
  time_t	   st_ctime;
  char		   st_uname[260];
  char		   st_gname[260];
};

/* Prevent redefinition by other headers, e.g. wchar.h.  */
#define _STAT_DEFINED

_CRTIMP int __cdecl __MINGW_NOTHROW	fstat (int, struct stat*);
_CRTIMP int __cdecl __MINGW_NOTHROW	chmod (const char*, int);
_CRTIMP int __cdecl __MINGW_NOTHROW	stat (const char*, struct stat*);

/* fileio.c and dired.c want lstat.  */
#define lstat stat

#endif	/* INC_SYS_STAT_H_ */

