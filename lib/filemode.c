/* filemode.c -- make a string describing file modes

   Copyright (C) 1985, 1990, 1993, 1998-2000, 2004, 2006, 2009-2011 Free
   Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#include "filemode.h"

/* The following is for Cray DMF (Data Migration Facility), which is a
   HSM file system.  A migrated file has a `st_dm_mode' that is
   different from the normal `st_mode', so any tests for migrated
   files should use the former.  */
#if HAVE_ST_DM_MODE
# define IS_MIGRATED_FILE(statp) \
    (S_ISOFD (statp->st_dm_mode) || S_ISOFL (statp->st_dm_mode))
#else
# define IS_MIGRATED_FILE(statp) 0
#endif

#if ! HAVE_DECL_STRMODE

/* Return a character indicating the type of file described by
   file mode BITS:
   '-' regular file
   'b' block special file
   'c' character special file
   'C' high performance ("contiguous data") file
   'd' directory
   'D' door
   'l' symbolic link
   'm' multiplexed file (7th edition Unix; obsolete)
   'n' network special file (HP-UX)
   'p' fifo (named pipe)
   'P' port
   's' socket
   'w' whiteout (4.4BSD)
   '?' some other file type  */

static char
ftypelet (mode_t bits)
{
  /* These are the most common, so test for them first.  */
  if (S_ISREG (bits))
    return '-';
  if (S_ISDIR (bits))
    return 'd';

  /* Other letters standardized by POSIX 1003.1-2004.  */
  if (S_ISBLK (bits))
    return 'b';
  if (S_ISCHR (bits))
    return 'c';
  if (S_ISLNK (bits))
    return 'l';
  if (S_ISFIFO (bits))
    return 'p';

  /* Other file types (though not letters) standardized by POSIX.  */
  if (S_ISSOCK (bits))
    return 's';

  /* Nonstandard file types.  */
  if (S_ISCTG (bits))
    return 'C';
  if (S_ISDOOR (bits))
    return 'D';
  if (S_ISMPB (bits) || S_ISMPC (bits))
    return 'm';
  if (S_ISNWK (bits))
    return 'n';
  if (S_ISPORT (bits))
    return 'P';
  if (S_ISWHT (bits))
    return 'w';

  return '?';
}

/* Like filemodestring, but rely only on MODE.  */

void
strmode (mode_t mode, char *str)
{
  str[0] = ftypelet (mode);
  str[1] = mode & S_IRUSR ? 'r' : '-';
  str[2] = mode & S_IWUSR ? 'w' : '-';
  str[3] = (mode & S_ISUID
            ? (mode & S_IXUSR ? 's' : 'S')
            : (mode & S_IXUSR ? 'x' : '-'));
  str[4] = mode & S_IRGRP ? 'r' : '-';
  str[5] = mode & S_IWGRP ? 'w' : '-';
  str[6] = (mode & S_ISGID
            ? (mode & S_IXGRP ? 's' : 'S')
            : (mode & S_IXGRP ? 'x' : '-'));
  str[7] = mode & S_IROTH ? 'r' : '-';
  str[8] = mode & S_IWOTH ? 'w' : '-';
  str[9] = (mode & S_ISVTX
            ? (mode & S_IXOTH ? 't' : 'T')
            : (mode & S_IXOTH ? 'x' : '-'));
  str[10] = ' ';
  str[11] = '\0';
}

#endif /* ! HAVE_DECL_STRMODE */

/* filemodestring - fill in string STR with an ls-style ASCII
   representation of the st_mode field of file stats block STATP.
   12 characters are stored in STR.
   The characters stored in STR are:

   0    File type, as in ftypelet above, except that other letters are used
        for files whose type cannot be determined solely from st_mode:

            'F' semaphore
            'M' migrated file (Cray DMF)
            'Q' message queue
            'S' shared memory object
            'T' typed memory object

   1    'r' if the owner may read, '-' otherwise.

   2    'w' if the owner may write, '-' otherwise.

   3    'x' if the owner may execute, 's' if the file is
        set-user-id, '-' otherwise.
        'S' if the file is set-user-id, but the execute
        bit isn't set.

   4    'r' if group members may read, '-' otherwise.

   5    'w' if group members may write, '-' otherwise.

   6    'x' if group members may execute, 's' if the file is
        set-group-id, '-' otherwise.
        'S' if it is set-group-id but not executable.

   7    'r' if any user may read, '-' otherwise.

   8    'w' if any user may write, '-' otherwise.

   9    'x' if any user may execute, 't' if the file is "sticky"
        (will be retained in swap space after execution), '-'
        otherwise.
        'T' if the file is sticky but not executable.

   10   ' ' for compatibility with 4.4BSD strmode,
        since this interface does not support ACLs.

   11   '\0'.  */

void
filemodestring (struct stat const *statp, char *str)
{
  strmode (statp->st_mode, str);

  if (S_TYPEISSEM (statp))
    str[0] = 'F';
  else if (IS_MIGRATED_FILE (statp))
    str[0] = 'M';
  else if (S_TYPEISMQ (statp))
    str[0] = 'Q';
  else if (S_TYPEISSHM (statp))
    str[0] = 'S';
  else if (S_TYPEISTMO (statp))
    str[0] = 'T';
}
