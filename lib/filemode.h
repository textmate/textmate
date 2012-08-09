/* Make a string describing file modes.

   Copyright (C) 1998-1999, 2003, 2006, 2009-2011 Free Software Foundation,
   Inc.

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

#ifndef FILEMODE_H_

# include <sys/types.h>
# include <sys/stat.h>

/* Get the declaration of strmode.  */
# if HAVE_DECL_STRMODE
#  include <string.h> /* MacOS X, FreeBSD, OpenBSD */
#  include <unistd.h> /* NetBSD */
# endif

# ifdef __cplusplus
extern "C" {
# endif

# if !HAVE_DECL_STRMODE
extern void strmode (mode_t mode, char *str);
# endif

extern void filemodestring (struct stat const *statp, char *str);

# ifdef __cplusplus
}
# endif

#endif
