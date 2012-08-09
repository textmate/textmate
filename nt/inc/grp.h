/* Replacement grp.h file for building GNU Emacs on Windows.

Copyright (C) 2003-2012  Free Software Foundation, Inc.

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

#ifndef _GRP_H
#define _GRP_H

#include <pwd.h> /* gid_t defined here */

/* Emacs uses only gr_name */
struct group {
  char *gr_name;	/* group name */
  gid_t gr_gid;		/* group numerical ID */
};

struct group *getgrgid(gid_t);

#endif /* _GRP_H */

