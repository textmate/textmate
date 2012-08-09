/* Mark beginning of data space to dump as pure, for GNU Emacs.
   Copyright (C) 1997, 2001-2012  Free Software Foundation, Inc.

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


#include <config.h>

#ifdef WINDOWSNT
/* See comments in lastfile.c.  */
char my_begdata[] = "Beginning of Emacs initialized data";
char my_begbss[1];  /* Do not initialize this variable.  */
static char _my_begbss[1];
char * my_begbss_static = _my_begbss;

/* Add a dummy reference to ensure emacs.obj is linked in.  */
extern int initialized;
static int * dummy = &initialized;
#endif

