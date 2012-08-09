/* pop.h: Header file for the "pop.c" client POP3 protocol.
   Copyright (C) 1991, 1993, 2001-2012  Free Software Foundation, Inc.

Author:  Jonathan Kamens <jik@security.ov.com>

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


#include <stdio.h>

#define GETLINE_MIN 1024	/* the getline buffer starts out this */
				/* size */
#define GETLINE_INCR 1024	/* the getline buffer is grown by this */
				/* size when it needs to grow */

extern char pop_error[];
extern int pop_debug;

struct _popserver
{
  int file, data;
  char *buffer;
  int buffer_size, buffer_index;
  int in_multi;
  int trash_started;
};

typedef struct _popserver *popserver;

/*
 * Valid flags for the pop_open function.
 */

#define POP_NO_KERBEROS	(1<<0)
#define POP_NO_HESIOD	(1<<1)
#define POP_NO_GETPASS 	(1<<2)

extern popserver pop_open (char *host, char *username, char *password,
                           int flags);
extern int pop_stat (popserver server, int *count, int *size);
extern int pop_list (popserver server, int message, int **IDs,
                     int **size);
extern int pop_retrieve (popserver server, int message, int markfrom,
                         char **);
extern int pop_retrieve_first (popserver server, int message,
                               char **response);
extern int pop_retrieve_next (popserver server, char **line);
extern int pop_retrieve_flush (popserver server);
extern int pop_top_first (popserver server, int message, int lines,
                          char **response);
extern int pop_top_next (popserver server, char **line);
extern int pop_top_flush (popserver server);
extern int pop_multi_first (popserver server, const char *command,
                            char **response);
extern int pop_multi_next (popserver server, char **line);
extern int pop_multi_flush (popserver server);
extern int pop_delete (popserver server, int message);
extern int pop_noop (popserver server);
extern int pop_last (popserver server);
extern int pop_reset (popserver server);
extern int pop_quit (popserver server);
extern void pop_close (popserver);

