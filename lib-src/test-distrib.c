/* test-distrib.c --- testing distribution of nonprinting chars

Copyright (C) 1987, 1993-1995, 1999, 2001-2012  Free Software Foundation, Inc.

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
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

/* Break string in two parts to avoid buggy C compilers that ignore characters
   after nulls in strings.  */

static char string1[] = "Testing distribution of nonprinting chars:\n\
Should be 0177: \177 Should be 0377: \377 Should be 0212: \212.\n\
Should be 0000: ";

static char string2[] = ".\n\
This file is read by the `test-distribution' program.\n\
If you change it, you will make that program fail.\n";

/* Like `read' but keeps trying until it gets SIZE bytes or reaches eof.  */
static int
cool_read (int fd, char *buf, size_t size)
{
  ssize_t num;
  ssize_t sofar = 0;

  while (1)
    {
      if ((num = read (fd, buf + sofar, size - sofar)) == 0)
	return sofar;
      else if (num < 0)
	return num;
      sofar += num;
    }
}

int
main (int argc, char **argv)
{
  int fd;
  char buf[300];

  if (argc != 2)
    {
      fprintf (stderr, "Usage: %s testfile\n", argv[0]);
      exit (EXIT_FAILURE);
    }
  fd = open (argv[1], O_RDONLY);
  if (fd < 0)
    {
      perror (argv[1]);
      exit (EXIT_FAILURE);
    }
  if (cool_read (fd, buf, sizeof string1) != sizeof string1 ||
      strcmp (buf, string1) ||
      cool_read (fd, buf, sizeof string2) != sizeof string2 - 1 ||
      strncmp (buf, string2, sizeof string2 - 1))
    {
      fprintf (stderr, "Data in file `%s' has been damaged.\n\
Most likely this means that many nonprinting characters\n\
have been corrupted in the files of Emacs, and it will not work.\n",
	       argv[1]);
      exit (EXIT_FAILURE);
    }
  close (fd);
  return EXIT_SUCCESS;
}


/* test-distrib.c ends here */
