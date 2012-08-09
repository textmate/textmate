/* Allocate X colors.  Used for testing with dense colormaps.

Copyright (C) 2001-2012  Free Software Foundation, Inc.

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


#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>

void
fatal (const char *fmt, ...)
{
  va_list ap;

  va_start (ap, fmt);
  vfprintf (stderr, fmt, ap);
  fputc ('\n', stderr);
  va_end (ap);
  exit (1);
}

void
usage (const char *progname)
{
  fprintf (stderr, "Usage %s options\n", progname);
  fprintf (stderr, "-n NCOLORS  allocate NCOLORS colors\n");
  exit (1);
}

int
main (int argc, char **argv)
{
  Display *dpy;
  int opt, ncolors = 0, i;
  XColor *allocated;
  int nallocated;
  XColor color;
  Colormap cmap;

  while ((opt = getopt (argc, argv, "n:")) != EOF)
    switch (opt)
      {
      case 'n':
	ncolors = atoi (optarg);
	break;

      case '?':
	usage (argv[0]);
      }

  if (ncolors == 0)
    usage (argv[0]);

  dpy = XOpenDisplay ("");
  if (dpy == NULL)
    fatal ("Cannot open display");
  cmap = DefaultColormap (dpy, 0);

  allocated = malloc (ncolors * sizeof *allocated);
  nallocated = 0;
  memset (&color, 0, sizeof color);

  while (nallocated < ncolors
	 && color.red < 65536)
    {
      allocated[nallocated] = color;
      if (XAllocColor (dpy, cmap, &allocated[nallocated]))
	{
	  for (i = 0; i < nallocated; ++i)
	    if (allocated[i].red == allocated[nallocated].red
		&& allocated[i].green == allocated[nallocated].green
		&& allocated[i].blue == allocated[nallocated].blue)
	      break;

	  if (i == nallocated)
	    {
	      printf ("allocated %d/%d/%d\n",
		      allocated[nallocated].red,
		      allocated[nallocated].green,
		      allocated[nallocated].blue);
	      ++nallocated;
	    }
	}

      ++color.red;
      ++color.green;
      ++color.blue;
    }

  fprintf (stderr, "Waiting.  Press ^C to stop.\n");
  while (1)
    sleep (10);

  XCloseDisplay (dpy);
  return 0;
}
