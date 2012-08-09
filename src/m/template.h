/* machine description file template.

Copyright (C) 1985-1986, 2001-2012  Free Software Foundation, Inc.

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

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.
   Ones defined so far include m68k and many others */

/* After adding support for a new machine, modify the large case
   statement in configure.in to recognize reasonable
   configuration names, and add a description of the system to
   `etc/MACHINES'.

   Check for any tests of $machine in configure.in, and add an entry
   for the new machine if needed.

   If you've just fixed a problem in an existing configuration file,
   you should also check `etc/MACHINES' to make sure its descriptions
   of known problems in that configuration should be updated.  */
