Copyright (C) 2001-2012  Free Software Foundation, Inc.
See the end of the file for license conditions.


This directory tree holds version 24.1 of GNU Emacs, the extensible,
customizable, self-documenting real-time display editor.

The file INSTALL in this directory says how to build and install GNU
Emacs on various systems, once you have unpacked or checked out the
entire Emacs file tree.

See the file etc/NEWS for information on new features and other
user-visible changes in recent versions of Emacs.

The file etc/PROBLEMS contains information on many common problems that
occur in building, installing and running Emacs.

You may encounter bugs in this release.  If you do, please report
them; your bug reports are valuable contributions to the FSF, since
they allow us to notice and fix problems on machines we don't have, or
in code we don't use often.  Please send bug reports to the mailing
list bug-gnu-emacs@gnu.org.  If possible, use M-x report-emacs-bug.

See the "Bugs" section of the Emacs manual for more information on how
to report bugs.  (The file `BUGS' in this directory explains how you
can find and read that section using the Info files that come with
Emacs.)  See `etc/MAILINGLISTS' for more information on mailing lists
relating to GNU packages.

The `etc' subdirectory contains several other files, named in capital
letters, which you might consider looking at when installing GNU
Emacs.

The file `configure' is a shell script to acclimate Emacs to the
oddities of your processor and operating system.  It creates the file
`Makefile' (a script for the `make' program), which automates the
process of building and installing Emacs.  See INSTALL for more
detailed information.

The file `configure.in' is the input used by the autoconf program to
construct the `configure' script.  Since Emacs has some configuration
requirements that autoconf can't meet directly, and for historical
reasons, `configure.in' uses an unholy marriage of custom-baked
configuration code and autoconf macros.  If you want to rebuild
`configure' from `configure.in', you will need to install a recent
version of autoconf and GNU m4.

The file `Makefile.in' is a template used by `configure' to create
`Makefile'.

The file `make-dist' is a shell script to build a distribution tar
file from the current Emacs tree, containing only those files
appropriate for distribution.  If you make extensive changes to Emacs,
this script will help you distribute your version to others.

There are several subdirectories:

`src'       holds the C code for Emacs (the Emacs Lisp interpreter and
            its primitives, the redisplay code, and some basic editing
            functions).
`lisp'      holds the Emacs Lisp code for Emacs (most everything else).
`leim'      holds the library of Emacs input methods, Lisp code and
            auxiliary data files required to type international characters
            which can't be directly produced by your keyboard.
`lib'       holds source code for libraries used by Emacs and its utilities
`lib-src'   holds the source code for some utility programs for use by or
            with Emacs, like movemail and etags.
`etc'       holds miscellaneous architecture-independent data files Emacs
            uses, like the tutorial text and tool bar images.
            The contents of the `lisp', `leim', `info', and `doc'
            subdirectories are architecture-independent too.
`info'      holds the Info documentation tree for Emacs.
`doc/emacs' holds the source code for the Emacs Manual.  If you modify the
            manual sources, you will need the `makeinfo' program to produce
            an updated manual.  `makeinfo' is part of the GNU Texinfo
            package; you need a suitably recent version of Texinfo.
`doc/lispref'   holds the source code for the Emacs Lisp reference manual.
`doc/lispintro' holds the source code for the Introduction to Programming
                in Emacs Lisp manual.
`msdos'     holds configuration files for compiling Emacs under MSDOG.
`nextstep'  holds instructions and some other files for compiling the
            Nextstep port of Emacs, for GNUstep and Mac OS X Cocoa.
`nt'        holds various command files and documentation files that pertain
            to building and running Emacs on Windows 9X/ME/NT/2000/XP.
`test'      holds tests for various aspects of Emacs's functionality.

   Building Emacs on non-Posix platforms requires tools that aren't part
of the standard distribution of the OS.  The platform-specific README
files and installation instructions should list the required tools.


NOTE ON COPYRIGHT YEARS

In copyright notices where the copyright holder is the Free Software
Foundation, then where a range of years appears, this is an inclusive
range that applies to every year in the range.  For example: 2005-2008
represents the years 2005, 2006, 2007, and 2008.


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
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
