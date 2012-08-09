/* System description file for MS-DOS

Copyright (C) 1993, 1996-1997, 2001-2012 Free Software Foundation, Inc.

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

/* Note: lots of stuff here was taken from s-msdos.h in demacs. */


/* Define symbols to identify the version of Unix this is.
   Define all the symbols that apply correctly.  */
#ifndef MSDOS
#define MSDOS
#endif

#ifndef __DJGPP__
You lose; /* Emacs for DOS must be compiled with DJGPP */
#endif

#define DOS_NT	/* MSDOS or WINDOWSNT */
#undef BSD_SYSTEM

/* SYSTEM_TYPE should indicate the kind of system you are using.
   It sets the Lisp variable system-type.  */
#define SYSTEM_TYPE "ms-dos"

/* subprocesses should be defined if you want to have code for
   asynchronous subprocesses (as used in M-x compile and M-x shell).
   This is the only system that needs this.  */
#undef subprocesses

/* Here, on a separate page, add any special hacks needed to make
   Emacs work on this system.  For example, you might define certain
   system call names that don't exist on your system, or that do
   different things on your system and must be used only through an
   encapsulation (which you should place, by convention, in sysdep.c).  */

/* Avoid incompatibilities between gmalloc.c and system header files
   in how to declare valloc.  */
#define GMALLOC_INHIBIT_VALLOC

/* This overrides the default value on editfns.c, since DJGPP
   does not have pw->pw_gecos.  */
#define USER_FULL_NAME (getenv ("NAME"))

/* setjmp and longjmp can safely replace _setjmp and _longjmp,
   but they will run slower.  */
#define _setjmp setjmp
#define _longjmp longjmp

#define DATA_START  (&etext + 1)

#define _NAIVE_DOS_REGS

/* command.com does not understand `...` so we define this.  */
#define SEPCHAR ';'

#define NULL_DEVICE "nul"

#define HAVE_INVERSE_HYPERBOLIC
#define FLOAT_CHECK_DOMAIN

/* Start of gnulib-related stuff  */

/* lib/ftoastr.c wants strtold, but DJGPP only has _strtold.  DJGPP >
   2.03 has it, but it also has _strtold as a stub that jumps to
   strtold, so use _strtold in all versions.  */
#define strtold _strtold

#if __DJGPP__ > 2 || __DJGPP_MINOR__ > 3
# define HAVE_LSTAT 1
#else
# define lstat stat
#endif

/* End of gnulib-related stuff.  */

/* When $TERM is "internal" then this is substituted:  */
#define INTERNAL_TERMINAL "pc|bios|IBM PC with color display:\
:co#80:li#25:Co#16:pa#256:km:ms:cm=<CM>:cl=<CL>:ce=<CE>:\
:se=</SO>:so=<SO>:us=<UL>:ue=</UL>:md=<BD>:mh=<DIM>:mb=<BL>:mr=<RV>:me=<NV>:\
:AB=<BG %d>:AF=<FG %d>:op=<DefC>:"

/* Define this to a function (Fdowncase, Fupcase) if your file system
   likes that.  */
#define FILE_SYSTEM_CASE Fmsdos_downcase_filename

/* Define this to be the separator between devices and paths.  */
#define DEVICE_SEP ':'

/* We'll support either convention on MSDOG.  */
#define IS_DIRECTORY_SEP(_c_) ((_c_) == '/' || (_c_) == '\\')
#define IS_ANY_SEP(_c_) (IS_DIRECTORY_SEP (_c_) || IS_DEVICE_SEP (_c_))


/* Mode line description of a buffer's type.  */
#define MODE_LINE_BINARY_TEXT(buf) (NILP(B_(buf,buffer_file_type)) ? "T" : "B")

/* We have (the code to control) a mouse.  */
#define HAVE_MOUSE

/* We can use mouse menus.  */
#define HAVE_MENUS

/* Define one of these for easier conditionals.  */
#ifdef HAVE_X_WINDOWS
/* We need a little extra space, see ../../lisp/loadup.el and the
   commentary below, in the non-X branch.  The 140KB number was
   measured on GNU/Linux and on MS-Windows.  */
#define SYSTEM_PURESIZE_EXTRA (-170000+140000)
#else
/* We need a little extra space, see ../../lisp/loadup.el.
   As of 20091024, DOS-specific files use up 62KB of pure space.  But
   overall, we end up wasting 130KB of pure space, because
   BASE_PURESIZE starts at 1.47MB, while we need only 1.3MB (including
   non-DOS specific files and load history; the latter is about 55K,
   but depends on the depth of the top-level Emacs directory in the
   directory tree).  Given the unknown policy of different DPMI
   hosts regarding loading of untouched pages, I'm not going to risk
   enlarging Emacs footprint by another 100+ KBytes.  */
#define SYSTEM_PURESIZE_EXTRA (-170000+65000)
#endif

/* Tell the garbage collector that setjmp is known to save all
   registers relevant for conservative garbage collection in the jmp_buf.  */
#define GC_SETJMP_WORKS 1
#define GC_MARK_STACK GC_MAKE_GCPROS_NOOPS
