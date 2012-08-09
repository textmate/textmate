/* Utility and Unix shadow routines for GNU Emacs support programs on NT.
   Copyright (C) 1994, 2002-2012 Free Software Foundation, Inc.

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


#include <pwd.h>
#include <malloc.h>

/* Include these headers now so we don't have to worry about include
   order dependencies in common source files.  */
#include <direct.h>
#include <io.h>
#include <stdio.h>

#ifdef sleep
#undef sleep
#endif
void sleep (unsigned long seconds);
char *getwd (char *dir);
int getppid (void);
char * getlogin (void);
char * cuserid (char * s);
unsigned getuid (void);
unsigned getegid (void);
unsigned getgid (void);
int setuid (unsigned uid);
int setregid (unsigned rgid, unsigned gid);
char * getpass (const char * prompt);
int fchown (int fd, unsigned uid, unsigned gid);

/* redirect or undo interceptions created by config.h */
#undef access
#define access  _access
#undef chdir
#define chdir   _chdir
#undef chmod
#define chmod   _chmod
#undef close
#define close   _close
#undef creat
#define creat   _creat
#undef ctime
#undef dup
#define dup     _dup
#undef dup2
#define dup2    _dup2
#undef fopen
#undef mkdir
#define mkdir   _mkdir
#undef mktemp
#define mktemp  _mktemp
#undef open
#define open    _open
#undef pipe
#define pipe    _pipe
#undef read
#define read    _read
#undef rename
#undef rmdir
#define rmdir   _rmdir
#undef unlink
#define unlink  _unlink
#undef write
#define write   _write

/* map to MSVC names */
#define execlp    _execlp
#define execvp    _execvp
#define fdopen	  _fdopen
#ifndef fileno
#define fileno	  _fileno
#endif
#define getcwd	  _getcwd
#define getw	  _getw
#define getpid    _getpid
#define isatty    _isatty
#define locking   _locking
#define logb      _logb
#define _longjmp  longjmp
#define lseek     _lseek
#define popen     _popen
#define pclose    _pclose
#define umask	  _umask
#define utime	  _utime
#define index     strchr
#define rindex    strrchr

/* Make standard winsock definitions available if needed.  */
#undef _WINSOCKAPI_
#undef _WINSOCK_H

/* end of ntlib.h */

