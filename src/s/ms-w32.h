/* System description file for Windows NT.

Copyright (C) 1993-1995, 2001-2012  Free Software Foundation, Inc.

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

/* Define symbols to identify the version of Unix this is.
   Define all the symbols that apply correctly.  */

#ifndef WINDOWSNT
#define WINDOWSNT
#endif
#ifndef DOS_NT
#define DOS_NT 	/* MSDOS or WINDOWSNT */
#endif

/* If you are compiling with a non-C calling convention but need to
   declare vararg routines differently, put it here.  */
#define _VARARGS_ __cdecl

/* If you are providing a function to something that will call the
   function back (like a signal handler and signal, or main) its calling
   convention must be whatever standard the libraries expect.  */
#define _CALLBACK_ __cdecl

/* SYSTEM_TYPE should indicate the kind of system you are using.
   It sets the Lisp variable system-type.  */
#define SYSTEM_TYPE "windows-nt"

#define NO_MATHERR 1

/* Letter to use in finding device name of first pty,
   if system supports pty's.  'a' means it is /dev/ptya0  */
#define FIRST_PTY_LETTER 'a'

/* Define HAVE_TIMEVAL if the system supports the BSD style clock values.
   Look in <sys/time.h> for a timeval structure.  */
#define HAVE_TIMEVAL 1

/* NT supports Winsock which is close enough (with some hacks).  */
#define HAVE_SOCKETS 1

/* But our select implementation doesn't allow us to make non-blocking
   connects.  So until that is fixed, this is necessary:  */
#define BROKEN_NON_BLOCKING_CONNECT 1

/* And the select implementation does 1-byte read-ahead waiting
   for received packets, so datagrams are broken too.  */
#define BROKEN_DATAGRAM_SOCKETS 1

#define MAIL_USE_POP 1
#define MAIL_USE_SYSTEM_LOCK 1

/* If the character used to separate elements of the executable path
   is not ':', #define this to be the appropriate character constant.  */
#define SEPCHAR ';'

#define ORDINARY_LINK 1

/* ============================================================ */

/* Here, add any special hacks needed to make Emacs work on this
   system.  For example, you might define certain system call names
   that don't exist on your system, or that do different things on
   your system and must be used only through an encapsulation (which
   you should place, by convention, in sysdep.c).  */

/* Define this to be the separator between devices and paths.  */
#define DEVICE_SEP ':'

/* We'll support either convention on NT.  */
#define IS_DIRECTORY_SEP(_c_) ((_c_) == '/' || (_c_) == '\\')
#define IS_ANY_SEP(_c_) (IS_DIRECTORY_SEP (_c_) || IS_DEVICE_SEP (_c_))

#include <sys/types.h>

#ifdef _MSC_VER
typedef unsigned long sigset_t;
typedef int ssize_t;
#endif

struct sigaction {
  int sa_flags;
  void (*sa_handler)(int);
  sigset_t sa_mask;
};
#define SIG_BLOCK       1
#define SIG_SETMASK     2
#define SIG_UNBLOCK     3

/* The null device on Windows NT.  */
#define NULL_DEVICE     "NUL:"

#ifndef MAXPATHLEN
#define MAXPATHLEN      _MAX_PATH
#endif

#define HAVE_SOUND  1
#define LISP_FLOAT_TYPE 1

#define HAVE_SYS_TIMEB_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_UNISTD_H 1
#undef  HAVE_UTIME_H
#undef  HAVE_LINUX_VERSION_H
#undef  HAVE_SYS_SYSTEMINFO_H
#define HAVE_PWD_H 1
#define TIME_WITH_SYS_TIME 1

#define HAVE_GETTIMEOFDAY 1
#define HAVE_GETHOSTNAME 1
#undef  HAVE_GETDOMAINNAME
#define HAVE_DUP2 1
#define HAVE_RENAME 1
#define HAVE_CLOSEDIR 1
#define HAVE_FSYNC 1		/* fsync is called _commit in MSVC.  */

#undef  TM_IN_SYS_TIME
#undef  HAVE_TM_ZONE

#define HAVE_LONG_FILE_NAMES 1

#define HAVE_MKDIR 1
#define HAVE_RMDIR 1
#define HAVE_RANDOM 1
#undef  HAVE_SYSINFO
#undef  HAVE_LRAND48
#define HAVE_MEMCMP 1
#define HAVE_MEMCPY 1
#define HAVE_MEMMOVE 1
#define HAVE_MEMSET 1
#define HAVE_LOGB 1
#define HAVE_FREXP 1
#define HAVE_FMOD 1
#undef  HAVE_RINT
#undef  HAVE_CBRT
#define HAVE_FTIME 1
#undef  HAVE_RES_INIT /* For -lresolv on Suns.  */
#undef  HAVE_SETSID
#undef  HAVE_FPATHCONF
#define HAVE_SELECT 1
#undef  HAVE_EUIDACCESS
#define HAVE_GETPAGESIZE 1
#define HAVE_TZSET 1
#define HAVE_SETLOCALE 1
#undef  HAVE_UTIMES
#undef  HAVE_SETRLIMIT
#undef  HAVE_SETPGID
/* If you think about defining HAVE_GETCWD, don't: the alternative
   getwd is redefined on w32.c, and does not really return the current
   directory, to get the desired results elsewhere in Emacs */
#undef  HAVE_GETCWD
#define HAVE_SHUTDOWN 1

#define LOCALTIME_CACHE
#define HAVE_INET_SOCKETS 1

#undef  HAVE_AIX_SMT_EXP
#define USE_TOOLKIT_SCROLL_BARS 1

/* Define if you have the ANSI `strerror' function.
   Otherwise you must have the variable `char *sys_errlist[]'.  */
#define HAVE_STRERROR 1

/* Define if `struct utimbuf' is declared by <utime.h>.  */
#undef  HAVE_STRUCT_UTIMBUF

#define HAVE_MOUSE 1
#define HAVE_H_ERRNO 1

#ifdef HAVE_NTGUI
#define HAVE_WINDOW_SYSTEM 1
#define HAVE_MENUS 1
#endif

/* Get some redefinitions in place.  */

#ifdef emacs

#ifdef _MSC_VER
#include <sys/timeb.h>
#include <sys/stat.h>
#include <signal.h>

/* MSVC gets link-time errors without these redirections.  */
#define fstat(a, b) sys_fstat(a, b)
#define stat(a, b)  sys_stat(a, b)
#define utime       sys_utime
#endif

/* Calls that are emulated or shadowed.  */
#undef access
#define access  sys_access
#undef chdir
#define chdir   sys_chdir
#undef chmod
#define chmod   sys_chmod
#define chown   sys_chown
#undef close
#define close   sys_close
#undef creat
#define creat   sys_creat
#define ctime	sys_ctime
#undef dup
#define dup     sys_dup
#undef dup2
#define dup2    sys_dup2
#define fopen   sys_fopen
#define link    sys_link
#define localtime sys_localtime
#define mkdir   sys_mkdir
#undef mktemp
#define mktemp  sys_mktemp
#undef open
#define open    sys_open
#define pipe    sys_pipe
#undef read
#define read    sys_read
#define rename  sys_rename
#define rmdir   sys_rmdir
#define select  sys_select
#define sleep   sys_sleep
#define strerror sys_strerror
#undef unlink
#define unlink  sys_unlink
#undef write
#define write   sys_write

/* Subprocess calls that are emulated.  */
#define spawnve sys_spawnve
#define wait    sys_wait
#define kill    sys_kill
#define signal  sys_signal

/* termcap.c calls that are emulated.  */
#define tputs   sys_tputs
#define tgetstr sys_tgetstr

/* cm.c calls that are emulated.  */
#define chcheckmagic sys_chcheckmagic
#define cmcostinit   sys_cmcostinit
#define cmgoto       sys_cmgoto
#define cmputc       sys_cmputc
#define Wcm_clear    sys_Wcm_clear

#endif /* emacs */

/* Map to MSVC names.  */
#define execlp    _execlp
#define execvp    _execvp
#define fdopen	  _fdopen
#ifndef fileno
#define fileno	  _fileno
#endif
#define fsync	  _commit
#define ftruncate _chsize
#define getpid    _getpid
#ifdef _MSC_VER
typedef int pid_t;
#define snprintf  _snprintf
#define strtoll   _strtoi64
#endif
#define isatty    _isatty
#define logb      _logb
#define _longjmp  longjmp
#define lseek     _lseek
#define popen     _popen
#define pclose    _pclose
#define umask	  _umask
#ifndef _MSC_VER
#define utimbuf	  _utimbuf
#endif
#define strdup    _strdup
#define strupr    _strupr
#define strnicmp  _strnicmp
#define stricmp   _stricmp
#define tzset     _tzset

/* Include time.h before redirecting tzname, since MSVC's time.h
   defines _tzname to call a function, but also declares tzname a
   2-element array.  Having the redirection before including the
   header thus has the effect of declaring a function that returns an
   array, and triggers an error message.  */
#include <time.h>
#define tzname    _tzname
#if !defined (_MSC_VER) || (_MSC_VER < 1400)
#undef  utime
#define utime	  _utime
#endif

/* This is hacky, but is necessary to avoid warnings about macro
   redefinitions using the SDK compilers.  */
#ifndef __STDC__
#define __STDC__ 1
#define MUST_UNDEF__STDC__
#endif
#include <direct.h>
#include <io.h>
#include <stdio.h>
#ifdef MUST_UNDEF__STDC__
#undef __STDC__
#undef MUST_UNDEF__STDC__
#endif

/* Defines that we need that aren't in the standard signal.h.  */
#define SIGHUP  1               /* Hang up */
#define SIGQUIT 3               /* Quit process */
#define SIGTRAP 5               /* Trace trap */
#define SIGKILL 9               /* Die, die die */
#define SIGPIPE 13              /* Write on pipe with no readers */
#define SIGALRM 14              /* Alarm */
#define SIGCHLD 18              /* Death of child */

#ifndef NSIG
#define NSIG 23
#endif

/* For integration with MSDOS support.  */
#define getdisk()               (_getdrive () - 1)
#ifdef emacs
#define getdefdir(_drv, _buf)   ((_buf[0] = (_drv + 'A' - 1), _buf[1] = ':', _buf[2] = '/', _buf[3] = 0), 1)
#else
#define getdefdir(_drv, _buf)   _getdcwd (_drv, _buf, MAXPATHLEN)
#endif

extern char *get_emacs_configuration (void);
extern char *get_emacs_configuration_options (void);
#define EMACS_CONFIGURATION 	get_emacs_configuration ()
#define EMACS_CONFIG_OPTIONS	get_emacs_configuration_options ()

/* Define this so that winsock.h definitions don't get included with
   windows.h.  For this to have proper effect, config.h must always be
   included before windows.h.  */
#define _WINSOCKAPI_    1
#define _WINSOCK_H

/* Defines size_t and alloca ().  */
#ifdef emacs
#define malloc e_malloc
#define free   e_free
#define realloc e_realloc
#define calloc e_calloc
#endif
#ifdef _MSC_VER
#define alloca _alloca
#else
#include <malloc.h>
#endif

#include <sys/stat.h>

/* Define for those source files that do not include enough NT system files.  */
#ifndef NULL
#ifdef __cplusplus
#define NULL	0
#else
#define NULL	((void *)0)
#endif
#endif

/* For proper declaration of environ.  */
#include <stdlib.h>
#ifndef sys_nerr
#define sys_nerr _sys_nerr
#endif
#include <string.h>

extern int getloadavg (double *, int);

/* We need a little extra space, see ../../lisp/loadup.el.  */
#define SYSTEM_PURESIZE_EXTRA 50000

/* For unexec to work on Alpha systems, we need to put Emacs'
   initialized data into a separate section from the CRT initialized
   data (because the Alpha linker freely reorders data variables, even
   across libraries, so our data and the CRT data get intermingled).

   Starting with MSVC 5.0, we must also place the uninitialized data
   into its own section.  VC5 intermingles uninitialized data from the CRT
   between Emacs' static uninitialized data and its public uninitialized
   data.  A separate .bss section for Emacs groups both static and
   public uninitialized together.

   Note that unexw32.c relies on this fact, and must be modified
   accordingly if this section name is changed, or if this pragma is
   removed.  Also, obviously, all files that define initialized data
   must include config.h to pick up this pragma.  */

/* Names must be < 8 bytes.  */
#ifdef _MSC_VER
#pragma data_seg("EMDATA")
#pragma bss_seg("EMBSS")
#endif

/* #define FULL_DEBUG */
/* #define EMACSDEBUG */

#ifdef EMACSDEBUG
extern void _DebPrint (const char *fmt, ...);
#define DebPrint(stuff) _DebPrint stuff
#else
#define DebPrint(stuff)
#endif


/* ============================================================ */
