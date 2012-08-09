/* Fully extensible Emacs, running on Unix, intended for GNU.

Copyright (C) 1985-1987, 1993-1995, 1997-1999, 2001-2012
  Free Software Foundation, Inc.

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
#include <signal.h>
#include <errno.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/file.h>
#include <setjmp.h>
#include <unistd.h>

#include "lisp.h"

#ifdef WINDOWSNT
#include <fcntl.h>
#include <windows.h> /* just for w32.h */
#include "w32.h"
#include "w32heap.h" /* for prototype of sbrk */
#endif

#ifdef NS_IMPL_GNUSTEP
/* At least under Debian, GSConfig is in a subdirectory.  --Stef  */
#include <GNUstepBase/GSConfig.h>
#endif

#include "commands.h"
#include "intervals.h"
#include "buffer.h"
#include "window.h"

#include "systty.h"
#include "blockinput.h"
#include "syssignal.h"
#include "process.h"
#include "frame.h"
#include "termhooks.h"
#include "keyboard.h"
#include "keymap.h"

#ifdef HAVE_GNUTLS
#include "gnutls.h"
#endif

#ifdef HAVE_NS
#include "nsterm.h"
#endif

#ifdef HAVE_X_WINDOWS
#include "xterm.h"
#endif

#ifdef HAVE_SETLOCALE
#include <locale.h>
#endif

#ifdef HAVE_SETRLIMIT
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifdef HAVE_PERSONALITY_LINUX32
#include <sys/personality.h>
#endif

#ifdef HAVE_LIBXML2
#include <libxml/parser.h>
#endif

#ifndef O_RDWR
#define O_RDWR 2
#endif

#ifdef HAVE_SETPGID
#if !defined (USG)
#undef setpgrp
#define setpgrp setpgid
#endif
#endif

static const char emacs_version[] = VERSION;
static const char emacs_copyright[] = "Copyright (C) 2012 Free Software Foundation, Inc.";

/* Make these values available in GDB, which doesn't see macros.  */

#ifdef USE_LSB_TAG
int gdb_use_lsb EXTERNALLY_VISIBLE = 1;
#else
int gdb_use_lsb EXTERNALLY_VISIBLE = 0;
#endif
#ifndef USE_LISP_UNION_TYPE
int gdb_use_union EXTERNALLY_VISIBLE  = 0;
#else
int gdb_use_union EXTERNALLY_VISIBLE = 1;
#endif
EMACS_INT gdb_valbits EXTERNALLY_VISIBLE = VALBITS;
EMACS_INT gdb_gctypebits EXTERNALLY_VISIBLE = GCTYPEBITS;
#if defined (DATA_SEG_BITS) && ! defined (USE_LSB_TAG)
EMACS_INT gdb_data_seg_bits EXTERNALLY_VISIBLE = DATA_SEG_BITS;
#else
EMACS_INT gdb_data_seg_bits EXTERNALLY_VISIBLE = 0;
#endif
EMACS_INT PVEC_FLAG EXTERNALLY_VISIBLE = PSEUDOVECTOR_FLAG;
EMACS_INT gdb_array_mark_flag EXTERNALLY_VISIBLE = ARRAY_MARK_FLAG;
/* GDB might say "No enum type named pvec_type" if we don't have at
   least one symbol with that type, and then xbacktrace could fail.  */
enum pvec_type gdb_pvec_type EXTERNALLY_VISIBLE = PVEC_TYPE_MASK;

/* Empty lisp strings.  To avoid having to build any others.  */
Lisp_Object empty_unibyte_string, empty_multibyte_string;

/* Set nonzero after Emacs has started up the first time.
  Prevents reinitialization of the Lisp world and keymaps
  on subsequent starts.  */
int initialized;

#ifdef DARWIN_OS
extern void unexec_init_emacs_zone (void);
#endif

#ifdef DOUG_LEA_MALLOC
/* Preserves a pointer to the memory allocated that copies that
   static data inside glibc's malloc.  */
static void *malloc_state_ptr;
/* From glibc, a routine that returns a copy of the malloc internal state.  */
extern void *malloc_get_state (void);
/* From glibc, a routine that overwrites the malloc internal state.  */
extern int malloc_set_state (void*);
/* Non-zero if the MALLOC_CHECK_ environment variable was set while
   dumping.  Used to work around a bug in glibc's malloc.  */
static int malloc_using_checking;
#endif

Lisp_Object Qfile_name_handler_alist;

Lisp_Object Qrisky_local_variable;

Lisp_Object Qkill_emacs;

/* If non-zero, Emacs should not attempt to use a window-specific code,
   but instead should use the virtual terminal under which it was started.  */
int inhibit_window_system;

/* If non-zero, a filter or a sentinel is running.  Tested to save the match
   data on the first attempt to change it inside asynchronous code.  */
int running_asynch_code;

#if defined (HAVE_X_WINDOWS) || defined (HAVE_NS)
/* If non-zero, -d was specified, meaning we're using some window system.  */
int display_arg;
#endif

/* An address near the bottom of the stack.
   Tells GC how to save a copy of the stack.  */
char *stack_bottom;

#if defined (DOUG_LEA_MALLOC) || defined (GNU_LINUX)
/* The address where the heap starts (from the first sbrk (0) call).  */
static void *my_heap_start;
#endif

#ifdef GNU_LINUX
/* The gap between BSS end and heap start as far as we can tell.  */
static uprintmax_t heap_bss_diff;
#endif

/* Nonzero means running Emacs without interactive terminal.  */
int noninteractive;

/* Nonzero means remove site-lisp directories from load-path.  */
int no_site_lisp;

/* Name for the server started by the daemon.*/
static char *daemon_name;

/* Pipe used to send exit notification to the daemon parent at
   startup.  */
int daemon_pipe[2];

/* Save argv and argc.  */
char **initial_argv;
int initial_argc;

static void sort_args (int argc, char **argv);
static void syms_of_emacs (void);

/* MSVC needs each string be shorter than 2048 bytes, so the usage
   strings below are split to not overflow this limit.  */
#define USAGE1 "\
Usage: %s [OPTION-OR-FILENAME]...\n\
\n\
Run Emacs, the extensible, customizable, self-documenting real-time\n\
display editor.  The recommended way to start Emacs for normal editing\n\
is with no options at all.\n\
\n\
Run M-x info RET m emacs RET m emacs invocation RET inside Emacs to\n\
read the main documentation for these command-line arguments.\n\
\n\
Initialization options:\n\
\n\
--batch                     do not do interactive display; implies -q\n\
--chdir DIR                 change to directory DIR\n\
--daemon                    start a server in the background\n\
--debug-init                enable Emacs Lisp debugger for init file\n\
--display, -d DISPLAY       use X server DISPLAY\n\
--no-desktop                do not load a saved desktop\n\
--no-init-file, -q          load neither ~/.emacs nor default.el\n\
--no-shared-memory, -nl     do not use shared memory\n\
--no-site-file              do not load site-start.el\n\
--no-site-lisp, -nsl        do not add site-lisp directories to load-path\n\
--no-splash                 do not display a splash screen on startup\n\
--no-window-system, -nw     do not communicate with X, ignoring $DISPLAY\n\
--quick, -Q                 equivalent to:\n\
                              -q --no-site-file --no-site-lisp --no-splash\n\
--script FILE               run FILE as an Emacs Lisp script\n\
--terminal, -t DEVICE       use DEVICE for terminal I/O\n\
--user, -u USER             load ~USER/.emacs instead of your own\n\
\n%s"

#define USAGE2 "\
Action options:\n\
\n\
FILE                    visit FILE using find-file\n\
+LINE                   go to line LINE in next FILE\n\
+LINE:COLUMN            go to line LINE, column COLUMN, in next FILE\n\
--directory, -L DIR     add DIR to variable load-path\n\
--eval EXPR             evaluate Emacs Lisp expression EXPR\n\
--execute EXPR          evaluate Emacs Lisp expression EXPR\n\
--file FILE             visit FILE using find-file\n\
--find-file FILE        visit FILE using find-file\n\
--funcall, -f FUNC      call Emacs Lisp function FUNC with no arguments\n\
--insert FILE           insert contents of FILE into current buffer\n\
--kill                  exit without asking for confirmation\n\
--load, -l FILE         load Emacs Lisp FILE using the load function\n\
--visit FILE            visit FILE using find-file\n\
\n"

#define USAGE3 "\
Display options:\n\
\n\
--background-color, -bg COLOR   window background color\n\
--basic-display, -D             disable many display features;\n\
                                  used for debugging Emacs\n\
--border-color, -bd COLOR       main border color\n\
--border-width, -bw WIDTH       width of main border\n\
--color, --color=MODE           override color mode for character terminals;\n\
                                  MODE defaults to `auto', and\n\
                                  can also be `never', `always',\n\
                                  or a mode name like `ansi8'\n\
--cursor-color, -cr COLOR       color of the Emacs cursor indicating point\n\
--font, -fn FONT                default font; must be fixed-width\n\
--foreground-color, -fg COLOR   window foreground color\n\
--fullheight, -fh               make the first frame high as the screen\n\
--fullscreen, -fs               make the first frame fullscreen\n\
--fullwidth, -fw                make the first frame wide as the screen\n\
--maximized, -mm                make the first frame maximized\n\
--geometry, -g GEOMETRY         window geometry\n\
--no-bitmap-icon, -nbi          do not use picture of gnu for Emacs icon\n\
--iconic                        start Emacs in iconified state\n\
--internal-border, -ib WIDTH    width between text and main border\n\
--line-spacing, -lsp PIXELS     additional space to put between lines\n\
--mouse-color, -ms COLOR        mouse cursor color in Emacs window\n\
--name NAME                     title for initial Emacs frame\n\
--no-blinking-cursor, -nbc      disable blinking cursor\n\
--reverse-video, -r, -rv        switch foreground and background\n\
--title, -T TITLE               title for initial Emacs frame\n\
--vertical-scroll-bars, -vb     enable vertical scroll bars\n\
--xrm XRESOURCES                set additional X resources\n\
--parent-id XID                 set parent window\n\
--help                          display this help and exit\n\
--version                       output version information and exit\n\
\n"

#define USAGE4 "\
You can generally also specify long option names with a single -; for\n\
example, -batch as well as --batch.  You can use any unambiguous\n\
abbreviation for a --option.\n\
\n\
Various environment variables and window system resources also affect\n\
Emacs' operation.  See the main documentation.\n\
\n\
Report bugs to bug-gnu-emacs@gnu.org.  First, please see the Bugs\n\
section of the Emacs manual or the file BUGS.\n"


/* Signal code for the fatal signal that was received.  */
static int fatal_error_code;

/* Nonzero if handling a fatal error already.  */
int fatal_error_in_progress;

/* If non-null, call this function from fatal_error_signal before
   committing suicide.  */

static void (*fatal_error_signal_hook) (void);

#ifdef FORWARD_SIGNAL_TO_MAIN_THREAD
/* When compiled with GTK and running under Gnome,
   multiple threads may be created.  Keep track of our main
   thread to make sure signals are delivered to it (see syssignal.h).  */

pthread_t main_thread;
#endif

#ifdef HAVE_NS
/* NS autrelease pool, for memory management.  */
static void *ns_pool;
#endif  

 

/* Handle bus errors, invalid instruction, etc.  */
#ifndef FLOAT_CATCH_SIGILL
static
#endif
void
fatal_error_signal (int sig)
{
  SIGNAL_THREAD_CHECK (sig);
  fatal_error_code = sig;
  signal (sig, SIG_DFL);

  TOTALLY_UNBLOCK_INPUT;

  /* If fatal error occurs in code below, avoid infinite recursion.  */
  if (! fatal_error_in_progress)
    {
      fatal_error_in_progress = 1;

      if (sig == SIGTERM || sig == SIGHUP || sig == SIGINT)
        Fkill_emacs (make_number (sig));

      shut_down_emacs (sig, 0, Qnil);
    }

  /* Signal the same code; this time it will really be fatal.
     Remember that since we're in a signal handler, the signal we're
     going to send is probably blocked, so we have to unblock it if we
     want to really receive it.  */
#ifndef MSDOS
  sigunblock (sigmask (fatal_error_code));
#endif

  if (fatal_error_signal_hook)
    fatal_error_signal_hook ();

  kill (getpid (), fatal_error_code);
}

#ifdef SIGDANGER

/* Handler for SIGDANGER.  */
void
memory_warning_signal (int sig)
{
  signal (sig, memory_warning_signal);
  SIGNAL_THREAD_CHECK (sig);

  malloc_warning ("Operating system warns that virtual memory is running low.\n");

  /* It might be unsafe to call do_auto_save now.  */
  force_auto_save_soon ();
}
#endif

/* We define abort, rather than using it from the library,
   so that GDB can return from a breakpoint here.
   MSDOS has its own definition in msdos.c.  */

#if ! defined (DOS_NT) && ! defined (NO_ABORT)

void
abort (void)
{
  kill (getpid (), SIGABRT);
  /* This shouldn't be executed, but it prevents a warning.  */
  exit (1);
}
#endif


/* Code for dealing with Lisp access to the Unix command line.  */

static void
init_cmdargs (int argc, char **argv, int skip_args)
{
  register int i;
  Lisp_Object name, dir, handler;
  int count = SPECPDL_INDEX ();
  Lisp_Object raw_name;

  initial_argv = argv;
  initial_argc = argc;

  raw_name = build_string (argv[0]);

  /* Add /: to the front of the name
     if it would otherwise be treated as magic.  */
  handler = Ffind_file_name_handler (raw_name, Qt);
  if (! NILP (handler))
    raw_name = concat2 (build_string ("/:"), raw_name);

  Vinvocation_name = Ffile_name_nondirectory (raw_name);
  Vinvocation_directory = Ffile_name_directory (raw_name);

  /* If we got no directory in argv[0], search PATH to find where
     Emacs actually came from.  */
  if (NILP (Vinvocation_directory))
    {
      Lisp_Object found;
      int yes = openp (Vexec_path, Vinvocation_name,
		       Vexec_suffixes, &found, make_number (X_OK));
      if (yes == 1)
	{
	  /* Add /: to the front of the name
	     if it would otherwise be treated as magic.  */
	  handler = Ffind_file_name_handler (found, Qt);
	  if (! NILP (handler))
	    found = concat2 (build_string ("/:"), found);
	  Vinvocation_directory = Ffile_name_directory (found);
	}
    }

  if (!NILP (Vinvocation_directory)
      && NILP (Ffile_name_absolute_p (Vinvocation_directory)))
    /* Emacs was started with relative path, like ./emacs.
       Make it absolute.  */
    Vinvocation_directory = Fexpand_file_name (Vinvocation_directory, Qnil);

  Vinstallation_directory = Qnil;

  if (!NILP (Vinvocation_directory))
    {
      dir = Vinvocation_directory;
      name = Fexpand_file_name (Vinvocation_name, dir);
      while (1)
	{
	  Lisp_Object tem, lib_src_exists;
	  Lisp_Object etc_exists, info_exists;

	  /* See if dir contains subdirs for use by Emacs.
	     Check for the ones that would exist in a build directory,
	     not including lisp and info.  */
	  tem = Fexpand_file_name (build_string ("lib-src"), dir);
	  lib_src_exists = Ffile_exists_p (tem);

#ifdef MSDOS
	  /* MSDOS installations frequently remove lib-src, but we still
	     must set installation-directory, or else info won't find
	     its files (it uses the value of installation-directory).  */
	  tem = Fexpand_file_name (build_string ("info"), dir);
	  info_exists = Ffile_exists_p (tem);
#else
	  info_exists = Qnil;
#endif

	  if (!NILP (lib_src_exists) || !NILP (info_exists))
	    {
	      tem = Fexpand_file_name (build_string ("etc"), dir);
	      etc_exists = Ffile_exists_p (tem);
	      if (!NILP (etc_exists))
		{
		  Vinstallation_directory
		    = Ffile_name_as_directory (dir);
		  break;
		}
	    }

	  /* See if dir's parent contains those subdirs.  */
	  tem = Fexpand_file_name (build_string ("../lib-src"), dir);
	  lib_src_exists = Ffile_exists_p (tem);


#ifdef MSDOS
	  /* See the MSDOS commentary above.  */
	  tem = Fexpand_file_name (build_string ("../info"), dir);
	  info_exists = Ffile_exists_p (tem);
#else
	  info_exists = Qnil;
#endif

	  if (!NILP (lib_src_exists) || !NILP (info_exists))
	    {
	      tem = Fexpand_file_name (build_string ("../etc"), dir);
	      etc_exists = Ffile_exists_p (tem);
	      if (!NILP (etc_exists))
		{
		  tem = Fexpand_file_name (build_string (".."), dir);
		  Vinstallation_directory
		    = Ffile_name_as_directory (tem);
		  break;
		}
	    }

	  /* If the Emacs executable is actually a link,
	     next try the dir that the link points into.  */
	  tem = Ffile_symlink_p (name);
	  if (!NILP (tem))
	    {
	      name = Fexpand_file_name (tem, dir);
	      dir = Ffile_name_directory (name);
	    }
	  else
	    break;
	}
    }

  Vcommand_line_args = Qnil;

  for (i = argc - 1; i >= 0; i--)
    {
      if (i == 0 || i > skip_args)
	/* For the moment, we keep arguments as is in unibyte strings.
	   They are decoded in the function command-line after we know
	   locale-coding-system.  */
	Vcommand_line_args
	  = Fcons (make_unibyte_string (argv[i], strlen (argv[i])),
		   Vcommand_line_args);
    }

  unbind_to (count, Qnil);
}

DEFUN ("invocation-name", Finvocation_name, Sinvocation_name, 0, 0, 0,
       doc: /* Return the program name that was used to run Emacs.
Any directory names are omitted.  */)
  (void)
{
  return Fcopy_sequence (Vinvocation_name);
}

DEFUN ("invocation-directory", Finvocation_directory, Sinvocation_directory,
       0, 0, 0,
       doc: /* Return the directory name in which the Emacs executable was located.  */)
  (void)
{
  return Fcopy_sequence (Vinvocation_directory);
}


#ifdef HAVE_TZSET
/* A valid but unlikely value for the TZ environment value.
   It is OK (though a bit slower) if the user actually chooses this value.  */
static char dump_tz[] = "UtC0";
#endif

#ifndef ORDINARY_LINK
/* We don't include crtbegin.o and crtend.o in the link,
   so these functions and variables might be missed.
   Provide dummy definitions to avoid error.
   (We don't have any real constructors or destructors.)  */
#ifdef __GNUC__

/* Define a dummy function F.  Declare F too, to pacify gcc
   -Wmissing-prototypes.  */
#define DEFINE_DUMMY_FUNCTION(f) \
  void f (void) EXTERNALLY_VISIBLE; void f (void) {}

#ifndef GCC_CTORS_IN_LIBC
DEFINE_DUMMY_FUNCTION (__do_global_ctors)
DEFINE_DUMMY_FUNCTION (__do_global_ctors_aux)
DEFINE_DUMMY_FUNCTION (__do_global_dtors)
/* GNU/Linux has a bug in its library; avoid an error.  */
#ifndef GNU_LINUX
char * __CTOR_LIST__[2] EXTERNALLY_VISIBLE = { (char *) (-1), 0 };
#endif
char * __DTOR_LIST__[2] EXTERNALLY_VISIBLE = { (char *) (-1), 0 };
#endif /* GCC_CTORS_IN_LIBC */
DEFINE_DUMMY_FUNCTION (__main)
#endif /* __GNUC__ */
#endif /* ORDINARY_LINK */

/* Test whether the next argument in ARGV matches SSTR or a prefix of
   LSTR (at least MINLEN characters).  If so, then if VALPTR is non-null
   (the argument is supposed to have a value) store in *VALPTR either
   the next argument or the portion of this one after the equal sign.
   ARGV is read starting at position *SKIPPTR; this index is advanced
   by the number of arguments used.

   Too bad we can't just use getopt for all of this, but we don't have
   enough information to do it right.  */

static int
argmatch (char **argv, int argc, const char *sstr, const char *lstr,
          int minlen, char **valptr, int *skipptr)
{
  char *p = NULL;
  ptrdiff_t arglen;
  char *arg;

  /* Don't access argv[argc]; give up in advance.  */
  if (argc <= *skipptr + 1)
    return 0;

  arg = argv[*skipptr+1];
  if (arg == NULL)
    return 0;
  if (strcmp (arg, sstr) == 0)
    {
      if (valptr != NULL)
	{
	  *valptr = argv[*skipptr+2];
	  *skipptr += 2;
	}
      else
	*skipptr += 1;
      return 1;
    }
  arglen = (valptr != NULL && (p = strchr (arg, '=')) != NULL
	    ? p - arg : strlen (arg));
  if (lstr == 0 || arglen < minlen || strncmp (arg, lstr, arglen) != 0)
    return 0;
  else if (valptr == NULL)
    {
      *skipptr += 1;
      return 1;
    }
  else if (p != NULL)
    {
      *valptr = p+1;
      *skipptr += 1;
      return 1;
    }
  else if (argv[*skipptr+2] != NULL)
    {
      *valptr = argv[*skipptr+2];
      *skipptr += 2;
      return 1;
    }
  else
    {
      return 0;
    }
}

#ifdef DOUG_LEA_MALLOC

/* malloc can be invoked even before main (e.g. by the dynamic
   linker), so the dumped malloc state must be restored as early as
   possible using this special hook.  */

static void
malloc_initialize_hook (void)
{
#ifndef USE_CRT_DLL
  extern char **environ;
#endif

  if (initialized)
    {
      if (!malloc_using_checking)
	/* Work around a bug in glibc's malloc.  MALLOC_CHECK_ must be
	   ignored if the heap to be restored was constructed without
	   malloc checking.  Can't use unsetenv, since that calls malloc.  */
	{
	  char **p;

	  for (p = environ; p && *p; p++)
	    if (strncmp (*p, "MALLOC_CHECK_=", 14) == 0)
	      {
		do
		  *p = p[1];
		while (*++p);
		break;
	      }
	}

      malloc_set_state (malloc_state_ptr);
#ifndef XMALLOC_OVERRUN_CHECK
      free (malloc_state_ptr);
#endif
    }
  else
    {
      if (my_heap_start == 0)
        my_heap_start = sbrk (0);
      malloc_using_checking = getenv ("MALLOC_CHECK_") != NULL;
    }
}

void (*__malloc_initialize_hook) (void) EXTERNALLY_VISIBLE = malloc_initialize_hook;

#endif /* DOUG_LEA_MALLOC */


/* ARGSUSED */
int
main (int argc, char **argv)
{
#if GC_MARK_STACK
  Lisp_Object dummy;
#endif
  char stack_bottom_variable;
  int do_initial_setlocale;
  int skip_args = 0;
#ifdef HAVE_SETRLIMIT
  struct rlimit rlim;
#endif
  int no_loadup = 0;
  char *junk = 0;
  char *dname_arg = 0;
#ifdef NS_IMPL_COCOA
  char dname_arg2[80];
#endif
  char *ch_to_dir;

#if GC_MARK_STACK
  stack_base = &dummy;
#endif

#if defined (USE_GTK) && defined (G_SLICE_ALWAYS_MALLOC)
  /* This is used by the Cygwin build.  */
  setenv ("G_SLICE", "always-malloc", 1);
#endif

#ifdef GNU_LINUX
  if (!initialized)
    {
      extern char my_endbss[];
      extern char *my_endbss_static;

      if (my_heap_start == 0)
        my_heap_start = sbrk (0);

      heap_bss_diff = (char *)my_heap_start - max (my_endbss, my_endbss_static);
    }
#endif

#ifdef RUN_TIME_REMAP
  if (initialized)
    run_time_remap (argv[0]);
#endif

/* If using unexmacosx.c (set by s/darwin.h), we must do this. */
#ifdef DARWIN_OS
  if (!initialized)
    unexec_init_emacs_zone ();
#endif

  sort_args (argc, argv);
  argc = 0;
  while (argv[argc]) argc++;

  if (argmatch (argv, argc, "-version", "--version", 3, NULL, &skip_args))
    {
      const char *version, *copyright;
      if (initialized)
	{
	  Lisp_Object tem, tem2;
	  tem = Fsymbol_value (intern_c_string ("emacs-version"));
	  tem2 = Fsymbol_value (intern_c_string ("emacs-copyright"));
	  if (!STRINGP (tem))
	    {
	      fprintf (stderr, "Invalid value of `emacs-version'\n");
	      exit (1);
	    }
	  if (!STRINGP (tem2))
	    {
	      fprintf (stderr, "Invalid value of `emacs-copyright'\n");
	      exit (1);
	    }
	  else
	    {
	      version = SSDATA (tem);
	      copyright = SSDATA (tem2);
	    }
	}
      else
	{
	  version = emacs_version;
	  copyright = emacs_copyright;
	}
      printf ("GNU Emacs %s\n", version);
      printf ("%s\n", copyright);
      printf ("GNU Emacs comes with ABSOLUTELY NO WARRANTY.\n");
      printf ("You may redistribute copies of Emacs\n");
      printf ("under the terms of the GNU General Public License.\n");
      printf ("For more information about these matters, ");
      printf ("see the file named COPYING.\n");
      exit (0);
    }

  if (argmatch (argv, argc, "-chdir", "--chdir", 4, &ch_to_dir, &skip_args))
    if (chdir (ch_to_dir) == -1)
      {
	fprintf (stderr, "%s: Can't chdir to %s: %s\n",
		 argv[0], ch_to_dir, strerror (errno));
	exit (1);
      }


#ifdef HAVE_PERSONALITY_LINUX32
  if (!initialized
      && (strcmp (argv[argc-1], "dump") == 0
          || strcmp (argv[argc-1], "bootstrap") == 0)
      && ! getenv ("EMACS_HEAP_EXEC"))
    {
      static char heapexec[] = "EMACS_HEAP_EXEC=true";
      /* Set this so we only do this once.  */
      putenv (heapexec);

      /* A flag to turn off address randomization which is introduced
         in linux kernel shipped with fedora core 4 */
#define ADD_NO_RANDOMIZE 0x0040000
      personality (PER_LINUX32 | ADD_NO_RANDOMIZE);
#undef  ADD_NO_RANDOMIZE

      execvp (argv[0], argv);

      /* If the exec fails, try to dump anyway.  */
      perror ("execvp");
    }
#endif /* HAVE_PERSONALITY_LINUX32 */

#if defined (HAVE_SETRLIMIT) && defined (RLIMIT_STACK)
  /* Extend the stack space available.
     Don't do that if dumping, since some systems (e.g. DJGPP)
     might define a smaller stack limit at that time.  */
  if (1
#ifndef CANNOT_DUMP
      && (!noninteractive || initialized)
#endif
      && !getrlimit (RLIMIT_STACK, &rlim))
    {
      long newlim;
      extern size_t re_max_failures;
      /* Approximate the amount regex.c needs per unit of re_max_failures.  */
      int ratio = 20 * sizeof (char *);
      /* Then add 33% to cover the size of the smaller stacks that regex.c
	 successively allocates and discards, on its way to the maximum.  */
      ratio += ratio / 3;
      /* Add in some extra to cover
	 what we're likely to use for other reasons.  */
      newlim = re_max_failures * ratio + 200000;
#ifdef __NetBSD__
      /* NetBSD (at least NetBSD 1.2G and former) has a bug in its
       stack allocation routine for new process that the allocation
       fails if stack limit is not on page boundary.  So, round up the
       new limit to page boundary.  */
      newlim = (newlim + getpagesize () - 1) / getpagesize () * getpagesize ();
#endif
      if (newlim > rlim.rlim_max)
	{
	  newlim = rlim.rlim_max;
	  /* Don't let regex.c overflow the stack we have.  */
	  re_max_failures = (newlim - 200000) / ratio;
	}
      if (rlim.rlim_cur < newlim)
	rlim.rlim_cur = newlim;

      setrlimit (RLIMIT_STACK, &rlim);
    }
#endif /* HAVE_SETRLIMIT and RLIMIT_STACK */

  /* Record (approximately) where the stack begins.  */
  stack_bottom = &stack_bottom_variable;

  clearerr (stdin);

#ifndef SYSTEM_MALLOC
  /* Arrange to get warning messages as memory fills up.  */
  memory_warnings (0, malloc_warning);

  /* Call malloc at least once, to run the initial __malloc_hook.
     Also call realloc and free for consistency.  */
  free (realloc (malloc (4), 4));

# ifndef SYNC_INPUT
  /* Arrange to disable interrupt input inside malloc etc.  */
  uninterrupt_malloc ();
# endif /* not SYNC_INPUT */
#endif	/* not SYSTEM_MALLOC */

#ifdef FORWARD_SIGNAL_TO_MAIN_THREAD
  main_thread = pthread_self ();
#endif /* FORWARD_SIGNAL_TO_MAIN_THREAD */

#if defined (MSDOS) || defined (WINDOWSNT)
  /* We do all file input/output as binary files.  When we need to translate
     newlines, we do that manually.  */
  _fmode = O_BINARY;
#endif /* MSDOS || WINDOWSNT */

#ifdef MSDOS
  if (!isatty (fileno (stdin)))
    setmode (fileno (stdin), O_BINARY);
  if (!isatty (fileno (stdout)))
    {
      fflush (stdout);
      setmode (fileno (stdout), O_BINARY);
    }
#endif /* MSDOS */

  /* Skip initial setlocale if LC_ALL is "C", as it's not needed in that case.
     The build procedure uses this while dumping, to ensure that the
     dumped Emacs does not have its system locale tables initialized,
     as that might cause screwups when the dumped Emacs starts up.  */
  {
    char *lc_all = getenv ("LC_ALL");
    do_initial_setlocale = ! lc_all || strcmp (lc_all, "C");
  }

  /* Set locale now, so that initial error messages are localized properly.
     fixup_locale must wait until later, since it builds strings.  */
  if (do_initial_setlocale)
    setlocale (LC_ALL, "");

  inhibit_window_system = 0;

  /* Handle the -t switch, which specifies filename to use as terminal.  */
  while (1)
    {
      char *term;
      if (argmatch (argv, argc, "-t", "--terminal", 4, &term, &skip_args))
	{
	  int result;
	  emacs_close (0);
	  emacs_close (1);
	  result = emacs_open (term, O_RDWR, 0);
	  if (result < 0 || dup (0) < 0)
	    {
	      char *errstring = strerror (errno);
	      fprintf (stderr, "%s: %s: %s\n", argv[0], term, errstring);
	      exit (1);
	    }
	  if (! isatty (0))
	    {
	      fprintf (stderr, "%s: %s: not a tty\n", argv[0], term);
	      exit (1);
	    }
	  fprintf (stderr, "Using %s\n", term);
#ifdef HAVE_WINDOW_SYSTEM
	  inhibit_window_system = 1; /* -t => -nw */
#endif
	}
      else
	break;
    }

  /* Command line option --no-windows is deprecated and thus not mentioned
     in the manual and usage information.  */
  if (argmatch (argv, argc, "-nw", "--no-window-system", 6, NULL, &skip_args)
      || argmatch (argv, argc, "-nw", "--no-windows", 6, NULL, &skip_args))
    inhibit_window_system = 1;

  /* Handle the -batch switch, which means don't do interactive display.  */
  noninteractive = 0;
  if (argmatch (argv, argc, "-batch", "--batch", 5, NULL, &skip_args))
    {
      noninteractive = 1;
      Vundo_outer_limit = Qnil;
    }
  if (argmatch (argv, argc, "-script", "--script", 3, &junk, &skip_args))
    {
      noninteractive = 1;	/* Set batch mode.  */
      /* Convert --script to -scriptload, un-skip it, and sort again
	 so that it will be handled in proper sequence.  */
      /* FIXME broken for --script=FILE - is that supposed to work?  */
      argv[skip_args - 1] = (char *) "-scriptload";
      skip_args -= 2;
      sort_args (argc, argv);
    }

  /* Handle the --help option, which gives a usage message.  */
  if (argmatch (argv, argc, "-help", "--help", 3, NULL, &skip_args))
    {
      printf (USAGE1, argv[0], USAGE2);
      printf (USAGE3);
      printf (USAGE4);
      exit (0);
    }

  if (argmatch (argv, argc, "-daemon", "--daemon", 5, NULL, &skip_args)
      || argmatch (argv, argc, "-daemon", "--daemon", 5, &dname_arg, &skip_args))
    {
#ifndef DOS_NT
      pid_t f;

      /* Start as a daemon: fork a new child process which will run the
	 rest of the initialization code, then exit.

	 Detaching a daemon requires the following steps:
	 - fork
	 - setsid
	 - exit the parent
	 - close the tty file-descriptors

	 We only want to do the last 2 steps once the daemon is ready to
	 serve requests, i.e. after loading .emacs (initialization).
	 OTOH initialization may start subprocesses (e.g. ispell) and these
	 should be run from the proper process (the one that will end up
	 running as daemon) and with the proper "session id" in order for
	 them to keep working after detaching, so fork and setsid need to be
	 performed before initialization.

	 We want to avoid exiting before the server socket is ready, so
	 use a pipe for synchronization.  The parent waits for the child
	 to close its end of the pipe (using `daemon-initialized')
	 before exiting.  */
      if (pipe (daemon_pipe) == -1)
	{
	  fprintf (stderr, "Cannot pipe!\n");
	  exit (1);
	}

#ifndef NS_IMPL_COCOA
#ifdef USE_GTK
      fprintf (stderr, "\nWarning: due to a long standing Gtk+ bug\nhttp://bugzilla.gnome.org/show_bug.cgi?id=85715\n\
Emacs might crash when run in daemon mode and the X11 connection is unexpectedly lost.\n\
Using an Emacs configured with --with-x-toolkit=lucid does not have this problem.\n");
#endif
      f = fork ();
#else /* NS_IMPL_COCOA */
      /* Under Cocoa we must do fork+exec as CoreFoundation lib fails in
         forked process: http://developer.apple.com/ReleaseNotes/
                                  CoreFoundation/CoreFoundation.html)
         We mark being in the exec'd process by a daemon name argument of
         form "--daemon=\nFD0,FD1\nNAME" where FD are the pipe file descriptors,
         NAME is the original daemon name, if any. */
      if (!dname_arg || !strchr (dname_arg, '\n'))
	  f = fork ();  /* in orig */
      else
	  f = 0;  /* in exec'd */
#endif /* NS_IMPL_COCOA */
      if (f > 0)
	{
	  int retval;
	  char buf[1];

	  /* Close unused writing end of the pipe.  */
	  close (daemon_pipe[1]);

	  /* Just wait for the child to close its end of the pipe.  */
	  do
	    {
	      retval = read (daemon_pipe[0], &buf, 1);
	    }
	  while (retval == -1 && errno == EINTR);

	  if (retval < 0)
	    {
	      fprintf (stderr, "Error reading status from child\n");
	      exit (1);
	    }
	  else if (retval == 0)
	    {
	      fprintf (stderr, "Error: server did not start correctly\n");
	      exit (1);
	    }

	  close (daemon_pipe[0]);
	  exit (0);
	}
      if (f < 0)
	{
	  fprintf (stderr, "Cannot fork!\n");
	  exit (1);
	}

#ifdef NS_IMPL_COCOA
      {
        /* In orig process, forked as child, OR in exec'd. */
        if (!dname_arg || !strchr (dname_arg, '\n'))
          {  /* In orig, child: now exec w/special daemon name. */
            char fdStr[80];
	    int fdStrlen =
	      snprintf (fdStr, sizeof fdStr,
			"--daemon=\n%d,%d\n%s", daemon_pipe[0],
			daemon_pipe[1], dname_arg ? dname_arg : "");

	    if (! (0 <= fdStrlen && fdStrlen < sizeof fdStr))
              {
                fprintf (stderr, "daemon: child name too long\n");
                exit (1);
              }

            argv[skip_args] = fdStr;

            execv (argv[0], argv);
            fprintf (stderr, "emacs daemon: exec failed: %d\n", errno);
            exit (1);
          }

        /* In exec'd: parse special dname into pipe and name info. */
        if (!dname_arg || !strchr (dname_arg, '\n')
            || strlen (dname_arg) < 1 || strlen (dname_arg) > 70)
          {
            fprintf (stderr, "emacs daemon: daemon name absent or too long\n");
            exit (1);
          }
        dname_arg2[0] = '\0';
        sscanf (dname_arg, "\n%d,%d\n%s", &(daemon_pipe[0]), &(daemon_pipe[1]),
                dname_arg2);
        dname_arg = *dname_arg2 ? dname_arg2 : NULL;
      }
#endif /* NS_IMPL_COCOA */

      if (dname_arg)
       	daemon_name = xstrdup (dname_arg);
      /* Close unused reading end of the pipe.  */
      close (daemon_pipe[0]);
      /* Make sure that the used end of the pipe is closed on exec, so
	 that it is not accessible to programs started from .emacs.  */
      fcntl (daemon_pipe[1], F_SETFD, FD_CLOEXEC);

#ifdef HAVE_SETSID
      setsid ();
#endif
#else /* DOS_NT */
      fprintf (stderr, "This platform does not support the -daemon flag.\n");
      exit (1);
#endif /* DOS_NT */
    }

  if (! noninteractive)
    {
#if defined (USG5) && defined (INTERRUPT_INPUT)
      setpgrp ();
#endif
#if defined (HAVE_PTHREAD) && !defined (SYSTEM_MALLOC) && !defined (DOUG_LEA_MALLOC)
      {
        extern void malloc_enable_thread (void);

	malloc_enable_thread ();
      }
#endif
    }

  init_signals ();

  /* Don't catch SIGHUP if dumping.  */
  if (1
#ifndef CANNOT_DUMP
      && initialized
#endif
      )
    {
      sigblock (sigmask (SIGHUP));
      /* In --batch mode, don't catch SIGHUP if already ignored.
	 That makes nohup work.  */
      if (! noninteractive
	  || signal (SIGHUP, SIG_IGN) != SIG_IGN)
	signal (SIGHUP, fatal_error_signal);
      sigunblock (sigmask (SIGHUP));
    }

  if (
#ifndef CANNOT_DUMP
      ! noninteractive || initialized
#else
      1
#endif
      )
    {
      /* Don't catch these signals in batch mode if dumping.
	 On some machines, this sets static data that would make
	 signal fail to work right when the dumped Emacs is run.  */
      signal (SIGQUIT, fatal_error_signal);
      signal (SIGILL, fatal_error_signal);
      signal (SIGTRAP, fatal_error_signal);
#ifdef SIGUSR1
      add_user_signal (SIGUSR1, "sigusr1");
#endif
#ifdef SIGUSR2
      add_user_signal (SIGUSR2, "sigusr2");
#endif
#ifdef SIGABRT
      signal (SIGABRT, fatal_error_signal);
#endif
#ifdef SIGHWE
      signal (SIGHWE, fatal_error_signal);
#endif
#ifdef SIGPRE
      signal (SIGPRE, fatal_error_signal);
#endif
#ifdef SIGORE
      signal (SIGORE, fatal_error_signal);
#endif
#ifdef SIGUME
      signal (SIGUME, fatal_error_signal);
#endif
#ifdef SIGDLK
      signal (SIGDLK, fatal_error_signal);
#endif
#ifdef SIGCPULIM
      signal (SIGCPULIM, fatal_error_signal);
#endif
#ifdef SIGIOT
      /* This is missing on some systems - OS/2, for example.  */
      signal (SIGIOT, fatal_error_signal);
#endif
#ifdef SIGEMT
      signal (SIGEMT, fatal_error_signal);
#endif
      signal (SIGFPE, fatal_error_signal);
#ifdef SIGBUS
      signal (SIGBUS, fatal_error_signal);
#endif
      signal (SIGSEGV, fatal_error_signal);
#ifdef SIGSYS
      signal (SIGSYS, fatal_error_signal);
#endif
      /*  May need special treatment on MS-Windows. See
          http://lists.gnu.org/archive/html/emacs-devel/2010-09/msg01062.html
          Please update the doc of kill-emacs, kill-emacs-hook, and
          NEWS if you change this.
      */
      if (noninteractive) signal (SIGINT, fatal_error_signal);
      signal (SIGTERM, fatal_error_signal);
#ifdef SIGXCPU
      signal (SIGXCPU, fatal_error_signal);
#endif
#ifdef SIGXFSZ
      signal (SIGXFSZ, fatal_error_signal);
#endif /* SIGXFSZ */

#ifdef SIGDANGER
      /* This just means available memory is getting low.  */
      signal (SIGDANGER, memory_warning_signal);
#endif

#ifdef AIX
/* 20 is SIGCHLD, 21 is SIGTTIN, 22 is SIGTTOU.  */
      signal (SIGXCPU, fatal_error_signal);
      signal (SIGIOINT, fatal_error_signal);
      signal (SIGGRANT, fatal_error_signal);
      signal (SIGRETRACT, fatal_error_signal);
      signal (SIGSOUND, fatal_error_signal);
      signal (SIGMSG, fatal_error_signal);
#endif /* AIX */
    }

  noninteractive1 = noninteractive;

/* Perform basic initializations (not merely interning symbols).  */

  if (!initialized)
    {
      init_alloc_once ();
      init_obarray ();
      init_eval_once ();
      init_character_once ();
      init_charset_once ();
      init_coding_once ();
      init_syntax_once ();	/* Create standard syntax table.  */
      init_category_once ();	/* Create standard category table.  */
		      /* Must be done before init_buffer.  */
      init_casetab_once ();
      init_buffer_once ();	/* Create buffer table and some buffers.  */
      init_minibuf_once ();	/* Create list of minibuffers.  */
				/* Must precede init_window_once.  */

      /* Call syms_of_xfaces before init_window_once because that
	 function creates Vterminal_frame.  Termcap frames now use
	 faces, and the face implementation uses some symbols as
	 face names.  */
      syms_of_xfaces ();
      /* XXX syms_of_keyboard uses some symbols in keymap.c.  It would
         be better to arrange things not to have this dependency.  */
      syms_of_keymap ();
      /* Call syms_of_keyboard before init_window_once because
	 keyboard sets up symbols that include some face names that
	 the X support will want to use.  This can happen when
	 CANNOT_DUMP is defined.  */
      syms_of_keyboard ();

      /* Called before syms_of_fileio, because it sets up Qerror_condition.  */
      syms_of_data ();
      syms_of_fileio ();
      /* Before syms_of_coding to initialize Vgc_cons_threshold.  */
      syms_of_alloc ();
      /* Before syms_of_coding because it initializes Qcharsetp.  */
      syms_of_charset ();
      /* Before init_window_once, because it sets up the
	 Vcoding_system_hash_table.  */
      syms_of_coding ();	/* This should be after syms_of_fileio.  */

      init_window_once ();	/* Init the window system.  */
#ifdef HAVE_WINDOW_SYSTEM
      init_fringe_once ();	/* Swap bitmaps if necessary. */
#endif /* HAVE_WINDOW_SYSTEM */
    }

  init_alloc ();

  if (do_initial_setlocale)
    {
      fixup_locale ();
      Vsystem_messages_locale = Vprevious_system_messages_locale;
      Vsystem_time_locale = Vprevious_system_time_locale;
    }

  init_eval ();
  init_data ();
#ifdef CLASH_DETECTION
  init_filelock ();
#endif
  init_atimer ();
  running_asynch_code = 0;

  no_loadup
    = argmatch (argv, argc, "-nl", "--no-loadup", 6, NULL, &skip_args);

  no_site_lisp
    = argmatch (argv, argc, "-nsl", "--no-site-lisp", 11, NULL, &skip_args);

#ifdef HAVE_NS
  ns_pool = ns_alloc_autorelease_pool ();
  if (!noninteractive)
    {
#ifdef NS_IMPL_COCOA
      if (skip_args < argc)
        {
	  /* FIXME: Do the right thing if getenv returns NULL, or if
	     chdir fails.  */
          if (!strncmp (argv[skip_args], "-psn", 4))
            {
              skip_args += 1;
              chdir (getenv ("HOME"));
            }
          else if (skip_args+1 < argc && !strncmp (argv[skip_args+1], "-psn", 4))
            {
              skip_args += 2;
              chdir (getenv ("HOME"));
            }
        }
#endif  /* COCOA */
    }
#endif /* HAVE_NS */

#ifdef HAVE_X_WINDOWS
  /* Stupid kludge to catch command-line display spec.  We can't
     handle this argument entirely in window system dependent code
     because we don't even know which window system dependent code
     to run until we've recognized this argument.  */
  {
    char *displayname = 0;
    int count_before = skip_args;

    /* Skip any number of -d options, but only use the last one.  */
    while (1)
      {
	int count_before_this = skip_args;

	if (argmatch (argv, argc, "-d", "--display", 3, &displayname, &skip_args))
	  display_arg = 1;
	else if (argmatch (argv, argc, "-display", 0, 3, &displayname, &skip_args))
	  display_arg = 1;
	else
	  break;

	count_before = count_before_this;
      }

    /* If we have the form --display=NAME,
       convert it into  -d name.
       This requires inserting a new element into argv.  */
    if (displayname && count_before < skip_args)
      {
	if (skip_args == count_before + 1)
	  {
	    memmove (argv + count_before + 3, argv + count_before + 2,
		     (argc - (count_before + 2)) * sizeof *argv);
	    argv[count_before + 2] = displayname;
	    argc++;
	  }
	argv[count_before + 1] = (char *) "-d";
      }

    if (! no_site_lisp)
      {
        if (argmatch (argv, argc, "-Q", "--quick", 3, NULL, &skip_args)
            || argmatch (argv, argc, "-quick", 0, 2, NULL, &skip_args))
          no_site_lisp = 1;
      }

    /* Don't actually discard this arg.  */
    skip_args = count_before;
  }
#else  /* !HAVE_X_WINDOWS */
  if (! no_site_lisp)
  {
    int count_before = skip_args;

    if (argmatch (argv, argc, "-Q", "--quick", 3, NULL, &skip_args)
        || argmatch (argv, argc, "-quick", 0, 2, NULL, &skip_args))
      no_site_lisp = 1;

    skip_args = count_before;
  }
#endif

  /* argmatch must not be used after here,
     except when building temacs
     because the -d argument has not been skipped in skip_args.  */

#ifdef MSDOS
  /* Call early 'cause init_environment needs it.  */
  init_dosfns ();
  /* Set defaults for several environment variables.  */
  if (initialized)
    init_environment (argc, argv, skip_args);
  else
    tzset ();
#endif /* MSDOS */

#ifdef WINDOWSNT
  globals_of_w32 ();
  /* Initialize environment from registry settings.  */
  init_environment (argv);
  init_ntproc ();	/* must precede init_editfns.  */
#endif

#ifdef HAVE_NS
#ifndef CANNOT_DUMP
  if (initialized)
#endif
    ns_init_paths ();
#endif

  /* Initialize and GC-protect Vinitial_environment and
     Vprocess_environment before set_initial_environment fills them
     in.  */
  if (!initialized)
    syms_of_callproc ();
  /* egetenv is a pretty low-level facility, which may get called in
     many circumstances; it seems flimsy to put off initializing it
     until calling init_callproc.  Do not do it when dumping.  */
  if (initialized || ((strcmp (argv[argc-1], "dump") != 0
		       && strcmp (argv[argc-1], "bootstrap") != 0)))
    set_initial_environment ();

  /* AIX crashes are reported in system versions 3.2.3 and 3.2.4
     if this is not done.  Do it after set_global_environment so that we
     don't pollute Vglobal_environment.  */
  /* Setting LANG here will defeat the startup locale processing...  */
#ifdef AIX
  putenv ("LANG=C");
#endif

  init_buffer ();	/* Init default directory of main buffer.  */

  init_callproc_1 ();	/* Must precede init_cmdargs and init_sys_modes.  */
  init_cmdargs (argc, argv, skip_args);	/* Must precede init_lread.  */

  if (initialized)
    {
      /* Erase any pre-dump messages in the message log, to avoid confusion.  */
      Lisp_Object old_log_max;
      old_log_max = Vmessage_log_max;
      XSETFASTINT (Vmessage_log_max, 0);
      message_dolog ("", 0, 1, 0);
      Vmessage_log_max = old_log_max;
    }

  init_callproc ();	/* Must follow init_cmdargs but not init_sys_modes.  */
  init_lread ();

  /* Intern the names of all standard functions and variables;
     define standard keys.  */

  if (!initialized)
    {
      /* The basic levels of Lisp must come first.  Note that
	 syms_of_data and some others have already been called.  */
      syms_of_chartab ();
      syms_of_lread ();
      syms_of_print ();
      syms_of_eval ();
      syms_of_fns ();
      syms_of_floatfns ();

      syms_of_buffer ();
      syms_of_bytecode ();
      syms_of_callint ();
      syms_of_casefiddle ();
      syms_of_casetab ();
      syms_of_category ();
      syms_of_ccl ();
      syms_of_character ();
      syms_of_cmds ();
      syms_of_dired ();
      syms_of_display ();
      syms_of_doc ();
      syms_of_editfns ();
      syms_of_emacs ();
      syms_of_filelock ();
      syms_of_indent ();
      syms_of_insdel ();
      /* syms_of_keymap (); */
      syms_of_macros ();
      syms_of_marker ();
      syms_of_minibuf ();
      syms_of_process ();
      syms_of_search ();
      syms_of_frame ();
      syms_of_syntax ();
      syms_of_terminal ();
      syms_of_term ();
      syms_of_undo ();
#ifdef HAVE_SOUND
      syms_of_sound ();
#endif
      syms_of_textprop ();
      syms_of_composite ();
#ifdef WINDOWSNT
      syms_of_ntproc ();
#endif /* WINDOWSNT */
      syms_of_window ();
      syms_of_xdisp ();
      syms_of_font ();
#ifdef HAVE_WINDOW_SYSTEM
      syms_of_fringe ();
      syms_of_image ();
#endif /* HAVE_WINDOW_SYSTEM */
#ifdef HAVE_X_WINDOWS
      syms_of_xterm ();
      syms_of_xfns ();
      syms_of_xmenu ();
      syms_of_fontset ();
      syms_of_xsettings ();
#ifdef HAVE_X_SM
      syms_of_xsmfns ();
#endif
#ifdef HAVE_X11
      syms_of_xselect ();
#endif
#endif /* HAVE_X_WINDOWS */

#ifdef HAVE_LIBXML2
      syms_of_xml ();
#endif

      syms_of_menu ();

#ifdef HAVE_NTGUI
      syms_of_w32term ();
      syms_of_w32fns ();
      syms_of_w32select ();
      syms_of_w32menu ();
      syms_of_fontset ();
#endif /* HAVE_NTGUI */

#ifdef MSDOS
      syms_of_xmenu ();
      syms_of_dosfns ();
      syms_of_msdos ();
      syms_of_win16select ();
#endif	/* MSDOS */

#ifdef HAVE_NS
      syms_of_nsterm ();
      syms_of_nsfns ();
      syms_of_nsmenu ();
      syms_of_nsselect ();
      syms_of_fontset ();
#endif /* HAVE_NS */

#ifdef HAVE_GNUTLS
      syms_of_gnutls ();
#endif

#ifdef HAVE_DBUS
      syms_of_dbusbind ();
#endif /* HAVE_DBUS */

#ifdef WINDOWSNT
      syms_of_ntterm ();
#endif /* WINDOWSNT */

      keys_of_casefiddle ();
      keys_of_cmds ();
      keys_of_buffer ();
      keys_of_keyboard ();
      keys_of_keymap ();
      keys_of_window ();
    }
  else
    {
      /* Initialization that must be done even if the global variable
	 initialized is non zero.  */
#ifdef HAVE_NTGUI
      globals_of_w32font ();
      globals_of_w32fns ();
      globals_of_w32menu ();
      globals_of_w32select ();
#endif  /* HAVE_NTGUI */
    }

  init_charset ();

  init_editfns (); /* init_process uses Voperating_system_release. */
  init_process (); /* init_display uses add_keyboard_wait_descriptor. */
  init_keyboard ();	/* This too must precede init_sys_modes.  */
  if (!noninteractive)
    init_display ();	/* Determine terminal type.  Calls init_sys_modes.  */
  init_fns ();
  init_xdisp ();
#ifdef HAVE_WINDOW_SYSTEM
  init_fringe ();
  init_image ();
#endif /* HAVE_WINDOW_SYSTEM */
  init_macros ();
  init_floatfns ();
#ifdef HAVE_SOUND
  init_sound ();
#endif
  init_window ();
  init_font ();

  if (!initialized)
    {
      char *file;
      /* Handle -l loadup, args passed by Makefile.  */
      if (argmatch (argv, argc, "-l", "--load", 3, &file, &skip_args))
	Vtop_level = Fcons (intern_c_string ("load"),
			    Fcons (build_string (file), Qnil));
      /* Unless next switch is -nl, load "loadup.el" first thing.  */
      if (! no_loadup)
	Vtop_level = Fcons (intern_c_string ("load"),
			    Fcons (build_string ("loadup.el"), Qnil));
    }

  if (initialized)
    {
#ifdef HAVE_TZSET
      {
	/* If the execution TZ happens to be the same as the dump TZ,
	   change it to some other value and then change it back,
	   to force the underlying implementation to reload the TZ info.
	   This is needed on implementations that load TZ info from files,
	   since the TZ file contents may differ between dump and execution.  */
	char *tz = getenv ("TZ");
	if (tz && !strcmp (tz, dump_tz))
	  {
	    ++*tz;
	    tzset ();
	    --*tz;
	  }
      }
#endif
    }

  /* Set up for profiling.  This is known to work on FreeBSD,
     GNU/Linux and MinGW.  It might work on some other systems too.
     Give it a try and tell us if it works on your system.  To compile
     for profiling, use the configure option --enable-profiling.  */
#if defined (__FreeBSD__) || defined (GNU_LINUX) || defined (__MINGW32__)
#ifdef PROFILING
  if (initialized)
    {
      extern void _mcleanup ();
#ifdef __MINGW32__
      extern unsigned char etext asm ("etext");
#else
      extern char etext;
#endif
#ifdef HAVE___EXECUTABLE_START
      /* This symbol is defined by GNU ld to the start of the text
	 segment.  */
      extern char __executable_start[];
#else
      extern void safe_bcopy ();
#endif

      atexit (_mcleanup);
#ifdef HAVE___EXECUTABLE_START
      monstartup (__executable_start, &etext);
#else
      /* This uses safe_bcopy because that function comes first in the
	 Emacs executable.  It might be better to use something that
	 gives the start of the text segment, but start_of_text is not
	 defined on all systems now.  */
      /* FIXME: Does not work on architectures with function
	 descriptors.  */
      monstartup (safe_bcopy, &etext);
#endif
    }
  else
    moncontrol (0);
#endif
#endif

  initialized = 1;

#ifdef LOCALTIME_CACHE
  /* Some versions of localtime have a bug.  They cache the value of the time
     zone rather than looking it up every time.  Since localtime() is
     called to bolt the undumping time into the undumped emacs, this
     results in localtime ignoring the TZ environment variable.
     This flushes the new TZ value into localtime.  */
  tzset ();
#endif /* defined (LOCALTIME_CACHE) */

  /* Enter editor command loop.  This never returns.  */
  Frecursive_edit ();
  /* NOTREACHED */
  return 0;
}

/* Sort the args so we can find the most important ones
   at the beginning of argv.  */

/* First, here's a table of all the standard options.  */

struct standard_args
{
  const char *name;
  const char *longname;
  int priority;
  int nargs;
};

static const struct standard_args standard_args[] =
{
  { "-version", "--version", 150, 0 },
  { "-chdir", "--chdir", 130, 1 },
  { "-t", "--terminal", 120, 1 },
  { "-nw", "--no-window-system", 110, 0 },
  { "-nw", "--no-windows", 110, 0 },
  { "-batch", "--batch", 100, 0 },
  { "-script", "--script", 100, 1 },
  { "-daemon", "--daemon", 99, 0 },
  { "-help", "--help", 90, 0 },
  { "-nl", "--no-loadup", 70, 0 },
  { "-nsl", "--no-site-lisp", 65, 0 },
  /* -d must come last before the options handled in startup.el.  */
  { "-d", "--display", 60, 1 },
  { "-display", 0, 60, 1 },
  /* Now for the options handled in `command-line' (startup.el).  */
  /* (Note that to imply -nsl, -Q is partially handled here.)  */
  { "-Q", "--quick", 55, 0 },
  { "-quick", 0, 55, 0 },
  { "-q", "--no-init-file", 50, 0 },
  { "-no-init-file", 0, 50, 0 },
  { "-no-site-file", "--no-site-file", 40, 0 },
  { "-u", "--user", 30, 1 },
  { "-user", 0, 30, 1 },
  { "-debug-init", "--debug-init", 20, 0 },
  { "-iconic", "--iconic", 15, 0 },
  { "-D", "--basic-display", 12, 0},
  { "-basic-display", 0, 12, 0},
  { "-nbc", "--no-blinking-cursor", 12, 0 },
  /* Now for the options handled in `command-line-1' (startup.el).  */
  { "-nbi", "--no-bitmap-icon", 10, 0 },
  { "-bg", "--background-color", 10, 1 },
  { "-background", 0, 10, 1 },
  { "-fg", "--foreground-color", 10, 1 },
  { "-foreground", 0, 10, 1 },
  { "-bd", "--border-color", 10, 1 },
  { "-bw", "--border-width", 10, 1 },
  { "-ib", "--internal-border", 10, 1 },
  { "-ms", "--mouse-color", 10, 1 },
  { "-cr", "--cursor-color", 10, 1 },
  { "-fn", "--font", 10, 1 },
  { "-font", 0, 10, 1 },
  { "-fs", "--fullscreen", 10, 0 },
  { "-fw", "--fullwidth", 10, 0 },
  { "-fh", "--fullheight", 10, 0 },
  { "-mm", "--maximized", 10, 0 },
  { "-g", "--geometry", 10, 1 },
  { "-geometry", 0, 10, 1 },
  { "-T", "--title", 10, 1 },
  { "-title", 0, 10, 1 },
  { "-name", "--name", 10, 1 },
  { "-xrm", "--xrm", 10, 1 },
  { "-parent-id", "--parent-id", 10, 1 },
  { "-r", "--reverse-video", 5, 0 },
  { "-rv", 0, 5, 0 },
  { "-reverse", 0, 5, 0 },
  { "-hb", "--horizontal-scroll-bars", 5, 0 },
  { "-vb", "--vertical-scroll-bars", 5, 0 },
  { "-color", "--color", 5, 0},
  { "-no-splash", "--no-splash", 3, 0 },
  { "-no-desktop", "--no-desktop", 3, 0 },
#ifdef HAVE_NS
  { "-NSAutoLaunch", 0, 5, 1 },
  { "-NXAutoLaunch", 0, 5, 1 },
  { "-disable-font-backend", "--disable-font-backend", 65, 0 },
  { "-_NSMachLaunch", 0, 85, 1 },
  { "-MachLaunch", 0, 85, 1 },
  { "-macosx", 0, 85, 0 },
  { "-NSHost", 0, 85, 1 },
#endif
  /* These have the same priority as ordinary file name args,
     so they are not reordered with respect to those.  */
  { "-L", "--directory", 0, 1 },
  { "-directory", 0, 0, 1 },
  { "-l", "--load", 0, 1 },
  { "-load", 0, 0, 1 },
  /* This has no longname, because using --scriptload confuses sort_args,
     because then the --script long option seems to match twice; ie
     you can't have a long option which is a prefix of another long
     option.  In any case, this is entirely an internal option.  */
  { "-scriptload", NULL, 0, 1 },
  { "-f", "--funcall", 0, 1 },
  { "-funcall", 0, 0, 1 },
  { "-eval", "--eval", 0, 1 },
  { "-execute", "--execute", 0, 1 },
  { "-find-file", "--find-file", 0, 1 },
  { "-visit", "--visit", 0, 1 },
  { "-file", "--file", 0, 1 },
  { "-insert", "--insert", 0, 1 },
#ifdef HAVE_NS
  { "-NXOpen", 0, 0, 1 },
  { "-NXOpenTemp", 0, 0, 1 },
  { "-NSOpen", 0, 0, 1 },
  { "-NSOpenTemp", 0, 0, 1 },
  { "-GSFilePath", 0, 0, 1 },
#endif
  /* This should be processed after ordinary file name args and the like.  */
  { "-kill", "--kill", -10, 0 },
};

/* Reorder the elements of ARGV (assumed to have ARGC elements)
   so that the highest priority ones come first.
   Do not change the order of elements of equal priority.
   If an option takes an argument, keep it and its argument together.

   If an option that takes no argument appears more
   than once, eliminate all but one copy of it.  */

static void
sort_args (int argc, char **argv)
{
  char **new = (char **) xmalloc (sizeof (char *) * argc);
  /* For each element of argv,
     the corresponding element of options is:
     0 for an option that takes no arguments,
     1 for an option that takes one argument, etc.
     -1 for an ordinary non-option argument.  */
  int *options = xnmalloc (argc, sizeof *options);
  int *priority = xnmalloc (argc, sizeof *priority);
  int to = 1;
  int incoming_used = 1;
  int from;
  int i;

  /* Categorize all the options,
     and figure out which argv elts are option arguments.  */
  for (from = 1; from < argc; from++)
    {
      options[from] = -1;
      priority[from] = 0;
      if (argv[from][0] == '-')
	{
	  int match;

	  /* If we have found "--", don't consider
	     any more arguments as options.  */
	  if (argv[from][1] == '-' && argv[from][2] == 0)
	    {
	      /* Leave the "--", and everything following it, at the end.  */
	      for (; from < argc; from++)
		{
		  priority[from] = -100;
		  options[from] = -1;
		}
	      break;
	    }

	  /* Look for a match with a known old-fashioned option.  */
	  for (i = 0; i < sizeof (standard_args) / sizeof (standard_args[0]); i++)
	    if (!strcmp (argv[from], standard_args[i].name))
	      {
		options[from] = standard_args[i].nargs;
		priority[from] = standard_args[i].priority;
		if (from + standard_args[i].nargs >= argc)
		  fatal ("Option `%s' requires an argument\n", argv[from]);
		from += standard_args[i].nargs;
		goto done;
	      }

	  /* Look for a match with a known long option.
	     MATCH is -1 if no match so far, -2 if two or more matches so far,
	     >= 0 (the table index of the match) if just one match so far.  */
	  if (argv[from][1] == '-')
	    {
	      char const *equals = strchr (argv[from], '=');
	      ptrdiff_t thislen =
		equals ? equals - argv[from] : strlen (argv[from]);

	      match = -1;

	      for (i = 0;
		   i < sizeof (standard_args) / sizeof (standard_args[0]); i++)
		if (standard_args[i].longname
		    && !strncmp (argv[from], standard_args[i].longname,
				 thislen))
		  {
		    if (match == -1)
		      match = i;
		    else
		      match = -2;
		  }

	      /* If we found exactly one match, use that.  */
	      if (match >= 0)
		{
		  options[from] = standard_args[match].nargs;
		  priority[from] = standard_args[match].priority;
		  /* If --OPTION=VALUE syntax is used,
		     this option uses just one argv element.  */
		  if (equals != 0)
		    options[from] = 0;
		  if (from + options[from] >= argc)
		    fatal ("Option `%s' requires an argument\n", argv[from]);
		  from += options[from];
		}
	      /* FIXME When match < 0, shouldn't there be some error,
		 or at least indication to the user that there was a
		 problem?  */
	    }
	done: ;
	}
    }

  /* Copy the arguments, in order of decreasing priority, to NEW.  */
  new[0] = argv[0];
  while (incoming_used < argc)
    {
      int best = -1;
      int best_priority = -9999;

      /* Find the highest priority remaining option.
	 If several have equal priority, take the first of them.  */
      for (from = 1; from < argc; from++)
	{
	  if (argv[from] != 0 && priority[from] > best_priority)
	    {
	      best_priority = priority[from];
	      best = from;
	    }
	  /* Skip option arguments--they are tied to the options.  */
	  if (options[from] > 0)
	    from += options[from];
	}

      if (best < 0)
	abort ();

      /* Copy the highest priority remaining option, with its args, to NEW.
         Unless it is a duplicate of the previous one.  */
      if (! (options[best] == 0
	     && ! strcmp (new[to - 1], argv[best])))
	{
	  new[to++] = argv[best];
	  for (i = 0; i < options[best]; i++)
	    new[to++] = argv[best + i + 1];
	}

      incoming_used += 1 + (options[best] > 0 ? options[best] : 0);

      /* Clear out this option in ARGV.  */
      argv[best] = 0;
      for (i = 0; i < options[best]; i++)
	argv[best + i + 1] = 0;
    }

  /* If duplicate options were deleted, fill up extra space with null ptrs.  */
  while (to < argc)
    new[to++] = 0;

  memcpy (argv, new, sizeof (char *) * argc);

  xfree (options);
  xfree (new);
  xfree (priority);
}

DEFUN ("kill-emacs", Fkill_emacs, Skill_emacs, 0, 1, "P",
       doc: /* Exit the Emacs job and kill it.
If ARG is an integer, return ARG as the exit program code.
If ARG is a string, stuff it as keyboard input.

This function is called upon receipt of the signals SIGTERM
or SIGHUP, and upon SIGINT in batch mode.

The value of `kill-emacs-hook', if not void,
is a list of functions (of no args),
all of which are called before Emacs is actually killed.  */)
  (Lisp_Object arg)
{
  struct gcpro gcpro1;
  Lisp_Object hook;
  int exit_code;

  GCPRO1 (arg);

  if (feof (stdin))
    arg = Qt;

  hook = intern ("kill-emacs-hook");
  Frun_hooks (1, &hook);

  UNGCPRO;

#ifdef HAVE_X_WINDOWS
  /* Transfer any clipboards we own to the clipboard manager.  */
  x_clipboard_manager_save_all ();
#endif

  shut_down_emacs (0, 0, STRINGP (arg) ? arg : Qnil);

#ifdef HAVE_NS
  ns_release_autorelease_pool (ns_pool);
#endif

  /* If we have an auto-save list file,
     kill it because we are exiting Emacs deliberately (not crashing).
     Do it after shut_down_emacs, which does an auto-save.  */
  if (STRINGP (Vauto_save_list_file_name))
    unlink (SSDATA (Vauto_save_list_file_name));

  exit_code = EXIT_SUCCESS;
  if (noninteractive && (fflush (stdout) || ferror (stdout)))
    exit_code = EXIT_FAILURE;
  exit (INTEGERP (arg) ? XINT (arg) : exit_code);
}


/* Perform an orderly shutdown of Emacs.  Autosave any modified
   buffers, kill any child processes, clean up the terminal modes (if
   we're in the foreground), and other stuff like that.  Don't perform
   any redisplay; this may be called when Emacs is shutting down in
   the background, or after its X connection has died.

   If SIG is a signal number, print a message for it.

   This is called by fatal signal handlers, X protocol error handlers,
   and Fkill_emacs.  */

void
shut_down_emacs (int sig, int no_x, Lisp_Object stuff)
{
  /* Prevent running of hooks from now on.  */
  Vrun_hooks = Qnil;

  /* Don't update display from now on.  */
  Vinhibit_redisplay = Qt;

  /* If we are controlling the terminal, reset terminal modes.  */
#ifndef DOS_NT
  {
    int pgrp = EMACS_GETPGRP (0);
    int tpgrp = tcgetpgrp (0);
    if ((tpgrp != -1) && tpgrp == pgrp)
      {
	reset_all_sys_modes ();
	if (sig && sig != SIGTERM)
	  fprintf (stderr, "Fatal error (%d)", sig);
      }
  }
#else
  fflush (stdout);
  reset_all_sys_modes ();
#endif

  stuff_buffered_input (stuff);

  inhibit_sentinels = 1;
  kill_buffer_processes (Qnil);
  Fdo_auto_save (Qt, Qnil);

#ifdef CLASH_DETECTION
  unlock_all_files ();
#endif

#if 0 /* This triggers a bug in XCloseDisplay and is not needed.  */
#ifdef HAVE_X_WINDOWS
  /* It's not safe to call intern here.  Maybe we are crashing.  */
  if (!noninteractive && SYMBOLP (Vinitial_window_system)
      && SCHARS (SYMBOL_NAME (Vinitial_window_system)) == 1
      && SREF (SYMBOL_NAME (Vinitial_window_system), 0) == 'x'
      && ! no_x)
    Fx_close_current_connection ();
#endif /* HAVE_X_WINDOWS */
#endif

#ifdef SIGIO
  /* There is a tendency for a SIGIO signal to arrive within exit,
     and cause a SIGHUP because the input descriptor is already closed.  */
  unrequest_sigio ();
  signal (SIGIO, SIG_IGN);
#endif

#ifdef WINDOWSNT
  term_ntproc ();
#endif

  /* Do this only if terminating normally, we want glyph matrices
     etc. in a core dump.  */
  if (sig == 0 || sig == SIGTERM)
    {
      check_glyph_memory ();
      check_message_stack ();
    }

#ifdef MSDOS
  dos_cleanup ();
#endif

#ifdef HAVE_NS
  ns_term_shutdown (sig);
#endif

#ifdef HAVE_LIBXML2
  xmlCleanupParser ();
#endif
}



#ifndef CANNOT_DUMP

#include "unexec.h"

DEFUN ("dump-emacs", Fdump_emacs, Sdump_emacs, 2, 2, 0,
       doc: /* Dump current state of Emacs into executable file FILENAME.
Take symbols from SYMFILE (presumably the file you executed to run Emacs).
This is used in the file `loadup.el' when building Emacs.

You must run Emacs in batch mode in order to dump it.  */)
  (Lisp_Object filename, Lisp_Object symfile)
{
  Lisp_Object tem;
  Lisp_Object symbol;
  int count = SPECPDL_INDEX ();

  check_pure_size ();

  if (! noninteractive)
    error ("Dumping Emacs works only in batch mode");

#ifdef GNU_LINUX

  /* Warn if the gap between BSS end and heap start is larger than this.  */
# define MAX_HEAP_BSS_DIFF (1024*1024)

  if (heap_bss_diff > MAX_HEAP_BSS_DIFF)
    {
      fprintf (stderr, "**************************************************\n");
      fprintf (stderr, "Warning: Your system has a gap between BSS and the\n");
      fprintf (stderr, "heap (%"pMu" bytes).  This usually means that exec-shield\n",
               heap_bss_diff);
      fprintf (stderr, "or something similar is in effect.  The dump may\n");
      fprintf (stderr, "fail because of this.  See the section about\n");
      fprintf (stderr, "exec-shield in etc/PROBLEMS for more information.\n");
      fprintf (stderr, "**************************************************\n");
    }
#endif /* GNU_LINUX */

  /* Bind `command-line-processed' to nil before dumping,
     so that the dumped Emacs will process its command line
     and set up to work with X windows if appropriate.  */
  symbol = intern ("command-line-processed");
  specbind (symbol, Qnil);

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);
  if (!NILP (symfile))
    {
      CHECK_STRING (symfile);
      if (SCHARS (symfile))
	symfile = Fexpand_file_name (symfile, Qnil);
    }

  tem = Vpurify_flag;
  Vpurify_flag = Qnil;

#ifdef HAVE_TZSET
  set_time_zone_rule (dump_tz);
#ifndef LOCALTIME_CACHE
  /* Force a tz reload, since set_time_zone_rule doesn't.  */
  tzset ();
#endif
#endif

  fflush (stdout);
  /* Tell malloc where start of impure now is.  */
  /* Also arrange for warnings when nearly out of space.  */
#ifndef SYSTEM_MALLOC
#ifndef WINDOWSNT
  /* On Windows, this was done before dumping, and that once suffices.
     Meanwhile, my_edata is not valid on Windows.  */
  {
    extern char my_edata[];
    memory_warnings (my_edata, malloc_warning);
  }
#endif /* not WINDOWSNT */
#if defined (HAVE_PTHREAD) && !defined SYNC_INPUT
  /* Pthread may call malloc before main, and then we will get an endless
     loop, because pthread_self (see alloc.c) calls malloc the first time
     it is called on some systems.  */
  reset_malloc_hooks ();
#endif
#endif /* not SYSTEM_MALLOC */
#ifdef DOUG_LEA_MALLOC
  malloc_state_ptr = malloc_get_state ();
#endif

#ifdef USE_MMAP_FOR_BUFFERS
  mmap_set_vars (0);
#endif
  unexec (SSDATA (filename), !NILP (symfile) ? SSDATA (symfile) : 0);
#ifdef USE_MMAP_FOR_BUFFERS
  mmap_set_vars (1);
#endif
#ifdef DOUG_LEA_MALLOC
  free (malloc_state_ptr);
#endif

  Vpurify_flag = tem;

  return unbind_to (count, Qnil);
}

#endif /* not CANNOT_DUMP */

#if HAVE_SETLOCALE
/* Recover from setlocale (LC_ALL, "").  */
void
fixup_locale (void)
{
  /* The Emacs Lisp reader needs LC_NUMERIC to be "C",
     so that numbers are read and printed properly for Emacs Lisp.  */
  setlocale (LC_NUMERIC, "C");
}

/* Set system locale CATEGORY, with previous locale *PLOCALE, to
   DESIRED_LOCALE.  */
static void
synchronize_locale (int category, Lisp_Object *plocale, Lisp_Object desired_locale)
{
  if (! EQ (*plocale, desired_locale))
    {
      *plocale = desired_locale;
      setlocale (category, (STRINGP (desired_locale)
			    ? SSDATA (desired_locale)
			    : ""));
    }
}

/* Set system time locale to match Vsystem_time_locale, if possible.  */
void
synchronize_system_time_locale (void)
{
  synchronize_locale (LC_TIME, &Vprevious_system_time_locale,
		      Vsystem_time_locale);
}

/* Set system messages locale to match Vsystem_messages_locale, if
   possible.  */
void
synchronize_system_messages_locale (void)
{
#ifdef LC_MESSAGES
  synchronize_locale (LC_MESSAGES, &Vprevious_system_messages_locale,
		      Vsystem_messages_locale);
#endif
}
#endif /* HAVE_SETLOCALE */

#ifndef SEPCHAR
#define SEPCHAR ':'
#endif

Lisp_Object
decode_env_path (const char *evarname, const char *defalt)
{
  const char *path, *p;
  Lisp_Object lpath, element, tem;

  /* It's okay to use getenv here, because this function is only used
     to initialize variables when Emacs starts up, and isn't called
     after that.  */
  if (evarname != 0)
    path = getenv (evarname);
  else
    path = 0;
  if (!path)
    path = defalt;
#ifdef DOS_NT
  /* Ensure values from the environment use the proper directory separator.  */
  if (path)
    {
      char *path_copy = alloca (strlen (path) + 1);
      strcpy (path_copy, path);
      dostounix_filename (path_copy);
      path = path_copy;
    }
#endif
  lpath = Qnil;
  while (1)
    {
      p = strchr (path, SEPCHAR);
      if (!p)
	p = path + strlen (path);
      element = (p - path ? make_string (path, p - path)
		 : build_string ("."));

      /* Add /: to the front of the name
	 if it would otherwise be treated as magic.  */
      tem = Ffind_file_name_handler (element, Qt);

      /* However, if the handler says "I'm safe",
	 don't bother adding /:.  */
      if (SYMBOLP (tem))
	{
	  Lisp_Object prop;
	  prop = Fget (tem, intern ("safe-magic"));
	  if (! NILP (prop))
	    tem = Qnil;
	}

      if (! NILP (tem))
	element = concat2 (build_string ("/:"), element);

      lpath = Fcons (element, lpath);
      if (*p)
	path = p + 1;
      else
	break;
    }
  return Fnreverse (lpath);
}

DEFUN ("daemonp", Fdaemonp, Sdaemonp, 0, 0, 0,
       doc: /* Return non-nil if the current emacs process is a daemon.
If the daemon was given a name argument, return that name. */)
  (void)
{
  if (IS_DAEMON)
    if (daemon_name)
      return build_string (daemon_name);
    else
      return Qt;
  else
    return Qnil;
}

DEFUN ("daemon-initialized", Fdaemon_initialized, Sdaemon_initialized, 0, 0, 0,
       doc: /* Mark the Emacs daemon as being initialized.
This finishes the daemonization process by doing the other half of detaching
from the parent process and its tty file descriptors.  */)
  (void)
{
  int nfd;
  int err = 0;

  if (!IS_DAEMON)
    error ("This function can only be called if emacs is run as a daemon");

  if (daemon_pipe[1] < 0)
    error ("The daemon has already been initialized");

  if (NILP (Vafter_init_time))
    error ("This function can only be called after loading the init files");

  /* Get rid of stdin, stdout and stderr.  */
  nfd = open ("/dev/null", O_RDWR);
  err |= nfd < 0;
  err |= dup2 (nfd, 0) < 0;
  err |= dup2 (nfd, 1) < 0;
  err |= dup2 (nfd, 2) < 0;
  err |= close (nfd) != 0;

  /* Closing the pipe will notify the parent that it can exit.
     FIXME: In case some other process inherited the pipe, closing it here
     won't notify the parent because it's still open elsewhere, so we
     additionally send a byte, just to make sure the parent really exits.
     Instead, we should probably close the pipe in start-process and
     call-process to make sure the pipe is never inherited by
     subprocesses.  */
  err |= write (daemon_pipe[1], "\n", 1) < 0;
  err |= close (daemon_pipe[1]) != 0;
  /* Set it to an invalid value so we know we've already run this function.  */
  daemon_pipe[1] = -1;

  if (err)
    error ("I/O error during daemon initialization");
  return Qt;
}

void
syms_of_emacs (void)
{
  DEFSYM (Qfile_name_handler_alist, "file-name-handler-alist");
  DEFSYM (Qrisky_local_variable, "risky-local-variable");
  DEFSYM (Qkill_emacs, "kill-emacs");

#ifndef CANNOT_DUMP
  defsubr (&Sdump_emacs);
#endif

  defsubr (&Skill_emacs);

  defsubr (&Sinvocation_name);
  defsubr (&Sinvocation_directory);
  defsubr (&Sdaemonp);
  defsubr (&Sdaemon_initialized);

  DEFVAR_LISP ("command-line-args", Vcommand_line_args,
	       doc: /* Args passed by shell to Emacs, as a list of strings.
Many arguments are deleted from the list as they are processed.  */);

  DEFVAR_LISP ("system-type", Vsystem_type,
	       doc: /* The value is a symbol indicating the type of operating system you are using.
Special values:
  `gnu'          compiled for a GNU Hurd system.
  `gnu/linux'    compiled for a GNU/Linux system.
  `gnu/kfreebsd' compiled for a GNU system with a FreeBSD kernel.
  `darwin'       compiled for Darwin (GNU-Darwin, Mac OS X, ...).
  `ms-dos'       compiled as an MS-DOS application.
  `windows-nt'   compiled as a native W32 application.
  `cygwin'       compiled using the Cygwin library.
Anything else (in Emacs 24.1, the possibilities are: aix, berkeley-unix,
hpux, irix, usg-unix-v) indicates some sort of Unix system.  */);
  Vsystem_type = intern_c_string (SYSTEM_TYPE);
  /* The above values are from SYSTEM_TYPE in include files under src/s.  */

  DEFVAR_LISP ("system-configuration", Vsystem_configuration,
	       doc: /* Value is string indicating configuration Emacs was built for.
On MS-Windows, the value reflects the OS flavor and version on which
Emacs is running.  */);
  Vsystem_configuration = build_string (EMACS_CONFIGURATION);

  DEFVAR_LISP ("system-configuration-options", Vsystem_configuration_options,
	       doc: /* String containing the configuration options Emacs was built with.  */);
  Vsystem_configuration_options = build_string (EMACS_CONFIG_OPTIONS);

  DEFVAR_BOOL ("noninteractive", noninteractive1,
	       doc: /* Non-nil means Emacs is running without interactive terminal.  */);

  DEFVAR_LISP ("kill-emacs-hook", Vkill_emacs_hook,
	       doc: /* Hook to be run when `kill-emacs' is called.
Since `kill-emacs' may be invoked when the terminal is disconnected (or
in other similar situations), functions placed on this hook should not
expect to be able to interact with the user.  To ask for confirmation,
see `kill-emacs-query-functions' instead.

Before Emacs 24.1, the hook was not run in batch mode, i.e., if
`noninteractive' was non-nil.  */);
  Vkill_emacs_hook = Qnil;

  DEFVAR_LISP ("path-separator", Vpath_separator,
	       doc: /* String containing the character that separates directories in
search paths, such as PATH and other similar environment variables.  */);
  {
    char c = SEPCHAR;
    Vpath_separator = make_string (&c, 1);
  }

  DEFVAR_LISP ("invocation-name", Vinvocation_name,
	       doc: /* The program name that was used to run Emacs.
Any directory names are omitted.  */);

  DEFVAR_LISP ("invocation-directory", Vinvocation_directory,
	       doc: /* The directory in which the Emacs executable was found, to run it.
The value is nil if that directory's name is not known.  */);

  DEFVAR_LISP ("installation-directory", Vinstallation_directory,
	       doc: /* A directory within which to look for the `lib-src' and `etc' directories.
In an installed Emacs, this is normally nil.  It is non-nil if
both `lib-src' (on MS-DOS, `info') and `etc' directories are found
within the variable `invocation-directory' or its parent.  For example,
this is the case when running an uninstalled Emacs executable from its
build directory.  */);
  Vinstallation_directory = Qnil;

  DEFVAR_LISP ("system-messages-locale", Vsystem_messages_locale,
	       doc: /* System locale for messages.  */);
  Vsystem_messages_locale = Qnil;

  DEFVAR_LISP ("previous-system-messages-locale",
	       Vprevious_system_messages_locale,
	       doc: /* Most recently used system locale for messages.  */);
  Vprevious_system_messages_locale = Qnil;

  DEFVAR_LISP ("system-time-locale", Vsystem_time_locale,
	       doc: /* System locale for time.  */);
  Vsystem_time_locale = Qnil;

  DEFVAR_LISP ("previous-system-time-locale", Vprevious_system_time_locale,
	       doc: /* Most recently used system locale for time.  */);
  Vprevious_system_time_locale = Qnil;

  DEFVAR_LISP ("before-init-time", Vbefore_init_time,
	       doc: /* Value of `current-time' before Emacs begins initialization.  */);
  Vbefore_init_time = Qnil;

  DEFVAR_LISP ("after-init-time", Vafter_init_time,
	       doc: /* Value of `current-time' after loading the init files.
This is nil during initialization.  */);
  Vafter_init_time = Qnil;

  DEFVAR_BOOL ("inhibit-x-resources", inhibit_x_resources,
	       doc: /* If non-nil, X resources, Windows Registry settings, and NS defaults are not used.  */);
  inhibit_x_resources = 0;

  DEFVAR_LISP ("emacs-copyright", Vemacs_copyright,
	       doc: /* Short copyright string for this version of Emacs.  */);
  Vemacs_copyright = build_string (emacs_copyright);

  DEFVAR_LISP ("emacs-version", Vemacs_version,
	       doc: /* Version numbers of this version of Emacs.  */);
  Vemacs_version = build_string (emacs_version);

  DEFVAR_LISP ("dynamic-library-alist", Vdynamic_library_alist,
    doc: /* Alist of dynamic libraries vs external files implementing them.
Each element is a list (LIBRARY FILE...), where the car is a symbol
representing a supported external library, and the rest are strings giving
alternate filenames for that library.

Emacs tries to load the library from the files in the order they appear on
the list; if none is loaded, the running session of Emacs won't have access
to that library.

Note that image types `pbm' and `xbm' do not need entries in this variable
because they do not depend on external libraries and are always available.

Also note that this is not a generic facility for accessing external
libraries; only those already known by Emacs will be loaded.  */);
  Vdynamic_library_alist = Qnil;
  Fput (intern_c_string ("dynamic-library-alist"), Qrisky_local_variable, Qt);

  /* Make sure IS_DAEMON starts up as false.  */
  daemon_pipe[1] = 0;
}
