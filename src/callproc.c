/* Synchronous subprocess invocation for GNU Emacs.
   Copyright (C) 1985-1988, 1993-1995, 1999-2012
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
#include <setjmp.h>
#include <sys/types.h>
#include <unistd.h>

#include <sys/file.h>
#include <fcntl.h>

#include "lisp.h"

#ifdef WINDOWSNT
#define NOMINMAX
#include <windows.h>
#include "w32.h"
#define _P_NOWAIT 1	/* from process.h */
#endif

#ifdef MSDOS	/* Demacs 1.1.1 91/10/16 HIRANO Satoshi */
#include <sys/stat.h>
#include <sys/param.h>
#endif /* MSDOS */

#include "commands.h"
#include "buffer.h"
#include "character.h"
#include "ccl.h"
#include "coding.h"
#include "composite.h"
#include <epaths.h>
#include "process.h"
#include "syssignal.h"
#include "systty.h"
#include "blockinput.h"
#include "frame.h"
#include "termhooks.h"

#ifdef MSDOS
#include "msdos.h"
#endif

#ifndef USE_CRT_DLL
extern char **environ;
#endif

#ifdef HAVE_SETPGID
#if !defined (USG)
#undef setpgrp
#define setpgrp setpgid
#endif
#endif

/* Pattern used by call-process-region to make temp files.  */
static Lisp_Object Vtemp_file_name_pattern;

/* True if we are about to fork off a synchronous process or if we
   are waiting for it.  */
int synch_process_alive;

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
const char *synch_process_death;

/* Nonzero => this is the signal number that terminated the subprocess.  */
int synch_process_termsig;

/* If synch_process_death is zero,
   this is exit code of synchronous subprocess.  */
int synch_process_retcode;


/* Clean up when exiting Fcall_process.
   On MSDOS, delete the temporary file on any kind of termination.
   On Unix, kill the process and any children on termination by signal.  */

/* Nonzero if this is termination due to exit.  */
static int call_process_exited;

static Lisp_Object Fgetenv_internal (Lisp_Object, Lisp_Object);

static Lisp_Object
call_process_kill (Lisp_Object fdpid)
{
  emacs_close (XFASTINT (Fcar (fdpid)));
  EMACS_KILLPG (XFASTINT (Fcdr (fdpid)), SIGKILL);
  synch_process_alive = 0;
  return Qnil;
}

static Lisp_Object
call_process_cleanup (Lisp_Object arg)
{
  Lisp_Object fdpid = Fcdr (arg);
#if defined (MSDOS)
  Lisp_Object file;
  int fd;
#else
  int pid;
#endif

  Fset_buffer (Fcar (arg));

#if defined (MSDOS)
  /* for MSDOS fdpid is really (fd . tempfile)  */
  fd = XFASTINT (Fcar (fdpid));
  file = Fcdr (fdpid);
  /* FD is -1 and FILE is "" when we didn't actually create a
     temporary file in call-process.  */
  if (fd >= 0)
    emacs_close (fd);
  if (!(strcmp (SDATA (file), NULL_DEVICE) == 0 || SREF (file, 0) == '\0'))
    unlink (SDATA (file));
#else /* not MSDOS */
  pid = XFASTINT (Fcdr (fdpid));

  if (call_process_exited)
    {
      emacs_close (XFASTINT (Fcar (fdpid)));
      return Qnil;
    }

  if (EMACS_KILLPG (pid, SIGINT) == 0)
    {
      int count = SPECPDL_INDEX ();
      record_unwind_protect (call_process_kill, fdpid);
      message1 ("Waiting for process to die...(type C-g again to kill it instantly)");
      immediate_quit = 1;
      QUIT;
      wait_for_termination (pid);
      immediate_quit = 0;
      specpdl_ptr = specpdl + count; /* Discard the unwind protect.  */
      message1 ("Waiting for process to die...done");
    }
  synch_process_alive = 0;
  emacs_close (XFASTINT (Fcar (fdpid)));
#endif /* not MSDOS */
  return Qnil;
}

DEFUN ("call-process", Fcall_process, Scall_process, 1, MANY, 0,
       doc: /* Call PROGRAM synchronously in separate process.
The remaining arguments are optional.
The program's input comes from file INFILE (nil means `/dev/null').
Insert output in BUFFER before point; t means current buffer; nil for BUFFER
 means discard it; 0 means discard and don't wait; and `(:file FILE)', where
 FILE is a file name string, means that it should be written to that file
 \(if the file already exists it is overwritten).
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining arguments are strings passed as command arguments to PROGRAM.

If executable PROGRAM can't be found as an executable, `call-process'
signals a Lisp error.  `call-process' reports errors in execution of
the program only through its return and output.

If BUFFER is 0, `call-process' returns immediately with value nil.
Otherwise it waits for PROGRAM to terminate
and returns a numeric exit status or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.

usage: (call-process PROGRAM &optional INFILE BUFFER DISPLAY &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object infile, buffer, current_dir, path;
  volatile int display_p_volatile;
  int fd[2];
  int filefd;
  register int pid;
#define CALLPROC_BUFFER_SIZE_MIN (16 * 1024)
#define CALLPROC_BUFFER_SIZE_MAX (4 * CALLPROC_BUFFER_SIZE_MIN)
  char buf[CALLPROC_BUFFER_SIZE_MAX];
  int bufsize = CALLPROC_BUFFER_SIZE_MIN;
  int count = SPECPDL_INDEX ();
  volatile USE_SAFE_ALLOCA;

  register const unsigned char **new_argv;
  /* File to use for stderr in the child.
     t means use same as standard output.  */
  Lisp_Object error_file;
  Lisp_Object output_file = Qnil;
#ifdef MSDOS	/* Demacs 1.1.1 91/10/16 HIRANO Satoshi */
  char *outf, *tempfile = NULL;
  int outfilefd;
#endif
  int fd_output = -1;
  struct coding_system process_coding; /* coding-system of process output */
  struct coding_system argument_coding;	/* coding-system of arguments */
  /* Set to the return value of Ffind_operation_coding_system.  */
  Lisp_Object coding_systems;
  int output_to_buffer = 1;

  /* Qt denotes that Ffind_operation_coding_system is not yet called.  */
  coding_systems = Qt;

  CHECK_STRING (args[0]);

  error_file = Qt;

#ifndef subprocesses
  /* Without asynchronous processes we cannot have BUFFER == 0.  */
  if (nargs >= 3
      && (INTEGERP (CONSP (args[2]) ? XCAR (args[2]) : args[2])))
    error ("Operating system cannot handle asynchronous subprocesses");
#endif /* subprocesses */

  /* Decide the coding-system for giving arguments.  */
  {
    Lisp_Object val, *args2;
    ptrdiff_t i;

    /* If arguments are supplied, we may have to encode them.  */
    if (nargs >= 5)
      {
	int must_encode = 0;
	Lisp_Object coding_attrs;

	for (i = 4; i < nargs; i++)
	  CHECK_STRING (args[i]);

	for (i = 4; i < nargs; i++)
	  if (STRING_MULTIBYTE (args[i]))
	    must_encode = 1;

	if (!NILP (Vcoding_system_for_write))
	  val = Vcoding_system_for_write;
	else if (! must_encode)
	  val = Qraw_text;
	else
	  {
	    SAFE_NALLOCA (args2, 1, nargs + 1);
	    args2[0] = Qcall_process;
	    for (i = 0; i < nargs; i++) args2[i + 1] = args[i];
	    coding_systems = Ffind_operation_coding_system (nargs + 1, args2);
	    val = CONSP (coding_systems) ? XCDR (coding_systems) : Qnil;
	  }
	val = complement_process_encoding_system (val);
	setup_coding_system (Fcheck_coding_system (val), &argument_coding);
	coding_attrs = CODING_ID_ATTRS (argument_coding.id);
	if (NILP (CODING_ATTR_ASCII_COMPAT (coding_attrs)))
	  {
	    /* We should not use an ASCII incompatible coding system.  */
	    val = raw_text_coding_system (val);
	    setup_coding_system (val, &argument_coding);
	  }
      }
  }

  if (nargs >= 2 && ! NILP (args[1]))
    {
      infile = Fexpand_file_name (args[1], BVAR (current_buffer, directory));
      CHECK_STRING (infile);
    }
  else
    infile = build_string (NULL_DEVICE);

  if (nargs >= 3)
    {
      buffer = args[2];

      /* If BUFFER is a list, its meaning is (BUFFER-FOR-STDOUT
	 FILE-FOR-STDERR), unless the first element is :file, in which case see
	 the next paragraph. */
      if (CONSP (buffer)
	  && (! SYMBOLP (XCAR (buffer))
	      || strcmp (SSDATA (SYMBOL_NAME (XCAR (buffer))), ":file")))
	{
	  if (CONSP (XCDR (buffer)))
	    {
	      Lisp_Object stderr_file;
	      stderr_file = XCAR (XCDR (buffer));

	      if (NILP (stderr_file) || EQ (Qt, stderr_file))
		error_file = stderr_file;
	      else
		error_file = Fexpand_file_name (stderr_file, Qnil);
	    }

	  buffer = XCAR (buffer);
	}

      /* If the buffer is (still) a list, it might be a (:file "file") spec. */
      if (CONSP (buffer)
	  && SYMBOLP (XCAR (buffer))
	  && ! strcmp (SSDATA (SYMBOL_NAME (XCAR (buffer))), ":file"))
	{
	  output_file = Fexpand_file_name (XCAR (XCDR (buffer)),
					   BVAR (current_buffer, directory));
	  CHECK_STRING (output_file);
	  buffer = Qnil;
	}

      if (!(EQ (buffer, Qnil)
	    || EQ (buffer, Qt)
	    || INTEGERP (buffer)))
	{
	  Lisp_Object spec_buffer;
	  spec_buffer = buffer;
	  buffer = Fget_buffer_create (buffer);
	  /* Mention the buffer name for a better error message.  */
	  if (NILP (buffer))
	    CHECK_BUFFER (spec_buffer);
	  CHECK_BUFFER (buffer);
	}
    }
  else
    buffer = Qnil;

  /* Make sure that the child will be able to chdir to the current
     buffer's current directory, or its unhandled equivalent.  We
     can't just have the child check for an error when it does the
     chdir, since it's in a vfork.

     We have to GCPRO around this because Fexpand_file_name,
     Funhandled_file_name_directory, and Ffile_accessible_directory_p
     might call a file name handling function.  The argument list is
     protected by the caller, so all we really have to worry about is
     buffer.  */
  {
    struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

    current_dir = BVAR (current_buffer, directory);

    GCPRO5 (infile, buffer, current_dir, error_file, output_file);

    current_dir = Funhandled_file_name_directory (current_dir);
    if (NILP (current_dir))
      /* If the file name handler says that current_dir is unreachable, use
	 a sensible default. */
      current_dir = build_string ("~/");
    current_dir = expand_and_dir_to_file (current_dir, Qnil);
    current_dir = Ffile_name_as_directory (current_dir);

    if (NILP (Ffile_accessible_directory_p (current_dir)))
      report_file_error ("Setting current directory",
			 Fcons (BVAR (current_buffer, directory), Qnil));

    if (STRING_MULTIBYTE (infile))
      infile = ENCODE_FILE (infile);
    if (STRING_MULTIBYTE (current_dir))
      current_dir = ENCODE_FILE (current_dir);
    if (STRINGP (error_file) && STRING_MULTIBYTE (error_file))
      error_file = ENCODE_FILE (error_file);
    if (STRINGP (output_file) && STRING_MULTIBYTE (output_file))
      output_file = ENCODE_FILE (output_file);
    UNGCPRO;
  }

  display_p_volatile = INTERACTIVE && nargs >= 4 && !NILP (args[3]);

  filefd = emacs_open (SSDATA (infile), O_RDONLY, 0);
  if (filefd < 0)
    {
      infile = DECODE_FILE (infile);
      report_file_error ("Opening process input file", Fcons (infile, Qnil));
    }

  if (STRINGP (output_file))
    {
#ifdef DOS_NT
      fd_output = emacs_open (SSDATA (output_file),
			      O_WRONLY | O_TRUNC | O_CREAT | O_TEXT,
			      S_IREAD | S_IWRITE);
#else  /* not DOS_NT */
      fd_output = creat (SSDATA (output_file), 0666);
#endif /* not DOS_NT */
      if (fd_output < 0)
	{
	  output_file = DECODE_FILE (output_file);
	  report_file_error ("Opening process output file",
			     Fcons (output_file, Qnil));
	}
      if (STRINGP (error_file) || NILP (error_file))
	output_to_buffer = 0;
    }

  /* Search for program; barf if not found.  */
  {
    struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

    GCPRO4 (infile, buffer, current_dir, error_file);
    openp (Vexec_path, args[0], Vexec_suffixes, &path, make_number (X_OK));
    UNGCPRO;
  }
  if (NILP (path))
    {
      emacs_close (filefd);
      report_file_error ("Searching for program", Fcons (args[0], Qnil));
    }

  /* If program file name starts with /: for quoting a magic name,
     discard that.  */
  if (SBYTES (path) > 2 && SREF (path, 0) == '/'
      && SREF (path, 1) == ':')
    path = Fsubstring (path, make_number (2), Qnil);

  SAFE_ALLOCA (new_argv, const unsigned char **,
	       (nargs > 4 ? nargs - 2 : 2) * sizeof *new_argv);
  if (nargs > 4)
    {
      ptrdiff_t i;
      struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

      GCPRO5 (infile, buffer, current_dir, path, error_file);
      argument_coding.dst_multibyte = 0;
      for (i = 4; i < nargs; i++)
	{
	  argument_coding.src_multibyte = STRING_MULTIBYTE (args[i]);
	  if (CODING_REQUIRE_ENCODING (&argument_coding))
	    /* We must encode this argument.  */
	    args[i] = encode_coding_string (&argument_coding, args[i], 1);
	}
      UNGCPRO;
      for (i = 4; i < nargs; i++)
	new_argv[i - 3] = SDATA (args[i]);
      new_argv[i - 3] = 0;
    }
  else
    new_argv[1] = 0;
  new_argv[0] = SDATA (path);

#ifdef MSDOS /* MW, July 1993 */

  /* If we're redirecting STDOUT to a file, that file is already open
     on fd_output.  */
  if (fd_output < 0)
    {
      if ((outf = egetenv ("TMPDIR")))
	strcpy (tempfile = alloca (strlen (outf) + 20), outf);
      else
	{
	  tempfile = alloca (20);
	  *tempfile = '\0';
	}
      dostounix_filename (tempfile);
      if (*tempfile == '\0' || tempfile[strlen (tempfile) - 1] != '/')
	strcat (tempfile, "/");
      strcat (tempfile, "detmp.XXX");
      mktemp (tempfile);
      outfilefd = creat (tempfile, S_IREAD | S_IWRITE);
      if (outfilefd < 0) {
	emacs_close (filefd);
	report_file_error ("Opening process output file",
			   Fcons (build_string (tempfile), Qnil));
      }
    }
  else
    outfilefd = fd_output;
  fd[0] = filefd;
  fd[1] = outfilefd;
#endif /* MSDOS */

  if (INTEGERP (buffer))
    fd[1] = emacs_open (NULL_DEVICE, O_WRONLY, 0), fd[0] = -1;
  else
    {
#ifndef MSDOS
      errno = 0;
      if (pipe (fd) == -1)
	{
	  emacs_close (filefd);
	  report_file_error ("Creating process pipe", Qnil);
	}
#endif
    }

  {
    /* child_setup must clobber environ in systems with true vfork.
       Protect it from permanent change.  */
    register char **save_environ = environ;
    register int fd1 = fd[1];
    int fd_error = fd1;
#ifdef HAVE_WORKING_VFORK
    sigset_t procmask;
    sigset_t blocked;
    struct sigaction sigpipe_action;
#endif

    if (fd_output >= 0)
      fd1 = fd_output;
#if 0  /* Some systems don't have sigblock.  */
    mask = sigblock (sigmask (SIGCHLD));
#endif

    /* Record that we're about to create a synchronous process.  */
    synch_process_alive = 1;

    /* These vars record information from process termination.
       Clear them now before process can possibly terminate,
       to avoid timing error if process terminates soon.  */
    synch_process_death = 0;
    synch_process_retcode = 0;
    synch_process_termsig = 0;

    if (NILP (error_file))
      fd_error = emacs_open (NULL_DEVICE, O_WRONLY, 0);
    else if (STRINGP (error_file))
      {
#ifdef DOS_NT
	fd_error = emacs_open (SSDATA (error_file),
			       O_WRONLY | O_TRUNC | O_CREAT | O_TEXT,
			       S_IREAD | S_IWRITE);
#else  /* not DOS_NT */
	fd_error = creat (SSDATA (error_file), 0666);
#endif /* not DOS_NT */
      }

    if (fd_error < 0)
      {
	emacs_close (filefd);
	if (fd[0] != filefd)
	  emacs_close (fd[0]);
	if (fd1 >= 0)
	  emacs_close (fd1);
#ifdef MSDOS
	unlink (tempfile);
#endif
	if (NILP (error_file))
	  error_file = build_string (NULL_DEVICE);
	else if (STRINGP (error_file))
	  error_file = DECODE_FILE (error_file);
	report_file_error ("Cannot redirect stderr", Fcons (error_file, Qnil));
      }

#ifdef MSDOS /* MW, July 1993 */
    /* Note that on MSDOS `child_setup' actually returns the child process
       exit status, not its PID, so we assign it to `synch_process_retcode'
       below.  */
    pid = child_setup (filefd, outfilefd, fd_error, (char **) new_argv,
		       0, current_dir);

    /* Record that the synchronous process exited and note its
       termination status.  */
    synch_process_alive = 0;
    synch_process_retcode = pid;
    if (synch_process_retcode < 0)  /* means it couldn't be exec'ed */
      {
	synchronize_system_messages_locale ();
	synch_process_death = strerror (errno);
      }

    emacs_close (outfilefd);
    if (fd_error != outfilefd)
      emacs_close (fd_error);
    fd1 = -1; /* No harm in closing that one!  */
    if (tempfile)
      {
	/* Since CRLF is converted to LF within `decode_coding', we
	   can always open a file with binary mode.  */
	fd[0] = emacs_open (tempfile, O_RDONLY | O_BINARY, 0);
	if (fd[0] < 0)
	  {
	    unlink (tempfile);
	    emacs_close (filefd);
	    report_file_error ("Cannot re-open temporary file",
			       Fcons (build_string (tempfile), Qnil));
	  }
      }
    else
      fd[0] = -1; /* We are not going to read from tempfile.   */
#else /* not MSDOS */
#ifdef WINDOWSNT
    pid = child_setup (filefd, fd1, fd_error, (char **) new_argv,
		       0, current_dir);
#else  /* not WINDOWSNT */

#ifdef HAVE_WORKING_VFORK
    /* On many hosts (e.g. Solaris 2.4), if a vforked child calls `signal',
       this sets the parent's signal handlers as well as the child's.
       So delay all interrupts whose handlers the child might munge,
       and record the current handlers so they can be restored later.  */
    sigemptyset (&blocked);
    sigaddset (&blocked, SIGPIPE);
    sigaction (SIGPIPE, 0, &sigpipe_action);
    pthread_sigmask (SIG_BLOCK, &blocked, &procmask);
#endif

    BLOCK_INPUT;

    /* vfork, and prevent local vars from being clobbered by the vfork.  */
    {
      Lisp_Object volatile buffer_volatile = buffer;
      Lisp_Object volatile coding_systems_volatile = coding_systems;
      Lisp_Object volatile current_dir_volatile = current_dir;
      int volatile fd1_volatile = fd1;
      int volatile fd_error_volatile = fd_error;
      int volatile fd_output_volatile = fd_output;
      int volatile output_to_buffer_volatile = output_to_buffer;
      unsigned char const **volatile new_argv_volatile = new_argv;

      pid = vfork ();

      buffer = buffer_volatile;
      coding_systems = coding_systems_volatile;
      current_dir = current_dir_volatile;
      fd1 = fd1_volatile;
      fd_error = fd_error_volatile;
      fd_output = fd_output_volatile;
      output_to_buffer = output_to_buffer_volatile;
      new_argv = new_argv_volatile;
    }

    if (pid == 0)
      {
	if (fd[0] >= 0)
	  emacs_close (fd[0]);
#ifdef HAVE_SETSID
	setsid ();
#endif
#if defined (USG)
	setpgrp ();
#else
	setpgrp (pid, pid);
#endif /* USG */

	/* GConf causes us to ignore SIGPIPE, make sure it is restored
	   in the child.  */
	signal (SIGPIPE, SIG_DFL);
#ifdef HAVE_WORKING_VFORK
	pthread_sigmask (SIG_SETMASK, &procmask, 0);
#endif

	child_setup (filefd, fd1, fd_error, (char **) new_argv,
		     0, current_dir);
      }

    UNBLOCK_INPUT;

#ifdef HAVE_WORKING_VFORK
    /* Restore the signal state.  */
    sigaction (SIGPIPE, &sigpipe_action, 0);
    pthread_sigmask (SIG_SETMASK, &procmask, 0);
#endif

#endif /* not WINDOWSNT */

    /* The MSDOS case did this already.  */
    if (fd_error >= 0)
      emacs_close (fd_error);
#endif /* not MSDOS */

    environ = save_environ;

    /* Close most of our fd's, but not fd[0]
       since we will use that to read input from.  */
    emacs_close (filefd);
    if (fd_output >= 0)
      emacs_close (fd_output);
    if (fd1 >= 0 && fd1 != fd_error)
      emacs_close (fd1);
  }

  if (pid < 0)
    {
      if (fd[0] >= 0)
	emacs_close (fd[0]);
      report_file_error ("Doing vfork", Qnil);
    }

  if (INTEGERP (buffer))
    {
      if (fd[0] >= 0)
	emacs_close (fd[0]);
      return Qnil;
    }

  /* Enable sending signal if user quits below.  */
  call_process_exited = 0;

#if defined (MSDOS)
  /* MSDOS needs different cleanup information.  */
  record_unwind_protect (call_process_cleanup,
			 Fcons (Fcurrent_buffer (),
				Fcons (make_number (fd[0]),
				       build_string (tempfile ? tempfile : ""))));
#else
  record_unwind_protect (call_process_cleanup,
			 Fcons (Fcurrent_buffer (),
				Fcons (make_number (fd[0]), make_number (pid))));
#endif /* not MSDOS */


  if (BUFFERP (buffer))
    Fset_buffer (buffer);

  if (NILP (buffer))
    {
      /* If BUFFER is nil, we must read process output once and then
	 discard it, so setup coding system but with nil.  */
      setup_coding_system (Qnil, &process_coding);
      process_coding.dst_multibyte = 0;
    }
  else
    {
      Lisp_Object val, *args2;

      val = Qnil;
      if (!NILP (Vcoding_system_for_read))
	val = Vcoding_system_for_read;
      else
	{
	  if (EQ (coding_systems, Qt))
	    {
	      ptrdiff_t i;

	      SAFE_NALLOCA (args2, 1, nargs + 1);
	      args2[0] = Qcall_process;
	      for (i = 0; i < nargs; i++) args2[i + 1] = args[i];
	      coding_systems
		= Ffind_operation_coding_system (nargs + 1, args2);
	    }
	  if (CONSP (coding_systems))
	    val = XCAR (coding_systems);
	  else if (CONSP (Vdefault_process_coding_system))
	    val = XCAR (Vdefault_process_coding_system);
	  else
	    val = Qnil;
	}
      Fcheck_coding_system (val);
      /* In unibyte mode, character code conversion should not take
	 place but EOL conversion should.  So, setup raw-text or one
	 of the subsidiary according to the information just setup.  */
      if (NILP (BVAR (current_buffer, enable_multibyte_characters))
	  && !NILP (val))
	val = raw_text_coding_system (val);
      setup_coding_system (val, &process_coding);
      process_coding.dst_multibyte
	= ! NILP (BVAR (current_buffer, enable_multibyte_characters));
    }
  process_coding.src_multibyte = 0;

  immediate_quit = 1;
  QUIT;

  if (output_to_buffer)
    {
      register EMACS_INT nread;
      int first = 1;
      EMACS_INT total_read = 0;
      int carryover = 0;
      int display_p = display_p_volatile;
      int display_on_the_fly = display_p;
      struct coding_system saved_coding;

      saved_coding = process_coding;
      while (1)
	{
	  /* Repeatedly read until we've filled as much as possible
	     of the buffer size we have.  But don't read
	     less than 1024--save that for the next bufferful.  */
	  nread = carryover;
	  while (nread < bufsize - 1024)
	    {
	      int this_read = emacs_read (fd[0], buf + nread,
					  bufsize - nread);

	      if (this_read < 0)
		goto give_up;

	      if (this_read == 0)
		{
		  process_coding.mode |= CODING_MODE_LAST_BLOCK;
		  break;
		}

	      nread += this_read;
	      total_read += this_read;

	      if (display_on_the_fly)
		break;
	    }

	  /* Now NREAD is the total amount of data in the buffer.  */
	  immediate_quit = 0;

	  if (!NILP (buffer))
	    {
	      if (NILP (BVAR (current_buffer, enable_multibyte_characters))
		  && ! CODING_MAY_REQUIRE_DECODING (&process_coding))
		insert_1_both (buf, nread, nread, 0, 1, 0);
	      else
		{			/* We have to decode the input.  */
		  Lisp_Object curbuf;
		  int count1 = SPECPDL_INDEX ();

		  XSETBUFFER (curbuf, current_buffer);
		  /* We cannot allow after-change-functions be run
		     during decoding, because that might modify the
		     buffer, while we rely on process_coding.produced to
		     faithfully reflect inserted text until we
		     TEMP_SET_PT_BOTH below.  */
		  specbind (Qinhibit_modification_hooks, Qt);
		  decode_coding_c_string (&process_coding,
					  (unsigned char *) buf, nread, curbuf);
		  unbind_to (count1, Qnil);
		  if (display_on_the_fly
		      && CODING_REQUIRE_DETECTION (&saved_coding)
		      && ! CODING_REQUIRE_DETECTION (&process_coding))
		    {
		      /* We have detected some coding system.  But,
			 there's a possibility that the detection was
			 done by insufficient data.  So, we give up
			 displaying on the fly.  */
		      if (process_coding.produced > 0)
			del_range_2 (process_coding.dst_pos,
				     process_coding.dst_pos_byte,
				     process_coding.dst_pos
				     + process_coding.produced_char,
				     process_coding.dst_pos_byte
				     + process_coding.produced, 0);
		      display_on_the_fly = 0;
		      process_coding = saved_coding;
		      carryover = nread;
		      /* This is to make the above condition always
			 fails in the future.  */
		      saved_coding.common_flags
			&= ~CODING_REQUIRE_DETECTION_MASK;
		      continue;
		    }

		  TEMP_SET_PT_BOTH (PT + process_coding.produced_char,
				    PT_BYTE + process_coding.produced);
		  carryover = process_coding.carryover_bytes;
		  if (carryover > 0)
		    memcpy (buf, process_coding.carryover,
			    process_coding.carryover_bytes);
		}
	    }

	  if (process_coding.mode & CODING_MODE_LAST_BLOCK)
	    break;

	  /* Make the buffer bigger as we continue to read more data,
	     but not past CALLPROC_BUFFER_SIZE_MAX.  */
	  if (bufsize < CALLPROC_BUFFER_SIZE_MAX && total_read > 32 * bufsize)
	    if ((bufsize *= 2) > CALLPROC_BUFFER_SIZE_MAX)
	      bufsize = CALLPROC_BUFFER_SIZE_MAX;

	  if (display_p)
	    {
	      if (first)
		prepare_menu_bars ();
	      first = 0;
	      redisplay_preserve_echo_area (1);
	      /* This variable might have been set to 0 for code
		 detection.  In that case, we set it back to 1 because
		 we should have already detected a coding system.  */
	      display_on_the_fly = 1;
	    }
	  immediate_quit = 1;
	  QUIT;
	}
    give_up: ;

      Vlast_coding_system_used = CODING_ID_NAME (process_coding.id);
      /* If the caller required, let the buffer inherit the
	 coding-system used to decode the process output.  */
      if (inherit_process_coding_system)
	call1 (intern ("after-insert-file-set-buffer-file-coding-system"),
	       make_number (total_read));
    }

#ifndef MSDOS
  /* Wait for it to terminate, unless it already has.  */
  if (output_to_buffer)
    wait_for_termination (pid);
  else
    interruptible_wait_for_termination (pid);
#endif

  immediate_quit = 0;

  /* Don't kill any children that the subprocess may have left behind
     when exiting.  */
  call_process_exited = 1;

  SAFE_FREE ();
  unbind_to (count, Qnil);

  if (synch_process_termsig)
    {
      const char *signame;

      synchronize_system_messages_locale ();
      signame = strsignal (synch_process_termsig);

      if (signame == 0)
	signame = "unknown";

      synch_process_death = signame;
    }

  if (synch_process_death)
    return code_convert_string_norecord (build_string (synch_process_death),
					 Vlocale_coding_system, 0);
  return make_number (synch_process_retcode);
}

static Lisp_Object
delete_temp_file (Lisp_Object name)
{
  /* Suppress jka-compr handling, etc.  */
  int count = SPECPDL_INDEX ();
  specbind (intern ("file-name-handler-alist"), Qnil);
  internal_delete_file (name);
  unbind_to (count, Qnil);
  return Qnil;
}

DEFUN ("call-process-region", Fcall_process_region, Scall_process_region,
       3, MANY, 0,
       doc: /* Send text from START to END to a synchronous process running PROGRAM.
The remaining arguments are optional.
Delete the text if fourth arg DELETE is non-nil.

Insert output in BUFFER before point; t means current buffer; nil for
 BUFFER means discard it; 0 means discard and don't wait; and `(:file
 FILE)', where FILE is a file name string, means that it should be
 written to that file (if the file already exists it is overwritten).
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

Sixth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining args are passed to PROGRAM at startup as command args.

If BUFFER is 0, `call-process-region' returns immediately with value nil.
Otherwise it waits for PROGRAM to terminate
and returns a numeric exit status or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you quit again.

usage: (call-process-region START END PROGRAM &optional DELETE BUFFER DISPLAY &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  struct gcpro gcpro1;
  Lisp_Object filename_string;
  register Lisp_Object start, end;
  int count = SPECPDL_INDEX ();
  /* Qt denotes we have not yet called Ffind_operation_coding_system.  */
  Lisp_Object coding_systems;
  Lisp_Object val, *args2;
  ptrdiff_t i;
  char *tempfile;
  Lisp_Object tmpdir, pattern;

  if (STRINGP (Vtemporary_file_directory))
    tmpdir = Vtemporary_file_directory;
  else
    {
#ifndef DOS_NT
      if (getenv ("TMPDIR"))
	tmpdir = build_string (getenv ("TMPDIR"));
      else
	tmpdir = build_string ("/tmp/");
#else /* DOS_NT */
      char *outf;
      if ((outf = egetenv ("TMPDIR"))
	  || (outf = egetenv ("TMP"))
	  || (outf = egetenv ("TEMP")))
	tmpdir = build_string (outf);
      else
	tmpdir = Ffile_name_as_directory (build_string ("c:/temp"));
#endif
    }

  {
    USE_SAFE_ALLOCA;
    pattern = Fexpand_file_name (Vtemp_file_name_pattern, tmpdir);
    SAFE_ALLOCA (tempfile, char *, SBYTES (pattern) + 1);
    memcpy (tempfile, SDATA (pattern), SBYTES (pattern) + 1);
    coding_systems = Qt;

#ifdef HAVE_MKSTEMP
    {
      int fd;

      BLOCK_INPUT;
      fd = mkstemp (tempfile);
      UNBLOCK_INPUT;
      if (fd == -1)
	report_file_error ("Failed to open temporary file",
			   Fcons (Vtemp_file_name_pattern, Qnil));
      else
	close (fd);
    }
#else
    mktemp (tempfile);
#endif

    filename_string = build_string (tempfile);
    GCPRO1 (filename_string);
    SAFE_FREE ();
  }

  start = args[0];
  end = args[1];
  /* Decide coding-system of the contents of the temporary file.  */
  if (!NILP (Vcoding_system_for_write))
    val = Vcoding_system_for_write;
  else if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
    val = Qraw_text;
  else
    {
      USE_SAFE_ALLOCA;
      SAFE_NALLOCA (args2, 1, nargs + 1);
      args2[0] = Qcall_process_region;
      for (i = 0; i < nargs; i++) args2[i + 1] = args[i];
      coding_systems = Ffind_operation_coding_system (nargs + 1, args2);
      val = CONSP (coding_systems) ? XCDR (coding_systems) : Qnil;
      SAFE_FREE ();
    }
  val = complement_process_encoding_system (val);

  {
    int count1 = SPECPDL_INDEX ();

    specbind (intern ("coding-system-for-write"), val);
    /* POSIX lets mk[s]temp use "."; don't invoke jka-compr if we
       happen to get a ".Z" suffix.  */
    specbind (intern ("file-name-handler-alist"), Qnil);
    Fwrite_region (start, end, filename_string, Qnil, Qlambda, Qnil, Qnil);

    unbind_to (count1, Qnil);
  }

  /* Note that Fcall_process takes care of binding
     coding-system-for-read.  */

  record_unwind_protect (delete_temp_file, filename_string);

  if (nargs > 3 && !NILP (args[3]))
    Fdelete_region (start, end);

  if (nargs > 3)
    {
      args += 2;
      nargs -= 2;
    }
  else
    {
      args[0] = args[2];
      nargs = 2;
    }
  args[1] = filename_string;

  RETURN_UNGCPRO (unbind_to (count, Fcall_process (nargs, args)));
}

#ifndef WINDOWSNT
static int relocate_fd (int fd, int minfd);
#endif

static char **
add_env (char **env, char **new_env, char *string)
{
  char **ep;
  int ok = 1;
  if (string == NULL)
    return new_env;

  /* See if this string duplicates any string already in the env.
     If so, don't put it in.
     When an env var has multiple definitions,
     we keep the definition that comes first in process-environment.  */
  for (ep = env; ok && ep != new_env; ep++)
    {
      char *p = *ep, *q = string;
      while (ok)
	{
	  if (*q != *p)
	    break;
	  if (*q == 0)
	    /* The string is a lone variable name; keep it for now, we
	       will remove it later.  It is a placeholder for a
	       variable that is not to be included in the environment.  */
	    break;
	  if (*q == '=')
	    ok = 0;
	  p++, q++;
	}
    }
  if (ok)
    *new_env++ = string;
  return new_env;
}

/* This is the last thing run in a newly forked inferior
   either synchronous or asynchronous.
   Copy descriptors IN, OUT and ERR as descriptors 0, 1 and 2.
   Initialize inferior's priority, pgrp, connected dir and environment.
   then exec another program based on new_argv.

   This function may change environ for the superior process.
   Therefore, the superior process must save and restore the value
   of environ around the vfork and the call to this function.

   SET_PGRP is nonzero if we should put the subprocess into a separate
   process group.

   CURRENT_DIR is an elisp string giving the path of the current
   directory the subprocess should have.  Since we can't really signal
   a decent error from within the child, this should be verified as an
   executable directory by the parent.  */

int
child_setup (int in, int out, int err, register char **new_argv, int set_pgrp, Lisp_Object current_dir)
{
  char **env;
  char *pwd_var;
#ifdef WINDOWSNT
  int cpid;
  HANDLE handles[3];
#endif /* WINDOWSNT */

  int pid = getpid ();

  /* Close Emacs's descriptors that this process should not have.  */
  close_process_descs ();

  /* DOS_NT isn't in a vfork, so if we are in the middle of load-file,
     we will lose if we call close_load_descs here.  */
#ifndef DOS_NT
  close_load_descs ();
#endif

  /* Note that use of alloca is always safe here.  It's obvious for systems
     that do not have true vfork or that have true (stack) alloca.
     If using vfork and C_ALLOCA (when Emacs used to include
     src/alloca.c) it is safe because that changes the superior's
     static variables as if the superior had done alloca and will be
     cleaned up in the usual way. */
  {
    register char *temp;
    size_t i; /* size_t, because ptrdiff_t might overflow here!  */

    i = SBYTES (current_dir);
#ifdef MSDOS
    /* MSDOS must have all environment variables malloc'ed, because
       low-level libc functions that launch subsidiary processes rely
       on that.  */
    pwd_var = (char *) xmalloc (i + 6);
#else
    pwd_var = (char *) alloca (i + 6);
#endif
    temp = pwd_var + 4;
    memcpy (pwd_var, "PWD=", 4);
    memcpy (temp, SDATA (current_dir), i);
    if (!IS_DIRECTORY_SEP (temp[i - 1])) temp[i++] = DIRECTORY_SEP;
    temp[i] = 0;

#ifndef DOS_NT
    /* We can't signal an Elisp error here; we're in a vfork.  Since
       the callers check the current directory before forking, this
       should only return an error if the directory's permissions
       are changed between the check and this chdir, but we should
       at least check.  */
    if (chdir (temp) < 0)
      _exit (errno);
#else /* DOS_NT */
    /* Get past the drive letter, so that d:/ is left alone.  */
    if (i > 2 && IS_DEVICE_SEP (temp[1]) && IS_DIRECTORY_SEP (temp[2]))
      {
	temp += 2;
	i -= 2;
      }
#endif /* DOS_NT */

    /* Strip trailing slashes for PWD, but leave "/" and "//" alone.  */
    while (i > 2 && IS_DIRECTORY_SEP (temp[i - 1]))
      temp[--i] = 0;
  }

  /* Set `env' to a vector of the strings in the environment.  */
  {
    register Lisp_Object tem;
    register char **new_env;
    char **p, **q;
    register int new_length;
    Lisp_Object display = Qnil;

    new_length = 0;

    for (tem = Vprocess_environment;
	 CONSP (tem) && STRINGP (XCAR (tem));
	 tem = XCDR (tem))
      {
	if (strncmp (SSDATA (XCAR (tem)), "DISPLAY", 7) == 0
	    && (SDATA (XCAR (tem)) [7] == '\0'
		|| SDATA (XCAR (tem)) [7] == '='))
	  /* DISPLAY is specified in process-environment.  */
	  display = Qt;
	new_length++;
      }

    /* If not provided yet, use the frame's DISPLAY.  */
    if (NILP (display))
      {
	Lisp_Object tmp = Fframe_parameter (selected_frame, Qdisplay);
	if (!STRINGP (tmp) && CONSP (Vinitial_environment))
	  /* If still not found, Look for DISPLAY in Vinitial_environment.  */
	  tmp = Fgetenv_internal (build_string ("DISPLAY"),
				  Vinitial_environment);
	if (STRINGP (tmp))
	  {
	    display = tmp;
	    new_length++;
	  }
      }

    /* new_length + 2 to include PWD and terminating 0.  */
    env = new_env = (char **) alloca ((new_length + 2) * sizeof (char *));
    /* If we have a PWD envvar, pass one down,
       but with corrected value.  */
    if (egetenv ("PWD"))
      *new_env++ = pwd_var;

    if (STRINGP (display))
      {
	char *vdata = (char *) alloca (sizeof "DISPLAY=" + SBYTES (display));
	strcpy (vdata, "DISPLAY=");
	strcat (vdata, SSDATA (display));
	new_env = add_env (env, new_env, vdata);
      }

    /* Overrides.  */
    for (tem = Vprocess_environment;
	 CONSP (tem) && STRINGP (XCAR (tem));
	 tem = XCDR (tem))
      new_env = add_env (env, new_env, SSDATA (XCAR (tem)));

    *new_env = 0;

    /* Remove variable names without values.  */
    p = q = env;
    while (*p != 0)
      {
	while (*q != 0 && strchr (*q, '=') == NULL)
	  q++;
	*p = *q++;
	if (*p != 0)
	  p++;
      }
  }


#ifdef WINDOWSNT
  prepare_standard_handles (in, out, err, handles);
  set_process_dir (SDATA (current_dir));
  /* Spawn the child.  (See ntproc.c:Spawnve).  */
  cpid = spawnve (_P_NOWAIT, new_argv[0], new_argv, env);
  reset_standard_handles (in, out, err, handles);
  if (cpid == -1)
    /* An error occurred while trying to spawn the process.  */
    report_file_error ("Spawning child process", Qnil);
  return cpid;

#else  /* not WINDOWSNT */
  /* Make sure that in, out, and err are not actually already in
     descriptors zero, one, or two; this could happen if Emacs is
     started with its standard in, out, or error closed, as might
     happen under X.  */
  {
    int oin = in, oout = out;

    /* We have to avoid relocating the same descriptor twice!  */

    in = relocate_fd (in, 3);

    if (out == oin)
      out = in;
    else
      out = relocate_fd (out, 3);

    if (err == oin)
      err = in;
    else if (err == oout)
      err = out;
    else
      err = relocate_fd (err, 3);
  }

#ifndef MSDOS
  emacs_close (0);
  emacs_close (1);
  emacs_close (2);

  dup2 (in, 0);
  dup2 (out, 1);
  dup2 (err, 2);
  emacs_close (in);
  if (out != in)
    emacs_close (out);
  if (err != in && err != out)
    emacs_close (err);

#if defined (USG)
#ifndef SETPGRP_RELEASES_CTTY
  setpgrp ();			/* No arguments but equivalent in this case */
#endif
#else /* not USG */
  setpgrp (pid, pid);
#endif /* not USG */

  /* setpgrp_of_tty is incorrect here; it uses input_fd.  */
  tcsetpgrp (0, pid);

  /* execvp does not accept an environment arg so the only way
     to pass this environment is to set environ.  Our caller
     is responsible for restoring the ambient value of environ.  */
  environ = env;
  execvp (new_argv[0], new_argv);

  emacs_write (1, "Can't exec program: ", 20);
  emacs_write (1, new_argv[0], strlen (new_argv[0]));
  emacs_write (1, "\n", 1);
  _exit (1);

#else /* MSDOS */
  pid = run_msdos_command (new_argv, pwd_var + 4, in, out, err, env);
  xfree (pwd_var);
  if (pid == -1)
    /* An error occurred while trying to run the subprocess.  */
    report_file_error ("Spawning child process", Qnil);
  return pid;
#endif  /* MSDOS */
#endif  /* not WINDOWSNT */
}

#ifndef WINDOWSNT
/* Move the file descriptor FD so that its number is not less than MINFD.
   If the file descriptor is moved at all, the original is freed.  */
static int
relocate_fd (int fd, int minfd)
{
  if (fd >= minfd)
    return fd;
  else
    {
      int new;
#ifdef F_DUPFD
      new = fcntl (fd, F_DUPFD, minfd);
#else
      new = dup (fd);
      if (new != -1)
	/* Note that we hold the original FD open while we recurse,
	   to guarantee we'll get a new FD if we need it.  */
	new = relocate_fd (new, minfd);
#endif
      if (new == -1)
	{
	  const char *message_1 = "Error while setting up child: ";
	  const char *errmessage = strerror (errno);
	  const char *message_2 = "\n";
	  emacs_write (2, message_1, strlen (message_1));
	  emacs_write (2, errmessage, strlen (errmessage));
	  emacs_write (2, message_2, strlen (message_2));
	  _exit (1);
	}
      emacs_close (fd);
      return new;
    }
}
#endif /* not WINDOWSNT */

static int
getenv_internal_1 (const char *var, ptrdiff_t varlen, char **value,
		   ptrdiff_t *valuelen, Lisp_Object env)
{
  for (; CONSP (env); env = XCDR (env))
    {
      Lisp_Object entry = XCAR (env);
      if (STRINGP (entry)
	  && SBYTES (entry) >= varlen
#ifdef WINDOWSNT
	  /* NT environment variables are case insensitive.  */
	  && ! strnicmp (SDATA (entry), var, varlen)
#else  /* not WINDOWSNT */
	  && ! memcmp (SDATA (entry), var, varlen)
#endif /* not WINDOWSNT */
	  )
	{
	  if (SBYTES (entry) > varlen && SREF (entry, varlen) == '=')
	    {
	      *value = SSDATA (entry) + (varlen + 1);
	      *valuelen = SBYTES (entry) - (varlen + 1);
	      return 1;
	    }
	  else if (SBYTES (entry) == varlen)
	    {
	      /* Lone variable names in Vprocess_environment mean that
		 variable should be removed from the environment. */
	      *value = NULL;
	      return 1;
	    }
	}
    }
  return 0;
}

static int
getenv_internal (const char *var, ptrdiff_t varlen, char **value,
		 ptrdiff_t *valuelen, Lisp_Object frame)
{
  /* Try to find VAR in Vprocess_environment first.  */
  if (getenv_internal_1 (var, varlen, value, valuelen,
			 Vprocess_environment))
    return *value ? 1 : 0;

  /* For DISPLAY try to get the values from the frame or the initial env.  */
  if (strcmp (var, "DISPLAY") == 0)
    {
      Lisp_Object display
	= Fframe_parameter (NILP (frame) ? selected_frame : frame, Qdisplay);
      if (STRINGP (display))
	{
	  *value    = SSDATA (display);
	  *valuelen = SBYTES (display);
	  return 1;
	}
      /* If still not found, Look for DISPLAY in Vinitial_environment.  */
      if (getenv_internal_1 (var, varlen, value, valuelen,
			     Vinitial_environment))
	return *value ? 1 : 0;
    }

  return 0;
}

DEFUN ("getenv-internal", Fgetenv_internal, Sgetenv_internal, 1, 2, 0,
       doc: /* Get the value of environment variable VARIABLE.
VARIABLE should be a string.  Value is nil if VARIABLE is undefined in
the environment.  Otherwise, value is a string.

This function searches `process-environment' for VARIABLE.

If optional parameter ENV is a list, then search this list instead of
`process-environment', and return t when encountering a negative entry
\(an entry for a variable with no value).  */)
  (Lisp_Object variable, Lisp_Object env)
{
  char *value;
  ptrdiff_t valuelen;

  CHECK_STRING (variable);
  if (CONSP (env))
    {
      if (getenv_internal_1 (SSDATA (variable), SBYTES (variable),
			     &value, &valuelen, env))
	return value ? make_string (value, valuelen) : Qt;
      else
	return Qnil;
    }
  else if (getenv_internal (SSDATA (variable), SBYTES (variable),
			    &value, &valuelen, env))
    return make_string (value, valuelen);
  else
    return Qnil;
}

/* A version of getenv that consults the Lisp environment lists,
   easily callable from C.  */
char *
egetenv (const char *var)
{
  char *value;
  ptrdiff_t valuelen;

  if (getenv_internal (var, strlen (var), &value, &valuelen, Qnil))
    return value;
  else
    return 0;
}


/* This is run before init_cmdargs.  */

void
init_callproc_1 (void)
{
  char *data_dir = egetenv ("EMACSDATA");
  char *doc_dir = egetenv ("EMACSDOC");

  Vdata_directory
    = Ffile_name_as_directory (build_string (data_dir ? data_dir
					     : PATH_DATA));
  Vdoc_directory
    = Ffile_name_as_directory (build_string (doc_dir ? doc_dir
					     : PATH_DOC));

  /* Check the EMACSPATH environment variable, defaulting to the
     PATH_EXEC path from epaths.h.  */
  Vexec_path = decode_env_path ("EMACSPATH", PATH_EXEC);
  Vexec_directory = Ffile_name_as_directory (Fcar (Vexec_path));
  Vexec_path = nconc2 (decode_env_path ("PATH", ""), Vexec_path);
}

/* This is run after init_cmdargs, when Vinstallation_directory is valid.  */

void
init_callproc (void)
{
  char *data_dir = egetenv ("EMACSDATA");

  register char * sh;
  Lisp_Object tempdir;

  if (!NILP (Vinstallation_directory))
    {
      /* Add to the path the lib-src subdir of the installation dir.  */
      Lisp_Object tem;
      tem = Fexpand_file_name (build_string ("lib-src"),
			       Vinstallation_directory);
#ifndef DOS_NT
	  /* MSDOS uses wrapped binaries, so don't do this.  */
      if (NILP (Fmember (tem, Vexec_path)))
	{
	  Vexec_path = decode_env_path ("EMACSPATH", PATH_EXEC);
	  Vexec_path = Fcons (tem, Vexec_path);
	  Vexec_path = nconc2 (decode_env_path ("PATH", ""), Vexec_path);
	}

      Vexec_directory = Ffile_name_as_directory (tem);
#endif /* not DOS_NT */

      /* Maybe use ../etc as well as ../lib-src.  */
      if (data_dir == 0)
	{
	  tem = Fexpand_file_name (build_string ("etc"),
				   Vinstallation_directory);
	  Vdoc_directory = Ffile_name_as_directory (tem);
	}
    }

  /* Look for the files that should be in etc.  We don't use
     Vinstallation_directory, because these files are never installed
     near the executable, and they are never in the build
     directory when that's different from the source directory.

     Instead, if these files are not in the nominal place, we try the
     source directory.  */
  if (data_dir == 0)
    {
      Lisp_Object tem, tem1, srcdir;

      srcdir = Fexpand_file_name (build_string ("../src/"),
				  build_string (PATH_DUMPLOADSEARCH));
      tem = Fexpand_file_name (build_string ("GNU"), Vdata_directory);
      tem1 = Ffile_exists_p (tem);
      if (!NILP (Fequal (srcdir, Vinvocation_directory)) || NILP (tem1))
	{
	  Lisp_Object newdir;
	  newdir = Fexpand_file_name (build_string ("../etc/"),
				      build_string (PATH_DUMPLOADSEARCH));
	  tem = Fexpand_file_name (build_string ("GNU"), newdir);
	  tem1 = Ffile_exists_p (tem);
	  if (!NILP (tem1))
	    Vdata_directory = newdir;
	}
    }

#ifndef CANNOT_DUMP
  if (initialized)
#endif
    {
      tempdir = Fdirectory_file_name (Vexec_directory);
      if (access (SSDATA (tempdir), 0) < 0)
	dir_warning ("Warning: arch-dependent data dir (%s) does not exist.\n",
		     Vexec_directory);
    }

  tempdir = Fdirectory_file_name (Vdata_directory);
  if (access (SSDATA (tempdir), 0) < 0)
    dir_warning ("Warning: arch-independent data dir (%s) does not exist.\n",
		 Vdata_directory);

  sh = (char *) getenv ("SHELL");
  Vshell_file_name = build_string (sh ? sh : "/bin/sh");

#ifdef DOS_NT
  Vshared_game_score_directory = Qnil;
#else
  Vshared_game_score_directory = build_string (PATH_GAME);
  if (NILP (Ffile_directory_p (Vshared_game_score_directory)))
    Vshared_game_score_directory = Qnil;
#endif
}

void
set_initial_environment (void)
{
  char **envp;
  for (envp = environ; *envp; envp++)
    Vprocess_environment = Fcons (build_string (*envp),
				  Vprocess_environment);
  /* Ideally, the `copy' shouldn't be necessary, but it seems it's frequent
     to use `delete' and friends on process-environment.  */
  Vinitial_environment = Fcopy_sequence (Vprocess_environment);
}

void
syms_of_callproc (void)
{
#ifndef DOS_NT
  Vtemp_file_name_pattern = build_string ("emacsXXXXXX");
#elif defined (WINDOWSNT)
  Vtemp_file_name_pattern = build_string ("emXXXXXX");
#else
  Vtemp_file_name_pattern = build_string ("detmp.XXX");
#endif
  staticpro (&Vtemp_file_name_pattern);

  DEFVAR_LISP ("shell-file-name", Vshell_file_name,
	       doc: /* *File name to load inferior shells from.
Initialized from the SHELL environment variable, or to a system-dependent
default if SHELL is not set.  */);

  DEFVAR_LISP ("exec-path", Vexec_path,
	       doc: /* *List of directories to search programs to run in subprocesses.
Each element is a string (directory name) or nil (try default directory).  */);

  DEFVAR_LISP ("exec-suffixes", Vexec_suffixes,
	       doc: /* *List of suffixes to try to find executable file names.
Each element is a string.  */);
  Vexec_suffixes = Qnil;

  DEFVAR_LISP ("exec-directory", Vexec_directory,
	       doc: /* Directory for executables for Emacs to invoke.
More generally, this includes any architecture-dependent files
that are built and installed from the Emacs distribution.  */);

  DEFVAR_LISP ("data-directory", Vdata_directory,
	       doc: /* Directory of machine-independent files that come with GNU Emacs.
These are files intended for Emacs to use while it runs.  */);

  DEFVAR_LISP ("doc-directory", Vdoc_directory,
	       doc: /* Directory containing the DOC file that comes with GNU Emacs.
This is usually the same as `data-directory'.  */);

  DEFVAR_LISP ("configure-info-directory", Vconfigure_info_directory,
	       doc: /* For internal use by the build procedure only.
This is the name of the directory in which the build procedure installed
Emacs's info files; the default value for `Info-default-directory-list'
includes this.  */);
  Vconfigure_info_directory = build_string (PATH_INFO);

  DEFVAR_LISP ("shared-game-score-directory", Vshared_game_score_directory,
	       doc: /* Directory of score files for games which come with GNU Emacs.
If this variable is nil, then Emacs is unable to use a shared directory.  */);
#ifdef DOS_NT
  Vshared_game_score_directory = Qnil;
#else
  Vshared_game_score_directory = build_string (PATH_GAME);
#endif

  DEFVAR_LISP ("initial-environment", Vinitial_environment,
	       doc: /* List of environment variables inherited from the parent process.
Each element should be a string of the form ENVVARNAME=VALUE.
The elements must normally be decoded (using `locale-coding-system') for use.  */);
  Vinitial_environment = Qnil;

  DEFVAR_LISP ("process-environment", Vprocess_environment,
	       doc: /* List of overridden environment variables for subprocesses to inherit.
Each element should be a string of the form ENVVARNAME=VALUE.

Entries in this list take precedence to those in the frame-local
environments.  Therefore, let-binding `process-environment' is an easy
way to temporarily change the value of an environment variable,
irrespective of where it comes from.  To use `process-environment' to
remove an environment variable, include only its name in the list,
without "=VALUE".

This variable is set to nil when Emacs starts.

If multiple entries define the same variable, the first one always
takes precedence.

Non-ASCII characters are encoded according to the initial value of
`locale-coding-system', i.e. the elements must normally be decoded for
use.

See `setenv' and `getenv'.  */);
  Vprocess_environment = Qnil;

  defsubr (&Scall_process);
  defsubr (&Sgetenv_internal);
  defsubr (&Scall_process_region);
}
