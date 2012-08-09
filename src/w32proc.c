/* Process support for GNU Emacs on the Microsoft W32 API.
   Copyright (C) 1992, 1995, 1999-2012  Free Software Foundation, Inc.

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

/*
   Drew Bliss                   Oct 14, 1993
     Adapted from alarm.c by Tim Fleehart
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <io.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/file.h>
#include <setjmp.h>

/* must include CRT headers *before* config.h */
#include <config.h>

#undef signal
#undef wait
#undef spawnve
#undef select
#undef kill

#include <windows.h>
#ifdef __GNUC__
/* This definition is missing from mingw32 headers. */
extern BOOL WINAPI IsValidLocale (LCID, DWORD);
#endif

#ifdef HAVE_LANGINFO_CODESET
#include <nl_types.h>
#include <langinfo.h>
#endif

#include "lisp.h"
#include "w32.h"
#include "w32heap.h"
#include "systime.h"
#include "syswait.h"
#include "process.h"
#include "syssignal.h"
#include "w32term.h"
#include "dispextern.h"		/* for xstrcasecmp */
#include "coding.h"

#define RVA_TO_PTR(var,section,filedata) \
  ((void *)((section)->PointerToRawData					\
	    + ((DWORD)(var) - (section)->VirtualAddress)		\
	    + (filedata).file_base))

Lisp_Object Qhigh, Qlow;

#ifdef EMACSDEBUG
void
_DebPrint (const char *fmt, ...)
{
  char buf[1024];
  va_list args;

  va_start (args, fmt);
  vsprintf (buf, fmt, args);
  va_end (args);
  OutputDebugString (buf);
}
#endif

typedef void (_CALLBACK_ *signal_handler) (int);

/* Signal handlers...SIG_DFL == 0 so this is initialized correctly.  */
static signal_handler sig_handlers[NSIG];

/* Fake signal implementation to record the SIGCHLD handler.  */
signal_handler
sys_signal (int sig, signal_handler handler)
{
  signal_handler old;

  if (sig != SIGCHLD)
    {
      errno = EINVAL;
      return SIG_ERR;
    }
  old = sig_handlers[sig];
  sig_handlers[sig] = handler;
  return old;
}

/* Defined in <process.h> which conflicts with the local copy */
#define _P_NOWAIT 1

/* Child process management list.  */
int child_proc_count = 0;
child_process child_procs[ MAX_CHILDREN ];
child_process *dead_child = NULL;

static DWORD WINAPI reader_thread (void *arg);

/* Find an unused process slot.  */
child_process *
new_child (void)
{
  child_process *cp;
  DWORD id;

  for (cp = child_procs + (child_proc_count-1); cp >= child_procs; cp--)
    if (!CHILD_ACTIVE (cp))
      goto Initialize;
  if (child_proc_count == MAX_CHILDREN)
    return NULL;
  cp = &child_procs[child_proc_count++];

 Initialize:
  memset (cp, 0, sizeof (*cp));
  cp->fd = -1;
  cp->pid = -1;
  cp->procinfo.hProcess = NULL;
  cp->status = STATUS_READ_ERROR;

  /* use manual reset event so that select() will function properly */
  cp->char_avail = CreateEvent (NULL, TRUE, FALSE, NULL);
  if (cp->char_avail)
    {
      cp->char_consumed = CreateEvent (NULL, FALSE, FALSE, NULL);
      if (cp->char_consumed)
        {
	  /* The 0x00010000 flag is STACK_SIZE_PARAM_IS_A_RESERVATION.
	     It means that the 64K stack we are requesting in the 2nd
	     argument is how much memory should be reserved for the
	     stack.  If we don't use this flag, the memory requested
	     by the 2nd argument is the amount actually _committed_,
	     but Windows reserves 8MB of memory for each thread's
	     stack.  (The 8MB figure comes from the -stack
	     command-line argument we pass to the linker when building
	     Emacs, but that's because we need a large stack for
	     Emacs's main thread.)  Since we request 2GB of reserved
	     memory at startup (see w32heap.c), which is close to the
	     maximum memory available for a 32-bit process on Windows,
	     the 8MB reservation for each thread causes failures in
	     starting subprocesses, because we create a thread running
	     reader_thread for each subprocess.  As 8MB of stack is
	     way too much for reader_thread, forcing Windows to
	     reserve less wins the day.  */
	  cp->thrd = CreateThread (NULL, 64 * 1024, reader_thread, cp,
				   0x00010000, &id);
	  if (cp->thrd)
	    return cp;
	}
    }
  delete_child (cp);
  return NULL;
}

void
delete_child (child_process *cp)
{
  int i;

  /* Should not be deleting a child that is still needed. */
  for (i = 0; i < MAXDESC; i++)
    if (fd_info[i].cp == cp)
      abort ();

  if (!CHILD_ACTIVE (cp))
    return;

  /* reap thread if necessary */
  if (cp->thrd)
    {
      DWORD rc;

      if (GetExitCodeThread (cp->thrd, &rc) && rc == STILL_ACTIVE)
        {
	  /* let the thread exit cleanly if possible */
	  cp->status = STATUS_READ_ERROR;
	  SetEvent (cp->char_consumed);
#if 0
          /* We used to forcibly terminate the thread here, but it
             is normally unnecessary, and in abnormal cases, the worst that
             will happen is we have an extra idle thread hanging around
             waiting for the zombie process.  */
	  if (WaitForSingleObject (cp->thrd, 1000) != WAIT_OBJECT_0)
	    {
	      DebPrint (("delete_child.WaitForSingleObject (thread) failed "
			 "with %lu for fd %ld\n", GetLastError (), cp->fd));
	      TerminateThread (cp->thrd, 0);
	    }
#endif
	}
      CloseHandle (cp->thrd);
      cp->thrd = NULL;
    }
  if (cp->char_avail)
    {
      CloseHandle (cp->char_avail);
      cp->char_avail = NULL;
    }
  if (cp->char_consumed)
    {
      CloseHandle (cp->char_consumed);
      cp->char_consumed = NULL;
    }

  /* update child_proc_count (highest numbered slot in use plus one) */
  if (cp == child_procs + child_proc_count - 1)
    {
      for (i = child_proc_count-1; i >= 0; i--)
	if (CHILD_ACTIVE (&child_procs[i]))
	  {
	    child_proc_count = i + 1;
	    break;
	  }
    }
  if (i < 0)
    child_proc_count = 0;
}

/* Find a child by pid.  */
static child_process *
find_child_pid (DWORD pid)
{
  child_process *cp;

  for (cp = child_procs + (child_proc_count-1); cp >= child_procs; cp--)
    if (CHILD_ACTIVE (cp) && pid == cp->pid)
      return cp;
  return NULL;
}


/* Thread proc for child process and socket reader threads. Each thread
   is normally blocked until woken by select() to check for input by
   reading one char.  When the read completes, char_avail is signaled
   to wake up the select emulator and the thread blocks itself again. */
static DWORD WINAPI
reader_thread (void *arg)
{
  child_process *cp;

  /* Our identity */
  cp = (child_process *)arg;

  /* We have to wait for the go-ahead before we can start */
  if (cp == NULL
      || WaitForSingleObject (cp->char_consumed, INFINITE) != WAIT_OBJECT_0
      || cp->fd < 0)
    return 1;

  for (;;)
    {
      int rc;

      if (fd_info[cp->fd].flags & FILE_LISTEN)
	rc = _sys_wait_accept (cp->fd);
      else
	rc = _sys_read_ahead (cp->fd);

      /* The name char_avail is a misnomer - it really just means the
	 read-ahead has completed, whether successfully or not. */
      if (!SetEvent (cp->char_avail))
        {
	  DebPrint (("reader_thread.SetEvent failed with %lu for fd %ld\n",
		     GetLastError (), cp->fd));
	  return 1;
	}

      if (rc == STATUS_READ_ERROR)
	return 1;

      /* If the read died, the child has died so let the thread die */
      if (rc == STATUS_READ_FAILED)
	break;

      /* Wait until our input is acknowledged before reading again */
      if (WaitForSingleObject (cp->char_consumed, INFINITE) != WAIT_OBJECT_0)
        {
	  DebPrint (("reader_thread.WaitForSingleObject failed with "
		     "%lu for fd %ld\n", GetLastError (), cp->fd));
	  break;
        }
    }
  return 0;
}

/* To avoid Emacs changing directory, we just record here the directory
   the new process should start in.  This is set just before calling
   sys_spawnve, and is not generally valid at any other time.  */
static char * process_dir;

static BOOL
create_child (char *exe, char *cmdline, char *env, int is_gui_app,
	      int * pPid, child_process *cp)
{
  STARTUPINFO start;
  SECURITY_ATTRIBUTES sec_attrs;
#if 0
  SECURITY_DESCRIPTOR sec_desc;
#endif
  DWORD flags;
  char dir[ MAXPATHLEN ];

  if (cp == NULL) abort ();

  memset (&start, 0, sizeof (start));
  start.cb = sizeof (start);

#ifdef HAVE_NTGUI
  if (NILP (Vw32_start_process_show_window) && !is_gui_app)
    start.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  else
    start.dwFlags = STARTF_USESTDHANDLES;
  start.wShowWindow = SW_HIDE;

  start.hStdInput = GetStdHandle (STD_INPUT_HANDLE);
  start.hStdOutput = GetStdHandle (STD_OUTPUT_HANDLE);
  start.hStdError = GetStdHandle (STD_ERROR_HANDLE);
#endif /* HAVE_NTGUI */

#if 0
  /* Explicitly specify no security */
  if (!InitializeSecurityDescriptor (&sec_desc, SECURITY_DESCRIPTOR_REVISION))
    goto EH_Fail;
  if (!SetSecurityDescriptorDacl (&sec_desc, TRUE, NULL, FALSE))
    goto EH_Fail;
#endif
  sec_attrs.nLength = sizeof (sec_attrs);
  sec_attrs.lpSecurityDescriptor = NULL /* &sec_desc */;
  sec_attrs.bInheritHandle = FALSE;

  strcpy (dir, process_dir);
  unixtodos_filename (dir);

  flags = (!NILP (Vw32_start_process_share_console)
	   ? CREATE_NEW_PROCESS_GROUP
	   : CREATE_NEW_CONSOLE);
  if (NILP (Vw32_start_process_inherit_error_mode))
    flags |= CREATE_DEFAULT_ERROR_MODE;
  if (!CreateProcess (exe, cmdline, &sec_attrs, NULL, TRUE,
		      flags, env, dir, &start, &cp->procinfo))
    goto EH_Fail;

  cp->pid = (int) cp->procinfo.dwProcessId;

  /* Hack for Windows 95, which assigns large (ie negative) pids */
  if (cp->pid < 0)
    cp->pid = -cp->pid;

  /* pid must fit in a Lisp_Int */
  cp->pid = cp->pid & INTMASK;

  *pPid = cp->pid;

  return TRUE;

 EH_Fail:
  DebPrint (("create_child.CreateProcess failed: %ld\n", GetLastError ()););
  return FALSE;
}

/* create_child doesn't know what emacs' file handle will be for waiting
   on output from the child, so we need to make this additional call
   to register the handle with the process
   This way the select emulator knows how to match file handles with
   entries in child_procs.  */
void
register_child (int pid, int fd)
{
  child_process *cp;

  cp = find_child_pid (pid);
  if (cp == NULL)
    {
      DebPrint (("register_child unable to find pid %lu\n", pid));
      return;
    }

#ifdef FULL_DEBUG
  DebPrint (("register_child registered fd %d with pid %lu\n", fd, pid));
#endif

  cp->fd = fd;

  /* thread is initially blocked until select is called; set status so
     that select will release thread */
  cp->status = STATUS_READ_ACKNOWLEDGED;

  /* attach child_process to fd_info */
  if (fd_info[fd].cp != NULL)
    {
      DebPrint (("register_child: fd_info[%d] apparently in use!\n", fd));
      abort ();
    }

  fd_info[fd].cp = cp;
}

/* When a process dies its pipe will break so the reader thread will
   signal failure to the select emulator.
   The select emulator then calls this routine to clean up.
   Since the thread signaled failure we can assume it is exiting.  */
static void
reap_subprocess (child_process *cp)
{
  if (cp->procinfo.hProcess)
    {
      /* Reap the process */
#ifdef FULL_DEBUG
      /* Process should have already died before we are called.  */
      if (WaitForSingleObject (cp->procinfo.hProcess, 0) != WAIT_OBJECT_0)
	DebPrint (("reap_subprocess: child fpr fd %d has not died yet!", cp->fd));
#endif
      CloseHandle (cp->procinfo.hProcess);
      cp->procinfo.hProcess = NULL;
      CloseHandle (cp->procinfo.hThread);
      cp->procinfo.hThread = NULL;
    }

  /* For asynchronous children, the child_proc resources will be freed
     when the last pipe read descriptor is closed; for synchronous
     children, we must explicitly free the resources now because
     register_child has not been called. */
  if (cp->fd == -1)
    delete_child (cp);
}

/* Wait for any of our existing child processes to die
   When it does, close its handle
   Return the pid and fill in the status if non-NULL.  */

int
sys_wait (int *status)
{
  DWORD active, retval;
  int nh;
  int pid;
  child_process *cp, *cps[MAX_CHILDREN];
  HANDLE wait_hnd[MAX_CHILDREN];

  nh = 0;
  if (dead_child != NULL)
    {
      /* We want to wait for a specific child */
      wait_hnd[nh] = dead_child->procinfo.hProcess;
      cps[nh] = dead_child;
      if (!wait_hnd[nh]) abort ();
      nh++;
      active = 0;
      goto get_result;
    }
  else
    {
      for (cp = child_procs + (child_proc_count-1); cp >= child_procs; cp--)
	/* some child_procs might be sockets; ignore them */
	if (CHILD_ACTIVE (cp) && cp->procinfo.hProcess
	    && (cp->fd < 0 || (fd_info[cp->fd].flags & FILE_AT_EOF) != 0))
	  {
	    wait_hnd[nh] = cp->procinfo.hProcess;
	    cps[nh] = cp;
	    nh++;
	  }
    }

  if (nh == 0)
    {
      /* Nothing to wait on, so fail */
      errno = ECHILD;
      return -1;
    }

  do
    {
      /* Check for quit about once a second. */
      QUIT;
      active = WaitForMultipleObjects (nh, wait_hnd, FALSE, 1000);
    } while (active == WAIT_TIMEOUT);

  if (active == WAIT_FAILED)
    {
      errno = EBADF;
      return -1;
    }
  else if (active >= WAIT_OBJECT_0
	   && active < WAIT_OBJECT_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_OBJECT_0;
    }
  else if (active >= WAIT_ABANDONED_0
	   && active < WAIT_ABANDONED_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_ABANDONED_0;
    }
  else
    abort ();

get_result:
  if (!GetExitCodeProcess (wait_hnd[active], &retval))
    {
      DebPrint (("Wait.GetExitCodeProcess failed with %lu\n",
		 GetLastError ()));
      retval = 1;
    }
  if (retval == STILL_ACTIVE)
    {
      /* Should never happen */
      DebPrint (("Wait.WaitForMultipleObjects returned an active process\n"));
      errno = EINVAL;
      return -1;
    }

  /* Massage the exit code from the process to match the format expected
     by the WIFSTOPPED et al macros in syswait.h.  Only WIFSIGNALED and
     WIFEXITED are supported; WIFSTOPPED doesn't make sense under NT.  */

  if (retval == STATUS_CONTROL_C_EXIT)
    retval = SIGINT;
  else
    retval <<= 8;

  cp = cps[active];
  pid = cp->pid;
#ifdef FULL_DEBUG
  DebPrint (("Wait signaled with process pid %d\n", cp->pid));
#endif

  if (status)
    {
      *status = retval;
    }
  else if (synch_process_alive)
    {
      synch_process_alive = 0;

      /* Report the status of the synchronous process.  */
      if (WIFEXITED (retval))
	synch_process_retcode = WRETCODE (retval);
      else if (WIFSIGNALED (retval))
	{
	  int code = WTERMSIG (retval);
	  char *signame;

	  synchronize_system_messages_locale ();
	  signame = strsignal (code);

	  if (signame == 0)
	    signame = "unknown";

	  synch_process_death = signame;
	}

      reap_subprocess (cp);
    }

  reap_subprocess (cp);

  return pid;
}

/* Old versions of w32api headers don't have separate 32-bit and
   64-bit defines, but the one they have matches the 32-bit variety.  */
#ifndef IMAGE_NT_OPTIONAL_HDR32_MAGIC
# define IMAGE_NT_OPTIONAL_HDR32_MAGIC IMAGE_NT_OPTIONAL_HDR_MAGIC
# define IMAGE_OPTIONAL_HEADER32 IMAGE_OPTIONAL_HEADER
#endif

static void
w32_executable_type (char * filename,
		     int * is_dos_app,
		     int * is_cygnus_app,
		     int * is_gui_app)
{
  file_data executable;
  char * p;

  /* Default values in case we can't tell for sure.  */
  *is_dos_app = FALSE;
  *is_cygnus_app = FALSE;
  *is_gui_app = FALSE;

  if (!open_input_file (&executable, filename))
    return;

  p = strrchr (filename, '.');

  /* We can only identify DOS .com programs from the extension. */
  if (p && xstrcasecmp (p, ".com") == 0)
    *is_dos_app = TRUE;
  else if (p && (xstrcasecmp (p, ".bat") == 0
		 || xstrcasecmp (p, ".cmd") == 0))
    {
      /* A DOS shell script - it appears that CreateProcess is happy to
	 accept this (somewhat surprisingly); presumably it looks at
	 COMSPEC to determine what executable to actually invoke.
	 Therefore, we have to do the same here as well. */
      /* Actually, I think it uses the program association for that
	 extension, which is defined in the registry.  */
      p = egetenv ("COMSPEC");
      if (p)
	w32_executable_type (p, is_dos_app, is_cygnus_app, is_gui_app);
    }
  else
    {
      /* Look for DOS .exe signature - if found, we must also check that
	 it isn't really a 16- or 32-bit Windows exe, since both formats
	 start with a DOS program stub.  Note that 16-bit Windows
	 executables use the OS/2 1.x format. */

      IMAGE_DOS_HEADER * dos_header;
      IMAGE_NT_HEADERS * nt_header;

      dos_header = (PIMAGE_DOS_HEADER) executable.file_base;
      if (dos_header->e_magic != IMAGE_DOS_SIGNATURE)
	goto unwind;

      nt_header = (PIMAGE_NT_HEADERS) ((char *) dos_header + dos_header->e_lfanew);

      if ((char *) nt_header > (char *) dos_header + executable.size)
	{
	  /* Some dos headers (pkunzip) have bogus e_lfanew fields.  */
	  *is_dos_app = TRUE;
	}
      else if (nt_header->Signature != IMAGE_NT_SIGNATURE
	       && LOWORD (nt_header->Signature) != IMAGE_OS2_SIGNATURE)
  	{
	  *is_dos_app = TRUE;
  	}
      else if (nt_header->Signature == IMAGE_NT_SIGNATURE)
  	{
          IMAGE_DATA_DIRECTORY *data_dir = NULL;
          if (nt_header->OptionalHeader.Magic == IMAGE_NT_OPTIONAL_HDR32_MAGIC)
            {
              /* Ensure we are using the 32 bit structure.  */
              IMAGE_OPTIONAL_HEADER32 *opt
                = (IMAGE_OPTIONAL_HEADER32*) &(nt_header->OptionalHeader);
              data_dir = opt->DataDirectory;
              *is_gui_app = (opt->Subsystem == IMAGE_SUBSYSTEM_WINDOWS_GUI);
            }
          /* MingW 3.12 has the required 64 bit structs, but in case older
             versions don't, only check 64 bit exes if we know how.  */
#ifdef IMAGE_NT_OPTIONAL_HDR64_MAGIC
          else if (nt_header->OptionalHeader.Magic
                   == IMAGE_NT_OPTIONAL_HDR64_MAGIC)
            {
              IMAGE_OPTIONAL_HEADER64 *opt
                = (IMAGE_OPTIONAL_HEADER64*) &(nt_header->OptionalHeader);
              data_dir = opt->DataDirectory;
              *is_gui_app = (opt->Subsystem == IMAGE_SUBSYSTEM_WINDOWS_GUI);
            }
#endif
          if (data_dir)
            {
              /* Look for cygwin.dll in DLL import list. */
              IMAGE_DATA_DIRECTORY import_dir =
                data_dir[IMAGE_DIRECTORY_ENTRY_IMPORT];
              IMAGE_IMPORT_DESCRIPTOR * imports;
              IMAGE_SECTION_HEADER * section;

              section = rva_to_section (import_dir.VirtualAddress, nt_header);
              imports = RVA_TO_PTR (import_dir.VirtualAddress, section,
                                    executable);

              for ( ; imports->Name; imports++)
                {
                  char * dllname = RVA_TO_PTR (imports->Name, section,
                                               executable);

                  /* The exact name of the cygwin dll has changed with
                     various releases, but hopefully this will be reasonably
                     future proof.  */
                  if (strncmp (dllname, "cygwin", 6) == 0)
                    {
                      *is_cygnus_app = TRUE;
                      break;
                    }
                }
            }
  	}
    }

unwind:
  close_file_data (&executable);
}

static int
compare_env (const void *strp1, const void *strp2)
{
  const char *str1 = *(const char **)strp1, *str2 = *(const char **)strp2;

  while (*str1 && *str2 && *str1 != '=' && *str2 != '=')
    {
      /* Sort order in command.com/cmd.exe is based on uppercasing
         names, so do the same here.  */
      if (toupper (*str1) > toupper (*str2))
	return 1;
      else if (toupper (*str1) < toupper (*str2))
	return -1;
      str1++, str2++;
    }

  if (*str1 == '=' && *str2 == '=')
    return 0;
  else if (*str1 == '=')
    return -1;
  else
    return 1;
}

static void
merge_and_sort_env (char **envp1, char **envp2, char **new_envp)
{
  char **optr, **nptr;
  int num;

  nptr = new_envp;
  optr = envp1;
  while (*optr)
    *nptr++ = *optr++;
  num = optr - envp1;

  optr = envp2;
  while (*optr)
    *nptr++ = *optr++;
  num += optr - envp2;

  qsort (new_envp, num, sizeof (char *), compare_env);

  *nptr = NULL;
}

/* When a new child process is created we need to register it in our list,
   so intercept spawn requests.  */
int
sys_spawnve (int mode, char *cmdname, char **argv, char **envp)
{
  Lisp_Object program, full;
  char *cmdline, *env, *parg, **targ;
  int arglen, numenv;
  int pid;
  child_process *cp;
  int is_dos_app, is_cygnus_app, is_gui_app;
  int do_quoting = 0;
  char escape_char;
  /* We pass our process ID to our children by setting up an environment
     variable in their environment.  */
  char ppid_env_var_buffer[64];
  char *extra_env[] = {ppid_env_var_buffer, NULL};
  /* These are the characters that cause an argument to need quoting.
     Arguments with whitespace characters need quoting to prevent the
     argument being split into two or more. Arguments with wildcards
     are also quoted, for consistency with posix platforms, where wildcards
     are not expanded if we run the program directly without a shell.
     Some extra whitespace characters need quoting in Cygwin programs,
     so this list is conditionally modified below.  */
  char *sepchars = " \t*?";

  /* We don't care about the other modes */
  if (mode != _P_NOWAIT)
    {
      errno = EINVAL;
      return -1;
    }

  /* Handle executable names without an executable suffix.  */
  program = make_string (cmdname, strlen (cmdname));
  if (NILP (Ffile_executable_p (program)))
    {
      struct gcpro gcpro1;

      full = Qnil;
      GCPRO1 (program);
      openp (Vexec_path, program, Vexec_suffixes, &full, make_number (X_OK));
      UNGCPRO;
      if (NILP (full))
	{
	  errno = EINVAL;
	  return -1;
	}
      program = full;
    }

  /* make sure argv[0] and cmdname are both in DOS format */
  cmdname = SDATA (program);
  unixtodos_filename (cmdname);
  argv[0] = cmdname;

  /* Determine whether program is a 16-bit DOS executable, or a w32
     executable that is implicitly linked to the Cygnus dll (implying it
     was compiled with the Cygnus GNU toolchain and hence relies on
     cygwin.dll to parse the command line - we use this to decide how to
     escape quote chars in command line args that must be quoted).

     Also determine whether it is a GUI app, so that we don't hide its
     initial window unless specifically requested.  */
  w32_executable_type (cmdname, &is_dos_app, &is_cygnus_app, &is_gui_app);

  /* On Windows 95, if cmdname is a DOS app, we invoke a helper
     application to start it by specifying the helper app as cmdname,
     while leaving the real app name as argv[0].  */
  if (is_dos_app)
    {
      cmdname = alloca (MAXPATHLEN);
      if (egetenv ("CMDPROXY"))
	strcpy (cmdname, egetenv ("CMDPROXY"));
      else
	{
	  strcpy (cmdname, SDATA (Vinvocation_directory));
	  strcat (cmdname, "cmdproxy.exe");
	}
      unixtodos_filename (cmdname);
    }

  /* we have to do some conjuring here to put argv and envp into the
     form CreateProcess wants...  argv needs to be a space separated/null
     terminated list of parameters, and envp is a null
     separated/double-null terminated list of parameters.

     Additionally, zero-length args and args containing whitespace or
     quote chars need to be wrapped in double quotes - for this to work,
     embedded quotes need to be escaped as well.  The aim is to ensure
     the child process reconstructs the argv array we start with
     exactly, so we treat quotes at the beginning and end of arguments
     as embedded quotes.

     The w32 GNU-based library from Cygnus doubles quotes to escape
     them, while MSVC uses backslash for escaping.  (Actually the MSVC
     startup code does attempt to recognize doubled quotes and accept
     them, but gets it wrong and ends up requiring three quotes to get a
     single embedded quote!)  So by default we decide whether to use
     quote or backslash as the escape character based on whether the
     binary is apparently a Cygnus compiled app.

     Note that using backslash to escape embedded quotes requires
     additional special handling if an embedded quote is already
     preceded by backslash, or if an arg requiring quoting ends with
     backslash.  In such cases, the run of escape characters needs to be
     doubled.  For consistency, we apply this special handling as long
     as the escape character is not quote.

     Since we have no idea how large argv and envp are likely to be we
     figure out list lengths on the fly and allocate them.  */

  if (!NILP (Vw32_quote_process_args))
    {
      do_quoting = 1;
      /* Override escape char by binding w32-quote-process-args to
	 desired character, or use t for auto-selection.  */
      if (INTEGERP (Vw32_quote_process_args))
	escape_char = XINT (Vw32_quote_process_args);
      else
	escape_char = is_cygnus_app ? '"' : '\\';
    }

  /* Cygwin apps needs quoting a bit more often.  */
  if (escape_char == '"')
    sepchars = "\r\n\t\f '";

  /* do argv...  */
  arglen = 0;
  targ = argv;
  while (*targ)
    {
      char * p = *targ;
      int need_quotes = 0;
      int escape_char_run = 0;

      if (*p == 0)
	need_quotes = 1;
      for ( ; *p; p++)
	{
	  if (escape_char == '"' && *p == '\\')
	    /* If it's a Cygwin app, \ needs to be escaped.  */
	    arglen++;
	  else if (*p == '"')
	    {
	      /* allow for embedded quotes to be escaped */
	      arglen++;
	      need_quotes = 1;
	      /* handle the case where the embedded quote is already escaped */
	      if (escape_char_run > 0)
		{
		  /* To preserve the arg exactly, we need to double the
		     preceding escape characters (plus adding one to
		     escape the quote character itself).  */
		  arglen += escape_char_run;
		}
	    }
	  else if (strchr (sepchars, *p) != NULL)
	    {
	      need_quotes = 1;
	    }

	  if (*p == escape_char && escape_char != '"')
	    escape_char_run++;
	  else
	    escape_char_run = 0;
	}
      if (need_quotes)
	{
	  arglen += 2;
	  /* handle the case where the arg ends with an escape char - we
	     must not let the enclosing quote be escaped.  */
	  if (escape_char_run > 0)
	    arglen += escape_char_run;
	}
      arglen += strlen (*targ++) + 1;
    }
  cmdline = alloca (arglen);
  targ = argv;
  parg = cmdline;
  while (*targ)
    {
      char * p = *targ;
      int need_quotes = 0;

      if (*p == 0)
	need_quotes = 1;

      if (do_quoting)
	{
	  for ( ; *p; p++)
	    if ((strchr (sepchars, *p) != NULL) || *p == '"')
	      need_quotes = 1;
	}
      if (need_quotes)
	{
	  int escape_char_run = 0;
	  char * first;
	  char * last;

	  p = *targ;
	  first = p;
	  last = p + strlen (p) - 1;
	  *parg++ = '"';
#if 0
	  /* This version does not escape quotes if they occur at the
	     beginning or end of the arg - this could lead to incorrect
	     behavior when the arg itself represents a command line
	     containing quoted args.  I believe this was originally done
	     as a hack to make some things work, before
	     `w32-quote-process-args' was added.  */
	  while (*p)
	    {
	      if (*p == '"' && p > first && p < last)
		*parg++ = escape_char;	/* escape embedded quotes */
	      *parg++ = *p++;
	    }
#else
	  for ( ; *p; p++)
	    {
	      if (*p == '"')
		{
		  /* double preceding escape chars if any */
		  while (escape_char_run > 0)
		    {
		      *parg++ = escape_char;
		      escape_char_run--;
		    }
		  /* escape all quote chars, even at beginning or end */
		  *parg++ = escape_char;
		}
	      else if (escape_char == '"' && *p == '\\')
		*parg++ = '\\';
	      *parg++ = *p;

	      if (*p == escape_char && escape_char != '"')
		escape_char_run++;
	      else
		escape_char_run = 0;
	    }
	  /* double escape chars before enclosing quote */
	  while (escape_char_run > 0)
	    {
	      *parg++ = escape_char;
	      escape_char_run--;
	    }
#endif
	  *parg++ = '"';
	}
      else
	{
	  strcpy (parg, *targ);
	  parg += strlen (*targ);
	}
      *parg++ = ' ';
      targ++;
    }
  *--parg = '\0';

  /* and envp...  */
  arglen = 1;
  targ = envp;
  numenv = 1; /* for end null */
  while (*targ)
    {
      arglen += strlen (*targ++) + 1;
      numenv++;
    }
  /* extra env vars... */
  sprintf (ppid_env_var_buffer, "EM_PARENT_PROCESS_ID=%d",
	   GetCurrentProcessId ());
  arglen += strlen (ppid_env_var_buffer) + 1;
  numenv++;

  /* merge env passed in and extra env into one, and sort it.  */
  targ = (char **) alloca (numenv * sizeof (char *));
  merge_and_sort_env (envp, extra_env, targ);

  /* concatenate env entries.  */
  env = alloca (arglen);
  parg = env;
  while (*targ)
    {
      strcpy (parg, *targ);
      parg += strlen (*targ++);
      *parg++ = '\0';
    }
  *parg++ = '\0';
  *parg = '\0';

  cp = new_child ();
  if (cp == NULL)
    {
      errno = EAGAIN;
      return -1;
    }

  /* Now create the process.  */
  if (!create_child (cmdname, cmdline, env, is_gui_app, &pid, cp))
    {
      delete_child (cp);
      errno = ENOEXEC;
      return -1;
    }

  return pid;
}

/* Emulate the select call
   Wait for available input on any of the given rfds, or timeout if
   a timeout is given and no input is detected
   wfds and efds are not supported and must be NULL.

   For simplicity, we detect the death of child processes here and
   synchronously call the SIGCHLD handler.  Since it is possible for
   children to be created without a corresponding pipe handle from which
   to read output, we wait separately on the process handles as well as
   the char_avail events for each process pipe.  We only call
   wait/reap_process when the process actually terminates.

   To reduce the number of places in which Emacs can be hung such that
   C-g is not able to interrupt it, we always wait on interrupt_handle
   (which is signaled by the input thread when C-g is detected).  If we
   detect that we were woken up by C-g, we return -1 with errno set to
   EINTR as on Unix.  */

/* From w32console.c */
extern HANDLE keyboard_handle;

/* From w32xfns.c */
extern HANDLE interrupt_handle;

/* From process.c */
extern int proc_buffered_char[];

int
sys_select (int nfds, SELECT_TYPE *rfds, SELECT_TYPE *wfds, SELECT_TYPE *efds,
	    EMACS_TIME *timeout)
{
  SELECT_TYPE orfds;
  DWORD timeout_ms, start_time;
  int i, nh, nc, nr;
  DWORD active;
  child_process *cp, *cps[MAX_CHILDREN];
  HANDLE wait_hnd[MAXDESC + MAX_CHILDREN];
  int fdindex[MAXDESC];   /* mapping from wait handles back to descriptors */

  timeout_ms = timeout ? (timeout->tv_sec * 1000 + timeout->tv_usec / 1000) : INFINITE;

  /* If the descriptor sets are NULL but timeout isn't, then just Sleep.  */
  if (rfds == NULL && wfds == NULL && efds == NULL && timeout != NULL)
    {
      Sleep (timeout_ms);
      return 0;
    }

  /* Otherwise, we only handle rfds, so fail otherwise.  */
  if (rfds == NULL || wfds != NULL || efds != NULL)
    {
      errno = EINVAL;
      return -1;
    }

  orfds = *rfds;
  FD_ZERO (rfds);
  nr = 0;

  /* Always wait on interrupt_handle, to detect C-g (quit).  */
  wait_hnd[0] = interrupt_handle;
  fdindex[0] = -1;

  /* Build a list of pipe handles to wait on.  */
  nh = 1;
  for (i = 0; i < nfds; i++)
    if (FD_ISSET (i, &orfds))
      {
	if (i == 0)
	  {
	    if (keyboard_handle)
	      {
		/* Handle stdin specially */
		wait_hnd[nh] = keyboard_handle;
		fdindex[nh] = i;
		nh++;
	      }

	    /* Check for any emacs-generated input in the queue since
	       it won't be detected in the wait */
	    if (detect_input_pending ())
	      {
		FD_SET (i, rfds);
		return 1;
	      }
	  }
	else
	  {
	    /* Child process and socket input */
	    cp = fd_info[i].cp;
	    if (cp)
	      {
		int current_status = cp->status;

		if (current_status == STATUS_READ_ACKNOWLEDGED)
		  {
		    /* Tell reader thread which file handle to use. */
		    cp->fd = i;
		    /* Wake up the reader thread for this process */
		    cp->status = STATUS_READ_READY;
		    if (!SetEvent (cp->char_consumed))
		      DebPrint (("nt_select.SetEvent failed with "
				 "%lu for fd %ld\n", GetLastError (), i));
		  }

#ifdef CHECK_INTERLOCK
		/* slightly crude cross-checking of interlock between threads */

		current_status = cp->status;
		if (WaitForSingleObject (cp->char_avail, 0) == WAIT_OBJECT_0)
		  {
		    /* char_avail has been signaled, so status (which may
		       have changed) should indicate read has completed
		       but has not been acknowledged. */
		    current_status = cp->status;
		    if (current_status != STATUS_READ_SUCCEEDED
			&& current_status != STATUS_READ_FAILED)
		      DebPrint (("char_avail set, but read not completed: status %d\n",
				 current_status));
		  }
		else
		  {
		    /* char_avail has not been signaled, so status should
		       indicate that read is in progress; small possibility
		       that read has completed but event wasn't yet signaled
		       when we tested it (because a context switch occurred
		       or if running on separate CPUs). */
		    if (current_status != STATUS_READ_READY
			&& current_status != STATUS_READ_IN_PROGRESS
			&& current_status != STATUS_READ_SUCCEEDED
			&& current_status != STATUS_READ_FAILED)
		      DebPrint (("char_avail reset, but read status is bad: %d\n",
				 current_status));
		  }
#endif
		wait_hnd[nh] = cp->char_avail;
		fdindex[nh] = i;
		if (!wait_hnd[nh]) abort ();
		nh++;
#ifdef FULL_DEBUG
		DebPrint (("select waiting on child %d fd %d\n",
			   cp-child_procs, i));
#endif
	      }
	    else
	      {
		/* Unable to find something to wait on for this fd, skip */

		/* Note that this is not a fatal error, and can in fact
		   happen in unusual circumstances.  Specifically, if
		   sys_spawnve fails, eg. because the program doesn't
		   exist, and debug-on-error is t so Fsignal invokes a
		   nested input loop, then the process output pipe is
		   still included in input_wait_mask with no child_proc
		   associated with it.  (It is removed when the debugger
		   exits the nested input loop and the error is thrown.)  */

		DebPrint (("sys_select: fd %ld is invalid! ignoring\n", i));
	      }
	  }
      }

count_children:
  /* Add handles of child processes.  */
  nc = 0;
  for (cp = child_procs + (child_proc_count-1); cp >= child_procs; cp--)
    /* Some child_procs might be sockets; ignore them.  Also some
       children may have died already, but we haven't finished reading
       the process output; ignore them too.  */
    if (CHILD_ACTIVE (cp) && cp->procinfo.hProcess
	&& (cp->fd < 0
	    || (fd_info[cp->fd].flags & FILE_SEND_SIGCHLD) == 0
	    || (fd_info[cp->fd].flags & FILE_AT_EOF) != 0)
	)
      {
	wait_hnd[nh + nc] = cp->procinfo.hProcess;
	cps[nc] = cp;
	nc++;
      }

  /* Nothing to look for, so we didn't find anything */
  if (nh + nc == 0)
    {
      if (timeout)
	Sleep (timeout_ms);
      return 0;
    }

  start_time = GetTickCount ();

  /* Wait for input or child death to be signaled.  If user input is
     allowed, then also accept window messages.  */
  if (FD_ISSET (0, &orfds))
    active = MsgWaitForMultipleObjects (nh + nc, wait_hnd, FALSE, timeout_ms,
					QS_ALLINPUT);
  else
    active = WaitForMultipleObjects (nh + nc, wait_hnd, FALSE, timeout_ms);

  if (active == WAIT_FAILED)
    {
      DebPrint (("select.WaitForMultipleObjects (%d, %lu) failed with %lu\n",
		 nh + nc, timeout_ms, GetLastError ()));
      /* don't return EBADF - this causes wait_reading_process_output to
	 abort; WAIT_FAILED is returned when single-stepping under
	 Windows 95 after switching thread focus in debugger, and
	 possibly at other times. */
      errno = EINTR;
      return -1;
    }
  else if (active == WAIT_TIMEOUT)
    {
      return 0;
    }
  else if (active >= WAIT_OBJECT_0
	   && active < WAIT_OBJECT_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_OBJECT_0;
    }
  else if (active >= WAIT_ABANDONED_0
	   && active < WAIT_ABANDONED_0+MAXIMUM_WAIT_OBJECTS)
    {
      active -= WAIT_ABANDONED_0;
    }
  else
    abort ();

  /* Loop over all handles after active (now officially documented as
     being the first signaled handle in the array).  We do this to
     ensure fairness, so that all channels with data available will be
     processed - otherwise higher numbered channels could be starved. */
  do
    {
      if (active == nh + nc)
	{
	  /* There are messages in the lisp thread's queue; we must
             drain the queue now to ensure they are processed promptly,
             because if we don't do so, we will not be woken again until
             further messages arrive.

	     NB. If ever we allow window message procedures to callback
	     into lisp, we will need to ensure messages are dispatched
	     at a safe time for lisp code to be run (*), and we may also
	     want to provide some hooks in the dispatch loop to cater
	     for modeless dialogs created by lisp (ie. to register
	     window handles to pass to IsDialogMessage).

	     (*) Note that MsgWaitForMultipleObjects above is an
	     internal dispatch point for messages that are sent to
	     windows created by this thread.  */
	  drain_message_queue ();
	}
      else if (active >= nh)
	{
	  cp = cps[active - nh];

	  /* We cannot always signal SIGCHLD immediately; if we have not
	     finished reading the process output, we must delay sending
	     SIGCHLD until we do.  */

	  if (cp->fd >= 0 && (fd_info[cp->fd].flags & FILE_AT_EOF) == 0)
	    fd_info[cp->fd].flags |= FILE_SEND_SIGCHLD;
	  /* SIG_DFL for SIGCHLD is ignore */
	  else if (sig_handlers[SIGCHLD] != SIG_DFL &&
		   sig_handlers[SIGCHLD] != SIG_IGN)
	    {
#ifdef FULL_DEBUG
	      DebPrint (("select calling SIGCHLD handler for pid %d\n",
			 cp->pid));
#endif
	      dead_child = cp;
	      sig_handlers[SIGCHLD] (SIGCHLD);
	      dead_child = NULL;
	    }
	}
      else if (fdindex[active] == -1)
	{
	  /* Quit (C-g) was detected.  */
	  errno = EINTR;
	  return -1;
	}
      else if (fdindex[active] == 0)
	{
	  /* Keyboard input available */
	  FD_SET (0, rfds);
	  nr++;
	}
      else
	{
	  /* must be a socket or pipe - read ahead should have
             completed, either succeeding or failing.  */
	  FD_SET (fdindex[active], rfds);
	  nr++;
	}

      /* Even though wait_reading_process_output only reads from at most
	 one channel, we must process all channels here so that we reap
	 all children that have died.  */
      while (++active < nh + nc)
	if (WaitForSingleObject (wait_hnd[active], 0) == WAIT_OBJECT_0)
	  break;
    } while (active < nh + nc);

  /* If no input has arrived and timeout hasn't expired, wait again.  */
  if (nr == 0)
    {
      DWORD elapsed = GetTickCount () - start_time;

      if (timeout_ms > elapsed)	/* INFINITE is MAX_UINT */
	{
	  if (timeout_ms != INFINITE)
	    timeout_ms -= elapsed;
	  goto count_children;
	}
    }

  return nr;
}

/* Substitute for certain kill () operations */

static BOOL CALLBACK
find_child_console (HWND hwnd, LPARAM arg)
{
  child_process * cp = (child_process *) arg;
  DWORD thread_id;
  DWORD process_id;

  thread_id = GetWindowThreadProcessId (hwnd, &process_id);
  if (process_id == cp->procinfo.dwProcessId)
    {
      char window_class[32];

      GetClassName (hwnd, window_class, sizeof (window_class));
      if (strcmp (window_class,
		  (os_subtype == OS_WIN95)
		  ? "tty"
		  : "ConsoleWindowClass") == 0)
	{
	  cp->hwnd = hwnd;
	  return FALSE;
	}
    }
  /* keep looking */
  return TRUE;
}

int
sys_kill (int pid, int sig)
{
  child_process *cp;
  HANDLE proc_hand;
  int need_to_free = 0;
  int rc = 0;

  /* Only handle signals that will result in the process dying */
  if (sig != SIGINT && sig != SIGKILL && sig != SIGQUIT && sig != SIGHUP)
    {
      errno = EINVAL;
      return -1;
    }

  cp = find_child_pid (pid);
  if (cp == NULL)
    {
      proc_hand = OpenProcess (PROCESS_TERMINATE, 0, pid);
      if (proc_hand == NULL)
        {
	  errno = EPERM;
	  return -1;
	}
      need_to_free = 1;
    }
  else
    {
      proc_hand = cp->procinfo.hProcess;
      pid = cp->procinfo.dwProcessId;

      /* Try to locate console window for process. */
      EnumWindows (find_child_console, (LPARAM) cp);
    }

  if (sig == SIGINT || sig == SIGQUIT)
    {
      if (NILP (Vw32_start_process_share_console) && cp && cp->hwnd)
	{
	  BYTE control_scan_code = (BYTE) MapVirtualKey (VK_CONTROL, 0);
	  /* Fake Ctrl-C for SIGINT, and Ctrl-Break for SIGQUIT.  */
	  BYTE vk_break_code = (sig == SIGINT) ? 'C' : VK_CANCEL;
	  BYTE break_scan_code = (BYTE) MapVirtualKey (vk_break_code, 0);
	  HWND foreground_window;

	  if (break_scan_code == 0)
	    {
	      /* Fake Ctrl-C for SIGQUIT if we can't manage Ctrl-Break. */
	      vk_break_code = 'C';
	      break_scan_code = (BYTE) MapVirtualKey (vk_break_code, 0);
	    }

	  foreground_window = GetForegroundWindow ();
	  if (foreground_window)
	    {
              /* NT 5.0, and apparently also Windows 98, will not allow
		 a Window to be set to foreground directly without the
		 user's involvement. The workaround is to attach
		 ourselves to the thread that owns the foreground
		 window, since that is the only thread that can set the
		 foreground window.  */
              DWORD foreground_thread, child_thread;
              foreground_thread =
		GetWindowThreadProcessId (foreground_window, NULL);
	      if (foreground_thread == GetCurrentThreadId ()
                  || !AttachThreadInput (GetCurrentThreadId (),
                                         foreground_thread, TRUE))
                foreground_thread = 0;

              child_thread = GetWindowThreadProcessId (cp->hwnd, NULL);
	      if (child_thread == GetCurrentThreadId ()
                  || !AttachThreadInput (GetCurrentThreadId (),
                                         child_thread, TRUE))
                child_thread = 0;

              /* Set the foreground window to the child.  */
              if (SetForegroundWindow (cp->hwnd))
                {
                  /* Generate keystrokes as if user had typed Ctrl-Break or
                     Ctrl-C.  */
                  keybd_event (VK_CONTROL, control_scan_code, 0, 0);
                  keybd_event (vk_break_code, break_scan_code,
		    (vk_break_code == 'C' ? 0 : KEYEVENTF_EXTENDEDKEY), 0);
                  keybd_event (vk_break_code, break_scan_code,
                    (vk_break_code == 'C' ? 0 : KEYEVENTF_EXTENDEDKEY)
                    | KEYEVENTF_KEYUP, 0);
                  keybd_event (VK_CONTROL, control_scan_code,
                               KEYEVENTF_KEYUP, 0);

                  /* Sleep for a bit to give time for Emacs frame to respond
                     to focus change events (if Emacs was active app).  */
                  Sleep (100);

                  SetForegroundWindow (foreground_window);
                }
              /* Detach from the foreground and child threads now that
                 the foreground switching is over.  */
              if (foreground_thread)
                AttachThreadInput (GetCurrentThreadId (),
                                   foreground_thread, FALSE);
              if (child_thread)
                AttachThreadInput (GetCurrentThreadId (),
                                   child_thread, FALSE);
            }
        }
      /* Ctrl-Break is NT equivalent of SIGINT.  */
      else if (!GenerateConsoleCtrlEvent (CTRL_BREAK_EVENT, pid))
        {
	  DebPrint (("sys_kill.GenerateConsoleCtrlEvent return %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
	  rc = -1;
	}
    }
  else
    {
      if (NILP (Vw32_start_process_share_console) && cp && cp->hwnd)
	{
#if 1
	  if (os_subtype == OS_WIN95)
	    {
/*
   Another possibility is to try terminating the VDM out-right by
   calling the Shell VxD (id 0x17) V86 interface, function #4
   "SHELL_Destroy_VM", ie.

     mov edx,4
     mov ebx,vm_handle
     call shellapi

   First need to determine the current VM handle, and then arrange for
   the shellapi call to be made from the system vm (by using
   Switch_VM_and_callback).

   Could try to invoke DestroyVM through CallVxD.

*/
#if 0
	      /* On Win95, posting WM_QUIT causes the 16-bit subsystem
		 to hang when cmdproxy is used in conjunction with
		 command.com for an interactive shell.  Posting
		 WM_CLOSE pops up a dialog that, when Yes is selected,
		 does the same thing.  TerminateProcess is also less
		 than ideal in that subprocesses tend to stick around
		 until the machine is shutdown, but at least it
		 doesn't freeze the 16-bit subsystem.  */
	      PostMessage (cp->hwnd, WM_QUIT, 0xff, 0);
#endif
	      if (!TerminateProcess (proc_hand, 0xff))
		{
		  DebPrint (("sys_kill.TerminateProcess returned %d "
			     "for pid %lu\n", GetLastError (), pid));
		  errno = EINVAL;
		  rc = -1;
		}
	    }
	  else
#endif
	    PostMessage (cp->hwnd, WM_CLOSE, 0, 0);
	}
      /* Kill the process.  On W32 this doesn't kill child processes
	 so it doesn't work very well for shells which is why it's not
	 used in every case.  */
      else if (!TerminateProcess (proc_hand, 0xff))
        {
	  DebPrint (("sys_kill.TerminateProcess returned %d "
		     "for pid %lu\n", GetLastError (), pid));
	  errno = EINVAL;
	  rc = -1;
        }
    }

  if (need_to_free)
    CloseHandle (proc_hand);

  return rc;
}

/* The following two routines are used to manipulate stdin, stdout, and
   stderr of our child processes.

   Assuming that in, out, and err are *not* inheritable, we make them
   stdin, stdout, and stderr of the child as follows:

   - Save the parent's current standard handles.
   - Set the std handles to inheritable duplicates of the ones being passed in.
     (Note that _get_osfhandle() is an io.h procedure that retrieves the
     NT file handle for a crt file descriptor.)
   - Spawn the child, which inherits in, out, and err as stdin,
     stdout, and stderr. (see Spawnve)
   - Close the std handles passed to the child.
   - Reset the parent's standard handles to the saved handles.
     (see reset_standard_handles)
   We assume that the caller closes in, out, and err after calling us.  */

void
prepare_standard_handles (int in, int out, int err, HANDLE handles[3])
{
  HANDLE parent;
  HANDLE newstdin, newstdout, newstderr;

  parent = GetCurrentProcess ();

  handles[0] = GetStdHandle (STD_INPUT_HANDLE);
  handles[1] = GetStdHandle (STD_OUTPUT_HANDLE);
  handles[2] = GetStdHandle (STD_ERROR_HANDLE);

  /* make inheritable copies of the new handles */
  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (in),
		       parent,
		       &newstdin,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating input handle for child", Qnil);

  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (out),
		       parent,
		       &newstdout,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating output handle for child", Qnil);

  if (!DuplicateHandle (parent,
		       (HANDLE) _get_osfhandle (err),
		       parent,
		       &newstderr,
		       0,
		       TRUE,
		       DUPLICATE_SAME_ACCESS))
    report_file_error ("Duplicating error handle for child", Qnil);

  /* and store them as our std handles */
  if (!SetStdHandle (STD_INPUT_HANDLE, newstdin))
    report_file_error ("Changing stdin handle", Qnil);

  if (!SetStdHandle (STD_OUTPUT_HANDLE, newstdout))
    report_file_error ("Changing stdout handle", Qnil);

  if (!SetStdHandle (STD_ERROR_HANDLE, newstderr))
    report_file_error ("Changing stderr handle", Qnil);
}

void
reset_standard_handles (int in, int out, int err, HANDLE handles[3])
{
  /* close the duplicated handles passed to the child */
  CloseHandle (GetStdHandle (STD_INPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_OUTPUT_HANDLE));
  CloseHandle (GetStdHandle (STD_ERROR_HANDLE));

  /* now restore parent's saved std handles */
  SetStdHandle (STD_INPUT_HANDLE, handles[0]);
  SetStdHandle (STD_OUTPUT_HANDLE, handles[1]);
  SetStdHandle (STD_ERROR_HANDLE, handles[2]);
}

void
set_process_dir (char * dir)
{
  process_dir = dir;
}

/* To avoid problems with winsock implementations that work over dial-up
   connections causing or requiring a connection to exist while Emacs is
   running, Emacs no longer automatically loads winsock on startup if it
   is present.  Instead, it will be loaded when open-network-stream is
   first called.

   To allow full control over when winsock is loaded, we provide these
   two functions to dynamically load and unload winsock.  This allows
   dial-up users to only be connected when they actually need to use
   socket services.  */

/* From w32.c */
extern HANDLE winsock_lib;
extern BOOL term_winsock (void);
extern BOOL init_winsock (int load_now);

DEFUN ("w32-has-winsock", Fw32_has_winsock, Sw32_has_winsock, 0, 1, 0,
       doc: /* Test for presence of the Windows socket library `winsock'.
Returns non-nil if winsock support is present, nil otherwise.

If the optional argument LOAD-NOW is non-nil, the winsock library is
also loaded immediately if not already loaded.  If winsock is loaded,
the winsock local hostname is returned (since this may be different from
the value of `system-name' and should supplant it), otherwise t is
returned to indicate winsock support is present.  */)
  (Lisp_Object load_now)
{
  int have_winsock;

  have_winsock = init_winsock (!NILP (load_now));
  if (have_winsock)
    {
      if (winsock_lib != NULL)
	{
	  /* Return new value for system-name.  The best way to do this
	     is to call init_system_name, saving and restoring the
	     original value to avoid side-effects.  */
	  Lisp_Object orig_hostname = Vsystem_name;
	  Lisp_Object hostname;

	  init_system_name ();
	  hostname = Vsystem_name;
	  Vsystem_name = orig_hostname;
	  return hostname;
	}
      return Qt;
    }
  return Qnil;
}

DEFUN ("w32-unload-winsock", Fw32_unload_winsock, Sw32_unload_winsock,
       0, 0, 0,
       doc: /* Unload the Windows socket library `winsock' if loaded.
This is provided to allow dial-up socket connections to be disconnected
when no longer needed.  Returns nil without unloading winsock if any
socket connections still exist.  */)
  (void)
{
  return term_winsock () ? Qt : Qnil;
}


/* Some miscellaneous functions that are Windows specific, but not GUI
   specific (ie. are applicable in terminal or batch mode as well).  */

DEFUN ("w32-short-file-name", Fw32_short_file_name, Sw32_short_file_name, 1, 1, 0,
       doc: /* Return the short file name version (8.3) of the full path of FILENAME.
If FILENAME does not exist, return nil.
All path elements in FILENAME are converted to their short names.  */)
  (Lisp_Object filename)
{
  char shortname[MAX_PATH];

  CHECK_STRING (filename);

  /* first expand it.  */
  filename = Fexpand_file_name (filename, Qnil);

  /* luckily, this returns the short version of each element in the path.  */
  if (GetShortPathName (SDATA (ENCODE_FILE (filename)), shortname, MAX_PATH) == 0)
    return Qnil;

  dostounix_filename (shortname);

  return build_string (shortname);
}


DEFUN ("w32-long-file-name", Fw32_long_file_name, Sw32_long_file_name,
       1, 1, 0,
       doc: /* Return the long file name version of the full path of FILENAME.
If FILENAME does not exist, return nil.
All path elements in FILENAME are converted to their long names.  */)
  (Lisp_Object filename)
{
  char longname[ MAX_PATH ];
  int drive_only = 0;

  CHECK_STRING (filename);

  if (SBYTES (filename) == 2
      && *(SDATA (filename) + 1) == ':')
    drive_only = 1;

  /* first expand it.  */
  filename = Fexpand_file_name (filename, Qnil);

  if (!w32_get_long_filename (SDATA (ENCODE_FILE (filename)), longname, MAX_PATH))
    return Qnil;

  dostounix_filename (longname);

  /* If we were passed only a drive, make sure that a slash is not appended
     for consistency with directories.  Allow for drive mapping via SUBST
     in case expand-file-name is ever changed to expand those.  */
  if (drive_only && longname[1] == ':' && longname[2] == '/' && !longname[3])
    longname[2] = '\0';

  return DECODE_FILE (build_string (longname));
}

DEFUN ("w32-set-process-priority", Fw32_set_process_priority,
       Sw32_set_process_priority, 2, 2, 0,
       doc: /* Set the priority of PROCESS to PRIORITY.
If PROCESS is nil, the priority of Emacs is changed, otherwise the
priority of the process whose pid is PROCESS is changed.
PRIORITY should be one of the symbols high, normal, or low;
any other symbol will be interpreted as normal.

If successful, the return value is t, otherwise nil.  */)
  (Lisp_Object process, Lisp_Object priority)
{
  HANDLE proc_handle = GetCurrentProcess ();
  DWORD  priority_class = NORMAL_PRIORITY_CLASS;
  Lisp_Object result = Qnil;

  CHECK_SYMBOL (priority);

  if (!NILP (process))
    {
      DWORD pid;
      child_process *cp;

      CHECK_NUMBER (process);

      /* Allow pid to be an internally generated one, or one obtained
	 externally.  This is necessary because real pids on Win95 are
	 negative.  */

      pid = XINT (process);
      cp = find_child_pid (pid);
      if (cp != NULL)
	pid = cp->procinfo.dwProcessId;

      proc_handle = OpenProcess (PROCESS_SET_INFORMATION, FALSE, pid);
    }

  if (EQ (priority, Qhigh))
    priority_class = HIGH_PRIORITY_CLASS;
  else if (EQ (priority, Qlow))
    priority_class = IDLE_PRIORITY_CLASS;

  if (proc_handle != NULL)
    {
      if (SetPriorityClass (proc_handle, priority_class))
	result = Qt;
      if (!NILP (process))
	CloseHandle (proc_handle);
    }

  return result;
}

#ifdef HAVE_LANGINFO_CODESET
/* Emulation of nl_langinfo.  Used in fns.c:Flocale_info.  */
char *
nl_langinfo (nl_item item)
{
  /* Conversion of Posix item numbers to their Windows equivalents.  */
  static const LCTYPE w32item[] = {
    LOCALE_IDEFAULTANSICODEPAGE,
    LOCALE_SDAYNAME1, LOCALE_SDAYNAME2, LOCALE_SDAYNAME3,
    LOCALE_SDAYNAME4, LOCALE_SDAYNAME5, LOCALE_SDAYNAME6, LOCALE_SDAYNAME7,
    LOCALE_SMONTHNAME1, LOCALE_SMONTHNAME2, LOCALE_SMONTHNAME3,
    LOCALE_SMONTHNAME4, LOCALE_SMONTHNAME5, LOCALE_SMONTHNAME6,
    LOCALE_SMONTHNAME7, LOCALE_SMONTHNAME8, LOCALE_SMONTHNAME9,
    LOCALE_SMONTHNAME10, LOCALE_SMONTHNAME11, LOCALE_SMONTHNAME12
  };

  static char *nl_langinfo_buf = NULL;
  static int   nl_langinfo_len = 0;

  if (nl_langinfo_len <= 0)
    nl_langinfo_buf = xmalloc (nl_langinfo_len = 1);

  if (item < 0 || item >= _NL_NUM)
    nl_langinfo_buf[0] = 0;
  else
    {
      LCID cloc = GetThreadLocale ();
      int need_len = GetLocaleInfo (cloc, w32item[item] | LOCALE_USE_CP_ACP,
				    NULL, 0);

      if (need_len <= 0)
	nl_langinfo_buf[0] = 0;
      else
	{
	  if (item == CODESET)
	    {
	      need_len += 2;	/* for the "cp" prefix */
	      if (need_len < 8)	/* for the case we call GetACP */
		need_len = 8;
	    }
	  if (nl_langinfo_len <= need_len)
	    nl_langinfo_buf = xrealloc (nl_langinfo_buf,
					nl_langinfo_len = need_len);
	  if (!GetLocaleInfo (cloc, w32item[item] | LOCALE_USE_CP_ACP,
			      nl_langinfo_buf, nl_langinfo_len))
	    nl_langinfo_buf[0] = 0;
	  else if (item == CODESET)
	    {
	      if (strcmp (nl_langinfo_buf, "0") == 0 /* CP_ACP */
		  || strcmp (nl_langinfo_buf, "1") == 0) /* CP_OEMCP */
		sprintf (nl_langinfo_buf, "cp%u", GetACP ());
	      else
		{
		  memmove (nl_langinfo_buf + 2, nl_langinfo_buf,
			   strlen (nl_langinfo_buf) + 1);
		  nl_langinfo_buf[0] = 'c';
		  nl_langinfo_buf[1] = 'p';
		}
	    }
	}
    }
  return nl_langinfo_buf;
}
#endif	/* HAVE_LANGINFO_CODESET */

DEFUN ("w32-get-locale-info", Fw32_get_locale_info,
       Sw32_get_locale_info, 1, 2, 0,
       doc: /* Return information about the Windows locale LCID.
By default, return a three letter locale code which encodes the default
language as the first two characters, and the country or regional variant
as the third letter.  For example, ENU refers to `English (United States)',
while ENC means `English (Canadian)'.

If the optional argument LONGFORM is t, the long form of the locale
name is returned, e.g. `English (United States)' instead; if LONGFORM
is a number, it is interpreted as an LCTYPE constant and the corresponding
locale information is returned.

If LCID (a 16-bit number) is not a valid locale, the result is nil.  */)
  (Lisp_Object lcid, Lisp_Object longform)
{
  int got_abbrev;
  int got_full;
  char abbrev_name[32] = { 0 };
  char full_name[256] = { 0 };

  CHECK_NUMBER (lcid);

  if (!IsValidLocale (XINT (lcid), LCID_SUPPORTED))
    return Qnil;

  if (NILP (longform))
    {
      got_abbrev = GetLocaleInfo (XINT (lcid),
				  LOCALE_SABBREVLANGNAME | LOCALE_USE_CP_ACP,
				  abbrev_name, sizeof (abbrev_name));
      if (got_abbrev)
	return build_string (abbrev_name);
    }
  else if (EQ (longform, Qt))
    {
      got_full = GetLocaleInfo (XINT (lcid),
				LOCALE_SLANGUAGE | LOCALE_USE_CP_ACP,
				full_name, sizeof (full_name));
      if (got_full)
	return DECODE_SYSTEM (build_string (full_name));
    }
  else if (NUMBERP (longform))
    {
      got_full = GetLocaleInfo (XINT (lcid),
				XINT (longform),
				full_name, sizeof (full_name));
      if (got_full)
	return make_unibyte_string (full_name, got_full);
    }

  return Qnil;
}


DEFUN ("w32-get-current-locale-id", Fw32_get_current_locale_id,
       Sw32_get_current_locale_id, 0, 0, 0,
       doc: /* Return Windows locale id for current locale setting.
This is a numerical value; use `w32-get-locale-info' to convert to a
human-readable form.  */)
  (void)
{
  return make_number (GetThreadLocale ());
}

static DWORD
int_from_hex (char * s)
{
  DWORD val = 0;
  static char hex[] = "0123456789abcdefABCDEF";
  char * p;

  while (*s && (p = strchr (hex, *s)) != NULL)
    {
      unsigned digit = p - hex;
      if (digit > 15)
	digit -= 6;
      val = val * 16 + digit;
      s++;
    }
  return val;
}

/* We need to build a global list, since the EnumSystemLocale callback
   function isn't given a context pointer.  */
Lisp_Object Vw32_valid_locale_ids;

static BOOL CALLBACK
enum_locale_fn (LPTSTR localeNum)
{
  DWORD id = int_from_hex (localeNum);
  Vw32_valid_locale_ids = Fcons (make_number (id), Vw32_valid_locale_ids);
  return TRUE;
}

DEFUN ("w32-get-valid-locale-ids", Fw32_get_valid_locale_ids,
       Sw32_get_valid_locale_ids, 0, 0, 0,
       doc: /* Return list of all valid Windows locale ids.
Each id is a numerical value; use `w32-get-locale-info' to convert to a
human-readable form.  */)
  (void)
{
  Vw32_valid_locale_ids = Qnil;

  EnumSystemLocales (enum_locale_fn, LCID_SUPPORTED);

  Vw32_valid_locale_ids = Fnreverse (Vw32_valid_locale_ids);
  return Vw32_valid_locale_ids;
}


DEFUN ("w32-get-default-locale-id", Fw32_get_default_locale_id, Sw32_get_default_locale_id, 0, 1, 0,
       doc: /* Return Windows locale id for default locale setting.
By default, the system default locale setting is returned; if the optional
parameter USERP is non-nil, the user default locale setting is returned.
This is a numerical value; use `w32-get-locale-info' to convert to a
human-readable form.  */)
  (Lisp_Object userp)
{
  if (NILP (userp))
    return make_number (GetSystemDefaultLCID ());
  return make_number (GetUserDefaultLCID ());
}


DEFUN ("w32-set-current-locale", Fw32_set_current_locale, Sw32_set_current_locale, 1, 1, 0,
       doc: /* Make Windows locale LCID be the current locale setting for Emacs.
If successful, the new locale id is returned, otherwise nil.  */)
  (Lisp_Object lcid)
{
  CHECK_NUMBER (lcid);

  if (!IsValidLocale (XINT (lcid), LCID_SUPPORTED))
    return Qnil;

  if (!SetThreadLocale (XINT (lcid)))
    return Qnil;

  /* Need to set input thread locale if present.  */
  if (dwWindowsThreadId)
    /* Reply is not needed.  */
    PostThreadMessage (dwWindowsThreadId, WM_EMACS_SETLOCALE, XINT (lcid), 0);

  return make_number (GetThreadLocale ());
}


/* We need to build a global list, since the EnumCodePages callback
   function isn't given a context pointer.  */
Lisp_Object Vw32_valid_codepages;

static BOOL CALLBACK
enum_codepage_fn (LPTSTR codepageNum)
{
  DWORD id = atoi (codepageNum);
  Vw32_valid_codepages = Fcons (make_number (id), Vw32_valid_codepages);
  return TRUE;
}

DEFUN ("w32-get-valid-codepages", Fw32_get_valid_codepages,
       Sw32_get_valid_codepages, 0, 0, 0,
       doc: /* Return list of all valid Windows codepages.  */)
  (void)
{
  Vw32_valid_codepages = Qnil;

  EnumSystemCodePages (enum_codepage_fn, CP_SUPPORTED);

  Vw32_valid_codepages = Fnreverse (Vw32_valid_codepages);
  return Vw32_valid_codepages;
}


DEFUN ("w32-get-console-codepage", Fw32_get_console_codepage,
       Sw32_get_console_codepage, 0, 0, 0,
       doc: /* Return current Windows codepage for console input.  */)
  (void)
{
  return make_number (GetConsoleCP ());
}


DEFUN ("w32-set-console-codepage", Fw32_set_console_codepage,
       Sw32_set_console_codepage, 1, 1, 0,
       doc: /* Make Windows codepage CP be the codepage for Emacs tty keyboard input.
This codepage setting affects keyboard input in tty mode.
If successful, the new CP is returned, otherwise nil.  */)
  (Lisp_Object cp)
{
  CHECK_NUMBER (cp);

  if (!IsValidCodePage (XINT (cp)))
    return Qnil;

  if (!SetConsoleCP (XINT (cp)))
    return Qnil;

  return make_number (GetConsoleCP ());
}


DEFUN ("w32-get-console-output-codepage", Fw32_get_console_output_codepage,
       Sw32_get_console_output_codepage, 0, 0, 0,
       doc: /* Return current Windows codepage for console output.  */)
  (void)
{
  return make_number (GetConsoleOutputCP ());
}


DEFUN ("w32-set-console-output-codepage", Fw32_set_console_output_codepage,
       Sw32_set_console_output_codepage, 1, 1, 0,
       doc: /* Make Windows codepage CP be the codepage for Emacs console output.
This codepage setting affects display in tty mode.
If successful, the new CP is returned, otherwise nil.  */)
  (Lisp_Object cp)
{
  CHECK_NUMBER (cp);

  if (!IsValidCodePage (XINT (cp)))
    return Qnil;

  if (!SetConsoleOutputCP (XINT (cp)))
    return Qnil;

  return make_number (GetConsoleOutputCP ());
}


DEFUN ("w32-get-codepage-charset", Fw32_get_codepage_charset,
       Sw32_get_codepage_charset, 1, 1, 0,
       doc: /* Return charset ID corresponding to codepage CP.
Returns nil if the codepage is not valid.  */)
  (Lisp_Object cp)
{
  CHARSETINFO info;

  CHECK_NUMBER (cp);

  if (!IsValidCodePage (XINT (cp)))
    return Qnil;

  if (TranslateCharsetInfo ((DWORD *) XINT (cp), &info, TCI_SRCCODEPAGE))
    return make_number (info.ciCharset);

  return Qnil;
}


DEFUN ("w32-get-valid-keyboard-layouts", Fw32_get_valid_keyboard_layouts,
       Sw32_get_valid_keyboard_layouts, 0, 0, 0,
       doc: /* Return list of Windows keyboard languages and layouts.
The return value is a list of pairs of language id and layout id.  */)
  (void)
{
  int num_layouts = GetKeyboardLayoutList (0, NULL);
  HKL * layouts = (HKL *) alloca (num_layouts * sizeof (HKL));
  Lisp_Object obj = Qnil;

  if (GetKeyboardLayoutList (num_layouts, layouts) == num_layouts)
    {
      while (--num_layouts >= 0)
	{
	  DWORD kl = (DWORD) layouts[num_layouts];

	  obj = Fcons (Fcons (make_number (kl & 0xffff),
			      make_number ((kl >> 16) & 0xffff)),
		       obj);
	}
    }

  return obj;
}


DEFUN ("w32-get-keyboard-layout", Fw32_get_keyboard_layout,
       Sw32_get_keyboard_layout, 0, 0, 0,
       doc: /* Return current Windows keyboard language and layout.
The return value is the cons of the language id and the layout id.  */)
  (void)
{
  DWORD kl = (DWORD) GetKeyboardLayout (dwWindowsThreadId);

  return Fcons (make_number (kl & 0xffff),
		make_number ((kl >> 16) & 0xffff));
}


DEFUN ("w32-set-keyboard-layout", Fw32_set_keyboard_layout,
       Sw32_set_keyboard_layout, 1, 1, 0,
       doc: /* Make LAYOUT be the current keyboard layout for Emacs.
The keyboard layout setting affects interpretation of keyboard input.
If successful, the new layout id is returned, otherwise nil.  */)
  (Lisp_Object layout)
{
  DWORD kl;

  CHECK_CONS (layout);
  CHECK_NUMBER_CAR (layout);
  CHECK_NUMBER_CDR (layout);

  kl = (XINT (XCAR (layout)) & 0xffff)
    | (XINT (XCDR (layout)) << 16);

  /* Synchronize layout with input thread.  */
  if (dwWindowsThreadId)
    {
      if (PostThreadMessage (dwWindowsThreadId, WM_EMACS_SETKEYBOARDLAYOUT,
			     (WPARAM) kl, 0))
	{
	  MSG msg;
	  GetMessage (&msg, NULL, WM_EMACS_DONE, WM_EMACS_DONE);

	  if (msg.wParam == 0)
	    return Qnil;
	}
    }
  else if (!ActivateKeyboardLayout ((HKL) kl, 0))
    return Qnil;

  return Fw32_get_keyboard_layout ();
}


void
syms_of_ntproc (void)
{
  DEFSYM (Qhigh, "high");
  DEFSYM (Qlow, "low");

  defsubr (&Sw32_has_winsock);
  defsubr (&Sw32_unload_winsock);

  defsubr (&Sw32_short_file_name);
  defsubr (&Sw32_long_file_name);
  defsubr (&Sw32_set_process_priority);
  defsubr (&Sw32_get_locale_info);
  defsubr (&Sw32_get_current_locale_id);
  defsubr (&Sw32_get_default_locale_id);
  defsubr (&Sw32_get_valid_locale_ids);
  defsubr (&Sw32_set_current_locale);

  defsubr (&Sw32_get_console_codepage);
  defsubr (&Sw32_set_console_codepage);
  defsubr (&Sw32_get_console_output_codepage);
  defsubr (&Sw32_set_console_output_codepage);
  defsubr (&Sw32_get_valid_codepages);
  defsubr (&Sw32_get_codepage_charset);

  defsubr (&Sw32_get_valid_keyboard_layouts);
  defsubr (&Sw32_get_keyboard_layout);
  defsubr (&Sw32_set_keyboard_layout);

  DEFVAR_LISP ("w32-quote-process-args", Vw32_quote_process_args,
	       doc: /* Non-nil enables quoting of process arguments to ensure correct parsing.
Because Windows does not directly pass argv arrays to child processes,
programs have to reconstruct the argv array by parsing the command
line string.  For an argument to contain a space, it must be enclosed
in double quotes or it will be parsed as multiple arguments.

If the value is a character, that character will be used to escape any
quote characters that appear, otherwise a suitable escape character
will be chosen based on the type of the program.  */);
  Vw32_quote_process_args = Qt;

  DEFVAR_LISP ("w32-start-process-show-window",
	       Vw32_start_process_show_window,
	       doc: /* When nil, new child processes hide their windows.
When non-nil, they show their window in the method of their choice.
This variable doesn't affect GUI applications, which will never be hidden.  */);
  Vw32_start_process_show_window = Qnil;

  DEFVAR_LISP ("w32-start-process-share-console",
	       Vw32_start_process_share_console,
	       doc: /* When nil, new child processes are given a new console.
When non-nil, they share the Emacs console; this has the limitation of
allowing only one DOS subprocess to run at a time (whether started directly
or indirectly by Emacs), and preventing Emacs from cleanly terminating the
subprocess group, but may allow Emacs to interrupt a subprocess that doesn't
otherwise respond to interrupts from Emacs.  */);
  Vw32_start_process_share_console = Qnil;

  DEFVAR_LISP ("w32-start-process-inherit-error-mode",
	       Vw32_start_process_inherit_error_mode,
	       doc: /* When nil, new child processes revert to the default error mode.
When non-nil, they inherit their error mode setting from Emacs, which stops
them blocking when trying to access unmounted drives etc.  */);
  Vw32_start_process_inherit_error_mode = Qt;

  DEFVAR_INT ("w32-pipe-read-delay", w32_pipe_read_delay,
	      doc: /* Forced delay before reading subprocess output.
This is done to improve the buffering of subprocess output, by
avoiding the inefficiency of frequently reading small amounts of data.

If positive, the value is the number of milliseconds to sleep before
reading the subprocess output.  If negative, the magnitude is the number
of time slices to wait (effectively boosting the priority of the child
process temporarily).  A value of zero disables waiting entirely.  */);
  w32_pipe_read_delay = 50;

  DEFVAR_LISP ("w32-downcase-file-names", Vw32_downcase_file_names,
	       doc: /* Non-nil means convert all-upper case file names to lower case.
This applies when performing completions and file name expansion.
Note that the value of this setting also affects remote file names,
so you probably don't want to set to non-nil if you use case-sensitive
filesystems via ange-ftp.  */);
  Vw32_downcase_file_names = Qnil;

#if 0
  DEFVAR_LISP ("w32-generate-fake-inodes", Vw32_generate_fake_inodes,
	       doc: /* Non-nil means attempt to fake realistic inode values.
This works by hashing the truename of files, and should detect
aliasing between long and short (8.3 DOS) names, but can have
false positives because of hash collisions.  Note that determining
the truename of a file can be slow.  */);
  Vw32_generate_fake_inodes = Qnil;
#endif

  DEFVAR_LISP ("w32-get-true-file-attributes", Vw32_get_true_file_attributes,
	       doc: /* Non-nil means determine accurate file attributes in `file-attributes'.
This option controls whether to issue additional system calls to determine
accurate link counts, file type, and ownership information.  It is more
useful for files on NTFS volumes, where hard links and file security are
supported, than on volumes of the FAT family.

Without these system calls, link count will always be reported as 1 and file
ownership will be attributed to the current user.
The default value `local' means only issue these system calls for files
on local fixed drives.  A value of nil means never issue them.
Any other non-nil value means do this even on remote and removable drives
where the performance impact may be noticeable even on modern hardware.  */);
  Vw32_get_true_file_attributes = Qlocal;

  staticpro (&Vw32_valid_locale_ids);
  staticpro (&Vw32_valid_codepages);
}
/* end of w32proc.c */
