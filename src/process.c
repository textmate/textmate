/* Asynchronous subprocess control for GNU Emacs.

Copyright (C) 1985-1988, 1993-1996, 1998-1999, 2001-2012
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
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <sys/types.h>		/* Some typedefs are used in sys/file.h.  */
#include <sys/file.h>
#include <sys/stat.h>
#include <setjmp.h>

#include <unistd.h>
#include <fcntl.h>

#include "lisp.h"

/* Only MS-DOS does not define `subprocesses'.  */
#ifdef subprocesses

#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>

/* Are local (unix) sockets supported?  */
#if defined (HAVE_SYS_UN_H)
#if !defined (AF_LOCAL) && defined (AF_UNIX)
#define AF_LOCAL AF_UNIX
#endif
#ifdef AF_LOCAL
#define HAVE_LOCAL_SOCKETS
#include <sys/un.h>
#endif
#endif

#include <sys/ioctl.h>
#if defined (HAVE_NET_IF_H)
#include <net/if.h>
#endif /* HAVE_NET_IF_H */

#if defined (HAVE_IFADDRS_H)
/* Must be after net/if.h */
#include <ifaddrs.h>

/* We only use structs from this header when we use getifaddrs.  */
#if defined (HAVE_NET_IF_DL_H)
#include <net/if_dl.h>
#endif

#endif

#ifdef NEED_BSDTTY
#include <bsdtty.h>
#endif

#ifdef HAVE_RES_INIT
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <resolv.h>
#endif

#ifdef HAVE_UTIL_H
#include <util.h>
#endif

#ifdef HAVE_PTY_H
#include <pty.h>
#endif

#endif	/* subprocesses */

#include "systime.h"
#include "systty.h"

#include "window.h"
#include "buffer.h"
#include "character.h"
#include "coding.h"
#include "process.h"
#include "frame.h"
#include "termhooks.h"
#include "termopts.h"
#include "commands.h"
#include "keyboard.h"
#include "blockinput.h"
#include "dispextern.h"
#include "composite.h"
#include "atimer.h"
#include "sysselect.h"
#include "syssignal.h"
#include "syswait.h"
#ifdef HAVE_GNUTLS
#include "gnutls.h"
#endif

#if defined (USE_GTK) || defined (HAVE_GCONF) || defined (HAVE_GSETTINGS)
#include "xgselect.h"
#endif
#ifdef HAVE_NS
#include "nsterm.h"
#endif

Lisp_Object Qeuid, Qegid, Qcomm, Qstate, Qppid, Qpgrp, Qsess, Qttname, Qtpgid;
Lisp_Object Qminflt, Qmajflt, Qcminflt, Qcmajflt, Qutime, Qstime, Qcstime;
Lisp_Object Qcutime, Qpri, Qnice, Qthcount, Qstart, Qvsize, Qrss, Qargs;
Lisp_Object Quser, Qgroup, Qetime, Qpcpu, Qpmem, Qtime, Qctime;
Lisp_Object QCname, QCtype;

/* Non-zero if keyboard input is on hold, zero otherwise.  */

static int kbd_is_on_hold;

/* Nonzero means don't run process sentinels.  This is used
   when exiting.  */
int inhibit_sentinels;

#ifdef subprocesses

Lisp_Object Qprocessp;
static Lisp_Object Qrun, Qstop, Qsignal;
static Lisp_Object Qopen, Qclosed, Qconnect, Qfailed, Qlisten;
Lisp_Object Qlocal;
static Lisp_Object Qipv4, Qdatagram, Qseqpacket;
static Lisp_Object Qreal, Qnetwork, Qserial;
#ifdef AF_INET6
static Lisp_Object Qipv6;
#endif
static Lisp_Object QCport, QCprocess;
Lisp_Object QCspeed;
Lisp_Object QCbytesize, QCstopbits, QCparity, Qodd, Qeven;
Lisp_Object QCflowcontrol, Qhw, Qsw, QCsummary;
static Lisp_Object QCbuffer, QChost, QCservice;
static Lisp_Object QClocal, QCremote, QCcoding;
static Lisp_Object QCserver, QCnowait, QCnoquery, QCstop;
static Lisp_Object QCsentinel, QClog, QCoptions, QCplist;
static Lisp_Object Qlast_nonmenu_event;
/* QCfamily is declared and initialized in xfaces.c,
   QCfilter in keyboard.c.  */
extern Lisp_Object QCfamily, QCfilter;

/* Qexit is declared and initialized in eval.c.  */

/* QCfamily is defined in xfaces.c.  */
extern Lisp_Object QCfamily;
/* QCfilter is defined in keyboard.c.  */
extern Lisp_Object QCfilter;

#define NETCONN_P(p) (EQ (XPROCESS (p)->type, Qnetwork))
#define NETCONN1_P(p) (EQ ((p)->type, Qnetwork))
#define SERIALCONN_P(p) (EQ (XPROCESS (p)->type, Qserial))
#define SERIALCONN1_P(p) (EQ ((p)->type, Qserial))

#ifndef HAVE_H_ERRNO
extern int h_errno;
#endif

/* Number of events of change of status of a process.  */
static int process_tick;
/* Number of events for which the user or sentinel has been notified.  */
static int update_tick;

/* Define NON_BLOCKING_CONNECT if we can support non-blocking connects.  */

/* Only W32 has this, it really means that select can't take write mask.  */
#ifdef BROKEN_NON_BLOCKING_CONNECT
#undef NON_BLOCKING_CONNECT
#define SELECT_CANT_DO_WRITE_MASK
#else
#ifndef NON_BLOCKING_CONNECT
#ifdef HAVE_SELECT
#if defined (HAVE_GETPEERNAME) || defined (GNU_LINUX)
#if defined (O_NONBLOCK) || defined (O_NDELAY)
#if defined (EWOULDBLOCK) || defined (EINPROGRESS)
#define NON_BLOCKING_CONNECT
#endif /* EWOULDBLOCK || EINPROGRESS */
#endif /* O_NONBLOCK || O_NDELAY */
#endif /* HAVE_GETPEERNAME || GNU_LINUX */
#endif /* HAVE_SELECT */
#endif /* NON_BLOCKING_CONNECT */
#endif /* BROKEN_NON_BLOCKING_CONNECT */

/* Define DATAGRAM_SOCKETS if datagrams can be used safely on
   this system.  We need to read full packets, so we need a
   "non-destructive" select.  So we require either native select,
   or emulation of select using FIONREAD.  */

#ifdef BROKEN_DATAGRAM_SOCKETS
#undef DATAGRAM_SOCKETS
#else
#ifndef DATAGRAM_SOCKETS
#if defined (HAVE_SELECT) || defined (FIONREAD)
#if defined (HAVE_SENDTO) && defined (HAVE_RECVFROM) && defined (EMSGSIZE)
#define DATAGRAM_SOCKETS
#endif /* HAVE_SENDTO && HAVE_RECVFROM && EMSGSIZE */
#endif /* HAVE_SELECT || FIONREAD */
#endif /* DATAGRAM_SOCKETS */
#endif /* BROKEN_DATAGRAM_SOCKETS */

#if defined HAVE_LOCAL_SOCKETS && defined DATAGRAM_SOCKETS
# define HAVE_SEQPACKET
#endif

#if !defined (ADAPTIVE_READ_BUFFERING) && !defined (NO_ADAPTIVE_READ_BUFFERING)
#ifdef EMACS_HAS_USECS
#define ADAPTIVE_READ_BUFFERING
#endif
#endif

#ifdef ADAPTIVE_READ_BUFFERING
#define READ_OUTPUT_DELAY_INCREMENT 10000
#define READ_OUTPUT_DELAY_MAX       (READ_OUTPUT_DELAY_INCREMENT * 5)
#define READ_OUTPUT_DELAY_MAX_MAX   (READ_OUTPUT_DELAY_INCREMENT * 7)

/* Number of processes which have a non-zero read_output_delay,
   and therefore might be delayed for adaptive read buffering.  */

static int process_output_delay_count;

/* Non-zero if any process has non-nil read_output_skip.  */

static int process_output_skip;

#else
#define process_output_delay_count 0
#endif

static Lisp_Object Fget_process (Lisp_Object);
static void create_process (Lisp_Object, char **, Lisp_Object);
#ifdef SIGIO
static int keyboard_bit_set (SELECT_TYPE *);
#endif
static void deactivate_process (Lisp_Object);
static void status_notify (struct Lisp_Process *);
static int read_process_output (Lisp_Object, int);
static void create_pty (Lisp_Object);

/* If we support a window system, turn on the code to poll periodically
   to detect C-g.  It isn't actually used when doing interrupt input.  */
#if defined (HAVE_WINDOW_SYSTEM) && !defined (USE_ASYNC_EVENTS)
#define POLL_FOR_INPUT
#endif

static Lisp_Object get_process (register Lisp_Object name);
static void exec_sentinel (Lisp_Object proc, Lisp_Object reason);

/* Mask of bits indicating the descriptors that we wait for input on.  */

static SELECT_TYPE input_wait_mask;

/* Mask that excludes keyboard input descriptor(s).  */

static SELECT_TYPE non_keyboard_wait_mask;

/* Mask that excludes process input descriptor(s).  */

static SELECT_TYPE non_process_wait_mask;

/* Mask for selecting for write.  */

static SELECT_TYPE write_mask;

#ifdef NON_BLOCKING_CONNECT
/* Mask of bits indicating the descriptors that we wait for connect to
   complete on.  Once they complete, they are removed from this mask
   and added to the input_wait_mask and non_keyboard_wait_mask.  */

static SELECT_TYPE connect_wait_mask;

/* Number of bits set in connect_wait_mask.  */
static int num_pending_connects;
#endif	/* NON_BLOCKING_CONNECT */

/* The largest descriptor currently in use for a process object.  */
static int max_process_desc;

/* The largest descriptor currently in use for input.  */
static int max_input_desc;

/* Indexed by descriptor, gives the process (if any) for that descriptor */
static Lisp_Object chan_process[MAXDESC];

/* Alist of elements (NAME . PROCESS) */
static Lisp_Object Vprocess_alist;

/* Buffered-ahead input char from process, indexed by channel.
   -1 means empty (no char is buffered).
   Used on sys V where the only way to tell if there is any
   output from the process is to read at least one char.
   Always -1 on systems that support FIONREAD.  */

static int proc_buffered_char[MAXDESC];

/* Table of `struct coding-system' for each process.  */
static struct coding_system *proc_decode_coding_system[MAXDESC];
static struct coding_system *proc_encode_coding_system[MAXDESC];

#ifdef DATAGRAM_SOCKETS
/* Table of `partner address' for datagram sockets.  */
static struct sockaddr_and_len {
  struct sockaddr *sa;
  int len;
} datagram_address[MAXDESC];
#define DATAGRAM_CHAN_P(chan)	(datagram_address[chan].sa != 0)
#define DATAGRAM_CONN_P(proc)	(PROCESSP (proc) && datagram_address[XPROCESS (proc)->infd].sa != 0)
#else
#define DATAGRAM_CHAN_P(chan)	(0)
#define DATAGRAM_CONN_P(proc)	(0)
#endif

/* Maximum number of bytes to send to a pty without an eof.  */
static int pty_max_bytes;



static struct fd_callback_data
{
  fd_callback func;
  void *data;
#define FOR_READ  1
#define FOR_WRITE 2
  int condition; /* mask of the defines above.  */
} fd_callback_info[MAXDESC];


/* Add a file descriptor FD to be monitored for when read is possible.
   When read is possible, call FUNC with argument DATA.  */

void
add_read_fd (int fd, fd_callback func, void *data)
{
  xassert (fd < MAXDESC);
  add_keyboard_wait_descriptor (fd);

  fd_callback_info[fd].func = func;
  fd_callback_info[fd].data = data;
  fd_callback_info[fd].condition |= FOR_READ;
}

/* Stop monitoring file descriptor FD for when read is possible.  */

void
delete_read_fd (int fd)
{
  xassert (fd < MAXDESC);
  delete_keyboard_wait_descriptor (fd);

  fd_callback_info[fd].condition &= ~FOR_READ;
  if (fd_callback_info[fd].condition == 0)
    {
      fd_callback_info[fd].func = 0;
      fd_callback_info[fd].data = 0;
    }
}

/* Add a file descriptor FD to be monitored for when write is possible.
   When write is possible, call FUNC with argument DATA.  */

void
add_write_fd (int fd, fd_callback func, void *data)
{
  xassert (fd < MAXDESC);
  FD_SET (fd, &write_mask);
  if (fd > max_input_desc)
    max_input_desc = fd;

  fd_callback_info[fd].func = func;
  fd_callback_info[fd].data = data;
  fd_callback_info[fd].condition |= FOR_WRITE;
}

/* Stop monitoring file descriptor FD for when write is possible.  */

void
delete_write_fd (int fd)
{
  int lim = max_input_desc;

  xassert (fd < MAXDESC);
  FD_CLR (fd, &write_mask);
  fd_callback_info[fd].condition &= ~FOR_WRITE;
  if (fd_callback_info[fd].condition == 0)
    {
      fd_callback_info[fd].func = 0;
      fd_callback_info[fd].data = 0;

      if (fd == max_input_desc)
        for (fd = lim; fd >= 0; fd--)
          if (FD_ISSET (fd, &input_wait_mask) || FD_ISSET (fd, &write_mask))
            {
              max_input_desc = fd;
              break;
            }

    }
}


/* Compute the Lisp form of the process status, p->status, from
   the numeric status that was returned by `wait'.  */

static Lisp_Object status_convert (int);

static void
update_status (struct Lisp_Process *p)
{
  eassert (p->raw_status_new);
  p->status = status_convert (p->raw_status);
  p->raw_status_new = 0;
}

/*  Convert a process status word in Unix format to
    the list that we use internally.  */

static Lisp_Object
status_convert (int w)
{
  if (WIFSTOPPED (w))
    return Fcons (Qstop, Fcons (make_number (WSTOPSIG (w)), Qnil));
  else if (WIFEXITED (w))
    return Fcons (Qexit, Fcons (make_number (WRETCODE (w)),
				WCOREDUMP (w) ? Qt : Qnil));
  else if (WIFSIGNALED (w))
    return Fcons (Qsignal, Fcons (make_number (WTERMSIG (w)),
				  WCOREDUMP (w) ? Qt : Qnil));
  else
    return Qrun;
}

/* Given a status-list, extract the three pieces of information
   and store them individually through the three pointers.  */

static void
decode_status (Lisp_Object l, Lisp_Object *symbol, int *code, int *coredump)
{
  Lisp_Object tem;

  if (SYMBOLP (l))
    {
      *symbol = l;
      *code = 0;
      *coredump = 0;
    }
  else
    {
      *symbol = XCAR (l);
      tem = XCDR (l);
      *code = XFASTINT (XCAR (tem));
      tem = XCDR (tem);
      *coredump = !NILP (tem);
    }
}

/* Return a string describing a process status list.  */

static Lisp_Object
status_message (struct Lisp_Process *p)
{
  Lisp_Object status = p->status;
  Lisp_Object symbol;
  int code, coredump;
  Lisp_Object string, string2;

  decode_status (status, &symbol, &code, &coredump);

  if (EQ (symbol, Qsignal) || EQ (symbol, Qstop))
    {
      char *signame;
      synchronize_system_messages_locale ();
      signame = strsignal (code);
      if (signame == 0)
	string = build_string ("unknown");
      else
	{
	  int c1, c2;

	  string = make_unibyte_string (signame, strlen (signame));
	  if (! NILP (Vlocale_coding_system))
	    string = (code_convert_string_norecord
		      (string, Vlocale_coding_system, 0));
	  c1 = STRING_CHAR (SDATA (string));
	  c2 = downcase (c1);
	  if (c1 != c2)
	    Faset (string, make_number (0), make_number (c2));
	}
      string2 = build_string (coredump ? " (core dumped)\n" : "\n");
      return concat2 (string, string2);
    }
  else if (EQ (symbol, Qexit))
    {
      if (NETCONN1_P (p))
	return build_string (code == 0 ? "deleted\n" : "connection broken by remote peer\n");
      if (code == 0)
	return build_string ("finished\n");
      string = Fnumber_to_string (make_number (code));
      string2 = build_string (coredump ? " (core dumped)\n" : "\n");
      return concat3 (build_string ("exited abnormally with code "),
		      string, string2);
    }
  else if (EQ (symbol, Qfailed))
    {
      string = Fnumber_to_string (make_number (code));
      string2 = build_string ("\n");
      return concat3 (build_string ("failed with code "),
		      string, string2);
    }
  else
    return Fcopy_sequence (Fsymbol_name (symbol));
}

#ifdef HAVE_PTYS

/* The file name of the pty opened by allocate_pty.  */
static char pty_name[24];

/* Open an available pty, returning a file descriptor.
   Return -1 on failure.
   The file name of the terminal corresponding to the pty
   is left in the variable pty_name.  */

static int
allocate_pty (void)
{
  int fd;

#ifdef PTY_ITERATION
  PTY_ITERATION
#else
  register int c, i;
  for (c = FIRST_PTY_LETTER; c <= 'z'; c++)
    for (i = 0; i < 16; i++)
#endif
      {
#ifdef PTY_NAME_SPRINTF
	PTY_NAME_SPRINTF
#else
	sprintf (pty_name, "/dev/pty%c%x", c, i);
#endif /* no PTY_NAME_SPRINTF */

#ifdef PTY_OPEN
	PTY_OPEN;
#else /* no PTY_OPEN */
	{
	  { /* Some systems name their pseudoterminals so that there are gaps in
	       the usual sequence - for example, on HP9000/S700 systems, there
	       are no pseudoterminals with names ending in 'f'.  So we wait for
	       three failures in a row before deciding that we've reached the
	       end of the ptys.  */
	    int failed_count = 0;
	    struct stat stb;

	    if (stat (pty_name, &stb) < 0)
	      {
		failed_count++;
		if (failed_count >= 3)
		  return -1;
	      }
	    else
	      failed_count = 0;
	  }
#  ifdef O_NONBLOCK
	  fd = emacs_open (pty_name, O_RDWR | O_NONBLOCK, 0);
#  else
	  fd = emacs_open (pty_name, O_RDWR | O_NDELAY, 0);
#  endif
	}
#endif /* no PTY_OPEN */

	if (fd >= 0)
	  {
	    /* check to make certain that both sides are available
	       this avoids a nasty yet stupid bug in rlogins */
#ifdef PTY_TTY_NAME_SPRINTF
	    PTY_TTY_NAME_SPRINTF
#else
	    sprintf (pty_name, "/dev/tty%c%x", c, i);
#endif /* no PTY_TTY_NAME_SPRINTF */
	    if (access (pty_name, 6) != 0)
	      {
		emacs_close (fd);
# ifndef __sgi
		continue;
# else
		return -1;
# endif /* __sgi */
	      }
	    setup_pty (fd);
	    return fd;
	  }
      }
  return -1;
}
#endif /* HAVE_PTYS */

static Lisp_Object
make_process (Lisp_Object name)
{
  register Lisp_Object val, tem, name1;
  register struct Lisp_Process *p;
  char suffix[sizeof "<>" + INT_STRLEN_BOUND (printmax_t)];
  printmax_t i;

  p = allocate_process ();

  p->infd = -1;
  p->outfd = -1;
  p->tick = 0;
  p->update_tick = 0;
  p->pid = 0;
  p->pty_flag = 0;
  p->raw_status_new = 0;
  p->status = Qrun;
  p->mark = Fmake_marker ();
  p->kill_without_query = 0;

#ifdef ADAPTIVE_READ_BUFFERING
  p->adaptive_read_buffering = 0;
  p->read_output_delay = 0;
  p->read_output_skip = 0;
#endif

#ifdef HAVE_GNUTLS
  p->gnutls_initstage = GNUTLS_STAGE_EMPTY;
  /* Default log level.  */
  p->gnutls_log_level = 0;
  /* GnuTLS handshakes attempted for this connection.  */
  p->gnutls_handshakes_tried = 0;
  p->gnutls_p = 0;
  p->gnutls_state = NULL;
  p->gnutls_x509_cred = NULL;
  p->gnutls_anon_cred = NULL;
#endif

  /* If name is already in use, modify it until it is unused.  */

  name1 = name;
  for (i = 1; ; i++)
    {
      tem = Fget_process (name1);
      if (NILP (tem)) break;
      sprintf (suffix, "<%"pMd">", i);
      name1 = concat2 (name, build_string (suffix));
    }
  name = name1;
  p->name = name;
  XSETPROCESS (val, p);
  Vprocess_alist = Fcons (Fcons (name, val), Vprocess_alist);
  return val;
}

static void
remove_process (register Lisp_Object proc)
{
  register Lisp_Object pair;

  pair = Frassq (proc, Vprocess_alist);
  Vprocess_alist = Fdelq (pair, Vprocess_alist);

  deactivate_process (proc);
}


DEFUN ("processp", Fprocessp, Sprocessp, 1, 1, 0,
       doc: /* Return t if OBJECT is a process.  */)
  (Lisp_Object object)
{
  return PROCESSP (object) ? Qt : Qnil;
}

DEFUN ("get-process", Fget_process, Sget_process, 1, 1, 0,
       doc: /* Return the process named NAME, or nil if there is none.  */)
  (register Lisp_Object name)
{
  if (PROCESSP (name))
    return name;
  CHECK_STRING (name);
  return Fcdr (Fassoc (name, Vprocess_alist));
}

/* This is how commands for the user decode process arguments.  It
   accepts a process, a process name, a buffer, a buffer name, or nil.
   Buffers denote the first process in the buffer, and nil denotes the
   current buffer.  */

static Lisp_Object
get_process (register Lisp_Object name)
{
  register Lisp_Object proc, obj;
  if (STRINGP (name))
    {
      obj = Fget_process (name);
      if (NILP (obj))
	obj = Fget_buffer (name);
      if (NILP (obj))
	error ("Process %s does not exist", SDATA (name));
    }
  else if (NILP (name))
    obj = Fcurrent_buffer ();
  else
    obj = name;

  /* Now obj should be either a buffer object or a process object.
   */
  if (BUFFERP (obj))
    {
      proc = Fget_buffer_process (obj);
      if (NILP (proc))
	error ("Buffer %s has no process", SDATA (BVAR (XBUFFER (obj), name)));
    }
  else
    {
      CHECK_PROCESS (obj);
      proc = obj;
    }
  return proc;
}


#ifdef SIGCHLD
/* Fdelete_process promises to immediately forget about the process, but in
   reality, Emacs needs to remember those processes until they have been
   treated by sigchld_handler; otherwise this handler would consider the
   process as being synchronous and say that the synchronous process is
   dead.  */
static Lisp_Object deleted_pid_list;
#endif

DEFUN ("delete-process", Fdelete_process, Sdelete_process, 1, 1, 0,
       doc: /* Delete PROCESS: kill it and forget about it immediately.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.  */)
  (register Lisp_Object process)
{
  register struct Lisp_Process *p;

  process = get_process (process);
  p = XPROCESS (process);

  p->raw_status_new = 0;
  if (NETCONN1_P (p) || SERIALCONN1_P (p))
    {
      p->status = Fcons (Qexit, Fcons (make_number (0), Qnil));
      p->tick = ++process_tick;
      status_notify (p);
      redisplay_preserve_echo_area (13);
    }
  else if (p->infd >= 0)
    {
#ifdef SIGCHLD
      Lisp_Object symbol;
      /* Assignment to EMACS_INT stops GCC whining about limited range
	 of data type.  */
      EMACS_INT pid = p->pid;

      /* No problem storing the pid here, as it is still in Vprocess_alist.  */
      deleted_pid_list = Fcons (make_fixnum_or_float (pid),
				/* GC treated elements set to nil.  */
				Fdelq (Qnil, deleted_pid_list));
      /* If the process has already signaled, remove it from the list.  */
      if (p->raw_status_new)
	update_status (p);
      symbol = p->status;
      if (CONSP (p->status))
	symbol = XCAR (p->status);
      if (EQ (symbol, Qsignal) || EQ (symbol, Qexit))
	deleted_pid_list
	  = Fdelete (make_fixnum_or_float (pid), deleted_pid_list);
      else
#endif
	{
	  Fkill_process (process, Qnil);
	  /* Do this now, since remove_process will make sigchld_handler do nothing.  */
	  p->status
	    = Fcons (Qsignal, Fcons (make_number (SIGKILL), Qnil));
	  p->tick = ++process_tick;
	  status_notify (p);
	  redisplay_preserve_echo_area (13);
	}
    }
  remove_process (process);
  return Qnil;
}

DEFUN ("process-status", Fprocess_status, Sprocess_status, 1, 1, 0,
       doc: /* Return the status of PROCESS.
The returned value is one of the following symbols:
run  -- for a process that is running.
stop -- for a process stopped but continuable.
exit -- for a process that has exited.
signal -- for a process that has got a fatal signal.
open -- for a network stream connection that is open.
listen -- for a network stream server that is listening.
closed -- for a network stream connection that is closed.
connect -- when waiting for a non-blocking connection to complete.
failed -- when a non-blocking connection has failed.
nil -- if arg is a process name and no such process exists.
PROCESS may be a process, a buffer, the name of a process, or
nil, indicating the current buffer's process.  */)
  (register Lisp_Object process)
{
  register struct Lisp_Process *p;
  register Lisp_Object status;

  if (STRINGP (process))
    process = Fget_process (process);
  else
    process = get_process (process);

  if (NILP (process))
    return process;

  p = XPROCESS (process);
  if (p->raw_status_new)
    update_status (p);
  status = p->status;
  if (CONSP (status))
    status = XCAR (status);
  if (NETCONN1_P (p) || SERIALCONN1_P (p))
    {
      if (EQ (status, Qexit))
	status = Qclosed;
      else if (EQ (p->command, Qt))
	status = Qstop;
      else if (EQ (status, Qrun))
	status = Qopen;
    }
  return status;
}

DEFUN ("process-exit-status", Fprocess_exit_status, Sprocess_exit_status,
       1, 1, 0,
       doc: /* Return the exit status of PROCESS or the signal number that killed it.
If PROCESS has not yet exited or died, return 0.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  if (XPROCESS (process)->raw_status_new)
    update_status (XPROCESS (process));
  if (CONSP (XPROCESS (process)->status))
    return XCAR (XCDR (XPROCESS (process)->status));
  return make_number (0);
}

DEFUN ("process-id", Fprocess_id, Sprocess_id, 1, 1, 0,
       doc: /* Return the process id of PROCESS.
This is the pid of the external process which PROCESS uses or talks to.
For a network connection, this value is nil.  */)
  (register Lisp_Object process)
{
  /* Assignment to EMACS_INT stops GCC whining about limited range of
     data type.  */
  EMACS_INT pid;

  CHECK_PROCESS (process);
  pid = XPROCESS (process)->pid;
  return (pid ? make_fixnum_or_float (pid) : Qnil);
}

DEFUN ("process-name", Fprocess_name, Sprocess_name, 1, 1, 0,
       doc: /* Return the name of PROCESS, as a string.
This is the name of the program invoked in PROCESS,
possibly modified to make it unique among process names.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->name;
}

DEFUN ("process-command", Fprocess_command, Sprocess_command, 1, 1, 0,
       doc: /* Return the command that was executed to start PROCESS.
This is a list of strings, the first string being the program executed
and the rest of the strings being the arguments given to it.
For a network or serial process, this is nil (process is running) or t
\(process is stopped).  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->command;
}

DEFUN ("process-tty-name", Fprocess_tty_name, Sprocess_tty_name, 1, 1, 0,
       doc: /* Return the name of the terminal PROCESS uses, or nil if none.
This is the terminal that the process itself reads and writes on,
not the name of the pty that Emacs uses to talk with that terminal.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->tty_name;
}

DEFUN ("set-process-buffer", Fset_process_buffer, Sset_process_buffer,
       2, 2, 0,
       doc: /* Set buffer associated with PROCESS to BUFFER (a buffer, or nil).
Return BUFFER.  */)
  (register Lisp_Object process, Lisp_Object buffer)
{
  struct Lisp_Process *p;

  CHECK_PROCESS (process);
  if (!NILP (buffer))
    CHECK_BUFFER (buffer);
  p = XPROCESS (process);
  p->buffer = buffer;
  if (NETCONN1_P (p) || SERIALCONN1_P (p))
    p->childp = Fplist_put (p->childp, QCbuffer, buffer);
  setup_process_coding_systems (process);
  return buffer;
}

DEFUN ("process-buffer", Fprocess_buffer, Sprocess_buffer,
       1, 1, 0,
       doc: /* Return the buffer PROCESS is associated with.
Output from PROCESS is inserted in this buffer unless PROCESS has a filter.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->buffer;
}

DEFUN ("process-mark", Fprocess_mark, Sprocess_mark,
       1, 1, 0,
       doc: /* Return the marker for the end of the last output from PROCESS.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->mark;
}

DEFUN ("set-process-filter", Fset_process_filter, Sset_process_filter,
       2, 2, 0,
       doc: /* Give PROCESS the filter function FILTER; nil means no filter.
A value of t means stop accepting output from the process.

When a process has a filter, its buffer is not used for output.
Instead, each time it does output, the entire string of output is
passed to the filter.

The filter gets two arguments: the process and the string of output.
The string argument is normally a multibyte string, except:
- if the process' input coding system is no-conversion or raw-text,
  it is a unibyte string (the non-converted input), or else
- if `default-enable-multibyte-characters' is nil, it is a unibyte
  string (the result of converting the decoded input multibyte
  string to unibyte with `string-make-unibyte').  */)
  (register Lisp_Object process, Lisp_Object filter)
{
  struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);

  /* Don't signal an error if the process' input file descriptor
     is closed.  This could make debugging Lisp more difficult,
     for example when doing something like

     (setq process (start-process ...))
     (debug)
     (set-process-filter process ...)  */

  if (p->infd >= 0)
    {
      if (EQ (filter, Qt) && !EQ (p->status, Qlisten))
	{
	  FD_CLR (p->infd, &input_wait_mask);
	  FD_CLR (p->infd, &non_keyboard_wait_mask);
	}
      else if (EQ (p->filter, Qt)
	       /* Network or serial process not stopped:  */
	       && !EQ (p->command, Qt))
	{
	  FD_SET (p->infd, &input_wait_mask);
	  FD_SET (p->infd, &non_keyboard_wait_mask);
	}
    }

  p->filter = filter;
  if (NETCONN1_P (p) || SERIALCONN1_P (p))
    p->childp = Fplist_put (p->childp, QCfilter, filter);
  setup_process_coding_systems (process);
  return filter;
}

DEFUN ("process-filter", Fprocess_filter, Sprocess_filter,
       1, 1, 0,
       doc: /* Returns the filter function of PROCESS; nil if none.
See `set-process-filter' for more info on filter functions.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->filter;
}

DEFUN ("set-process-sentinel", Fset_process_sentinel, Sset_process_sentinel,
       2, 2, 0,
       doc: /* Give PROCESS the sentinel SENTINEL; nil for none.
The sentinel is called as a function when the process changes state.
It gets two arguments: the process, and a string describing the change.  */)
  (register Lisp_Object process, Lisp_Object sentinel)
{
  struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);

  p->sentinel = sentinel;
  if (NETCONN1_P (p) || SERIALCONN1_P (p))
    p->childp = Fplist_put (p->childp, QCsentinel, sentinel);
  return sentinel;
}

DEFUN ("process-sentinel", Fprocess_sentinel, Sprocess_sentinel,
       1, 1, 0,
       doc: /* Return the sentinel of PROCESS; nil if none.
See `set-process-sentinel' for more info on sentinels.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->sentinel;
}

DEFUN ("set-process-window-size", Fset_process_window_size,
       Sset_process_window_size, 3, 3, 0,
       doc: /* Tell PROCESS that it has logical window size HEIGHT and WIDTH.  */)
  (register Lisp_Object process, Lisp_Object height, Lisp_Object width)
{
  CHECK_PROCESS (process);
  CHECK_NATNUM (height);
  CHECK_NATNUM (width);

  if (XPROCESS (process)->infd < 0
      || set_window_size (XPROCESS (process)->infd,
			  XINT (height), XINT (width)) <= 0)
    return Qnil;
  else
    return Qt;
}

DEFUN ("set-process-inherit-coding-system-flag",
       Fset_process_inherit_coding_system_flag,
       Sset_process_inherit_coding_system_flag, 2, 2, 0,
       doc: /* Determine whether buffer of PROCESS will inherit coding-system.
If the second argument FLAG is non-nil, then the variable
`buffer-file-coding-system' of the buffer associated with PROCESS
will be bound to the value of the coding system used to decode
the process output.

This is useful when the coding system specified for the process buffer
leaves either the character code conversion or the end-of-line conversion
unspecified, or if the coding system used to decode the process output
is more appropriate for saving the process buffer.

Binding the variable `inherit-process-coding-system' to non-nil before
starting the process is an alternative way of setting the inherit flag
for the process which will run.

This function returns FLAG.  */)
  (register Lisp_Object process, Lisp_Object flag)
{
  CHECK_PROCESS (process);
  XPROCESS (process)->inherit_coding_system_flag = !NILP (flag);
  return flag;
}

DEFUN ("set-process-query-on-exit-flag",
       Fset_process_query_on_exit_flag, Sset_process_query_on_exit_flag,
       2, 2, 0,
       doc: /* Specify if query is needed for PROCESS when Emacs is exited.
If the second argument FLAG is non-nil, Emacs will query the user before
exiting or killing a buffer if PROCESS is running.  This function
returns FLAG.  */)
  (register Lisp_Object process, Lisp_Object flag)
{
  CHECK_PROCESS (process);
  XPROCESS (process)->kill_without_query = NILP (flag);
  return flag;
}

DEFUN ("process-query-on-exit-flag",
       Fprocess_query_on_exit_flag, Sprocess_query_on_exit_flag,
       1, 1, 0,
       doc: /* Return the current value of query-on-exit flag for PROCESS.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return (XPROCESS (process)->kill_without_query ? Qnil : Qt);
}

#ifdef DATAGRAM_SOCKETS
static Lisp_Object Fprocess_datagram_address (Lisp_Object);
#endif

DEFUN ("process-contact", Fprocess_contact, Sprocess_contact,
       1, 2, 0,
       doc: /* Return the contact info of PROCESS; t for a real child.
For a network or serial connection, the value depends on the optional
KEY arg.  If KEY is nil, value is a cons cell of the form (HOST
SERVICE) for a network connection or (PORT SPEED) for a serial
connection.  If KEY is t, the complete contact information for the
connection is returned, else the specific value for the keyword KEY is
returned.  See `make-network-process' or `make-serial-process' for a
list of keywords.  */)
  (register Lisp_Object process, Lisp_Object key)
{
  Lisp_Object contact;

  CHECK_PROCESS (process);
  contact = XPROCESS (process)->childp;

#ifdef DATAGRAM_SOCKETS
  if (DATAGRAM_CONN_P (process)
      && (EQ (key, Qt) || EQ (key, QCremote)))
    contact = Fplist_put (contact, QCremote,
			  Fprocess_datagram_address (process));
#endif

  if ((!NETCONN_P (process) && !SERIALCONN_P (process)) || EQ (key, Qt))
    return contact;
  if (NILP (key) && NETCONN_P (process))
    return Fcons (Fplist_get (contact, QChost),
		  Fcons (Fplist_get (contact, QCservice), Qnil));
  if (NILP (key) && SERIALCONN_P (process))
    return Fcons (Fplist_get (contact, QCport),
		  Fcons (Fplist_get (contact, QCspeed), Qnil));
  return Fplist_get (contact, key);
}

DEFUN ("process-plist", Fprocess_plist, Sprocess_plist,
       1, 1, 0,
       doc: /* Return the plist of PROCESS.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return XPROCESS (process)->plist;
}

DEFUN ("set-process-plist", Fset_process_plist, Sset_process_plist,
       2, 2, 0,
       doc: /* Replace the plist of PROCESS with PLIST.  Returns PLIST.  */)
  (register Lisp_Object process, Lisp_Object plist)
{
  CHECK_PROCESS (process);
  CHECK_LIST (plist);

  XPROCESS (process)->plist = plist;
  return plist;
}

#if 0 /* Turned off because we don't currently record this info
	 in the process.  Perhaps add it.  */
DEFUN ("process-connection", Fprocess_connection, Sprocess_connection, 1, 1, 0,
       doc: /* Return the connection type of PROCESS.
The value is nil for a pipe, t or `pty' for a pty, or `stream' for
a socket connection.  */)
  (Lisp_Object process)
{
  return XPROCESS (process)->type;
}
#endif

DEFUN ("process-type", Fprocess_type, Sprocess_type, 1, 1, 0,
       doc: /* Return the connection type of PROCESS.
The value is either the symbol `real', `network', or `serial'.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.  */)
  (Lisp_Object process)
{
  Lisp_Object proc;
  proc = get_process (process);
  return XPROCESS (proc)->type;
}

DEFUN ("format-network-address", Fformat_network_address, Sformat_network_address,
       1, 2, 0,
       doc: /* Convert network ADDRESS from internal format to a string.
A 4 or 5 element vector represents an IPv4 address (with port number).
An 8 or 9 element vector represents an IPv6 address (with port number).
If optional second argument OMIT-PORT is non-nil, don't include a port
number in the string, even when present in ADDRESS.
Returns nil if format of ADDRESS is invalid.  */)
  (Lisp_Object address, Lisp_Object omit_port)
{
  if (NILP (address))
    return Qnil;

  if (STRINGP (address))  /* AF_LOCAL */
    return address;

  if (VECTORP (address))  /* AF_INET or AF_INET6 */
    {
      register struct Lisp_Vector *p = XVECTOR (address);
      EMACS_INT size = p->header.size;
      Lisp_Object args[10];
      int nargs, i;

      if (size == 4 || (size == 5 && !NILP (omit_port)))
	{
	  args[0] = build_string ("%d.%d.%d.%d");
	  nargs = 4;
	}
      else if (size == 5)
	{
	  args[0] = build_string ("%d.%d.%d.%d:%d");
	  nargs = 5;
	}
      else if (size == 8 || (size == 9 && !NILP (omit_port)))
	{
	  args[0] = build_string ("%x:%x:%x:%x:%x:%x:%x:%x");
	  nargs = 8;
	}
      else if (size == 9)
	{
	  args[0] = build_string ("[%x:%x:%x:%x:%x:%x:%x:%x]:%d");
	  nargs = 9;
	}
      else
	return Qnil;

      for (i = 0; i < nargs; i++)
	{
	  EMACS_INT element = XINT (p->contents[i]);

	  if (element < 0 || element > 65535)
	    return Qnil;

	  if (nargs <= 5         /* IPv4 */
	      && i < 4           /* host, not port */
	      && element > 255)
	    return Qnil;

	  args[i+1] = p->contents[i];
	}

      return Fformat (nargs+1, args);
    }

  if (CONSP (address))
    {
      Lisp_Object args[2];
      args[0] = build_string ("<Family %d>");
      args[1] = Fcar (address);
      return Fformat (2, args);
    }

  return Qnil;
}

DEFUN ("process-list", Fprocess_list, Sprocess_list, 0, 0, 0,
       doc: /* Return a list of all processes.  */)
  (void)
{
  return Fmapcar (Qcdr, Vprocess_alist);
}

/* Starting asynchronous inferior processes.  */

static Lisp_Object start_process_unwind (Lisp_Object proc);

DEFUN ("start-process", Fstart_process, Sstart_process, 3, MANY, 0,
       doc: /* Start a program in a subprocess.  Return the process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.

Process output (both standard output and standard error streams) goes
at end of BUFFER, unless you specify an output stream or filter
function to handle the output.  BUFFER may also be nil, meaning that
this process is not associated with any buffer.

PROGRAM is the program file name.  It is searched for in `exec-path'
(which see).  If nil, just associate a pty with the buffer.  Remaining
arguments are strings to give program as arguments.

If you want to separate standard output from standard error, invoke
the command through a shell and redirect one of them using the shell
syntax.

usage: (start-process NAME BUFFER PROGRAM &rest PROGRAM-ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object buffer, name, program, proc, current_dir, tem;
  register unsigned char **new_argv;
  ptrdiff_t i;
  int count = SPECPDL_INDEX ();

  buffer = args[1];
  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);

  /* Make sure that the child will be able to chdir to the current
     buffer's current directory, or its unhandled equivalent.  We
     can't just have the child check for an error when it does the
     chdir, since it's in a vfork.

     We have to GCPRO around this because Fexpand_file_name and
     Funhandled_file_name_directory might call a file name handling
     function.  The argument list is protected by the caller, so all
     we really have to worry about is buffer.  */
  {
    struct gcpro gcpro1, gcpro2;

    current_dir = BVAR (current_buffer, directory);

    GCPRO2 (buffer, current_dir);

    current_dir = Funhandled_file_name_directory (current_dir);
    if (NILP (current_dir))
      /* If the file name handler says that current_dir is unreachable, use
	 a sensible default. */
      current_dir = build_string ("~/");
    current_dir = expand_and_dir_to_file (current_dir, Qnil);
    if (NILP (Ffile_accessible_directory_p (current_dir)))
      report_file_error ("Setting current directory",
			 Fcons (BVAR (current_buffer, directory), Qnil));

    UNGCPRO;
  }

  name = args[0];
  CHECK_STRING (name);

  program = args[2];

  if (!NILP (program))
    CHECK_STRING (program);

  proc = make_process (name);
  /* If an error occurs and we can't start the process, we want to
     remove it from the process list.  This means that each error
     check in create_process doesn't need to call remove_process
     itself; it's all taken care of here.  */
  record_unwind_protect (start_process_unwind, proc);

  XPROCESS (proc)->childp = Qt;
  XPROCESS (proc)->plist = Qnil;
  XPROCESS (proc)->type = Qreal;
  XPROCESS (proc)->buffer = buffer;
  XPROCESS (proc)->sentinel = Qnil;
  XPROCESS (proc)->filter = Qnil;
  XPROCESS (proc)->command = Flist (nargs - 2, args + 2);

#ifdef HAVE_GNUTLS
  /* AKA GNUTLS_INITSTAGE(proc).  */
  XPROCESS (proc)->gnutls_initstage = GNUTLS_STAGE_EMPTY;
  XPROCESS (proc)->gnutls_cred_type = Qnil;
#endif

#ifdef ADAPTIVE_READ_BUFFERING
  XPROCESS (proc)->adaptive_read_buffering
    = (NILP (Vprocess_adaptive_read_buffering) ? 0
       : EQ (Vprocess_adaptive_read_buffering, Qt) ? 1 : 2);
#endif

  /* Make the process marker point into the process buffer (if any).  */
  if (BUFFERP (buffer))
    set_marker_both (XPROCESS (proc)->mark, buffer,
		     BUF_ZV (XBUFFER (buffer)),
		     BUF_ZV_BYTE (XBUFFER (buffer)));

  {
    /* Decide coding systems for communicating with the process.  Here
       we don't setup the structure coding_system nor pay attention to
       unibyte mode.  They are done in create_process.  */

    /* Qt denotes we have not yet called Ffind_operation_coding_system.  */
    Lisp_Object coding_systems = Qt;
    Lisp_Object val, *args2;
    struct gcpro gcpro1, gcpro2;

    val = Vcoding_system_for_read;
    if (NILP (val))
      {
	args2 = (Lisp_Object *) alloca ((nargs + 1) * sizeof *args2);
	args2[0] = Qstart_process;
	for (i = 0; i < nargs; i++) args2[i + 1] = args[i];
	GCPRO2 (proc, current_dir);
	if (!NILP (program))
	  coding_systems = Ffind_operation_coding_system (nargs + 1, args2);
	UNGCPRO;
	if (CONSP (coding_systems))
	  val = XCAR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCAR (Vdefault_process_coding_system);
      }
    XPROCESS (proc)->decode_coding_system = val;

    val = Vcoding_system_for_write;
    if (NILP (val))
      {
	if (EQ (coding_systems, Qt))
	  {
	    args2 = (Lisp_Object *) alloca ((nargs + 1) * sizeof *args2);
	    args2[0] = Qstart_process;
	    for (i = 0; i < nargs; i++) args2[i + 1] = args[i];
	    GCPRO2 (proc, current_dir);
	    if (!NILP (program))
	      coding_systems = Ffind_operation_coding_system (nargs + 1, args2);
	    UNGCPRO;
	  }
	if (CONSP (coding_systems))
	  val = XCDR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCDR (Vdefault_process_coding_system);
      }
    XPROCESS (proc)->encode_coding_system = val;
    /* Note: At this moment, the above coding system may leave
       text-conversion or eol-conversion unspecified.  They will be
       decided after we read output from the process and decode it by
       some coding system, or just before we actually send a text to
       the process.  */
  }


  XPROCESS (proc)->decoding_buf = empty_unibyte_string;
  XPROCESS (proc)->decoding_carryover = 0;
  XPROCESS (proc)->encoding_buf = empty_unibyte_string;

  XPROCESS (proc)->inherit_coding_system_flag
    = !(NILP (buffer) || !inherit_process_coding_system);

  if (!NILP (program))
    {
      /* If program file name is not absolute, search our path for it.
	 Put the name we will really use in TEM.  */
      if (!IS_DIRECTORY_SEP (SREF (program, 0))
	  && !(SCHARS (program) > 1
	       && IS_DEVICE_SEP (SREF (program, 1))))
	{
	  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

	  tem = Qnil;
	  GCPRO4 (name, program, buffer, current_dir);
	  openp (Vexec_path, program, Vexec_suffixes, &tem, make_number (X_OK));
	  UNGCPRO;
	  if (NILP (tem))
	    report_file_error ("Searching for program", Fcons (program, Qnil));
	  tem = Fexpand_file_name (tem, Qnil);
	}
      else
	{
	  if (!NILP (Ffile_directory_p (program)))
	    error ("Specified program for new process is a directory");
	  tem = program;
	}

      /* If program file name starts with /: for quoting a magic name,
	 discard that.  */
      if (SBYTES (tem) > 2 && SREF (tem, 0) == '/'
	  && SREF (tem, 1) == ':')
	tem = Fsubstring (tem, make_number (2), Qnil);

      {
	Lisp_Object arg_encoding = Qnil;
	struct gcpro gcpro1;
	GCPRO1 (tem);

	/* Encode the file name and put it in NEW_ARGV.
	   That's where the child will use it to execute the program.  */
	tem = Fcons (ENCODE_FILE (tem), Qnil);

	/* Here we encode arguments by the coding system used for sending
	   data to the process.  We don't support using different coding
	   systems for encoding arguments and for encoding data sent to the
	   process.  */

	for (i = 3; i < nargs; i++)
	  {
	    tem = Fcons (args[i], tem);
	    CHECK_STRING (XCAR (tem));
	    if (STRING_MULTIBYTE (XCAR (tem)))
	      {
		if (NILP (arg_encoding))
		  arg_encoding = (complement_process_encoding_system
				  (XPROCESS (proc)->encode_coding_system));
		XSETCAR (tem,
			 code_convert_string_norecord
			 (XCAR (tem), arg_encoding, 1));
	      }
	  }

	UNGCPRO;
      }

      /* Now that everything is encoded we can collect the strings into
	 NEW_ARGV.  */
      new_argv = (unsigned char **) alloca ((nargs - 1) * sizeof (char *));
      new_argv[nargs - 2] = 0;

      for (i = nargs - 2; i-- != 0; )
	{
	  new_argv[i] = SDATA (XCAR (tem));
	  tem = XCDR (tem);
	}

      create_process (proc, (char **) new_argv, current_dir);
    }
  else
    create_pty (proc);

  return unbind_to (count, proc);
}

/* This function is the unwind_protect form for Fstart_process.  If
   PROC doesn't have its pid set, then we know someone has signaled
   an error and the process wasn't started successfully, so we should
   remove it from the process list.  */
static Lisp_Object
start_process_unwind (Lisp_Object proc)
{
  if (!PROCESSP (proc))
    abort ();

  /* Was PROC started successfully?
     -2 is used for a pty with no process, eg for gdb.  */
  if (XPROCESS (proc)->pid <= 0 && XPROCESS (proc)->pid != -2)
    remove_process (proc);

  return Qnil;
}

static void
create_process_1 (struct atimer *timer)
{
  /* Nothing to do.  */
}


static void
create_process (Lisp_Object process, char **new_argv, Lisp_Object current_dir)
{
  int inchannel, outchannel;
  pid_t pid;
  int sv[2];
#if !defined (WINDOWSNT) && defined (FD_CLOEXEC)
  int wait_child_setup[2];
#endif
  sigset_t procmask;
  sigset_t blocked;
  struct sigaction sigint_action;
  struct sigaction sigquit_action;
  struct sigaction sigpipe_action;
#ifdef AIX
  struct sigaction sighup_action;
#endif
  /* Use volatile to protect variables from being clobbered by longjmp.  */
  volatile int forkin, forkout;
  volatile int pty_flag = 0;
#ifndef USE_CRT_DLL
  extern char **environ;
#endif

  inchannel = outchannel = -1;

#ifdef HAVE_PTYS
  if (!NILP (Vprocess_connection_type))
    outchannel = inchannel = allocate_pty ();

  if (inchannel >= 0)
    {
#if ! defined (USG) || defined (USG_SUBTTY_WORKS)
      /* On most USG systems it does not work to open the pty's tty here,
	 then close it and reopen it in the child.  */
#ifdef O_NOCTTY
      /* Don't let this terminal become our controlling terminal
	 (in case we don't have one).  */
      forkout = forkin = emacs_open (pty_name, O_RDWR | O_NOCTTY, 0);
#else
      forkout = forkin = emacs_open (pty_name, O_RDWR, 0);
#endif
      if (forkin < 0)
	report_file_error ("Opening pty", Qnil);
#else
      forkin = forkout = -1;
#endif /* not USG, or USG_SUBTTY_WORKS */
      pty_flag = 1;
    }
  else
#endif /* HAVE_PTYS */
    {
      int tem;
      tem = pipe (sv);
      if (tem < 0)
	report_file_error ("Creating pipe", Qnil);
      inchannel = sv[0];
      forkout = sv[1];
      tem = pipe (sv);
      if (tem < 0)
	{
	  emacs_close (inchannel);
	  emacs_close (forkout);
	  report_file_error ("Creating pipe", Qnil);
	}
      outchannel = sv[1];
      forkin = sv[0];
    }

#if !defined (WINDOWSNT) && defined (FD_CLOEXEC)
    {
      int tem;

      tem = pipe (wait_child_setup);
      if (tem < 0)
	report_file_error ("Creating pipe", Qnil);
      tem = fcntl (wait_child_setup[1], F_GETFD, 0);
      if (tem >= 0)
	tem = fcntl (wait_child_setup[1], F_SETFD, tem | FD_CLOEXEC);
      if (tem < 0)
	{
	  emacs_close (wait_child_setup[0]);
	  emacs_close (wait_child_setup[1]);
	  report_file_error ("Setting file descriptor flags", Qnil);
	}
    }
#endif

#ifdef O_NONBLOCK
  fcntl (inchannel, F_SETFL, O_NONBLOCK);
  fcntl (outchannel, F_SETFL, O_NONBLOCK);
#else
#ifdef O_NDELAY
  fcntl (inchannel, F_SETFL, O_NDELAY);
  fcntl (outchannel, F_SETFL, O_NDELAY);
#endif
#endif

  /* Record this as an active process, with its channels.
     As a result, child_setup will close Emacs's side of the pipes.  */
  chan_process[inchannel] = process;
  XPROCESS (process)->infd = inchannel;
  XPROCESS (process)->outfd = outchannel;

  /* Previously we recorded the tty descriptor used in the subprocess.
     It was only used for getting the foreground tty process, so now
     we just reopen the device (see emacs_get_tty_pgrp) as this is
     more portable (see USG_SUBTTY_WORKS above).  */

  XPROCESS (process)->pty_flag = pty_flag;
  XPROCESS (process)->status = Qrun;

  /* Delay interrupts until we have a chance to store
     the new fork's pid in its process structure */
  sigemptyset (&blocked);
#ifdef SIGCHLD
  sigaddset (&blocked, SIGCHLD);
#endif
#ifdef HAVE_WORKING_VFORK
  /* On many hosts (e.g. Solaris 2.4), if a vforked child calls `signal',
     this sets the parent's signal handlers as well as the child's.
     So delay all interrupts whose handlers the child might munge,
     and record the current handlers so they can be restored later.  */
  sigaddset (&blocked, SIGINT );  sigaction (SIGINT , 0, &sigint_action );
  sigaddset (&blocked, SIGQUIT);  sigaction (SIGQUIT, 0, &sigquit_action);
  sigaddset (&blocked, SIGPIPE);  sigaction (SIGPIPE, 0, &sigpipe_action);
#ifdef AIX
  sigaddset (&blocked, SIGHUP );  sigaction (SIGHUP , 0, &sighup_action );
#endif
#endif /* HAVE_WORKING_VFORK */
  pthread_sigmask (SIG_BLOCK, &blocked, &procmask);

  FD_SET (inchannel, &input_wait_mask);
  FD_SET (inchannel, &non_keyboard_wait_mask);
  if (inchannel > max_process_desc)
    max_process_desc = inchannel;

  /* Until we store the proper pid, enable sigchld_handler
     to recognize an unknown pid as standing for this process.
     It is very important not to let this `marker' value stay
     in the table after this function has returned; if it does
     it might cause call-process to hang and subsequent asynchronous
     processes to get their return values scrambled.  */
  XPROCESS (process)->pid = -1;

  /* This must be called after the above line because it may signal an
     error. */
  setup_process_coding_systems (process);

  BLOCK_INPUT;

  {
    /* child_setup must clobber environ on systems with true vfork.
       Protect it from permanent change.  */
    char **save_environ = environ;
    volatile Lisp_Object encoded_current_dir = ENCODE_FILE (current_dir);

#ifndef WINDOWSNT
    pid = vfork ();
    if (pid == 0)
#endif /* not WINDOWSNT */
      {
	int xforkin = forkin;
	int xforkout = forkout;

#if 0 /* This was probably a mistake--it duplicates code later on,
	 but fails to handle all the cases.  */
	/* Make sure SIGCHLD is not blocked in the child.  */
	sigsetmask (SIGEMPTYMASK);
#endif

	/* Make the pty be the controlling terminal of the process.  */
#ifdef HAVE_PTYS
	/* First, disconnect its current controlling terminal.  */
#ifdef HAVE_SETSID
	/* We tried doing setsid only if pty_flag, but it caused
	   process_set_signal to fail on SGI when using a pipe.  */
	setsid ();
	/* Make the pty's terminal the controlling terminal.  */
	if (pty_flag && xforkin >= 0)
	  {
#ifdef TIOCSCTTY
	    /* We ignore the return value
	       because faith@cs.unc.edu says that is necessary on Linux.  */
	    ioctl (xforkin, TIOCSCTTY, 0);
#endif
	  }
#else /* not HAVE_SETSID */
#ifdef USG
	/* It's very important to call setpgrp here and no time
	   afterwards.  Otherwise, we lose our controlling tty which
	   is set when we open the pty. */
	setpgrp ();
#endif /* USG */
#endif /* not HAVE_SETSID */
#if defined (LDISC1)
	if (pty_flag && xforkin >= 0)
	  {
	    struct termios t;
	    tcgetattr (xforkin, &t);
	    t.c_lflag = LDISC1;
	    if (tcsetattr (xforkin, TCSANOW, &t) < 0)
	      emacs_write (1, "create_process/tcsetattr LDISC1 failed\n", 39);
	  }
#else
#if defined (NTTYDISC) && defined (TIOCSETD)
	if (pty_flag && xforkin >= 0)
	  {
	    /* Use new line discipline.  */
	    int ldisc = NTTYDISC;
	    ioctl (xforkin, TIOCSETD, &ldisc);
	  }
#endif
#endif
#ifdef TIOCNOTTY
	/* In 4.3BSD, the TIOCSPGRP bug has been fixed, and now you
	   can do TIOCSPGRP only to the process's controlling tty.  */
	if (pty_flag)
	  {
	    /* I wonder: would just ioctl (0, TIOCNOTTY, 0) work here?
	       I can't test it since I don't have 4.3.  */
	    int j = emacs_open ("/dev/tty", O_RDWR, 0);
	    if (j >= 0)
	      {
		ioctl (j, TIOCNOTTY, 0);
		emacs_close (j);
	      }
#ifndef USG
	    /* In order to get a controlling terminal on some versions
	       of BSD, it is necessary to put the process in pgrp 0
	       before it opens the terminal.  */
#ifdef HAVE_SETPGID
	    setpgid (0, 0);
#else
	    setpgrp (0, 0);
#endif
#endif
	  }
#endif /* TIOCNOTTY */

#if !defined (DONT_REOPEN_PTY)
/*** There is a suggestion that this ought to be a
     conditional on TIOCSPGRP,
     or !(defined (HAVE_SETSID) && defined (TIOCSCTTY)).
     Trying the latter gave the wrong results on Debian GNU/Linux 1.1;
     that system does seem to need this code, even though
     both HAVE_SETSID and TIOCSCTTY are defined.  */
	/* Now close the pty (if we had it open) and reopen it.
	   This makes the pty the controlling terminal of the subprocess.  */
	if (pty_flag)
	  {

	    /* I wonder if emacs_close (emacs_open (pty_name, ...))
	       would work?  */
	    if (xforkin >= 0)
	      emacs_close (xforkin);
	    xforkout = xforkin = emacs_open (pty_name, O_RDWR, 0);

	    if (xforkin < 0)
	      {
		emacs_write (1, "Couldn't open the pty terminal ", 31);
		emacs_write (1, pty_name, strlen (pty_name));
		emacs_write (1, "\n", 1);
		_exit (1);
	      }

	  }
#endif /* not DONT_REOPEN_PTY */

#ifdef SETUP_SLAVE_PTY
	if (pty_flag)
	  {
	    SETUP_SLAVE_PTY;
	  }
#endif /* SETUP_SLAVE_PTY */
#ifdef AIX
	/* On AIX, we've disabled SIGHUP above once we start a child on a pty.
	   Now reenable it in the child, so it will die when we want it to.  */
	if (pty_flag)
	  signal (SIGHUP, SIG_DFL);
#endif
#endif /* HAVE_PTYS */

	signal (SIGINT, SIG_DFL);
	signal (SIGQUIT, SIG_DFL);
	/* GConf causes us to ignore SIGPIPE, make sure it is restored
	   in the child.  */
	signal (SIGPIPE, SIG_DFL);

	/* Stop blocking signals in the child.  */
	pthread_sigmask (SIG_SETMASK, &procmask, 0);

	if (pty_flag)
	  child_setup_tty (xforkout);
#ifdef WINDOWSNT
	pid = child_setup (xforkin, xforkout, xforkout,
			   new_argv, 1, encoded_current_dir);
#else  /* not WINDOWSNT */
#ifdef FD_CLOEXEC
	emacs_close (wait_child_setup[0]);
#endif
	child_setup (xforkin, xforkout, xforkout,
		     new_argv, 1, encoded_current_dir);
#endif /* not WINDOWSNT */
      }
    environ = save_environ;
  }

  UNBLOCK_INPUT;

  /* This runs in the Emacs process.  */
  if (pid < 0)
    {
      if (forkin >= 0)
	emacs_close (forkin);
      if (forkin != forkout && forkout >= 0)
	emacs_close (forkout);
    }
  else
    {
      /* vfork succeeded.  */
      XPROCESS (process)->pid = pid;

#ifdef WINDOWSNT
      register_child (pid, inchannel);
#endif /* WINDOWSNT */

      /* If the subfork execv fails, and it exits,
	 this close hangs.  I don't know why.
	 So have an interrupt jar it loose.  */
      {
	struct atimer *timer;
	EMACS_TIME offset;

	stop_polling ();
	EMACS_SET_SECS_USECS (offset, 1, 0);
	timer = start_atimer (ATIMER_RELATIVE, offset, create_process_1, 0);

	if (forkin >= 0)
	  emacs_close (forkin);

	cancel_atimer (timer);
	start_polling ();
      }

      if (forkin != forkout && forkout >= 0)
	emacs_close (forkout);

#ifdef HAVE_PTYS
      if (pty_flag)
	XPROCESS (process)->tty_name = build_string (pty_name);
      else
#endif
	XPROCESS (process)->tty_name = Qnil;

#if !defined (WINDOWSNT) && defined (FD_CLOEXEC)
      /* Wait for child_setup to complete in case that vfork is
	 actually defined as fork.  The descriptor wait_child_setup[1]
	 of a pipe is closed at the child side either by close-on-exec
	 on successful execvp or the _exit call in child_setup.  */
      {
	char dummy;

	emacs_close (wait_child_setup[1]);
	emacs_read (wait_child_setup[0], &dummy, 1);
	emacs_close (wait_child_setup[0]);
      }
#endif
    }

  /* Restore the signal state whether vfork succeeded or not.
     (We will signal an error, below, if it failed.)  */
#ifdef HAVE_WORKING_VFORK
  /* Restore the parent's signal handlers.  */
  sigaction (SIGINT, &sigint_action, 0);
  sigaction (SIGQUIT, &sigquit_action, 0);
  sigaction (SIGPIPE, &sigpipe_action, 0);
#ifdef AIX
  sigaction (SIGHUP, &sighup_action, 0);
#endif
#endif /* HAVE_WORKING_VFORK */
  /* Stop blocking signals in the parent.  */
  pthread_sigmask (SIG_SETMASK, &procmask, 0);

  /* Now generate the error if vfork failed.  */
  if (pid < 0)
    report_file_error ("Doing vfork", Qnil);
}

void
create_pty (Lisp_Object process)
{
  int inchannel, outchannel;
  int pty_flag = 0;

  inchannel = outchannel = -1;

#ifdef HAVE_PTYS
  if (!NILP (Vprocess_connection_type))
    outchannel = inchannel = allocate_pty ();

  if (inchannel >= 0)
    {
#if ! defined (USG) || defined (USG_SUBTTY_WORKS)
      /* On most USG systems it does not work to open the pty's tty here,
	 then close it and reopen it in the child.  */
#ifdef O_NOCTTY
      /* Don't let this terminal become our controlling terminal
	 (in case we don't have one).  */
      int forkout = emacs_open (pty_name, O_RDWR | O_NOCTTY, 0);
#else
      int forkout = emacs_open (pty_name, O_RDWR, 0);
#endif
      if (forkout < 0)
	report_file_error ("Opening pty", Qnil);
#if defined (DONT_REOPEN_PTY)
      /* In the case that vfork is defined as fork, the parent process
	 (Emacs) may send some data before the child process completes
	 tty options setup.  So we setup tty before forking.  */
      child_setup_tty (forkout);
#endif /* DONT_REOPEN_PTY */
#endif /* not USG, or USG_SUBTTY_WORKS */
      pty_flag = 1;
    }
#endif /* HAVE_PTYS */

#ifdef O_NONBLOCK
  fcntl (inchannel, F_SETFL, O_NONBLOCK);
  fcntl (outchannel, F_SETFL, O_NONBLOCK);
#else
#ifdef O_NDELAY
  fcntl (inchannel, F_SETFL, O_NDELAY);
  fcntl (outchannel, F_SETFL, O_NDELAY);
#endif
#endif

  /* Record this as an active process, with its channels.
     As a result, child_setup will close Emacs's side of the pipes.  */
  chan_process[inchannel] = process;
  XPROCESS (process)->infd = inchannel;
  XPROCESS (process)->outfd = outchannel;

  /* Previously we recorded the tty descriptor used in the subprocess.
     It was only used for getting the foreground tty process, so now
     we just reopen the device (see emacs_get_tty_pgrp) as this is
     more portable (see USG_SUBTTY_WORKS above).  */

  XPROCESS (process)->pty_flag = pty_flag;
  XPROCESS (process)->status = Qrun;
  setup_process_coding_systems (process);

  FD_SET (inchannel, &input_wait_mask);
  FD_SET (inchannel, &non_keyboard_wait_mask);
  if (inchannel > max_process_desc)
    max_process_desc = inchannel;

  XPROCESS (process)->pid = -2;
#ifdef HAVE_PTYS
  if (pty_flag)
    XPROCESS (process)->tty_name = build_string (pty_name);
  else
#endif
    XPROCESS (process)->tty_name = Qnil;
}


/* Convert an internal struct sockaddr to a lisp object (vector or string).
   The address family of sa is not included in the result.  */

static Lisp_Object
conv_sockaddr_to_lisp (struct sockaddr *sa, int len)
{
  Lisp_Object address;
  int i;
  unsigned char *cp;
  register struct Lisp_Vector *p;

  /* Workaround for a bug in getsockname on BSD: Names bound to
     sockets in the UNIX domain are inaccessible; getsockname returns
     a zero length name.  */
  if (len < offsetof (struct sockaddr, sa_family) + sizeof (sa->sa_family))
    return empty_unibyte_string;

  switch (sa->sa_family)
    {
    case AF_INET:
      {
	struct sockaddr_in *sin = (struct sockaddr_in *) sa;
	len = sizeof (sin->sin_addr) + 1;
	address = Fmake_vector (make_number (len), Qnil);
	p = XVECTOR (address);
	p->contents[--len] = make_number (ntohs (sin->sin_port));
	cp = (unsigned char *) &sin->sin_addr;
	break;
      }
#ifdef AF_INET6
    case AF_INET6:
      {
	struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *) sa;
	uint16_t *ip6 = (uint16_t *) &sin6->sin6_addr;
	len = sizeof (sin6->sin6_addr)/2 + 1;
	address = Fmake_vector (make_number (len), Qnil);
	p = XVECTOR (address);
	p->contents[--len] = make_number (ntohs (sin6->sin6_port));
	for (i = 0; i < len; i++)
	  p->contents[i] = make_number (ntohs (ip6[i]));
	return address;
      }
#endif
#ifdef HAVE_LOCAL_SOCKETS
    case AF_LOCAL:
      {
	struct sockaddr_un *sockun = (struct sockaddr_un *) sa;
	for (i = 0; i < sizeof (sockun->sun_path); i++)
	  if (sockun->sun_path[i] == 0)
	    break;
	return make_unibyte_string (sockun->sun_path, i);
      }
#endif
    default:
      len -= offsetof (struct sockaddr, sa_family) + sizeof (sa->sa_family);
      address = Fcons (make_number (sa->sa_family),
		       Fmake_vector (make_number (len), Qnil));
      p = XVECTOR (XCDR (address));
      cp = (unsigned char *) &sa->sa_family + sizeof (sa->sa_family);
      break;
    }

  i = 0;
  while (i < len)
    p->contents[i++] = make_number (*cp++);

  return address;
}


/* Get family and required size for sockaddr structure to hold ADDRESS.  */

static int
get_lisp_to_sockaddr_size (Lisp_Object address, int *familyp)
{
  register struct Lisp_Vector *p;

  if (VECTORP (address))
    {
      p = XVECTOR (address);
      if (p->header.size == 5)
	{
	  *familyp = AF_INET;
	  return sizeof (struct sockaddr_in);
	}
#ifdef AF_INET6
      else if (p->header.size == 9)
	{
	  *familyp = AF_INET6;
	  return sizeof (struct sockaddr_in6);
	}
#endif
    }
#ifdef HAVE_LOCAL_SOCKETS
  else if (STRINGP (address))
    {
      *familyp = AF_LOCAL;
      return sizeof (struct sockaddr_un);
    }
#endif
  else if (CONSP (address) && INTEGERP (XCAR (address)) && VECTORP (XCDR (address)))
    {
      struct sockaddr *sa;
      *familyp = XINT (XCAR (address));
      p = XVECTOR (XCDR (address));
      return p->header.size + sizeof (sa->sa_family);
    }
  return 0;
}

/* Convert an address object (vector or string) to an internal sockaddr.

   The address format has been basically validated by
   get_lisp_to_sockaddr_size, but this does not mean FAMILY is valid;
   it could have come from user data.  So if FAMILY is not valid,
   we return after zeroing *SA.  */

static void
conv_lisp_to_sockaddr (int family, Lisp_Object address, struct sockaddr *sa, int len)
{
  register struct Lisp_Vector *p;
  register unsigned char *cp = NULL;
  register int i;

  memset (sa, 0, len);

  if (VECTORP (address))
    {
      p = XVECTOR (address);
      if (family == AF_INET)
	{
	  struct sockaddr_in *sin = (struct sockaddr_in *) sa;
	  len = sizeof (sin->sin_addr) + 1;
	  i = XINT (p->contents[--len]);
	  sin->sin_port = htons (i);
	  cp = (unsigned char *)&sin->sin_addr;
	  sa->sa_family = family;
	}
#ifdef AF_INET6
      else if (family == AF_INET6)
	{
	  struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *) sa;
	  uint16_t *ip6 = (uint16_t *)&sin6->sin6_addr;
	  len = sizeof (sin6->sin6_addr) + 1;
	  i = XINT (p->contents[--len]);
	  sin6->sin6_port = htons (i);
	  for (i = 0; i < len; i++)
	    if (INTEGERP (p->contents[i]))
	      {
		int j = XFASTINT (p->contents[i]) & 0xffff;
		ip6[i] = ntohs (j);
	      }
	  sa->sa_family = family;
	  return;
	}
#endif
      else
	return;
    }
  else if (STRINGP (address))
    {
#ifdef HAVE_LOCAL_SOCKETS
      if (family == AF_LOCAL)
	{
	  struct sockaddr_un *sockun = (struct sockaddr_un *) sa;
	  cp = SDATA (address);
	  for (i = 0; i < sizeof (sockun->sun_path) && *cp; i++)
	    sockun->sun_path[i] = *cp++;
	  sa->sa_family = family;
	}
#endif
      return;
    }
  else
    {
      p = XVECTOR (XCDR (address));
      cp = (unsigned char *)sa + sizeof (sa->sa_family);
    }

  for (i = 0; i < len; i++)
    if (INTEGERP (p->contents[i]))
      *cp++ = XFASTINT (p->contents[i]) & 0xff;
}

#ifdef DATAGRAM_SOCKETS
DEFUN ("process-datagram-address", Fprocess_datagram_address, Sprocess_datagram_address,
       1, 1, 0,
       doc: /* Get the current datagram address associated with PROCESS.  */)
  (Lisp_Object process)
{
  int channel;

  CHECK_PROCESS (process);

  if (!DATAGRAM_CONN_P (process))
    return Qnil;

  channel = XPROCESS (process)->infd;
  return conv_sockaddr_to_lisp (datagram_address[channel].sa,
				datagram_address[channel].len);
}

DEFUN ("set-process-datagram-address", Fset_process_datagram_address, Sset_process_datagram_address,
       2, 2, 0,
       doc: /* Set the datagram address for PROCESS to ADDRESS.
Returns nil upon error setting address, ADDRESS otherwise.  */)
  (Lisp_Object process, Lisp_Object address)
{
  int channel;
  int family, len;

  CHECK_PROCESS (process);

  if (!DATAGRAM_CONN_P (process))
    return Qnil;

  channel = XPROCESS (process)->infd;

  len = get_lisp_to_sockaddr_size (address, &family);
  if (datagram_address[channel].len != len)
    return Qnil;
  conv_lisp_to_sockaddr (family, address, datagram_address[channel].sa, len);
  return address;
}
#endif


static const struct socket_options {
  /* The name of this option.  Should be lowercase version of option
     name without SO_ prefix. */
  const char *name;
  /* Option level SOL_... */
  int optlevel;
  /* Option number SO_... */
  int optnum;
  enum { SOPT_UNKNOWN, SOPT_BOOL, SOPT_INT, SOPT_IFNAME, SOPT_LINGER } opttype;
  enum { OPIX_NONE=0, OPIX_MISC=1, OPIX_REUSEADDR=2 } optbit;
} socket_options[] =
  {
#ifdef SO_BINDTODEVICE
    { ":bindtodevice", SOL_SOCKET, SO_BINDTODEVICE, SOPT_IFNAME, OPIX_MISC },
#endif
#ifdef SO_BROADCAST
    { ":broadcast", SOL_SOCKET, SO_BROADCAST, SOPT_BOOL, OPIX_MISC },
#endif
#ifdef SO_DONTROUTE
    { ":dontroute", SOL_SOCKET, SO_DONTROUTE, SOPT_BOOL, OPIX_MISC },
#endif
#ifdef SO_KEEPALIVE
    { ":keepalive", SOL_SOCKET, SO_KEEPALIVE, SOPT_BOOL, OPIX_MISC },
#endif
#ifdef SO_LINGER
    { ":linger", SOL_SOCKET, SO_LINGER, SOPT_LINGER, OPIX_MISC },
#endif
#ifdef SO_OOBINLINE
    { ":oobinline", SOL_SOCKET, SO_OOBINLINE, SOPT_BOOL, OPIX_MISC },
#endif
#ifdef SO_PRIORITY
    { ":priority", SOL_SOCKET, SO_PRIORITY, SOPT_INT, OPIX_MISC },
#endif
#ifdef SO_REUSEADDR
    { ":reuseaddr", SOL_SOCKET, SO_REUSEADDR, SOPT_BOOL, OPIX_REUSEADDR },
#endif
    { 0, 0, 0, SOPT_UNKNOWN, OPIX_NONE }
  };

/* Set option OPT to value VAL on socket S.

   Returns (1<<socket_options[OPT].optbit) if option is known, 0 otherwise.
   Signals an error if setting a known option fails.
*/

static int
set_socket_option (int s, Lisp_Object opt, Lisp_Object val)
{
  char *name;
  const struct socket_options *sopt;
  int ret = 0;

  CHECK_SYMBOL (opt);

  name = SSDATA (SYMBOL_NAME (opt));
  for (sopt = socket_options; sopt->name; sopt++)
    if (strcmp (name, sopt->name) == 0)
      break;

  switch (sopt->opttype)
    {
    case SOPT_BOOL:
      {
	int optval;
	optval = NILP (val) ? 0 : 1;
	ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			  &optval, sizeof (optval));
	break;
      }

    case SOPT_INT:
      {
	int optval;
	if (INTEGERP (val))
	  optval = XINT (val);
	else
	  error ("Bad option value for %s", name);
	ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			  &optval, sizeof (optval));
	break;
      }

#ifdef SO_BINDTODEVICE
    case SOPT_IFNAME:
      {
	char devname[IFNAMSIZ+1];

	/* This is broken, at least in the Linux 2.4 kernel.
	   To unbind, the arg must be a zero integer, not the empty string.
	   This should work on all systems.   KFS. 2003-09-23.  */
	memset (devname, 0, sizeof devname);
	if (STRINGP (val))
	  {
	    char *arg = SSDATA (val);
	    int len = min (strlen (arg), IFNAMSIZ);
	    memcpy (devname, arg, len);
	  }
	else if (!NILP (val))
	  error ("Bad option value for %s", name);
	ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			  devname, IFNAMSIZ);
	break;
      }
#endif

#ifdef SO_LINGER
    case SOPT_LINGER:
      {
	struct linger linger;

	linger.l_onoff = 1;
	linger.l_linger = 0;
	if (INTEGERP (val))
	  linger.l_linger = XINT (val);
	else
	  linger.l_onoff = NILP (val) ? 0 : 1;
	ret = setsockopt (s, sopt->optlevel, sopt->optnum,
			  &linger, sizeof (linger));
	break;
      }
#endif

    default:
      return 0;
    }

  if (ret < 0)
    report_file_error ("Cannot set network option",
		       Fcons (opt, Fcons (val, Qnil)));
  return (1 << sopt->optbit);
}


DEFUN ("set-network-process-option",
       Fset_network_process_option, Sset_network_process_option,
       3, 4, 0,
       doc: /* For network process PROCESS set option OPTION to value VALUE.
See `make-network-process' for a list of options and values.
If optional fourth arg NO-ERROR is non-nil, don't signal an error if
OPTION is not a supported option, return nil instead; otherwise return t.  */)
  (Lisp_Object process, Lisp_Object option, Lisp_Object value, Lisp_Object no_error)
{
  int s;
  struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);
  if (!NETCONN1_P (p))
    error ("Process is not a network process");

  s = p->infd;
  if (s < 0)
    error ("Process is not running");

  if (set_socket_option (s, option, value))
    {
      p->childp = Fplist_put (p->childp, option, value);
      return Qt;
    }

  if (NILP (no_error))
    error ("Unknown or unsupported option");

  return Qnil;
}


DEFUN ("serial-process-configure",
       Fserial_process_configure,
       Sserial_process_configure,
       0, MANY, 0,
       doc: /* Configure speed, bytesize, etc. of a serial process.

Arguments are specified as keyword/argument pairs.  Attributes that
are not given are re-initialized from the process's current
configuration (available via the function `process-contact') or set to
reasonable default values.  The following arguments are defined:

:process PROCESS
:name NAME
:buffer BUFFER
:port PORT
-- Any of these arguments can be given to identify the process that is
to be configured.  If none of these arguments is given, the current
buffer's process is used.

:speed SPEED -- SPEED is the speed of the serial port in bits per
second, also called baud rate.  Any value can be given for SPEED, but
most serial ports work only at a few defined values between 1200 and
115200, with 9600 being the most common value.  If SPEED is nil, the
serial port is not configured any further, i.e., all other arguments
are ignored.  This may be useful for special serial ports such as
Bluetooth-to-serial converters which can only be configured through AT
commands.  A value of nil for SPEED can be used only when passed
through `make-serial-process' or `serial-term'.

:bytesize BYTESIZE -- BYTESIZE is the number of bits per byte, which
can be 7 or 8.  If BYTESIZE is not given or nil, a value of 8 is used.

:parity PARITY -- PARITY can be nil (don't use parity), the symbol
`odd' (use odd parity), or the symbol `even' (use even parity).  If
PARITY is not given, no parity is used.

:stopbits STOPBITS -- STOPBITS is the number of stopbits used to
terminate a byte transmission.  STOPBITS can be 1 or 2.  If STOPBITS
is not given or nil, 1 stopbit is used.

:flowcontrol FLOWCONTROL -- FLOWCONTROL determines the type of
flowcontrol to be used, which is either nil (don't use flowcontrol),
the symbol `hw' (use RTS/CTS hardware flowcontrol), or the symbol `sw'
\(use XON/XOFF software flowcontrol).  If FLOWCONTROL is not given, no
flowcontrol is used.

`serial-process-configure' is called by `make-serial-process' for the
initial configuration of the serial port.

Examples:

\(serial-process-configure :process "/dev/ttyS0" :speed 1200)

\(serial-process-configure
    :buffer "COM1" :stopbits 1 :parity 'odd :flowcontrol 'hw)

\(serial-process-configure :port "\\\\.\\COM13" :bytesize 7)

usage: (serial-process-configure &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  struct Lisp_Process *p;
  Lisp_Object contact = Qnil;
  Lisp_Object proc = Qnil;
  struct gcpro gcpro1;

  contact = Flist (nargs, args);
  GCPRO1 (contact);

  proc = Fplist_get (contact, QCprocess);
  if (NILP (proc))
    proc = Fplist_get (contact, QCname);
  if (NILP (proc))
    proc = Fplist_get (contact, QCbuffer);
  if (NILP (proc))
    proc = Fplist_get (contact, QCport);
  proc = get_process (proc);
  p = XPROCESS (proc);
  if (!EQ (p->type, Qserial))
    error ("Not a serial process");

  if (NILP (Fplist_get (p->childp, QCspeed)))
    {
      UNGCPRO;
      return Qnil;
    }

  serial_configure (p, contact);

  UNGCPRO;
  return Qnil;
}

/* Used by make-serial-process to recover from errors.  */
static Lisp_Object
make_serial_process_unwind (Lisp_Object proc)
{
  if (!PROCESSP (proc))
    abort ();
  remove_process (proc);
  return Qnil;
}

DEFUN ("make-serial-process", Fmake_serial_process, Smake_serial_process,
       0, MANY, 0,
       doc: /* Create and return a serial port process.

In Emacs, serial port connections are represented by process objects,
so input and output work as for subprocesses, and `delete-process'
closes a serial port connection.  However, a serial process has no
process id, it cannot be signaled, and the status codes are different
from normal processes.

`make-serial-process' creates a process and a buffer, on which you
probably want to use `process-send-string'.  Try \\[serial-term] for
an interactive terminal.  See below for examples.

Arguments are specified as keyword/argument pairs.  The following
arguments are defined:

:port PORT -- (mandatory) PORT is the path or name of the serial port.
For example, this could be "/dev/ttyS0" on Unix.  On Windows, this
could be "COM1", or "\\\\.\\COM10" for ports higher than COM9 (double
the backslashes in strings).

:speed SPEED -- (mandatory) is handled by `serial-process-configure',
which this function calls.

:name NAME -- NAME is the name of the process.  If NAME is not given,
the value of PORT is used.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at the end of that buffer,
unless you specify an output stream or filter function to handle the
output.  If BUFFER is not given, the value of NAME is used.

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:noquery BOOL -- When exiting Emacs, query the user if BOOL is nil and
the process is running.  If BOOL is not given, query before exiting.

:stop BOOL -- Start process in the `stopped' state if BOOL is non-nil.
In the stopped state, a serial process does not accept incoming data,
but you can send outgoing data.  The stopped state is cleared by
`continue-process' and set by `stop-process'.

:filter FILTER -- Install FILTER as the process filter.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:plist PLIST -- Install PLIST as the initial plist of the process.

:bytesize
:parity
:stopbits
:flowcontrol
-- This function calls `serial-process-configure' to handle these
arguments.

The original argument list, possibly modified by later configuration,
is available via the function `process-contact'.

Examples:

\(make-serial-process :port "/dev/ttyS0" :speed 9600)

\(make-serial-process :port "COM1" :speed 115200 :stopbits 2)

\(make-serial-process :port "\\\\.\\COM13" :speed 1200 :bytesize 7 :parity 'odd)

\(make-serial-process :port "/dev/tty.BlueConsole-SPP-1" :speed nil)

usage:  (make-serial-process &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  int fd = -1;
  Lisp_Object proc, contact, port;
  struct Lisp_Process *p;
  struct gcpro gcpro1;
  Lisp_Object name, buffer;
  Lisp_Object tem, val;
  int specpdl_count = -1;

  if (nargs == 0)
    return Qnil;

  contact = Flist (nargs, args);
  GCPRO1 (contact);

  port = Fplist_get (contact, QCport);
  if (NILP (port))
    error ("No port specified");
  CHECK_STRING (port);

  if (NILP (Fplist_member (contact, QCspeed)))
    error (":speed not specified");
  if (!NILP (Fplist_get (contact, QCspeed)))
    CHECK_NUMBER (Fplist_get (contact, QCspeed));

  name = Fplist_get (contact, QCname);
  if (NILP (name))
    name = port;
  CHECK_STRING (name);
  proc = make_process (name);
  specpdl_count = SPECPDL_INDEX ();
  record_unwind_protect (make_serial_process_unwind, proc);
  p = XPROCESS (proc);

  fd = serial_open (SSDATA (port));
  p->infd = fd;
  p->outfd = fd;
  if (fd > max_process_desc)
    max_process_desc = fd;
  chan_process[fd] = proc;

  buffer = Fplist_get (contact, QCbuffer);
  if (NILP (buffer))
    buffer = name;
  buffer = Fget_buffer_create (buffer);
  p->buffer = buffer;

  p->childp = contact;
  p->plist = Fcopy_sequence (Fplist_get (contact, QCplist));
  p->type = Qserial;
  p->sentinel = Fplist_get (contact, QCsentinel);
  p->filter = Fplist_get (contact, QCfilter);
  p->log = Qnil;
  if (tem = Fplist_get (contact, QCnoquery), !NILP (tem))
    p->kill_without_query = 1;
  if (tem = Fplist_get (contact, QCstop), !NILP (tem))
    p->command = Qt;
  p->pty_flag = 0;

  if (!EQ (p->command, Qt))
    {
      FD_SET (fd, &input_wait_mask);
      FD_SET (fd, &non_keyboard_wait_mask);
    }

  if (BUFFERP (buffer))
    {
      set_marker_both (p->mark, buffer,
		       BUF_ZV (XBUFFER (buffer)),
		       BUF_ZV_BYTE (XBUFFER (buffer)));
    }

  tem = Fplist_member (contact, QCcoding);
  if (!NILP (tem) && (!CONSP (tem) || !CONSP (XCDR (tem))))
    tem = Qnil;

  val = Qnil;
  if (!NILP (tem))
    {
      val = XCAR (XCDR (tem));
      if (CONSP (val))
	val = XCAR (val);
    }
  else if (!NILP (Vcoding_system_for_read))
    val = Vcoding_system_for_read;
  else if ((!NILP (buffer) && NILP (BVAR (XBUFFER (buffer), enable_multibyte_characters)))
	   || (NILP (buffer) && NILP (BVAR (&buffer_defaults, enable_multibyte_characters))))
    val = Qnil;
  p->decode_coding_system = val;

  val = Qnil;
  if (!NILP (tem))
    {
      val = XCAR (XCDR (tem));
      if (CONSP (val))
	val = XCDR (val);
    }
  else if (!NILP (Vcoding_system_for_write))
    val = Vcoding_system_for_write;
  else if ((!NILP (buffer) && NILP (BVAR (XBUFFER (buffer), enable_multibyte_characters)))
	   || (NILP (buffer) && NILP (BVAR (&buffer_defaults, enable_multibyte_characters))))
    val = Qnil;
  p->encode_coding_system = val;

  setup_process_coding_systems (proc);
  p->decoding_buf = empty_unibyte_string;
  p->decoding_carryover = 0;
  p->encoding_buf = empty_unibyte_string;
  p->inherit_coding_system_flag
    = !(!NILP (tem) || NILP (buffer) || !inherit_process_coding_system);

  Fserial_process_configure (nargs, args);

  specpdl_ptr = specpdl + specpdl_count;

  UNGCPRO;
  return proc;
}

/* Create a network stream/datagram client/server process.  Treated
   exactly like a normal process when reading and writing.  Primary
   differences are in status display and process deletion.  A network
   connection has no PID; you cannot signal it.  All you can do is
   stop/continue it and deactivate/close it via delete-process */

DEFUN ("make-network-process", Fmake_network_process, Smake_network_process,
       0, MANY, 0,
       doc: /* Create and return a network server or client process.

In Emacs, network connections are represented by process objects, so
input and output work as for subprocesses and `delete-process' closes
a network connection.  However, a network process has no process id,
it cannot be signaled, and the status codes are different from normal
processes.

Arguments are specified as keyword/argument pairs.  The following
arguments are defined:

:name NAME -- NAME is name for process.  It is modified if necessary
to make it unique.

:buffer BUFFER -- BUFFER is the buffer (or buffer-name) to associate
with the process.  Process output goes at end of that buffer, unless
you specify an output stream or filter function to handle the output.
BUFFER may be also nil, meaning that this process is not associated
with any buffer.

:host HOST -- HOST is name of the host to connect to, or its IP
address.  The symbol `local' specifies the local host.  If specified
for a server process, it must be a valid name or address for the local
host, and only clients connecting to that address will be accepted.

:service SERVICE -- SERVICE is name of the service desired, or an
integer specifying a port number to connect to.  If SERVICE is t,
a random port number is selected for the server.  (If Emacs was
compiled with getaddrinfo, a port number can also be specified as a
string, e.g. "80", as well as an integer.  This is not portable.)

:type TYPE -- TYPE is the type of connection.  The default (nil) is a
stream type connection, `datagram' creates a datagram type connection,
`seqpacket' creates a reliable datagram connection.

:family FAMILY -- FAMILY is the address (and protocol) family for the
service specified by HOST and SERVICE.  The default (nil) is to use
whatever address family (IPv4 or IPv6) that is defined for the host
and port number specified by HOST and SERVICE.  Other address families
supported are:
  local -- for a local (i.e. UNIX) address specified by SERVICE.
  ipv4  -- use IPv4 address family only.
  ipv6  -- use IPv6 address family only.

:local ADDRESS -- ADDRESS is the local address used for the connection.
This parameter is ignored when opening a client process. When specified
for a server process, the FAMILY, HOST and SERVICE args are ignored.

:remote ADDRESS -- ADDRESS is the remote partner's address for the
connection.  This parameter is ignored when opening a stream server
process.  For a datagram server process, it specifies the initial
setting of the remote datagram address.  When specified for a client
process, the FAMILY, HOST, and SERVICE args are ignored.

The format of ADDRESS depends on the address family:
- An IPv4 address is represented as an vector of integers [A B C D P]
corresponding to numeric IP address A.B.C.D and port number P.
- A local address is represented as a string with the address in the
local address space.
- An "unsupported family" address is represented by a cons (F . AV)
where F is the family number and AV is a vector containing the socket
address data with one element per address data byte.  Do not rely on
this format in portable code, as it may depend on implementation
defined constants, data sizes, and data structure alignment.

:coding CODING -- If CODING is a symbol, it specifies the coding
system used for both reading and writing for this process.  If CODING
is a cons (DECODING . ENCODING), DECODING is used for reading, and
ENCODING is used for writing.

:nowait BOOL -- If BOOL is non-nil for a stream type client process,
return without waiting for the connection to complete; instead, the
sentinel function will be called with second arg matching "open" (if
successful) or "failed" when the connect completes.  Default is to use
a blocking connect (i.e. wait) for stream type connections.

:noquery BOOL -- Query the user unless BOOL is non-nil, and process is
running when Emacs is exited.

:stop BOOL -- Start process in the `stopped' state if BOOL non-nil.
In the stopped state, a server process does not accept new
connections, and a client process does not handle incoming traffic.
The stopped state is cleared by `continue-process' and set by
`stop-process'.

:filter FILTER -- Install FILTER as the process filter.

:filter-multibyte BOOL -- If BOOL is non-nil, strings given to the
process filter are multibyte, otherwise they are unibyte.
If this keyword is not specified, the strings are multibyte if
the default value of `enable-multibyte-characters' is non-nil.

:sentinel SENTINEL -- Install SENTINEL as the process sentinel.

:log LOG -- Install LOG as the server process log function.  This
function is called when the server accepts a network connection from a
client.  The arguments are SERVER, CLIENT, and MESSAGE, where SERVER
is the server process, CLIENT is the new process for the connection,
and MESSAGE is a string.

:plist PLIST -- Install PLIST as the new process' initial plist.

:server QLEN -- if QLEN is non-nil, create a server process for the
specified FAMILY, SERVICE, and connection type (stream or datagram).
If QLEN is an integer, it is used as the max. length of the server's
pending connection queue (also known as the backlog); the default
queue length is 5.  Default is to create a client process.

The following network options can be specified for this connection:

:broadcast BOOL    -- Allow send and receive of datagram broadcasts.
:dontroute BOOL    -- Only send to directly connected hosts.
:keepalive BOOL    -- Send keep-alive messages on network stream.
:linger BOOL or TIMEOUT -- Send queued messages before closing.
:oobinline BOOL    -- Place out-of-band data in receive data stream.
:priority INT      -- Set protocol defined priority for sent packets.
:reuseaddr BOOL    -- Allow reusing a recently used local address
                      (this is allowed by default for a server process).
:bindtodevice NAME -- bind to interface NAME.  Using this may require
                      special privileges on some systems.

Consult the relevant system programmer's manual pages for more
information on using these options.


A server process will listen for and accept connections from clients.
When a client connection is accepted, a new network process is created
for the connection with the following parameters:

- The client's process name is constructed by concatenating the server
process' NAME and a client identification string.
- If the FILTER argument is non-nil, the client process will not get a
separate process buffer; otherwise, the client's process buffer is a newly
created buffer named after the server process' BUFFER name or process
NAME concatenated with the client identification string.
- The connection type and the process filter and sentinel parameters are
inherited from the server process' TYPE, FILTER and SENTINEL.
- The client process' contact info is set according to the client's
addressing information (typically an IP address and a port number).
- The client process' plist is initialized from the server's plist.

Notice that the FILTER and SENTINEL args are never used directly by
the server process.  Also, the BUFFER argument is not used directly by
the server process, but via the optional :log function, accepted (and
failed) connections may be logged in the server process' buffer.

The original argument list, modified with the actual connection
information, is available via the `process-contact' function.

usage: (make-network-process &rest ARGS)  */)
  (ptrdiff_t nargs, Lisp_Object *args)
{
  Lisp_Object proc;
  Lisp_Object contact;
  struct Lisp_Process *p;
#ifdef HAVE_GETADDRINFO
  struct addrinfo ai, *res, *lres;
  struct addrinfo hints;
  const char *portstring;
  char portbuf[128];
#else /* HAVE_GETADDRINFO */
  struct _emacs_addrinfo
  {
    int ai_family;
    int ai_socktype;
    int ai_protocol;
    int ai_addrlen;
    struct sockaddr *ai_addr;
    struct _emacs_addrinfo *ai_next;
  } ai, *res, *lres;
#endif /* HAVE_GETADDRINFO */
  struct sockaddr_in address_in;
#ifdef HAVE_LOCAL_SOCKETS
  struct sockaddr_un address_un;
#endif
  int port;
  int ret = 0;
  int xerrno = 0;
  int s = -1, outch, inch;
  struct gcpro gcpro1;
  int count = SPECPDL_INDEX ();
  int count1;
  Lisp_Object QCaddress;  /* one of QClocal or QCremote */
  Lisp_Object tem;
  Lisp_Object name, buffer, host, service, address;
  Lisp_Object filter, sentinel;
  int is_non_blocking_client = 0;
  int is_server = 0, backlog = 5;
  int socktype;
  int family = -1;

  if (nargs == 0)
    return Qnil;

  /* Save arguments for process-contact and clone-process.  */
  contact = Flist (nargs, args);
  GCPRO1 (contact);

#ifdef WINDOWSNT
  /* Ensure socket support is loaded if available. */
  init_winsock (TRUE);
#endif

  /* :type TYPE  (nil: stream, datagram */
  tem = Fplist_get (contact, QCtype);
  if (NILP (tem))
    socktype = SOCK_STREAM;
#ifdef DATAGRAM_SOCKETS
  else if (EQ (tem, Qdatagram))
    socktype = SOCK_DGRAM;
#endif
#ifdef HAVE_SEQPACKET
  else if (EQ (tem, Qseqpacket))
    socktype = SOCK_SEQPACKET;
#endif
  else
    error ("Unsupported connection type");

  /* :server BOOL */
  tem = Fplist_get (contact, QCserver);
  if (!NILP (tem))
    {
      /* Don't support network sockets when non-blocking mode is
	 not available, since a blocked Emacs is not useful.  */
#if !defined (O_NONBLOCK) && !defined (O_NDELAY)
      error ("Network servers not supported");
#else
      is_server = 1;
      if (INTEGERP (tem))
	backlog = XINT (tem);
#endif
    }

  /* Make QCaddress an alias for :local (server) or :remote (client).  */
  QCaddress = is_server ? QClocal : QCremote;

  /* :nowait BOOL */
  if (!is_server && socktype != SOCK_DGRAM
      && (tem = Fplist_get (contact, QCnowait), !NILP (tem)))
    {
#ifndef NON_BLOCKING_CONNECT
      error ("Non-blocking connect not supported");
#else
      is_non_blocking_client = 1;
#endif
    }

  name = Fplist_get (contact, QCname);
  buffer = Fplist_get (contact, QCbuffer);
  filter = Fplist_get (contact, QCfilter);
  sentinel = Fplist_get (contact, QCsentinel);

  CHECK_STRING (name);

  /* Initialize addrinfo structure in case we don't use getaddrinfo.  */
  ai.ai_socktype = socktype;
  ai.ai_protocol = 0;
  ai.ai_next = NULL;
  res = &ai;

  /* :local ADDRESS or :remote ADDRESS */
  address = Fplist_get (contact, QCaddress);
  if (!NILP (address))
    {
      host = service = Qnil;

      if (!(ai.ai_addrlen = get_lisp_to_sockaddr_size (address, &family)))
	error ("Malformed :address");
      ai.ai_family = family;
      ai.ai_addr = alloca (ai.ai_addrlen);
      conv_lisp_to_sockaddr (family, address, ai.ai_addr, ai.ai_addrlen);
      goto open_socket;
    }

  /* :family FAMILY -- nil (for Inet), local, or integer.  */
  tem = Fplist_get (contact, QCfamily);
  if (NILP (tem))
    {
#if defined (HAVE_GETADDRINFO) && defined (AF_INET6)
      family = AF_UNSPEC;
#else
      family = AF_INET;
#endif
    }
#ifdef HAVE_LOCAL_SOCKETS
  else if (EQ (tem, Qlocal))
    family = AF_LOCAL;
#endif
#ifdef AF_INET6
  else if (EQ (tem, Qipv6))
    family = AF_INET6;
#endif
  else if (EQ (tem, Qipv4))
    family = AF_INET;
  else if (INTEGERP (tem))
    family = XINT (tem);
  else
    error ("Unknown address family");

  ai.ai_family = family;

  /* :service SERVICE -- string, integer (port number), or t (random port).  */
  service = Fplist_get (contact, QCservice);

  /* :host HOST -- hostname, ip address, or 'local for localhost.  */
  host = Fplist_get (contact, QChost);
  if (!NILP (host))
    {
      if (EQ (host, Qlocal))
	/* Depending on setup, "localhost" may map to different IPv4 and/or
	   IPv6 addresses, so it's better to be explicit.  (Bug#6781) */
	host = build_string ("127.0.0.1");
      CHECK_STRING (host);
    }

#ifdef HAVE_LOCAL_SOCKETS
  if (family == AF_LOCAL)
    {
      if (!NILP (host))
	{
	  message (":family local ignores the :host \"%s\" property",
		   SDATA (host));
	  contact = Fplist_put (contact, QChost, Qnil);
	  host = Qnil;
	}
      CHECK_STRING (service);
      memset (&address_un, 0, sizeof address_un);
      address_un.sun_family = AF_LOCAL;
      strncpy (address_un.sun_path, SSDATA (service), sizeof address_un.sun_path);
      ai.ai_addr = (struct sockaddr *) &address_un;
      ai.ai_addrlen = sizeof address_un;
      goto open_socket;
    }
#endif

  /* Slow down polling to every ten seconds.
     Some kernels have a bug which causes retrying connect to fail
     after a connect.  Polling can interfere with gethostbyname too.  */
#ifdef POLL_FOR_INPUT
  if (socktype != SOCK_DGRAM)
    {
      record_unwind_protect (unwind_stop_other_atimers, Qnil);
      bind_polling_period (10);
    }
#endif

#ifdef HAVE_GETADDRINFO
  /* If we have a host, use getaddrinfo to resolve both host and service.
     Otherwise, use getservbyname to lookup the service.  */
  if (!NILP (host))
    {

      /* SERVICE can either be a string or int.
	 Convert to a C string for later use by getaddrinfo.  */
      if (EQ (service, Qt))
	portstring = "0";
      else if (INTEGERP (service))
	{
	  sprintf (portbuf, "%"pI"d", XINT (service));
	  portstring = portbuf;
	}
      else
	{
	  CHECK_STRING (service);
	  portstring = SSDATA (service);
	}

      immediate_quit = 1;
      QUIT;
      memset (&hints, 0, sizeof (hints));
      hints.ai_flags = 0;
      hints.ai_family = family;
      hints.ai_socktype = socktype;
      hints.ai_protocol = 0;

#ifdef HAVE_RES_INIT
      res_init ();
#endif

      ret = getaddrinfo (SSDATA (host), portstring, &hints, &res);
      if (ret)
#ifdef HAVE_GAI_STRERROR
	error ("%s/%s %s", SSDATA (host), portstring, gai_strerror (ret));
#else
	error ("%s/%s getaddrinfo error %d", SSDATA (host), portstring, ret);
#endif
      immediate_quit = 0;

      goto open_socket;
    }
#endif /* HAVE_GETADDRINFO */

  /* We end up here if getaddrinfo is not defined, or in case no hostname
     has been specified (e.g. for a local server process).  */

  if (EQ (service, Qt))
    port = 0;
  else if (INTEGERP (service))
    port = htons ((unsigned short) XINT (service));
  else
    {
      struct servent *svc_info;
      CHECK_STRING (service);
      svc_info = getservbyname (SSDATA (service),
				(socktype == SOCK_DGRAM ? "udp" : "tcp"));
      if (svc_info == 0)
	error ("Unknown service: %s", SDATA (service));
      port = svc_info->s_port;
    }

  memset (&address_in, 0, sizeof address_in);
  address_in.sin_family = family;
  address_in.sin_addr.s_addr = INADDR_ANY;
  address_in.sin_port = port;

#ifndef HAVE_GETADDRINFO
  if (!NILP (host))
    {
      struct hostent *host_info_ptr;

      /* gethostbyname may fail with TRY_AGAIN, but we don't honor that,
	 as it may `hang' Emacs for a very long time.  */
      immediate_quit = 1;
      QUIT;

#ifdef HAVE_RES_INIT
      res_init ();
#endif

      host_info_ptr = gethostbyname (SDATA (host));
      immediate_quit = 0;

      if (host_info_ptr)
	{
	  memcpy (&address_in.sin_addr, host_info_ptr->h_addr,
		  host_info_ptr->h_length);
	  family = host_info_ptr->h_addrtype;
	  address_in.sin_family = family;
	}
      else
	/* Attempt to interpret host as numeric inet address */
	{
	  unsigned long numeric_addr;
	  numeric_addr = inet_addr (SSDATA (host));
	  if (numeric_addr == -1)
	    error ("Unknown host \"%s\"", SDATA (host));

	  memcpy (&address_in.sin_addr, &numeric_addr,
		  sizeof (address_in.sin_addr));
	}

    }
#endif /* not HAVE_GETADDRINFO */

  ai.ai_family = family;
  ai.ai_addr = (struct sockaddr *) &address_in;
  ai.ai_addrlen = sizeof address_in;

 open_socket:

  /* Do this in case we never enter the for-loop below.  */
  count1 = SPECPDL_INDEX ();
  s = -1;

  for (lres = res; lres; lres = lres->ai_next)
    {
      ptrdiff_t optn;
      int optbits;

#ifdef WINDOWSNT
    retry_connect:
#endif

      s = socket (lres->ai_family, lres->ai_socktype, lres->ai_protocol);
      if (s < 0)
	{
	  xerrno = errno;
	  continue;
	}

#ifdef DATAGRAM_SOCKETS
      if (!is_server && socktype == SOCK_DGRAM)
	break;
#endif /* DATAGRAM_SOCKETS */

#ifdef NON_BLOCKING_CONNECT
      if (is_non_blocking_client)
	{
#ifdef O_NONBLOCK
	  ret = fcntl (s, F_SETFL, O_NONBLOCK);
#else
	  ret = fcntl (s, F_SETFL, O_NDELAY);
#endif
	  if (ret < 0)
	    {
	      xerrno = errno;
	      emacs_close (s);
	      s = -1;
	      continue;
	    }
	}
#endif

      /* Make us close S if quit.  */
      record_unwind_protect (close_file_unwind, make_number (s));

      /* Parse network options in the arg list.
	 We simply ignore anything which isn't a known option (including other keywords).
	 An error is signaled if setting a known option fails.  */
      for (optn = optbits = 0; optn < nargs-1; optn += 2)
	optbits |= set_socket_option (s, args[optn], args[optn+1]);

      if (is_server)
	{
	  /* Configure as a server socket.  */

	  /* SO_REUSEADDR = 1 is default for server sockets; must specify
	     explicit :reuseaddr key to override this.  */
#ifdef HAVE_LOCAL_SOCKETS
	  if (family != AF_LOCAL)
#endif
	    if (!(optbits & (1 << OPIX_REUSEADDR)))
	      {
		int optval = 1;
		if (setsockopt (s, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof optval))
		  report_file_error ("Cannot set reuse option on server socket", Qnil);
	      }

	  if (bind (s, lres->ai_addr, lres->ai_addrlen))
	    report_file_error ("Cannot bind server socket", Qnil);

#ifdef HAVE_GETSOCKNAME
	  if (EQ (service, Qt))
	    {
	      struct sockaddr_in sa1;
	      socklen_t len1 = sizeof (sa1);
	      if (getsockname (s, (struct sockaddr *)&sa1, &len1) == 0)
		{
		  ((struct sockaddr_in *)(lres->ai_addr))->sin_port = sa1.sin_port;
		  service = make_number (ntohs (sa1.sin_port));
		  contact = Fplist_put (contact, QCservice, service);
		}
	    }
#endif

	  if (socktype != SOCK_DGRAM && listen (s, backlog))
	    report_file_error ("Cannot listen on server socket", Qnil);

	  break;
	}

      immediate_quit = 1;
      QUIT;

      ret = connect (s, lres->ai_addr, lres->ai_addrlen);
      xerrno = errno;

      if (ret == 0 || xerrno == EISCONN)
	{
	  /* The unwind-protect will be discarded afterwards.
	     Likewise for immediate_quit.  */
	  break;
	}

#ifdef NON_BLOCKING_CONNECT
#ifdef EINPROGRESS
      if (is_non_blocking_client && xerrno == EINPROGRESS)
	break;
#else
#ifdef EWOULDBLOCK
      if (is_non_blocking_client && xerrno == EWOULDBLOCK)
	break;
#endif
#endif
#endif

#ifndef WINDOWSNT
      if (xerrno == EINTR)
	{
	  /* Unlike most other syscalls connect() cannot be called
	     again.  (That would return EALREADY.)  The proper way to
	     wait for completion is select(). */
	  int sc;
	  socklen_t len;
	  SELECT_TYPE fdset;
	retry_select:
	  FD_ZERO (&fdset);
	  FD_SET (s, &fdset);
	  QUIT;
	  sc = select (s + 1, (SELECT_TYPE *)0, &fdset, (SELECT_TYPE *)0,
		       (EMACS_TIME *)0);
	  if (sc == -1)
	    {
	      if (errno == EINTR)
		goto retry_select;
	      else
		report_file_error ("select failed", Qnil);
	    }
	  eassert (sc > 0);

	  len = sizeof xerrno;
	  eassert (FD_ISSET (s, &fdset));
	  if (getsockopt (s, SOL_SOCKET, SO_ERROR, &xerrno, &len) == -1)
	    report_file_error ("getsockopt failed", Qnil);
	  if (xerrno)
	    errno = xerrno, report_file_error ("error during connect", Qnil);
	  else
	    break;
	}
#endif /* !WINDOWSNT */

      immediate_quit = 0;

      /* Discard the unwind protect closing S.  */
      specpdl_ptr = specpdl + count1;
      emacs_close (s);
      s = -1;

#ifdef WINDOWSNT
      if (xerrno == EINTR)
	goto retry_connect;
#endif
    }

  if (s >= 0)
    {
#ifdef DATAGRAM_SOCKETS
      if (socktype == SOCK_DGRAM)
	{
	  if (datagram_address[s].sa)
	    abort ();
	  datagram_address[s].sa = (struct sockaddr *) xmalloc (lres->ai_addrlen);
	  datagram_address[s].len = lres->ai_addrlen;
	  if (is_server)
	    {
	      Lisp_Object remote;
	      memset (datagram_address[s].sa, 0, lres->ai_addrlen);
	      if (remote = Fplist_get (contact, QCremote), !NILP (remote))
		{
		  int rfamily, rlen;
		  rlen = get_lisp_to_sockaddr_size (remote, &rfamily);
		  if (rfamily == lres->ai_family && rlen == lres->ai_addrlen)
		    conv_lisp_to_sockaddr (rfamily, remote,
					   datagram_address[s].sa, rlen);
		}
	    }
	  else
	    memcpy (datagram_address[s].sa, lres->ai_addr, lres->ai_addrlen);
	}
#endif
      contact = Fplist_put (contact, QCaddress,
			    conv_sockaddr_to_lisp (lres->ai_addr, lres->ai_addrlen));
#ifdef HAVE_GETSOCKNAME
      if (!is_server)
	{
	  struct sockaddr_in sa1;
	  socklen_t len1 = sizeof (sa1);
	  if (getsockname (s, (struct sockaddr *)&sa1, &len1) == 0)
	    contact = Fplist_put (contact, QClocal,
				  conv_sockaddr_to_lisp ((struct sockaddr *)&sa1, len1));
	}
#endif
    }

  immediate_quit = 0;

#ifdef HAVE_GETADDRINFO
  if (res != &ai)
    {
      BLOCK_INPUT;
      freeaddrinfo (res);
      UNBLOCK_INPUT;
    }
#endif

  /* Discard the unwind protect for closing S, if any.  */
  specpdl_ptr = specpdl + count1;

  /* Unwind bind_polling_period and request_sigio.  */
  unbind_to (count, Qnil);

  if (s < 0)
    {
      /* If non-blocking got this far - and failed - assume non-blocking is
	 not supported after all.  This is probably a wrong assumption, but
	 the normal blocking calls to open-network-stream handles this error
	 better.  */
      if (is_non_blocking_client)
	  return Qnil;

      errno = xerrno;
      if (is_server)
	report_file_error ("make server process failed", contact);
      else
	report_file_error ("make client process failed", contact);
    }

  inch = s;
  outch = s;

  if (!NILP (buffer))
    buffer = Fget_buffer_create (buffer);
  proc = make_process (name);

  chan_process[inch] = proc;

#ifdef O_NONBLOCK
  fcntl (inch, F_SETFL, O_NONBLOCK);
#else
#ifdef O_NDELAY
  fcntl (inch, F_SETFL, O_NDELAY);
#endif
#endif

  p = XPROCESS (proc);

  p->childp = contact;
  p->plist = Fcopy_sequence (Fplist_get (contact, QCplist));
  p->type = Qnetwork;

  p->buffer = buffer;
  p->sentinel = sentinel;
  p->filter = filter;
  p->log = Fplist_get (contact, QClog);
  if (tem = Fplist_get (contact, QCnoquery), !NILP (tem))
    p->kill_without_query = 1;
  if ((tem = Fplist_get (contact, QCstop), !NILP (tem)))
    p->command = Qt;
  p->pid = 0;
  p->infd  = inch;
  p->outfd = outch;
  if (is_server && socktype != SOCK_DGRAM)
    p->status = Qlisten;

  /* Make the process marker point into the process buffer (if any).  */
  if (BUFFERP (buffer))
    set_marker_both (p->mark, buffer,
		     BUF_ZV (XBUFFER (buffer)),
		     BUF_ZV_BYTE (XBUFFER (buffer)));

#ifdef NON_BLOCKING_CONNECT
  if (is_non_blocking_client)
    {
      /* We may get here if connect did succeed immediately.  However,
	 in that case, we still need to signal this like a non-blocking
	 connection.  */
      p->status = Qconnect;
      if (!FD_ISSET (inch, &connect_wait_mask))
	{
	  FD_SET (inch, &connect_wait_mask);
	  FD_SET (inch, &write_mask);
	  num_pending_connects++;
	}
    }
  else
#endif
    /* A server may have a client filter setting of Qt, but it must
       still listen for incoming connects unless it is stopped.  */
    if ((!EQ (p->filter, Qt) && !EQ (p->command, Qt))
	|| (EQ (p->status, Qlisten) && NILP (p->command)))
      {
	FD_SET (inch, &input_wait_mask);
	FD_SET (inch, &non_keyboard_wait_mask);
      }

  if (inch > max_process_desc)
    max_process_desc = inch;

  tem = Fplist_member (contact, QCcoding);
  if (!NILP (tem) && (!CONSP (tem) || !CONSP (XCDR (tem))))
    tem = Qnil;  /* No error message (too late!).  */

  {
    /* Setup coding systems for communicating with the network stream.  */
    struct gcpro gcpro1;
    /* Qt denotes we have not yet called Ffind_operation_coding_system.  */
    Lisp_Object coding_systems = Qt;
    Lisp_Object fargs[5], val;

    if (!NILP (tem))
      {
	val = XCAR (XCDR (tem));
	if (CONSP (val))
	  val = XCAR (val);
      }
    else if (!NILP (Vcoding_system_for_read))
      val = Vcoding_system_for_read;
    else if ((!NILP (buffer) && NILP (BVAR (XBUFFER (buffer), enable_multibyte_characters)))
	     || (NILP (buffer) && NILP (BVAR (&buffer_defaults, enable_multibyte_characters))))
      /* We dare not decode end-of-line format by setting VAL to
	 Qraw_text, because the existing Emacs Lisp libraries
	 assume that they receive bare code including a sequence of
	 CR LF.  */
      val = Qnil;
    else
      {
	if (NILP (host) || NILP (service))
	  coding_systems = Qnil;
	else
	  {
	    fargs[0] = Qopen_network_stream, fargs[1] = name,
	      fargs[2] = buffer, fargs[3] = host, fargs[4] = service;
	    GCPRO1 (proc);
	    coding_systems = Ffind_operation_coding_system (5, fargs);
	    UNGCPRO;
	  }
	if (CONSP (coding_systems))
	  val = XCAR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCAR (Vdefault_process_coding_system);
	else
	  val = Qnil;
      }
    p->decode_coding_system = val;

    if (!NILP (tem))
      {
	val = XCAR (XCDR (tem));
	if (CONSP (val))
	  val = XCDR (val);
      }
    else if (!NILP (Vcoding_system_for_write))
      val = Vcoding_system_for_write;
    else if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
      val = Qnil;
    else
      {
	if (EQ (coding_systems, Qt))
	  {
	    if (NILP (host) || NILP (service))
	      coding_systems = Qnil;
	    else
	      {
		fargs[0] = Qopen_network_stream, fargs[1] = name,
		  fargs[2] = buffer, fargs[3] = host, fargs[4] = service;
		GCPRO1 (proc);
		coding_systems = Ffind_operation_coding_system (5, fargs);
		UNGCPRO;
	      }
	  }
	if (CONSP (coding_systems))
	  val = XCDR (coding_systems);
	else if (CONSP (Vdefault_process_coding_system))
	  val = XCDR (Vdefault_process_coding_system);
	else
	  val = Qnil;
      }
    p->encode_coding_system = val;
  }
  setup_process_coding_systems (proc);

  p->decoding_buf = empty_unibyte_string;
  p->decoding_carryover = 0;
  p->encoding_buf = empty_unibyte_string;

  p->inherit_coding_system_flag
    = !(!NILP (tem) || NILP (buffer) || !inherit_process_coding_system);

  UNGCPRO;
  return proc;
}


#if defined (HAVE_NET_IF_H)

#ifdef SIOCGIFCONF
DEFUN ("network-interface-list", Fnetwork_interface_list, Snetwork_interface_list, 0, 0, 0,
       doc: /* Return an alist of all network interfaces and their network address.
Each element is a cons, the car of which is a string containing the
interface name, and the cdr is the network address in internal
format; see the description of ADDRESS in `make-network-process'.  */)
  (void)
{
  struct ifconf ifconf;
  struct ifreq *ifreq;
  void *buf = NULL;
  ptrdiff_t buf_size = 512;
  int s, i;
  Lisp_Object res;

  s = socket (AF_INET, SOCK_STREAM, 0);
  if (s < 0)
    return Qnil;

  do
    {
      buf = xpalloc (buf, &buf_size, 1, INT_MAX, 1);
      ifconf.ifc_buf = buf;
      ifconf.ifc_len = buf_size;
      if (ioctl (s, SIOCGIFCONF, &ifconf))
	{
	  close (s);
	  xfree (buf);
	  return Qnil;
	}
    }
  while (ifconf.ifc_len == buf_size);

  close (s);

  res = Qnil;
  ifreq = ifconf.ifc_req;
  while ((char *) ifreq < (char *) ifconf.ifc_req + ifconf.ifc_len)
    {
      struct ifreq *ifq = ifreq;
#ifdef HAVE_STRUCT_IFREQ_IFR_ADDR_SA_LEN
#define SIZEOF_IFREQ(sif)						\
      ((sif)->ifr_addr.sa_len < sizeof (struct sockaddr)		\
       ? sizeof (*(sif)) : sizeof ((sif)->ifr_name) + (sif)->ifr_addr.sa_len)

      int len = SIZEOF_IFREQ (ifq);
#else
      int len = sizeof (*ifreq);
#endif
      char namebuf[sizeof (ifq->ifr_name) + 1];
      i += len;
      ifreq = (struct ifreq *) ((char *) ifreq + len);

      if (ifq->ifr_addr.sa_family != AF_INET)
	continue;

      memcpy (namebuf, ifq->ifr_name, sizeof (ifq->ifr_name));
      namebuf[sizeof (ifq->ifr_name)] = 0;
      res = Fcons (Fcons (build_string (namebuf),
			  conv_sockaddr_to_lisp (&ifq->ifr_addr,
						 sizeof (struct sockaddr))),
		   res);
    }

  xfree (buf);
  return res;
}
#endif /* SIOCGIFCONF */

#if defined (SIOCGIFADDR) || defined (SIOCGIFHWADDR) || defined (SIOCGIFFLAGS)

struct ifflag_def {
  int flag_bit;
  const char *flag_sym;
};

static const struct ifflag_def ifflag_table[] = {
#ifdef IFF_UP
  { IFF_UP,		"up" },
#endif
#ifdef IFF_BROADCAST
  { IFF_BROADCAST,	"broadcast" },
#endif
#ifdef IFF_DEBUG
  { IFF_DEBUG,		"debug" },
#endif
#ifdef IFF_LOOPBACK
  { IFF_LOOPBACK,	"loopback" },
#endif
#ifdef IFF_POINTOPOINT
  { IFF_POINTOPOINT,	"pointopoint" },
#endif
#ifdef IFF_RUNNING
  { IFF_RUNNING,	"running" },
#endif
#ifdef IFF_NOARP
  { IFF_NOARP,		"noarp" },
#endif
#ifdef IFF_PROMISC
  { IFF_PROMISC,	"promisc" },
#endif
#ifdef IFF_NOTRAILERS
#ifdef NS_IMPL_COCOA
  /* Really means smart, notrailers is obsolete */
  { IFF_NOTRAILERS,	"smart" },
#else
  { IFF_NOTRAILERS,	"notrailers" },
#endif
#endif
#ifdef IFF_ALLMULTI
  { IFF_ALLMULTI,	"allmulti" },
#endif
#ifdef IFF_MASTER
  { IFF_MASTER,		"master" },
#endif
#ifdef IFF_SLAVE
  { IFF_SLAVE,		"slave" },
#endif
#ifdef IFF_MULTICAST
  { IFF_MULTICAST,	"multicast" },
#endif
#ifdef IFF_PORTSEL
  { IFF_PORTSEL,	"portsel" },
#endif
#ifdef IFF_AUTOMEDIA
  { IFF_AUTOMEDIA,	"automedia" },
#endif
#ifdef IFF_DYNAMIC
  { IFF_DYNAMIC,	"dynamic" },
#endif
#ifdef IFF_OACTIVE
  { IFF_OACTIVE,	"oactive" },	/* OpenBSD: transmission in progress */
#endif
#ifdef IFF_SIMPLEX
  { IFF_SIMPLEX,	"simplex" },	/* OpenBSD: can't hear own transmissions */
#endif
#ifdef IFF_LINK0
  { IFF_LINK0,		"link0" },	/* OpenBSD: per link layer defined bit */
#endif
#ifdef IFF_LINK1
  { IFF_LINK1,		"link1" },	/* OpenBSD: per link layer defined bit */
#endif
#ifdef IFF_LINK2
  { IFF_LINK2,		"link2" },	/* OpenBSD: per link layer defined bit */
#endif
  { 0, 0 }
};

DEFUN ("network-interface-info", Fnetwork_interface_info, Snetwork_interface_info, 1, 1, 0,
       doc: /* Return information about network interface named IFNAME.
The return value is a list (ADDR BCAST NETMASK HWADDR FLAGS),
where ADDR is the layer 3 address, BCAST is the layer 3 broadcast address,
NETMASK is the layer 3 network mask, HWADDR is the layer 2 address, and
FLAGS is the current flags of the interface.  */)
  (Lisp_Object ifname)
{
  struct ifreq rq;
  Lisp_Object res = Qnil;
  Lisp_Object elt;
  int s;
  int any = 0;
#if (! (defined SIOCGIFHWADDR && defined HAVE_STRUCT_IFREQ_IFR_HWADDR)	\
     && defined HAVE_GETIFADDRS && defined LLADDR)
  struct ifaddrs *ifap;
#endif

  CHECK_STRING (ifname);

  memset (rq.ifr_name, 0, sizeof rq.ifr_name);
  strncpy (rq.ifr_name, SSDATA (ifname), sizeof (rq.ifr_name));

  s = socket (AF_INET, SOCK_STREAM, 0);
  if (s < 0)
    return Qnil;

  elt = Qnil;
#if defined (SIOCGIFFLAGS) && defined (HAVE_STRUCT_IFREQ_IFR_FLAGS)
  if (ioctl (s, SIOCGIFFLAGS, &rq) == 0)
    {
      int flags = rq.ifr_flags;
      const struct ifflag_def *fp;
      int fnum;

      /* If flags is smaller than int (i.e. short) it may have the high bit set
         due to IFF_MULTICAST.  In that case, sign extending it into
         an int is wrong.  */
      if (flags < 0 && sizeof (rq.ifr_flags) < sizeof (flags))
        flags = (unsigned short) rq.ifr_flags;

      any = 1;
      for (fp = ifflag_table; flags != 0 && fp->flag_sym; fp++)
	{
	  if (flags & fp->flag_bit)
	    {
	      elt = Fcons (intern (fp->flag_sym), elt);
	      flags -= fp->flag_bit;
	    }
	}
      for (fnum = 0; flags && fnum < 32; flags >>= 1, fnum++)
	{
	  if (flags & 1)
	    {
	      elt = Fcons (make_number (fnum), elt);
	    }
	}
    }
#endif
  res = Fcons (elt, res);

  elt = Qnil;
#if defined (SIOCGIFHWADDR) && defined (HAVE_STRUCT_IFREQ_IFR_HWADDR)
  if (ioctl (s, SIOCGIFHWADDR, &rq) == 0)
    {
      Lisp_Object hwaddr = Fmake_vector (make_number (6), Qnil);
      register struct Lisp_Vector *p = XVECTOR (hwaddr);
      int n;

      any = 1;
      for (n = 0; n < 6; n++)
	p->contents[n] = make_number (((unsigned char *)&rq.ifr_hwaddr.sa_data[0])[n]);
      elt = Fcons (make_number (rq.ifr_hwaddr.sa_family), hwaddr);
    }
#elif defined (HAVE_GETIFADDRS) && defined (LLADDR)
  if (getifaddrs (&ifap) != -1)
    {
      Lisp_Object hwaddr = Fmake_vector (make_number (6), Qnil);
      register struct Lisp_Vector *p = XVECTOR (hwaddr);
      struct ifaddrs *it;

      for (it = ifap; it != NULL; it = it->ifa_next)
        {
          struct sockaddr_dl *sdl = (struct sockaddr_dl*) it->ifa_addr;
          unsigned char linkaddr[6];
          int n;

          if (it->ifa_addr->sa_family != AF_LINK
              || strcmp (it->ifa_name, SSDATA (ifname)) != 0
              || sdl->sdl_alen != 6)
            continue;

          memcpy (linkaddr, LLADDR (sdl), sdl->sdl_alen);
          for (n = 0; n < 6; n++)
            p->contents[n] = make_number (linkaddr[n]);

          elt = Fcons (make_number (it->ifa_addr->sa_family), hwaddr);
          break;
        }
    }
#ifdef HAVE_FREEIFADDRS
  freeifaddrs (ifap);
#endif

#endif /* HAVE_GETIFADDRS && LLADDR */

  res = Fcons (elt, res);

  elt = Qnil;
#if defined (SIOCGIFNETMASK) && (defined (HAVE_STRUCT_IFREQ_IFR_NETMASK) || defined (HAVE_STRUCT_IFREQ_IFR_ADDR))
  if (ioctl (s, SIOCGIFNETMASK, &rq) == 0)
    {
      any = 1;
#ifdef HAVE_STRUCT_IFREQ_IFR_NETMASK
      elt = conv_sockaddr_to_lisp (&rq.ifr_netmask, sizeof (rq.ifr_netmask));
#else
      elt = conv_sockaddr_to_lisp (&rq.ifr_addr, sizeof (rq.ifr_addr));
#endif
    }
#endif
  res = Fcons (elt, res);

  elt = Qnil;
#if defined (SIOCGIFBRDADDR) && defined (HAVE_STRUCT_IFREQ_IFR_BROADADDR)
  if (ioctl (s, SIOCGIFBRDADDR, &rq) == 0)
    {
      any = 1;
      elt = conv_sockaddr_to_lisp (&rq.ifr_broadaddr, sizeof (rq.ifr_broadaddr));
    }
#endif
  res = Fcons (elt, res);

  elt = Qnil;
#if defined (SIOCGIFADDR) && defined (HAVE_STRUCT_IFREQ_IFR_ADDR)
  if (ioctl (s, SIOCGIFADDR, &rq) == 0)
    {
      any = 1;
      elt = conv_sockaddr_to_lisp (&rq.ifr_addr, sizeof (rq.ifr_addr));
    }
#endif
  res = Fcons (elt, res);

  close (s);

  return any ? res : Qnil;
}
#endif
#endif	/* defined (HAVE_NET_IF_H) */

/* Turn off input and output for process PROC.  */

static void
deactivate_process (Lisp_Object proc)
{
  register int inchannel, outchannel;
  register struct Lisp_Process *p = XPROCESS (proc);

#ifdef HAVE_GNUTLS
  /* Delete GnuTLS structures in PROC, if any.  */
  emacs_gnutls_deinit (proc);
#endif /* HAVE_GNUTLS */

  inchannel  = p->infd;
  outchannel = p->outfd;

#ifdef ADAPTIVE_READ_BUFFERING
  if (p->read_output_delay > 0)
    {
      if (--process_output_delay_count < 0)
	process_output_delay_count = 0;
      p->read_output_delay = 0;
      p->read_output_skip = 0;
    }
#endif

  if (inchannel >= 0)
    {
      /* Beware SIGCHLD hereabouts. */
      flush_pending_output (inchannel);
      emacs_close (inchannel);
      if (outchannel >= 0 && outchannel != inchannel)
 	emacs_close (outchannel);

      p->infd  = -1;
      p->outfd = -1;
#ifdef DATAGRAM_SOCKETS
      if (DATAGRAM_CHAN_P (inchannel))
	{
	  xfree (datagram_address[inchannel].sa);
	  datagram_address[inchannel].sa = 0;
	  datagram_address[inchannel].len = 0;
	}
#endif
      chan_process[inchannel] = Qnil;
      FD_CLR (inchannel, &input_wait_mask);
      FD_CLR (inchannel, &non_keyboard_wait_mask);
#ifdef NON_BLOCKING_CONNECT
      if (FD_ISSET (inchannel, &connect_wait_mask))
	{
	  FD_CLR (inchannel, &connect_wait_mask);
	  FD_CLR (inchannel, &write_mask);
	  if (--num_pending_connects < 0)
	    abort ();
	}
#endif
      if (inchannel == max_process_desc)
	{
	  int i;
	  /* We just closed the highest-numbered process input descriptor,
	     so recompute the highest-numbered one now.  */
	  max_process_desc = 0;
	  for (i = 0; i < MAXDESC; i++)
	    if (!NILP (chan_process[i]))
	      max_process_desc = i;
	}
    }
}


DEFUN ("accept-process-output", Faccept_process_output, Saccept_process_output,
       0, 4, 0,
       doc: /* Allow any pending output from subprocesses to be read by Emacs.
It is read into the process' buffers or given to their filter functions.
Non-nil arg PROCESS means do not return until some output has been received
from PROCESS.

Non-nil second arg SECONDS and third arg MILLISEC are number of seconds
and milliseconds to wait; return after that much time whether or not
there is any subprocess output.  If SECONDS is a floating point number,
it specifies a fractional number of seconds to wait.
The MILLISEC argument is obsolete and should be avoided.

If optional fourth arg JUST-THIS-ONE is non-nil, only accept output
from PROCESS, suspending reading output from other processes.
If JUST-THIS-ONE is an integer, don't run any timers either.
Return non-nil if we received any output before the timeout expired.  */)
  (register Lisp_Object process, Lisp_Object seconds, Lisp_Object millisec, Lisp_Object just_this_one)
{
  int secs, usecs = 0;

  if (! NILP (process))
    CHECK_PROCESS (process);
  else
    just_this_one = Qnil;

  if (!NILP (millisec))
    { /* Obsolete calling convention using integers rather than floats.  */
      CHECK_NUMBER (millisec);
      if (NILP (seconds))
	seconds = make_float (XINT (millisec) / 1000.0);
      else
	{
	  CHECK_NUMBER (seconds);
	  seconds = make_float (XINT (millisec) / 1000.0 + XINT (seconds));
	}
    }

  if (!NILP (seconds))
    {
      if (INTEGERP (seconds))
	secs = XINT (seconds);
      else if (FLOATP (seconds))
	{
	  double timeout = XFLOAT_DATA (seconds);
	  secs = (int) timeout;
	  usecs = (int) ((timeout - (double) secs) * 1000000);
	}
      else
	wrong_type_argument (Qnumberp, seconds);

      if (secs < 0 || (secs == 0 && usecs == 0))
	secs = -1, usecs = 0;
    }
  else
    secs = NILP (process) ? -1 : 0;

  return
    (wait_reading_process_output (secs, usecs, 0, 0,
				  Qnil,
				  !NILP (process) ? XPROCESS (process) : NULL,
				  NILP (just_this_one) ? 0 :
				  !INTEGERP (just_this_one) ? 1 : -1)
     ? Qt : Qnil);
}

/* Accept a connection for server process SERVER on CHANNEL.  */

static int connect_counter = 0;

static void
server_accept_connection (Lisp_Object server, int channel)
{
  Lisp_Object proc, caller, name, buffer;
  Lisp_Object contact, host, service;
  struct Lisp_Process *ps= XPROCESS (server);
  struct Lisp_Process *p;
  int s;
  union u_sockaddr {
    struct sockaddr sa;
    struct sockaddr_in in;
#ifdef AF_INET6
    struct sockaddr_in6 in6;
#endif
#ifdef HAVE_LOCAL_SOCKETS
    struct sockaddr_un un;
#endif
  } saddr;
  socklen_t len = sizeof saddr;

  s = accept (channel, &saddr.sa, &len);

  if (s < 0)
    {
      int code = errno;

      if (code == EAGAIN)
	return;
#ifdef EWOULDBLOCK
      if (code == EWOULDBLOCK)
	return;
#endif

      if (!NILP (ps->log))
	call3 (ps->log, server, Qnil,
	       concat3 (build_string ("accept failed with code"),
			Fnumber_to_string (make_number (code)),
			build_string ("\n")));
      return;
    }

  connect_counter++;

  /* Setup a new process to handle the connection.  */

  /* Generate a unique identification of the caller, and build contact
     information for this process.  */
  host = Qt;
  service = Qnil;
  switch (saddr.sa.sa_family)
    {
    case AF_INET:
      {
	Lisp_Object args[5];
	unsigned char *ip = (unsigned char *)&saddr.in.sin_addr.s_addr;
	args[0] = build_string ("%d.%d.%d.%d");
	args[1] = make_number (*ip++);
	args[2] = make_number (*ip++);
	args[3] = make_number (*ip++);
	args[4] = make_number (*ip++);
	host = Fformat (5, args);
	service = make_number (ntohs (saddr.in.sin_port));

	args[0] = build_string (" <%s:%d>");
	args[1] = host;
	args[2] = service;
	caller = Fformat (3, args);
      }
      break;

#ifdef AF_INET6
    case AF_INET6:
      {
	Lisp_Object args[9];
	uint16_t *ip6 = (uint16_t *)&saddr.in6.sin6_addr;
	int i;
	args[0] = build_string ("%x:%x:%x:%x:%x:%x:%x:%x");
	for (i = 0; i < 8; i++)
	  args[i+1] = make_number (ntohs (ip6[i]));
	host = Fformat (9, args);
	service = make_number (ntohs (saddr.in.sin_port));

	args[0] = build_string (" <[%s]:%d>");
	args[1] = host;
	args[2] = service;
	caller = Fformat (3, args);
      }
      break;
#endif

#ifdef HAVE_LOCAL_SOCKETS
    case AF_LOCAL:
#endif
    default:
      caller = Fnumber_to_string (make_number (connect_counter));
      caller = concat3 (build_string (" <"), caller, build_string (">"));
      break;
    }

  /* Create a new buffer name for this process if it doesn't have a
     filter.  The new buffer name is based on the buffer name or
     process name of the server process concatenated with the caller
     identification.  */

  if (!NILP (ps->filter) && !EQ (ps->filter, Qt))
    buffer = Qnil;
  else
    {
      buffer = ps->buffer;
      if (!NILP (buffer))
	buffer = Fbuffer_name (buffer);
      else
	buffer = ps->name;
      if (!NILP (buffer))
	{
	  buffer = concat2 (buffer, caller);
	  buffer = Fget_buffer_create (buffer);
	}
    }

  /* Generate a unique name for the new server process.  Combine the
     server process name with the caller identification.  */

  name = concat2 (ps->name, caller);
  proc = make_process (name);

  chan_process[s] = proc;

#ifdef O_NONBLOCK
  fcntl (s, F_SETFL, O_NONBLOCK);
#else
#ifdef O_NDELAY
  fcntl (s, F_SETFL, O_NDELAY);
#endif
#endif

  p = XPROCESS (proc);

  /* Build new contact information for this setup.  */
  contact = Fcopy_sequence (ps->childp);
  contact = Fplist_put (contact, QCserver, Qnil);
  contact = Fplist_put (contact, QChost, host);
  if (!NILP (service))
    contact = Fplist_put (contact, QCservice, service);
  contact = Fplist_put (contact, QCremote,
			conv_sockaddr_to_lisp (&saddr.sa, len));
#ifdef HAVE_GETSOCKNAME
  len = sizeof saddr;
  if (getsockname (s, &saddr.sa, &len) == 0)
    contact = Fplist_put (contact, QClocal,
			  conv_sockaddr_to_lisp (&saddr.sa, len));
#endif

  p->childp = contact;
  p->plist = Fcopy_sequence (ps->plist);
  p->type = Qnetwork;

  p->buffer = buffer;
  p->sentinel = ps->sentinel;
  p->filter = ps->filter;
  p->command = Qnil;
  p->pid = 0;
  p->infd  = s;
  p->outfd = s;
  p->status = Qrun;

  /* Client processes for accepted connections are not stopped initially.  */
  if (!EQ (p->filter, Qt))
    {
      FD_SET (s, &input_wait_mask);
      FD_SET (s, &non_keyboard_wait_mask);
    }

  if (s > max_process_desc)
    max_process_desc = s;

  /* Setup coding system for new process based on server process.
     This seems to be the proper thing to do, as the coding system
     of the new process should reflect the settings at the time the
     server socket was opened; not the current settings. */

  p->decode_coding_system = ps->decode_coding_system;
  p->encode_coding_system = ps->encode_coding_system;
  setup_process_coding_systems (proc);

  p->decoding_buf = empty_unibyte_string;
  p->decoding_carryover = 0;
  p->encoding_buf = empty_unibyte_string;

  p->inherit_coding_system_flag
    = (NILP (buffer) ? 0 : ps->inherit_coding_system_flag);

  if (!NILP (ps->log))
      call3 (ps->log, server, proc,
	     concat3 (build_string ("accept from "),
		      (STRINGP (host) ? host : build_string ("-")),
		      build_string ("\n")));

  if (!NILP (p->sentinel))
    exec_sentinel (proc,
		   concat3 (build_string ("open from "),
			    (STRINGP (host) ? host : build_string ("-")),
			    build_string ("\n")));
}

/* This variable is different from waiting_for_input in keyboard.c.
   It is used to communicate to a lisp process-filter/sentinel (via the
   function Fwaiting_for_user_input_p below) whether Emacs was waiting
   for user-input when that process-filter was called.
   waiting_for_input cannot be used as that is by definition 0 when
   lisp code is being evalled.
   This is also used in record_asynch_buffer_change.
   For that purpose, this must be 0
   when not inside wait_reading_process_output.  */
static int waiting_for_user_input_p;

static Lisp_Object
wait_reading_process_output_unwind (Lisp_Object data)
{
  waiting_for_user_input_p = XINT (data);
  return Qnil;
}

/* This is here so breakpoints can be put on it.  */
static void
wait_reading_process_output_1 (void)
{
}

/* Use a wrapper around select to work around a bug in gdb 5.3.
   Normally, the wrapper is optimized away by inlining.

   If emacs is stopped inside select, the gdb backtrace doesn't
   show the function which called select, so it is practically
   impossible to step through wait_reading_process_output.  */

#ifndef select
static inline int
select_wrapper (int n, fd_set *rfd, fd_set *wfd, fd_set *xfd, struct timeval *tmo)
{
  return select (n, rfd, wfd, xfd, tmo);
}
#define select select_wrapper
#endif

/* Read and dispose of subprocess output while waiting for timeout to
   elapse and/or keyboard input to be available.

   TIME_LIMIT is:
     timeout in seconds, or
     zero for no limit, or
     -1 means gobble data immediately available but don't wait for any.

   MICROSECS is:
     an additional duration to wait, measured in microseconds.
     If this is nonzero and time_limit is 0, then the timeout
     consists of MICROSECS only.

   READ_KBD is a lisp value:
     0 to ignore keyboard input, or
     1 to return when input is available, or
     -1 meaning caller will actually read the input, so don't throw to
       the quit handler, or

   DO_DISPLAY != 0 means redisplay should be done to show subprocess
     output that arrives.

   If WAIT_FOR_CELL is a cons cell, wait until its car is non-nil
     (and gobble terminal input into the buffer if any arrives).

   If WAIT_PROC is specified, wait until something arrives from that
     process.  The return value is true if we read some input from
     that process.

   If JUST_WAIT_PROC is non-nil, handle only output from WAIT_PROC
     (suspending output from other processes).  A negative value
     means don't run any timers either.

   If WAIT_PROC is specified, then the function returns true if we
     received input from that process before the timeout elapsed.
   Otherwise, return true if we received input from any process.  */

int
wait_reading_process_output (int time_limit, int microsecs, int read_kbd,
			     int do_display,
			     Lisp_Object wait_for_cell,
			     struct Lisp_Process *wait_proc, int just_wait_proc)
{
  register int channel, nfds;
  SELECT_TYPE Available;
  SELECT_TYPE Writeok;
  int check_write;
  int check_delay, no_avail;
  int xerrno;
  Lisp_Object proc;
  EMACS_TIME timeout, end_time;
  int wait_channel = -1;
  int got_some_input = 0;
  int count = SPECPDL_INDEX ();

  FD_ZERO (&Available);
  FD_ZERO (&Writeok);

  if (time_limit == 0 && microsecs == 0 && wait_proc && !NILP (Vinhibit_quit)
      && !(CONSP (wait_proc->status) && EQ (XCAR (wait_proc->status), Qexit)))
    message ("Blocking call to accept-process-output with quit inhibited!!");

  /* If wait_proc is a process to watch, set wait_channel accordingly.  */
  if (wait_proc != NULL)
    wait_channel = wait_proc->infd;

  record_unwind_protect (wait_reading_process_output_unwind,
			 make_number (waiting_for_user_input_p));
  waiting_for_user_input_p = read_kbd;

  /* Since we may need to wait several times,
     compute the absolute time to return at.  */
  if (time_limit || microsecs)
    {
      EMACS_GET_TIME (end_time);
      EMACS_SET_SECS_USECS (timeout, time_limit, microsecs);
      EMACS_ADD_TIME (end_time, end_time, timeout);
    }

  while (1)
    {
      int timeout_reduced_for_timers = 0;

      /* If calling from keyboard input, do not quit
	 since we want to return C-g as an input character.
	 Otherwise, do pending quit if requested.  */
      if (read_kbd >= 0)
	QUIT;
#ifdef SYNC_INPUT
      else
	process_pending_signals ();
#endif

      /* Exit now if the cell we're waiting for became non-nil.  */
      if (! NILP (wait_for_cell) && ! NILP (XCAR (wait_for_cell)))
	break;

      /* Compute time from now till when time limit is up */
      /* Exit if already run out */
      if (time_limit == -1)
	{
	  /* -1 specified for timeout means
	     gobble output available now
	     but don't wait at all. */

	  EMACS_SET_SECS_USECS (timeout, 0, 0);
	}
      else if (time_limit || microsecs)
	{
	  EMACS_GET_TIME (timeout);
	  EMACS_SUB_TIME (timeout, end_time, timeout);
	  if (EMACS_TIME_NEG_P (timeout))
	    break;
	}
      else
	{
	  EMACS_SET_SECS_USECS (timeout, 100000, 0);
	}

      /* Normally we run timers here.
	 But not if wait_for_cell; in those cases,
	 the wait is supposed to be short,
	 and those callers cannot handle running arbitrary Lisp code here.  */
      if (NILP (wait_for_cell)
	  && just_wait_proc >= 0)
	{
	  EMACS_TIME timer_delay;

	  do
	    {
	      int old_timers_run = timers_run;
	      struct buffer *old_buffer = current_buffer;
	      Lisp_Object old_window = selected_window;

	      timer_delay = timer_check ();

	      /* If a timer has run, this might have changed buffers
		 an alike.  Make read_key_sequence aware of that.  */
	      if (timers_run != old_timers_run
		  && (old_buffer != current_buffer
		      || !EQ (old_window, selected_window))
		  && waiting_for_user_input_p == -1)
		record_asynch_buffer_change ();

	      if (timers_run != old_timers_run && do_display)
		/* We must retry, since a timer may have requeued itself
		   and that could alter the time_delay.  */
		redisplay_preserve_echo_area (9);
	      else
		break;
	    }
	  while (!detect_input_pending ());

	  /* If there is unread keyboard input, also return.  */
	  if (read_kbd != 0
	      && requeued_events_pending_p ())
	    break;

	  if (! EMACS_TIME_NEG_P (timer_delay) && time_limit != -1)
	    {
	      EMACS_TIME difference;
	      EMACS_SUB_TIME (difference, timer_delay, timeout);
	      if (EMACS_TIME_NEG_P (difference))
		{
		  timeout = timer_delay;
		  timeout_reduced_for_timers = 1;
		}
	    }
	  /* If time_limit is -1, we are not going to wait at all.  */
	  else if (time_limit != -1)
	    {
	      /* This is so a breakpoint can be put here.  */
	      wait_reading_process_output_1 ();
	    }
	}

      /* Cause C-g and alarm signals to take immediate action,
	 and cause input available signals to zero out timeout.

	 It is important that we do this before checking for process
	 activity.  If we get a SIGCHLD after the explicit checks for
	 process activity, timeout is the only way we will know.  */
      if (read_kbd < 0)
	set_waiting_for_input (&timeout);

      /* If status of something has changed, and no input is
	 available, notify the user of the change right away.  After
	 this explicit check, we'll let the SIGCHLD handler zap
	 timeout to get our attention.  */
      if (update_tick != process_tick)
	{
	  SELECT_TYPE Atemp;
	  SELECT_TYPE Ctemp;

          if (kbd_on_hold_p ())
            FD_ZERO (&Atemp);
          else
            Atemp = input_wait_mask;
	  Ctemp = write_mask;

	  EMACS_SET_SECS_USECS (timeout, 0, 0);
	  if ((select (max (max_process_desc, max_input_desc) + 1,
		       &Atemp,
#ifdef NON_BLOCKING_CONNECT
		       (num_pending_connects > 0 ? &Ctemp : (SELECT_TYPE *)0),
#else
		       (SELECT_TYPE *)0,
#endif
		       (SELECT_TYPE *)0, &timeout)
	       <= 0))
	    {
	      /* It's okay for us to do this and then continue with
		 the loop, since timeout has already been zeroed out.  */
	      clear_waiting_for_input ();
	      status_notify (NULL);
	      if (do_display) redisplay_preserve_echo_area (13);
	    }
	}

      /* Don't wait for output from a non-running process.  Just
	 read whatever data has already been received.  */
      if (wait_proc && wait_proc->raw_status_new)
	update_status (wait_proc);
      if (wait_proc
	  && ! EQ (wait_proc->status, Qrun)
	  && ! EQ (wait_proc->status, Qconnect))
	{
	  int nread, total_nread = 0;

	  clear_waiting_for_input ();
	  XSETPROCESS (proc, wait_proc);

	  /* Read data from the process, until we exhaust it.  */
	  while (wait_proc->infd >= 0)
	    {
	      nread = read_process_output (proc, wait_proc->infd);

	      if (nread == 0)
		break;

	      if (0 < nread)
		{
		  total_nread += nread;
		  got_some_input = 1;
		}
#ifdef EIO
	      else if (nread == -1 && EIO == errno)
		break;
#endif
#ifdef EAGAIN
	      else if (nread == -1 && EAGAIN == errno)
		break;
#endif
#ifdef EWOULDBLOCK
	      else if (nread == -1 && EWOULDBLOCK == errno)
		break;
#endif
	    }
	  if (total_nread > 0 && do_display)
	    redisplay_preserve_echo_area (10);

	  break;
	}

      /* Wait till there is something to do */

      if (wait_proc && just_wait_proc)
	{
	  if (wait_proc->infd < 0)  /* Terminated */
	    break;
	  FD_SET (wait_proc->infd, &Available);
	  check_delay = 0;
          check_write = 0;
	}
      else if (!NILP (wait_for_cell))
	{
	  Available = non_process_wait_mask;
	  check_delay = 0;
	  check_write = 0;
	}
      else
	{
	  if (! read_kbd)
	    Available = non_keyboard_wait_mask;
	  else
	    Available = input_wait_mask;
          Writeok = write_mask;
#ifdef SELECT_CANT_DO_WRITE_MASK
          check_write = 0;
#else
          check_write = 1;
#endif
 	  check_delay = wait_channel >= 0 ? 0 : process_output_delay_count;
	}

      /* If frame size has changed or the window is newly mapped,
	 redisplay now, before we start to wait.  There is a race
	 condition here; if a SIGIO arrives between now and the select
	 and indicates that a frame is trashed, the select may block
	 displaying a trashed screen.  */
      if (frame_garbaged && do_display)
	{
	  clear_waiting_for_input ();
	  redisplay_preserve_echo_area (11);
	  if (read_kbd < 0)
	    set_waiting_for_input (&timeout);
	}

      /* Skip the `select' call if input is available and we're
	 waiting for keyboard input or a cell change (which can be
	 triggered by processing X events).  In the latter case, set
	 nfds to 1 to avoid breaking the loop.  */
      no_avail = 0;
      if ((read_kbd || !NILP (wait_for_cell))
	  && detect_input_pending ())
	{
	  nfds = read_kbd ? 0 : 1;
	  no_avail = 1;
	}

      if (!no_avail)
	{

#ifdef ADAPTIVE_READ_BUFFERING
	  /* Set the timeout for adaptive read buffering if any
	     process has non-zero read_output_skip and non-zero
	     read_output_delay, and we are not reading output for a
	     specific wait_channel.  It is not executed if
	     Vprocess_adaptive_read_buffering is nil.  */
	  if (process_output_skip && check_delay > 0)
	    {
	      int usecs = EMACS_USECS (timeout);
	      if (EMACS_SECS (timeout) > 0 || usecs > READ_OUTPUT_DELAY_MAX)
		usecs = READ_OUTPUT_DELAY_MAX;
	      for (channel = 0; check_delay > 0 && channel <= max_process_desc; channel++)
		{
		  proc = chan_process[channel];
		  if (NILP (proc))
		    continue;
		  /* Find minimum non-zero read_output_delay among the
		     processes with non-zero read_output_skip.  */
		  if (XPROCESS (proc)->read_output_delay > 0)
		    {
		      check_delay--;
		      if (!XPROCESS (proc)->read_output_skip)
			continue;
		      FD_CLR (channel, &Available);
		      XPROCESS (proc)->read_output_skip = 0;
		      if (XPROCESS (proc)->read_output_delay < usecs)
			usecs = XPROCESS (proc)->read_output_delay;
		    }
		}
	      EMACS_SET_SECS_USECS (timeout, 0, usecs);
	      process_output_skip = 0;
	    }
#endif
#if defined (USE_GTK) || defined (HAVE_GCONF) || defined (HAVE_GSETTINGS)
          nfds = xg_select
#elif defined (HAVE_NS)
	  nfds = ns_select
#else
	  nfds = select
#endif
            (max (max_process_desc, max_input_desc) + 1,
             &Available,
             (check_write ? &Writeok : (SELECT_TYPE *)0),
             (SELECT_TYPE *)0, &timeout);

#ifdef HAVE_GNUTLS
          /* GnuTLS buffers data internally.  In lowat mode it leaves
             some data in the TCP buffers so that select works, but
             with custom pull/push functions we need to check if some
             data is available in the buffers manually.  */
          if (nfds == 0)
	    {
	      if (! wait_proc)
		{
		  /* We're not waiting on a specific process, so loop
		     through all the channels and check for data.
		     This is a workaround needed for some versions of
		     the gnutls library -- 2.12.14 has been confirmed
		     to need it.  See
		     http://comments.gmane.org/gmane.emacs.devel/145074 */
		  for (channel = 0; channel < MAXDESC; ++channel)
		    if (! NILP (chan_process[channel]))
		      {
			struct Lisp_Process *p =
			  XPROCESS (chan_process[channel]);
			if (p && p->gnutls_p && p->infd
			    && ((emacs_gnutls_record_check_pending
				 (p->gnutls_state))
				> 0))
			  {
			    nfds++;
			    FD_SET (p->infd, &Available);
			  }
		      }
		}
	      else
		{
		  /* Check this specific channel. */
		  if (wait_proc->gnutls_p /* Check for valid process.  */
		      /* Do we have pending data?  */
		      && ((emacs_gnutls_record_check_pending
			   (wait_proc->gnutls_state))
			  > 0))
		    {
		      nfds = 1;
		      /* Set to Available.  */
		      FD_SET (wait_proc->infd, &Available);
		    }
		}
	    }
#endif
	}

      xerrno = errno;

      /* Make C-g and alarm signals set flags again */
      clear_waiting_for_input ();

      /*  If we woke up due to SIGWINCH, actually change size now.  */
      do_pending_window_change (0);

      if (time_limit && nfds == 0 && ! timeout_reduced_for_timers)
	/* We wanted the full specified time, so return now.  */
	break;
      if (nfds < 0)
	{
	  if (xerrno == EINTR)
	    no_avail = 1;
	  else if (xerrno == EBADF)
	    {
#ifdef AIX
	      /* AIX doesn't handle PTY closure the same way BSD does.  On AIX,
		 the child's closure of the pts gives the parent a SIGHUP, and
		 the ptc file descriptor is automatically closed,
		 yielding EBADF here or at select() call above.
		 So, SIGHUP is ignored (see def of PTY_TTY_NAME_SPRINTF
		 in m/ibmrt-aix.h), and here we just ignore the select error.
		 Cleanup occurs c/o status_notify after SIGCLD. */
	      no_avail = 1; /* Cannot depend on values returned */
#else
	      abort ();
#endif
	    }
	  else
	    error ("select error: %s", emacs_strerror (xerrno));
	}

      if (no_avail)
	{
	  FD_ZERO (&Available);
	  check_write = 0;
	}

#if 0 /* When polling is used, interrupt_input is 0,
	 so get_input_pending should read the input.
	 So this should not be needed.  */
      /* If we are using polling for input,
	 and we see input available, make it get read now.
	 Otherwise it might not actually get read for a second.
	 And on hpux, since we turn off polling in wait_reading_process_output,
	 it might never get read at all if we don't spend much time
	 outside of wait_reading_process_output.  */
      if (read_kbd && interrupt_input
	  && keyboard_bit_set (&Available)
	  && input_polling_used ())
	kill (getpid (), SIGALRM);
#endif

      /* Check for keyboard input */
      /* If there is any, return immediately
	 to give it higher priority than subprocesses */

      if (read_kbd != 0)
	{
	  int old_timers_run = timers_run;
	  struct buffer *old_buffer = current_buffer;
	  Lisp_Object old_window = selected_window;
	  int leave = 0;

	  if (detect_input_pending_run_timers (do_display))
	    {
	      swallow_events (do_display);
	      if (detect_input_pending_run_timers (do_display))
		leave = 1;
	    }

	  /* If a timer has run, this might have changed buffers
	     an alike.  Make read_key_sequence aware of that.  */
	  if (timers_run != old_timers_run
	      && waiting_for_user_input_p == -1
	      && (old_buffer != current_buffer
	      || !EQ (old_window, selected_window)))
	    record_asynch_buffer_change ();

	  if (leave)
	    break;
	}

      /* If there is unread keyboard input, also return.  */
      if (read_kbd != 0
	  && requeued_events_pending_p ())
	break;

      /* If we are not checking for keyboard input now,
	 do process events (but don't run any timers).
	 This is so that X events will be processed.
	 Otherwise they may have to wait until polling takes place.
	 That would causes delays in pasting selections, for example.

	 (We used to do this only if wait_for_cell.)  */
      if (read_kbd == 0 && detect_input_pending ())
	{
	  swallow_events (do_display);
#if 0  /* Exiting when read_kbd doesn't request that seems wrong, though.  */
	  if (detect_input_pending ())
	    break;
#endif
	}

      /* Exit now if the cell we're waiting for became non-nil.  */
      if (! NILP (wait_for_cell) && ! NILP (XCAR (wait_for_cell)))
	break;

#ifdef SIGIO
      /* If we think we have keyboard input waiting, but didn't get SIGIO,
	 go read it.  This can happen with X on BSD after logging out.
	 In that case, there really is no input and no SIGIO,
	 but select says there is input.  */

      if (read_kbd && interrupt_input
	  && keyboard_bit_set (&Available) && ! noninteractive)
	kill (getpid (), SIGIO);
#endif

      if (! wait_proc)
	got_some_input |= nfds > 0;

      /* If checking input just got us a size-change event from X,
	 obey it now if we should.  */
      if (read_kbd || ! NILP (wait_for_cell))
	do_pending_window_change (0);

      /* Check for data from a process.  */
      if (no_avail || nfds == 0)
	continue;

      for (channel = 0; channel <= max_input_desc; ++channel)
        {
          struct fd_callback_data *d = &fd_callback_info[channel];
          if (FD_ISSET (channel, &Available)
              && d->func != 0
              && (d->condition & FOR_READ) != 0)
            d->func (channel, d->data, 1);
          if (FD_ISSET (channel, &write_mask)
              && d->func != 0
              && (d->condition & FOR_WRITE) != 0)
            d->func (channel, d->data, 0);
          }

      for (channel = 0; channel <= max_process_desc; channel++)
	{
	  if (FD_ISSET (channel, &Available)
	      && FD_ISSET (channel, &non_keyboard_wait_mask)
              && !FD_ISSET (channel, &non_process_wait_mask))
	    {
	      int nread;

	      /* If waiting for this channel, arrange to return as
		 soon as no more input to be processed.  No more
		 waiting.  */
	      if (wait_channel == channel)
		{
		  wait_channel = -1;
		  time_limit = -1;
		  got_some_input = 1;
		}
	      proc = chan_process[channel];
	      if (NILP (proc))
		continue;

	      /* If this is a server stream socket, accept connection.  */
	      if (EQ (XPROCESS (proc)->status, Qlisten))
		{
		  server_accept_connection (proc, channel);
		  continue;
		}

	      /* Read data from the process, starting with our
		 buffered-ahead character if we have one.  */

	      nread = read_process_output (proc, channel);
	      if (nread > 0)
		{
		  /* Since read_process_output can run a filter,
		     which can call accept-process-output,
		     don't try to read from any other processes
		     before doing the select again.  */
		  FD_ZERO (&Available);

		  if (do_display)
		    redisplay_preserve_echo_area (12);
		}
#ifdef EWOULDBLOCK
	      else if (nread == -1 && errno == EWOULDBLOCK)
		;
#endif
	      /* ISC 4.1 defines both EWOULDBLOCK and O_NONBLOCK,
		 and Emacs uses O_NONBLOCK, so what we get is EAGAIN.  */
#ifdef O_NONBLOCK
	      else if (nread == -1 && errno == EAGAIN)
		;
#else
#ifdef O_NDELAY
	      else if (nread == -1 && errno == EAGAIN)
		;
	      /* Note that we cannot distinguish between no input
		 available now and a closed pipe.
		 With luck, a closed pipe will be accompanied by
		 subprocess termination and SIGCHLD.  */
	      else if (nread == 0 && !NETCONN_P (proc) && !SERIALCONN_P (proc))
		;
#endif /* O_NDELAY */
#endif /* O_NONBLOCK */
#ifdef HAVE_PTYS
	      /* On some OSs with ptys, when the process on one end of
		 a pty exits, the other end gets an error reading with
		 errno = EIO instead of getting an EOF (0 bytes read).
		 Therefore, if we get an error reading and errno =
		 EIO, just continue, because the child process has
		 exited and should clean itself up soon (e.g. when we
		 get a SIGCHLD).

		 However, it has been known to happen that the SIGCHLD
		 got lost.  So raise the signal again just in case.
		 It can't hurt.  */
	      else if (nread == -1 && errno == EIO)
		{
		  struct Lisp_Process *p = XPROCESS (proc);

		  /* Clear the descriptor now, so we only raise the
		     signal once.  */
		  FD_CLR (channel, &input_wait_mask);
		  FD_CLR (channel, &non_keyboard_wait_mask);

		  if (p->pid == -2)
		    {
		      /* If the EIO occurs on a pty, sigchld_handler's
			 wait3() will not find the process object to
			 delete.  Do it here.  */
		      p->tick = ++process_tick;
		      p->status = Qfailed;
		    }
                  else
		    kill (getpid (), SIGCHLD);
		}
#endif /* HAVE_PTYS */
	      /* If we can detect process termination, don't consider the
		 process gone just because its pipe is closed.  */
#ifdef SIGCHLD
	      else if (nread == 0 && !NETCONN_P (proc) && !SERIALCONN_P (proc))
		;
#endif
	      else
		{
		  /* Preserve status of processes already terminated.  */
		  XPROCESS (proc)->tick = ++process_tick;
		  deactivate_process (proc);
		  if (XPROCESS (proc)->raw_status_new)
		    update_status (XPROCESS (proc));
		  if (EQ (XPROCESS (proc)->status, Qrun))
		    XPROCESS (proc)->status
		      = Fcons (Qexit, Fcons (make_number (256), Qnil));
		}
	    }
#ifdef NON_BLOCKING_CONNECT
	  if (FD_ISSET (channel, &Writeok)
	      && FD_ISSET (channel, &connect_wait_mask))
	    {
	      struct Lisp_Process *p;

	      FD_CLR (channel, &connect_wait_mask);
              FD_CLR (channel, &write_mask);
	      if (--num_pending_connects < 0)
		abort ();

	      proc = chan_process[channel];
	      if (NILP (proc))
		continue;

	      p = XPROCESS (proc);

#ifdef GNU_LINUX
	      /* getsockopt(,,SO_ERROR,,) is said to hang on some systems.
		 So only use it on systems where it is known to work.  */
	      {
		socklen_t xlen = sizeof (xerrno);
		if (getsockopt (channel, SOL_SOCKET, SO_ERROR, &xerrno, &xlen))
		  xerrno = errno;
	      }
#else
	      {
		struct sockaddr pname;
		int pnamelen = sizeof (pname);

		/* If connection failed, getpeername will fail.  */
		xerrno = 0;
		if (getpeername (channel, &pname, &pnamelen) < 0)
		  {
		    /* Obtain connect failure code through error slippage.  */
		    char dummy;
		    xerrno = errno;
		    if (errno == ENOTCONN && read (channel, &dummy, 1) < 0)
		      xerrno = errno;
		  }
	      }
#endif
	      if (xerrno)
		{
		  p->tick = ++process_tick;
		  p->status = Fcons (Qfailed, Fcons (make_number (xerrno), Qnil));
		  deactivate_process (proc);
		}
	      else
		{
		  p->status = Qrun;
		  /* Execute the sentinel here.  If we had relied on
		     status_notify to do it later, it will read input
		     from the process before calling the sentinel.  */
		  exec_sentinel (proc, build_string ("open\n"));
		  if (!EQ (p->filter, Qt) && !EQ (p->command, Qt))
		    {
		      FD_SET (p->infd, &input_wait_mask);
		      FD_SET (p->infd, &non_keyboard_wait_mask);
		    }
		}
	    }
#endif /* NON_BLOCKING_CONNECT */
	}			/* end for each file descriptor */
    }				/* end while exit conditions not met */

  unbind_to (count, Qnil);

  /* If calling from keyboard input, do not quit
     since we want to return C-g as an input character.
     Otherwise, do pending quit if requested.  */
  if (read_kbd >= 0)
    {
      /* Prevent input_pending from remaining set if we quit.  */
      clear_input_pending ();
      QUIT;
    }

  return got_some_input;
}

/* Given a list (FUNCTION ARGS...), apply FUNCTION to the ARGS.  */

static Lisp_Object
read_process_output_call (Lisp_Object fun_and_args)
{
  return apply1 (XCAR (fun_and_args), XCDR (fun_and_args));
}

static Lisp_Object
read_process_output_error_handler (Lisp_Object error_val)
{
  cmd_error_internal (error_val, "error in process filter: ");
  Vinhibit_quit = Qt;
  update_echo_area ();
  Fsleep_for (make_number (2), Qnil);
  return Qt;
}

/* Read pending output from the process channel,
   starting with our buffered-ahead character if we have one.
   Yield number of decoded characters read.

   This function reads at most 4096 characters.
   If you want to read all available subprocess output,
   you must call it repeatedly until it returns zero.

   The characters read are decoded according to PROC's coding-system
   for decoding.  */

static int
read_process_output (Lisp_Object proc, register int channel)
{
  register ssize_t nbytes;
  char *chars;
  register Lisp_Object outstream;
  register struct Lisp_Process *p = XPROCESS (proc);
  register EMACS_INT opoint;
  struct coding_system *coding = proc_decode_coding_system[channel];
  int carryover = p->decoding_carryover;
  int readmax = 4096;
  int count = SPECPDL_INDEX ();
  Lisp_Object odeactivate;

  chars = (char *) alloca (carryover + readmax);
  if (carryover)
    /* See the comment above.  */
    memcpy (chars, SDATA (p->decoding_buf), carryover);

#ifdef DATAGRAM_SOCKETS
  /* We have a working select, so proc_buffered_char is always -1.  */
  if (DATAGRAM_CHAN_P (channel))
    {
      socklen_t len = datagram_address[channel].len;
      nbytes = recvfrom (channel, chars + carryover, readmax,
			 0, datagram_address[channel].sa, &len);
    }
  else
#endif
    {
      int buffered = 0 <= proc_buffered_char[channel];
      if (buffered)
	{
	  chars[carryover] = proc_buffered_char[channel];
	  proc_buffered_char[channel] = -1;
	}
#ifdef HAVE_GNUTLS
      if (p->gnutls_p)
	nbytes = emacs_gnutls_read (p, chars + carryover + buffered,
				    readmax - buffered);
      else
#endif
	nbytes = emacs_read (channel, chars + carryover + buffered,
			     readmax - buffered);
#ifdef ADAPTIVE_READ_BUFFERING
      if (nbytes > 0 && p->adaptive_read_buffering)
	{
	  int delay = p->read_output_delay;
	  if (nbytes < 256)
	    {
	      if (delay < READ_OUTPUT_DELAY_MAX_MAX)
		{
		  if (delay == 0)
		    process_output_delay_count++;
		  delay += READ_OUTPUT_DELAY_INCREMENT * 2;
		}
	    }
	  else if (delay > 0 && nbytes == readmax - buffered)
	    {
	      delay -= READ_OUTPUT_DELAY_INCREMENT;
	      if (delay == 0)
		process_output_delay_count--;
	    }
	  p->read_output_delay = delay;
	  if (delay)
	    {
	      p->read_output_skip = 1;
	      process_output_skip = 1;
	    }
	}
#endif
      nbytes += buffered;
      nbytes += buffered && nbytes <= 0;
    }

  p->decoding_carryover = 0;

  /* At this point, NBYTES holds number of bytes just received
     (including the one in proc_buffered_char[channel]).  */
  if (nbytes <= 0)
    {
      if (nbytes < 0 || coding->mode & CODING_MODE_LAST_BLOCK)
	return nbytes;
      coding->mode |= CODING_MODE_LAST_BLOCK;
    }

  /* Now set NBYTES how many bytes we must decode.  */
  nbytes += carryover;

  odeactivate = Vdeactivate_mark;
  /* There's no good reason to let process filters change the current
     buffer, and many callers of accept-process-output, sit-for, and
     friends don't expect current-buffer to be changed from under them.  */
  record_unwind_protect (set_buffer_if_live, Fcurrent_buffer ());

  /* Read and dispose of the process output.  */
  outstream = p->filter;
  if (!NILP (outstream))
    {
      Lisp_Object text;
      int outer_running_asynch_code = running_asynch_code;
      int waiting = waiting_for_user_input_p;

      /* No need to gcpro these, because all we do with them later
	 is test them for EQness, and none of them should be a string.  */
#if 0
      Lisp_Object obuffer, okeymap;
      XSETBUFFER (obuffer, current_buffer);
      okeymap = BVAR (current_buffer, keymap);
#endif

      /* We inhibit quit here instead of just catching it so that
	 hitting ^G when a filter happens to be running won't screw
	 it up.  */
      specbind (Qinhibit_quit, Qt);
      specbind (Qlast_nonmenu_event, Qt);

      /* In case we get recursively called,
	 and we already saved the match data nonrecursively,
	 save the same match data in safely recursive fashion.  */
      if (outer_running_asynch_code)
	{
	  Lisp_Object tem;
	  /* Don't clobber the CURRENT match data, either!  */
	  tem = Fmatch_data (Qnil, Qnil, Qnil);
	  restore_search_regs ();
	  record_unwind_save_match_data ();
	  Fset_match_data (tem, Qt);
	}

      /* For speed, if a search happens within this code,
	 save the match data in a special nonrecursive fashion.  */
      running_asynch_code = 1;

      decode_coding_c_string (coding, (unsigned char *) chars, nbytes, Qt);
      text = coding->dst_object;
      Vlast_coding_system_used = CODING_ID_NAME (coding->id);
      /* A new coding system might be found.  */
      if (!EQ (p->decode_coding_system, Vlast_coding_system_used))
	{
	  p->decode_coding_system = Vlast_coding_system_used;

	  /* Don't call setup_coding_system for
	     proc_decode_coding_system[channel] here.  It is done in
	     detect_coding called via decode_coding above.  */

	  /* If a coding system for encoding is not yet decided, we set
	     it as the same as coding-system for decoding.

	     But, before doing that we must check if
	     proc_encode_coding_system[p->outfd] surely points to a
	     valid memory because p->outfd will be changed once EOF is
	     sent to the process.  */
	  if (NILP (p->encode_coding_system)
	      && proc_encode_coding_system[p->outfd])
	    {
	      p->encode_coding_system
		= coding_inherit_eol_type (Vlast_coding_system_used, Qnil);
	      setup_coding_system (p->encode_coding_system,
				   proc_encode_coding_system[p->outfd]);
	    }
	}

      if (coding->carryover_bytes > 0)
	{
	  if (SCHARS (p->decoding_buf) < coding->carryover_bytes)
	    p->decoding_buf = make_uninit_string (coding->carryover_bytes);
	  memcpy (SDATA (p->decoding_buf), coding->carryover,
		  coding->carryover_bytes);
	  p->decoding_carryover = coding->carryover_bytes;
	}
      if (SBYTES (text) > 0)
	/* FIXME: It's wrong to wrap or not based on debug-on-error, and
	   sometimes it's simply wrong to wrap (e.g. when called from
	   accept-process-output).  */
	internal_condition_case_1 (read_process_output_call,
				   Fcons (outstream,
					  Fcons (proc, Fcons (text, Qnil))),
				   !NILP (Vdebug_on_error) ? Qnil : Qerror,
				   read_process_output_error_handler);

      /* If we saved the match data nonrecursively, restore it now.  */
      restore_search_regs ();
      running_asynch_code = outer_running_asynch_code;

      /* Restore waiting_for_user_input_p as it was
	 when we were called, in case the filter clobbered it.  */
      waiting_for_user_input_p = waiting;

#if 0 /* Call record_asynch_buffer_change unconditionally,
	 because we might have changed minor modes or other things
	 that affect key bindings.  */
      if (! EQ (Fcurrent_buffer (), obuffer)
	  || ! EQ (current_buffer->keymap, okeymap))
#endif
	/* But do it only if the caller is actually going to read events.
	   Otherwise there's no need to make him wake up, and it could
	   cause trouble (for example it would make sit_for return).  */
	if (waiting_for_user_input_p == -1)
	  record_asynch_buffer_change ();
    }

  /* If no filter, write into buffer if it isn't dead.  */
  else if (!NILP (p->buffer) && !NILP (BVAR (XBUFFER (p->buffer), name)))
    {
      Lisp_Object old_read_only;
      EMACS_INT old_begv, old_zv;
      EMACS_INT old_begv_byte, old_zv_byte;
      EMACS_INT before, before_byte;
      EMACS_INT opoint_byte;
      Lisp_Object text;
      struct buffer *b;

      Fset_buffer (p->buffer);
      opoint = PT;
      opoint_byte = PT_BYTE;
      old_read_only = BVAR (current_buffer, read_only);
      old_begv = BEGV;
      old_zv = ZV;
      old_begv_byte = BEGV_BYTE;
      old_zv_byte = ZV_BYTE;

      BVAR (current_buffer, read_only) = Qnil;

      /* Insert new output into buffer
	 at the current end-of-output marker,
	 thus preserving logical ordering of input and output.  */
      if (XMARKER (p->mark)->buffer)
	SET_PT_BOTH (clip_to_bounds (BEGV, marker_position (p->mark), ZV),
		     clip_to_bounds (BEGV_BYTE, marker_byte_position (p->mark),
				     ZV_BYTE));
      else
	SET_PT_BOTH (ZV, ZV_BYTE);
      before = PT;
      before_byte = PT_BYTE;

      /* If the output marker is outside of the visible region, save
	 the restriction and widen.  */
      if (! (BEGV <= PT && PT <= ZV))
	Fwiden ();

      decode_coding_c_string (coding, (unsigned char *) chars, nbytes, Qt);
      text = coding->dst_object;
      Vlast_coding_system_used = CODING_ID_NAME (coding->id);
      /* A new coding system might be found.  See the comment in the
	 similar code in the previous `if' block.  */
      if (!EQ (p->decode_coding_system, Vlast_coding_system_used))
	{
	  p->decode_coding_system = Vlast_coding_system_used;
	  if (NILP (p->encode_coding_system)
	      && proc_encode_coding_system[p->outfd])
	    {
	      p->encode_coding_system
		= coding_inherit_eol_type (Vlast_coding_system_used, Qnil);
	      setup_coding_system (p->encode_coding_system,
				   proc_encode_coding_system[p->outfd]);
	    }
	}
      if (coding->carryover_bytes > 0)
	{
	  if (SCHARS (p->decoding_buf) < coding->carryover_bytes)
	    p->decoding_buf = make_uninit_string (coding->carryover_bytes);
	  memcpy (SDATA (p->decoding_buf), coding->carryover,
		  coding->carryover_bytes);
	  p->decoding_carryover = coding->carryover_bytes;
	}
      /* Adjust the multibyteness of TEXT to that of the buffer.  */
      if (NILP (BVAR (current_buffer, enable_multibyte_characters))
	  != ! STRING_MULTIBYTE (text))
	text = (STRING_MULTIBYTE (text)
		? Fstring_as_unibyte (text)
		: Fstring_to_multibyte (text));
      /* Insert before markers in case we are inserting where
	 the buffer's mark is, and the user's next command is Meta-y.  */
      insert_from_string_before_markers (text, 0, 0,
					 SCHARS (text), SBYTES (text), 0);

      /* Make sure the process marker's position is valid when the
	 process buffer is changed in the signal_after_change above.
	 W3 is known to do that.  */
      if (BUFFERP (p->buffer)
	  && (b = XBUFFER (p->buffer), b != current_buffer))
	set_marker_both (p->mark, p->buffer, BUF_PT (b), BUF_PT_BYTE (b));
      else
	set_marker_both (p->mark, p->buffer, PT, PT_BYTE);

      update_mode_lines++;

      /* Make sure opoint and the old restrictions
	 float ahead of any new text just as point would.  */
      if (opoint >= before)
	{
	  opoint += PT - before;
	  opoint_byte += PT_BYTE - before_byte;
	}
      if (old_begv > before)
	{
	  old_begv += PT - before;
	  old_begv_byte += PT_BYTE - before_byte;
	}
      if (old_zv >= before)
	{
	  old_zv += PT - before;
	  old_zv_byte += PT_BYTE - before_byte;
	}

      /* If the restriction isn't what it should be, set it.  */
      if (old_begv != BEGV || old_zv != ZV)
	Fnarrow_to_region (make_number (old_begv), make_number (old_zv));


      BVAR (current_buffer, read_only) = old_read_only;
      SET_PT_BOTH (opoint, opoint_byte);
    }
  /* Handling the process output should not deactivate the mark.  */
  Vdeactivate_mark = odeactivate;

  unbind_to (count, Qnil);
  return nbytes;
}

/* Sending data to subprocess */

static jmp_buf send_process_frame;
static Lisp_Object process_sent_to;

#ifndef FORWARD_SIGNAL_TO_MAIN_THREAD
static void send_process_trap (int) NO_RETURN;
#endif

static void
send_process_trap (int ignore)
{
  SIGNAL_THREAD_CHECK (SIGPIPE);
  sigunblock (sigmask (SIGPIPE));
  longjmp (send_process_frame, 1);
}

/* Send some data to process PROC.
   BUF is the beginning of the data; LEN is the number of characters.
   OBJECT is the Lisp object that the data comes from.  If OBJECT is
   nil or t, it means that the data comes from C string.

   If OBJECT is not nil, the data is encoded by PROC's coding-system
   for encoding before it is sent.

   This function can evaluate Lisp code and can garbage collect.  */

static void
send_process (volatile Lisp_Object proc, const char *volatile buf,
	      volatile EMACS_INT len, volatile Lisp_Object object)
{
  /* Use volatile to protect variables from being clobbered by longjmp.  */
  struct Lisp_Process *p = XPROCESS (proc);
  ssize_t rv;
  struct coding_system *coding;
  struct gcpro gcpro1;
  void (*volatile old_sigpipe) (int);

  GCPRO1 (object);

  if (p->raw_status_new)
    update_status (p);
  if (! EQ (p->status, Qrun))
    error ("Process %s not running", SDATA (p->name));
  if (p->outfd < 0)
    error ("Output file descriptor of %s is closed", SDATA (p->name));

  coding = proc_encode_coding_system[p->outfd];
  Vlast_coding_system_used = CODING_ID_NAME (coding->id);

  if ((STRINGP (object) && STRING_MULTIBYTE (object))
      || (BUFFERP (object)
	  && !NILP (BVAR (XBUFFER (object), enable_multibyte_characters)))
      || EQ (object, Qt))
    {
      p->encode_coding_system
	= complement_process_encoding_system (p->encode_coding_system);
      if (!EQ (Vlast_coding_system_used, p->encode_coding_system))
	{
	  /* The coding system for encoding was changed to raw-text
	     because we sent a unibyte text previously.  Now we are
	     sending a multibyte text, thus we must encode it by the
	     original coding system specified for the current process.

	     Another reason we come here is that the coding system
	     was just complemented and a new one was returned by
	     complement_process_encoding_system.  */
	  setup_coding_system (p->encode_coding_system, coding);
	  Vlast_coding_system_used = p->encode_coding_system;
	}
      coding->src_multibyte = 1;
    }
  else
    {
      coding->src_multibyte = 0;
      /* For sending a unibyte text, character code conversion should
	 not take place but EOL conversion should.  So, setup raw-text
	 or one of the subsidiary if we have not yet done it.  */
      if (CODING_REQUIRE_ENCODING (coding))
	{
	  if (CODING_REQUIRE_FLUSHING (coding))
	    {
	      /* But, before changing the coding, we must flush out data.  */
	      coding->mode |= CODING_MODE_LAST_BLOCK;
	      send_process (proc, "", 0, Qt);
	      coding->mode &= CODING_MODE_LAST_BLOCK;
	    }
	  setup_coding_system (raw_text_coding_system
			       (Vlast_coding_system_used),
			       coding);
	  coding->src_multibyte = 0;
	}
    }
  coding->dst_multibyte = 0;

  if (CODING_REQUIRE_ENCODING (coding))
    {
      coding->dst_object = Qt;
      if (BUFFERP (object))
	{
	  EMACS_INT from_byte, from, to;
	  EMACS_INT save_pt, save_pt_byte;
	  struct buffer *cur = current_buffer;

	  set_buffer_internal (XBUFFER (object));
	  save_pt = PT, save_pt_byte = PT_BYTE;

	  from_byte = PTR_BYTE_POS ((unsigned char *) buf);
	  from = BYTE_TO_CHAR (from_byte);
	  to = BYTE_TO_CHAR (from_byte + len);
	  TEMP_SET_PT_BOTH (from, from_byte);
	  encode_coding_object (coding, object, from, from_byte,
				to, from_byte + len, Qt);
	  TEMP_SET_PT_BOTH (save_pt, save_pt_byte);
	  set_buffer_internal (cur);
	}
      else if (STRINGP (object))
	{
	  encode_coding_object (coding, object, 0, 0, SCHARS (object),
				SBYTES (object), Qt);
	}
      else
	{
	  coding->dst_object = make_unibyte_string (buf, len);
	  coding->produced = len;
	}

      len = coding->produced;
      object = coding->dst_object;
      buf = SSDATA (object);
    }

  if (pty_max_bytes == 0)
    {
#if defined (HAVE_FPATHCONF) && defined (_PC_MAX_CANON)
      pty_max_bytes = fpathconf (p->outfd, _PC_MAX_CANON);
      if (pty_max_bytes < 0)
	pty_max_bytes = 250;
#else
      pty_max_bytes = 250;
#endif
      /* Deduct one, to leave space for the eof.  */
      pty_max_bytes--;
    }

  /* 2000-09-21: Emacs 20.7, sparc-sun-solaris-2.6, GCC 2.95.2,
     CFLAGS="-g -O": The value of the parameter `proc' is clobbered
     when returning with longjmp despite being declared volatile.  */
  if (!setjmp (send_process_frame))
    {
      p = XPROCESS (proc);  /* Repair any setjmp clobbering.  */

      process_sent_to = proc;
      while (len > 0)
	{
	  EMACS_INT this = len;

	  /* Send this batch, using one or more write calls.  */
	  while (this > 0)
	    {
	      EMACS_INT written = 0;
	      int outfd = p->outfd;
	      old_sigpipe = (void (*) (int)) signal (SIGPIPE, send_process_trap);
#ifdef DATAGRAM_SOCKETS
	      if (DATAGRAM_CHAN_P (outfd))
		{
		  rv = sendto (outfd, buf, this,
			       0, datagram_address[outfd].sa,
			       datagram_address[outfd].len);
		  if (0 <= rv)
		    written = rv;
		  else if (errno == EMSGSIZE)
		    {
		      signal (SIGPIPE, old_sigpipe);
		      report_file_error ("sending datagram",
					 Fcons (proc, Qnil));
		    }
		}
	      else
#endif
		{
#ifdef HAVE_GNUTLS
		  if (p->gnutls_p)
		    written = emacs_gnutls_write (p, buf, this);
		  else
#endif
		    written = emacs_write (outfd, buf, this);
		  rv = (written ? 0 : -1);
#ifdef ADAPTIVE_READ_BUFFERING
		  if (p->read_output_delay > 0
		      && p->adaptive_read_buffering == 1)
		    {
		      p->read_output_delay = 0;
		      process_output_delay_count--;
		      p->read_output_skip = 0;
		    }
#endif
		}
	      signal (SIGPIPE, old_sigpipe);

	      if (rv < 0)
		{
		  if (0
#ifdef EWOULDBLOCK
		      || errno == EWOULDBLOCK
#endif
#ifdef EAGAIN
		      || errno == EAGAIN
#endif
		      )
		    /* Buffer is full.  Wait, accepting input;
		       that may allow the program
		       to finish doing output and read more.  */
		    {
		      EMACS_INT offset = 0;

#ifdef BROKEN_PTY_READ_AFTER_EAGAIN
		      /* A gross hack to work around a bug in FreeBSD.
			 In the following sequence, read(2) returns
			 bogus data:

			 write(2)	 1022 bytes
			 write(2)   954 bytes, get EAGAIN
			 read(2)   1024 bytes in process_read_output
			 read(2)     11 bytes in process_read_output

			 That is, read(2) returns more bytes than have
			 ever been written successfully.  The 1033 bytes
			 read are the 1022 bytes written successfully
			 after processing (for example with CRs added if
			 the terminal is set up that way which it is
			 here).  The same bytes will be seen again in a
			 later read(2), without the CRs.  */

		      if (errno == EAGAIN)
			{
			  int flags = FWRITE;
			  ioctl (p->outfd, TIOCFLUSH, &flags);
			}
#endif /* BROKEN_PTY_READ_AFTER_EAGAIN */

		      /* Running filters might relocate buffers or strings.
			 Arrange to relocate BUF.  */
		      if (BUFFERP (object))
			offset = BUF_PTR_BYTE_POS (XBUFFER (object),
						   (unsigned char *) buf);
		      else if (STRINGP (object))
			offset = buf - SSDATA (object);

#ifdef EMACS_HAS_USECS
		      wait_reading_process_output (0, 20000, 0, 0, Qnil, NULL, 0);
#else
		      wait_reading_process_output (1, 0, 0, 0, Qnil, NULL, 0);
#endif

		      if (BUFFERP (object))
			buf = (char *) BUF_BYTE_ADDRESS (XBUFFER (object),
							 offset);
		      else if (STRINGP (object))
			buf = offset + SSDATA (object);
		    }
		  else
		    /* This is a real error.  */
		    report_file_error ("writing to process", Fcons (proc, Qnil));
		}
	      buf += written;
	      len -= written;
	      this -= written;
	    }
	}
    }
  else
    {
      signal (SIGPIPE, old_sigpipe);
      proc = process_sent_to;
      p = XPROCESS (proc);
      p->raw_status_new = 0;
      p->status = Fcons (Qexit, Fcons (make_number (256), Qnil));
      p->tick = ++process_tick;
      deactivate_process (proc);
      error ("SIGPIPE raised on process %s; closed it", SDATA (p->name));
    }

  UNGCPRO;
}

DEFUN ("process-send-region", Fprocess_send_region, Sprocess_send_region,
       3, 3, 0,
       doc: /* Send current contents of region as input to PROCESS.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.
Called from program, takes three arguments, PROCESS, START and END.
If the region is more than 500 characters long,
it is sent in several bunches.  This may happen even for shorter regions.
Output from processes can arrive in between bunches.  */)
  (Lisp_Object process, Lisp_Object start, Lisp_Object end)
{
  Lisp_Object proc;
  EMACS_INT start1, end1;

  proc = get_process (process);
  validate_region (&start, &end);

  if (XINT (start) < GPT && XINT (end) > GPT)
    move_gap (XINT (start));

  start1 = CHAR_TO_BYTE (XINT (start));
  end1 = CHAR_TO_BYTE (XINT (end));
  send_process (proc, (char *) BYTE_POS_ADDR (start1), end1 - start1,
		Fcurrent_buffer ());

  return Qnil;
}

DEFUN ("process-send-string", Fprocess_send_string, Sprocess_send_string,
       2, 2, 0,
       doc: /* Send PROCESS the contents of STRING as input.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.
If STRING is more than 500 characters long,
it is sent in several bunches.  This may happen even for shorter strings.
Output from processes can arrive in between bunches.  */)
  (Lisp_Object process, Lisp_Object string)
{
  Lisp_Object proc;
  CHECK_STRING (string);
  proc = get_process (process);
  send_process (proc, SSDATA (string),
		SBYTES (string), string);
  return Qnil;
}

/* Return the foreground process group for the tty/pty that
   the process P uses.  */
static int
emacs_get_tty_pgrp (struct Lisp_Process *p)
{
  int gid = -1;

#ifdef TIOCGPGRP
  if (ioctl (p->infd, TIOCGPGRP, &gid) == -1 && ! NILP (p->tty_name))
    {
      int fd;
      /* Some OS:es (Solaris 8/9) does not allow TIOCGPGRP from the
	 master side.  Try the slave side.  */
      fd = emacs_open (SSDATA (p->tty_name), O_RDONLY, 0);

      if (fd != -1)
	{
	  ioctl (fd, TIOCGPGRP, &gid);
	  emacs_close (fd);
	}
    }
#endif /* defined (TIOCGPGRP ) */

  return gid;
}

DEFUN ("process-running-child-p", Fprocess_running_child_p,
       Sprocess_running_child_p, 0, 1, 0,
       doc: /* Return t if PROCESS has given the terminal to a child.
If the operating system does not make it possible to find out,
return t unconditionally.  */)
  (Lisp_Object process)
{
  /* Initialize in case ioctl doesn't exist or gives an error,
     in a way that will cause returning t.  */
  int gid;
  Lisp_Object proc;
  struct Lisp_Process *p;

  proc = get_process (process);
  p = XPROCESS (proc);

  if (!EQ (p->type, Qreal))
    error ("Process %s is not a subprocess",
	   SDATA (p->name));
  if (p->infd < 0)
    error ("Process %s is not active",
	   SDATA (p->name));

  gid = emacs_get_tty_pgrp (p);

  if (gid == p->pid)
    return Qnil;
  return Qt;
}

/* send a signal number SIGNO to PROCESS.
   If CURRENT_GROUP is t, that means send to the process group
   that currently owns the terminal being used to communicate with PROCESS.
   This is used for various commands in shell mode.
   If CURRENT_GROUP is lambda, that means send to the process group
   that currently owns the terminal, but only if it is NOT the shell itself.

   If NOMSG is zero, insert signal-announcements into process's buffers
   right away.

   If we can, we try to signal PROCESS by sending control characters
   down the pty.  This allows us to signal inferiors who have changed
   their uid, for which killpg would return an EPERM error.  */

static void
process_send_signal (Lisp_Object process, int signo, Lisp_Object current_group,
		     int nomsg)
{
  Lisp_Object proc;
  register struct Lisp_Process *p;
  int gid;
  int no_pgrp = 0;

  proc = get_process (process);
  p = XPROCESS (proc);

  if (!EQ (p->type, Qreal))
    error ("Process %s is not a subprocess",
	   SDATA (p->name));
  if (p->infd < 0)
    error ("Process %s is not active",
	   SDATA (p->name));

  if (!p->pty_flag)
    current_group = Qnil;

  /* If we are using pgrps, get a pgrp number and make it negative.  */
  if (NILP (current_group))
    /* Send the signal to the shell's process group.  */
    gid = p->pid;
  else
    {
#ifdef SIGNALS_VIA_CHARACTERS
      /* If possible, send signals to the entire pgrp
	 by sending an input character to it.  */

      struct termios t;
      cc_t *sig_char = NULL;

      tcgetattr (p->infd, &t);

      switch (signo)
	{
	case SIGINT:
	  sig_char = &t.c_cc[VINTR];
	  break;

	case SIGQUIT:
	  sig_char = &t.c_cc[VQUIT];
	  break;

  	case SIGTSTP:
#if defined (VSWTCH) && !defined (PREFER_VSUSP)
	  sig_char = &t.c_cc[VSWTCH];
#else
	  sig_char = &t.c_cc[VSUSP];
#endif
	  break;
	}

      if (sig_char && *sig_char != CDISABLE)
	{
	  send_process (proc, (char *) sig_char, 1, Qnil);
	  return;
	}
      /* If we can't send the signal with a character,
	 fall through and send it another way.  */

      /* The code above may fall through if it can't
	 handle the signal.  */
#endif /* defined (SIGNALS_VIA_CHARACTERS) */

#ifdef TIOCGPGRP
      /* Get the current pgrp using the tty itself, if we have that.
	 Otherwise, use the pty to get the pgrp.
	 On pfa systems, saka@pfu.fujitsu.co.JP writes:
	 "TIOCGPGRP symbol defined in sys/ioctl.h at E50.
	 But, TIOCGPGRP does not work on E50 ;-P works fine on E60"
	 His patch indicates that if TIOCGPGRP returns an error, then
	 we should just assume that p->pid is also the process group id.  */

      gid = emacs_get_tty_pgrp (p);

      if (gid == -1)
	/* If we can't get the information, assume
	   the shell owns the tty.  */
	gid = p->pid;

      /* It is not clear whether anything really can set GID to -1.
	 Perhaps on some system one of those ioctls can or could do so.
	 Or perhaps this is vestigial.  */
      if (gid == -1)
	no_pgrp = 1;
#else  /* ! defined (TIOCGPGRP ) */
      /* Can't select pgrps on this system, so we know that
	 the child itself heads the pgrp.  */
      gid = p->pid;
#endif /* ! defined (TIOCGPGRP ) */

      /* If current_group is lambda, and the shell owns the terminal,
	 don't send any signal.  */
      if (EQ (current_group, Qlambda) && gid == p->pid)
	return;
    }

  switch (signo)
    {
#ifdef SIGCONT
    case SIGCONT:
      p->raw_status_new = 0;
      p->status = Qrun;
      p->tick = ++process_tick;
      if (!nomsg)
	{
	  status_notify (NULL);
	  redisplay_preserve_echo_area (13);
	}
      break;
#endif /* ! defined (SIGCONT) */
    case SIGINT:
    case SIGQUIT:
    case SIGKILL:
      flush_pending_output (p->infd);
      break;
    }

  /* If we don't have process groups, send the signal to the immediate
     subprocess.  That isn't really right, but it's better than any
     obvious alternative.  */
  if (no_pgrp)
    {
      kill (p->pid, signo);
      return;
    }

  /* gid may be a pid, or minus a pgrp's number */
#ifdef TIOCSIGSEND
  if (!NILP (current_group))
    {
      if (ioctl (p->infd, TIOCSIGSEND, signo) == -1)
	EMACS_KILLPG (gid, signo);
    }
  else
    {
      gid = - p->pid;
      kill (gid, signo);
    }
#else /* ! defined (TIOCSIGSEND) */
  EMACS_KILLPG (gid, signo);
#endif /* ! defined (TIOCSIGSEND) */
}

DEFUN ("interrupt-process", Finterrupt_process, Sinterrupt_process, 0, 2, 0,
       doc: /* Interrupt process PROCESS.
PROCESS may be a process, a buffer, or the name of a process or buffer.
No arg or nil means current buffer's process.
Second arg CURRENT-GROUP non-nil means send signal to
the current process-group of the process's controlling terminal
rather than to the process's own process group.
If the process is a shell, this means interrupt current subjob
rather than the shell.

If CURRENT-GROUP is `lambda', and if the shell owns the terminal,
don't send the signal.  */)
  (Lisp_Object process, Lisp_Object current_group)
{
  process_send_signal (process, SIGINT, current_group, 0);
  return process;
}

DEFUN ("kill-process", Fkill_process, Skill_process, 0, 2, 0,
       doc: /* Kill process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.  */)
  (Lisp_Object process, Lisp_Object current_group)
{
  process_send_signal (process, SIGKILL, current_group, 0);
  return process;
}

DEFUN ("quit-process", Fquit_process, Squit_process, 0, 2, 0,
       doc: /* Send QUIT signal to process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.  */)
  (Lisp_Object process, Lisp_Object current_group)
{
  process_send_signal (process, SIGQUIT, current_group, 0);
  return process;
}

DEFUN ("stop-process", Fstop_process, Sstop_process, 0, 2, 0,
       doc: /* Stop process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.
If PROCESS is a network or serial process, inhibit handling of incoming
traffic.  */)
  (Lisp_Object process, Lisp_Object current_group)
{
  if (PROCESSP (process) && (NETCONN_P (process) || SERIALCONN_P (process)))
    {
      struct Lisp_Process *p;

      p = XPROCESS (process);
      if (NILP (p->command)
	  && p->infd >= 0)
	{
	  FD_CLR (p->infd, &input_wait_mask);
	  FD_CLR (p->infd, &non_keyboard_wait_mask);
	}
      p->command = Qt;
      return process;
    }
#ifndef SIGTSTP
  error ("No SIGTSTP support");
#else
  process_send_signal (process, SIGTSTP, current_group, 0);
#endif
  return process;
}

DEFUN ("continue-process", Fcontinue_process, Scontinue_process, 0, 2, 0,
       doc: /* Continue process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.
If PROCESS is a network or serial process, resume handling of incoming
traffic.  */)
  (Lisp_Object process, Lisp_Object current_group)
{
  if (PROCESSP (process) && (NETCONN_P (process) || SERIALCONN_P (process)))
    {
      struct Lisp_Process *p;

      p = XPROCESS (process);
      if (EQ (p->command, Qt)
	  && p->infd >= 0
	  && (!EQ (p->filter, Qt) || EQ (p->status, Qlisten)))
	{
	  FD_SET (p->infd, &input_wait_mask);
	  FD_SET (p->infd, &non_keyboard_wait_mask);
#ifdef WINDOWSNT
	  if (fd_info[ p->infd ].flags & FILE_SERIAL)
	    PurgeComm (fd_info[ p->infd ].hnd, PURGE_RXABORT | PURGE_RXCLEAR);
#else /* not WINDOWSNT */
	  tcflush (p->infd, TCIFLUSH);
#endif /* not WINDOWSNT */
	}
      p->command = Qnil;
      return process;
    }
#ifdef SIGCONT
    process_send_signal (process, SIGCONT, current_group, 0);
#else
    error ("No SIGCONT support");
#endif
  return process;
}

DEFUN ("signal-process", Fsignal_process, Ssignal_process,
       2, 2, "sProcess (name or number): \nnSignal code: ",
       doc: /* Send PROCESS the signal with code SIGCODE.
PROCESS may also be a number specifying the process id of the
process to signal; in this case, the process need not be a child of
this Emacs.
SIGCODE may be an integer, or a symbol whose name is a signal name.  */)
  (Lisp_Object process, Lisp_Object sigcode)
{
  pid_t pid;

  if (INTEGERP (process))
    {
      pid = XINT (process);
      goto got_it;
    }

  if (FLOATP (process))
    {
      pid = (pid_t) XFLOAT_DATA (process);
      goto got_it;
    }

  if (STRINGP (process))
    {
      Lisp_Object tem;
      if (tem = Fget_process (process), NILP (tem))
	{
	  pid = XINT (Fstring_to_number (process, make_number (10)));
	  if (pid > 0)
	    goto got_it;
	}
      process = tem;
    }
  else
    process = get_process (process);

  if (NILP (process))
    return process;

  CHECK_PROCESS (process);
  pid = XPROCESS (process)->pid;
  if (pid <= 0)
    error ("Cannot signal process %s", SDATA (XPROCESS (process)->name));

 got_it:

#define parse_signal(NAME, VALUE)		\
  else if (!xstrcasecmp (name, NAME))		\
    XSETINT (sigcode, VALUE)

  if (INTEGERP (sigcode))
    ;
  else
    {
      char *name;

      CHECK_SYMBOL (sigcode);
      name = SSDATA (SYMBOL_NAME (sigcode));

      if (!strncmp (name, "SIG", 3) || !strncmp (name, "sig", 3))
	name += 3;

      if (0)
	;
#ifdef SIGUSR1
      parse_signal ("usr1", SIGUSR1);
#endif
#ifdef SIGUSR2
      parse_signal ("usr2", SIGUSR2);
#endif
#ifdef SIGTERM
      parse_signal ("term", SIGTERM);
#endif
#ifdef SIGHUP
      parse_signal ("hup", SIGHUP);
#endif
#ifdef SIGINT
      parse_signal ("int", SIGINT);
#endif
#ifdef SIGQUIT
      parse_signal ("quit", SIGQUIT);
#endif
#ifdef SIGILL
      parse_signal ("ill", SIGILL);
#endif
#ifdef SIGABRT
      parse_signal ("abrt", SIGABRT);
#endif
#ifdef SIGEMT
      parse_signal ("emt", SIGEMT);
#endif
#ifdef SIGKILL
      parse_signal ("kill", SIGKILL);
#endif
#ifdef SIGFPE
      parse_signal ("fpe", SIGFPE);
#endif
#ifdef SIGBUS
      parse_signal ("bus", SIGBUS);
#endif
#ifdef SIGSEGV
      parse_signal ("segv", SIGSEGV);
#endif
#ifdef SIGSYS
      parse_signal ("sys", SIGSYS);
#endif
#ifdef SIGPIPE
      parse_signal ("pipe", SIGPIPE);
#endif
#ifdef SIGALRM
      parse_signal ("alrm", SIGALRM);
#endif
#ifdef SIGURG
      parse_signal ("urg", SIGURG);
#endif
#ifdef SIGSTOP
      parse_signal ("stop", SIGSTOP);
#endif
#ifdef SIGTSTP
      parse_signal ("tstp", SIGTSTP);
#endif
#ifdef SIGCONT
      parse_signal ("cont", SIGCONT);
#endif
#ifdef SIGCHLD
      parse_signal ("chld", SIGCHLD);
#endif
#ifdef SIGTTIN
      parse_signal ("ttin", SIGTTIN);
#endif
#ifdef SIGTTOU
      parse_signal ("ttou", SIGTTOU);
#endif
#ifdef SIGIO
      parse_signal ("io", SIGIO);
#endif
#ifdef SIGXCPU
      parse_signal ("xcpu", SIGXCPU);
#endif
#ifdef SIGXFSZ
      parse_signal ("xfsz", SIGXFSZ);
#endif
#ifdef SIGVTALRM
      parse_signal ("vtalrm", SIGVTALRM);
#endif
#ifdef SIGPROF
      parse_signal ("prof", SIGPROF);
#endif
#ifdef SIGWINCH
      parse_signal ("winch", SIGWINCH);
#endif
#ifdef SIGINFO
      parse_signal ("info", SIGINFO);
#endif
      else
	error ("Undefined signal name %s", name);
    }

#undef parse_signal

  return make_number (kill (pid, XINT (sigcode)));
}

DEFUN ("process-send-eof", Fprocess_send_eof, Sprocess_send_eof, 0, 1, 0,
       doc: /* Make PROCESS see end-of-file in its input.
EOF comes after any text already sent to it.
PROCESS may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process.
If PROCESS is a network connection, or is a process communicating
through a pipe (as opposed to a pty), then you cannot send any more
text to PROCESS after you call this function.
If PROCESS is a serial process, wait until all output written to the
process has been transmitted to the serial port.  */)
  (Lisp_Object process)
{
  Lisp_Object proc;
  struct coding_system *coding;

  if (DATAGRAM_CONN_P (process))
    return process;

  proc = get_process (process);
  coding = proc_encode_coding_system[XPROCESS (proc)->outfd];

  /* Make sure the process is really alive.  */
  if (XPROCESS (proc)->raw_status_new)
    update_status (XPROCESS (proc));
  if (! EQ (XPROCESS (proc)->status, Qrun))
    error ("Process %s not running", SDATA (XPROCESS (proc)->name));

  if (CODING_REQUIRE_FLUSHING (coding))
    {
      coding->mode |= CODING_MODE_LAST_BLOCK;
      send_process (proc, "", 0, Qnil);
    }

  if (XPROCESS (proc)->pty_flag)
    send_process (proc, "\004", 1, Qnil);
  else if (EQ (XPROCESS (proc)->type, Qserial))
    {
#ifndef WINDOWSNT
      if (tcdrain (XPROCESS (proc)->outfd) != 0)
	error ("tcdrain() failed: %s", emacs_strerror (errno));
#endif /* not WINDOWSNT */
      /* Do nothing on Windows because writes are blocking.  */
    }
  else
    {
      int old_outfd, new_outfd;

#ifdef HAVE_SHUTDOWN
      /* If this is a network connection, or socketpair is used
	 for communication with the subprocess, call shutdown to cause EOF.
	 (In some old system, shutdown to socketpair doesn't work.
	 Then we just can't win.)  */
      if (EQ (XPROCESS (proc)->type, Qnetwork)
	  || XPROCESS (proc)->outfd == XPROCESS (proc)->infd)
	shutdown (XPROCESS (proc)->outfd, 1);
      /* In case of socketpair, outfd == infd, so don't close it.  */
      if (XPROCESS (proc)->outfd != XPROCESS (proc)->infd)
	emacs_close (XPROCESS (proc)->outfd);
#else /* not HAVE_SHUTDOWN */
      emacs_close (XPROCESS (proc)->outfd);
#endif /* not HAVE_SHUTDOWN */
      new_outfd = emacs_open (NULL_DEVICE, O_WRONLY, 0);
      if (new_outfd < 0)
	abort ();
      old_outfd = XPROCESS (proc)->outfd;

      if (!proc_encode_coding_system[new_outfd])
	proc_encode_coding_system[new_outfd]
	  = (struct coding_system *) xmalloc (sizeof (struct coding_system));
      memcpy (proc_encode_coding_system[new_outfd],
	      proc_encode_coding_system[old_outfd],
	      sizeof (struct coding_system));
      memset (proc_encode_coding_system[old_outfd], 0,
	      sizeof (struct coding_system));

      XPROCESS (proc)->outfd = new_outfd;
    }
  return process;
}

/* On receipt of a signal that a child status has changed, loop asking
   about children with changed statuses until the system says there
   are no more.

   All we do is change the status; we do not run sentinels or print
   notifications.  That is saved for the next time keyboard input is
   done, in order to avoid timing errors.

   ** WARNING: this can be called during garbage collection.
   Therefore, it must not be fooled by the presence of mark bits in
   Lisp objects.

   ** USG WARNING: Although it is not obvious from the documentation
   in signal(2), on a USG system the SIGCLD handler MUST NOT call
   signal() before executing at least one wait(), otherwise the
   handler will be called again, resulting in an infinite loop.  The
   relevant portion of the documentation reads "SIGCLD signals will be
   queued and the signal-catching function will be continually
   reentered until the queue is empty".  Invoking signal() causes the
   kernel to reexamine the SIGCLD queue.  Fred Fish, UniSoft Systems
   Inc.

   ** Malloc WARNING: This should never call malloc either directly or
   indirectly; if it does, that is a bug  */

#ifdef SIGCHLD
static void
sigchld_handler (int signo)
{
  int old_errno = errno;
  Lisp_Object proc;
  struct Lisp_Process *p;

  SIGNAL_THREAD_CHECK (signo);

  while (1)
    {
      pid_t pid;
      int w;
      Lisp_Object tail;

#ifdef WNOHANG
#ifndef WUNTRACED
#define WUNTRACED 0
#endif /* no WUNTRACED */
      /* Keep trying to get a status until we get a definitive result.  */
      do
	{
	  errno = 0;
	  pid = wait3 (&w, WNOHANG | WUNTRACED, 0);
	}
      while (pid < 0 && errno == EINTR);

      if (pid <= 0)
	{
	  /* PID == 0 means no processes found, PID == -1 means a real
	     failure.  We have done all our job, so return.  */

	  errno = old_errno;
	  return;
	}
#else
      pid = wait (&w);
#endif /* no WNOHANG */

      /* Find the process that signaled us, and record its status.  */

      /* The process can have been deleted by Fdelete_process.  */
      for (tail = deleted_pid_list; CONSP (tail); tail = XCDR (tail))
	{
	  Lisp_Object xpid = XCAR (tail);
	  if ((INTEGERP (xpid) && pid == (pid_t) XINT (xpid))
	      || (FLOATP (xpid) && pid == (pid_t) XFLOAT_DATA (xpid)))
	    {
	      XSETCAR (tail, Qnil);
	      goto sigchld_end_of_loop;
	    }
	}

      /* Otherwise, if it is asynchronous, it is in Vprocess_alist.  */
      p = 0;
      for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
	{
	  proc = XCDR (XCAR (tail));
	  p = XPROCESS (proc);
	  if (EQ (p->type, Qreal) && p->pid == pid)
	    break;
	  p = 0;
	}

      /* Look for an asynchronous process whose pid hasn't been filled
	 in yet.  */
      if (p == 0)
	for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
	  {
	    proc = XCDR (XCAR (tail));
	    p = XPROCESS (proc);
	    if (p->pid == -1)
	      break;
	    p = 0;
	  }

      /* Change the status of the process that was found.  */
      if (p != 0)
	{
	  int clear_desc_flag = 0;

	  p->tick = ++process_tick;
	  p->raw_status = w;
	  p->raw_status_new = 1;

	  /* If process has terminated, stop waiting for its output.  */
	  if ((WIFSIGNALED (w) || WIFEXITED (w))
	      && p->infd >= 0)
	    clear_desc_flag = 1;

	  /* We use clear_desc_flag to avoid a compiler bug in Microsoft C.  */
	  if (clear_desc_flag)
	    {
	      FD_CLR (p->infd, &input_wait_mask);
	      FD_CLR (p->infd, &non_keyboard_wait_mask);
	    }

	  /* Tell wait_reading_process_output that it needs to wake up and
	     look around.  */
	  if (input_available_clear_time)
	    EMACS_SET_SECS_USECS (*input_available_clear_time, 0, 0);
	}

      /* There was no asynchronous process found for that pid: we have
	 a synchronous process.  */
      else
	{
	  synch_process_alive = 0;

	  /* Report the status of the synchronous process.  */
	  if (WIFEXITED (w))
	    synch_process_retcode = WRETCODE (w);
	  else if (WIFSIGNALED (w))
	    synch_process_termsig = WTERMSIG (w);

	  /* Tell wait_reading_process_output that it needs to wake up and
	     look around.  */
	  if (input_available_clear_time)
	    EMACS_SET_SECS_USECS (*input_available_clear_time, 0, 0);
	}

    sigchld_end_of_loop:
      ;

      /* On some systems, we must return right away.
	 If any more processes want to signal us, we will
	 get another signal.
	 Otherwise (on systems that have WNOHANG), loop around
	 to use up all the processes that have something to tell us.  */
#if (defined WINDOWSNT \
     || (defined USG && !defined GNU_LINUX \
	 && !(defined HPUX && defined WNOHANG)))
      errno = old_errno;
      return;
#endif /* USG, but not HPUX with WNOHANG */
    }
}
#endif /* SIGCHLD */


static Lisp_Object
exec_sentinel_unwind (Lisp_Object data)
{
  XPROCESS (XCAR (data))->sentinel = XCDR (data);
  return Qnil;
}

static Lisp_Object
exec_sentinel_error_handler (Lisp_Object error_val)
{
  cmd_error_internal (error_val, "error in process sentinel: ");
  Vinhibit_quit = Qt;
  update_echo_area ();
  Fsleep_for (make_number (2), Qnil);
  return Qt;
}

static void
exec_sentinel (Lisp_Object proc, Lisp_Object reason)
{
  Lisp_Object sentinel, odeactivate;
  register struct Lisp_Process *p = XPROCESS (proc);
  int count = SPECPDL_INDEX ();
  int outer_running_asynch_code = running_asynch_code;
  int waiting = waiting_for_user_input_p;

  if (inhibit_sentinels)
    return;

  /* No need to gcpro these, because all we do with them later
     is test them for EQness, and none of them should be a string.  */
  odeactivate = Vdeactivate_mark;
#if 0
  Lisp_Object obuffer, okeymap;
  XSETBUFFER (obuffer, current_buffer);
  okeymap = BVAR (current_buffer, keymap);
#endif

  /* There's no good reason to let sentinels change the current
     buffer, and many callers of accept-process-output, sit-for, and
     friends don't expect current-buffer to be changed from under them.  */
  record_unwind_protect (set_buffer_if_live, Fcurrent_buffer ());

  sentinel = p->sentinel;
  if (NILP (sentinel))
    return;

  /* Zilch the sentinel while it's running, to avoid recursive invocations;
     assure that it gets restored no matter how the sentinel exits.  */
  p->sentinel = Qnil;
  record_unwind_protect (exec_sentinel_unwind, Fcons (proc, sentinel));
  /* Inhibit quit so that random quits don't screw up a running filter.  */
  specbind (Qinhibit_quit, Qt);
  specbind (Qlast_nonmenu_event, Qt); /* Why? --Stef  */

  /* In case we get recursively called,
     and we already saved the match data nonrecursively,
     save the same match data in safely recursive fashion.  */
  if (outer_running_asynch_code)
    {
      Lisp_Object tem;
      tem = Fmatch_data (Qnil, Qnil, Qnil);
      restore_search_regs ();
      record_unwind_save_match_data ();
      Fset_match_data (tem, Qt);
    }

  /* For speed, if a search happens within this code,
     save the match data in a special nonrecursive fashion.  */
  running_asynch_code = 1;

  internal_condition_case_1 (read_process_output_call,
			     Fcons (sentinel,
				    Fcons (proc, Fcons (reason, Qnil))),
			     !NILP (Vdebug_on_error) ? Qnil : Qerror,
			     exec_sentinel_error_handler);

  /* If we saved the match data nonrecursively, restore it now.  */
  restore_search_regs ();
  running_asynch_code = outer_running_asynch_code;

  Vdeactivate_mark = odeactivate;

  /* Restore waiting_for_user_input_p as it was
     when we were called, in case the filter clobbered it.  */
  waiting_for_user_input_p = waiting;

#if 0
  if (! EQ (Fcurrent_buffer (), obuffer)
      || ! EQ (current_buffer->keymap, okeymap))
#endif
    /* But do it only if the caller is actually going to read events.
       Otherwise there's no need to make him wake up, and it could
       cause trouble (for example it would make sit_for return).  */
    if (waiting_for_user_input_p == -1)
      record_asynch_buffer_change ();

  unbind_to (count, Qnil);
}

/* Report all recent events of a change in process status
   (either run the sentinel or output a message).
   This is usually done while Emacs is waiting for keyboard input
   but can be done at other times.  */

static void
status_notify (struct Lisp_Process *deleting_process)
{
  register Lisp_Object proc, buffer;
  Lisp_Object tail, msg;
  struct gcpro gcpro1, gcpro2;

  tail = Qnil;
  msg = Qnil;
  /* We need to gcpro tail; if read_process_output calls a filter
     which deletes a process and removes the cons to which tail points
     from Vprocess_alist, and then causes a GC, tail is an unprotected
     reference.  */
  GCPRO2 (tail, msg);

  /* Set this now, so that if new processes are created by sentinels
     that we run, we get called again to handle their status changes.  */
  update_tick = process_tick;

  for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
    {
      Lisp_Object symbol;
      register struct Lisp_Process *p;

      proc = Fcdr (XCAR (tail));
      p = XPROCESS (proc);

      if (p->tick != p->update_tick)
	{
	  p->update_tick = p->tick;

	  /* If process is still active, read any output that remains.  */
	  while (! EQ (p->filter, Qt)
		 && ! EQ (p->status, Qconnect)
		 && ! EQ (p->status, Qlisten)
		 /* Network or serial process not stopped:  */
		 && ! EQ (p->command, Qt)
		 && p->infd >= 0
		 && p != deleting_process
		 && read_process_output (proc, p->infd) > 0);

	  buffer = p->buffer;

	  /* Get the text to use for the message.  */
	  if (p->raw_status_new)
	    update_status (p);
	  msg = status_message (p);

	  /* If process is terminated, deactivate it or delete it.  */
	  symbol = p->status;
	  if (CONSP (p->status))
	    symbol = XCAR (p->status);

	  if (EQ (symbol, Qsignal) || EQ (symbol, Qexit)
	      || EQ (symbol, Qclosed))
	    {
	      if (delete_exited_processes)
		remove_process (proc);
	      else
		deactivate_process (proc);
	    }

	  /* The actions above may have further incremented p->tick.
	     So set p->update_tick again
	     so that an error in the sentinel will not cause
	     this code to be run again.  */
	  p->update_tick = p->tick;
	  /* Now output the message suitably.  */
	  if (!NILP (p->sentinel))
	    exec_sentinel (proc, msg);
	  /* Don't bother with a message in the buffer
	     when a process becomes runnable.  */
	  else if (!EQ (symbol, Qrun) && !NILP (buffer))
	    {
	      Lisp_Object tem;
	      struct buffer *old = current_buffer;
	      EMACS_INT opoint, opoint_byte;
	      EMACS_INT before, before_byte;

	      /* Avoid error if buffer is deleted
		 (probably that's why the process is dead, too) */
	      if (NILP (BVAR (XBUFFER (buffer), name)))
		continue;
	      Fset_buffer (buffer);

	      opoint = PT;
	      opoint_byte = PT_BYTE;
	      /* Insert new output into buffer
		 at the current end-of-output marker,
		 thus preserving logical ordering of input and output.  */
	      if (XMARKER (p->mark)->buffer)
		Fgoto_char (p->mark);
	      else
		SET_PT_BOTH (ZV, ZV_BYTE);

	      before = PT;
	      before_byte = PT_BYTE;

	      tem = BVAR (current_buffer, read_only);
	      BVAR (current_buffer, read_only) = Qnil;
	      insert_string ("\nProcess ");
	      Finsert (1, &p->name);
	      insert_string (" ");
	      Finsert (1, &msg);
	      BVAR (current_buffer, read_only) = tem;
	      set_marker_both (p->mark, p->buffer, PT, PT_BYTE);

	      if (opoint >= before)
		SET_PT_BOTH (opoint + (PT - before),
			     opoint_byte + (PT_BYTE - before_byte));
	      else
		SET_PT_BOTH (opoint, opoint_byte);

	      set_buffer_internal (old);
	    }
	}
    } /* end for */

  update_mode_lines++;  /* in case buffers use %s in mode-line-format */
  UNGCPRO;
}


DEFUN ("set-process-coding-system", Fset_process_coding_system,
       Sset_process_coding_system, 1, 3, 0,
       doc: /* Set coding systems of PROCESS to DECODING and ENCODING.
DECODING will be used to decode subprocess output and ENCODING to
encode subprocess input.  */)
  (register Lisp_Object process, Lisp_Object decoding, Lisp_Object encoding)
{
  register struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);
  if (p->infd < 0)
    error ("Input file descriptor of %s closed", SDATA (p->name));
  if (p->outfd < 0)
    error ("Output file descriptor of %s closed", SDATA (p->name));
  Fcheck_coding_system (decoding);
  Fcheck_coding_system (encoding);
  encoding = coding_inherit_eol_type (encoding, Qnil);
  p->decode_coding_system = decoding;
  p->encode_coding_system = encoding;
  setup_process_coding_systems (process);

  return Qnil;
}

DEFUN ("process-coding-system",
       Fprocess_coding_system, Sprocess_coding_system, 1, 1, 0,
       doc: /* Return a cons of coding systems for decoding and encoding of PROCESS.  */)
  (register Lisp_Object process)
{
  CHECK_PROCESS (process);
  return Fcons (XPROCESS (process)->decode_coding_system,
		XPROCESS (process)->encode_coding_system);
}

DEFUN ("set-process-filter-multibyte", Fset_process_filter_multibyte,
       Sset_process_filter_multibyte, 2, 2, 0,
       doc: /* Set multibyteness of the strings given to PROCESS's filter.
If FLAG is non-nil, the filter is given multibyte strings.
If FLAG is nil, the filter is given unibyte strings.  In this case,
all character code conversion except for end-of-line conversion is
suppressed.  */)
  (Lisp_Object process, Lisp_Object flag)
{
  register struct Lisp_Process *p;

  CHECK_PROCESS (process);
  p = XPROCESS (process);
  if (NILP (flag))
    p->decode_coding_system = raw_text_coding_system (p->decode_coding_system);
  setup_process_coding_systems (process);

  return Qnil;
}

DEFUN ("process-filter-multibyte-p", Fprocess_filter_multibyte_p,
       Sprocess_filter_multibyte_p, 1, 1, 0,
       doc: /* Return t if a multibyte string is given to PROCESS's filter.*/)
  (Lisp_Object process)
{
  register struct Lisp_Process *p;
  struct coding_system *coding;

  CHECK_PROCESS (process);
  p = XPROCESS (process);
  coding = proc_decode_coding_system[p->infd];
  return (CODING_FOR_UNIBYTE (coding) ? Qnil : Qt);
}




# ifdef HAVE_GPM

void
add_gpm_wait_descriptor (int desc)
{
  add_keyboard_wait_descriptor (desc);
}

void
delete_gpm_wait_descriptor (int desc)
{
  delete_keyboard_wait_descriptor (desc);
}

# endif

# ifdef SIGIO

/* Return nonzero if *MASK has a bit set
   that corresponds to one of the keyboard input descriptors.  */

static int
keyboard_bit_set (fd_set *mask)
{
  int fd;

  for (fd = 0; fd <= max_input_desc; fd++)
    if (FD_ISSET (fd, mask) && FD_ISSET (fd, &input_wait_mask)
	&& !FD_ISSET (fd, &non_keyboard_wait_mask))
      return 1;

  return 0;
}
# endif

#else  /* not subprocesses */

/* Defined on msdos.c.  */
extern int sys_select (int, SELECT_TYPE *, SELECT_TYPE *, SELECT_TYPE *,
		       EMACS_TIME *);

/* Implementation of wait_reading_process_output, assuming that there
   are no subprocesses.  Used only by the MS-DOS build.

   Wait for timeout to elapse and/or keyboard input to be available.

   time_limit is:
     timeout in seconds, or
     zero for no limit, or
     -1 means gobble data immediately available but don't wait for any.

   read_kbd is a Lisp_Object:
     0 to ignore keyboard input, or
     1 to return when input is available, or
     -1 means caller will actually read the input, so don't throw to
       the quit handler.

   see full version for other parameters. We know that wait_proc will
     always be NULL, since `subprocesses' isn't defined.

   do_display != 0 means redisplay should be done to show subprocess
   output that arrives.

   Return true if we received input from any process.  */

int
wait_reading_process_output (int time_limit, int microsecs, int read_kbd,
			     int do_display,
			     Lisp_Object wait_for_cell,
			     struct Lisp_Process *wait_proc, int just_wait_proc)
{
  register int nfds;
  EMACS_TIME end_time, timeout;
  SELECT_TYPE waitchannels;
  int xerrno;

  /* What does time_limit really mean?  */
  if (time_limit || microsecs)
    {
      EMACS_GET_TIME (end_time);
      EMACS_SET_SECS_USECS (timeout, time_limit, microsecs);
      EMACS_ADD_TIME (end_time, end_time, timeout);
    }

  /* Turn off periodic alarms (in case they are in use)
     and then turn off any other atimers,
     because the select emulator uses alarms.  */
  stop_polling ();
  turn_on_atimers (0);

  while (1)
    {
      int timeout_reduced_for_timers = 0;

      /* If calling from keyboard input, do not quit
	 since we want to return C-g as an input character.
	 Otherwise, do pending quit if requested.  */
      if (read_kbd >= 0)
	QUIT;

      /* Exit now if the cell we're waiting for became non-nil.  */
      if (! NILP (wait_for_cell) && ! NILP (XCAR (wait_for_cell)))
	break;

      /* Compute time from now till when time limit is up */
      /* Exit if already run out */
      if (time_limit == -1)
	{
	  /* -1 specified for timeout means
	     gobble output available now
	     but don't wait at all. */

	  EMACS_SET_SECS_USECS (timeout, 0, 0);
	}
      else if (time_limit || microsecs)
	{
	  EMACS_GET_TIME (timeout);
	  EMACS_SUB_TIME (timeout, end_time, timeout);
	  if (EMACS_TIME_NEG_P (timeout))
	    break;
	}
      else
	{
	  EMACS_SET_SECS_USECS (timeout, 100000, 0);
	}

      /* If our caller will not immediately handle keyboard events,
	 run timer events directly.
	 (Callers that will immediately read keyboard events
	 call timer_delay on their own.)  */
      if (NILP (wait_for_cell))
	{
	  EMACS_TIME timer_delay;

	  do
	    {
	      int old_timers_run = timers_run;
	      timer_delay = timer_check ();
	      if (timers_run != old_timers_run && do_display)
		/* We must retry, since a timer may have requeued itself
		   and that could alter the time delay.  */
		redisplay_preserve_echo_area (14);
	      else
		break;
	    }
	  while (!detect_input_pending ());

	  /* If there is unread keyboard input, also return.  */
	  if (read_kbd != 0
	      && requeued_events_pending_p ())
	    break;

	  if (! EMACS_TIME_NEG_P (timer_delay) && time_limit != -1)
	    {
	      EMACS_TIME difference;
	      EMACS_SUB_TIME (difference, timer_delay, timeout);
	      if (EMACS_TIME_NEG_P (difference))
		{
		  timeout = timer_delay;
		  timeout_reduced_for_timers = 1;
		}
	    }
	}

      /* Cause C-g and alarm signals to take immediate action,
	 and cause input available signals to zero out timeout.  */
      if (read_kbd < 0)
	set_waiting_for_input (&timeout);

      /* Wait till there is something to do.  */

      if (! read_kbd && NILP (wait_for_cell))
	FD_ZERO (&waitchannels);
      else
	FD_SET (0, &waitchannels);

      /* If a frame has been newly mapped and needs updating,
	 reprocess its display stuff.  */
      if (frame_garbaged && do_display)
	{
	  clear_waiting_for_input ();
	  redisplay_preserve_echo_area (15);
	  if (read_kbd < 0)
	    set_waiting_for_input (&timeout);
	}

      if (read_kbd && detect_input_pending ())
	{
	  nfds = 0;
	  FD_ZERO (&waitchannels);
	}
      else
	nfds = select (1, &waitchannels, (SELECT_TYPE *)0, (SELECT_TYPE *)0,
		       &timeout);

      xerrno = errno;

      /* Make C-g and alarm signals set flags again */
      clear_waiting_for_input ();

      /*  If we woke up due to SIGWINCH, actually change size now.  */
      do_pending_window_change (0);

      if (time_limit && nfds == 0 && ! timeout_reduced_for_timers)
	/* We waited the full specified time, so return now.  */
	break;

      if (nfds == -1)
	{
	  /* If the system call was interrupted, then go around the
	     loop again.  */
	  if (xerrno == EINTR)
	    FD_ZERO (&waitchannels);
	  else
	    error ("select error: %s", emacs_strerror (xerrno));
	}

      /* Check for keyboard input */

      if (read_kbd
	  && detect_input_pending_run_timers (do_display))
	{
	  swallow_events (do_display);
	  if (detect_input_pending_run_timers (do_display))
	    break;
	}

      /* If there is unread keyboard input, also return.  */
      if (read_kbd
	  && requeued_events_pending_p ())
	break;

      /* If wait_for_cell. check for keyboard input
	 but don't run any timers.
	 ??? (It seems wrong to me to check for keyboard
	 input at all when wait_for_cell, but the code
	 has been this way since July 1994.
	 Try changing this after version 19.31.)  */
      if (! NILP (wait_for_cell)
	  && detect_input_pending ())
	{
	  swallow_events (do_display);
	  if (detect_input_pending ())
	    break;
	}

      /* Exit now if the cell we're waiting for became non-nil.  */
      if (! NILP (wait_for_cell) && ! NILP (XCAR (wait_for_cell)))
	break;
    }

  start_polling ();

  return 0;
}

#endif	/* not subprocesses */

/* The following functions are needed even if async subprocesses are
   not supported.  Some of them are no-op stubs in that case.  */

/* Add DESC to the set of keyboard input descriptors.  */

void
add_keyboard_wait_descriptor (int desc)
{
#ifdef subprocesses /* actually means "not MSDOS" */
  FD_SET (desc, &input_wait_mask);
  FD_SET (desc, &non_process_wait_mask);
  if (desc > max_input_desc)
    max_input_desc = desc;
#endif
}

/* From now on, do not expect DESC to give keyboard input.  */

void
delete_keyboard_wait_descriptor (int desc)
{
#ifdef subprocesses
  int fd;
  int lim = max_input_desc;

  FD_CLR (desc, &input_wait_mask);
  FD_CLR (desc, &non_process_wait_mask);

  if (desc == max_input_desc)
    for (fd = 0; fd < lim; fd++)
      if (FD_ISSET (fd, &input_wait_mask) || FD_ISSET (fd, &write_mask))
        max_input_desc = fd;
#endif
}

/* Setup coding systems of PROCESS.  */

void
setup_process_coding_systems (Lisp_Object process)
{
#ifdef subprocesses
  struct Lisp_Process *p = XPROCESS (process);
  int inch = p->infd;
  int outch = p->outfd;
  Lisp_Object coding_system;

  if (inch < 0 || outch < 0)
    return;

  if (!proc_decode_coding_system[inch])
    proc_decode_coding_system[inch]
      = (struct coding_system *) xmalloc (sizeof (struct coding_system));
  coding_system = p->decode_coding_system;
  if (! NILP (p->filter))
    ;
  else if (BUFFERP (p->buffer))
    {
      if (NILP (BVAR (XBUFFER (p->buffer), enable_multibyte_characters)))
	coding_system = raw_text_coding_system (coding_system);
    }
  setup_coding_system (coding_system, proc_decode_coding_system[inch]);

  if (!proc_encode_coding_system[outch])
    proc_encode_coding_system[outch]
      = (struct coding_system *) xmalloc (sizeof (struct coding_system));
  setup_coding_system (p->encode_coding_system,
		       proc_encode_coding_system[outch]);
#endif
}

/* Close all descriptors currently in use for communication
   with subprocess.  This is used in a newly-forked subprocess
   to get rid of irrelevant descriptors.  */

void
close_process_descs (void)
{
#ifndef DOS_NT
  int i;
  for (i = 0; i < MAXDESC; i++)
    {
      Lisp_Object process;
      process = chan_process[i];
      if (!NILP (process))
	{
	  int in  = XPROCESS (process)->infd;
	  int out = XPROCESS (process)->outfd;
	  if (in >= 0)
	    emacs_close (in);
	  if (out >= 0 && in != out)
	    emacs_close (out);
	}
    }
#endif
}

DEFUN ("get-buffer-process", Fget_buffer_process, Sget_buffer_process, 1, 1, 0,
       doc: /* Return the (or a) process associated with BUFFER.
BUFFER may be a buffer or the name of one.  */)
  (register Lisp_Object buffer)
{
#ifdef subprocesses
  register Lisp_Object buf, tail, proc;

  if (NILP (buffer)) return Qnil;
  buf = Fget_buffer (buffer);
  if (NILP (buf)) return Qnil;

  for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
    {
      proc = Fcdr (XCAR (tail));
      if (PROCESSP (proc) && EQ (XPROCESS (proc)->buffer, buf))
	return proc;
    }
#endif	/* subprocesses */
  return Qnil;
}

DEFUN ("process-inherit-coding-system-flag",
       Fprocess_inherit_coding_system_flag, Sprocess_inherit_coding_system_flag,
       1, 1, 0,
       doc: /* Return the value of inherit-coding-system flag for PROCESS.
If this flag is t, `buffer-file-coding-system' of the buffer
associated with PROCESS will inherit the coding system used to decode
the process output.  */)
  (register Lisp_Object process)
{
#ifdef subprocesses
  CHECK_PROCESS (process);
  return XPROCESS (process)->inherit_coding_system_flag ? Qt : Qnil;
#else
  /* Ignore the argument and return the value of
     inherit-process-coding-system.  */
  return inherit_process_coding_system ? Qt : Qnil;
#endif
}

/* Kill all processes associated with `buffer'.
   If `buffer' is nil, kill all processes  */

void
kill_buffer_processes (Lisp_Object buffer)
{
#ifdef subprocesses
  Lisp_Object tail, proc;

  for (tail = Vprocess_alist; CONSP (tail); tail = XCDR (tail))
    {
      proc = XCDR (XCAR (tail));
      if (PROCESSP (proc)
	  && (NILP (buffer) || EQ (XPROCESS (proc)->buffer, buffer)))
	{
	  if (NETCONN_P (proc) || SERIALCONN_P (proc))
	    Fdelete_process (proc);
	  else if (XPROCESS (proc)->infd >= 0)
	    process_send_signal (proc, SIGHUP, Qnil, 1);
	}
    }
#else  /* subprocesses */
  /* Since we have no subprocesses, this does nothing.  */
#endif /* subprocesses */
}

DEFUN ("waiting-for-user-input-p", Fwaiting_for_user_input_p,
       Swaiting_for_user_input_p, 0, 0, 0,
       doc: /* Returns non-nil if Emacs is waiting for input from the user.
This is intended for use by asynchronous process output filters and sentinels.  */)
  (void)
{
#ifdef subprocesses
  return (waiting_for_user_input_p ? Qt : Qnil);
#else
  return Qnil;
#endif
}

/* Stop reading input from keyboard sources.  */

void
hold_keyboard_input (void)
{
  kbd_is_on_hold = 1;
}

/* Resume reading input from keyboard sources.  */

void
unhold_keyboard_input (void)
{
  kbd_is_on_hold = 0;
}

/* Return non-zero if keyboard input is on hold, zero otherwise.  */

int
kbd_on_hold_p (void)
{
  return kbd_is_on_hold;
}


/* Enumeration of and access to system processes a-la ps(1).  */

DEFUN ("list-system-processes", Flist_system_processes, Slist_system_processes,
       0, 0, 0,
       doc: /* Return a list of numerical process IDs of all running processes.
If this functionality is unsupported, return nil.

See `process-attributes' for getting attributes of a process given its ID.  */)
  (void)
{
  return list_system_processes ();
}

DEFUN ("process-attributes", Fprocess_attributes,
       Sprocess_attributes, 1, 1, 0,
       doc: /* Return attributes of the process given by its PID, a number.

Value is an alist where each element is a cons cell of the form

    \(KEY . VALUE)

If this functionality is unsupported, the value is nil.

See `list-system-processes' for getting a list of all process IDs.

The KEYs of the attributes that this function may return are listed
below, together with the type of the associated VALUE (in parentheses).
Not all platforms support all of these attributes; unsupported
attributes will not appear in the returned alist.
Unless explicitly indicated otherwise, numbers can have either
integer or floating point values.

 euid    -- Effective user User ID of the process (number)
 user    -- User name corresponding to euid (string)
 egid    -- Effective user Group ID of the process (number)
 group   -- Group name corresponding to egid (string)
 comm    -- Command name (executable name only) (string)
 state   -- Process state code, such as "S", "R", or "T" (string)
 ppid    -- Parent process ID (number)
 pgrp    -- Process group ID (number)
 sess    -- Session ID, i.e. process ID of session leader (number)
 ttname  -- Controlling tty name (string)
 tpgid   -- ID of foreground process group on the process's tty (number)
 minflt  -- number of minor page faults (number)
 majflt  -- number of major page faults (number)
 cminflt -- cumulative number of minor page faults (number)
 cmajflt -- cumulative number of major page faults (number)
 utime   -- user time used by the process, in the (HIGH LOW USEC) format
 stime   -- system time used by the process, in the (HIGH LOW USEC) format
 time    -- sum of utime and stime, in the (HIGH LOW USEC) format
 cutime  -- user time used by the process and its children, (HIGH LOW USEC)
 cstime  -- system time used by the process and its children, (HIGH LOW USEC)
 ctime   -- sum of cutime and cstime, in the (HIGH LOW USEC) format
 pri     -- priority of the process (number)
 nice    -- nice value of the process (number)
 thcount -- process thread count (number)
 start   -- time the process started, in the (HIGH LOW USEC) format
 vsize   -- virtual memory size of the process in KB's (number)
 rss     -- resident set size of the process in KB's (number)
 etime   -- elapsed time the process is running, in (HIGH LOW USEC) format
 pcpu    -- percents of CPU time used by the process (floating-point number)
 pmem    -- percents of total physical memory used by process's resident set
              (floating-point number)
 args    -- command line which invoked the process (string).  */)
  ( Lisp_Object pid)
{
  return system_process_attributes (pid);
}


void
init_process (void)
{
#ifdef subprocesses
  register int i;

  inhibit_sentinels = 0;

#ifdef SIGCHLD
#ifndef CANNOT_DUMP
  if (! noninteractive || initialized)
#endif
    signal (SIGCHLD, sigchld_handler);
#endif

  FD_ZERO (&input_wait_mask);
  FD_ZERO (&non_keyboard_wait_mask);
  FD_ZERO (&non_process_wait_mask);
  FD_ZERO (&write_mask);
  max_process_desc = 0;
  memset (fd_callback_info, 0, sizeof (fd_callback_info));

#ifdef NON_BLOCKING_CONNECT
  FD_ZERO (&connect_wait_mask);
  num_pending_connects = 0;
#endif

#ifdef ADAPTIVE_READ_BUFFERING
  process_output_delay_count = 0;
  process_output_skip = 0;
#endif

  /* Don't do this, it caused infinite select loops.  The display
     method should call add_keyboard_wait_descriptor on stdin if it
     needs that.  */
#if 0
  FD_SET (0, &input_wait_mask);
#endif

  Vprocess_alist = Qnil;
#ifdef SIGCHLD
  deleted_pid_list = Qnil;
#endif
  for (i = 0; i < MAXDESC; i++)
    {
      chan_process[i] = Qnil;
      proc_buffered_char[i] = -1;
    }
  memset (proc_decode_coding_system, 0, sizeof proc_decode_coding_system);
  memset (proc_encode_coding_system, 0, sizeof proc_encode_coding_system);
#ifdef DATAGRAM_SOCKETS
  memset (datagram_address, 0, sizeof datagram_address);
#endif

 {
   Lisp_Object subfeatures = Qnil;
   const struct socket_options *sopt;

#define ADD_SUBFEATURE(key, val) \
  subfeatures = pure_cons (pure_cons (key, pure_cons (val, Qnil)), subfeatures)

#ifdef NON_BLOCKING_CONNECT
   ADD_SUBFEATURE (QCnowait, Qt);
#endif
#ifdef DATAGRAM_SOCKETS
   ADD_SUBFEATURE (QCtype, Qdatagram);
#endif
#ifdef HAVE_SEQPACKET
   ADD_SUBFEATURE (QCtype, Qseqpacket);
#endif
#ifdef HAVE_LOCAL_SOCKETS
   ADD_SUBFEATURE (QCfamily, Qlocal);
#endif
   ADD_SUBFEATURE (QCfamily, Qipv4);
#ifdef AF_INET6
   ADD_SUBFEATURE (QCfamily, Qipv6);
#endif
#ifdef HAVE_GETSOCKNAME
   ADD_SUBFEATURE (QCservice, Qt);
#endif
#if defined (O_NONBLOCK) || defined (O_NDELAY)
   ADD_SUBFEATURE (QCserver, Qt);
#endif

   for (sopt = socket_options; sopt->name; sopt++)
     subfeatures = pure_cons (intern_c_string (sopt->name), subfeatures);

   Fprovide (intern_c_string ("make-network-process"), subfeatures);
 }

#if defined (DARWIN_OS)
  /* PTYs are broken on Darwin < 6, but are sometimes useful for interactive
     processes.  As such, we only change the default value.  */
 if (initialized)
  {
    char const *release = (STRINGP (Voperating_system_release)
			   ? SSDATA (Voperating_system_release)
			   : 0);
    if (!release || !release[0] || (release[0] < MIN_PTY_KERNEL_VERSION
				    && release[1] == '.')) {
      Vprocess_connection_type = Qnil;
    }
  }
#endif
#endif	/* subprocesses */
  kbd_is_on_hold = 0;
}

void
syms_of_process (void)
{
#ifdef subprocesses

  DEFSYM (Qprocessp, "processp");
  DEFSYM (Qrun, "run");
  DEFSYM (Qstop, "stop");
  DEFSYM (Qsignal, "signal");

  /* Qexit is already staticpro'd by syms_of_eval; don't staticpro it
     here again.

     Qexit = intern_c_string ("exit");
     staticpro (&Qexit); */

  DEFSYM (Qopen, "open");
  DEFSYM (Qclosed, "closed");
  DEFSYM (Qconnect, "connect");
  DEFSYM (Qfailed, "failed");
  DEFSYM (Qlisten, "listen");
  DEFSYM (Qlocal, "local");
  DEFSYM (Qipv4, "ipv4");
#ifdef AF_INET6
  DEFSYM (Qipv6, "ipv6");
#endif
  DEFSYM (Qdatagram, "datagram");
  DEFSYM (Qseqpacket, "seqpacket");

  DEFSYM (QCport, ":port");
  DEFSYM (QCspeed, ":speed");
  DEFSYM (QCprocess, ":process");

  DEFSYM (QCbytesize, ":bytesize");
  DEFSYM (QCstopbits, ":stopbits");
  DEFSYM (QCparity, ":parity");
  DEFSYM (Qodd, "odd");
  DEFSYM (Qeven, "even");
  DEFSYM (QCflowcontrol, ":flowcontrol");
  DEFSYM (Qhw, "hw");
  DEFSYM (Qsw, "sw");
  DEFSYM (QCsummary, ":summary");

  DEFSYM (Qreal, "real");
  DEFSYM (Qnetwork, "network");
  DEFSYM (Qserial, "serial");
  DEFSYM (QCbuffer, ":buffer");
  DEFSYM (QChost, ":host");
  DEFSYM (QCservice, ":service");
  DEFSYM (QClocal, ":local");
  DEFSYM (QCremote, ":remote");
  DEFSYM (QCcoding, ":coding");
  DEFSYM (QCserver, ":server");
  DEFSYM (QCnowait, ":nowait");
  DEFSYM (QCsentinel, ":sentinel");
  DEFSYM (QClog, ":log");
  DEFSYM (QCnoquery, ":noquery");
  DEFSYM (QCstop, ":stop");
  DEFSYM (QCoptions, ":options");
  DEFSYM (QCplist, ":plist");

  DEFSYM (Qlast_nonmenu_event, "last-nonmenu-event");

  staticpro (&Vprocess_alist);
#ifdef SIGCHLD
  staticpro (&deleted_pid_list);
#endif

#endif	/* subprocesses */

  DEFSYM (QCname, ":name");
  DEFSYM (QCtype, ":type");

  DEFSYM (Qeuid, "euid");
  DEFSYM (Qegid, "egid");
  DEFSYM (Quser, "user");
  DEFSYM (Qgroup, "group");
  DEFSYM (Qcomm, "comm");
  DEFSYM (Qstate, "state");
  DEFSYM (Qppid, "ppid");
  DEFSYM (Qpgrp, "pgrp");
  DEFSYM (Qsess, "sess");
  DEFSYM (Qttname, "ttname");
  DEFSYM (Qtpgid, "tpgid");
  DEFSYM (Qminflt, "minflt");
  DEFSYM (Qmajflt, "majflt");
  DEFSYM (Qcminflt, "cminflt");
  DEFSYM (Qcmajflt, "cmajflt");
  DEFSYM (Qutime, "utime");
  DEFSYM (Qstime, "stime");
  DEFSYM (Qtime, "time");
  DEFSYM (Qcutime, "cutime");
  DEFSYM (Qcstime, "cstime");
  DEFSYM (Qctime, "ctime");
  DEFSYM (Qpri, "pri");
  DEFSYM (Qnice, "nice");
  DEFSYM (Qthcount, "thcount");
  DEFSYM (Qstart, "start");
  DEFSYM (Qvsize, "vsize");
  DEFSYM (Qrss, "rss");
  DEFSYM (Qetime, "etime");
  DEFSYM (Qpcpu, "pcpu");
  DEFSYM (Qpmem, "pmem");
  DEFSYM (Qargs, "args");

  DEFVAR_BOOL ("delete-exited-processes", delete_exited_processes,
	       doc: /* *Non-nil means delete processes immediately when they exit.
A value of nil means don't delete them until `list-processes' is run.  */);

  delete_exited_processes = 1;

#ifdef subprocesses
  DEFVAR_LISP ("process-connection-type", Vprocess_connection_type,
	       doc: /* Control type of device used to communicate with subprocesses.
Values are nil to use a pipe, or t or `pty' to use a pty.
The value has no effect if the system has no ptys or if all ptys are busy:
then a pipe is used in any case.
The value takes effect when `start-process' is called.  */);
  Vprocess_connection_type = Qt;

#ifdef ADAPTIVE_READ_BUFFERING
  DEFVAR_LISP ("process-adaptive-read-buffering", Vprocess_adaptive_read_buffering,
	       doc: /* If non-nil, improve receive buffering by delaying after short reads.
On some systems, when Emacs reads the output from a subprocess, the output data
is read in very small blocks, potentially resulting in very poor performance.
This behavior can be remedied to some extent by setting this variable to a
non-nil value, as it will automatically delay reading from such processes, to
allow them to produce more output before Emacs tries to read it.
If the value is t, the delay is reset after each write to the process; any other
non-nil value means that the delay is not reset on write.
The variable takes effect when `start-process' is called.  */);
  Vprocess_adaptive_read_buffering = Qt;
#endif

  defsubr (&Sprocessp);
  defsubr (&Sget_process);
  defsubr (&Sdelete_process);
  defsubr (&Sprocess_status);
  defsubr (&Sprocess_exit_status);
  defsubr (&Sprocess_id);
  defsubr (&Sprocess_name);
  defsubr (&Sprocess_tty_name);
  defsubr (&Sprocess_command);
  defsubr (&Sset_process_buffer);
  defsubr (&Sprocess_buffer);
  defsubr (&Sprocess_mark);
  defsubr (&Sset_process_filter);
  defsubr (&Sprocess_filter);
  defsubr (&Sset_process_sentinel);
  defsubr (&Sprocess_sentinel);
  defsubr (&Sset_process_window_size);
  defsubr (&Sset_process_inherit_coding_system_flag);
  defsubr (&Sset_process_query_on_exit_flag);
  defsubr (&Sprocess_query_on_exit_flag);
  defsubr (&Sprocess_contact);
  defsubr (&Sprocess_plist);
  defsubr (&Sset_process_plist);
  defsubr (&Sprocess_list);
  defsubr (&Sstart_process);
  defsubr (&Sserial_process_configure);
  defsubr (&Smake_serial_process);
  defsubr (&Sset_network_process_option);
  defsubr (&Smake_network_process);
  defsubr (&Sformat_network_address);
#if defined (HAVE_NET_IF_H)
#ifdef SIOCGIFCONF
  defsubr (&Snetwork_interface_list);
#endif
#if defined (SIOCGIFADDR) || defined (SIOCGIFHWADDR) || defined (SIOCGIFFLAGS)
  defsubr (&Snetwork_interface_info);
#endif
#endif /* defined (HAVE_NET_IF_H) */
#ifdef DATAGRAM_SOCKETS
  defsubr (&Sprocess_datagram_address);
  defsubr (&Sset_process_datagram_address);
#endif
  defsubr (&Saccept_process_output);
  defsubr (&Sprocess_send_region);
  defsubr (&Sprocess_send_string);
  defsubr (&Sinterrupt_process);
  defsubr (&Skill_process);
  defsubr (&Squit_process);
  defsubr (&Sstop_process);
  defsubr (&Scontinue_process);
  defsubr (&Sprocess_running_child_p);
  defsubr (&Sprocess_send_eof);
  defsubr (&Ssignal_process);
  defsubr (&Swaiting_for_user_input_p);
  defsubr (&Sprocess_type);
  defsubr (&Sset_process_coding_system);
  defsubr (&Sprocess_coding_system);
  defsubr (&Sset_process_filter_multibyte);
  defsubr (&Sprocess_filter_multibyte_p);

#endif	/* subprocesses */

  defsubr (&Sget_buffer_process);
  defsubr (&Sprocess_inherit_coding_system_flag);
  defsubr (&Slist_system_processes);
  defsubr (&Sprocess_attributes);
}
