/* Definitions for asynchronous process control in GNU Emacs.
   Copyright (C) 1985, 1994, 2001-2012  Free Software Foundation, Inc.

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

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include <unistd.h>

#ifdef HAVE_GNUTLS
#include "gnutls.h"
#endif

/* This structure records information about a subprocess
   or network connection.

   Every field in this structure except for the header
   must be a Lisp_Object, for GC's sake.  */

struct Lisp_Process
  {
    struct vectorlike_header header;

    /* Name of subprocess terminal.  */
    Lisp_Object tty_name;
    /* Name of this process */
    Lisp_Object name;
    /* List of command arguments that this process was run with.
       Is set to t for a stopped network process; nil otherwise. */
    Lisp_Object command;
    /* (funcall FILTER PROC STRING)  (if FILTER is non-nil)
       to dispose of a bunch of chars from the process all at once */
    Lisp_Object filter;
    /* (funcall SENTINEL PROCESS) when process state changes */
    Lisp_Object sentinel;
    /* (funcall LOG SERVER CLIENT MESSAGE) when a server process
       accepts a connection from a client.  */
    Lisp_Object log;
    /* Buffer that output is going to */
    Lisp_Object buffer;
    /* t if this is a real child process.  For a network or serial
       connection, it is a plist based on the arguments to
       make-network-process or make-serial-process.  */
    Lisp_Object childp;
    /* Plist for programs to keep per-process state information, parameters, etc.  */
    Lisp_Object plist;
    /* Symbol indicating the type of process: real, network, serial  */
    Lisp_Object type;
    /* Marker set to end of last buffer-inserted output from this process */
    Lisp_Object mark;
    /* Symbol indicating status of process.
       This may be a symbol: run, open, or closed.
       Or it may be a list, whose car is stop, exit or signal
       and whose cdr is a pair (EXIT_CODE . COREDUMP_FLAG)
       or (SIGNAL_NUMBER . COREDUMP_FLAG).  */
    Lisp_Object status;
    /* Coding-system for decoding the input from this process.  */
    Lisp_Object decode_coding_system;
    /* Working buffer for decoding.  */
    Lisp_Object decoding_buf;
    /* Coding-system for encoding the output to this process.  */
    Lisp_Object encode_coding_system;
    /* Working buffer for encoding.  */
    Lisp_Object encoding_buf;

#ifdef HAVE_GNUTLS
    Lisp_Object gnutls_cred_type;
#endif

    /* After this point, there are no Lisp_Objects any more.  */
    /* alloc.c assumes that `pid' is the first such non-Lisp slot.  */

    /* Number of this process.
       allocate_process assumes this is the first non-Lisp_Object field.
       A value 0 is used for pseudo-processes such as network or serial
       connections.  */
    pid_t pid;
    /* Descriptor by which we read from this process */
    int infd;
    /* Descriptor by which we write to this process */
    int outfd;
    /* Event-count of last event in which this process changed status.  */
    int tick;
    /* Event-count of last such event reported.  */
    int update_tick;
    /* Size of carryover in decoding.  */
    int decoding_carryover;
    /* Hysteresis to try to read process output in larger blocks.
       On some systems, e.g. GNU/Linux, Emacs is seen as
       an interactive app also when reading process output, meaning
       that process output can be read in as little as 1 byte at a
       time.  Value is micro-seconds to delay reading output from
       this process.  Range is 0 .. 50000.  */
    int read_output_delay;
    /* Should we delay reading output from this process.
       Initialized from `Vprocess_adaptive_read_buffering'.
       0 = nil, 1 = t, 2 = other.  */
    unsigned int adaptive_read_buffering : 2;
    /* Skip reading this process on next read.  */
    unsigned int read_output_skip : 1;
    /* Non-nil means kill silently if Emacs is exited.
       This is the inverse of the `query-on-exit' flag.  */
    unsigned int kill_without_query : 1;
    /* Non-nil if communicating through a pty.  */
    unsigned int pty_flag : 1;
    /* Flag to set coding-system of the process buffer from the
       coding_system used to decode process output.  */
    unsigned int inherit_coding_system_flag : 1;
    /* Record the process status in the raw form in which it comes from `wait'.
       This is to avoid consing in a signal handler.  The `raw_status_new'
       flag indicates that `raw_status' contains a new status that still
       needs to be synced to `status'.  */
    unsigned int raw_status_new : 1;
    int raw_status;

#ifdef HAVE_GNUTLS
    gnutls_initstage_t gnutls_initstage;
    gnutls_session_t gnutls_state;
    gnutls_certificate_client_credentials gnutls_x509_cred;
    gnutls_anon_client_credentials_t gnutls_anon_cred;
    int gnutls_log_level;
    int gnutls_handshakes_tried;
    int gnutls_p;
#endif
};

/* Every field in the preceding structure except for the first two
   must be a Lisp_Object, for GC's sake.  */

#define ChannelMask(n) (1 << (n))

/* True if we are about to fork off a synchronous process or if we
   are waiting for it.  */
extern int synch_process_alive;

/* Communicate exit status of sync process to from sigchld_handler
   to Fcall_process.  */

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
extern const char *synch_process_death;

/* Nonzero => this is the signal number that terminated the subprocess.  */
extern int synch_process_termsig;

/* If synch_process_death is zero,
   this is exit code of synchronous subprocess.  */
extern int synch_process_retcode;

/* The name of the file open to get a null file, or a data sink.
   MS-DOS, and OS/2 redefine this.  */
#ifndef NULL_DEVICE
#define NULL_DEVICE "/dev/null"
#endif

/* Nonzero means don't run process sentinels.  This is used
   when exiting.  */
extern int inhibit_sentinels;

extern Lisp_Object Qeuid, Qegid, Qcomm, Qstate, Qppid, Qpgrp, Qsess, Qttname;
extern Lisp_Object Qminflt, Qmajflt, Qcminflt, Qcmajflt, Qutime, Qstime;
extern Lisp_Object Qcutime, Qpri, Qnice, Qthcount, Qstart, Qvsize, Qrss, Qargs;
extern Lisp_Object Quser, Qgroup, Qetime, Qpcpu, Qpmem, Qtpgid, Qcstime;
extern Lisp_Object Qtime, Qctime;
extern Lisp_Object QCspeed;
extern Lisp_Object QCbytesize, QCstopbits, QCparity, Qodd, Qeven;
extern Lisp_Object QCflowcontrol, Qhw, Qsw, QCsummary;

extern Lisp_Object list_system_processes (void);
extern Lisp_Object system_process_attributes (Lisp_Object);

extern void hold_keyboard_input (void);
extern void unhold_keyboard_input (void);
extern int kbd_on_hold_p (void);

typedef void (*fd_callback)(int fd, void *data, int for_read);

extern void add_read_fd (int fd, fd_callback func, void *data);
extern void delete_read_fd (int fd);
extern void add_write_fd (int fd, fd_callback func, void *data);
extern void delete_write_fd (int fd);
