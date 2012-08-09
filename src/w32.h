#ifndef EMACS_W32_H
#define EMACS_W32_H

/* Support routines for the NT version of Emacs.
   Copyright (C) 1994, 2001-2012  Free Software Foundation, Inc.

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


/* File descriptor set emulation.  */

/* MSVC runtime library has limit of 64 descriptors by default */
#define FD_SETSIZE  64
typedef struct {
  unsigned int bits[FD_SETSIZE / 32];
} fd_set;

/* standard access macros */
#define FD_SET(n, p) \
  do { \
    if ((n) < FD_SETSIZE) { \
      (p)->bits[(n)/32] |= (1 << (n)%32); \
    } \
  } while (0)
#define FD_CLR(n, p) \
  do { \
    if ((n) < FD_SETSIZE) { \
      (p)->bits[(n)/32] &= ~(1 << (n)%32); \
    } \
  } while (0)
#define FD_ISSET(n, p) ((n) < FD_SETSIZE ? ((p)->bits[(n)/32] & (1 << (n)%32)) : 0)
#define FD_ZERO(p) memset((p), 0, sizeof(fd_set))

#define SELECT_TYPE fd_set

/* ------------------------------------------------------------------------- */

/* child_process.status values */
enum {
  STATUS_READ_ERROR = -1,
  STATUS_READ_READY,
  STATUS_READ_IN_PROGRESS,
  STATUS_READ_FAILED,
  STATUS_READ_SUCCEEDED,
  STATUS_READ_ACKNOWLEDGED
};

/* This structure is used for both pipes and sockets; for
   a socket, the process handle in pi is NULL. */
typedef struct _child_process
{
  int                   fd;
  int                   pid;
  HANDLE                char_avail;
  HANDLE                char_consumed;
  HANDLE                thrd;
  HWND                  hwnd;
  PROCESS_INFORMATION   procinfo;
  volatile int          status;
  char                  chr;
  OVERLAPPED            ovl_read;
  OVERLAPPED            ovl_write;
} child_process;

#define MAXDESC FD_SETSIZE
#define MAX_CHILDREN  MAXDESC/2
#define CHILD_ACTIVE(cp) ((cp)->char_avail != NULL)

/* parallel array of private info on file handles */
typedef struct
{
  unsigned         flags;
  HANDLE           hnd;
  child_process *  cp;
} filedesc;

extern filedesc fd_info [ MAXDESC ];

/* fd_info flag definitions */
#define FILE_READ               0x0001
#define FILE_WRITE              0x0002
#define FILE_LISTEN		0x0004
#define FILE_BINARY             0x0010
#define FILE_LAST_CR            0x0020
#define FILE_AT_EOF             0x0040
#define FILE_SEND_SIGCHLD       0x0080
#define FILE_PIPE               0x0100
#define FILE_SOCKET             0x0200
#define FILE_NDELAY             0x0400
#define FILE_SERIAL             0x0800

extern child_process * new_child (void);
extern void delete_child (child_process *cp);

/* ------------------------------------------------------------------------- */

/* Equivalent of strerror for W32 error codes.  */
extern char * w32_strerror (int error_no);

/* Validate a pointer.  */
extern int w32_valid_pointer_p (void *, int);

/* Get long (aka "true") form of file name, if it exists.  */
extern BOOL w32_get_long_filename (char * name, char * buf, int size);

/* Prepare our standard handles for proper inheritance by child processes.  */
extern void prepare_standard_handles (int in, int out,
				      int err, HANDLE handles[4]);

/* Reset our standard handles to their original state.  */
extern void reset_standard_handles (int in, int out,
				    int err, HANDLE handles[4]);

/* Return the string resource associated with KEY of type TYPE.  */
extern LPBYTE w32_get_resource (char * key, LPDWORD type);

extern void init_ntproc (void);
extern void term_ntproc (void);
extern void globals_of_w32 (void);
extern void syms_of_w32term (void);
extern void syms_of_w32fns (void);
extern void globals_of_w32fns (void);
extern void syms_of_w32select (void);
extern void globals_of_w32select (void);
extern void term_w32select (void);
extern void syms_of_w32menu (void);
extern void globals_of_w32menu (void);
extern void syms_of_fontset (void);
extern void syms_of_w32font (void);

extern int _sys_read_ahead (int fd);
extern int _sys_wait_accept (int fd);

extern Lisp_Object Vlibrary_cache, QCloaded_from;
extern HMODULE w32_delayed_load (Lisp_Object, Lisp_Object);

#ifdef HAVE_GNUTLS
#include <gnutls/gnutls.h>

/* GnuTLS pull (read from remote) interface.  */
extern ssize_t emacs_gnutls_pull (gnutls_transport_ptr_t p,
                                  void* buf, size_t sz);

/* GnuTLS push (write to remote) interface.  */
extern ssize_t emacs_gnutls_push (gnutls_transport_ptr_t p,
                                  const void* buf, size_t sz);
#endif /* HAVE_GNUTLS */

#endif /* EMACS_W32_H */

