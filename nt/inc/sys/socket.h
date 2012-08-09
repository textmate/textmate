/* Copyright (C) 1995, 2001-2012  Free Software Foundation, Inc.

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


/* Workable version of <sys/socket.h> based on winsock.h */

#ifndef _SOCKET_H_
#define _SOCKET_H_

/* defeat the multiple include protection */
#ifdef _WINSOCKAPI_
#undef _WINSOCKAPI_
#endif
#ifdef _WINSOCK_H
#undef _WINSOCK_H
#endif

/* avoid confusion with our version of select */
#ifdef select
#undef select
#define MUST_REDEF_SELECT
#endif

/* avoid clashing with our version of FD_SET if already defined */
#ifdef FD_SET
#undef FD_SET
#undef FD_CLR
#undef FD_ISSET
#undef FD_ZERO
#endif

/* avoid duplicate definition of timeval */
#ifdef HAVE_TIMEVAL
#define timeval ws_timeval
#endif

#include <winsock2.h>
#include <ws2tcpip.h>
/* process.c uses uint16_t (from C99) for IPv6, but
   apparently it is not defined in some versions of mingw and msvc.  */
#ifndef UINT16_C
typedef unsigned short uint16_t;
#endif

/* redefine select to reference our version */
#ifdef MUST_REDEF_SELECT
#define select sys_select
#undef MUST_REDEF_SELECT
#endif

/* revert to our version of FD_SET */
#undef FD_SET
#undef FD_CLR
#undef FD_ISSET
#undef FD_ZERO

/* allow us to provide our own version of fd_set */
#define fd_set ws_fd_set
#include "w32.h"

#ifdef HAVE_TIMEVAL
#undef timeval
#endif

/* shadow functions where we provide our own wrapper */
#define socket         sys_socket
#define bind           sys_bind
#define connect        sys_connect
#define htons          sys_htons
#define ntohs          sys_ntohs
#define inet_addr      sys_inet_addr
#define gethostname    sys_gethostname
#define gethostbyname  sys_gethostbyname
#define getpeername    sys_getpeername
#define getservbyname  sys_getservbyname
#define shutdown       sys_shutdown
#define setsockopt     sys_setsockopt
#define listen         sys_listen
#define getsockname    sys_getsockname
#define accept         sys_accept
#define recvfrom       sys_recvfrom
#define sendto         sys_sendto

int sys_socket(int af, int type, int protocol);
int sys_bind (int s, const struct sockaddr *addr, int namelen);
int sys_connect (int s, const struct sockaddr *addr, int namelen);
u_short sys_htons (u_short hostshort);
u_short sys_ntohs (u_short netshort);
unsigned long sys_inet_addr (const char * cp);
int sys_gethostname (char * name, int namelen);
struct hostent * sys_gethostbyname (const char * name);
struct servent * sys_getservbyname (const char * name, const char * proto);
int sys_getpeername (int s, struct sockaddr *addr, int * namelen);
int sys_shutdown (int socket, int how);
int sys_setsockopt (int s, int level, int oname, const void * oval, int olen);
int sys_listen (int s, int backlog);
int sys_getsockname (int s, struct sockaddr * name, int * namelen);
int sys_accept (int s, struct sockaddr *addr, int *addrlen);
int sys_recvfrom (int s, char *buf, int len, int flags,
		  struct sockaddr *from, int * fromlen);
int sys_sendto (int s, const char * buf, int len, int flags,
		const struct sockaddr *to, int tolen);

/* In addition to wrappers for the winsock functions, we also provide
   an fcntl function, for setting sockets to non-blocking mode.  */
int fcntl (int s, int cmd, int options);
#define F_SETFL   4
#define O_NDELAY  04000

/* we are providing a real h_errno variable */
#undef h_errno
extern int h_errno;

/* map winsock error codes to standard names */
#define EWOULDBLOCK             WSAEWOULDBLOCK
#define EINPROGRESS             WSAEINPROGRESS
#define EALREADY                WSAEALREADY
#define ENOTSOCK                WSAENOTSOCK
#define EDESTADDRREQ            WSAEDESTADDRREQ
#define EMSGSIZE                WSAEMSGSIZE
#define EPROTOTYPE              WSAEPROTOTYPE
#define ENOPROTOOPT             WSAENOPROTOOPT
#define EPROTONOSUPPORT         WSAEPROTONOSUPPORT
#define ESOCKTNOSUPPORT         WSAESOCKTNOSUPPORT
#define EOPNOTSUPP              WSAEOPNOTSUPP
#define EPFNOSUPPORT            WSAEPFNOSUPPORT
#define EAFNOSUPPORT            WSAEAFNOSUPPORT
#define EADDRINUSE              WSAEADDRINUSE
#define EADDRNOTAVAIL           WSAEADDRNOTAVAIL
#define ENETDOWN                WSAENETDOWN
#define ENETUNREACH             WSAENETUNREACH
#define ENETRESET               WSAENETRESET
#define ECONNABORTED            WSAECONNABORTED
#define ECONNRESET              WSAECONNRESET
#define ENOBUFS                 WSAENOBUFS
#define EISCONN                 WSAEISCONN
#define ENOTCONN                WSAENOTCONN
#define ESHUTDOWN               WSAESHUTDOWN
#define ETOOMANYREFS            WSAETOOMANYREFS
#define ETIMEDOUT               WSAETIMEDOUT
#define ECONNREFUSED            WSAECONNREFUSED
#define ELOOP                   WSAELOOP
/* #define ENAMETOOLONG            WSAENAMETOOLONG */
#define EHOSTDOWN               WSAEHOSTDOWN
#define EHOSTUNREACH            WSAEHOSTUNREACH
/* #define ENOTEMPTY               WSAENOTEMPTY */
#define EPROCLIM                WSAEPROCLIM
#define EUSERS                  WSAEUSERS
#define EDQUOT                  WSAEDQUOT
#define ESTALE                  WSAESTALE
#define EREMOTE                 WSAEREMOTE

#endif /* _SOCKET_H_ */

/* end of socket.h */
