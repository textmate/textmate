/* pop.c: client routines for talking to a POP3-protocol post-office server

Copyright (C) 1991, 1993, 1996-1997, 1999, 2001-2012
  Free Software Foundation, Inc.

Author: Jonathan Kamens <jik@security.ov.com>

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


#ifdef HAVE_CONFIG_H
#include <config.h>
#else
#define MAIL_USE_POP
#endif

#ifdef MAIL_USE_POP

#include <sys/types.h>
#ifdef WINDOWSNT
#include "ntlib.h"
#include <winsock.h>
#undef SOCKET_ERROR
#define RECV(s,buf,len,flags) recv (s,buf,len,flags)
#define SEND(s,buf,len,flags) send (s,buf,len,flags)
#define CLOSESOCKET(s) closesocket (s)
#else
#include <netinet/in.h>
#include <sys/socket.h>
#define RECV(s,buf,len,flags) read (s,buf,len)
#define SEND(s,buf,len,flags) write (s,buf,len)
#define CLOSESOCKET(s) close (s)
#endif
#include <pop.h>

#ifdef sun
#include <malloc.h>
#endif /* sun */

#ifdef HESIOD
#include <hesiod.h>
/*
 * It really shouldn't be necessary to put this declaration here, but
 * the version of hesiod.h that Athena has installed in release 7.2
 * doesn't declare this function; I don't know if the 7.3 version of
 * hesiod.h does.
 */
extern struct servent *hes_getservbyname (/* char *, char * */);
#endif

#include <pwd.h>
#include <netdb.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#ifdef KERBEROS
# ifdef HAVE_KRB5_H
#  include <krb5.h>
# endif
# ifdef HAVE_KRB_H
#  include <krb.h>
# else
#  ifdef HAVE_KERBEROSIV_KRB_H
#   include <kerberosIV/krb.h>
#  else
#   ifdef HAVE_KERBEROS_KRB_H
#    include <kerberos/krb.h>
#   endif
#  endif
# endif
# ifdef HAVE_COM_ERR_H
#  include <com_err.h>
# endif
#endif /* KERBEROS */

#include <min-max.h>

#ifdef KERBEROS
#ifndef KERBEROS5
extern int krb_sendauth (/* long, int, KTEXT, char *, char *, char *,
			    u_long, MSG_DAT *, CREDENTIALS *, Key_schedule,
			    struct sockaddr_in *, struct sockaddr_in *,
			    char * */);
extern char *krb_realmofhost (/* char * */);
#endif /* ! KERBEROS5 */
#endif /* KERBEROS */

#ifndef WINDOWSNT
#if !defined (HAVE_H_ERRNO) || !defined (HAVE_CONFIG_H)
extern int h_errno;
#endif
#endif

static int socket_connection (char *, int);
static int pop_getline (popserver, char **);
static int sendline (popserver, const char *);
static int fullwrite (int, char *, int);
static int getok (popserver);
#if 0
static int gettermination (popserver);
#endif
static void pop_trash (popserver);
static char *find_crlf (char *, int);

#define ERROR_MAX 160		/* a pretty arbitrary size, but needs
				   to be bigger than the original
				   value of 80 */
#define POP_PORT 110
#define POP_SERVICE "pop3"	/* we don't want the POP2 port! */
#ifdef KERBEROS
#define KPOP_PORT 1109
#define KPOP_SERVICE "kpop"	/* never used: look for 20060515 to see why */
#endif

char pop_error[ERROR_MAX];
int pop_debug = 0;

/*
 * Function: pop_open (char *host, char *username, char *password,
 * 		       int flags)
 *
 * Purpose: Establishes a connection with a post-office server, and
 * 	completes the authorization portion of the session.
 *
 * Arguments:
 * 	host	The server host with which the connection should be
 * 		established.  Optional.  If omitted, internal
 * 		heuristics will be used to determine the server host,
 * 		if possible.
 * 	username
 * 		The username of the mail-drop to access.  Optional.
 * 		If omitted, internal heuristics will be used to
 * 		determine the username, if possible.
 * 	password
 * 		The password to use for authorization.  If omitted,
 * 		internal heuristics will be used to determine the
 * 		password, if possible.
 * 	flags	A bit mask containing flags controlling certain
 * 		functions of the routine.  Valid flags are defined in
 * 		the file pop.h
 *
 * Return value: Upon successful establishment of a connection, a
 * 	non-null popserver will be returned.  Otherwise, null will be
 * 	returned, and the string variable pop_error will contain an
 * 	explanation of the error.
 */
popserver
pop_open (char *host, char *username, char *password, int flags)
{
  int sock;
  popserver server;

  /* Determine the user name */
  if (! username)
    {
      username = getenv ("USER");
      if (! (username && *username))
	{
	  username = getlogin ();
	  if (! (username && *username))
	    {
	      struct passwd *passwd;
	      passwd = getpwuid (getuid ());
	      if (passwd && passwd->pw_name && *passwd->pw_name)
		{
		  username = passwd->pw_name;
		}
	      else
		{
		  strcpy (pop_error, "Could not determine username");
		  return (0);
		}
	    }
	}
    }

  /*
   *  Determine the mail host.
   */

  if (! host)
    {
      host = getenv ("MAILHOST");
    }

#ifdef HESIOD
  if ((! host) && (! (flags & POP_NO_HESIOD)))
    {
      struct hes_postoffice *office;
      office = hes_getmailhost (username);
      if (office && office->po_type && (! strcmp (office->po_type, "POP"))
	  && office->po_name && *office->po_name && office->po_host
	  && *office->po_host)
	{
	  host = office->po_host;
	  username = office->po_name;
	}
    }
#endif

#ifdef MAILHOST
  if (! host)
    {
      host = MAILHOST;
    }
#endif

  if (! host)
    {
      strcpy (pop_error, "Could not determine POP server");
      return (0);
    }

  /* Determine the password */
#ifdef KERBEROS
#define DONT_NEED_PASSWORD (! (flags & POP_NO_KERBEROS))
#else
#define DONT_NEED_PASSWORD 0
#endif

  if ((! password) && (! DONT_NEED_PASSWORD))
    {
      if (! (flags & POP_NO_GETPASS))
	{
	  password = getpass ("Enter POP password:");
	}
      if (! password)
	{
	  strcpy (pop_error, "Could not determine POP password");
	  return (0);
	}
    }
  if (password)			/* always true, detected 20060515 */
    flags |= POP_NO_KERBEROS;
  else
    password = username;	/* dead code, detected 20060515 */
  /** "kpop" service is  never used: look for 20060515 to see why **/

  sock = socket_connection (host, flags);
  if (sock == -1)
    return (0);

  server = (popserver) malloc (sizeof (struct _popserver));
  if (! server)
    {
      strcpy (pop_error, "Out of memory in pop_open");
      return (0);
    }
  server->buffer = (char *) malloc (GETLINE_MIN);
  if (! server->buffer)
    {
      strcpy (pop_error, "Out of memory in pop_open");
      free ((char *) server);
      return (0);
    }

  server->file = sock;
  server->data = 0;
  server->buffer_index = 0;
  server->buffer_size = GETLINE_MIN;
  server->in_multi = 0;
  server->trash_started = 0;

  if (getok (server))
    return (0);

  /*
   * I really shouldn't use the pop_error variable like this, but....
   */
  if (strlen (username) > ERROR_MAX - 6)
    {
      pop_close (server);
      strcpy (pop_error,
	      "Username too long; recompile pop.c with larger ERROR_MAX");
      return (0);
    }
  sprintf (pop_error, "USER %s", username);

  if (sendline (server, pop_error) || getok (server))
    {
      return (0);
    }

  if (strlen (password) > ERROR_MAX - 6)
    {
      pop_close (server);
      strcpy (pop_error,
	      "Password too long; recompile pop.c with larger ERROR_MAX");
      return (0);
    }
  sprintf (pop_error, "PASS %s", password);

  if (sendline (server, pop_error) || getok (server))
    {
      return (0);
    }

  return (server);
}

/*
 * Function: pop_stat
 *
 * Purpose: Issue the STAT command to the server and return (in the
 * 	value parameters) the number of messages in the maildrop and
 * 	the total size of the maildrop.
 *
 * Return value: 0 on success, or non-zero with an error in pop_error
 * 	in failure.
 *
 * Side effects: On failure, may make further operations on the
 * 	connection impossible.
 */
int
pop_stat (popserver server, int *count, int *size)
{
  char *fromserver;
  char *end_ptr;

  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_stat");
      return (-1);
    }

  if (sendline (server, "STAT") || (pop_getline (server, &fromserver) < 0))
    return (-1);

  if (strncmp (fromserver, "+OK ", 4))
    {
      if (0 == strncmp (fromserver, "-ERR", 4))
	{
	  strncpy (pop_error, fromserver, ERROR_MAX);
	  pop_error[ERROR_MAX-1] = '\0';
	}
      else
	{
	  strcpy (pop_error,
		  "Unexpected response from POP server in pop_stat");
	  pop_trash (server);
	}
      return (-1);
    }

  errno = 0;
  *count = strtol (&fromserver[4], &end_ptr, 10);
  /* Check validity of string-to-integer conversion. */
  if (fromserver + 4 == end_ptr || *end_ptr != ' ' || errno)
    {
      strcpy (pop_error, "Unexpected response from POP server in pop_stat");
      pop_trash (server);
      return (-1);
    }

  fromserver = end_ptr;

  errno = 0;
  *size = strtol (fromserver + 1, &end_ptr, 10);
  if (fromserver + 1 == end_ptr || errno)
    {
      strcpy (pop_error, "Unexpected response from POP server in pop_stat");
      pop_trash (server);
      return (-1);
    }

  return (0);
}

/*
 * Function: pop_list
 *
 * Purpose: Performs the POP "list" command and returns (in value
 * 	parameters) two malloc'd zero-terminated arrays -- one of
 * 	message IDs, and a parallel one of sizes.
 *
 * Arguments:
 * 	server	The pop connection to talk to.
 * 	message	The number of the one message about which to get
 * 		information, or 0 to get information about all
 * 		messages.
 *
 * Return value: 0 on success, non-zero with error in pop_error on
 * 	failure.
 *
 * Side effects: On failure, may make further operations on the
 * 	connection impossible.
 */
int
pop_list (popserver server, int message, int **IDs, int **sizes)
{
  int how_many, i;
  char *fromserver;

  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_list");
      return (-1);
    }

  if (message)
    how_many = 1;
  else
    {
      int count, size;
      if (pop_stat (server, &count, &size))
	return (-1);
      how_many = count;
    }

  *IDs = (int *) malloc ((how_many + 1) * sizeof (int));
  *sizes = (int *) malloc ((how_many + 1) * sizeof (int));
  if (! (*IDs && *sizes))
    {
      strcpy (pop_error, "Out of memory in pop_list");
      return (-1);
    }

  if (message)
    {
      sprintf (pop_error, "LIST %d", message);
      if (sendline (server, pop_error))
	{
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      if (pop_getline (server, &fromserver) < 0)
	{
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      if (strncmp (fromserver, "+OK ", 4))
	{
	  if (! strncmp (fromserver, "-ERR", 4))
	    {
	      strncpy (pop_error, fromserver, ERROR_MAX);
	      pop_error[ERROR_MAX-1] = '\0';
	    }
	  else
	    {
	      strcpy (pop_error,
		      "Unexpected response from server in pop_list");
	      pop_trash (server);
	    }
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      (*IDs)[0] = atoi (&fromserver[4]);
      fromserver = strchr (&fromserver[4], ' ');
      if (! fromserver)
	{
	  strcpy (pop_error,
		  "Badly formatted response from server in pop_list");
	  pop_trash (server);
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      (*sizes)[0] = atoi (fromserver);
      (*IDs)[1] = (*sizes)[1] = 0;
      return (0);
    }
  else
    {
      if (pop_multi_first (server, "LIST", &fromserver))
	{
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      for (i = 0; i < how_many; i++)
	{
	  if (pop_multi_next (server, &fromserver) <= 0)
	    {
	      free ((char *) *IDs);
	      free ((char *) *sizes);
	      return (-1);
	    }
	  (*IDs)[i] = atoi (fromserver);
	  fromserver = strchr (fromserver, ' ');
	  if (! fromserver)
	    {
	      strcpy (pop_error,
		      "Badly formatted response from server in pop_list");
	      free ((char *) *IDs);
	      free ((char *) *sizes);
	      pop_trash (server);
	      return (-1);
	    }
	  (*sizes)[i] = atoi (fromserver);
	}
      if (pop_multi_next (server, &fromserver) < 0)
	{
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      else if (fromserver)
	{
	  strcpy (pop_error,
		  "Too many response lines from server in pop_list");
	  free ((char *) *IDs);
	  free ((char *) *sizes);
	  return (-1);
	}
      (*IDs)[i] = (*sizes)[i] = 0;
      return (0);
    }
}

/*
 * Function: pop_retrieve
 *
 * Purpose: Retrieve a specified message from the maildrop.
 *
 * Arguments:
 * 	server	The server to retrieve from.
 * 	message	The message number to retrieve.
 *	markfrom
 * 		If true, then mark the string "From " at the beginning
 * 		of lines with '>'.
 *	msg_buf	Output parameter to which a buffer containing the
 * 		message is assigned.
 *
 * Return value: The number of bytes in msg_buf, which may contain
 * 	embedded nulls, not including its final null, or -1 on error
 * 	with pop_error set.
 *
 * Side effects: May kill connection on error.
 */
int
pop_retrieve (popserver server, int message, int markfrom, char **msg_buf)
{
  int *IDs, *sizes, bufsize, fromcount = 0, cp = 0;
  char *ptr, *fromserver;
  int ret;

  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_retrieve");
      return (-1);
    }

  if (pop_list (server, message, &IDs, &sizes))
    return (-1);

  if (pop_retrieve_first (server, message, &fromserver))
    {
      return (-1);
    }

  /*
   * The "5" below is an arbitrary constant -- I assume that if
   * there are "From" lines in the text to be marked, there
   * probably won't be more than 5 of them.  If there are, I
   * allocate more space for them below.
   */
  bufsize = sizes[0] + (markfrom ? 5 : 0);
  ptr = (char *)malloc (bufsize);
  free ((char *) IDs);
  free ((char *) sizes);

  if (! ptr)
    {
      strcpy (pop_error, "Out of memory in pop_retrieve");
      pop_retrieve_flush (server);
      return (-1);
    }

  while ((ret = pop_retrieve_next (server, &fromserver)) >= 0)
    {
      if (! fromserver)
	{
	  ptr[cp] = '\0';
	  *msg_buf = ptr;
	  return (cp);
	}
      if (markfrom && fromserver[0] == 'F' && fromserver[1] == 'r' &&
	  fromserver[2] == 'o' && fromserver[3] == 'm' &&
	  fromserver[4] == ' ')
	{
	  if (++fromcount == 5)
	    {
	      bufsize += 5;
	      ptr = (char *)realloc (ptr, bufsize);
	      if (! ptr)
		{
		  strcpy (pop_error, "Out of memory in pop_retrieve");
		  pop_retrieve_flush (server);
		  return (-1);
		}
	      fromcount = 0;
	    }
	  ptr[cp++] = '>';
	}
      memcpy (&ptr[cp], fromserver, ret);
      cp += ret;
      ptr[cp++] = '\n';
    }

  free (ptr);
  return (-1);
}

int
pop_retrieve_first (popserver server, int message, char **response)
{
  sprintf (pop_error, "RETR %d", message);
  return (pop_multi_first (server, pop_error, response));
}

/*
  Returns a negative number on error, 0 to indicate that the data has
  all been read (i.e., the server has returned a "." termination
  line), or a positive number indicating the number of bytes in the
  returned buffer (which is null-terminated and may contain embedded
  nulls, but the returned bytecount doesn't include the final null).
  */

int
pop_retrieve_next (popserver server, char **line)
{
  return (pop_multi_next (server, line));
}

int
pop_retrieve_flush (popserver server)
{
  return (pop_multi_flush (server));
}

int
pop_top_first (popserver server, int message, int lines, char **response)
{
  sprintf (pop_error, "TOP %d %d", message, lines);
  return (pop_multi_first (server, pop_error, response));
}

/*
  Returns a negative number on error, 0 to indicate that the data has
  all been read (i.e., the server has returned a "." termination
  line), or a positive number indicating the number of bytes in the
  returned buffer (which is null-terminated and may contain embedded
  nulls, but the returned bytecount doesn't include the final null).
  */

int
pop_top_next (popserver server, char **line)
{
  return (pop_multi_next (server, line));
}

int
pop_top_flush (popserver server)
{
  return (pop_multi_flush (server));
}

int
pop_multi_first (popserver server, const char *command, char **response)
{
  if (server->in_multi)
    {
      strcpy (pop_error,
	      "Already in multi-line query in pop_multi_first");
      return (-1);
    }

  if (sendline (server, command) || (pop_getline (server, response) < 0))
    {
      return (-1);
    }

  if (0 == strncmp (*response, "-ERR", 4))
    {
      strncpy (pop_error, *response, ERROR_MAX);
      pop_error[ERROR_MAX-1] = '\0';
      return (-1);
    }
  else if (0 == strncmp (*response, "+OK", 3))
    {
      for (*response += 3; **response == ' '; (*response)++) /* empty */;
      server->in_multi = 1;
      return (0);
    }
  else
    {
      strcpy (pop_error,
	      "Unexpected response from server in pop_multi_first");
      return (-1);
    }
}

/*
  Read the next line of data from SERVER and place a pointer to it
  into LINE.  Return -1 on error, 0 if there are no more lines to read
  (i.e., the server has returned a line containing only "."), or a
  positive number indicating the number of bytes in the LINE buffer
  (not including the final null).  The data in that buffer may contain
  embedded nulls, but does not contain the final CRLF. When returning
  0, LINE is set to null. */

int
pop_multi_next (popserver server, char **line)
{
  char *fromserver;
  int ret;

  if (! server->in_multi)
    {
      strcpy (pop_error, "Not in multi-line query in pop_multi_next");
      return (-1);
    }

  if ((ret = pop_getline (server, &fromserver)) < 0)
    {
      return (-1);
    }

  if (fromserver[0] == '.')
    {
      if (! fromserver[1])
	{
	  *line = 0;
	  server->in_multi = 0;
	  return (0);
	}
      else
	{
	  *line = fromserver + 1;
	  return (ret - 1);
	}
    }
  else
    {
      *line = fromserver;
      return (ret);
    }
}

int
pop_multi_flush (popserver server)
{
  char *line;
  int ret;

  if (! server->in_multi)
    {
      return (0);
    }

  while ((ret = pop_multi_next (server, &line)))
    {
      if (ret < 0)
	return (-1);
    }

  return (0);
}

/* Function: pop_delete
 *
 * Purpose: Delete a specified message.
 *
 * Arguments:
 * 	server	Server from which to delete the message.
 * 	message	Message to delete.
 *
 * Return value: 0 on success, non-zero with error in pop_error
 * 	otherwise.
 */
int
pop_delete (popserver server, int message)
{
  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_delete");
      return (-1);
    }

  sprintf (pop_error, "DELE %d", message);

  if (sendline (server, pop_error) || getok (server))
    return (-1);

  return (0);
}

/*
 * Function: pop_noop
 *
 * Purpose: Send a noop command to the server.
 *
 * Argument:
 * 	server	The server to send to.
 *
 * Return value: 0 on success, non-zero with error in pop_error
 * 	otherwise.
 *
 * Side effects: Closes connection on error.
 */
int
pop_noop (popserver server)
{
  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_noop");
      return (-1);
    }

  if (sendline (server, "NOOP") || getok (server))
    return (-1);

  return (0);
}

/*
 * Function: pop_last
 *
 * Purpose: Find out the highest seen message from the server.
 *
 * Arguments:
 * 	server	The server.
 *
 * Return value: If successful, the highest seen message, which is
 * 	greater than or equal to 0.  Otherwise, a negative number with
 * 	the error explained in pop_error.
 *
 * Side effects: Closes the connection on error.
 */
int
pop_last (popserver server)
{
  char *fromserver;

  if (server->in_multi)
    {
      strcpy (pop_error, "In multi-line query in pop_last");
      return (-1);
    }

  if (sendline (server, "LAST"))
    return (-1);

  if (pop_getline (server, &fromserver) < 0)
    return (-1);

  if (! strncmp (fromserver, "-ERR", 4))
    {
      strncpy (pop_error, fromserver, ERROR_MAX);
      pop_error[ERROR_MAX-1] = '\0';
      return (-1);
    }
  else if (strncmp (fromserver, "+OK ", 4))
    {
      strcpy (pop_error, "Unexpected response from server in pop_last");
      pop_trash (server);
      return (-1);
    }
  else
    {
      char *end_ptr;
      int count;
      errno = 0;
      count = strtol (&fromserver[4], &end_ptr, 10);
      if (fromserver + 4 == end_ptr || errno)
	{
	  strcpy (pop_error, "Unexpected response from server in pop_last");
	  pop_trash (server);
	  return (-1);
	}
      return count;
    }
}

/*
 * Function: pop_reset
 *
 * Purpose: Reset the server to its initial connect state
 *
 * Arguments:
 * 	server	The server.
 *
 * Return value: 0 for success, non-0 with error in pop_error
 * 	otherwise.
 *
 * Side effects: Closes the connection on error.
 */
int
pop_reset (popserver server)
{
  if (pop_retrieve_flush (server))
    {
      return (-1);
    }

  if (sendline (server, "RSET") || getok (server))
    return (-1);

  return (0);
}

/*
 * Function: pop_quit
 *
 * Purpose: Quit the connection to the server,
 *
 * Arguments:
 * 	server	The server to quit.
 *
 * Return value: 0 for success, non-zero otherwise with error in
 * 	pop_error.
 *
 * Side Effects: The popserver passed in is unusable after this
 * 	function is called, even if an error occurs.
 */
int
pop_quit (popserver server)
{
  int ret = 0;

  if (server->file >= 0)
    {
      if (pop_retrieve_flush (server))
	{
	  ret = -1;
	}

      if (sendline (server, "QUIT") || getok (server))
	{
	  ret = -1;
	}

      close (server->file);
    }

  free (server->buffer);
  free ((char *) server);

  return (ret);
}

#ifdef WINDOWSNT
static int have_winsock = 0;
#endif

/*
 * Function: socket_connection
 *
 * Purpose: Opens the network connection with the mail host, without
 * 	doing any sort of I/O with it or anything.
 *
 * Arguments:
 * 	host	The host to which to connect.
 *	flags	Option flags.
 *
 * Return value: A file descriptor indicating the connection, or -1
 * 	indicating failure, in which case an error has been copied
 * 	into pop_error.
 */
static int
socket_connection (char *host, int flags)
{
#ifdef HAVE_GETADDRINFO
  struct addrinfo *res, *it;
  struct addrinfo hints;
  int ret;
#else /* !HAVE_GETADDRINFO */
  struct hostent *hostent;
#endif
  struct servent *servent;
  struct sockaddr_in addr;
  char found_port = 0;
  const char *service;
  int sock;
  char *realhost;
#ifdef KERBEROS
#ifdef KERBEROS5
  krb5_error_code rem;
  krb5_context kcontext = 0;
  krb5_auth_context auth_context = 0;
  krb5_ccache ccdef;
  krb5_principal client, server;
  krb5_error *err_ret;
  register char *cp;
#else
  KTEXT ticket;
  MSG_DAT msg_data;
  CREDENTIALS cred;
  Key_schedule schedule;
  int rem;
#endif /* KERBEROS5 */
#endif /* KERBEROS */

  int try_count = 0;
  int connect_ok;

#ifdef WINDOWSNT
  {
    WSADATA winsockData;
    if (WSAStartup (0x101, &winsockData) == 0)
      have_winsock = 1;
  }
#endif

  memset (&addr, 0, sizeof (addr));
  addr.sin_family = AF_INET;

  /** "kpop" service is  never used: look for 20060515 to see why **/
#ifdef KERBEROS
  service = (flags & POP_NO_KERBEROS) ? POP_SERVICE : KPOP_SERVICE;
#else
  service = POP_SERVICE;
#endif

#ifdef HESIOD
  if (! (flags & POP_NO_HESIOD))
    {
      servent = hes_getservbyname (service, "tcp");
      if (servent)
	{
	  addr.sin_port = servent->s_port;
	  found_port = 1;
	}
    }
#endif
  if (! found_port)
    {
      servent = getservbyname (service, "tcp");
      if (servent)
	{
	  addr.sin_port = servent->s_port;
	}
      else
	{
  /** "kpop" service is  never used: look for 20060515 to see why **/
#ifdef KERBEROS
	  addr.sin_port = htons ((flags & POP_NO_KERBEROS) ?
				POP_PORT : KPOP_PORT);
#else
	  addr.sin_port = htons (POP_PORT);
#endif
	}
    }

#define POP_SOCKET_ERROR "Could not create socket for POP connection: "

  sock = socket (PF_INET, SOCK_STREAM, 0);
  if (sock < 0)
    {
      strcpy (pop_error, POP_SOCKET_ERROR);
      strncat (pop_error, strerror (errno),
	       ERROR_MAX - sizeof (POP_SOCKET_ERROR));
      return (-1);

    }

#ifdef HAVE_GETADDRINFO
  memset (&hints, 0, sizeof (hints));
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_CANONNAME;
  hints.ai_family = AF_INET;
  do
    {
      ret = getaddrinfo (host, service, &hints, &res);
      try_count++;
      if (ret != 0 && (ret != EAI_AGAIN || try_count == 5))
	{
	  strcpy (pop_error, "Could not determine POP server's address");
	  return (-1);
	}
    } while (ret != 0);

  if (ret == 0)
    {
      it = res;
      while (it)
        {
          if (it->ai_addrlen == sizeof (addr))
            {
              struct sockaddr_in *in_a = (struct sockaddr_in *) it->ai_addr;
              memcpy (&addr.sin_addr, &in_a->sin_addr, sizeof (addr.sin_addr));
              if (! connect (sock, (struct sockaddr *) &addr, sizeof (addr)))
                break;
            }
          it = it->ai_next;
        }
      connect_ok = it != NULL;
      if (connect_ok)
        {
          realhost = alloca (strlen (it->ai_canonname) + 1);
          strcpy (realhost, it->ai_canonname);
        }
      freeaddrinfo (res);
    }
#else /* !HAVE_GETADDRINFO */
  do
    {
      hostent = gethostbyname (host);
      try_count++;
      if ((! hostent) && ((h_errno != TRY_AGAIN) || (try_count == 5)))
	{
	  strcpy (pop_error, "Could not determine POP server's address");
	  return (-1);
	}
    } while (! hostent);

  while (*hostent->h_addr_list)
    {
      memcpy (&addr.sin_addr, *hostent->h_addr_list, hostent->h_length);
      if (! connect (sock, (struct sockaddr *) &addr, sizeof (addr)))
	break;
      hostent->h_addr_list++;
    }
  connect_ok = *hostent->h_addr_list != NULL;
  if (! connect_ok)
    {
      realhost = alloca (strlen (hostent->h_name) + 1);
      strcpy (realhost, hostent->h_name);
    }

#endif /* !HAVE_GETADDRINFO */

#define CONNECT_ERROR "Could not connect to POP server: "

  if (! connect_ok)
    {
      CLOSESOCKET (sock);
      strcpy (pop_error, CONNECT_ERROR);
      strncat (pop_error, strerror (errno),
	       ERROR_MAX - sizeof (CONNECT_ERROR));
      return (-1);

    }

#ifdef KERBEROS

#define KRB_ERROR "Kerberos error connecting to POP server: "
  if (! (flags & POP_NO_KERBEROS))
    {
#ifdef KERBEROS5
      if ((rem = krb5_init_context (&kcontext)))
	{
	krb5error:
	  if (auth_context)
	    krb5_auth_con_free (kcontext, auth_context);
	  if (kcontext)
	    krb5_free_context (kcontext);
	  strcpy (pop_error, KRB_ERROR);
	  strncat (pop_error, error_message (rem),
		   ERROR_MAX - sizeof (KRB_ERROR));
	  CLOSESOCKET (sock);
	  return (-1);
	}

      if ((rem = krb5_auth_con_init (kcontext, &auth_context)))
	goto krb5error;

      if (rem = krb5_cc_default (kcontext, &ccdef))
	goto krb5error;

      if (rem = krb5_cc_get_principal (kcontext, ccdef, &client))
	goto krb5error;

      for (cp = realhost; *cp; cp++)
	{
	  if (isupper (*cp))
	    {
	      *cp = tolower (*cp);
	    }
	}

      if (rem = krb5_sname_to_principal (kcontext, realhost,
					 POP_SERVICE, FALSE, &server))
	goto krb5error;

      rem = krb5_sendauth (kcontext, &auth_context,
			   (krb5_pointer) &sock, "KPOPV1.0", client, server,
			  AP_OPTS_MUTUAL_REQUIRED,
			  0,	/* no checksum */
			  0,	/* no creds, use ccache instead */
			  ccdef,
			  &err_ret,
			  0,	/* don't need subsession key */
			  0);	/* don't need reply */
      krb5_free_principal (kcontext, server);
      if (rem)
	{
	  strcpy (pop_error, KRB_ERROR);
	  strncat (pop_error, error_message (rem),
		   ERROR_MAX - sizeof (KRB_ERROR));
#if defined HAVE_KRB5_ERROR_TEXT
	  if (err_ret && err_ret->text.length)
	    {
	      strncat (pop_error, " [server says '",
		       ERROR_MAX - strlen (pop_error) - 1);
	      strncat (pop_error, err_ret->text.data,
		       min (ERROR_MAX - strlen (pop_error) - 1,
			    err_ret->text.length));
	      strncat (pop_error, "']",
		       ERROR_MAX - strlen (pop_error) - 1);
	    }
#elif defined HAVE_KRB5_ERROR_E_TEXT
	  if (err_ret && err_ret->e_text && strlen (*err_ret->e_text))
	    {
	      strncat (pop_error, " [server says '",
		       ERROR_MAX - strlen (pop_error) - 1);
	      strncat (pop_error, *err_ret->e_text,
		       ERROR_MAX - strlen (pop_error) - 1);
	      strncat (pop_error, "']",
		       ERROR_MAX - strlen (pop_error) - 1);
	    }
#endif
	  if (err_ret)
	    krb5_free_error (kcontext, err_ret);
	  krb5_auth_con_free (kcontext, auth_context);
	  krb5_free_context (kcontext);

	  CLOSESOCKET (sock);
	  return (-1);
	}
#else  /* ! KERBEROS5 */
      ticket = (KTEXT) malloc (sizeof (KTEXT_ST));
      rem = krb_sendauth (0L, sock, ticket, "pop", realhost,
			  (char *) krb_realmofhost (realhost),
			  (unsigned long) 0, &msg_data, &cred, schedule,
			  (struct sockaddr_in *) 0,
			  (struct sockaddr_in *) 0,
			  "KPOPV0.1");
      free ((char *) ticket);
      if (rem != KSUCCESS)
	{
	  strcpy (pop_error, KRB_ERROR);
	  strncat (pop_error, krb_err_txt[rem],
		   ERROR_MAX - sizeof (KRB_ERROR));
	  CLOSESOCKET (sock);
	  return (-1);
	}
#endif /* KERBEROS5 */
    }
#endif /* KERBEROS */

  return (sock);
} /* socket_connection */

/*
 * Function: pop_getline
 *
 * Purpose: Get a line of text from the connection and return a
 * 	pointer to it.  The carriage return and linefeed at the end of
 * 	the line are stripped, but periods at the beginnings of lines
 * 	are NOT dealt with in any special way.
 *
 * Arguments:
 * 	server	The server from which to get the line of text.
 *
 * Returns: The number of characters in the line, which is returned in
 * 	LINE, not including the final null.  A return value of 0
 * 	indicates a blank line.  A negative return value indicates an
 * 	error (in which case the contents of LINE are undefined.  In
 * 	case of error, an error message is copied into pop_error.
 *
 * Notes: The line returned is overwritten with each call to pop_getline.
 *
 * Side effects: Closes the connection on error.
 *
 * THE RETURNED LINE MAY CONTAIN EMBEDDED NULLS!
 */
static int
pop_getline (popserver server, char **line)
{
#define GETLINE_ERROR "Error reading from server: "

  int ret;
  int search_offset = 0;

  if (server->data)
    {
      char *cp = find_crlf (server->buffer + server->buffer_index,
			    server->data);
      if (cp)
	{
	  int found;
	  int data_used;

	  found = server->buffer_index;
	  data_used = (cp + 2) - server->buffer - found;

	  *cp = '\0';		/* terminate the string to be returned */
	  server->data -= data_used;
	  server->buffer_index += data_used;

	  if (pop_debug)
	    /* Embedded nulls will truncate this output prematurely,
	       but that's OK because it's just for debugging anyway. */
	    fprintf (stderr, "<<< %s\n", server->buffer + found);
	  *line = server->buffer + found;
	  return (data_used - 2);
	}
      else
	{
	  memmove (server->buffer, server->buffer + server->buffer_index,
		   server->data);
	  /* Record the fact that we've searched the data already in
             the buffer for a CRLF, so that when we search below, we
             don't have to search the same data twice.  There's a "-
             1" here to account for the fact that the last character
             of the data we have may be the CR of a CRLF pair, of
             which we haven't read the second half yet, so we may have
             to search it again when we read more data. */
	  search_offset = server->data - 1;
	  server->buffer_index = 0;
	}
    }
  else
    {
      server->buffer_index = 0;
    }

  while (1)
    {
      /* There's a "- 1" here to leave room for the null that we put
         at the end of the read data below.  We put the null there so
         that find_crlf knows where to stop when we call it. */
      if (server->data == server->buffer_size - 1)
	{
	  server->buffer_size += GETLINE_INCR;
	  server->buffer = (char *)realloc (server->buffer, server->buffer_size);
	  if (! server->buffer)
	    {
	      strcpy (pop_error, "Out of memory in pop_getline");
	      pop_trash (server);
	      return (-1);
	    }
	}
      ret = RECV (server->file, server->buffer + server->data,
		  server->buffer_size - server->data - 1, 0);
      if (ret < 0)
	{
	  strcpy (pop_error, GETLINE_ERROR);
	  strncat (pop_error, strerror (errno),
		   ERROR_MAX - sizeof (GETLINE_ERROR));
	  pop_trash (server);
	  return (-1);
	}
      else if (ret == 0)
	{
	  strcpy (pop_error, "Unexpected EOF from server in pop_getline");
	  pop_trash (server);
	  return (-1);
	}
      else
	{
	  char *cp;
	  server->data += ret;
	  server->buffer[server->data] = '\0';

	  cp = find_crlf (server->buffer + search_offset,
			  server->data - search_offset);
	  if (cp)
	    {
	      int data_used = (cp + 2) - server->buffer;
	      *cp = '\0';
	      server->data -= data_used;
	      server->buffer_index = data_used;

	      if (pop_debug)
		fprintf (stderr, "<<< %s\n", server->buffer);
	      *line = server->buffer;
	      return (data_used - 2);
	    }
	  /* As above, the "- 1" here is to account for the fact that
	     we may have read a CR without its accompanying LF. */
	  search_offset += ret - 1;
	}
    }

  /* NOTREACHED */
}

/*
 * Function: sendline
 *
 * Purpose: Sends a line of text to the POP server.  The line of text
 * 	passed into this function should NOT have the carriage return
 * 	and linefeed on the end of it.  Periods at beginnings of lines
 * 	will NOT be treated specially by this function.
 *
 * Arguments:
 * 	server	The server to which to send the text.
 * 	line	The line of text to send.
 *
 * Return value: Upon successful completion, a value of 0 will be
 * 	returned.  Otherwise, a non-zero value will be returned, and
 * 	an error will be copied into pop_error.
 *
 * Side effects: Closes the connection on error.
 */
static int
sendline (popserver server, const char *line)
{
#define SENDLINE_ERROR "Error writing to POP server: "
  int ret;
  char *buf;

  /* Combine the string and the CR-LF into one buffer.  Otherwise, two
     reasonable network stack optimizations, Nagle's algorithm and
     delayed acks, combine to delay us a fraction of a second on every
     message we send.  (Movemail writes line without \r\n, client
     kernel sends packet, server kernel delays the ack to see if it
     can combine it with data, movemail writes \r\n, client kernel
     waits because it has unacked data already in its outgoing queue,
     client kernel eventually times out and sends.)

     This can be something like 0.2s per command, which can add up
     over a few dozen messages, and is a big chunk of the time we
     spend fetching mail from a server close by.  */
  buf = alloca (strlen (line) + 3);
  strcpy (buf, line);
  strcat (buf, "\r\n");
  ret = fullwrite (server->file, buf, strlen (buf));

  if (ret < 0)
    {
      pop_trash (server);
      strcpy (pop_error, SENDLINE_ERROR);
      strncat (pop_error, strerror (errno),
	       ERROR_MAX - sizeof (SENDLINE_ERROR));
      return (ret);
    }

  if (pop_debug)
    fprintf (stderr, ">>> %s\n", line);

  return (0);
}

/*
 * Procedure: fullwrite
 *
 * Purpose: Just like write, but keeps trying until the entire string
 * 	has been written.
 *
 * Return value: Same as write.  Pop_error is not set.
 */
static int
fullwrite (int fd, char *buf, int nbytes)
{
  char *cp;
  int ret = 0;

  cp = buf;
  while (nbytes && ((ret = SEND (fd, cp, nbytes, 0)) > 0))
    {
      cp += ret;
      nbytes -= ret;
    }

  return (ret);
}

/*
 * Procedure getok
 *
 * Purpose: Reads a line from the server.  If the return indicator is
 * 	positive, return with a zero exit status.  If not, return with
 * 	a negative exit status.
 *
 * Arguments:
 * 	server	The server to read from.
 *
 * Returns: 0 for success, else for failure and puts error in pop_error.
 *
 * Side effects: On failure, may make the connection unusable.
 */
static int
getok (popserver server)
{
  char *fromline;

  if (pop_getline (server, &fromline) < 0)
    {
      return (-1);
    }

  if (! strncmp (fromline, "+OK", 3))
    return (0);
  else if (! strncmp (fromline, "-ERR", 4))
    {
      strncpy (pop_error, fromline, ERROR_MAX);
      pop_error[ERROR_MAX-1] = '\0';
      return (-1);
    }
  else
    {
      strcpy (pop_error,
	      "Unexpected response from server; expecting +OK or -ERR");
      pop_trash (server);
      return (-1);
    }
}

#if 0
/*
 * Function: gettermination
 *
 * Purpose: Gets the next line and verifies that it is a termination
 * 	line (nothing but a dot).
 *
 * Return value: 0 on success, non-zero with pop_error set on error.
 *
 * Side effects: Closes the connection on error.
 */
static int
gettermination (server)
     popserver server;
{
  char *fromserver;

  if (pop_getline (server, &fromserver) < 0)
    return (-1);

  if (strcmp (fromserver, "."))
    {
      strcpy (pop_error,
	      "Unexpected response from server in gettermination");
      pop_trash (server);
      return (-1);
    }

  return (0);
}
#endif

/*
 * Function pop_close
 *
 * Purpose: Close a pop connection, sending a "RSET" command to try to
 * 	preserve any changes that were made and a "QUIT" command to
 * 	try to get the server to quit, but ignoring any responses that
 * 	are received.
 *
 * Side effects: The server is unusable after this function returns.
 * 	Changes made to the maildrop since the session was started (or
 * 	since the last pop_reset) may be lost.
 */
void
pop_close (popserver server)
{
  pop_trash (server);
  free ((char *) server);

  return;
}

/*
 * Function: pop_trash
 *
 * Purpose: Like pop_close or pop_quit, but doesn't deallocate the
 * 	memory associated with the server.  It is valid to call
 * 	pop_close or pop_quit after this function has been called.
 */
static void
pop_trash (popserver server)
{
  if (server->file >= 0)
    {
      /* avoid recursion; sendline can call pop_trash */
      if (server->trash_started)
	return;
      server->trash_started = 1;

      sendline (server, "RSET");
      sendline (server, "QUIT");

      CLOSESOCKET (server->file);
      server->file = -1;
      if (server->buffer)
	{
	  free (server->buffer);
	  server->buffer = 0;
	}
    }

#ifdef WINDOWSNT
  if (have_winsock)
    WSACleanup ();
#endif
}

/* Return a pointer to the first CRLF in IN_STRING, which can contain
   embedded nulls and has LEN characters in it not including the final
   null, or 0 if it does not contain one.  */

static char *
find_crlf (char *in_string, int len)
{
  while (len--)
    {
      if (*in_string == '\r')
	{
	  if (*++in_string == '\n')
	    return (in_string - 1);
	}
      else
	in_string++;
    }
  return (0);
}

#endif /* MAIL_USE_POP */
