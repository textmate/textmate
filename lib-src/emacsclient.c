/* Client process that communicates with GNU Emacs acting as server.
   Copyright (C) 1986-1987, 1994, 1999-2012 Free Software Foundation, Inc.

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

#ifdef WINDOWSNT

/* config.h defines these, which disables sockets altogether! */
# undef _WINSOCKAPI_
# undef _WINSOCK_H

# include <malloc.h>
# include <stdlib.h>
# include <windows.h>
# include <commctrl.h>
# include <io.h>
# include <winsock2.h>

# define NO_SOCKETS_IN_FILE_SYSTEM

# define HSOCKET SOCKET
# define CLOSE_SOCKET closesocket
# define INITIALIZE() (initialize_sockets ())

char *w32_getenv (char *);
#define egetenv(VAR) w32_getenv(VAR)

#else /* !WINDOWSNT */

# include "syswait.h"

# ifdef HAVE_INET_SOCKETS
#  include <netinet/in.h>
#  ifdef HAVE_SOCKETS
#    include <sys/types.h>
#    include <sys/socket.h>
#    include <sys/un.h>
#  endif /* HAVE_SOCKETS */
# endif
# include <arpa/inet.h>

# define INVALID_SOCKET -1
# define HSOCKET int
# define CLOSE_SOCKET close
# define INITIALIZE()

# ifndef WCONTINUED
#  define WCONTINUED 8
# endif

#define egetenv(VAR) getenv(VAR)

#endif /* !WINDOWSNT */

#undef signal

#include <stdarg.h>
#include <ctype.h>
#include <stdio.h>
#include <getopt.h>
#include <unistd.h>

#include <pwd.h>
#include <sys/stat.h>
#include <signal.h>
#include <errno.h>



char *getenv (const char *), *getwd (char *);
#ifdef HAVE_GETCWD
char *(getcwd) (char *, size_t);
#endif

#ifndef VERSION
#define VERSION "unspecified"
#endif


#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

/* Additional space when allocating buffers for filenames, etc.  */
#define EXTRA_SPACE 100

/* Use this to suppress gcc's `...may be used before initialized' warnings. */
#ifdef lint
# define IF_LINT(Code) Code
#else
# define IF_LINT(Code) /* empty */
#endif

#ifdef min
#undef min
#endif
#define min(x, y) (((x) < (y)) ? (x) : (y))


/* Name used to invoke this program.  */
const char *progname;

/* The second argument to main. */
char **main_argv;

/* Nonzero means don't wait for a response from Emacs.  --no-wait.  */
int nowait = 0;

/* Nonzero means don't print messages for successful operations.  --quiet. */
int quiet = 0;

/* Nonzero means args are expressions to be evaluated.  --eval.  */
int eval = 0;

/* Nonzero means don't open a new frame.  Inverse of --create-frame.  */
int current_frame = 1;

/* The display on which Emacs should work.  --display.  */
const char *display = NULL;

/* The parent window ID, if we are opening a frame via XEmbed.  */
char *parent_id = NULL;

/* Nonzero means open a new Emacs frame on the current terminal. */
int tty = 0;

/* If non-NULL, the name of an editor to fallback to if the server
   is not running.  --alternate-editor.   */
const char *alternate_editor = NULL;

/* If non-NULL, the filename of the UNIX socket.  */
const char *socket_name = NULL;

/* If non-NULL, the filename of the authentication file.  */
const char *server_file = NULL;

/* PID of the Emacs server process.  */
int emacs_pid = 0;

/* If non-NULL, a string that should form a frame parameter alist to
   be used for the new frame */
const char *frame_parameters = NULL;

static void print_help_and_exit (void) NO_RETURN;
static void fail (void) NO_RETURN;


struct option longopts[] =
{
  { "no-wait",	no_argument,	   NULL, 'n' },
  { "quiet",	no_argument,	   NULL, 'q' },
  { "eval",	no_argument,	   NULL, 'e' },
  { "help",	no_argument,	   NULL, 'H' },
  { "version",	no_argument,	   NULL, 'V' },
  { "tty",	no_argument,       NULL, 't' },
  { "nw",	no_argument,       NULL, 't' },
  { "create-frame", no_argument,   NULL, 'c' },
  { "alternate-editor", required_argument, NULL, 'a' },
  { "frame-parameters", required_argument, NULL, 'F' },
#ifndef NO_SOCKETS_IN_FILE_SYSTEM
  { "socket-name",	required_argument, NULL, 's' },
#endif
  { "server-file",	required_argument, NULL, 'f' },
#ifndef WINDOWSNT
  { "display",	required_argument, NULL, 'd' },
#endif
  { "parent-id", required_argument, NULL, 'p' },
  { 0, 0, 0, 0 }
};


/* Like malloc but get fatal error if memory is exhausted.  */

static void *
xmalloc (size_t size)
{
  void *result = malloc (size);
  if (result == NULL)
    {
      perror ("malloc");
      exit (EXIT_FAILURE);
    }
  return result;
}

/* From sysdep.c */
#if !defined (HAVE_GET_CURRENT_DIR_NAME) || defined (BROKEN_GET_CURRENT_DIR_NAME)

/* From lisp.h */
#ifndef DIRECTORY_SEP
#define DIRECTORY_SEP '/'
#endif
#ifndef IS_DIRECTORY_SEP
#define IS_DIRECTORY_SEP(_c_) ((_c_) == DIRECTORY_SEP)
#endif
#ifndef IS_DEVICE_SEP
#ifndef DEVICE_SEP
#define IS_DEVICE_SEP(_c_) 0
#else
#define IS_DEVICE_SEP(_c_) ((_c_) == DEVICE_SEP)
#endif
#endif

char *get_current_dir_name (void);

/* Return the current working directory.  Returns NULL on errors.
   Any other returned value must be freed with free.  This is used
   only when get_current_dir_name is not defined on the system.  */
char*
get_current_dir_name (void)
{
  char *buf;
  const char *pwd;
  struct stat dotstat, pwdstat;
  /* If PWD is accurate, use it instead of calling getwd.  PWD is
     sometimes a nicer name, and using it may avoid a fatal error if a
     parent directory is searchable but not readable.  */
    if ((pwd = egetenv ("PWD")) != 0
      && (IS_DIRECTORY_SEP (*pwd) || (*pwd && IS_DEVICE_SEP (pwd[1])))
      && stat (pwd, &pwdstat) == 0
      && stat (".", &dotstat) == 0
      && dotstat.st_ino == pwdstat.st_ino
      && dotstat.st_dev == pwdstat.st_dev
#ifdef MAXPATHLEN
      && strlen (pwd) < MAXPATHLEN
#endif
      )
    {
      buf = (char *) xmalloc (strlen (pwd) + 1);
      strcpy (buf, pwd);
    }
#ifdef HAVE_GETCWD
  else
    {
      size_t buf_size = 1024;
      for (;;)
        {
	  int tmp_errno;
	  buf = malloc (buf_size);
	  if (! buf)
	    break;
          if (getcwd (buf, buf_size) == buf)
            break;
	  tmp_errno = errno;
	  free (buf);
	  if (tmp_errno != ERANGE)
            {
              errno = tmp_errno;
              return NULL;
            }
          buf_size *= 2;
	  if (! buf_size)
	    {
	      errno = ENOMEM;
	      return NULL;
	    }
        }
    }
#else
  else
    {
      /* We need MAXPATHLEN here.  */
      buf = (char *) xmalloc (MAXPATHLEN + 1);
      if (getwd (buf) == NULL)
        {
          int tmp_errno = errno;
          free (buf);
          errno = tmp_errno;
          return NULL;
        }
    }
#endif
  return buf;
}
#endif

#ifdef WINDOWSNT

/* Like strdup but get a fatal error if memory is exhausted. */

char *
xstrdup (const char *s)
{
  char *result = strdup (s);
  if (result == NULL)
    {
      perror ("strdup");
      exit (EXIT_FAILURE);
    }
  return result;
}

#define REG_ROOT "SOFTWARE\\GNU\\Emacs"

/* Retrieve an environment variable from the Emacs subkeys of the registry.
   Return NULL if the variable was not found, or it was empty.
   This code is based on w32_get_resource (w32.c).  */
char *
w32_get_resource (HKEY predefined, char *key, LPDWORD type)
{
  HKEY hrootkey = NULL;
  char *result = NULL;
  DWORD cbData;

  if (RegOpenKeyEx (predefined, REG_ROOT, 0, KEY_READ, &hrootkey) == ERROR_SUCCESS)
    {
      if (RegQueryValueEx (hrootkey, key, NULL, NULL, NULL, &cbData) == ERROR_SUCCESS)
	{
	  result = (char *) xmalloc (cbData);

	  if ((RegQueryValueEx (hrootkey, key, NULL, type, result, &cbData) != ERROR_SUCCESS)
	      || (*result == 0))
	    {
	      free (result);
	      result = NULL;
	    }
	}

      RegCloseKey (hrootkey);
    }

  return result;
}

/*
  getenv wrapper for Windows

  Value is allocated on the heap, and can be free'd.

  This is needed to duplicate Emacs's behavior, which is to look for
  environment variables in the registry if they don't appear in the
  environment.  */
char *
w32_getenv (char *envvar)
{
  char *value;
  DWORD dwType;

  if ((value = getenv (envvar)))
    /* Found in the environment.  strdup it, because values returned
       by getenv cannot be free'd.  */
    return xstrdup (value);

  if (! (value = w32_get_resource (HKEY_CURRENT_USER, envvar, &dwType)) &&
      ! (value = w32_get_resource (HKEY_LOCAL_MACHINE, envvar, &dwType)))
    {
      /* "w32console" is what Emacs on Windows uses for tty-type under -nw.  */
      if (strcmp (envvar, "TERM") == 0)
	return xstrdup ("w32console");
      /* Found neither in the environment nor in the registry.  */
      return NULL;
    }

  if (dwType == REG_SZ)
    /* Registry; no need to expand.  */
    return value;

  if (dwType == REG_EXPAND_SZ)
    {
      DWORD size;

      if ((size = ExpandEnvironmentStrings (value, NULL, 0)))
	{
	  char *buffer = (char *) xmalloc (size);
	  if (ExpandEnvironmentStrings (value, buffer, size))
	    {
	      /* Found and expanded.  */
	      free (value);
	      return buffer;
	    }

	  /* Error expanding.  */
	  free (buffer);
	}
    }

  /* Not the right type, or not correctly expanded.  */
  free (value);
  return NULL;
}

void
w32_set_user_model_id (void)
{
  HMODULE shell;
  HRESULT (WINAPI * set_user_model) (wchar_t * id);

  /* On Windows 7 and later, we need to set the user model ID
     to associate emacsclient launched files with Emacs frames
     in the UI.  */
  shell = LoadLibrary ("shell32.dll");
  if (shell)
    {
      set_user_model
	= (void *) GetProcAddress (shell,
				   "SetCurrentProcessExplicitAppUserModelID");
      /* If the function is defined, then we are running on Windows 7
	 or newer, and the UI uses this to group related windows
	 together.  Since emacs, runemacs, emacsclient are related, we
	 want them grouped even though the executables are different,
	 so we need to set a consistent ID between them.  */
      if (set_user_model)
	set_user_model (L"GNU.Emacs");

      FreeLibrary (shell);
    }
}

int
w32_window_app (void)
{
  static int window_app = -1;
  char szTitle[MAX_PATH];

  if (window_app < 0)
    {
      /* Checking for STDOUT does not work; it's a valid handle also in
         nonconsole apps.  Testing for the console title seems to work. */
      window_app = (GetConsoleTitleA (szTitle, MAX_PATH) == 0);
      if (window_app)
        InitCommonControls ();
    }

  return window_app;
}

/*
  execvp wrapper for Windows.  Quotes arguments with embedded spaces.

  This is necessary due to the broken implementation of exec* routines in
  the Microsoft libraries: they concatenate the arguments together without
  quoting special characters, and pass the result to CreateProcess, with
  predictably bad results.  By contrast, POSIX execvp passes the arguments
  directly into the argv array of the child process.
*/
int
w32_execvp (const char *path, char **argv)
{
  int i;

  /* Required to allow a .BAT script as alternate editor.  */
  argv[0] = (char *) alternate_editor;

  for (i = 0; argv[i]; i++)
    if (strchr (argv[i], ' '))
      {
	char *quoted = alloca (strlen (argv[i]) + 3);
	sprintf (quoted, "\"%s\"", argv[i]);
	argv[i] = quoted;
      }

  return execvp (path, argv);
}

#undef execvp
#define execvp w32_execvp

/* Emulation of ttyname for Windows.  */
char *
ttyname (int fd)
{
  return "CONOUT$";
}

#endif /* WINDOWSNT */

/* Display a normal or error message.
   On Windows, use a message box if compiled as a Windows app.  */
static void message (int, const char *, ...) ATTRIBUTE_FORMAT_PRINTF (2, 3);
static void
message (int is_error, const char *format, ...)
{
  va_list args;

  va_start (args, format);

#ifdef WINDOWSNT
  if (w32_window_app ())
    {
      char msg[2048];
      vsnprintf (msg, sizeof msg, format, args);
      msg[sizeof msg - 1] = '\0';

      if (is_error)
	MessageBox (NULL, msg, "Emacsclient ERROR", MB_ICONERROR);
      else
	MessageBox (NULL, msg, "Emacsclient", MB_ICONINFORMATION);
    }
  else
#endif
    {
      FILE *f = is_error ? stderr : stdout;

      vfprintf (f, format, args);
      fflush (f);
    }

  va_end (args);
}

/* Decode the options from argv and argc.
   The global variable `optind' will say how many arguments we used up.  */

static void
decode_options (int argc, char **argv)
{
  alternate_editor = egetenv ("ALTERNATE_EDITOR");

  while (1)
    {
      int opt = getopt_long_only (argc, argv,
#ifndef NO_SOCKETS_IN_FILE_SYSTEM
			     "VHneqa:s:f:d:F:tc",
#else
			     "VHneqa:f:d:F:tc",
#endif
			     longopts, 0);

      if (opt == EOF)
	break;

      switch (opt)
	{
	case 0:
	  /* If getopt returns 0, then it has already processed a
	     long-named option.  We should do nothing.  */
	  break;

	case 'a':
	  alternate_editor = optarg;
	  break;

#ifndef NO_SOCKETS_IN_FILE_SYSTEM
	case 's':
	  socket_name = optarg;
	  break;
#endif

	case 'f':
	  server_file = optarg;
	  break;

	  /* We used to disallow this argument in w32, but it seems better
	     to allow it, for the occasional case where the user is
	     connecting with a w32 client to a server compiled with X11
	     support.  */
	case 'd':
	  display = optarg;
	  break;

	case 'n':
	  nowait = 1;
	  break;

	case 'e':
	  eval = 1;
	  break;

	case 'q':
	  quiet = 1;
	  break;

	case 'V':
	  message (FALSE, "emacsclient %s\n", VERSION);
	  exit (EXIT_SUCCESS);
	  break;

        case 't':
          tty = 1;
	  current_frame = 0;
          break;

        case 'c':
          current_frame = 0;
          break;

	case 'p':
	  parent_id = optarg;
          current_frame = 0;
	  break;

	case 'H':
	  print_help_and_exit ();
	  break;

        case 'F':
          frame_parameters = optarg;
          break;

	default:
	  message (TRUE, "Try `%s --help' for more information\n", progname);
	  exit (EXIT_FAILURE);
	  break;
	}
    }

  /* If the -c option is used (without -t) and no --display argument
     is provided, try $DISPLAY.
     Without the -c option, we used to set `display' to $DISPLAY by
     default, but this changed the default behavior and is sometimes
     inconvenient.  So we force users to use "--display $DISPLAY" if
     they want Emacs to connect to their current display.  */
  if (!current_frame && !tty && !display)
    {
      display = egetenv ("DISPLAY");
#ifdef NS_IMPL_COCOA
      /* Under Cocoa, we don't really use displays the same way as in X,
         so provide a dummy. */
      if (!display || strlen (display) == 0)
        display = "ns";
#endif
    }

  /* A null-string display is invalid.  */
  if (display && strlen (display) == 0)
    display = NULL;

  /* If no display is available, new frames are tty frames.  */
  if (!current_frame && !display)
    tty = 1;

#ifdef WINDOWSNT
  /* Emacs on Windows does not support graphical and text terminal
     frames in the same instance.  So, treat the -t and -c options as
     equivalent, and open a new frame on the server's terminal.
     Ideally, we would only set tty = 1 when the serve is running in a
     console, but alas we don't know that.  As a workaround, always
     ask for a tty frame, and let server.el figure it out.  */
  if (!current_frame)
    {
      display = NULL;
      tty = 1;
    }

  if (alternate_editor && alternate_editor[0] == '\0')
    {
      message (TRUE, "--alternate-editor argument or ALTERNATE_EDITOR variable cannot be\n\
an empty string");
      exit (EXIT_FAILURE);
    }
#endif /* WINDOWSNT */
}


static void
print_help_and_exit (void)
{
  /* Spaces and tabs are significant in this message; they're chosen so the
     message aligns properly both in a tty and in a Windows message box.
     Please try to preserve them; otherwise the output is very hard to read
     when using emacsclientw.  */
  message (FALSE,
	   "Usage: %s [OPTIONS] FILE...\n\
Tell the Emacs server to visit the specified files.\n\
Every FILE can be either just a FILENAME or [+LINE[:COLUMN]] FILENAME.\n\
\n\
The following OPTIONS are accepted:\n\
-V, --version		Just print version info and return\n\
-H, --help    		Print this usage information message\n\
-nw, -t, --tty 		Open a new Emacs frame on the current terminal\n\
-c, --create-frame    	Create a new frame instead of trying to\n\
			use the current Emacs frame\n\
-F ALIST, --frame-parameters=ALIST\n\
			Set the parameters of a new frame\n\
-e, --eval    		Evaluate the FILE arguments as ELisp expressions\n\
-n, --no-wait		Don't wait for the server to return\n\
-q, --quiet		Don't display messages on success\n\
-d DISPLAY, --display=DISPLAY\n\
			Visit the file in the given display\n\
--parent-id=ID          Open in parent window ID, via XEmbed\n"
#ifndef NO_SOCKETS_IN_FILE_SYSTEM
"-s SOCKET, --socket-name=SOCKET\n\
			Set filename of the UNIX socket for communication\n"
#endif
"-f SERVER, --server-file=SERVER\n\
			Set filename of the TCP authentication file\n\
-a EDITOR, --alternate-editor=EDITOR\n\
			Editor to fallback to if the server is not running\n"
#ifndef WINDOWSNT
"			If EDITOR is the empty string, start Emacs in daemon\n\
			mode and try connecting again\n"
#endif /* not WINDOWSNT */
"\n\
Report bugs with M-x report-emacs-bug.\n", progname);
  exit (EXIT_SUCCESS);
}

/*
  Try to run a different command, or --if no alternate editor is
  defined-- exit with an errorcode.
  Uses argv, but gets it from the global variable main_argv.
*/
static void
fail (void)
{
  if (alternate_editor)
    {
      int i = optind - 1;

      execvp (alternate_editor, main_argv + i);
      message (TRUE, "%s: error executing alternate editor \"%s\"\n",
	       progname, alternate_editor);
    }
  exit (EXIT_FAILURE);
}


#if !defined (HAVE_SOCKETS) || !defined (HAVE_INET_SOCKETS)

int
main (int argc, char **argv)
{
  main_argv = argv;
  progname = argv[0];
  message (TRUE, "%s: Sorry, the Emacs server is supported only\n"
	   "on systems with Berkeley sockets.\n",
	   argv[0]);
  fail ();
}

#else /* HAVE_SOCKETS && HAVE_INET_SOCKETS */

#define AUTH_KEY_LENGTH      64
#define SEND_BUFFER_SIZE   4096

extern char *strerror (int);

/* Buffer to accumulate data to send in TCP connections.  */
char send_buffer[SEND_BUFFER_SIZE + 1];
int sblen = 0;	/* Fill pointer for the send buffer.  */
/* Socket used to communicate with the Emacs server process.  */
HSOCKET emacs_socket = 0;

/* On Windows, the socket library was historically separate from the standard
   C library, so errors are handled differently.  */
static void
sock_err_message (const char *function_name)
{
#ifdef WINDOWSNT
  char* msg = NULL;

  FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM
                 | FORMAT_MESSAGE_ALLOCATE_BUFFER
                 | FORMAT_MESSAGE_ARGUMENT_ARRAY,
                 NULL, WSAGetLastError (), 0, (LPTSTR)&msg, 0, NULL);

  message (TRUE, "%s: %s: %s\n", progname, function_name, msg);

  LocalFree (msg);
#else
  message (TRUE, "%s: %s: %s\n", progname, function_name, strerror (errno));
#endif
}


/* Let's send the data to Emacs when either
   - the data ends in "\n", or
   - the buffer is full (but this shouldn't happen)
   Otherwise, we just accumulate it.  */
static void
send_to_emacs (HSOCKET s, const char *data)
{
  size_t dlen;

  if (!data)
    return;

  dlen = strlen (data);
  while (*data)
    {
      size_t part = min (dlen, SEND_BUFFER_SIZE - sblen);
      memcpy (&send_buffer[sblen], data, part);
      data += part;
      sblen += part;

      if (sblen == SEND_BUFFER_SIZE
	  || (sblen > 0 && send_buffer[sblen-1] == '\n'))
	{
	  int sent = send (s, send_buffer, sblen, 0);
	  if (sent < 0)
	    {
	      message (TRUE, "%s: failed to send %d bytes to socket: %s\n",
		       progname, sblen, strerror (errno));
	      fail ();
	    }
	  if (sent != sblen)
	    memmove (send_buffer, &send_buffer[sent], sblen - sent);
	  sblen -= sent;
	}

      dlen -= part;
    }
}


/* In STR, insert a & before each &, each space, each newline, and
   any initial -.  Change spaces to underscores, too, so that the
   return value never contains a space.

   Does not change the string.  Outputs the result to S.  */
static void
quote_argument (HSOCKET s, const char *str)
{
  char *copy = (char *) xmalloc (strlen (str) * 2 + 1);
  const char *p;
  char *q;

  p = str;
  q = copy;
  while (*p)
    {
      if (*p == ' ')
	{
	  *q++ = '&';
	  *q++ = '_';
	  p++;
	}
      else if (*p == '\n')
	{
	  *q++ = '&';
	  *q++ = 'n';
	  p++;
	}
      else
	{
	  if (*p == '&' || (*p == '-' && p == str))
	    *q++ = '&';
	  *q++ = *p++;
	}
    }
  *q++ = 0;

  send_to_emacs (s, copy);

  free (copy);
}


/* The inverse of quote_argument.  Removes quoting in string STR by
   modifying the string in place.   Returns STR. */

static char *
unquote_argument (char *str)
{
  char *p, *q;

  if (! str)
    return str;

  p = str;
  q = str;
  while (*p)
    {
      if (*p == '&')
        {
          p++;
          if (*p == '&')
            *p = '&';
          else if (*p == '_')
            *p = ' ';
          else if (*p == 'n')
            *p = '\n';
          else if (*p == '-')
            *p = '-';
        }
      *q++ = *p++;
    }
  *q = 0;
  return str;
}


static int
file_name_absolute_p (const char *filename)
{
  /* Sanity check, it shouldn't happen.  */
  if (! filename) return FALSE;

  /* /xxx is always an absolute path.  */
  if (filename[0] == '/') return TRUE;

  /* Empty filenames (which shouldn't happen) are relative.  */
  if (filename[0] == '\0') return FALSE;

#ifdef WINDOWSNT
  /* X:\xxx is always absolute.  */
  if (isalpha ((unsigned char) filename[0])
      && filename[1] == ':' && (filename[2] == '\\' || filename[2] == '/'))
    return TRUE;

  /* Both \xxx and \\xxx\yyy are absolute.  */
  if (filename[0] == '\\') return TRUE;
#endif

  return FALSE;
}

#ifdef WINDOWSNT
/* Wrapper to make WSACleanup a cdecl, as required by atexit.  */
void __cdecl
close_winsock (void)
{
  WSACleanup ();
}

/* Initialize the WinSock2 library.  */
void
initialize_sockets (void)
{
  WSADATA wsaData;

  if (WSAStartup (MAKEWORD (2, 0), &wsaData))
    {
      message (TRUE, "%s: error initializing WinSock2\n", progname);
      exit (EXIT_FAILURE);
    }

  atexit (close_winsock);
}
#endif /* WINDOWSNT */


/*
 * Read the information needed to set up a TCP comm channel with
 * the Emacs server: host, port, and authentication string.
 */
static int
get_server_config (const char *config_file, struct sockaddr_in *server,
		   char *authentication)
{
  char dotted[32];
  char *port;
  FILE *config = NULL;

  if (file_name_absolute_p (config_file))
    config = fopen (config_file, "rb");
  else
    {
      const char *home = egetenv ("HOME");

      if (home)
        {
	  char *path = xmalloc (strlen (home) + strlen (config_file)
				+ EXTRA_SPACE);
	  strcpy (path, home);
	  strcat (path, "/.emacs.d/server/");
	  strcat (path, config_file);
          config = fopen (path, "rb");
	  free (path);
        }
#ifdef WINDOWSNT
      if (!config && (home = egetenv ("APPDATA")))
        {
	  char *path = xmalloc (strlen (home) + strlen (config_file)
				+ EXTRA_SPACE);
	  strcpy (path, home);
	  strcat (path, "/.emacs.d/server/");
	  strcat (path, config_file);
          config = fopen (path, "rb");
	  free (path);
        }
#endif
    }

  if (! config)
    return FALSE;

  if (fgets (dotted, sizeof dotted, config)
      && (port = strchr (dotted, ':')))
    *port++ = '\0';
  else
    {
      message (TRUE, "%s: invalid configuration info\n", progname);
      exit (EXIT_FAILURE);
    }

  server->sin_family = AF_INET;
  server->sin_addr.s_addr = inet_addr (dotted);
  server->sin_port = htons (atoi (port));

  if (! fread (authentication, AUTH_KEY_LENGTH, 1, config))
    {
      message (TRUE, "%s: cannot read authentication info\n", progname);
      exit (EXIT_FAILURE);
    }

  fclose (config);

  return TRUE;
}

static HSOCKET
set_tcp_socket (const char *local_server_file)
{
  HSOCKET s;
  struct sockaddr_in server;
  struct linger l_arg = {1, 1};
  char auth_string[AUTH_KEY_LENGTH + 1];

  if (! get_server_config (local_server_file, &server, auth_string))
    return INVALID_SOCKET;

  if (server.sin_addr.s_addr != inet_addr ("127.0.0.1") && !quiet)
    message (FALSE, "%s: connected to remote socket at %s\n",
             progname, inet_ntoa (server.sin_addr));

  /*
   * Open up an AF_INET socket
   */
  if ((s = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
    {
      sock_err_message ("socket");
      return INVALID_SOCKET;
    }

  /*
   * Set up the socket
   */
  if (connect (s, (struct sockaddr *) &server, sizeof server) < 0)
    {
      sock_err_message ("connect");
      return INVALID_SOCKET;
    }

  setsockopt (s, SOL_SOCKET, SO_LINGER, (char *) &l_arg, sizeof l_arg);

  /*
   * Send the authentication
   */
  auth_string[AUTH_KEY_LENGTH] = '\0';

  send_to_emacs (s, "-auth ");
  send_to_emacs (s, auth_string);
  send_to_emacs (s, " ");

  return s;
}


/* Returns 1 if PREFIX is a prefix of STRING. */
static int
strprefix (const char *prefix, const char *string)
{
  return !strncmp (prefix, string, strlen (prefix));
}

/* Get tty name and type.  If successful, return the type in TTY_TYPE
   and the name in TTY_NAME, and return 1.  Otherwise, fail if NOABORT
   is zero, or return 0 if NOABORT is non-zero.  */

static int
find_tty (const char **tty_type, const char **tty_name, int noabort)
{
  const char *type = egetenv ("TERM");
  const char *name = ttyname (fileno (stdout));

  if (!name)
    {
      if (noabort)
	return 0;
      else
	{
	  message (TRUE, "%s: could not get terminal name\n", progname);
	  fail ();
	}
    }

  if (!type)
    {
      if (noabort)
	return 0;
      else
	{
	  message (TRUE, "%s: please set the TERM variable to your terminal type\n",
		   progname);
	  fail ();
	}
    }

  if (strcmp (type, "eterm") == 0)
    {
      if (noabort)
	return 0;
      else
	{
	  /* This causes nasty, MULTI_KBOARD-related input lockouts. */
	  message (TRUE, "%s: opening a frame in an Emacs term buffer"
		   " is not supported\n", progname);
	  fail ();
	}
    }

  *tty_name = name;
  *tty_type = type;
  return 1;
}


#if !defined (NO_SOCKETS_IN_FILE_SYSTEM)

/* Three possibilities:
   2 - can't be `stat'ed		(sets errno)
   1 - isn't owned by us
   0 - success: none of the above */

static int
socket_status (const char *name)
{
  struct stat statbfr;

  if (stat (name, &statbfr) == -1)
    return 2;

  if (statbfr.st_uid != geteuid ())
    return 1;

  return 0;
}


/* A signal handler that passes the signal to the Emacs process.
   Useful for SIGWINCH.  */

static void
pass_signal_to_emacs (int signalnum)
{
  int old_errno = errno;

  if (emacs_pid)
    kill (emacs_pid, signalnum);

  signal (signalnum, pass_signal_to_emacs);
  errno = old_errno;
}

/* Signal handler for SIGCONT; notify the Emacs process that it can
   now resume our tty frame.  */

static void
handle_sigcont (int signalnum)
{
  int old_errno = errno;

  if (tcgetpgrp (1) == getpgrp ())
    {
      /* We are in the foreground. */
      send_to_emacs (emacs_socket, "-resume \n");
    }
  else
    {
      /* We are in the background; cancel the continue. */
      kill (getpid (), SIGSTOP);
    }

  signal (signalnum, handle_sigcont);
  errno = old_errno;
}

/* Signal handler for SIGTSTP; notify the Emacs process that we are
   going to sleep.  Normally the suspend is initiated by Emacs via
   server-handle-suspend-tty, but if the server gets out of sync with
   reality, we may get a SIGTSTP on C-z.  Handling this signal and
   notifying Emacs about it should get things under control again. */

static void
handle_sigtstp (int signalnum)
{
  int old_errno = errno;
  sigset_t set;

  if (emacs_socket)
    send_to_emacs (emacs_socket, "-suspend \n");

  /* Unblock this signal and call the default handler by temporarily
     changing the handler and resignaling.  */
  sigprocmask (SIG_BLOCK, NULL, &set);
  sigdelset (&set, signalnum);
  signal (signalnum, SIG_DFL);
  kill (getpid (), signalnum);
  sigprocmask (SIG_SETMASK, &set, NULL); /* Let's the above signal through. */
  signal (signalnum, handle_sigtstp);

  errno = old_errno;
}


/* Set up signal handlers before opening a frame on the current tty.  */

static void
init_signals (void)
{
  /* Set up signal handlers. */
  signal (SIGWINCH, pass_signal_to_emacs);

  /* Don't pass SIGINT and SIGQUIT to Emacs, because it has no way of
     deciding which terminal the signal came from.  C-g is now a
     normal input event on secondary terminals.  */
#if 0
  signal (SIGINT, pass_signal_to_emacs);
  signal (SIGQUIT, pass_signal_to_emacs);
#endif

  signal (SIGCONT, handle_sigcont);
  signal (SIGTSTP, handle_sigtstp);
  signal (SIGTTOU, handle_sigtstp);
}


static HSOCKET
set_local_socket (const char *local_socket_name)
{
  HSOCKET s;
  struct sockaddr_un server;

  /*
   * Open up an AF_UNIX socket in this person's home directory
   */

  if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
    {
      message (TRUE, "%s: socket: %s\n", progname, strerror (errno));
      return INVALID_SOCKET;
    }

  server.sun_family = AF_UNIX;

  {
    int sock_status;
    int use_tmpdir = 0;
    int saved_errno;
    const char *server_name = local_socket_name;
    const char *tmpdir IF_LINT ( = NULL);
    char *tmpdir_storage = NULL;
    char *socket_name_storage = NULL;

    if (!strchr (local_socket_name, '/') && !strchr (local_socket_name, '\\'))
      {
	/* socket_name is a file name component.  */
	long uid = geteuid ();
	ptrdiff_t tmpdirlen;
	use_tmpdir = 1;
	tmpdir = egetenv ("TMPDIR");
	if (!tmpdir)
          {
#ifdef DARWIN_OS
#ifndef _CS_DARWIN_USER_TEMP_DIR
#define _CS_DARWIN_USER_TEMP_DIR 65537
#endif
            size_t n = confstr (_CS_DARWIN_USER_TEMP_DIR, NULL, (size_t) 0);
            if (n > 0)
              {
		tmpdir = tmpdir_storage = xmalloc (n);
                confstr (_CS_DARWIN_USER_TEMP_DIR, tmpdir, n);
              }
            else
#endif
              tmpdir = "/tmp";
          }
	tmpdirlen = strlen (tmpdir);
	socket_name_storage =
	  xmalloc (tmpdirlen + strlen (server_name) + EXTRA_SPACE);
	strcpy (socket_name_storage, tmpdir);
	sprintf (socket_name_storage + tmpdirlen, "/emacs%ld/", uid);
	strcat (socket_name_storage + tmpdirlen, server_name);
	local_socket_name = socket_name_storage;
      }

    if (strlen (local_socket_name) < sizeof (server.sun_path))
      strcpy (server.sun_path, local_socket_name);
    else
      {
        message (TRUE, "%s: socket-name %s too long\n",
                 progname, local_socket_name);
        fail ();
      }

    /* See if the socket exists, and if it's owned by us. */
    sock_status = socket_status (server.sun_path);
    saved_errno = errno;
    if (sock_status && use_tmpdir)
      {
	/* Failing that, see if LOGNAME or USER exist and differ from
	   our euid.  If so, look for a socket based on the UID
	   associated with the name.  This is reminiscent of the logic
	   that init_editfns uses to set the global Vuser_full_name.  */

	const char *user_name = egetenv ("LOGNAME");

	if (!user_name)
	  user_name = egetenv ("USER");

	if (user_name)
	  {
	    struct passwd *pw = getpwnam (user_name);

	    if (pw && (pw->pw_uid != geteuid ()))
	      {
		/* We're running under su, apparently. */
		long uid = pw->pw_uid;
		ptrdiff_t tmpdirlen = strlen (tmpdir);
		char *user_socket_name
		  = xmalloc (tmpdirlen + strlen (server_name) + EXTRA_SPACE);
		strcpy (user_socket_name, tmpdir);
		sprintf (user_socket_name + tmpdirlen, "/emacs%ld/", uid);
		strcat (user_socket_name + tmpdirlen, server_name);

		if (strlen (user_socket_name) < sizeof (server.sun_path))
		  strcpy (server.sun_path, user_socket_name);
		else
		  {
		    message (TRUE, "%s: socket-name %s too long\n",
			     progname, user_socket_name);
		    exit (EXIT_FAILURE);
		  }
		free (user_socket_name);

		sock_status = socket_status (server.sun_path);
                saved_errno = errno;
	      }
	    else
	      errno = saved_errno;
	  }
      }

    free (socket_name_storage);
    free (tmpdir_storage);

    switch (sock_status)
      {
      case 1:
	/* There's a socket, but it isn't owned by us.  This is OK if
	   we are root. */
	if (0 != geteuid ())
	  {
	    message (TRUE, "%s: Invalid socket owner\n", progname);
	    return INVALID_SOCKET;
	  }
	break;

      case 2:
	/* `stat' failed */
	if (saved_errno == ENOENT)
	  message (TRUE,
		   "%s: can't find socket; have you started the server?\n\
To start the server in Emacs, type \"M-x server-start\".\n",
		   progname);
	else
	  message (TRUE, "%s: can't stat %s: %s\n",
		   progname, server.sun_path, strerror (saved_errno));
	return INVALID_SOCKET;
      }
  }

  if (connect (s, (struct sockaddr *) &server, strlen (server.sun_path) + 2)
      < 0)
    {
      message (TRUE, "%s: connect: %s\n", progname, strerror (errno));
      return INVALID_SOCKET;
    }

  return s;
}
#endif /* ! NO_SOCKETS_IN_FILE_SYSTEM */

static HSOCKET
set_socket (int no_exit_if_error)
{
  HSOCKET s;
  const char *local_server_file = server_file;

  INITIALIZE ();

#ifndef NO_SOCKETS_IN_FILE_SYSTEM
  /* Explicit --socket-name argument.  */
  if (socket_name)
    {
      s = set_local_socket (socket_name);
      if ((s != INVALID_SOCKET) || no_exit_if_error)
	return s;
      message (TRUE, "%s: error accessing socket \"%s\"\n",
	       progname, socket_name);
      exit (EXIT_FAILURE);
    }
#endif

  /* Explicit --server-file arg or EMACS_SERVER_FILE variable.  */
  if (!local_server_file)
    local_server_file = egetenv ("EMACS_SERVER_FILE");

  if (local_server_file)
    {
      s = set_tcp_socket (local_server_file);
      if ((s != INVALID_SOCKET) || no_exit_if_error)
	return s;

      message (TRUE, "%s: error accessing server file \"%s\"\n",
	       progname, local_server_file);
      exit (EXIT_FAILURE);
    }

#ifndef NO_SOCKETS_IN_FILE_SYSTEM
  /* Implicit local socket.  */
  s = set_local_socket ("server");
  if (s != INVALID_SOCKET)
    return s;
#endif

  /* Implicit server file.  */
  s = set_tcp_socket ("server");
  if ((s != INVALID_SOCKET) || no_exit_if_error)
    return s;

  /* No implicit or explicit socket, and no alternate editor.  */
  message (TRUE, "%s: No socket or alternate editor.  Please use:\n\n"
#ifndef NO_SOCKETS_IN_FILE_SYSTEM
"\t--socket-name\n"
#endif
"\t--server-file      (or environment variable EMACS_SERVER_FILE)\n\
\t--alternate-editor (or environment variable ALTERNATE_EDITOR)\n",
           progname);
  exit (EXIT_FAILURE);
}

#ifdef WINDOWSNT
FARPROC set_fg;  /* Pointer to AllowSetForegroundWindow.  */
FARPROC get_wc;  /* Pointer to RealGetWindowClassA.  */

BOOL CALLBACK
w32_find_emacs_process (HWND hWnd, LPARAM lParam)
{
  DWORD pid;
  char class[6];

  /* Reject any window not of class "Emacs".  */
  if (! get_wc (hWnd, class, sizeof (class))
      || strcmp (class, "Emacs"))
    return TRUE;

  /* We only need the process id, not the thread id.  */
  (void) GetWindowThreadProcessId (hWnd, &pid);

  /* Not the one we're looking for.  */
  if (pid != (DWORD) emacs_pid) return TRUE;

  /* OK, let's raise it.  */
  set_fg (emacs_pid);

  /* Stop enumeration.  */
  return FALSE;
}

/*
 * Search for a window of class "Emacs" and owned by a process with
 * process id = emacs_pid.  If found, allow it to grab the focus.
 */
void
w32_give_focus (void)
{
  HANDLE user32;

  /* It shouldn't happen when dealing with TCP sockets.  */
  if (!emacs_pid) return;

  user32 = GetModuleHandle ("user32.dll");

  if (!user32)
    return;

  /* Modern Windows restrict which processes can set the foreground window.
     emacsclient can allow Emacs to grab the focus by calling the function
     AllowSetForegroundWindow.  Unfortunately, older Windows (W95, W98 and
     NT) lack this function, so we have to check its availability.  */
  if ((set_fg = GetProcAddress (user32, "AllowSetForegroundWindow"))
      && (get_wc = GetProcAddress (user32, "RealGetWindowClassA")))
    EnumWindows (w32_find_emacs_process, (LPARAM) 0);
}
#endif

/* Start the emacs daemon and try to connect to it.  */

static void
start_daemon_and_retry_set_socket (void)
{
#ifndef WINDOWSNT
  pid_t dpid;
  int status;

  dpid = fork ();

  if (dpid > 0)
    {
      pid_t w;
      w = waitpid (dpid, &status, WUNTRACED | WCONTINUED);

      if ((w == -1) || !WIFEXITED (status) || WEXITSTATUS (status))
	{
	  message (TRUE, "Error: Could not start the Emacs daemon\n");
	  exit (EXIT_FAILURE);
	}

      /* Try connecting, the daemon should have started by now.  */
      message (TRUE, "Emacs daemon should have started, trying to connect again\n");
      if ((emacs_socket = set_socket (1)) == INVALID_SOCKET)
	{
	  message (TRUE, "Error: Cannot connect even after starting the Emacs daemon\n");
	  exit (EXIT_FAILURE);
	}
    }
  else if (dpid < 0)
    {
      fprintf (stderr, "Error: Cannot fork!\n");
      exit (EXIT_FAILURE);
    }
  else
    {
      char emacs[] = "emacs";
      char daemon_option[] = "--daemon";
      char *d_argv[] = {emacs, daemon_option, 0 };
      if (socket_name != NULL)
	{
	  /* Pass  --daemon=socket_name as argument.  */
	  const char *deq = "--daemon=";
	  char *daemon_arg = xmalloc (strlen (deq)
				      + strlen (socket_name) + 1);
	  strcpy (daemon_arg, deq);
	  strcat (daemon_arg, socket_name);
	  d_argv[1] = daemon_arg;
	}
      execvp ("emacs", d_argv);
      message (TRUE, "%s: error starting emacs daemon\n", progname);
    }
#endif /* WINDOWSNT */
}

int
main (int argc, char **argv)
{
  int rl = 0, needlf = 0;
  char *cwd, *str;
  char string[BUFSIZ+1];
  int start_daemon_if_needed;
  int exit_status = EXIT_SUCCESS;

  main_argv = argv;
  progname = argv[0];

#ifdef WINDOWSNT
  /* On Windows 7 and later, we need to explicitly associate emacsclient
     with emacs so the UI behaves sensibly.  */
  w32_set_user_model_id ();
#endif

  /* Process options.  */
  decode_options (argc, argv);

  if ((argc - optind < 1) && !eval && current_frame)
    {
      message (TRUE, "%s: file name or argument required\n"
	       "Try `%s --help' for more information\n",
	       progname, progname);
      exit (EXIT_FAILURE);
    }

  /* If alternate_editor is the empty string, start the emacs daemon
     in case of failure to connect.  */
  start_daemon_if_needed = (alternate_editor
			    && (alternate_editor[0] == '\0'));

  emacs_socket = set_socket (alternate_editor || start_daemon_if_needed);
  if (emacs_socket == INVALID_SOCKET)
    {
      if (! start_daemon_if_needed)
	fail ();

      start_daemon_and_retry_set_socket ();
    }

  cwd = get_current_dir_name ();
  if (cwd == 0)
    {
      /* getwd puts message in STRING if it fails.  */
      message (TRUE, "%s: %s\n", progname,
	       "Cannot get current working directory");
      fail ();
    }

#ifdef WINDOWSNT
  w32_give_focus ();
#endif

  /* Send over our environment and current directory. */
  if (!current_frame)
    {
#ifndef WINDOWSNT
      /* This is defined in stdlib.h on MS-Windows.  It's defined in
	 unistd.h on some POSIX hosts, but not all (Bug#10155).  */
      extern char **environ;
#endif
      int i;
      for (i = 0; environ[i]; i++)
        {
          send_to_emacs (emacs_socket, "-env ");
          quote_argument (emacs_socket, environ[i]);
          send_to_emacs (emacs_socket, " ");
        }
    }
  send_to_emacs (emacs_socket, "-dir ");
  quote_argument (emacs_socket, cwd);
  send_to_emacs (emacs_socket, "/");
  send_to_emacs (emacs_socket, " ");

 retry:
  if (nowait)
    send_to_emacs (emacs_socket, "-nowait ");

  if (current_frame)
    send_to_emacs (emacs_socket, "-current-frame ");

  if (display)
    {
      send_to_emacs (emacs_socket, "-display ");
      quote_argument (emacs_socket, display);
      send_to_emacs (emacs_socket, " ");
    }

  if (parent_id)
    {
      send_to_emacs (emacs_socket, "-parent-id ");
      quote_argument (emacs_socket, parent_id);
      send_to_emacs (emacs_socket, " ");
    }

  if (frame_parameters && !current_frame)
    {
      send_to_emacs (emacs_socket, "-frame-parameters ");
      quote_argument (emacs_socket, frame_parameters);
      send_to_emacs (emacs_socket, " ");
    }

  /* If using the current frame, send tty information to Emacs anyway.
     In daemon mode, Emacs may need to occupy this tty if no other
     frame is available.  */
  if (tty || (current_frame && !eval))
    {
      const char *tty_type, *tty_name;

      if (find_tty (&tty_type, &tty_name, !tty))
	{
#if !defined (NO_SOCKETS_IN_FILE_SYSTEM)
	  init_signals ();
#endif
	  send_to_emacs (emacs_socket, "-tty ");
	  quote_argument (emacs_socket, tty_name);
	  send_to_emacs (emacs_socket, " ");
	  quote_argument (emacs_socket, tty_type);
	  send_to_emacs (emacs_socket, " ");
	}
    }

  if (!current_frame && !tty)
    send_to_emacs (emacs_socket, "-window-system ");

  if ((argc - optind > 0))
    {
      int i;
      for (i = optind; i < argc; i++)
	{

	  if (eval)
            {
              /* Don't prepend cwd or anything like that.  */
              send_to_emacs (emacs_socket, "-eval ");
              quote_argument (emacs_socket, argv[i]);
              send_to_emacs (emacs_socket, " ");
              continue;
            }

          if (*argv[i] == '+')
            {
	      char *p = argv[i] + 1;
	      while (isdigit ((unsigned char) *p) || *p == ':') p++;
	      if (*p == 0)
                {
                  send_to_emacs (emacs_socket, "-position ");
                  quote_argument (emacs_socket, argv[i]);
                  send_to_emacs (emacs_socket, " ");
                  continue;
                }
            }
#ifdef WINDOWSNT
	  else if (! file_name_absolute_p (argv[i])
		   && (isalpha (argv[i][0]) && argv[i][1] == ':'))
	    /* Windows can have a different default directory for each
	       drive, so the cwd passed via "-dir" is not sufficient
	       to account for that.
	       If the user uses <drive>:<relpath>, we hence need to be
	       careful to expand <relpath> with the default directory
	       corresponding to <drive>.  */
	    {
	      char *filename = (char *) xmalloc (MAX_PATH);
	      DWORD size;

	      size = GetFullPathName (argv[i], MAX_PATH, filename, NULL);
	      if (size > 0 && size < MAX_PATH)
		argv[i] = filename;
	      else
		free (filename);
	    }
#endif

          send_to_emacs (emacs_socket, "-file ");
          quote_argument (emacs_socket, argv[i]);
          send_to_emacs (emacs_socket, " ");
        }
    }
  else if (eval)
    {
      /* Read expressions interactively.  */
      while ((str = fgets (string, BUFSIZ, stdin)))
	{
	  send_to_emacs (emacs_socket, "-eval ");
	  quote_argument (emacs_socket, str);
	}
      send_to_emacs (emacs_socket, " ");
    }

  send_to_emacs (emacs_socket, "\n");

  /* Wait for an answer. */
  if (!eval && !tty && !nowait && !quiet)
    {
      printf ("Waiting for Emacs...");
      needlf = 2;
    }
  fflush (stdout);
  fsync (1);

  /* Now, wait for an answer and print any messages.  */
  while (exit_status == EXIT_SUCCESS)
    {
      char *p, *end_p;
      do
        {
          errno = 0;
          rl = recv (emacs_socket, string, BUFSIZ, 0);
        }
      /* If we receive a signal (e.g. SIGWINCH, which we pass
	 through to Emacs), on some OSes we get EINTR and must retry. */
      while (rl < 0 && errno == EINTR);

      if (rl <= 0)
        break;

      string[rl] = '\0';

      /* Loop over all NL-terminated messages.  */
      for (end_p = p = string; end_p != NULL && *end_p != '\0'; p = end_p)
	{
	  end_p = strchr (p, '\n');
	  if (end_p != NULL)
	    *end_p++ = '\0';

	  if (strprefix ("-emacs-pid ", p))
	    {
	      /* -emacs-pid PID: The process id of the Emacs process. */
	      emacs_pid = strtol (p + strlen ("-emacs-pid"), NULL, 10);
	    }
	  else if (strprefix ("-window-system-unsupported ", p))
	    {
	      /* -window-system-unsupported: Emacs was compiled without X
		 support.  Try again on the terminal. */
	      nowait = 0;
	      tty = 1;
	      goto retry;
	    }
	  else if (strprefix ("-print ", p))
	    {
	      /* -print STRING: Print STRING on the terminal. */
	      str = unquote_argument (p + strlen ("-print "));
	      if (needlf)
		printf ("\n");
	      printf ("%s", str);
	      needlf = str[0] == '\0' ? needlf : str[strlen (str) - 1] != '\n';
	    }
	  else if (strprefix ("-print-nonl ", p))
	    {
	      /* -print-nonl STRING: Print STRING on the terminal.
	         Used to continue a preceding -print command.  */
	      str = unquote_argument (p + strlen ("-print-nonl "));
	      printf ("%s", str);
	      needlf = str[0] == '\0' ? needlf : str[strlen (str) - 1] != '\n';
	    }
	  else if (strprefix ("-error ", p))
	    {
	      /* -error DESCRIPTION: Signal an error on the terminal. */
	      str = unquote_argument (p + strlen ("-error "));
	      if (needlf)
		printf ("\n");
	      fprintf (stderr, "*ERROR*: %s", str);
	      needlf = str[0] == '\0' ? needlf : str[strlen (str) - 1] != '\n';
	      exit_status = EXIT_FAILURE;
	    }
#ifdef SIGSTOP
	  else if (strprefix ("-suspend ", p))
	    {
	      /* -suspend: Suspend this terminal, i.e., stop the process. */
	      if (needlf)
		printf ("\n");
	      needlf = 0;
	      kill (0, SIGSTOP);
	    }
#endif
	  else
	    {
	      /* Unknown command. */
	      if (needlf)
		printf ("\n");
	      needlf = 0;
	      printf ("*ERROR*: Unknown message: %s\n", p);
	    }
	}
    }

  if (needlf)
    printf ("\n");
  fflush (stdout);
  fsync (1);

  if (rl < 0)
    exit_status = EXIT_FAILURE;

  CLOSE_SOCKET (emacs_socket);
  return exit_status;
}

#endif /* HAVE_SOCKETS && HAVE_INET_SOCKETS */


#ifndef HAVE_STRERROR
char *
strerror (errnum)
     int errnum;
{
  extern char *sys_errlist[];
  extern int sys_nerr;

  if (errnum >= 0 && errnum < sys_nerr)
    return sys_errlist[errnum];
  return (char *) "Unknown error";
}

#endif /* ! HAVE_STRERROR */


/* emacsclient.c ends here */
