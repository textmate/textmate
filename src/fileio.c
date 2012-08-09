/* File IO for GNU Emacs.

Copyright (C) 1985-1988, 1993-2012 Free Software Foundation, Inc.

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
#include <limits.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <setjmp.h>
#include <unistd.h>

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include <ctype.h>
#include <errno.h>

#ifdef HAVE_LIBSELINUX
#include <selinux/selinux.h>
#include <selinux/context.h>
#endif

#include "lisp.h"
#include "intervals.h"
#include "buffer.h"
#include "character.h"
#include "coding.h"
#include "window.h"
#include "blockinput.h"
#include "frame.h"
#include "dispextern.h"

#ifdef WINDOWSNT
#define NOMINMAX 1
#include <windows.h>
#include <fcntl.h>
#endif /* not WINDOWSNT */

#ifdef MSDOS
#include "msdos.h"
#include <sys/param.h>
#include <fcntl.h>
#endif

#ifdef DOS_NT
/* On Windows, drive letters must be alphabetic - on DOS, the Netware
   redirector allows the six letters between 'Z' and 'a' as well.  */
#ifdef MSDOS
#define IS_DRIVE(x) ((x) >= 'A' && (x) <= 'z')
#endif
#ifdef WINDOWSNT
#define IS_DRIVE(x) isalpha ((unsigned char) (x))
#endif
/* Need to lower-case the drive letter, or else expanded
   filenames will sometimes compare unequal, because
   `expand-file-name' doesn't always down-case the drive letter.  */
#define DRIVE_LETTER(x) (tolower ((unsigned char) (x)))
#endif

#include "systime.h"

#ifdef HPUX
#include <netio.h>
#endif

#include "commands.h"

#ifndef FILE_SYSTEM_CASE
#define FILE_SYSTEM_CASE(filename)  (filename)
#endif

/* Nonzero during writing of auto-save files */
static int auto_saving;

/* Nonzero umask during creation of auto-save directories */
static int auto_saving_dir_umask;

/* Set by auto_save_1 to mode of original file so Fwrite_region will create
   a new file with the same mode as the original */
static int auto_save_mode_bits;

/* Set by auto_save_1 if an error occurred during the last auto-save. */
static int auto_save_error_occurred;

/* The symbol bound to coding-system-for-read when
   insert-file-contents is called for recovering a file.  This is not
   an actual coding system name, but just an indicator to tell
   insert-file-contents to use `emacs-mule' with a special flag for
   auto saving and recovering a file.  */
static Lisp_Object Qauto_save_coding;

/* Property name of a file name handler,
   which gives a list of operations it handles..  */
static Lisp_Object Qoperations;

/* Lisp functions for translating file formats */
static Lisp_Object Qformat_decode, Qformat_annotate_function;

/* Lisp function for setting buffer-file-coding-system and the
   multibyteness of the current buffer after inserting a file.  */
static Lisp_Object Qafter_insert_file_set_coding;

static Lisp_Object Qwrite_region_annotate_functions;
/* Each time an annotation function changes the buffer, the new buffer
   is added here.  */
static Lisp_Object Vwrite_region_annotation_buffers;

#ifdef HAVE_FSYNC
#endif

static Lisp_Object Qdelete_by_moving_to_trash;

/* Lisp function for moving files to trash.  */
static Lisp_Object Qmove_file_to_trash;

/* Lisp function for recursively copying directories.  */
static Lisp_Object Qcopy_directory;

/* Lisp function for recursively deleting directories.  */
static Lisp_Object Qdelete_directory;

#ifdef WINDOWSNT
#endif

Lisp_Object Qfile_error;
static Lisp_Object Qfile_already_exists, Qfile_date_error;
static Lisp_Object Qexcl;
Lisp_Object Qfile_name_history;

static Lisp_Object Qcar_less_than_car;

static Lisp_Object Fmake_symbolic_link (Lisp_Object, Lisp_Object, Lisp_Object);
static int a_write (int, Lisp_Object, EMACS_INT, EMACS_INT,
                    Lisp_Object *, struct coding_system *);
static int e_write (int, Lisp_Object, EMACS_INT, EMACS_INT,
		    struct coding_system *);


void
report_file_error (const char *string, Lisp_Object data)
{
  Lisp_Object errstring;
  int errorno = errno;
  char *str;

  synchronize_system_messages_locale ();
  str = strerror (errorno);
  errstring = code_convert_string_norecord (make_unibyte_string (str,
								 strlen (str)),
					    Vlocale_coding_system, 0);

  while (1)
    switch (errorno)
      {
      case EEXIST:
	xsignal (Qfile_already_exists, Fcons (errstring, data));
	break;
      default:
	/* System error messages are capitalized.  Downcase the initial
	   unless it is followed by a slash.  (The slash case caters to
	   error messages that begin with "I/O" or, in German, "E/A".)  */
	if (STRING_MULTIBYTE (errstring)
	    && ! EQ (Faref (errstring, make_number (1)), make_number ('/')))
	  {
	    int c;

	    str = SSDATA (errstring);
	    c = STRING_CHAR ((unsigned char *) str);
	    Faset (errstring, make_number (0), make_number (downcase (c)));
	  }

	xsignal (Qfile_error,
		 Fcons (build_string (string), Fcons (errstring, data)));
      }
}

Lisp_Object
close_file_unwind (Lisp_Object fd)
{
  emacs_close (XFASTINT (fd));
  return Qnil;
}

/* Restore point, having saved it as a marker.  */

Lisp_Object
restore_point_unwind (Lisp_Object location)
{
  Fgoto_char (location);
  Fset_marker (location, Qnil, Qnil);
  return Qnil;
}


static Lisp_Object Qexpand_file_name;
static Lisp_Object Qsubstitute_in_file_name;
static Lisp_Object Qdirectory_file_name;
static Lisp_Object Qfile_name_directory;
static Lisp_Object Qfile_name_nondirectory;
static Lisp_Object Qunhandled_file_name_directory;
static Lisp_Object Qfile_name_as_directory;
static Lisp_Object Qcopy_file;
static Lisp_Object Qmake_directory_internal;
static Lisp_Object Qmake_directory;
static Lisp_Object Qdelete_directory_internal;
Lisp_Object Qdelete_file;
static Lisp_Object Qrename_file;
static Lisp_Object Qadd_name_to_file;
static Lisp_Object Qmake_symbolic_link;
Lisp_Object Qfile_exists_p;
static Lisp_Object Qfile_executable_p;
static Lisp_Object Qfile_readable_p;
static Lisp_Object Qfile_writable_p;
static Lisp_Object Qfile_symlink_p;
static Lisp_Object Qaccess_file;
Lisp_Object Qfile_directory_p;
static Lisp_Object Qfile_regular_p;
static Lisp_Object Qfile_accessible_directory_p;
static Lisp_Object Qfile_modes;
static Lisp_Object Qset_file_modes;
static Lisp_Object Qset_file_times;
static Lisp_Object Qfile_selinux_context;
static Lisp_Object Qset_file_selinux_context;
static Lisp_Object Qfile_newer_than_file_p;
Lisp_Object Qinsert_file_contents;
Lisp_Object Qwrite_region;
static Lisp_Object Qverify_visited_file_modtime;
static Lisp_Object Qset_visited_file_modtime;

DEFUN ("find-file-name-handler", Ffind_file_name_handler,
       Sfind_file_name_handler, 2, 2, 0,
       doc: /* Return FILENAME's handler function for OPERATION, if it has one.
Otherwise, return nil.
A file name is handled if one of the regular expressions in
`file-name-handler-alist' matches it.

If OPERATION equals `inhibit-file-name-operation', then we ignore
any handlers that are members of `inhibit-file-name-handlers',
but we still do run any other handlers.  This lets handlers
use the standard functions without calling themselves recursively.  */)
  (Lisp_Object filename, Lisp_Object operation)
{
  /* This function must not munge the match data.  */
  Lisp_Object chain, inhibited_handlers, result;
  int pos = -1;

  result = Qnil;
  CHECK_STRING (filename);

  if (EQ (operation, Vinhibit_file_name_operation))
    inhibited_handlers = Vinhibit_file_name_handlers;
  else
    inhibited_handlers = Qnil;

  for (chain = Vfile_name_handler_alist; CONSP (chain);
       chain = XCDR (chain))
    {
      Lisp_Object elt;
      elt = XCAR (chain);
      if (CONSP (elt))
	{
	  Lisp_Object string = XCAR (elt);
	  EMACS_INT match_pos;
	  Lisp_Object handler = XCDR (elt);
	  Lisp_Object operations = Qnil;

	  if (SYMBOLP (handler))
	    operations = Fget (handler, Qoperations);

	  if (STRINGP (string)
	      && (match_pos = fast_string_match (string, filename)) > pos
	      && (NILP (operations) || ! NILP (Fmemq (operation, operations))))
	    {
	      Lisp_Object tem;

	      handler = XCDR (elt);
	      tem = Fmemq (handler, inhibited_handlers);
	      if (NILP (tem))
		{
		  result = handler;
		  pos = match_pos;
		}
	    }
	}

      QUIT;
    }
  return result;
}

DEFUN ("file-name-directory", Ffile_name_directory, Sfile_name_directory,
       1, 1, 0,
       doc: /* Return the directory component in file name FILENAME.
Return nil if FILENAME does not include a directory.
Otherwise return a directory name.
Given a Unix syntax file name, returns a string ending in slash.  */)
  (Lisp_Object filename)
{
#ifndef DOS_NT
  register const char *beg;
#else
  register char *beg;
#endif
  register const char *p;
  Lisp_Object handler;

  CHECK_STRING (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_name_directory);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qfile_name_directory,
					filename);
      return STRINGP (handled_name) ? handled_name : Qnil;
    }

  filename = FILE_SYSTEM_CASE (filename);
#ifdef DOS_NT
  beg = (char *) alloca (SBYTES (filename) + 1);
  memcpy (beg, SSDATA (filename), SBYTES (filename) + 1);
#else
  beg = SSDATA (filename);
#endif
  p = beg + SBYTES (filename);

  while (p != beg && !IS_DIRECTORY_SEP (p[-1])
#ifdef DOS_NT
	 /* only recognize drive specifier at the beginning */
	 && !(p[-1] == ':'
	      /* handle the "/:d:foo" and "/:foo" cases correctly  */
	      && ((p == beg + 2 && !IS_DIRECTORY_SEP (*beg))
		  || (p == beg + 4 && IS_DIRECTORY_SEP (*beg))))
#endif
	 ) p--;

  if (p == beg)
    return Qnil;
#ifdef DOS_NT
  /* Expansion of "c:" to drive and default directory.  */
  if (p[-1] == ':')
    {
      /* MAXPATHLEN+1 is guaranteed to be enough space for getdefdir.  */
      char *res = alloca (MAXPATHLEN + 1);
      char *r = res;

      if (p == beg + 4 && IS_DIRECTORY_SEP (*beg) && beg[1] == ':')
	{
	  strncpy (res, beg, 2);
	  beg += 2;
	  r += 2;
	}

      if (getdefdir (toupper ((unsigned char) *beg) - 'A' + 1, r))
	{
	  if (!IS_DIRECTORY_SEP (res[strlen (res) - 1]))
	    strcat (res, "/");
	  beg = res;
	  p = beg + strlen (beg);
	}
    }
  dostounix_filename (beg);
#endif /* DOS_NT */

  return make_specified_string (beg, -1, p - beg, STRING_MULTIBYTE (filename));
}

DEFUN ("file-name-nondirectory", Ffile_name_nondirectory,
       Sfile_name_nondirectory, 1, 1, 0,
       doc: /* Return file name FILENAME sans its directory.
For example, in a Unix-syntax file name,
this is everything after the last slash,
or the entire name if it contains no slash.  */)
  (Lisp_Object filename)
{
  register const char *beg, *p, *end;
  Lisp_Object handler;

  CHECK_STRING (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_name_nondirectory);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qfile_name_nondirectory,
					filename);
      if (STRINGP (handled_name))
	return handled_name;
      error ("Invalid handler in `file-name-handler-alist'");
    }

  beg = SSDATA (filename);
  end = p = beg + SBYTES (filename);

  while (p != beg && !IS_DIRECTORY_SEP (p[-1])
#ifdef DOS_NT
	 /* only recognize drive specifier at beginning */
	 && !(p[-1] == ':'
	      /* handle the "/:d:foo" case correctly  */
	      && (p == beg + 2 || (p == beg + 4 && IS_DIRECTORY_SEP (*beg))))
#endif
	 )
    p--;

  return make_specified_string (p, -1, end - p, STRING_MULTIBYTE (filename));
}

DEFUN ("unhandled-file-name-directory", Funhandled_file_name_directory,
       Sunhandled_file_name_directory, 1, 1, 0,
       doc: /* Return a directly usable directory name somehow associated with FILENAME.
A `directly usable' directory name is one that may be used without the
intervention of any file handler.
If FILENAME is a directly usable file itself, return
\(file-name-directory FILENAME).
If FILENAME refers to a file which is not accessible from a local process,
then this should return nil.
The `call-process' and `start-process' functions use this function to
get a current directory to run processes in.  */)
  (Lisp_Object filename)
{
  Lisp_Object handler;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qunhandled_file_name_directory);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qunhandled_file_name_directory,
					filename);
      return STRINGP (handled_name) ? handled_name : Qnil;
    }

  return Ffile_name_directory (filename);
}


static char *
file_name_as_directory (char *out, const char *in)
{
  ptrdiff_t len = strlen (in);

  if (len == 0)
    {
      out[0] = '.';
      out[1] = '/';
      out[2] = 0;
      return out;
    }

  strcpy (out, in);

  /* For Unix syntax, Append a slash if necessary */
  if (!IS_DIRECTORY_SEP (out[len - 1]))
    {
      out[len] = DIRECTORY_SEP;
      out[len + 1] = '\0';
    }
#ifdef DOS_NT
  dostounix_filename (out);
#endif
  return out;
}

DEFUN ("file-name-as-directory", Ffile_name_as_directory,
       Sfile_name_as_directory, 1, 1, 0,
       doc: /* Return a string representing the file name FILE interpreted as a directory.
This operation exists because a directory is also a file, but its name as
a directory is different from its name as a file.
The result can be used as the value of `default-directory'
or passed as second argument to `expand-file-name'.
For a Unix-syntax file name, just appends a slash.  */)
  (Lisp_Object file)
{
  char *buf;
  Lisp_Object handler;

  CHECK_STRING (file);
  if (NILP (file))
    return Qnil;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qfile_name_as_directory);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qfile_name_as_directory,
					file);
      if (STRINGP (handled_name))
	return handled_name;
      error ("Invalid handler in `file-name-handler-alist'");
    }

  buf = (char *) alloca (SBYTES (file) + 10);
  file_name_as_directory (buf, SSDATA (file));
  return make_specified_string (buf, -1, strlen (buf),
				STRING_MULTIBYTE (file));
}

/*
 * Convert from directory name to filename.
 * On UNIX, it's simple: just make sure there isn't a terminating /

 * Value is nonzero if the string output is different from the input.
 */

static int
directory_file_name (char *src, char *dst)
{
  ptrdiff_t slen;

  slen = strlen (src);

  /* Process as Unix format: just remove any final slash.
     But leave "/" unchanged; do not change it to "".  */
  strcpy (dst, src);
  if (slen > 1
      && IS_DIRECTORY_SEP (dst[slen - 1])
#ifdef DOS_NT
      && !IS_ANY_SEP (dst[slen - 2])
#endif
      )
    dst[slen - 1] = 0;
#ifdef DOS_NT
  dostounix_filename (dst);
#endif
  return 1;
}

DEFUN ("directory-file-name", Fdirectory_file_name, Sdirectory_file_name,
       1, 1, 0,
       doc: /* Returns the file name of the directory named DIRECTORY.
This is the name of the file that holds the data for the directory DIRECTORY.
This operation exists because a directory is also a file, but its name as
a directory is different from its name as a file.
In Unix-syntax, this function just removes the final slash.  */)
  (Lisp_Object directory)
{
  char *buf;
  Lisp_Object handler;

  CHECK_STRING (directory);

  if (NILP (directory))
    return Qnil;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (directory, Qdirectory_file_name);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qdirectory_file_name,
					directory);
      if (STRINGP (handled_name))
	return handled_name;
      error ("Invalid handler in `file-name-handler-alist'");
    }

  buf = (char *) alloca (SBYTES (directory) + 20);
  directory_file_name (SSDATA (directory), buf);
  return make_specified_string (buf, -1, strlen (buf),
				STRING_MULTIBYTE (directory));
}

static const char make_temp_name_tbl[64] =
{
  'A','B','C','D','E','F','G','H',
  'I','J','K','L','M','N','O','P',
  'Q','R','S','T','U','V','W','X',
  'Y','Z','a','b','c','d','e','f',
  'g','h','i','j','k','l','m','n',
  'o','p','q','r','s','t','u','v',
  'w','x','y','z','0','1','2','3',
  '4','5','6','7','8','9','-','_'
};

static unsigned make_temp_name_count, make_temp_name_count_initialized_p;

/* Value is a temporary file name starting with PREFIX, a string.

   The Emacs process number forms part of the result, so there is
   no danger of generating a name being used by another process.
   In addition, this function makes an attempt to choose a name
   which has no existing file.  To make this work, PREFIX should be
   an absolute file name.

   BASE64_P non-zero means add the pid as 3 characters in base64
   encoding.  In this case, 6 characters will be added to PREFIX to
   form the file name.  Otherwise, if Emacs is running on a system
   with long file names, add the pid as a decimal number.

   This function signals an error if no unique file name could be
   generated.  */

Lisp_Object
make_temp_name (Lisp_Object prefix, int base64_p)
{
  Lisp_Object val;
  int len, clen;
  printmax_t pid;
  char *p, *data;
  char pidbuf[INT_BUFSIZE_BOUND (printmax_t)];
  int pidlen;

  CHECK_STRING (prefix);

  /* VAL is created by adding 6 characters to PREFIX.  The first
     three are the PID of this process, in base 64, and the second
     three are incremented if the file already exists.  This ensures
     262144 unique file names per PID per PREFIX.  */

  pid = getpid ();

  if (base64_p)
    {
      pidbuf[0] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidbuf[1] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidbuf[2] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidlen = 3;
    }
  else
    {
#ifdef HAVE_LONG_FILE_NAMES
      pidlen = sprintf (pidbuf, "%"pMd, pid);
#else
      pidbuf[0] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidbuf[1] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidbuf[2] = make_temp_name_tbl[pid & 63], pid >>= 6;
      pidlen = 3;
#endif
    }

  len = SBYTES (prefix); clen = SCHARS (prefix);
  val = make_uninit_multibyte_string (clen + 3 + pidlen, len + 3 + pidlen);
  if (!STRING_MULTIBYTE (prefix))
    STRING_SET_UNIBYTE (val);
  data = SSDATA (val);
  memcpy (data, SSDATA (prefix), len);
  p = data + len;

  memcpy (p, pidbuf, pidlen);
  p += pidlen;

  /* Here we try to minimize useless stat'ing when this function is
     invoked many times successively with the same PREFIX.  We achieve
     this by initializing count to a random value, and incrementing it
     afterwards.

     We don't want make-temp-name to be called while dumping,
     because then make_temp_name_count_initialized_p would get set
     and then make_temp_name_count would not be set when Emacs starts.  */

  if (!make_temp_name_count_initialized_p)
    {
      make_temp_name_count = time (NULL);
      make_temp_name_count_initialized_p = 1;
    }

  while (1)
    {
      struct stat ignored;
      unsigned num = make_temp_name_count;

      p[0] = make_temp_name_tbl[num & 63], num >>= 6;
      p[1] = make_temp_name_tbl[num & 63], num >>= 6;
      p[2] = make_temp_name_tbl[num & 63], num >>= 6;

      /* Poor man's congruential RN generator.  Replace with
         ++make_temp_name_count for debugging.  */
      make_temp_name_count += 25229;
      make_temp_name_count %= 225307;

      if (stat (data, &ignored) < 0)
	{
	  /* We want to return only if errno is ENOENT.  */
	  if (errno == ENOENT)
	    return val;
	  else
	    /* The error here is dubious, but there is little else we
	       can do.  The alternatives are to return nil, which is
	       as bad as (and in many cases worse than) throwing the
	       error, or to ignore the error, which will likely result
	       in looping through 225307 stat's, which is not only
	       dog-slow, but also useless since eventually nil would
	       have to be returned anyway.  */
	    report_file_error ("Cannot create temporary name for prefix",
			       Fcons (prefix, Qnil));
	  /* not reached */
	}
    }
}


DEFUN ("make-temp-name", Fmake_temp_name, Smake_temp_name, 1, 1, 0,
       doc: /* Generate temporary file name (string) starting with PREFIX (a string).
The Emacs process number forms part of the result,
so there is no danger of generating a name being used by another process.

In addition, this function makes an attempt to choose a name
which has no existing file.  To make this work,
PREFIX should be an absolute file name.

There is a race condition between calling `make-temp-name' and creating the
file which opens all kinds of security holes.  For that reason, you should
probably use `make-temp-file' instead, except in three circumstances:

* If you are creating the file in the user's home directory.
* If you are creating a directory rather than an ordinary file.
* If you are taking special precautions as `make-temp-file' does.  */)
  (Lisp_Object prefix)
{
  return make_temp_name (prefix, 0);
}



DEFUN ("expand-file-name", Fexpand_file_name, Sexpand_file_name, 1, 2, 0,
       doc: /* Convert filename NAME to absolute, and canonicalize it.
Second arg DEFAULT-DIRECTORY is directory to start with if NAME is relative
\(does not start with slash or tilde); if DEFAULT-DIRECTORY is nil or missing,
the current buffer's value of `default-directory' is used.
NAME should be a string that is a valid file name for the underlying
filesystem.
File name components that are `.' are removed, and
so are file name components followed by `..', along with the `..' itself;
note that these simplifications are done without checking the resulting
file names in the file system.
Multiple consecutive slashes are collapsed into a single slash,
except at the beginning of the file name when they are significant (e.g.,
UNC file names on MS-Windows.)
An initial `~/' expands to your home directory.
An initial `~USER/' expands to USER's home directory.
See also the function `substitute-in-file-name'.

For technical reasons, this function can return correct but
non-intuitive results for the root directory; for instance,
\(expand-file-name ".." "/") returns "/..".  For this reason, use
\(directory-file-name (file-name-directory dirname)) to traverse a
filesystem tree, not (expand-file-name ".."  dirname).  */)
  (Lisp_Object name, Lisp_Object default_directory)
{
  /* These point to SDATA and need to be careful with string-relocation
     during GC (via DECODE_FILE).  */
  char *nm;
  const char *newdir;
  /* This should only point to alloca'd data.  */
  char *target;

  ptrdiff_t tlen;
  struct passwd *pw;
#ifdef DOS_NT
  int drive = 0;
  int collapse_newdir = 1;
  int is_escaped = 0;
#endif /* DOS_NT */
  ptrdiff_t length;
  Lisp_Object handler, result, handled_name;
  int multibyte;
  Lisp_Object hdir;

  CHECK_STRING (name);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (name, Qexpand_file_name);
  if (!NILP (handler))
    {
      handled_name = call3 (handler, Qexpand_file_name,
			    name, default_directory);
      if (STRINGP (handled_name))
	return handled_name;
      error ("Invalid handler in `file-name-handler-alist'");
    }


  /* Use the buffer's default-directory if DEFAULT_DIRECTORY is omitted.  */
  if (NILP (default_directory))
    default_directory = BVAR (current_buffer, directory);
  if (! STRINGP (default_directory))
    {
#ifdef DOS_NT
      /* "/" is not considered a root directory on DOS_NT, so using "/"
	 here causes an infinite recursion in, e.g., the following:

            (let (default-directory)
	      (expand-file-name "a"))

	 To avoid this, we set default_directory to the root of the
	 current drive.  */
      default_directory = build_string (emacs_root_dir ());
#else
      default_directory = build_string ("/");
#endif
    }

  if (!NILP (default_directory))
    {
      handler = Ffind_file_name_handler (default_directory, Qexpand_file_name);
      if (!NILP (handler))
	{
	  handled_name = call3 (handler, Qexpand_file_name,
				name, default_directory);
	  if (STRINGP (handled_name))
	    return handled_name;
	  error ("Invalid handler in `file-name-handler-alist'");
	}
    }

  {
    char *o = SSDATA (default_directory);

    /* Make sure DEFAULT_DIRECTORY is properly expanded.
       It would be better to do this down below where we actually use
       default_directory.  Unfortunately, calling Fexpand_file_name recursively
       could invoke GC, and the strings might be relocated.  This would
       be annoying because we have pointers into strings lying around
       that would need adjusting, and people would add new pointers to
       the code and forget to adjust them, resulting in intermittent bugs.
       Putting this call here avoids all that crud.

       The EQ test avoids infinite recursion.  */
    if (! NILP (default_directory) && !EQ (default_directory, name)
	/* Save time in some common cases - as long as default_directory
	   is not relative, it can be canonicalized with name below (if it
	   is needed at all) without requiring it to be expanded now.  */
#ifdef DOS_NT
	/* Detect MSDOS file names with drive specifiers.  */
	&& ! (IS_DRIVE (o[0]) && IS_DEVICE_SEP (o[1])
	      && IS_DIRECTORY_SEP (o[2]))
#ifdef WINDOWSNT
	/* Detect Windows file names in UNC format.  */
	&& ! (IS_DIRECTORY_SEP (o[0]) && IS_DIRECTORY_SEP (o[1]))
#endif
#else /* not DOS_NT */
      /* Detect Unix absolute file names (/... alone is not absolute on
	 DOS or Windows).  */
	&& ! (IS_DIRECTORY_SEP (o[0]))
#endif /* not DOS_NT */
	)
      {
	struct gcpro gcpro1;

	GCPRO1 (name);
	default_directory = Fexpand_file_name (default_directory, Qnil);
	UNGCPRO;
      }
  }
  name = FILE_SYSTEM_CASE (name);
  multibyte = STRING_MULTIBYTE (name);
  if (multibyte != STRING_MULTIBYTE (default_directory))
    {
      if (multibyte)
	default_directory = string_to_multibyte (default_directory);
      else
	{
	  name = string_to_multibyte (name);
	  multibyte = 1;
	}
    }

  /* Make a local copy of nm[] to protect it from GC in DECODE_FILE below. */
  nm = (char *) alloca (SBYTES (name) + 1);
  memcpy (nm, SSDATA (name), SBYTES (name) + 1);

#ifdef DOS_NT
  /* Note if special escape prefix is present, but remove for now.  */
  if (nm[0] == '/' && nm[1] == ':')
    {
      is_escaped = 1;
      nm += 2;
    }

  /* Find and remove drive specifier if present; this makes nm absolute
     even if the rest of the name appears to be relative.  Only look for
     drive specifier at the beginning.  */
  if (IS_DRIVE (nm[0]) && IS_DEVICE_SEP (nm[1]))
    {
      drive = (unsigned char) nm[0];
      nm += 2;
    }

#ifdef WINDOWSNT
  /* If we see "c://somedir", we want to strip the first slash after the
     colon when stripping the drive letter.  Otherwise, this expands to
     "//somedir".  */
  if (drive && IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1]))
    nm++;

  /* Discard any previous drive specifier if nm is now in UNC format. */
  if (IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1]))
    {
      drive = 0;
    }
#endif /* WINDOWSNT */
#endif /* DOS_NT */

  /* If nm is absolute, look for `/./' or `/../' or `//''sequences; if
     none are found, we can probably return right away.  We will avoid
     allocating a new string if name is already fully expanded.  */
  if (
      IS_DIRECTORY_SEP (nm[0])
#ifdef MSDOS
      && drive && !is_escaped
#endif
#ifdef WINDOWSNT
      && (drive || IS_DIRECTORY_SEP (nm[1])) && !is_escaped
#endif
      )
    {
      /* If it turns out that the filename we want to return is just a
	 suffix of FILENAME, we don't need to go through and edit
	 things; we just need to construct a new string using data
	 starting at the middle of FILENAME.  If we set lose to a
	 non-zero value, that means we've discovered that we can't do
	 that cool trick.  */
      int lose = 0;
      char *p = nm;

      while (*p)
	{
	  /* Since we know the name is absolute, we can assume that each
	     element starts with a "/".  */

	  /* "." and ".." are hairy.  */
	  if (IS_DIRECTORY_SEP (p[0])
	      && p[1] == '.'
	      && (IS_DIRECTORY_SEP (p[2])
		  || p[2] == 0
		  || (p[2] == '.' && (IS_DIRECTORY_SEP (p[3])
				      || p[3] == 0))))
	    lose = 1;
	  /* We want to replace multiple `/' in a row with a single
	     slash.  */
	  else if (p > nm
		   && IS_DIRECTORY_SEP (p[0])
		   && IS_DIRECTORY_SEP (p[1]))
	    lose = 1;
	  p++;
	}
      if (!lose)
	{
#ifdef DOS_NT
	  /* Make sure directories are all separated with /, but
	     avoid allocation of a new string when not required. */
	  dostounix_filename (nm);
#ifdef WINDOWSNT
	  if (IS_DIRECTORY_SEP (nm[1]))
	    {
	      if (strcmp (nm, SSDATA (name)) != 0)
		name = make_specified_string (nm, -1, strlen (nm), multibyte);
	    }
	  else
#endif
	  /* drive must be set, so this is okay */
	  if (strcmp (nm - 2, SSDATA (name)) != 0)
	    {
	      char temp[] = " :";

	      name = make_specified_string (nm, -1, p - nm, multibyte);
	      temp[0] = DRIVE_LETTER (drive);
	      name = concat2 (build_string (temp), name);
	    }
	  return name;
#else /* not DOS_NT */
	  if (strcmp (nm, SSDATA (name)) == 0)
	    return name;
	  return make_specified_string (nm, -1, strlen (nm), multibyte);
#endif /* not DOS_NT */
	}
    }

  /* At this point, nm might or might not be an absolute file name.  We
     need to expand ~ or ~user if present, otherwise prefix nm with
     default_directory if nm is not absolute, and finally collapse /./
     and /foo/../ sequences.

     We set newdir to be the appropriate prefix if one is needed:
       - the relevant user directory if nm starts with ~ or ~user
       - the specified drive's working dir (DOS/NT only) if nm does not
         start with /
       - the value of default_directory.

     Note that these prefixes are not guaranteed to be absolute (except
     for the working dir of a drive).  Therefore, to ensure we always
     return an absolute name, if the final prefix is not absolute we
     append it to the current working directory.  */

  newdir = 0;

  if (nm[0] == '~')		/* prefix ~ */
    {
      if (IS_DIRECTORY_SEP (nm[1])
	  || nm[1] == 0)	/* ~ by itself */
	{
	  Lisp_Object tem;

	  if (!(newdir = egetenv ("HOME")))
	    newdir = "";
	  nm++;
	  /* egetenv may return a unibyte string, which will bite us since
	     we expect the directory to be multibyte.  */
	  tem = build_string (newdir);
	  if (!STRING_MULTIBYTE (tem))
	    {
	      hdir = DECODE_FILE (tem);
	      newdir = SSDATA (hdir);
	    }
#ifdef DOS_NT
	  collapse_newdir = 0;
#endif
	}
      else			/* ~user/filename */
	{
	  char *o, *p;
	  for (p = nm; *p && (!IS_DIRECTORY_SEP (*p)); p++);
	  o = alloca (p - nm + 1);
	  memcpy (o, nm, p - nm);
	  o [p - nm] = 0;

	  BLOCK_INPUT;
	  pw = (struct passwd *) getpwnam (o + 1);
	  UNBLOCK_INPUT;
	  if (pw)
	    {
	      newdir = pw->pw_dir;
	      nm = p;
#ifdef DOS_NT
	      collapse_newdir = 0;
#endif
	    }

	  /* If we don't find a user of that name, leave the name
	     unchanged; don't move nm forward to p.  */
	}
    }

#ifdef DOS_NT
  /* On DOS and Windows, nm is absolute if a drive name was specified;
     use the drive's current directory as the prefix if needed.  */
  if (!newdir && drive)
    {
      /* Get default directory if needed to make nm absolute. */
      char *adir = NULL;
      if (!IS_DIRECTORY_SEP (nm[0]))
	{
	  adir = alloca (MAXPATHLEN + 1);
	  if (!getdefdir (toupper (drive) - 'A' + 1, adir))
	    adir = NULL;
	}
      if (!adir)
	{
	  /* Either nm starts with /, or drive isn't mounted. */
	  adir = alloca (4);
	  adir[0] = DRIVE_LETTER (drive);
	  adir[1] = ':';
	  adir[2] = '/';
	  adir[3] = 0;
	}
      newdir = adir;
    }
#endif /* DOS_NT */

  /* Finally, if no prefix has been specified and nm is not absolute,
     then it must be expanded relative to default_directory. */

  if (1
#ifndef DOS_NT
      /* /... alone is not absolute on DOS and Windows. */
      && !IS_DIRECTORY_SEP (nm[0])
#endif
#ifdef WINDOWSNT
      && !(IS_DIRECTORY_SEP (nm[0]) && IS_DIRECTORY_SEP (nm[1]))
#endif
      && !newdir)
    {
      newdir = SSDATA (default_directory);
#ifdef DOS_NT
      /* Note if special escape prefix is present, but remove for now.  */
      if (newdir[0] == '/' && newdir[1] == ':')
	{
	  is_escaped = 1;
	  newdir += 2;
	}
#endif
    }

#ifdef DOS_NT
  if (newdir)
    {
      /* First ensure newdir is an absolute name. */
      if (
	  /* Detect MSDOS file names with drive specifiers.  */
	  ! (IS_DRIVE (newdir[0])
	     && IS_DEVICE_SEP (newdir[1]) && IS_DIRECTORY_SEP (newdir[2]))
#ifdef WINDOWSNT
	  /* Detect Windows file names in UNC format.  */
	  && ! (IS_DIRECTORY_SEP (newdir[0]) && IS_DIRECTORY_SEP (newdir[1]))
#endif
	  )
	{
	  /* Effectively, let newdir be (expand-file-name newdir cwd).
	     Because of the admonition against calling expand-file-name
	     when we have pointers into lisp strings, we accomplish this
	     indirectly by prepending newdir to nm if necessary, and using
	     cwd (or the wd of newdir's drive) as the new newdir. */
	  char *adir;
	  if (IS_DRIVE (newdir[0]) && IS_DEVICE_SEP (newdir[1]))
	    {
	      drive = (unsigned char) newdir[0];
	      newdir += 2;
	    }
	  if (!IS_DIRECTORY_SEP (nm[0]))
	    {
	      char * tmp = alloca (strlen (newdir) + strlen (nm) + 2);
	      file_name_as_directory (tmp, newdir);
	      strcat (tmp, nm);
	      nm = tmp;
	    }
	  adir = alloca (MAXPATHLEN + 1);
	  if (drive)
	    {
	      if (!getdefdir (toupper (drive) - 'A' + 1, adir))
		newdir = "/";
	    }
	  else
	    getwd (adir);
	  newdir = adir;
	}

      /* Strip off drive name from prefix, if present. */
      if (IS_DRIVE (newdir[0]) && IS_DEVICE_SEP (newdir[1]))
	{
	  drive = newdir[0];
	  newdir += 2;
	}

      /* Keep only a prefix from newdir if nm starts with slash
         (//server/share for UNC, nothing otherwise).  */
      if (IS_DIRECTORY_SEP (nm[0]) && collapse_newdir)
	{
#ifdef WINDOWSNT
	  if (IS_DIRECTORY_SEP (newdir[0]) && IS_DIRECTORY_SEP (newdir[1]))
	    {
	      char *adir = strcpy (alloca (strlen (newdir) + 1), newdir);
	      char *p = adir + 2;
	      while (*p && !IS_DIRECTORY_SEP (*p)) p++;
	      p++;
	      while (*p && !IS_DIRECTORY_SEP (*p)) p++;
	      *p = 0;
	      newdir = adir;
	    }
	  else
#endif
	    newdir = "";
	}
    }
#endif /* DOS_NT */

  if (newdir)
    {
      /* Get rid of any slash at the end of newdir, unless newdir is
	 just / or // (an incomplete UNC name).  */
      length = strlen (newdir);
      if (length > 1 && IS_DIRECTORY_SEP (newdir[length - 1])
#ifdef WINDOWSNT
	  && !(length == 2 && IS_DIRECTORY_SEP (newdir[0]))
#endif
	  )
	{
	  char *temp = (char *) alloca (length);
	  memcpy (temp, newdir, length - 1);
	  temp[length - 1] = 0;
	  newdir = temp;
	}
      tlen = length + 1;
    }
  else
    tlen = 0;

  /* Now concatenate the directory and name to new space in the stack frame */
  tlen += strlen (nm) + 1;
#ifdef DOS_NT
  /* Reserve space for drive specifier and escape prefix, since either
     or both may need to be inserted.  (The Microsoft x86 compiler
     produces incorrect code if the following two lines are combined.)  */
  target = (char *) alloca (tlen + 4);
  target += 4;
#else  /* not DOS_NT */
  target = (char *) alloca (tlen);
#endif /* not DOS_NT */
  *target = 0;

  if (newdir)
    {
      if (nm[0] == 0 || IS_DIRECTORY_SEP (nm[0]))
	{
#ifdef DOS_NT
	  /* If newdir is effectively "C:/", then the drive letter will have
	     been stripped and newdir will be "/".  Concatenating with an
	     absolute directory in nm produces "//", which will then be
	     incorrectly treated as a network share.  Ignore newdir in
	     this case (keeping the drive letter).  */
	  if (!(drive && nm[0] && IS_DIRECTORY_SEP (newdir[0])
		&& newdir[1] == '\0'))
#endif
	    strcpy (target, newdir);
	}
      else
	file_name_as_directory (target, newdir);
    }

  strcat (target, nm);

  /* Now canonicalize by removing `//', `/.' and `/foo/..' if they
     appear.  */
  {
    char *p = target;
    char *o = target;

    while (*p)
      {
	if (!IS_DIRECTORY_SEP (*p))
	  {
	    *o++ = *p++;
	  }
	else if (p[1] == '.'
		 && (IS_DIRECTORY_SEP (p[2])
		     || p[2] == 0))
	  {
	    /* If "/." is the entire filename, keep the "/".  Otherwise,
	       just delete the whole "/.".  */
	    if (o == target && p[2] == '\0')
	      *o++ = *p;
	    p += 2;
	  }
	else if (p[1] == '.' && p[2] == '.'
		 /* `/../' is the "superroot" on certain file systems.
		    Turned off on DOS_NT systems because they have no
		    "superroot" and because this causes us to produce
		    file names like "d:/../foo" which fail file-related
		    functions of the underlying OS.  (To reproduce, try a
		    long series of "../../" in default_directory, longer
		    than the number of levels from the root.)  */
#ifndef DOS_NT
		 && o != target
#endif
		 && (IS_DIRECTORY_SEP (p[3]) || p[3] == 0))
	  {
#ifdef WINDOWSNT
	    char *prev_o = o;
#endif
	    while (o != target && (--o) && !IS_DIRECTORY_SEP (*o))
	      ;
#ifdef WINDOWSNT
	    /* Don't go below server level in UNC filenames.  */
	    if (o == target + 1 && IS_DIRECTORY_SEP (*o)
		&& IS_DIRECTORY_SEP (*target))
	      o = prev_o;
	    else
#endif
	    /* Keep initial / only if this is the whole name.  */
	    if (o == target && IS_ANY_SEP (*o) && p[3] == 0)
	      ++o;
	    p += 3;
	  }
	else if (p > target && IS_DIRECTORY_SEP (p[1]))
	  /* Collapse multiple `/' in a row.  */
	  p++;
	else
	  {
	    *o++ = *p++;
	  }
      }

#ifdef DOS_NT
    /* At last, set drive name. */
#ifdef WINDOWSNT
    /* Except for network file name.  */
    if (!(IS_DIRECTORY_SEP (target[0]) && IS_DIRECTORY_SEP (target[1])))
#endif /* WINDOWSNT */
      {
	if (!drive) abort ();
	target -= 2;
	target[0] = DRIVE_LETTER (drive);
	target[1] = ':';
      }
    /* Reinsert the escape prefix if required.  */
    if (is_escaped)
      {
	target -= 2;
	target[0] = '/';
	target[1] = ':';
      }
    dostounix_filename (target);
#endif /* DOS_NT */

    result = make_specified_string (target, -1, o - target, multibyte);
  }

  /* Again look to see if the file name has special constructs in it
     and perhaps call the corresponding file handler.  This is needed
     for filenames such as "/foo/../user@host:/bar/../baz".  Expanding
     the ".." component gives us "/user@host:/bar/../baz" which needs
     to be expanded again. */
  handler = Ffind_file_name_handler (result, Qexpand_file_name);
  if (!NILP (handler))
    {
      handled_name = call3 (handler, Qexpand_file_name,
			    result, default_directory);
      if (STRINGP (handled_name))
	return handled_name;
      error ("Invalid handler in `file-name-handler-alist'");
    }

  return result;
}

#if 0
/* PLEASE DO NOT DELETE THIS COMMENTED-OUT VERSION!
   This is the old version of expand-file-name, before it was thoroughly
   rewritten for Emacs 10.31.  We leave this version here commented-out,
   because the code is very complex and likely to have subtle bugs.  If
   bugs _are_ found, it might be of interest to look at the old code and
   see what did it do in the relevant situation.

   Don't remove this code: it's true that it will be accessible
   from the repository, but a few years from deletion, people will
   forget it is there.  */

/* Changed this DEFUN to a DEAFUN, so as not to confuse `make-docfile'.  */
DEAFUN ("expand-file-name", Fexpand_file_name, Sexpand_file_name, 1, 2, 0,
  "Convert FILENAME to absolute, and canonicalize it.\n\
Second arg DEFAULT is directory to start with if FILENAME is relative\n\
\(does not start with slash); if DEFAULT is nil or missing,\n\
the current buffer's value of default-directory is used.\n\
Filenames containing `.' or `..' as components are simplified;\n\
initial `~/' expands to your home directory.\n\
See also the function `substitute-in-file-name'.")
     (name, defalt)
     Lisp_Object name, defalt;
{
  unsigned char *nm;

  register unsigned char *newdir, *p, *o;
  ptrdiff_t tlen;
  unsigned char *target;
  struct passwd *pw;
  int lose;

  CHECK_STRING (name);
  nm = SDATA (name);

  /* If nm is absolute, flush ...// and detect /./ and /../.
     If no /./ or /../ we can return right away.  */
  if (nm[0] == '/')
    {
      p = nm;
      lose = 0;
      while (*p)
	{
	  if (p[0] == '/' && p[1] == '/'
	      )
	    nm = p + 1;
	  if (p[0] == '/' && p[1] == '~')
	    nm = p + 1, lose = 1;
	  if (p[0] == '/' && p[1] == '.'
	      && (p[2] == '/' || p[2] == 0
		  || (p[2] == '.' && (p[3] == '/' || p[3] == 0))))
	    lose = 1;
	  p++;
	}
      if (!lose)
	{
	  if (nm == SDATA (name))
	    return name;
	  return build_string (nm);
	}
    }

  /* Now determine directory to start with and put it in NEWDIR */

  newdir = 0;

  if (nm[0] == '~')             /* prefix ~ */
    if (nm[1] == '/' || nm[1] == 0)/* ~/filename */
      {
	if (!(newdir = (unsigned char *) egetenv ("HOME")))
	  newdir = (unsigned char *) "";
	nm++;
      }
    else  /* ~user/filename */
      {
	/* Get past ~ to user */
	unsigned char *user = nm + 1;
	/* Find end of name. */
	unsigned char *ptr = (unsigned char *) strchr (user, '/');
	ptrdiff_t len = ptr ? ptr - user : strlen (user);
	/* Copy the user name into temp storage. */
	o = (unsigned char *) alloca (len + 1);
	memcpy (o, user, len);
	o[len] = 0;

	/* Look up the user name. */
	BLOCK_INPUT;
	pw = (struct passwd *) getpwnam (o + 1);
	UNBLOCK_INPUT;
	if (!pw)
	  error ("\"%s\" isn't a registered user", o + 1);

	newdir = (unsigned char *) pw->pw_dir;

	/* Discard the user name from NM.  */
	nm += len;
      }

  if (nm[0] != '/' && !newdir)
    {
      if (NILP (defalt))
	defalt = current_buffer->directory;
      CHECK_STRING (defalt);
      newdir = SDATA (defalt);
    }

  /* Now concatenate the directory and name to new space in the stack frame */

  tlen = (newdir ? strlen (newdir) + 1 : 0) + strlen (nm) + 1;
  target = (unsigned char *) alloca (tlen);
  *target = 0;

  if (newdir)
    {
      if (nm[0] == 0 || nm[0] == '/')
	strcpy (target, newdir);
      else
      file_name_as_directory (target, newdir);
    }

  strcat (target, nm);

  /* Now canonicalize by removing /. and /foo/.. if they appear */

  p = target;
  o = target;

  while (*p)
    {
      if (*p != '/')
	{
	  *o++ = *p++;
	}
      else if (!strncmp (p, "//", 2)
	       )
	{
	  o = target;
	  p++;
	}
      else if (p[0] == '/' && p[1] == '.'
	       && (p[2] == '/' || p[2] == 0))
	p += 2;
      else if (!strncmp (p, "/..", 3)
	       /* `/../' is the "superroot" on certain file systems.  */
	       && o != target
	       && (p[3] == '/' || p[3] == 0))
	{
	  while (o != target && *--o != '/')
	    ;
	  if (o == target && *o == '/')
	    ++o;
	  p += 3;
	}
      else
	{
	  *o++ = *p++;
	}
    }

  return make_string (target, o - target);
}
#endif

/* If /~ or // appears, discard everything through first slash.  */
static int
file_name_absolute_p (const char *filename)
{
  return
    (IS_DIRECTORY_SEP (*filename) || *filename == '~'
#ifdef DOS_NT
     || (IS_DRIVE (*filename) && IS_DEVICE_SEP (filename[1])
	 && IS_DIRECTORY_SEP (filename[2]))
#endif
     );
}

static char *
search_embedded_absfilename (char *nm, char *endp)
{
  char *p, *s;

  for (p = nm + 1; p < endp; p++)
    {
      if ((0
	   || IS_DIRECTORY_SEP (p[-1]))
	  && file_name_absolute_p (p)
#if defined (WINDOWSNT) || defined (CYGWIN)
	  /* // at start of file name is meaningful in Apollo,
	     WindowsNT and Cygwin systems.  */
	  && !(IS_DIRECTORY_SEP (p[0]) && p - 1 == nm)
#endif /* not (WINDOWSNT || CYGWIN) */
	      )
	{
	  for (s = p; *s && (!IS_DIRECTORY_SEP (*s)); s++);
	  if (p[0] == '~' && s > p + 1)	/* we've got "/~something/" */
	    {
	      char *o = alloca (s - p + 1);
	      struct passwd *pw;
	      memcpy (o, p, s - p);
	      o [s - p] = 0;

	      /* If we have ~user and `user' exists, discard
		 everything up to ~.  But if `user' does not exist, leave
		 ~user alone, it might be a literal file name.  */
	      BLOCK_INPUT;
	      pw = getpwnam (o + 1);
	      UNBLOCK_INPUT;
	      if (pw)
		return p;
	    }
	  else
	    return p;
	}
    }
  return NULL;
}

DEFUN ("substitute-in-file-name", Fsubstitute_in_file_name,
       Ssubstitute_in_file_name, 1, 1, 0,
       doc: /* Substitute environment variables referred to in FILENAME.
`$FOO' where FOO is an environment variable name means to substitute
the value of that variable.  The variable name should be terminated
with a character not a letter, digit or underscore; otherwise, enclose
the entire variable name in braces.

If `/~' appears, all of FILENAME through that `/' is discarded.
If `//' appears, everything up to and including the first of
those `/' is discarded.  */)
  (Lisp_Object filename)
{
  char *nm;

  register char *s, *p, *o, *x, *endp;
  char *target = NULL;
  int total = 0;
  int substituted = 0;
  int multibyte;
  char *xnm;
  Lisp_Object handler;

  CHECK_STRING (filename);

  multibyte = STRING_MULTIBYTE (filename);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qsubstitute_in_file_name);
  if (!NILP (handler))
    {
      Lisp_Object handled_name = call2 (handler, Qsubstitute_in_file_name,
					filename);
      if (STRINGP (handled_name))
	return handled_name;
      error ("Invalid handler in `file-name-handler-alist'");
    }

  /* Always work on a copy of the string, in case GC happens during
     decode of environment variables, causing the original Lisp_String
     data to be relocated.  */
  nm = (char *) alloca (SBYTES (filename) + 1);
  memcpy (nm, SDATA (filename), SBYTES (filename) + 1);

#ifdef DOS_NT
  dostounix_filename (nm);
  substituted = (strcmp (nm, SDATA (filename)) != 0);
#endif
  endp = nm + SBYTES (filename);

  /* If /~ or // appears, discard everything through first slash.  */
  p = search_embedded_absfilename (nm, endp);
  if (p)
    /* Start over with the new string, so we check the file-name-handler
       again.  Important with filenames like "/home/foo//:/hello///there"
       which would substitute to "/:/hello///there" rather than "/there".  */
    return Fsubstitute_in_file_name
      (make_specified_string (p, -1, endp - p, multibyte));

  /* See if any variables are substituted into the string
     and find the total length of their values in `total' */

  for (p = nm; p != endp;)
    if (*p != '$')
      p++;
    else
      {
	p++;
	if (p == endp)
	  goto badsubst;
	else if (*p == '$')
	  {
	    /* "$$" means a single "$" */
	    p++;
	    total -= 1;
	    substituted = 1;
	    continue;
	  }
	else if (*p == '{')
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose;
	    s = p;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = (char *) alloca (s - o + 1);
	strncpy (target, o, s - o);
	target[s - o] = 0;
#ifdef DOS_NT
	strupr (target); /* $home == $HOME etc.  */
#endif /* DOS_NT */

	/* Get variable value */
	o = egetenv (target);
	if (o)
	  {
	    /* Don't try to guess a maximum length - UTF8 can use up to
	       four bytes per character.  This code is unlikely to run
	       in a situation that requires performance, so decoding the
	       env variables twice should be acceptable. Note that
	       decoding may cause a garbage collect.  */
	    Lisp_Object orig, decoded;
	    orig = make_unibyte_string (o, strlen (o));
	    decoded = DECODE_FILE (orig);
	    total += SBYTES (decoded);
	    substituted = 1;
	  }
	else if (*p == '}')
	  goto badvar;
      }

  if (!substituted)
    return filename;

  /* If substitution required, recopy the string and do it */
  /* Make space in stack frame for the new copy */
  xnm = (char *) alloca (SBYTES (filename) + total + 1);
  x = xnm;

  /* Copy the rest of the name through, replacing $ constructs with values */
  for (p = nm; *p;)
    if (*p != '$')
      *x++ = *p++;
    else
      {
	p++;
	if (p == endp)
	  goto badsubst;
	else if (*p == '$')
	  {
	    *x++ = *p++;
	    continue;
	  }
	else if (*p == '{')
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose;
	    s = p++;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = (char *) alloca (s - o + 1);
	strncpy (target, o, s - o);
	target[s - o] = 0;
#ifdef DOS_NT
	strupr (target); /* $home == $HOME etc.  */
#endif /* DOS_NT */

	/* Get variable value */
	o = egetenv (target);
	if (!o)
	  {
	    *x++ = '$';
	    strcpy (x, target); x+= strlen (target);
	  }
	else
	  {
	    Lisp_Object orig, decoded;
	    ptrdiff_t orig_length, decoded_length;
	    orig_length = strlen (o);
	    orig = make_unibyte_string (o, orig_length);
	    decoded = DECODE_FILE (orig);
	    decoded_length = SBYTES (decoded);
	    strncpy (x, SSDATA (decoded), decoded_length);
	    x += decoded_length;

	    /* If environment variable needed decoding, return value
	       needs to be multibyte.  */
	    if (decoded_length != orig_length
		|| strncmp (SSDATA (decoded), o, orig_length))
	      multibyte = 1;
	  }
      }

  *x = 0;

  /* If /~ or // appears, discard everything through first slash.  */
  while ((p = search_embedded_absfilename (xnm, x)))
    /* This time we do not start over because we've already expanded envvars
       and replaced $$ with $.  Maybe we should start over as well, but we'd
       need to quote some $ to $$ first.  */
    xnm = p;

  return make_specified_string (xnm, -1, x - xnm, multibyte);

 badsubst:
  error ("Bad format environment-variable substitution");
 missingclose:
  error ("Missing \"}\" in environment-variable substitution");
 badvar:
  error ("Substituting nonexistent environment variable \"%s\"", target);

  /* NOTREACHED */
  return Qnil;
}

/* A slightly faster and more convenient way to get
   (directory-file-name (expand-file-name FOO)).  */

Lisp_Object
expand_and_dir_to_file (Lisp_Object filename, Lisp_Object defdir)
{
  register Lisp_Object absname;

  absname = Fexpand_file_name (filename, defdir);

  /* Remove final slash, if any (unless this is the root dir).
     stat behaves differently depending!  */
  if (SCHARS (absname) > 1
      && IS_DIRECTORY_SEP (SREF (absname, SBYTES (absname) - 1))
      && !IS_DEVICE_SEP (SREF (absname, SBYTES (absname)-2)))
    /* We cannot take shortcuts; they might be wrong for magic file names.  */
    absname = Fdirectory_file_name (absname);
  return absname;
}

/* Signal an error if the file ABSNAME already exists.
   If INTERACTIVE is nonzero, ask the user whether to proceed,
   and bypass the error if the user says to go ahead.
   QUERYSTRING is a name for the action that is being considered
   to alter the file.

   *STATPTR is used to store the stat information if the file exists.
   If the file does not exist, STATPTR->st_mode is set to 0.
   If STATPTR is null, we don't store into it.

   If QUICK is nonzero, we ask for y or n, not yes or no.  */

static void
barf_or_query_if_file_exists (Lisp_Object absname, const char *querystring,
			      int interactive, struct stat *statptr, int quick)
{
  register Lisp_Object tem, encoded_filename;
  struct stat statbuf;
  struct gcpro gcpro1;

  encoded_filename = ENCODE_FILE (absname);

  /* stat is a good way to tell whether the file exists,
     regardless of what access permissions it has.  */
  if (lstat (SSDATA (encoded_filename), &statbuf) >= 0)
    {
      if (S_ISDIR (statbuf.st_mode))
	xsignal2 (Qfile_error,
		  build_string ("File is a directory"), absname);

      if (! interactive)
	xsignal2 (Qfile_already_exists,
		  build_string ("File already exists"), absname);
      GCPRO1 (absname);
      tem = format2 ("File %s already exists; %s anyway? ",
		     absname, build_string (querystring));
      if (quick)
	tem = call1 (intern ("y-or-n-p"), tem);
      else
	tem = do_yes_or_no_p (tem);
      UNGCPRO;
      if (NILP (tem))
	xsignal2 (Qfile_already_exists,
		  build_string ("File already exists"), absname);
      if (statptr)
	*statptr = statbuf;
    }
  else
    {
      if (statptr)
	statptr->st_mode = 0;
    }
  return;
}

DEFUN ("copy-file", Fcopy_file, Scopy_file, 2, 6,
       "fCopy file: \nGCopy %s to file: \np\nP",
       doc: /* Copy FILE to NEWNAME.  Both args must be strings.
If NEWNAME names a directory, copy FILE there.

This function always sets the file modes of the output file to match
the input file.

The optional third argument OK-IF-ALREADY-EXISTS specifies what to do
if file NEWNAME already exists.  If OK-IF-ALREADY-EXISTS is nil, we
signal a `file-already-exists' error without overwriting.  If
OK-IF-ALREADY-EXISTS is a number, we request confirmation from the user
about overwriting; this is what happens in interactive use with M-x.
Any other value for OK-IF-ALREADY-EXISTS means to overwrite the
existing file.

Fourth arg KEEP-TIME non-nil means give the output file the same
last-modified time as the old one.  (This works on only some systems.)

A prefix arg makes KEEP-TIME non-nil.

If PRESERVE-UID-GID is non-nil, we try to transfer the
uid and gid of FILE to NEWNAME.

If PRESERVE-SELINUX-CONTEXT is non-nil and SELinux is enabled
on the system, we copy the SELinux context of FILE to NEWNAME.  */)
  (Lisp_Object file, Lisp_Object newname, Lisp_Object ok_if_already_exists, Lisp_Object keep_time, Lisp_Object preserve_uid_gid, Lisp_Object preserve_selinux_context)
{
  int ifd, ofd;
  EMACS_INT n;
  char buf[16 * 1024];
  struct stat st, out_st;
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  int count = SPECPDL_INDEX ();
  int input_file_statable_p;
  Lisp_Object encoded_file, encoded_newname;
#if HAVE_LIBSELINUX
  security_context_t con;
  int fail, conlength = 0;
#endif

  encoded_file = encoded_newname = Qnil;
  GCPRO4 (file, newname, encoded_file, encoded_newname);
  CHECK_STRING (file);
  CHECK_STRING (newname);

  if (!NILP (Ffile_directory_p (newname)))
    newname = Fexpand_file_name (Ffile_name_nondirectory (file), newname);
  else
    newname = Fexpand_file_name (newname, Qnil);

  file = Fexpand_file_name (file, Qnil);

  /* If the input file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qcopy_file);
  /* Likewise for output file name.  */
  if (NILP (handler))
    handler = Ffind_file_name_handler (newname, Qcopy_file);
  if (!NILP (handler))
    RETURN_UNGCPRO (call7 (handler, Qcopy_file, file, newname,
			   ok_if_already_exists, keep_time, preserve_uid_gid,
			   preserve_selinux_context));

  encoded_file = ENCODE_FILE (file);
  encoded_newname = ENCODE_FILE (newname);

  if (NILP (ok_if_already_exists)
      || INTEGERP (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, "copy to it",
				  INTEGERP (ok_if_already_exists), &out_st, 0);
  else if (stat (SSDATA (encoded_newname), &out_st) < 0)
    out_st.st_mode = 0;

#ifdef WINDOWSNT
  if (!CopyFile (SDATA (encoded_file),
		 SDATA (encoded_newname),
		 FALSE))
    report_file_error ("Copying file", Fcons (file, Fcons (newname, Qnil)));
  /* CopyFile retains the timestamp by default.  */
  else if (NILP (keep_time))
    {
      EMACS_TIME now;
      DWORD attributes;
      char * filename;

      EMACS_GET_TIME (now);
      filename = SDATA (encoded_newname);

      /* Ensure file is writable while its modified time is set.  */
      attributes = GetFileAttributes (filename);
      SetFileAttributes (filename, attributes & ~FILE_ATTRIBUTE_READONLY);
      if (set_file_times (filename, now, now))
	{
	  /* Restore original attributes.  */
	  SetFileAttributes (filename, attributes);
	  xsignal2 (Qfile_date_error,
		    build_string ("Cannot set file date"), newname);
	}
      /* Restore original attributes.  */
      SetFileAttributes (filename, attributes);
    }
#else /* not WINDOWSNT */
  immediate_quit = 1;
  ifd = emacs_open (SSDATA (encoded_file), O_RDONLY, 0);
  immediate_quit = 0;

  if (ifd < 0)
    report_file_error ("Opening input file", Fcons (file, Qnil));

  record_unwind_protect (close_file_unwind, make_number (ifd));

  /* We can only copy regular files and symbolic links.  Other files are not
     copyable by us. */
  input_file_statable_p = (fstat (ifd, &st) >= 0);

#if HAVE_LIBSELINUX
  if (!NILP (preserve_selinux_context) && is_selinux_enabled ())
    {
      conlength = fgetfilecon (ifd, &con);
      if (conlength == -1)
	report_file_error ("Doing fgetfilecon", Fcons (file, Qnil));
    }
#endif

  if (out_st.st_mode != 0
      && st.st_dev == out_st.st_dev && st.st_ino == out_st.st_ino)
    {
      errno = 0;
      report_file_error ("Input and output files are the same",
			 Fcons (file, Fcons (newname, Qnil)));
    }

  if (input_file_statable_p)
    {
      if (!(S_ISREG (st.st_mode)) && !(S_ISLNK (st.st_mode)))
	{
#if defined (EISDIR)
	  /* Get a better looking error message. */
	  errno = EISDIR;
#endif /* EISDIR */
	  report_file_error ("Non-regular file", Fcons (file, Qnil));
	}
    }

#ifdef MSDOS
  /* System's default file type was set to binary by _fmode in emacs.c.  */
  ofd = emacs_open (SDATA (encoded_newname),
		    O_WRONLY | O_TRUNC | O_CREAT
		    | (NILP (ok_if_already_exists) ? O_EXCL : 0),
		    S_IREAD | S_IWRITE);
#else  /* not MSDOS */
  {
    int new_mask = 0666;
    if (input_file_statable_p)
      {
	if (!NILP (preserve_uid_gid))
	  new_mask = 0600;
	new_mask &= st.st_mode;
      }
    ofd = emacs_open (SSDATA (encoded_newname),
		      (O_WRONLY | O_TRUNC | O_CREAT
		       | (NILP (ok_if_already_exists) ? O_EXCL : 0)),
		      new_mask);
  }
#endif /* not MSDOS */
  if (ofd < 0)
    report_file_error ("Opening output file", Fcons (newname, Qnil));

  record_unwind_protect (close_file_unwind, make_number (ofd));

  immediate_quit = 1;
  QUIT;
  while ((n = emacs_read (ifd, buf, sizeof buf)) > 0)
    if (emacs_write (ofd, buf, n) != n)
      report_file_error ("I/O error", Fcons (newname, Qnil));
  immediate_quit = 0;

#ifndef MSDOS
  /* Preserve the original file modes, and if requested, also its
     owner and group.  */
  if (input_file_statable_p)
    {
      int mode_mask = 07777;
      if (!NILP (preserve_uid_gid))
	{
	  /* Attempt to change owner and group.  If that doesn't work
	     attempt to change just the group, as that is sometimes allowed.
	     Adjust the mode mask to eliminate setuid or setgid bits
	     that are inappropriate if the owner and group are wrong.  */
	  if (fchown (ofd, st.st_uid, st.st_gid) != 0)
	    {
	      mode_mask &= ~06000;
	      if (fchown (ofd, -1, st.st_gid) == 0)
		mode_mask |= 02000;
	    }
	}
      if (fchmod (ofd, st.st_mode & mode_mask) != 0)
	report_file_error ("Doing chmod", Fcons (newname, Qnil));
    }
#endif	/* not MSDOS */

#if HAVE_LIBSELINUX
  if (conlength > 0)
    {
      /* Set the modified context back to the file.  */
      fail = fsetfilecon (ofd, con);
      /* See http://debbugs.gnu.org/11245 for ENOTSUP.  */
      if (fail && errno != ENOTSUP)
	report_file_error ("Doing fsetfilecon", Fcons (newname, Qnil));

      freecon (con);
    }
#endif

  /* Closing the output clobbers the file times on some systems.  */
  if (emacs_close (ofd) < 0)
    report_file_error ("I/O error", Fcons (newname, Qnil));

  if (input_file_statable_p)
    {
      if (!NILP (keep_time))
	{
	  EMACS_TIME atime, mtime;
	  EMACS_SET_SECS_USECS (atime, st.st_atime, 0);
	  EMACS_SET_SECS_USECS (mtime, st.st_mtime, 0);
	  if (set_file_times (SSDATA (encoded_newname),
			      atime, mtime))
	    xsignal2 (Qfile_date_error,
		      build_string ("Cannot set file date"), newname);
	}
    }

  emacs_close (ifd);

#ifdef MSDOS
  if (input_file_statable_p)
    {
      /* In DJGPP v2.0 and later, fstat usually returns true file mode bits,
         and if it can't, it tells so.  Otherwise, under MSDOS we usually
         get only the READ bit, which will make the copied file read-only,
         so it's better not to chmod at all.  */
      if ((_djstat_flags & _STFAIL_WRITEBIT) == 0)
	chmod (SDATA (encoded_newname), st.st_mode & 07777);
    }
#endif /* MSDOS */
#endif /* not WINDOWSNT */

  /* Discard the unwind protects.  */
  specpdl_ptr = specpdl + count;

  UNGCPRO;
  return Qnil;
}

DEFUN ("make-directory-internal", Fmake_directory_internal,
       Smake_directory_internal, 1, 1, 0,
       doc: /* Create a new directory named DIRECTORY.  */)
  (Lisp_Object directory)
{
  const char *dir;
  Lisp_Object handler;
  Lisp_Object encoded_dir;

  CHECK_STRING (directory);
  directory = Fexpand_file_name (directory, Qnil);

  handler = Ffind_file_name_handler (directory, Qmake_directory_internal);
  if (!NILP (handler))
    return call2 (handler, Qmake_directory_internal, directory);

  encoded_dir = ENCODE_FILE (directory);

  dir = SSDATA (encoded_dir);

#ifdef WINDOWSNT
  if (mkdir (dir) != 0)
#else
  if (mkdir (dir, 0777 & ~auto_saving_dir_umask) != 0)
#endif
    report_file_error ("Creating directory", list1 (directory));

  return Qnil;
}

DEFUN ("delete-directory-internal", Fdelete_directory_internal,
       Sdelete_directory_internal, 1, 1, 0,
       doc: /* Delete the directory named DIRECTORY.  Does not follow symlinks.  */)
  (Lisp_Object directory)
{
  const char *dir;
  Lisp_Object encoded_dir;

  CHECK_STRING (directory);
  directory = Fdirectory_file_name (Fexpand_file_name (directory, Qnil));
  encoded_dir = ENCODE_FILE (directory);
  dir = SSDATA (encoded_dir);

  if (rmdir (dir) != 0)
    report_file_error ("Removing directory", list1 (directory));

  return Qnil;
}

DEFUN ("delete-file", Fdelete_file, Sdelete_file, 1, 2,
       "(list (read-file-name \
                (if (and delete-by-moving-to-trash (null current-prefix-arg)) \
                    \"Move file to trash: \" \"Delete file: \") \
                nil default-directory (confirm-nonexistent-file-or-buffer)) \
              (null current-prefix-arg))",
       doc: /* Delete file named FILENAME.  If it is a symlink, remove the symlink.
If file has multiple names, it continues to exist with the other names.
TRASH non-nil means to trash the file instead of deleting, provided
`delete-by-moving-to-trash' is non-nil.

When called interactively, TRASH is t if no prefix argument is given.
With a prefix argument, TRASH is nil.  */)
  (Lisp_Object filename, Lisp_Object trash)
{
  Lisp_Object handler;
  Lisp_Object encoded_file;
  struct gcpro gcpro1;

  GCPRO1 (filename);
  if (!NILP (Ffile_directory_p (filename))
      && NILP (Ffile_symlink_p (filename)))
    xsignal2 (Qfile_error,
	      build_string ("Removing old name: is a directory"),
	      filename);
  UNGCPRO;
  filename = Fexpand_file_name (filename, Qnil);

  handler = Ffind_file_name_handler (filename, Qdelete_file);
  if (!NILP (handler))
    return call3 (handler, Qdelete_file, filename, trash);

  if (delete_by_moving_to_trash && !NILP (trash))
    return call1 (Qmove_file_to_trash, filename);

  encoded_file = ENCODE_FILE (filename);

  if (0 > unlink (SSDATA (encoded_file)))
    report_file_error ("Removing old name", list1 (filename));
  return Qnil;
}

static Lisp_Object
internal_delete_file_1 (Lisp_Object ignore)
{
  return Qt;
}

/* Delete file FILENAME, returning 1 if successful and 0 if failed.
   This ignores `delete-by-moving-to-trash'.  */

int
internal_delete_file (Lisp_Object filename)
{
  Lisp_Object tem;

  tem = internal_condition_case_2 (Fdelete_file, filename, Qnil,
				   Qt, internal_delete_file_1);
  return NILP (tem);
}

DEFUN ("rename-file", Frename_file, Srename_file, 2, 3,
       "fRename file: \nGRename %s to file: \np",
       doc: /* Rename FILE as NEWNAME.  Both args must be strings.
If file has names other than FILE, it continues to have those names.
Signals a `file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.  */)
  (Lisp_Object file, Lisp_Object newname, Lisp_Object ok_if_already_exists)
{
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  Lisp_Object encoded_file, encoded_newname, symlink_target;

  symlink_target = encoded_file = encoded_newname = Qnil;
  GCPRO5 (file, newname, encoded_file, encoded_newname, symlink_target);
  CHECK_STRING (file);
  CHECK_STRING (newname);
  file = Fexpand_file_name (file, Qnil);

  if ((!NILP (Ffile_directory_p (newname)))
#ifdef DOS_NT
      /* If the file names are identical but for the case,
	 don't attempt to move directory to itself. */
      && (NILP (Fstring_equal (Fdowncase (file), Fdowncase (newname))))
#endif
      )
    {
      Lisp_Object fname = NILP (Ffile_directory_p (file))
	? file : Fdirectory_file_name (file);
      newname = Fexpand_file_name (Ffile_name_nondirectory (fname), newname);
    }
  else
    newname = Fexpand_file_name (newname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qrename_file);
  if (NILP (handler))
    handler = Ffind_file_name_handler (newname, Qrename_file);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qrename_file,
			   file, newname, ok_if_already_exists));

  encoded_file = ENCODE_FILE (file);
  encoded_newname = ENCODE_FILE (newname);

#ifdef DOS_NT
  /* If the file names are identical but for the case, don't ask for
     confirmation: they simply want to change the letter-case of the
     file name.  */
  if (NILP (Fstring_equal (Fdowncase (file), Fdowncase (newname))))
#endif
  if (NILP (ok_if_already_exists)
      || INTEGERP (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, "rename to it",
				  INTEGERP (ok_if_already_exists), 0, 0);
  if (0 > rename (SSDATA (encoded_file), SSDATA (encoded_newname)))
    {
      if (errno == EXDEV)
	{
          int count;
          symlink_target = Ffile_symlink_p (file);
          if (! NILP (symlink_target))
            Fmake_symbolic_link (symlink_target, newname,
                                 NILP (ok_if_already_exists) ? Qnil : Qt);
	  else if (!NILP (Ffile_directory_p (file)))
	    call4 (Qcopy_directory, file, newname, Qt, Qnil);
	  else
	    /* We have already prompted if it was an integer, so don't
	       have copy-file prompt again.  */
	    Fcopy_file (file, newname,
			NILP (ok_if_already_exists) ? Qnil : Qt,
			Qt, Qt, Qt);

	  count = SPECPDL_INDEX ();
	  specbind (Qdelete_by_moving_to_trash, Qnil);

	  if (!NILP (Ffile_directory_p (file)) && NILP (symlink_target))
	    call2 (Qdelete_directory, file, Qt);
	  else
	    Fdelete_file (file, Qnil);
	  unbind_to (count, Qnil);
	}
      else
	report_file_error ("Renaming", list2 (file, newname));
    }
  UNGCPRO;
  return Qnil;
}

DEFUN ("add-name-to-file", Fadd_name_to_file, Sadd_name_to_file, 2, 3,
       "fAdd name to file: \nGName to add to %s: \np",
       doc: /* Give FILE additional name NEWNAME.  Both args must be strings.
Signals a `file-already-exists' error if a file NEWNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.  */)
  (Lisp_Object file, Lisp_Object newname, Lisp_Object ok_if_already_exists)
{
  Lisp_Object handler;
  Lisp_Object encoded_file, encoded_newname;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  GCPRO4 (file, newname, encoded_file, encoded_newname);
  encoded_file = encoded_newname = Qnil;
  CHECK_STRING (file);
  CHECK_STRING (newname);
  file = Fexpand_file_name (file, Qnil);

  if (!NILP (Ffile_directory_p (newname)))
    newname = Fexpand_file_name (Ffile_name_nondirectory (file), newname);
  else
    newname = Fexpand_file_name (newname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (file, Qadd_name_to_file);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qadd_name_to_file, file,
			   newname, ok_if_already_exists));

  /* If the new name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (newname, Qadd_name_to_file);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qadd_name_to_file, file,
			   newname, ok_if_already_exists));

  encoded_file = ENCODE_FILE (file);
  encoded_newname = ENCODE_FILE (newname);

  if (NILP (ok_if_already_exists)
      || INTEGERP (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, "make it a new name",
				  INTEGERP (ok_if_already_exists), 0, 0);

  unlink (SSDATA (newname));
  if (0 > link (SSDATA (encoded_file), SSDATA (encoded_newname)))
    report_file_error ("Adding new name", list2 (file, newname));

  UNGCPRO;
  return Qnil;
}

DEFUN ("make-symbolic-link", Fmake_symbolic_link, Smake_symbolic_link, 2, 3,
       "FMake symbolic link to file: \nGMake symbolic link to file %s: \np",
       doc: /* Make a symbolic link to FILENAME, named LINKNAME.
Both args must be strings.
Signals a `file-already-exists' error if a file LINKNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if LINKNAME already exists.
This happens for interactive use with M-x.  */)
  (Lisp_Object filename, Lisp_Object linkname, Lisp_Object ok_if_already_exists)
{
  Lisp_Object handler;
  Lisp_Object encoded_filename, encoded_linkname;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  GCPRO4 (filename, linkname, encoded_filename, encoded_linkname);
  encoded_filename = encoded_linkname = Qnil;
  CHECK_STRING (filename);
  CHECK_STRING (linkname);
  /* If the link target has a ~, we must expand it to get
     a truly valid file name.  Otherwise, do not expand;
     we want to permit links to relative file names.  */
  if (SREF (filename, 0) == '~')
    filename = Fexpand_file_name (filename, Qnil);

  if (!NILP (Ffile_directory_p (linkname)))
    linkname = Fexpand_file_name (Ffile_name_nondirectory (filename), linkname);
  else
    linkname = Fexpand_file_name (linkname, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qmake_symbolic_link);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qmake_symbolic_link, filename,
			   linkname, ok_if_already_exists));

  /* If the new link name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (linkname, Qmake_symbolic_link);
  if (!NILP (handler))
    RETURN_UNGCPRO (call4 (handler, Qmake_symbolic_link, filename,
			   linkname, ok_if_already_exists));

  encoded_filename = ENCODE_FILE (filename);
  encoded_linkname = ENCODE_FILE (linkname);

  if (NILP (ok_if_already_exists)
      || INTEGERP (ok_if_already_exists))
    barf_or_query_if_file_exists (linkname, "make it a link",
				  INTEGERP (ok_if_already_exists), 0, 0);
  if (0 > symlink (SSDATA (encoded_filename),
		   SSDATA (encoded_linkname)))
    {
      /* If we didn't complain already, silently delete existing file.  */
      if (errno == EEXIST)
	{
	  unlink (SSDATA (encoded_linkname));
	  if (0 <= symlink (SSDATA (encoded_filename),
			    SSDATA (encoded_linkname)))
	    {
	      UNGCPRO;
	      return Qnil;
	    }
	}
      if (errno == ENOSYS)
	{
	  UNGCPRO;
	  xsignal1 (Qfile_error,
		    build_string ("Symbolic links are not supported"));
	}

      report_file_error ("Making symbolic link", list2 (filename, linkname));
    }
  UNGCPRO;
  return Qnil;
}


DEFUN ("file-name-absolute-p", Ffile_name_absolute_p, Sfile_name_absolute_p,
       1, 1, 0,
       doc: /* Return t if file FILENAME specifies an absolute file name.
On Unix, this is a name starting with a `/' or a `~'.  */)
  (Lisp_Object filename)
{
  CHECK_STRING (filename);
  return file_name_absolute_p (SSDATA (filename)) ? Qt : Qnil;
}

/* Return nonzero if file FILENAME exists and can be executed.  */

static int
check_executable (char *filename)
{
#ifdef DOS_NT
  struct stat st;
  if (stat (filename, &st) < 0)
    return 0;
  return ((st.st_mode & S_IEXEC) != 0);
#else /* not DOS_NT */
#ifdef HAVE_EUIDACCESS
  return (euidaccess (filename, 1) >= 0);
#else
  /* Access isn't quite right because it uses the real uid
     and we really want to test with the effective uid.
     But Unix doesn't give us a right way to do it.  */
  return (access (filename, 1) >= 0);
#endif
#endif /* not DOS_NT */
}

/* Return nonzero if file FILENAME exists and can be written.  */

static int
check_writable (const char *filename)
{
#ifdef MSDOS
  struct stat st;
  if (stat (filename, &st) < 0)
    return 0;
  return (st.st_mode & S_IWRITE || S_ISDIR (st.st_mode));
#else /* not MSDOS */
#ifdef HAVE_EUIDACCESS
  int res = (euidaccess (filename, 2) >= 0);
#ifdef CYGWIN
  /* euidaccess may have returned failure because Cygwin couldn't
     determine the file's UID or GID; if so, we return success. */
  if (!res)
    {
      struct stat st;
      if (stat (filename, &st) < 0)
        return 0;
      res = (st.st_uid == -1 || st.st_gid == -1);
    }
#endif /* CYGWIN */
  return res;
#else /* not HAVE_EUIDACCESS */
  /* Access isn't quite right because it uses the real uid
     and we really want to test with the effective uid.
     But Unix doesn't give us a right way to do it.
     Opening with O_WRONLY could work for an ordinary file,
     but would lose for directories.  */
  return (access (filename, 2) >= 0);
#endif /* not HAVE_EUIDACCESS */
#endif /* not MSDOS */
}

DEFUN ("file-exists-p", Ffile_exists_p, Sfile_exists_p, 1, 1, 0,
       doc: /* Return t if file FILENAME exists (whether or not you can read it.)
See also `file-readable-p' and `file-attributes'.
This returns nil for a symlink to a nonexistent file.
Use `file-symlink-p' to test for such links.  */)
  (Lisp_Object filename)
{
  Lisp_Object absname;
  Lisp_Object handler;
  struct stat statbuf;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_exists_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_exists_p, absname);

  absname = ENCODE_FILE (absname);

  return (stat (SSDATA (absname), &statbuf) >= 0) ? Qt : Qnil;
}

DEFUN ("file-executable-p", Ffile_executable_p, Sfile_executable_p, 1, 1, 0,
       doc: /* Return t if FILENAME can be executed by you.
For a directory, this means you can access files in that directory.  */)
  (Lisp_Object filename)
{
  Lisp_Object absname;
  Lisp_Object handler;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_executable_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_executable_p, absname);

  absname = ENCODE_FILE (absname);

  return (check_executable (SSDATA (absname)) ? Qt : Qnil);
}

DEFUN ("file-readable-p", Ffile_readable_p, Sfile_readable_p, 1, 1, 0,
       doc: /* Return t if file FILENAME exists and you can read it.
See also `file-exists-p' and `file-attributes'.  */)
  (Lisp_Object filename)
{
  Lisp_Object absname;
  Lisp_Object handler;
  int desc;
  int flags;
  struct stat statbuf;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_readable_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_readable_p, absname);

  absname = ENCODE_FILE (absname);

#if defined (DOS_NT) || defined (macintosh)
  /* Under MS-DOS, Windows, and Macintosh, open does not work for
     directories.  */
  if (access (SDATA (absname), 0) == 0)
    return Qt;
  return Qnil;
#else /* not DOS_NT and not macintosh */
  flags = O_RDONLY;
#ifdef O_NONBLOCK
  /* Opening a fifo without O_NONBLOCK can wait.
     We don't want to wait.  But we don't want to mess wth O_NONBLOCK
     except in the case of a fifo, on a system which handles it.  */
  desc = stat (SSDATA (absname), &statbuf);
  if (desc < 0)
    return Qnil;
  if (S_ISFIFO (statbuf.st_mode))
    flags |= O_NONBLOCK;
#endif
  desc = emacs_open (SSDATA (absname), flags, 0);
  if (desc < 0)
    return Qnil;
  emacs_close (desc);
  return Qt;
#endif /* not DOS_NT and not macintosh */
}

/* Having this before file-symlink-p mysteriously caused it to be forgotten
   on the RT/PC.  */
DEFUN ("file-writable-p", Ffile_writable_p, Sfile_writable_p, 1, 1, 0,
       doc: /* Return t if file FILENAME can be written or created by you.  */)
  (Lisp_Object filename)
{
  Lisp_Object absname, dir, encoded;
  Lisp_Object handler;
  struct stat statbuf;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_writable_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_writable_p, absname);

  encoded = ENCODE_FILE (absname);
  if (stat (SSDATA (encoded), &statbuf) >= 0)
    return (check_writable (SSDATA (encoded))
	    ? Qt : Qnil);

  dir = Ffile_name_directory (absname);
#ifdef MSDOS
  if (!NILP (dir))
    dir = Fdirectory_file_name (dir);
#endif /* MSDOS */

  dir = ENCODE_FILE (dir);
#ifdef WINDOWSNT
  /* The read-only attribute of the parent directory doesn't affect
     whether a file or directory can be created within it.  Some day we
     should check ACLs though, which do affect this.  */
  if (stat (SDATA (dir), &statbuf) < 0)
    return Qnil;
  return S_ISDIR (statbuf.st_mode) ? Qt : Qnil;
#else
  return (check_writable (!NILP (dir) ? SSDATA (dir) : "")
	  ? Qt : Qnil);
#endif
}

DEFUN ("access-file", Faccess_file, Saccess_file, 2, 2, 0,
       doc: /* Access file FILENAME, and get an error if that does not work.
The second argument STRING is used in the error message.
If there is no error, returns nil.  */)
  (Lisp_Object filename, Lisp_Object string)
{
  Lisp_Object handler, encoded_filename, absname;
  int fd;

  CHECK_STRING (filename);
  absname = Fexpand_file_name (filename, Qnil);

  CHECK_STRING (string);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qaccess_file);
  if (!NILP (handler))
    return call3 (handler, Qaccess_file, absname, string);

  encoded_filename = ENCODE_FILE (absname);

  fd = emacs_open (SSDATA (encoded_filename), O_RDONLY, 0);
  if (fd < 0)
    report_file_error (SSDATA (string), Fcons (filename, Qnil));
  emacs_close (fd);

  return Qnil;
}

DEFUN ("file-symlink-p", Ffile_symlink_p, Sfile_symlink_p, 1, 1, 0,
       doc: /* Return non-nil if file FILENAME is the name of a symbolic link.
The value is the link target, as a string.
Otherwise it returns nil.

This function returns t when given the name of a symlink that
points to a nonexistent file.  */)
  (Lisp_Object filename)
{
  Lisp_Object handler;
  char *buf;
  Lisp_Object val;
  char readlink_buf[READLINK_BUFSIZE];

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_symlink_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_symlink_p, filename);

  filename = ENCODE_FILE (filename);

  buf = emacs_readlink (SSDATA (filename), readlink_buf);
  if (! buf)
    return Qnil;

  val = build_string (buf);
  if (buf[0] == '/' && strchr (buf, ':'))
    val = concat2 (build_string ("/:"), val);
  if (buf != readlink_buf)
    xfree (buf);
  val = DECODE_FILE (val);
  return val;
}

DEFUN ("file-directory-p", Ffile_directory_p, Sfile_directory_p, 1, 1, 0,
       doc: /* Return t if FILENAME names an existing directory.
Symbolic links to directories count as directories.
See `file-symlink-p' to distinguish symlinks.  */)
  (Lisp_Object filename)
{
  register Lisp_Object absname;
  struct stat st;
  Lisp_Object handler;

  absname = expand_and_dir_to_file (filename, BVAR (current_buffer, directory));

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_directory_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_directory_p, absname);

  absname = ENCODE_FILE (absname);

  if (stat (SSDATA (absname), &st) < 0)
    return Qnil;
  return S_ISDIR (st.st_mode) ? Qt : Qnil;
}

DEFUN ("file-accessible-directory-p", Ffile_accessible_directory_p,
       Sfile_accessible_directory_p, 1, 1, 0,
       doc: /* Return t if file FILENAME names a directory you can open.
For the value to be t, FILENAME must specify the name of a directory as a file,
and the directory must allow you to open files in it.  In order to use a
directory as a buffer's current directory, this predicate must return true.
A directory name spec may be given instead; then the value is t
if the directory so specified exists and really is a readable and
searchable directory.  */)
  (Lisp_Object filename)
{
  Lisp_Object handler;
  int tem;
  struct gcpro gcpro1;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qfile_accessible_directory_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_accessible_directory_p, filename);

  GCPRO1 (filename);
  tem = (NILP (Ffile_directory_p (filename))
	 || NILP (Ffile_executable_p (filename)));
  UNGCPRO;
  return tem ? Qnil : Qt;
}

DEFUN ("file-regular-p", Ffile_regular_p, Sfile_regular_p, 1, 1, 0,
       doc: /* Return t if FILENAME names a regular file.
This is the sort of file that holds an ordinary stream of data bytes.
Symbolic links to regular files count as regular files.
See `file-symlink-p' to distinguish symlinks.  */)
  (Lisp_Object filename)
{
  register Lisp_Object absname;
  struct stat st;
  Lisp_Object handler;

  absname = expand_and_dir_to_file (filename, BVAR (current_buffer, directory));

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_regular_p);
  if (!NILP (handler))
    return call2 (handler, Qfile_regular_p, absname);

  absname = ENCODE_FILE (absname);

#ifdef WINDOWSNT
  {
    int result;
    Lisp_Object tem = Vw32_get_true_file_attributes;

    /* Tell stat to use expensive method to get accurate info.  */
    Vw32_get_true_file_attributes = Qt;
    result = stat (SDATA (absname), &st);
    Vw32_get_true_file_attributes = tem;

    if (result < 0)
      return Qnil;
    return S_ISREG (st.st_mode) ? Qt : Qnil;
  }
#else
  if (stat (SSDATA (absname), &st) < 0)
    return Qnil;
  return S_ISREG (st.st_mode) ? Qt : Qnil;
#endif
}

DEFUN ("file-selinux-context", Ffile_selinux_context,
       Sfile_selinux_context, 1, 1, 0,
       doc: /* Return SELinux context of file named FILENAME.
The return value is a list (USER ROLE TYPE RANGE), where the list
elements are strings naming the user, role, type, and range of the
file's SELinux security context.

Return (nil nil nil nil) if the file is nonexistent or inaccessible,
or if SELinux is disabled, or if Emacs lacks SELinux support.  */)
  (Lisp_Object filename)
{
  Lisp_Object absname;
  Lisp_Object values[4];
  Lisp_Object handler;
#if HAVE_LIBSELINUX
  security_context_t con;
  int conlength;
  context_t context;
#endif

  absname = expand_and_dir_to_file (filename, BVAR (current_buffer, directory));

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_selinux_context);
  if (!NILP (handler))
    return call2 (handler, Qfile_selinux_context, absname);

  absname = ENCODE_FILE (absname);

  values[0] = Qnil;
  values[1] = Qnil;
  values[2] = Qnil;
  values[3] = Qnil;
#if HAVE_LIBSELINUX
  if (is_selinux_enabled ())
    {
      conlength = lgetfilecon (SSDATA (absname), &con);
      if (conlength > 0)
	{
	  context = context_new (con);
	  if (context_user_get (context))
	    values[0] = build_string (context_user_get (context));
	  if (context_role_get (context))
	    values[1] = build_string (context_role_get (context));
	  if (context_type_get (context))
	    values[2] = build_string (context_type_get (context));
	  if (context_range_get (context))
	    values[3] = build_string (context_range_get (context));
	  context_free (context);
	}
      if (con)
	freecon (con);
    }
#endif

  return Flist (sizeof (values) / sizeof (values[0]), values);
}

DEFUN ("set-file-selinux-context", Fset_file_selinux_context,
       Sset_file_selinux_context, 2, 2, 0,
       doc: /* Set SELinux context of file named FILENAME to CONTEXT.
CONTEXT should be a list (USER ROLE TYPE RANGE), where the list
elements are strings naming the components of a SELinux context.

This function does nothing if SELinux is disabled, or if Emacs was not
compiled with SELinux support.  */)
  (Lisp_Object filename, Lisp_Object context)
{
  Lisp_Object absname;
  Lisp_Object handler;
#if HAVE_LIBSELINUX
  Lisp_Object encoded_absname;
  Lisp_Object user = CAR_SAFE (context);
  Lisp_Object role = CAR_SAFE (CDR_SAFE (context));
  Lisp_Object type = CAR_SAFE (CDR_SAFE (CDR_SAFE (context)));
  Lisp_Object range = CAR_SAFE (CDR_SAFE (CDR_SAFE (CDR_SAFE (context))));
  security_context_t con;
  int fail, conlength;
  context_t parsed_con;
#endif

  absname = Fexpand_file_name (filename, BVAR (current_buffer, directory));

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qset_file_selinux_context);
  if (!NILP (handler))
    return call3 (handler, Qset_file_selinux_context, absname, context);

#if HAVE_LIBSELINUX
  if (is_selinux_enabled ())
    {
      /* Get current file context. */
      encoded_absname = ENCODE_FILE (absname);
      conlength = lgetfilecon (SSDATA (encoded_absname), &con);
      if (conlength > 0)
	{
	  parsed_con = context_new (con);
	  /* Change the parts defined in the parameter.*/
	  if (STRINGP (user))
	    {
	      if (context_user_set (parsed_con, SSDATA (user)))
		error ("Doing context_user_set");
	    }
	  if (STRINGP (role))
	    {
	      if (context_role_set (parsed_con, SSDATA (role)))
		error ("Doing context_role_set");
	    }
	  if (STRINGP (type))
	    {
	      if (context_type_set (parsed_con, SSDATA (type)))
		error ("Doing context_type_set");
	    }
	  if (STRINGP (range))
	    {
	      if (context_range_set (parsed_con, SSDATA (range)))
		error ("Doing context_range_set");
	    }

	  /* Set the modified context back to the file.  */
	  fail = lsetfilecon (SSDATA (encoded_absname),
			      context_str (parsed_con));
          /* See http://debbugs.gnu.org/11245 for ENOTSUP.  */
	  if (fail && errno != ENOTSUP)
	    report_file_error ("Doing lsetfilecon", Fcons (absname, Qnil));

	  context_free (parsed_con);
	}
      else
	report_file_error ("Doing lgetfilecon", Fcons (absname, Qnil));

      if (con)
	freecon (con);
    }
#endif

  return Qnil;
}

DEFUN ("file-modes", Ffile_modes, Sfile_modes, 1, 1, 0,
       doc: /* Return mode bits of file named FILENAME, as an integer.
Return nil, if file does not exist or is not accessible.  */)
  (Lisp_Object filename)
{
  Lisp_Object absname;
  struct stat st;
  Lisp_Object handler;

  absname = expand_and_dir_to_file (filename, BVAR (current_buffer, directory));

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qfile_modes);
  if (!NILP (handler))
    return call2 (handler, Qfile_modes, absname);

  absname = ENCODE_FILE (absname);

  if (stat (SSDATA (absname), &st) < 0)
    return Qnil;

  return make_number (st.st_mode & 07777);
}

DEFUN ("set-file-modes", Fset_file_modes, Sset_file_modes, 2, 2,
       "(let ((file (read-file-name \"File: \")))			\
	  (list file (read-file-modes nil file)))",
       doc: /* Set mode bits of file named FILENAME to MODE (an integer).
Only the 12 low bits of MODE are used.

Interactively, mode bits are read by `read-file-modes', which accepts
symbolic notation, like the `chmod' command from GNU Coreutils.  */)
  (Lisp_Object filename, Lisp_Object mode)
{
  Lisp_Object absname, encoded_absname;
  Lisp_Object handler;

  absname = Fexpand_file_name (filename, BVAR (current_buffer, directory));
  CHECK_NUMBER (mode);

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qset_file_modes);
  if (!NILP (handler))
    return call3 (handler, Qset_file_modes, absname, mode);

  encoded_absname = ENCODE_FILE (absname);

  if (chmod (SSDATA (encoded_absname), XINT (mode) & 07777) < 0)
    report_file_error ("Doing chmod", Fcons (absname, Qnil));

  return Qnil;
}

DEFUN ("set-default-file-modes", Fset_default_file_modes, Sset_default_file_modes, 1, 1, 0,
       doc: /* Set the file permission bits for newly created files.
The argument MODE should be an integer; only the low 9 bits are used.
This setting is inherited by subprocesses.  */)
  (Lisp_Object mode)
{
  CHECK_NUMBER (mode);

  umask ((~ XINT (mode)) & 0777);

  return Qnil;
}

DEFUN ("default-file-modes", Fdefault_file_modes, Sdefault_file_modes, 0, 0, 0,
       doc: /* Return the default file protection for created files.
The value is an integer.  */)
  (void)
{
  int realmask;
  Lisp_Object value;

  realmask = umask (0);
  umask (realmask);

  XSETINT (value, (~ realmask) & 0777);
  return value;
}


DEFUN ("set-file-times", Fset_file_times, Sset_file_times, 1, 2, 0,
       doc: /* Set times of file FILENAME to TIMESTAMP.
Set both access and modification times.
Return t on success, else nil.
Use the current time if TIMESTAMP is nil.  TIMESTAMP is in the format of
`current-time'. */)
  (Lisp_Object filename, Lisp_Object timestamp)
{
  Lisp_Object absname, encoded_absname;
  Lisp_Object handler;
  time_t sec;
  int usec;

  if (! lisp_time_argument (timestamp, &sec, &usec))
    error ("Invalid time specification");

  absname = Fexpand_file_name (filename, BVAR (current_buffer, directory));

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname, Qset_file_times);
  if (!NILP (handler))
    return call3 (handler, Qset_file_times, absname, timestamp);

  encoded_absname = ENCODE_FILE (absname);

  {
    EMACS_TIME t;

    EMACS_SET_SECS (t, sec);
    EMACS_SET_USECS (t, usec);

    if (set_file_times (SSDATA (encoded_absname), t, t))
      {
#ifdef DOS_NT
        struct stat st;

        /* Setting times on a directory always fails.  */
        if (stat (SSDATA (encoded_absname), &st) == 0 && S_ISDIR (st.st_mode))
          return Qnil;
#endif
        report_file_error ("Setting file times", Fcons (absname, Qnil));
        return Qnil;
      }
  }

  return Qt;
}

#ifdef HAVE_SYNC
DEFUN ("unix-sync", Funix_sync, Sunix_sync, 0, 0, "",
       doc: /* Tell Unix to finish all pending disk updates.  */)
  (void)
{
  sync ();
  return Qnil;
}

#endif /* HAVE_SYNC */

DEFUN ("file-newer-than-file-p", Ffile_newer_than_file_p, Sfile_newer_than_file_p, 2, 2, 0,
       doc: /* Return t if file FILE1 is newer than file FILE2.
If FILE1 does not exist, the answer is nil;
otherwise, if FILE2 does not exist, the answer is t.  */)
  (Lisp_Object file1, Lisp_Object file2)
{
  Lisp_Object absname1, absname2;
  struct stat st;
  int mtime1;
  Lisp_Object handler;
  struct gcpro gcpro1, gcpro2;

  CHECK_STRING (file1);
  CHECK_STRING (file2);

  absname1 = Qnil;
  GCPRO2 (absname1, file2);
  absname1 = expand_and_dir_to_file (file1, BVAR (current_buffer, directory));
  absname2 = expand_and_dir_to_file (file2, BVAR (current_buffer, directory));
  UNGCPRO;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (absname1, Qfile_newer_than_file_p);
  if (NILP (handler))
    handler = Ffind_file_name_handler (absname2, Qfile_newer_than_file_p);
  if (!NILP (handler))
    return call3 (handler, Qfile_newer_than_file_p, absname1, absname2);

  GCPRO2 (absname1, absname2);
  absname1 = ENCODE_FILE (absname1);
  absname2 = ENCODE_FILE (absname2);
  UNGCPRO;

  if (stat (SSDATA (absname1), &st) < 0)
    return Qnil;

  mtime1 = st.st_mtime;

  if (stat (SSDATA (absname2), &st) < 0)
    return Qt;

  return (mtime1 > st.st_mtime) ? Qt : Qnil;
}

#ifndef READ_BUF_SIZE
#define READ_BUF_SIZE (64 << 10)
#endif

/* This function is called after Lisp functions to decide a coding
   system are called, or when they cause an error.  Before they are
   called, the current buffer is set unibyte and it contains only a
   newly inserted text (thus the buffer was empty before the
   insertion).

   The functions may set markers, overlays, text properties, or even
   alter the buffer contents, change the current buffer.

   Here, we reset all those changes by:
	o set back the current buffer.
	o move all markers and overlays to BEG.
	o remove all text properties.
	o set back the buffer multibyteness.  */

static Lisp_Object
decide_coding_unwind (Lisp_Object unwind_data)
{
  Lisp_Object multibyte, undo_list, buffer;

  multibyte = XCAR (unwind_data);
  unwind_data = XCDR (unwind_data);
  undo_list = XCAR (unwind_data);
  buffer = XCDR (unwind_data);

  if (current_buffer != XBUFFER (buffer))
    set_buffer_internal (XBUFFER (buffer));
  adjust_markers_for_delete (BEG, BEG_BYTE, Z, Z_BYTE);
  adjust_overlays_for_delete (BEG, Z - BEG);
  BUF_INTERVALS (current_buffer) = 0;
  TEMP_SET_PT_BOTH (BEG, BEG_BYTE);

  /* Now we are safe to change the buffer's multibyteness directly.  */
  BVAR (current_buffer, enable_multibyte_characters) = multibyte;
  BVAR (current_buffer, undo_list) = undo_list;

  return Qnil;
}


/* Used to pass values from insert-file-contents to read_non_regular.  */

static int non_regular_fd;
static EMACS_INT non_regular_inserted;
static EMACS_INT non_regular_nbytes;


/* Read from a non-regular file.
   Read non_regular_nbytes bytes max from non_regular_fd.
   Non_regular_inserted specifies where to put the read bytes.
   Value is the number of bytes read.  */

static Lisp_Object
read_non_regular (Lisp_Object ignore)
{
  EMACS_INT nbytes;

  immediate_quit = 1;
  QUIT;
  nbytes = emacs_read (non_regular_fd,
		       ((char *) BEG_ADDR + PT_BYTE - BEG_BYTE
			+ non_regular_inserted),
		       non_regular_nbytes);
  immediate_quit = 0;
  return make_number (nbytes);
}


/* Condition-case handler used when reading from non-regular files
   in insert-file-contents.  */

static Lisp_Object
read_non_regular_quit (Lisp_Object ignore)
{
  return Qnil;
}

/* Reposition FD to OFFSET, based on WHENCE.  This acts like lseek
   except that it also tests for OFFSET being out of lseek's range.  */
static off_t
emacs_lseek (int fd, EMACS_INT offset, int whence)
{
  /* Use "&" rather than "&&" to suppress a bogus GCC warning; see
     <http://gcc.gnu.org/bugzilla/show_bug.cgi?id=43772>.  */
  if (! ((TYPE_MINIMUM (off_t) <= offset) & (offset <= TYPE_MAXIMUM (off_t))))
    {
      errno = EINVAL;
      return -1;
    }
  return lseek (fd, offset, whence);
}


DEFUN ("insert-file-contents", Finsert_file_contents, Sinsert_file_contents,
       1, 5, 0,
       doc: /* Insert contents of file FILENAME after point.
Returns list of absolute file name and number of characters inserted.
If second argument VISIT is non-nil, the buffer's visited filename and
last save file modtime are set, and it is marked unmodified.  If
visiting and the file does not exist, visiting is completed before the
error is signaled.

The optional third and fourth arguments BEG and END specify what portion
of the file to insert.  These arguments count bytes in the file, not
characters in the buffer.  If VISIT is non-nil, BEG and END must be nil.

If optional fifth argument REPLACE is non-nil, replace the current
buffer contents (in the accessible portion) with the file contents.
This is better than simply deleting and inserting the whole thing
because (1) it preserves some marker positions and (2) it puts less data
in the undo list.  When REPLACE is non-nil, the second return value is
the number of characters that replace previous buffer contents.

This function does code conversion according to the value of
`coding-system-for-read' or `file-coding-system-alist', and sets the
variable `last-coding-system-used' to the coding system actually used.  */)
  (Lisp_Object filename, Lisp_Object visit, Lisp_Object beg, Lisp_Object end, Lisp_Object replace)
{
  struct stat st;
  register int fd;
  EMACS_INT inserted = 0;
  int nochange = 0;
  register EMACS_INT how_much;
  off_t beg_offset, end_offset;
  register EMACS_INT unprocessed;
  int count = SPECPDL_INDEX ();
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  Lisp_Object handler, val, insval, orig_filename, old_undo;
  Lisp_Object p;
  EMACS_INT total = 0;
  int not_regular = 0;
  int save_errno = 0;
  char read_buf[READ_BUF_SIZE];
  struct coding_system coding;
  char buffer[1 << 14];
  int replace_handled = 0;
  int set_coding_system = 0;
  Lisp_Object coding_system;
  int read_quit = 0;
  Lisp_Object old_Vdeactivate_mark = Vdeactivate_mark;
  int we_locked_file = 0;
  int deferred_remove_unwind_protect = 0;

  if (current_buffer->base_buffer && ! NILP (visit))
    error ("Cannot do file visiting in an indirect buffer");

  if (!NILP (BVAR (current_buffer, read_only)))
    Fbarf_if_buffer_read_only ();

  val = Qnil;
  p = Qnil;
  orig_filename = Qnil;
  old_undo = Qnil;

  GCPRO5 (filename, val, p, orig_filename, old_undo);

  CHECK_STRING (filename);
  filename = Fexpand_file_name (filename, Qnil);

  /* The value Qnil means that the coding system is not yet
     decided.  */
  coding_system = Qnil;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qinsert_file_contents);
  if (!NILP (handler))
    {
      val = call6 (handler, Qinsert_file_contents, filename,
		   visit, beg, end, replace);
      if (CONSP (val) && CONSP (XCDR (val)))
	inserted = XINT (XCAR (XCDR (val)));
      goto handled;
    }

  orig_filename = filename;
  filename = ENCODE_FILE (filename);

  fd = -1;

#ifdef WINDOWSNT
  {
    Lisp_Object tem = Vw32_get_true_file_attributes;

    /* Tell stat to use expensive method to get accurate info.  */
    Vw32_get_true_file_attributes = Qt;
    total = stat (SSDATA (filename), &st);
    Vw32_get_true_file_attributes = tem;
  }
  if (total < 0)
#else
  if (stat (SSDATA (filename), &st) < 0)
#endif /* WINDOWSNT */
    {
    badopen:
      save_errno = errno;
      if (NILP (visit))
	report_file_error ("Opening input file", Fcons (orig_filename, Qnil));
      st.st_mtime = -1;
      st.st_size = -1;
      how_much = 0;
      if (!NILP (Vcoding_system_for_read))
	Fset (Qbuffer_file_coding_system, Vcoding_system_for_read);
      goto notfound;
    }

  /* This code will need to be changed in order to work on named
     pipes, and it's probably just not worth it.  So we should at
     least signal an error.  */
  if (!S_ISREG (st.st_mode))
    {
      not_regular = 1;

      if (! NILP (visit))
	goto notfound;

      if (! NILP (replace) || ! NILP (beg) || ! NILP (end))
	xsignal2 (Qfile_error,
		  build_string ("not a regular file"), orig_filename);
    }

  if (fd < 0)
    if ((fd = emacs_open (SSDATA (filename), O_RDONLY, 0)) < 0)
      goto badopen;

  /* Replacement should preserve point as it preserves markers.  */
  if (!NILP (replace))
    record_unwind_protect (restore_point_unwind, Fpoint_marker ());

  record_unwind_protect (close_file_unwind, make_number (fd));


  if (!NILP (visit))
    {
      if (!NILP (beg) || !NILP (end))
	error ("Attempt to visit less than an entire file");
      if (BEG < Z && NILP (replace))
	error ("Cannot do file visiting in a non-empty buffer");
    }

  if (!NILP (beg))
    {
      if (! (RANGED_INTEGERP (0, beg, TYPE_MAXIMUM (off_t))))
	wrong_type_argument (intern ("file-offset"), beg);
      beg_offset = XFASTINT (beg);
    }
  else
    beg_offset = 0;

  if (!NILP (end))
    {
      if (! (RANGED_INTEGERP (0, end, TYPE_MAXIMUM (off_t))))
	wrong_type_argument (intern ("file-offset"), end);
      end_offset = XFASTINT (end);
    }
  else
    {
      if (not_regular)
	end_offset = TYPE_MAXIMUM (off_t);
      else
	{
	  end_offset = st.st_size;

	  /* A negative size can happen on a platform that allows file
	     sizes greater than the maximum off_t value.  */
	  if (end_offset < 0)
	    buffer_overflow ();

	  /* The file size returned from stat may be zero, but data
	     may be readable nonetheless, for example when this is a
	     file in the /proc filesystem.  */
	  if (end_offset == 0)
	    end_offset = READ_BUF_SIZE;
	}
    }

  /* Check now whether the buffer will become too large,
     in the likely case where the file's length is not changing.
     This saves a lot of needless work before a buffer overflow.  */
  if (! not_regular)
    {
      /* The likely offset where we will stop reading.  We could read
	 more (or less), if the file grows (or shrinks) as we read it.  */
      off_t likely_end = min (end_offset, st.st_size);

      if (beg_offset < likely_end)
	{
	  ptrdiff_t buf_bytes =
	    Z_BYTE - (!NILP (replace) ? ZV_BYTE - BEGV_BYTE  : 0);
	  ptrdiff_t buf_growth_max = BUF_BYTES_MAX - buf_bytes;
	  off_t likely_growth = likely_end - beg_offset;
	  if (buf_growth_max < likely_growth)
	    buffer_overflow ();
	}
    }

  /* Prevent redisplay optimizations.  */
  current_buffer->clip_changed = 1;

  if (EQ (Vcoding_system_for_read, Qauto_save_coding))
    {
      coding_system = coding_inherit_eol_type (Qutf_8_emacs, Qunix);
      setup_coding_system (coding_system, &coding);
      /* Ensure we set Vlast_coding_system_used.  */
      set_coding_system = 1;
    }
  else if (BEG < Z)
    {
      /* Decide the coding system to use for reading the file now
         because we can't use an optimized method for handling
         `coding:' tag if the current buffer is not empty.  */
      if (!NILP (Vcoding_system_for_read))
	coding_system = Vcoding_system_for_read;
      else
	{
	  /* Don't try looking inside a file for a coding system
	     specification if it is not seekable.  */
	  if (! not_regular && ! NILP (Vset_auto_coding_function))
	    {
	      /* Find a coding system specified in the heading two
		 lines or in the tailing several lines of the file.
		 We assume that the 1K-byte and 3K-byte for heading
		 and tailing respectively are sufficient for this
		 purpose.  */
	      EMACS_INT nread;

	      if (st.st_size <= (1024 * 4))
		nread = emacs_read (fd, read_buf, 1024 * 4);
	      else
		{
		  nread = emacs_read (fd, read_buf, 1024);
		  if (nread >= 0)
		    {
		      if (lseek (fd, st.st_size - (1024 * 3), SEEK_SET) < 0)
			report_file_error ("Setting file position",
					   Fcons (orig_filename, Qnil));
		      nread += emacs_read (fd, read_buf + nread, 1024 * 3);
		    }
		}

	      if (nread < 0)
		error ("IO error reading %s: %s",
		       SDATA (orig_filename), emacs_strerror (errno));
	      else if (nread > 0)
		{
		  struct buffer *prev = current_buffer;
		  Lisp_Object workbuf;
		  struct buffer *buf;

		  record_unwind_protect (Fset_buffer, Fcurrent_buffer ());

		  workbuf = Fget_buffer_create (build_string (" *code-converting-work*"));
		  buf = XBUFFER (workbuf);

		  delete_all_overlays (buf);
		  BVAR (buf, directory) = BVAR (current_buffer, directory);
		  BVAR (buf, read_only) = Qnil;
		  BVAR (buf, filename) = Qnil;
		  BVAR (buf, undo_list) = Qt;
		  eassert (buf->overlays_before == NULL);
		  eassert (buf->overlays_after == NULL);

		  set_buffer_internal (buf);
		  Ferase_buffer ();
		  BVAR (buf, enable_multibyte_characters) = Qnil;

		  insert_1_both ((char *) read_buf, nread, nread, 0, 0, 0);
		  TEMP_SET_PT_BOTH (BEG, BEG_BYTE);
		  coding_system = call2 (Vset_auto_coding_function,
					 filename, make_number (nread));
		  set_buffer_internal (prev);

		  /* Discard the unwind protect for recovering the
                     current buffer.  */
		  specpdl_ptr--;

		  /* Rewind the file for the actual read done later.  */
		  if (lseek (fd, 0, SEEK_SET) < 0)
		    report_file_error ("Setting file position",
				       Fcons (orig_filename, Qnil));
		}
	    }

	  if (NILP (coding_system))
	    {
	      /* If we have not yet decided a coding system, check
                 file-coding-system-alist.  */
	      Lisp_Object args[6];

	      args[0] = Qinsert_file_contents, args[1] = orig_filename;
	      args[2] = visit, args[3] = beg, args[4] = end, args[5] = replace;
	      coding_system = Ffind_operation_coding_system (6, args);
	      if (CONSP (coding_system))
		coding_system = XCAR (coding_system);
	    }
	}

      if (NILP (coding_system))
	coding_system = Qundecided;
      else
	CHECK_CODING_SYSTEM (coding_system);

      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	/* We must suppress all character code conversion except for
	   end-of-line conversion.  */
	coding_system = raw_text_coding_system (coding_system);

      setup_coding_system (coding_system, &coding);
      /* Ensure we set Vlast_coding_system_used.  */
      set_coding_system = 1;
    }

  /* If requested, replace the accessible part of the buffer
     with the file contents.  Avoid replacing text at the
     beginning or end of the buffer that matches the file contents;
     that preserves markers pointing to the unchanged parts.

     Here we implement this feature in an optimized way
     for the case where code conversion is NOT needed.
     The following if-statement handles the case of conversion
     in a less optimal way.

     If the code conversion is "automatic" then we try using this
     method and hope for the best.
     But if we discover the need for conversion, we give up on this method
     and let the following if-statement handle the replace job.  */
  if (!NILP (replace)
      && BEGV < ZV
      && (NILP (coding_system)
	  || ! CODING_REQUIRE_DECODING (&coding)))
    {
      /* same_at_start and same_at_end count bytes,
	 because file access counts bytes
	 and BEG and END count bytes.  */
      EMACS_INT same_at_start = BEGV_BYTE;
      EMACS_INT same_at_end = ZV_BYTE;
      EMACS_INT overlap;
      /* There is still a possibility we will find the need to do code
	 conversion.  If that happens, we set this variable to 1 to
	 give up on handling REPLACE in the optimized way.  */
      int giveup_match_end = 0;

      if (beg_offset != 0)
	{
	  if (lseek (fd, beg_offset, SEEK_SET) < 0)
	    report_file_error ("Setting file position",
			       Fcons (orig_filename, Qnil));
	}

      immediate_quit = 1;
      QUIT;
      /* Count how many chars at the start of the file
	 match the text at the beginning of the buffer.  */
      while (1)
	{
	  EMACS_INT nread, bufpos;

	  nread = emacs_read (fd, buffer, sizeof buffer);
	  if (nread < 0)
	    error ("IO error reading %s: %s",
		   SSDATA (orig_filename), emacs_strerror (errno));
	  else if (nread == 0)
	    break;

	  if (CODING_REQUIRE_DETECTION (&coding))
	    {
	      coding_system = detect_coding_system ((unsigned char *) buffer,
						    nread, nread, 1, 0,
						    coding_system);
	      setup_coding_system (coding_system, &coding);
	    }

	  if (CODING_REQUIRE_DECODING (&coding))
	    /* We found that the file should be decoded somehow.
               Let's give up here.  */
	    {
	      giveup_match_end = 1;
	      break;
	    }

	  bufpos = 0;
	  while (bufpos < nread && same_at_start < ZV_BYTE
		 && FETCH_BYTE (same_at_start) == buffer[bufpos])
	    same_at_start++, bufpos++;
	  /* If we found a discrepancy, stop the scan.
	     Otherwise loop around and scan the next bufferful.  */
	  if (bufpos != nread)
	    break;
	}
      immediate_quit = 0;
      /* If the file matches the buffer completely,
	 there's no need to replace anything.  */
      if (same_at_start - BEGV_BYTE == end_offset - beg_offset)
	{
	  emacs_close (fd);
	  specpdl_ptr--;
	  /* Truncate the buffer to the size of the file.  */
	  del_range_1 (same_at_start, same_at_end, 0, 0);
	  goto handled;
	}
      immediate_quit = 1;
      QUIT;
      /* Count how many chars at the end of the file
	 match the text at the end of the buffer.  But, if we have
	 already found that decoding is necessary, don't waste time.  */
      while (!giveup_match_end)
	{
	  int total_read, nread, bufpos, trial;
	  off_t curpos;

	  /* At what file position are we now scanning?  */
	  curpos = end_offset - (ZV_BYTE - same_at_end);
	  /* If the entire file matches the buffer tail, stop the scan.  */
	  if (curpos == 0)
	    break;
	  /* How much can we scan in the next step?  */
	  trial = min (curpos, sizeof buffer);
	  if (lseek (fd, curpos - trial, SEEK_SET) < 0)
	    report_file_error ("Setting file position",
			       Fcons (orig_filename, Qnil));

	  total_read = nread = 0;
	  while (total_read < trial)
	    {
	      nread = emacs_read (fd, buffer + total_read, trial - total_read);
	      if (nread < 0)
		error ("IO error reading %s: %s",
		       SDATA (orig_filename), emacs_strerror (errno));
	      else if (nread == 0)
		break;
	      total_read += nread;
	    }

	  /* Scan this bufferful from the end, comparing with
	     the Emacs buffer.  */
	  bufpos = total_read;

	  /* Compare with same_at_start to avoid counting some buffer text
	     as matching both at the file's beginning and at the end.  */
	  while (bufpos > 0 && same_at_end > same_at_start
		 && FETCH_BYTE (same_at_end - 1) == buffer[bufpos - 1])
	    same_at_end--, bufpos--;

	  /* If we found a discrepancy, stop the scan.
	     Otherwise loop around and scan the preceding bufferful.  */
	  if (bufpos != 0)
	    {
	      /* If this discrepancy is because of code conversion,
		 we cannot use this method; giveup and try the other.  */
	      if (same_at_end > same_at_start
		  && FETCH_BYTE (same_at_end - 1) >= 0200
		  && ! NILP (BVAR (current_buffer, enable_multibyte_characters))
		  && (CODING_MAY_REQUIRE_DECODING (&coding)))
		giveup_match_end = 1;
	      break;
	    }

	  if (nread == 0)
	    break;
	}
      immediate_quit = 0;

      if (! giveup_match_end)
	{
	  EMACS_INT temp;

	  /* We win!  We can handle REPLACE the optimized way.  */

	  /* Extend the start of non-matching text area to multibyte
             character boundary.  */
	  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	    while (same_at_start > BEGV_BYTE
		   && ! CHAR_HEAD_P (FETCH_BYTE (same_at_start)))
	      same_at_start--;

	  /* Extend the end of non-matching text area to multibyte
             character boundary.  */
	  if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	    while (same_at_end < ZV_BYTE
		   && ! CHAR_HEAD_P (FETCH_BYTE (same_at_end)))
	      same_at_end++;

	  /* Don't try to reuse the same piece of text twice.  */
	  overlap = (same_at_start - BEGV_BYTE
		     - (same_at_end
			+ (! NILP (end) ? end_offset : st.st_size) - ZV_BYTE));
	  if (overlap > 0)
	    same_at_end += overlap;

	  /* Arrange to read only the nonmatching middle part of the file.  */
	  beg_offset += same_at_start - BEGV_BYTE;
	  end_offset -= ZV_BYTE - same_at_end;

	  del_range_byte (same_at_start, same_at_end, 0);
	  /* Insert from the file at the proper position.  */
	  temp = BYTE_TO_CHAR (same_at_start);
	  SET_PT_BOTH (temp, same_at_start);

	  /* If display currently starts at beginning of line,
	     keep it that way.  */
	  if (XBUFFER (XWINDOW (selected_window)->buffer) == current_buffer)
	    XWINDOW (selected_window)->start_at_line_beg = Fbolp ();

	  replace_handled = 1;
	}
    }

  /* If requested, replace the accessible part of the buffer
     with the file contents.  Avoid replacing text at the
     beginning or end of the buffer that matches the file contents;
     that preserves markers pointing to the unchanged parts.

     Here we implement this feature for the case where code conversion
     is needed, in a simple way that needs a lot of memory.
     The preceding if-statement handles the case of no conversion
     in a more optimized way.  */
  if (!NILP (replace) && ! replace_handled && BEGV < ZV)
    {
      EMACS_INT same_at_start = BEGV_BYTE;
      EMACS_INT same_at_end = ZV_BYTE;
      EMACS_INT same_at_start_charpos;
      EMACS_INT inserted_chars;
      EMACS_INT overlap;
      EMACS_INT bufpos;
      unsigned char *decoded;
      EMACS_INT temp;
      EMACS_INT this = 0;
      int this_count = SPECPDL_INDEX ();
      int multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));
      Lisp_Object conversion_buffer;
      struct gcpro gcpro1;

      conversion_buffer = code_conversion_save (1, multibyte);

      /* First read the whole file, performing code conversion into
	 CONVERSION_BUFFER.  */

      if (lseek (fd, beg_offset, SEEK_SET) < 0)
	report_file_error ("Setting file position",
			   Fcons (orig_filename, Qnil));

      total = st.st_size;	/* Total bytes in the file.  */
      how_much = 0;		/* Bytes read from file so far.  */
      inserted = 0;		/* Bytes put into CONVERSION_BUFFER so far.  */
      unprocessed = 0;		/* Bytes not processed in previous loop.  */

      GCPRO1 (conversion_buffer);
      while (how_much < total)
	{
	  /* We read one bunch by one (READ_BUF_SIZE bytes) to allow
	     quitting while reading a huge while.  */
	  /* `try'' is reserved in some compilers (Microsoft C).  */
	  EMACS_INT trytry = min (total - how_much,
				  READ_BUF_SIZE - unprocessed);

	  /* Allow quitting out of the actual I/O.  */
	  immediate_quit = 1;
	  QUIT;
	  this = emacs_read (fd, read_buf + unprocessed, trytry);
	  immediate_quit = 0;

	  if (this <= 0)
	    break;

	  how_much += this;

	  BUF_TEMP_SET_PT (XBUFFER (conversion_buffer),
			   BUF_Z (XBUFFER (conversion_buffer)));
	  decode_coding_c_string (&coding, (unsigned char *) read_buf,
				  unprocessed + this, conversion_buffer);
	  unprocessed = coding.carryover_bytes;
	  if (coding.carryover_bytes > 0)
	    memcpy (read_buf, coding.carryover, unprocessed);
	}
      UNGCPRO;
      emacs_close (fd);

      /* We should remove the unwind_protect calling
	 close_file_unwind, but other stuff has been added the stack,
	 so defer the removal till we reach the `handled' label.  */
      deferred_remove_unwind_protect = 1;

      /* At this point, HOW_MUCH should equal TOTAL, or should be <= 0
	 if we couldn't read the file.  */

      if (this < 0)
	error ("IO error reading %s: %s",
	       SDATA (orig_filename), emacs_strerror (errno));

      if (unprocessed > 0)
	{
	  coding.mode |= CODING_MODE_LAST_BLOCK;
	  decode_coding_c_string (&coding, (unsigned char *) read_buf,
				  unprocessed, conversion_buffer);
	  coding.mode &= ~CODING_MODE_LAST_BLOCK;
	}

      coding_system = CODING_ID_NAME (coding.id);
      set_coding_system = 1;
      decoded = BUF_BEG_ADDR (XBUFFER (conversion_buffer));
      inserted = (BUF_Z_BYTE (XBUFFER (conversion_buffer))
		  - BUF_BEG_BYTE (XBUFFER (conversion_buffer)));

      /* Compare the beginning of the converted string with the buffer
	 text.  */

      bufpos = 0;
      while (bufpos < inserted && same_at_start < same_at_end
	     && FETCH_BYTE (same_at_start) == decoded[bufpos])
	same_at_start++, bufpos++;

      /* If the file matches the head of buffer completely,
	 there's no need to replace anything.  */

      if (bufpos == inserted)
	{
	  /* Truncate the buffer to the size of the file.  */
	  if (same_at_start == same_at_end)
	    nochange = 1;
	  else
	    del_range_byte (same_at_start, same_at_end, 0);
	  inserted = 0;

	  unbind_to (this_count, Qnil);
	  goto handled;
	}

      /* Extend the start of non-matching text area to the previous
	 multibyte character boundary.  */
      if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	while (same_at_start > BEGV_BYTE
	       && ! CHAR_HEAD_P (FETCH_BYTE (same_at_start)))
	  same_at_start--;

      /* Scan this bufferful from the end, comparing with
	 the Emacs buffer.  */
      bufpos = inserted;

      /* Compare with same_at_start to avoid counting some buffer text
	 as matching both at the file's beginning and at the end.  */
      while (bufpos > 0 && same_at_end > same_at_start
	     && FETCH_BYTE (same_at_end - 1) == decoded[bufpos - 1])
	same_at_end--, bufpos--;

      /* Extend the end of non-matching text area to the next
	 multibyte character boundary.  */
      if (! NILP (BVAR (current_buffer, enable_multibyte_characters)))
	while (same_at_end < ZV_BYTE
	       && ! CHAR_HEAD_P (FETCH_BYTE (same_at_end)))
	  same_at_end++;

      /* Don't try to reuse the same piece of text twice.  */
      overlap = same_at_start - BEGV_BYTE - (same_at_end + inserted - ZV_BYTE);
      if (overlap > 0)
	same_at_end += overlap;

      /* If display currently starts at beginning of line,
	 keep it that way.  */
      if (XBUFFER (XWINDOW (selected_window)->buffer) == current_buffer)
	XWINDOW (selected_window)->start_at_line_beg = Fbolp ();

      /* Replace the chars that we need to replace,
	 and update INSERTED to equal the number of bytes
	 we are taking from the decoded string.  */
      inserted -= (ZV_BYTE - same_at_end) + (same_at_start - BEGV_BYTE);

      if (same_at_end != same_at_start)
	{
	  del_range_byte (same_at_start, same_at_end, 0);
	  temp = GPT;
	  same_at_start = GPT_BYTE;
	}
      else
	{
	  temp = BYTE_TO_CHAR (same_at_start);
	}
      /* Insert from the file at the proper position.  */
      SET_PT_BOTH (temp, same_at_start);
      same_at_start_charpos
	= buf_bytepos_to_charpos (XBUFFER (conversion_buffer),
				  same_at_start - BEGV_BYTE
				  + BUF_BEG_BYTE (XBUFFER (conversion_buffer)));
      inserted_chars
	= (buf_bytepos_to_charpos (XBUFFER (conversion_buffer),
				   same_at_start + inserted - BEGV_BYTE
				  + BUF_BEG_BYTE (XBUFFER (conversion_buffer)))
	   - same_at_start_charpos);
      /* This binding is to avoid ask-user-about-supersession-threat
	 being called in insert_from_buffer (via in
	 prepare_to_modify_buffer).  */
      specbind (intern ("buffer-file-name"), Qnil);
      insert_from_buffer (XBUFFER (conversion_buffer),
			  same_at_start_charpos, inserted_chars, 0);
      /* Set `inserted' to the number of inserted characters.  */
      inserted = PT - temp;
      /* Set point before the inserted characters.  */
      SET_PT_BOTH (temp, same_at_start);

      unbind_to (this_count, Qnil);

      goto handled;
    }

  if (! not_regular)
    total = end_offset - beg_offset;
  else
    /* For a special file, all we can do is guess.  */
    total = READ_BUF_SIZE;

  if (NILP (visit) && total > 0)
    {
#ifdef CLASH_DETECTION
      if (!NILP (BVAR (current_buffer, file_truename))
	  /* Make binding buffer-file-name to nil effective.  */
	  && !NILP (BVAR (current_buffer, filename))
	  && SAVE_MODIFF >= MODIFF)
	we_locked_file = 1;
#endif /* CLASH_DETECTION */
      prepare_to_modify_buffer (GPT, GPT, NULL);
    }

  move_gap (PT);
  if (GAP_SIZE < total)
    make_gap (total - GAP_SIZE);

  if (beg_offset != 0 || !NILP (replace))
    {
      if (lseek (fd, beg_offset, SEEK_SET) < 0)
	report_file_error ("Setting file position",
			   Fcons (orig_filename, Qnil));
    }

  /* In the following loop, HOW_MUCH contains the total bytes read so
     far for a regular file, and not changed for a special file.  But,
     before exiting the loop, it is set to a negative value if I/O
     error occurs.  */
  how_much = 0;

  /* Total bytes inserted.  */
  inserted = 0;

  /* Here, we don't do code conversion in the loop.  It is done by
     decode_coding_gap after all data are read into the buffer.  */
  {
    EMACS_INT gap_size = GAP_SIZE;

    while (how_much < total)
      {
	/* try is reserved in some compilers (Microsoft C) */
	EMACS_INT trytry = min (total - how_much, READ_BUF_SIZE);
	EMACS_INT this;

	if (not_regular)
	  {
	    Lisp_Object nbytes;

	    /* Maybe make more room.  */
	    if (gap_size < trytry)
	      {
		make_gap (total - gap_size);
		gap_size = GAP_SIZE;
	      }

	    /* Read from the file, capturing `quit'.  When an
	       error occurs, end the loop, and arrange for a quit
	       to be signaled after decoding the text we read.  */
	    non_regular_fd = fd;
	    non_regular_inserted = inserted;
	    non_regular_nbytes = trytry;
	    nbytes = internal_condition_case_1 (read_non_regular,
						Qnil, Qerror,
						read_non_regular_quit);
	    if (NILP (nbytes))
	      {
		read_quit = 1;
		break;
	      }

	    this = XINT (nbytes);
	  }
	else
	  {
	    /* Allow quitting out of the actual I/O.  We don't make text
	       part of the buffer until all the reading is done, so a C-g
	       here doesn't do any harm.  */
	    immediate_quit = 1;
	    QUIT;
	    this = emacs_read (fd,
			       ((char *) BEG_ADDR + PT_BYTE - BEG_BYTE
				+ inserted),
			       trytry);
	    immediate_quit = 0;
	  }

	if (this <= 0)
	  {
	    how_much = this;
	    break;
	  }

	gap_size -= this;

	/* For a regular file, where TOTAL is the real size,
	   count HOW_MUCH to compare with it.
	   For a special file, where TOTAL is just a buffer size,
	   so don't bother counting in HOW_MUCH.
	   (INSERTED is where we count the number of characters inserted.)  */
	if (! not_regular)
	  how_much += this;
	inserted += this;
      }
  }

  /* Now we have read all the file data into the gap.
     If it was empty, undo marking the buffer modified.  */

  if (inserted == 0)
    {
#ifdef CLASH_DETECTION
      if (we_locked_file)
	unlock_file (BVAR (current_buffer, file_truename));
#endif
      Vdeactivate_mark = old_Vdeactivate_mark;
    }
  else
    Vdeactivate_mark = Qt;

  /* Make the text read part of the buffer.  */
  GAP_SIZE -= inserted;
  GPT      += inserted;
  GPT_BYTE += inserted;
  ZV       += inserted;
  ZV_BYTE  += inserted;
  Z        += inserted;
  Z_BYTE   += inserted;

  if (GAP_SIZE > 0)
    /* Put an anchor to ensure multi-byte form ends at gap.  */
    *GPT_ADDR = 0;

  emacs_close (fd);

  /* Discard the unwind protect for closing the file.  */
  specpdl_ptr--;

  if (how_much < 0)
    error ("IO error reading %s: %s",
	   SDATA (orig_filename), emacs_strerror (errno));

 notfound:

  if (NILP (coding_system))
    {
      /* The coding system is not yet decided.  Decide it by an
	 optimized method for handling `coding:' tag.

	 Note that we can get here only if the buffer was empty
	 before the insertion.  */

      if (!NILP (Vcoding_system_for_read))
	coding_system = Vcoding_system_for_read;
      else
	{
	  /* Since we are sure that the current buffer was empty
	     before the insertion, we can toggle
	     enable-multibyte-characters directly here without taking
	     care of marker adjustment.  By this way, we can run Lisp
	     program safely before decoding the inserted text.  */
	  Lisp_Object unwind_data;
	  int count1 = SPECPDL_INDEX ();

	  unwind_data = Fcons (BVAR (current_buffer, enable_multibyte_characters),
			       Fcons (BVAR (current_buffer, undo_list),
				      Fcurrent_buffer ()));
	  BVAR (current_buffer, enable_multibyte_characters) = Qnil;
	  BVAR (current_buffer, undo_list) = Qt;
	  record_unwind_protect (decide_coding_unwind, unwind_data);

	  if (inserted > 0 && ! NILP (Vset_auto_coding_function))
	    {
	      coding_system = call2 (Vset_auto_coding_function,
				     filename, make_number (inserted));
	    }

	  if (NILP (coding_system))
	    {
	      /* If the coding system is not yet decided, check
		 file-coding-system-alist.  */
	      Lisp_Object args[6];

	      args[0] = Qinsert_file_contents, args[1] = orig_filename;
	      args[2] = visit, args[3] = beg, args[4] = end, args[5] = Qnil;
	      coding_system = Ffind_operation_coding_system (6, args);
	      if (CONSP (coding_system))
		coding_system = XCAR (coding_system);
	    }
	  unbind_to (count1, Qnil);
	  inserted = Z_BYTE - BEG_BYTE;
	}

      if (NILP (coding_system))
	coding_system = Qundecided;
      else
	CHECK_CODING_SYSTEM (coding_system);

      if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	/* We must suppress all character code conversion except for
	   end-of-line conversion.  */
	coding_system = raw_text_coding_system (coding_system);
      setup_coding_system (coding_system, &coding);
      /* Ensure we set Vlast_coding_system_used.  */
      set_coding_system = 1;
    }

  if (!NILP (visit))
    {
      /* When we visit a file by raw-text, we change the buffer to
	 unibyte.  */
      if (CODING_FOR_UNIBYTE (&coding)
	  /* Can't do this if part of the buffer might be preserved.  */
	  && NILP (replace))
	/* Visiting a file with these coding system makes the buffer
	   unibyte. */
	BVAR (current_buffer, enable_multibyte_characters) = Qnil;
    }

  coding.dst_multibyte = ! NILP (BVAR (current_buffer, enable_multibyte_characters));
  if (CODING_MAY_REQUIRE_DECODING (&coding)
      && (inserted > 0 || CODING_REQUIRE_FLUSHING (&coding)))
    {
      move_gap_both (PT, PT_BYTE);
      GAP_SIZE += inserted;
      ZV_BYTE -= inserted;
      Z_BYTE -= inserted;
      ZV -= inserted;
      Z -= inserted;
      decode_coding_gap (&coding, inserted, inserted);
      inserted = coding.produced_char;
      coding_system = CODING_ID_NAME (coding.id);
    }
  else if (inserted > 0)
    adjust_after_insert (PT, PT_BYTE, PT + inserted, PT_BYTE + inserted,
			 inserted);

  /* Call after-change hooks for the inserted text, aside from the case
     of normal visiting (not with REPLACE), which is done in a new buffer
     "before" the buffer is changed.  */
  if (inserted > 0 && total > 0
      && (NILP (visit) || !NILP (replace)))
    {
      signal_after_change (PT, 0, inserted);
      update_compositions (PT, PT, CHECK_BORDER);
    }

  /* Now INSERTED is measured in characters.  */

 handled:

  if (deferred_remove_unwind_protect)
    /* If requested above, discard the unwind protect for closing the
       file.  */
    specpdl_ptr--;

  if (!NILP (visit))
    {
      if (!EQ (BVAR (current_buffer, undo_list), Qt) && !nochange)
	BVAR (current_buffer, undo_list) = Qnil;

      if (NILP (handler))
	{
	  current_buffer->modtime = st.st_mtime;
	  current_buffer->modtime_size = st.st_size;
	  BVAR (current_buffer, filename) = orig_filename;
	}

      SAVE_MODIFF = MODIFF;
      BUF_AUTOSAVE_MODIFF (current_buffer) = MODIFF;
      XSETFASTINT (BVAR (current_buffer, save_length), Z - BEG);
#ifdef CLASH_DETECTION
      if (NILP (handler))
	{
	  if (!NILP (BVAR (current_buffer, file_truename)))
	    unlock_file (BVAR (current_buffer, file_truename));
	  unlock_file (filename);
	}
#endif /* CLASH_DETECTION */
      if (not_regular)
	xsignal2 (Qfile_error,
		  build_string ("not a regular file"), orig_filename);
    }

  if (set_coding_system)
    Vlast_coding_system_used = coding_system;

  if (! NILP (Ffboundp (Qafter_insert_file_set_coding)))
    {
      insval = call2 (Qafter_insert_file_set_coding, make_number (inserted),
		      visit);
      if (! NILP (insval))
	{
	  CHECK_NUMBER (insval);
	  inserted = XFASTINT (insval);
	}
    }

  /* Decode file format.  */
  if (inserted > 0)
    {
      /* Don't run point motion or modification hooks when decoding.  */
      int count1 = SPECPDL_INDEX ();
      EMACS_INT old_inserted = inserted;
      specbind (Qinhibit_point_motion_hooks, Qt);
      specbind (Qinhibit_modification_hooks, Qt);

      /* Save old undo list and don't record undo for decoding.  */
      old_undo = BVAR (current_buffer, undo_list);
      BVAR (current_buffer, undo_list) = Qt;

      if (NILP (replace))
	{
	  insval = call3 (Qformat_decode,
			  Qnil, make_number (inserted), visit);
	  CHECK_NUMBER (insval);
	  inserted = XFASTINT (insval);
	}
      else
	{
	  /* If REPLACE is non-nil and we succeeded in not replacing the
	     beginning or end of the buffer text with the file's contents,
	     call format-decode with `point' positioned at the beginning
	     of the buffer and `inserted' equaling the number of
	     characters in the buffer.  Otherwise, format-decode might
	     fail to correctly analyze the beginning or end of the buffer.
	     Hence we temporarily save `point' and `inserted' here and
	     restore `point' iff format-decode did not insert or delete
	     any text.  Otherwise we leave `point' at point-min.  */
	  EMACS_INT opoint = PT;
	  EMACS_INT opoint_byte = PT_BYTE;
	  EMACS_INT oinserted = ZV - BEGV;
	  int ochars_modiff = CHARS_MODIFF;

	  TEMP_SET_PT_BOTH (BEGV, BEGV_BYTE);
	  insval = call3 (Qformat_decode,
			  Qnil, make_number (oinserted), visit);
	  CHECK_NUMBER (insval);
	  if (ochars_modiff == CHARS_MODIFF)
	    /* format_decode didn't modify buffer's characters => move
	       point back to position before inserted text and leave
	       value of inserted alone.  */
	    SET_PT_BOTH (opoint, opoint_byte);
	  else
	    /* format_decode modified buffer's characters => consider
	       entire buffer changed and leave point at point-min.  */
	    inserted = XFASTINT (insval);
	}

      /* For consistency with format-decode call these now iff inserted > 0
	 (martin 2007-06-28).  */
      p = Vafter_insert_file_functions;
      while (CONSP (p))
	{
	  if (NILP (replace))
	    {
	      insval = call1 (XCAR (p), make_number (inserted));
	      if (!NILP (insval))
		{
		  CHECK_NUMBER (insval);
		  inserted = XFASTINT (insval);
		}
	    }
	  else
	    {
	      /* For the rationale of this see the comment on
		 format-decode above.  */
	      EMACS_INT opoint = PT;
	      EMACS_INT opoint_byte = PT_BYTE;
	      EMACS_INT oinserted = ZV - BEGV;
	      int ochars_modiff = CHARS_MODIFF;

	      TEMP_SET_PT_BOTH (BEGV, BEGV_BYTE);
	      insval = call1 (XCAR (p), make_number (oinserted));
	      if (!NILP (insval))
		{
		  CHECK_NUMBER (insval);
		  if (ochars_modiff == CHARS_MODIFF)
		    /* after_insert_file_functions didn't modify
		       buffer's characters => move point back to
		       position before inserted text and leave value of
		       inserted alone.  */
		    SET_PT_BOTH (opoint, opoint_byte);
		  else
		    /* after_insert_file_functions did modify buffer's
	               characters => consider entire buffer changed and
	               leave point at point-min.  */
		    inserted = XFASTINT (insval);
		}
	    }

	  QUIT;
	  p = XCDR (p);
	}

      if (NILP (visit))
	{
	  BVAR (current_buffer, undo_list) = old_undo;
	  if (CONSP (old_undo) && inserted != old_inserted)
	    {
	      /* Adjust the last undo record for the size change during
		 the format conversion.  */
	      Lisp_Object tem = XCAR (old_undo);
	      if (CONSP (tem) && INTEGERP (XCAR (tem))
		  && INTEGERP (XCDR (tem))
		  && XFASTINT (XCDR (tem)) == PT + old_inserted)
		XSETCDR (tem, make_number (PT + inserted));
	    }
	}
      else
	/* If undo_list was Qt before, keep it that way.
	   Otherwise start with an empty undo_list.  */
	BVAR (current_buffer, undo_list) = EQ (old_undo, Qt) ? Qt : Qnil;

      unbind_to (count1, Qnil);
    }

  if (!NILP (visit)
      && current_buffer->modtime == -1)
    {
      /* If visiting nonexistent file, return nil.  */
      errno = save_errno;
      report_file_error ("Opening input file", Fcons (orig_filename, Qnil));
    }

  if (read_quit)
    Fsignal (Qquit, Qnil);

  /* ??? Retval needs to be dealt with in all cases consistently.  */
  if (NILP (val))
    val = Fcons (orig_filename,
		 Fcons (make_number (inserted),
			Qnil));

  RETURN_UNGCPRO (unbind_to (count, val));
}

static Lisp_Object build_annotations (Lisp_Object, Lisp_Object);

static Lisp_Object
build_annotations_unwind (Lisp_Object arg)
{
  Vwrite_region_annotation_buffers = arg;
  return Qnil;
}

/* Decide the coding-system to encode the data with.  */

static Lisp_Object
choose_write_coding_system (Lisp_Object start, Lisp_Object end, Lisp_Object filename,
			    Lisp_Object append, Lisp_Object visit, Lisp_Object lockname,
			    struct coding_system *coding)
{
  Lisp_Object val;
  Lisp_Object eol_parent = Qnil;

  if (auto_saving
      && NILP (Fstring_equal (BVAR (current_buffer, filename),
			      BVAR (current_buffer, auto_save_file_name))))
    {
      val = Qutf_8_emacs;
      eol_parent = Qunix;
    }
  else if (!NILP (Vcoding_system_for_write))
    {
      val = Vcoding_system_for_write;
      if (coding_system_require_warning
	  && !NILP (Ffboundp (Vselect_safe_coding_system_function)))
	/* Confirm that VAL can surely encode the current region.  */
	val = call5 (Vselect_safe_coding_system_function,
		     start, end, Fcons (Qt, Fcons (val, Qnil)),
		     Qnil, filename);
    }
  else
    {
      /* If the variable `buffer-file-coding-system' is set locally,
	 it means that the file was read with some kind of code
	 conversion or the variable is explicitly set by users.  We
	 had better write it out with the same coding system even if
	 `enable-multibyte-characters' is nil.

	 If it is not set locally, we anyway have to convert EOL
	 format if the default value of `buffer-file-coding-system'
	 tells that it is not Unix-like (LF only) format.  */
      int using_default_coding = 0;
      int force_raw_text = 0;

      val = BVAR (current_buffer, buffer_file_coding_system);
      if (NILP (val)
	  || NILP (Flocal_variable_p (Qbuffer_file_coding_system, Qnil)))
	{
	  val = Qnil;
	  if (NILP (BVAR (current_buffer, enable_multibyte_characters)))
	    force_raw_text = 1;
	}

      if (NILP (val))
	{
	  /* Check file-coding-system-alist.  */
	  Lisp_Object args[7], coding_systems;

	  args[0] = Qwrite_region; args[1] = start; args[2] = end;
	  args[3] = filename; args[4] = append; args[5] = visit;
	  args[6] = lockname;
	  coding_systems = Ffind_operation_coding_system (7, args);
	  if (CONSP (coding_systems) && !NILP (XCDR (coding_systems)))
	    val = XCDR (coding_systems);
	}

      if (NILP (val))
	{
	  /* If we still have not decided a coding system, use the
	     default value of buffer-file-coding-system.  */
	  val = BVAR (current_buffer, buffer_file_coding_system);
	  using_default_coding = 1;
	}

      if (! NILP (val) && ! force_raw_text)
	{
	  Lisp_Object spec, attrs;

	  CHECK_CODING_SYSTEM_GET_SPEC (val, spec);
	  attrs = AREF (spec, 0);
	  if (EQ (CODING_ATTR_TYPE (attrs), Qraw_text))
	    force_raw_text = 1;
	}

      if (!force_raw_text
	  && !NILP (Ffboundp (Vselect_safe_coding_system_function)))
	/* Confirm that VAL can surely encode the current region.  */
	val = call5 (Vselect_safe_coding_system_function,
		     start, end, val, Qnil, filename);

      /* If the decided coding-system doesn't specify end-of-line
	 format, we use that of
	 `default-buffer-file-coding-system'.  */
      if (! using_default_coding
	  && ! NILP (BVAR (&buffer_defaults, buffer_file_coding_system)))
	val = (coding_inherit_eol_type
	       (val, BVAR (&buffer_defaults, buffer_file_coding_system)));

      /* If we decide not to encode text, use `raw-text' or one of its
	 subsidiaries.  */
      if (force_raw_text)
	val = raw_text_coding_system (val);
    }

  val = coding_inherit_eol_type (val, eol_parent);
  setup_coding_system (val, coding);

  if (!STRINGP (start) && !NILP (BVAR (current_buffer, selective_display)))
    coding->mode |= CODING_MODE_SELECTIVE_DISPLAY;
  return val;
}

DEFUN ("write-region", Fwrite_region, Swrite_region, 3, 7,
       "r\nFWrite region to file: \ni\ni\ni\np",
       doc: /* Write current region into specified file.
When called from a program, requires three arguments:
START, END and FILENAME.  START and END are normally buffer positions
specifying the part of the buffer to write.
If START is nil, that means to use the entire buffer contents.
If START is a string, then output that string to the file
instead of any buffer contents; END is ignored.

Optional fourth argument APPEND if non-nil means
  append to existing file contents (if any).  If it is an integer,
  seek to that offset in the file before writing.
Optional fifth argument VISIT, if t or a string, means
  set the last-save-file-modtime of buffer to this file's modtime
  and mark buffer not modified.
If VISIT is a string, it is a second file name;
  the output goes to FILENAME, but the buffer is marked as visiting VISIT.
  VISIT is also the file name to lock and unlock for clash detection.
If VISIT is neither t nor nil nor a string,
  that means do not display the \"Wrote file\" message.
The optional sixth arg LOCKNAME, if non-nil, specifies the name to
  use for locking and unlocking, overriding FILENAME and VISIT.
The optional seventh arg MUSTBENEW, if non-nil, insists on a check
  for an existing file with the same name.  If MUSTBENEW is `excl',
  that means to get an error if the file already exists; never overwrite.
  If MUSTBENEW is neither nil nor `excl', that means ask for
  confirmation before overwriting, but do go ahead and overwrite the file
  if the user confirms.

This does code conversion according to the value of
`coding-system-for-write', `buffer-file-coding-system', or
`file-coding-system-alist', and sets the variable
`last-coding-system-used' to the coding system actually used.

This calls `write-region-annotate-functions' at the start, and
`write-region-post-annotation-function' at the end.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object filename, Lisp_Object append, Lisp_Object visit, Lisp_Object lockname, Lisp_Object mustbenew)
{
  register int desc;
  int failure;
  int save_errno = 0;
  const char *fn;
  struct stat st;
  int count = SPECPDL_INDEX ();
  int count1;
  Lisp_Object handler;
  Lisp_Object visit_file;
  Lisp_Object annotations;
  Lisp_Object encoded_filename;
  int visiting = (EQ (visit, Qt) || STRINGP (visit));
  int quietly = !NILP (visit);
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;
  struct buffer *given_buffer;
  struct coding_system coding;

  if (current_buffer->base_buffer && visiting)
    error ("Cannot do file visiting in an indirect buffer");

  if (!NILP (start) && !STRINGP (start))
    validate_region (&start, &end);

  visit_file = Qnil;
  GCPRO5 (start, filename, visit, visit_file, lockname);

  filename = Fexpand_file_name (filename, Qnil);

  if (!NILP (mustbenew) && !EQ (mustbenew, Qexcl))
    barf_or_query_if_file_exists (filename, "overwrite", 1, 0, 1);

  if (STRINGP (visit))
    visit_file = Fexpand_file_name (visit, Qnil);
  else
    visit_file = filename;

  if (NILP (lockname))
    lockname = visit_file;

  annotations = Qnil;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (filename, Qwrite_region);
  /* If FILENAME has no handler, see if VISIT has one.  */
  if (NILP (handler) && STRINGP (visit))
    handler = Ffind_file_name_handler (visit, Qwrite_region);

  if (!NILP (handler))
    {
      Lisp_Object val;
      val = call6 (handler, Qwrite_region, start, end,
		   filename, append, visit);

      if (visiting)
	{
	  SAVE_MODIFF = MODIFF;
	  XSETFASTINT (BVAR (current_buffer, save_length), Z - BEG);
	  BVAR (current_buffer, filename) = visit_file;
	}
      UNGCPRO;
      return val;
    }

  record_unwind_protect (save_restriction_restore, save_restriction_save ());

  /* Special kludge to simplify auto-saving.  */
  if (NILP (start))
    {
      /* Do it later, so write-region-annotate-function can work differently
	 if we save "the buffer" vs "a region".
	 This is useful in tar-mode.  --Stef
      XSETFASTINT (start, BEG);
      XSETFASTINT (end, Z); */
      Fwiden ();
    }

  record_unwind_protect (build_annotations_unwind,
			 Vwrite_region_annotation_buffers);
  Vwrite_region_annotation_buffers = Fcons (Fcurrent_buffer (), Qnil);
  count1 = SPECPDL_INDEX ();

  given_buffer = current_buffer;

  if (!STRINGP (start))
    {
      annotations = build_annotations (start, end);

      if (current_buffer != given_buffer)
	{
	  XSETFASTINT (start, BEGV);
	  XSETFASTINT (end, ZV);
	}
    }

  if (NILP (start))
    {
      XSETFASTINT (start, BEGV);
      XSETFASTINT (end, ZV);
    }

  UNGCPRO;

  GCPRO5 (start, filename, annotations, visit_file, lockname);

  /* Decide the coding-system to encode the data with.
     We used to make this choice before calling build_annotations, but that
     leads to problems when a write-annotate-function takes care of
     unsavable chars (as was the case with X-Symbol).  */
  Vlast_coding_system_used
    = choose_write_coding_system (start, end, filename,
				  append, visit, lockname, &coding);

#ifdef CLASH_DETECTION
  if (!auto_saving)
    lock_file (lockname);
#endif /* CLASH_DETECTION */

  encoded_filename = ENCODE_FILE (filename);

  fn = SSDATA (encoded_filename);
  desc = -1;
  if (!NILP (append))
#ifdef DOS_NT
    desc = emacs_open (fn, O_WRONLY | O_BINARY, 0);
#else  /* not DOS_NT */
    desc = emacs_open (fn, O_WRONLY, 0);
#endif /* not DOS_NT */

  if (desc < 0 && (NILP (append) || errno == ENOENT))
#ifdef DOS_NT
  desc = emacs_open (fn,
		     O_WRONLY | O_CREAT | O_BINARY
		     | (EQ (mustbenew, Qexcl) ? O_EXCL : O_TRUNC),
		     S_IREAD | S_IWRITE);
#else  /* not DOS_NT */
  desc = emacs_open (fn, O_WRONLY | O_TRUNC | O_CREAT
		     | (EQ (mustbenew, Qexcl) ? O_EXCL : 0),
		     auto_saving ? auto_save_mode_bits : 0666);
#endif /* not DOS_NT */

  if (desc < 0)
    {
#ifdef CLASH_DETECTION
      save_errno = errno;
      if (!auto_saving) unlock_file (lockname);
      errno = save_errno;
#endif /* CLASH_DETECTION */
      UNGCPRO;
      report_file_error ("Opening output file", Fcons (filename, Qnil));
    }

  record_unwind_protect (close_file_unwind, make_number (desc));

  if (!NILP (append) && !NILP (Ffile_regular_p (filename)))
    {
      off_t ret;

      if (NUMBERP (append))
	ret = emacs_lseek (desc, XINT (append), SEEK_CUR);
      else
	ret = lseek (desc, 0, SEEK_END);
      if (ret < 0)
	{
#ifdef CLASH_DETECTION
	  save_errno = errno;
	  if (!auto_saving) unlock_file (lockname);
	  errno = save_errno;
#endif /* CLASH_DETECTION */
	  UNGCPRO;
	  report_file_error ("Lseek error", Fcons (filename, Qnil));
	}
    }

  UNGCPRO;

  failure = 0;
  immediate_quit = 1;

  if (STRINGP (start))
    {
      failure = 0 > a_write (desc, start, 0, SCHARS (start),
			     &annotations, &coding);
      save_errno = errno;
    }
  else if (XINT (start) != XINT (end))
    {
      failure = 0 > a_write (desc, Qnil,
			     XINT (start), XINT (end) - XINT (start),
			     &annotations, &coding);
      save_errno = errno;
    }
  else
    {
      /* If file was empty, still need to write the annotations */
      coding.mode |= CODING_MODE_LAST_BLOCK;
      failure = 0 > a_write (desc, Qnil, XINT (end), 0, &annotations, &coding);
      save_errno = errno;
    }

  if (CODING_REQUIRE_FLUSHING (&coding)
      && !(coding.mode & CODING_MODE_LAST_BLOCK)
      && ! failure)
    {
      /* We have to flush out a data. */
      coding.mode |= CODING_MODE_LAST_BLOCK;
      failure = 0 > e_write (desc, Qnil, 1, 1, &coding);
      save_errno = errno;
    }

  immediate_quit = 0;

#ifdef HAVE_FSYNC
  /* Note fsync appears to change the modtime on BSD4.2 (both vax and sun).
     Disk full in NFS may be reported here.  */
  /* mib says that closing the file will try to write as fast as NFS can do
     it, and that means the fsync here is not crucial for autosave files.  */
  if (!auto_saving && !write_region_inhibit_fsync && fsync (desc) < 0)
    {
      /* If fsync fails with EINTR, don't treat that as serious.  Also
	 ignore EINVAL which happens when fsync is not supported on this
	 file.  */
      if (errno != EINTR && errno != EINVAL)
	failure = 1, save_errno = errno;
    }
#endif

  /* NFS can report a write failure now.  */
  if (emacs_close (desc) < 0)
    failure = 1, save_errno = errno;

  stat (fn, &st);

  /* Discard the unwind protect for close_file_unwind.  */
  specpdl_ptr = specpdl + count1;

  /* Call write-region-post-annotation-function. */
  while (CONSP (Vwrite_region_annotation_buffers))
    {
      Lisp_Object buf = XCAR (Vwrite_region_annotation_buffers);
      if (!NILP (Fbuffer_live_p (buf)))
  	{
  	  Fset_buffer (buf);
  	  if (FUNCTIONP (Vwrite_region_post_annotation_function))
  	    call0 (Vwrite_region_post_annotation_function);
  	}
      Vwrite_region_annotation_buffers
  	= XCDR (Vwrite_region_annotation_buffers);
    }

  unbind_to (count, Qnil);

#ifdef CLASH_DETECTION
  if (!auto_saving)
    unlock_file (lockname);
#endif /* CLASH_DETECTION */

  /* Do this before reporting IO error
     to avoid a "file has changed on disk" warning on
     next attempt to save.  */
  if (visiting)
    {
      current_buffer->modtime = st.st_mtime;
      current_buffer->modtime_size = st.st_size;
    }

  if (failure)
    error ("IO error writing %s: %s", SDATA (filename),
	   emacs_strerror (save_errno));

  if (visiting)
    {
      SAVE_MODIFF = MODIFF;
      XSETFASTINT (BVAR (current_buffer, save_length), Z - BEG);
      BVAR (current_buffer, filename) = visit_file;
      update_mode_lines++;
    }
  else if (quietly)
    {
      if (auto_saving
	  && ! NILP (Fstring_equal (BVAR (current_buffer, filename),
				    BVAR (current_buffer, auto_save_file_name))))
	SAVE_MODIFF = MODIFF;

      return Qnil;
    }

  if (!auto_saving)
    message_with_string ((INTEGERP (append)
			  ? "Updated %s"
			  : ! NILP (append)
			  ? "Added to %s"
			  : "Wrote %s"),
			 visit_file, 1);

  return Qnil;
}

Lisp_Object merge (Lisp_Object, Lisp_Object, Lisp_Object);

DEFUN ("car-less-than-car", Fcar_less_than_car, Scar_less_than_car, 2, 2, 0,
       doc: /* Return t if (car A) is numerically less than (car B).  */)
  (Lisp_Object a, Lisp_Object b)
{
  return Flss (Fcar (a), Fcar (b));
}

/* Build the complete list of annotations appropriate for writing out
   the text between START and END, by calling all the functions in
   write-region-annotate-functions and merging the lists they return.
   If one of these functions switches to a different buffer, we assume
   that buffer contains altered text.  Therefore, the caller must
   make sure to restore the current buffer in all cases,
   as save-excursion would do.  */

static Lisp_Object
build_annotations (Lisp_Object start, Lisp_Object end)
{
  Lisp_Object annotations;
  Lisp_Object p, res;
  struct gcpro gcpro1, gcpro2;
  Lisp_Object original_buffer;
  int i, used_global = 0;

  XSETBUFFER (original_buffer, current_buffer);

  annotations = Qnil;
  p = Vwrite_region_annotate_functions;
  GCPRO2 (annotations, p);
  while (CONSP (p))
    {
      struct buffer *given_buffer = current_buffer;
      if (EQ (Qt, XCAR (p)) && !used_global)
	{ /* Use the global value of the hook.  */
	  Lisp_Object arg[2];
	  used_global = 1;
	  arg[0] = Fdefault_value (Qwrite_region_annotate_functions);
	  arg[1] = XCDR (p);
	  p = Fappend (2, arg);
	  continue;
	}
      Vwrite_region_annotations_so_far = annotations;
      res = call2 (XCAR (p), start, end);
      /* If the function makes a different buffer current,
	 assume that means this buffer contains altered text to be output.
	 Reset START and END from the buffer bounds
	 and discard all previous annotations because they should have
	 been dealt with by this function.  */
      if (current_buffer != given_buffer)
	{
	  Vwrite_region_annotation_buffers
	    = Fcons (Fcurrent_buffer (),
		     Vwrite_region_annotation_buffers);
	  XSETFASTINT (start, BEGV);
	  XSETFASTINT (end, ZV);
	  annotations = Qnil;
	}
      Flength (res);   /* Check basic validity of return value */
      annotations = merge (annotations, res, Qcar_less_than_car);
      p = XCDR (p);
    }

  /* Now do the same for annotation functions implied by the file-format */
  if (auto_saving && (!EQ (BVAR (current_buffer, auto_save_file_format), Qt)))
    p = BVAR (current_buffer, auto_save_file_format);
  else
    p = BVAR (current_buffer, file_format);
  for (i = 0; CONSP (p); p = XCDR (p), ++i)
    {
      struct buffer *given_buffer = current_buffer;

      Vwrite_region_annotations_so_far = annotations;

      /* Value is either a list of annotations or nil if the function
         has written annotations to a temporary buffer, which is now
         current.  */
      res = call5 (Qformat_annotate_function, XCAR (p), start, end,
		   original_buffer, make_number (i));
      if (current_buffer != given_buffer)
	{
	  XSETFASTINT (start, BEGV);
	  XSETFASTINT (end, ZV);
	  annotations = Qnil;
	}

      if (CONSP (res))
	annotations = merge (annotations, res, Qcar_less_than_car);
    }

  UNGCPRO;
  return annotations;
}


/* Write to descriptor DESC the NCHARS chars starting at POS of STRING.
   If STRING is nil, POS is the character position in the current buffer.
   Intersperse with them the annotations from *ANNOT
   which fall within the range of POS to POS + NCHARS,
   each at its appropriate position.

   We modify *ANNOT by discarding elements as we use them up.

   The return value is negative in case of system call failure.  */

static int
a_write (int desc, Lisp_Object string, EMACS_INT pos,
	 register EMACS_INT nchars, Lisp_Object *annot,
	 struct coding_system *coding)
{
  Lisp_Object tem;
  EMACS_INT nextpos;
  EMACS_INT lastpos = pos + nchars;

  while (NILP (*annot) || CONSP (*annot))
    {
      tem = Fcar_safe (Fcar (*annot));
      nextpos = pos - 1;
      if (INTEGERP (tem))
	nextpos = XFASTINT (tem);

      /* If there are no more annotations in this range,
	 output the rest of the range all at once.  */
      if (! (nextpos >= pos && nextpos <= lastpos))
	return e_write (desc, string, pos, lastpos, coding);

      /* Output buffer text up to the next annotation's position.  */
      if (nextpos > pos)
	{
	  if (0 > e_write (desc, string, pos, nextpos, coding))
	    return -1;
	  pos = nextpos;
	}
      /* Output the annotation.  */
      tem = Fcdr (Fcar (*annot));
      if (STRINGP (tem))
	{
	  if (0 > e_write (desc, tem, 0, SCHARS (tem), coding))
	    return -1;
	}
      *annot = Fcdr (*annot);
    }
  return 0;
}


/* Write text in the range START and END into descriptor DESC,
   encoding them with coding system CODING.  If STRING is nil, START
   and END are character positions of the current buffer, else they
   are indexes to the string STRING.  */

static int
e_write (int desc, Lisp_Object string, EMACS_INT start, EMACS_INT end,
	 struct coding_system *coding)
{
  if (STRINGP (string))
    {
      start = 0;
      end = SCHARS (string);
    }

  /* We used to have a code for handling selective display here.  But,
     now it is handled within encode_coding.  */

  while (start < end)
    {
      if (STRINGP (string))
	{
	  coding->src_multibyte = SCHARS (string) < SBYTES (string);
	  if (CODING_REQUIRE_ENCODING (coding))
	    {
	      encode_coding_object (coding, string,
				    start, string_char_to_byte (string, start),
				    end, string_char_to_byte (string, end), Qt);
	    }
	  else
	    {
	      coding->dst_object = string;
	      coding->consumed_char = SCHARS (string);
	      coding->produced = SBYTES (string);
	    }
	}
      else
	{
	  EMACS_INT start_byte = CHAR_TO_BYTE (start);
	  EMACS_INT end_byte = CHAR_TO_BYTE (end);

	  coding->src_multibyte = (end - start) < (end_byte - start_byte);
	  if (CODING_REQUIRE_ENCODING (coding))
	    {
	      encode_coding_object (coding, Fcurrent_buffer (),
				    start, start_byte, end, end_byte, Qt);
	    }
	  else
	    {
	      coding->dst_object = Qnil;
	      coding->dst_pos_byte = start_byte;
	      if (start >= GPT || end <= GPT)
		{
		  coding->consumed_char = end - start;
		  coding->produced = end_byte - start_byte;
		}
	      else
		{
		  coding->consumed_char = GPT - start;
		  coding->produced = GPT_BYTE - start_byte;
		}
	    }
	}

      if (coding->produced > 0)
	{
	  coding->produced -=
	    emacs_write (desc,
			 STRINGP (coding->dst_object)
			 ? SSDATA (coding->dst_object)
			 : (char *) BYTE_POS_ADDR (coding->dst_pos_byte),
			 coding->produced);

	  if (coding->produced)
	    return -1;
	}
      start += coding->consumed_char;
    }

  return 0;
}

DEFUN ("verify-visited-file-modtime", Fverify_visited_file_modtime,
       Sverify_visited_file_modtime, 0, 1, 0,
       doc: /* Return t if last mod time of BUF's visited file matches what BUF records.
This means that the file has not been changed since it was visited or saved.
If BUF is omitted or nil, it defaults to the current buffer.
See Info node `(elisp)Modification Time' for more details.  */)
  (Lisp_Object buf)
{
  struct buffer *b;
  struct stat st;
  Lisp_Object handler;
  Lisp_Object filename;

  if (NILP (buf))
    b = current_buffer;
  else
    {
      CHECK_BUFFER (buf);
      b = XBUFFER (buf);
    }

  if (!STRINGP (BVAR (b, filename))) return Qt;
  if (b->modtime == 0) return Qt;

  /* If the file name has special constructs in it,
     call the corresponding file handler.  */
  handler = Ffind_file_name_handler (BVAR (b, filename),
				     Qverify_visited_file_modtime);
  if (!NILP (handler))
    return call2 (handler, Qverify_visited_file_modtime, buf);

  filename = ENCODE_FILE (BVAR (b, filename));

  if (stat (SSDATA (filename), &st) < 0)
    {
      /* If the file doesn't exist now and didn't exist before,
	 we say that it isn't modified, provided the error is a tame one.  */
      if (errno == ENOENT || errno == EACCES || errno == ENOTDIR)
	st.st_mtime = -1;
      else
	st.st_mtime = 0;
    }
  if ((st.st_mtime == b->modtime
       /* If both are positive, accept them if they are off by one second.  */
       || (st.st_mtime > 0 && b->modtime > 0
	   && (st.st_mtime - 1 == b->modtime
	       || st.st_mtime == b->modtime - 1)))
      && (st.st_size == b->modtime_size
          || b->modtime_size < 0))
    return Qt;
  return Qnil;
}

DEFUN ("clear-visited-file-modtime", Fclear_visited_file_modtime,
       Sclear_visited_file_modtime, 0, 0, 0,
       doc: /* Clear out records of last mod time of visited file.
Next attempt to save will certainly not complain of a discrepancy.  */)
  (void)
{
  current_buffer->modtime = 0;
  current_buffer->modtime_size = -1;
  return Qnil;
}

DEFUN ("visited-file-modtime", Fvisited_file_modtime,
       Svisited_file_modtime, 0, 0, 0,
       doc: /* Return the current buffer's recorded visited file modification time.
The value is a list of the form (HIGH LOW), like the time values that
`file-attributes' returns.  If the current buffer has no recorded file
modification time, this function returns 0.  If the visited file
doesn't exist, HIGH will be -1.
See Info node `(elisp)Modification Time' for more details.  */)
  (void)
{
  if (! current_buffer->modtime)
    return make_number (0);
  return make_time (current_buffer->modtime);
}

DEFUN ("set-visited-file-modtime", Fset_visited_file_modtime,
       Sset_visited_file_modtime, 0, 1, 0,
       doc: /* Update buffer's recorded modification time from the visited file's time.
Useful if the buffer was not read from the file normally
or if the file itself has been changed for some known benign reason.
An argument specifies the modification time value to use
\(instead of that of the visited file), in the form of a list
\(HIGH . LOW) or (HIGH LOW).  */)
  (Lisp_Object time_list)
{
  if (!NILP (time_list))
    {
      CONS_TO_INTEGER (time_list, time_t, current_buffer->modtime);
      current_buffer->modtime_size = -1;
    }
  else
    {
      register Lisp_Object filename;
      struct stat st;
      Lisp_Object handler;

      filename = Fexpand_file_name (BVAR (current_buffer, filename), Qnil);

      /* If the file name has special constructs in it,
	 call the corresponding file handler.  */
      handler = Ffind_file_name_handler (filename, Qset_visited_file_modtime);
      if (!NILP (handler))
	/* The handler can find the file name the same way we did.  */
	return call2 (handler, Qset_visited_file_modtime, Qnil);

      filename = ENCODE_FILE (filename);

      if (stat (SSDATA (filename), &st) >= 0)
        {
	  current_buffer->modtime = st.st_mtime;
          current_buffer->modtime_size = st.st_size;
        }
    }

  return Qnil;
}

static Lisp_Object
auto_save_error (Lisp_Object error_val)
{
  Lisp_Object args[3], msg;
  int i, nbytes;
  struct gcpro gcpro1;
  char *msgbuf;
  USE_SAFE_ALLOCA;

  auto_save_error_occurred = 1;

  ring_bell (XFRAME (selected_frame));

  args[0] = build_string ("Auto-saving %s: %s");
  args[1] = BVAR (current_buffer, name);
  args[2] = Ferror_message_string (error_val);
  msg = Fformat (3, args);
  GCPRO1 (msg);
  nbytes = SBYTES (msg);
  SAFE_ALLOCA (msgbuf, char *, nbytes);
  memcpy (msgbuf, SDATA (msg), nbytes);

  for (i = 0; i < 3; ++i)
    {
      if (i == 0)
	message2 (msgbuf, nbytes, STRING_MULTIBYTE (msg));
      else
	message2_nolog (msgbuf, nbytes, STRING_MULTIBYTE (msg));
      Fsleep_for (make_number (1), Qnil);
    }

  SAFE_FREE ();
  UNGCPRO;
  return Qnil;
}

static Lisp_Object
auto_save_1 (void)
{
  struct stat st;
  Lisp_Object modes;

  auto_save_mode_bits = 0666;

  /* Get visited file's mode to become the auto save file's mode.  */
  if (! NILP (BVAR (current_buffer, filename)))
    {
      if (stat (SSDATA (BVAR (current_buffer, filename)), &st) >= 0)
	/* But make sure we can overwrite it later!  */
	auto_save_mode_bits = (st.st_mode | 0600) & 0777;
      else if ((modes = Ffile_modes (BVAR (current_buffer, filename)),
		INTEGERP (modes)))
	/* Remote files don't cooperate with stat.  */
	auto_save_mode_bits = (XINT (modes) | 0600) & 0777;
    }

  return
    Fwrite_region (Qnil, Qnil, BVAR (current_buffer, auto_save_file_name), Qnil,
		   NILP (Vauto_save_visited_file_name) ? Qlambda : Qt,
		   Qnil, Qnil);
}

static Lisp_Object
do_auto_save_unwind (Lisp_Object arg)  /* used as unwind-protect function */

{
  FILE *stream = (FILE *) XSAVE_VALUE (arg)->pointer;
  auto_saving = 0;
  if (stream != NULL)
    {
      BLOCK_INPUT;
      fclose (stream);
      UNBLOCK_INPUT;
    }
  return Qnil;
}

static Lisp_Object
do_auto_save_unwind_1 (Lisp_Object value)  /* used as unwind-protect function */

{
  minibuffer_auto_raise = XINT (value);
  return Qnil;
}

static Lisp_Object
do_auto_save_make_dir (Lisp_Object dir)
{
  Lisp_Object result;

  auto_saving_dir_umask = 077;
  result = call2 (Qmake_directory, dir, Qt);
  auto_saving_dir_umask = 0;
  return result;
}

static Lisp_Object
do_auto_save_eh (Lisp_Object ignore)
{
  auto_saving_dir_umask = 0;
  return Qnil;
}

DEFUN ("do-auto-save", Fdo_auto_save, Sdo_auto_save, 0, 2, "",
       doc: /* Auto-save all buffers that need it.
This is all buffers that have auto-saving enabled
and are changed since last auto-saved.
Auto-saving writes the buffer into a file
so that your editing is not lost if the system crashes.
This file is not the file you visited; that changes only when you save.
Normally we run the normal hook `auto-save-hook' before saving.

A non-nil NO-MESSAGE argument means do not print any message if successful.
A non-nil CURRENT-ONLY argument means save only current buffer.  */)
  (Lisp_Object no_message, Lisp_Object current_only)
{
  struct buffer *old = current_buffer, *b;
  Lisp_Object tail, buf, hook;
  int auto_saved = 0;
  int do_handled_files;
  Lisp_Object oquit;
  FILE *stream = NULL;
  int count = SPECPDL_INDEX ();
  int orig_minibuffer_auto_raise = minibuffer_auto_raise;
  int old_message_p = 0;
  struct gcpro gcpro1, gcpro2;

  if (max_specpdl_size < specpdl_size + 40)
    max_specpdl_size = specpdl_size + 40;

  if (minibuf_level)
    no_message = Qt;

  if (NILP (no_message))
    {
      old_message_p = push_message ();
      record_unwind_protect (pop_message_unwind, Qnil);
    }

  /* Ordinarily don't quit within this function,
     but don't make it impossible to quit (in case we get hung in I/O).  */
  oquit = Vquit_flag;
  Vquit_flag = Qnil;

  /* No GCPRO needed, because (when it matters) all Lisp_Object variables
     point to non-strings reached from Vbuffer_alist.  */

  hook = intern ("auto-save-hook");
  Frun_hooks (1, &hook);

  if (STRINGP (Vauto_save_list_file_name))
    {
      Lisp_Object listfile;

      listfile = Fexpand_file_name (Vauto_save_list_file_name, Qnil);

      /* Don't try to create the directory when shutting down Emacs,
         because creating the directory might signal an error, and
         that would leave Emacs in a strange state.  */
      if (!NILP (Vrun_hooks))
	{
	  Lisp_Object dir;
	  dir = Qnil;
	  GCPRO2 (dir, listfile);
	  dir = Ffile_name_directory (listfile);
	  if (NILP (Ffile_directory_p (dir)))
	    internal_condition_case_1 (do_auto_save_make_dir,
				       dir, Qt,
				       do_auto_save_eh);
	  UNGCPRO;
	}

      stream = fopen (SSDATA (listfile), "w");
    }

  record_unwind_protect (do_auto_save_unwind,
			 make_save_value (stream, 0));
  record_unwind_protect (do_auto_save_unwind_1,
			 make_number (minibuffer_auto_raise));
  minibuffer_auto_raise = 0;
  auto_saving = 1;
  auto_save_error_occurred = 0;

  /* On first pass, save all files that don't have handlers.
     On second pass, save all files that do have handlers.

     If Emacs is crashing, the handlers may tweak what is causing
     Emacs to crash in the first place, and it would be a shame if
     Emacs failed to autosave perfectly ordinary files because it
     couldn't handle some ange-ftp'd file.  */

  for (do_handled_files = 0; do_handled_files < 2; do_handled_files++)
    for (tail = Vbuffer_alist; CONSP (tail); tail = XCDR (tail))
      {
	buf = XCDR (XCAR (tail));
	b = XBUFFER (buf);

	/* Record all the buffers that have auto save mode
	   in the special file that lists them.  For each of these buffers,
	   Record visited name (if any) and auto save name.  */
	if (STRINGP (BVAR (b, auto_save_file_name))
	    && stream != NULL && do_handled_files == 0)
	  {
	    BLOCK_INPUT;
	    if (!NILP (BVAR (b, filename)))
	      {
		fwrite (SDATA (BVAR (b, filename)), 1,
			SBYTES (BVAR (b, filename)), stream);
	      }
	    putc ('\n', stream);
	    fwrite (SDATA (BVAR (b, auto_save_file_name)), 1,
		    SBYTES (BVAR (b, auto_save_file_name)), stream);
	    putc ('\n', stream);
	    UNBLOCK_INPUT;
	  }

	if (!NILP (current_only)
	    && b != current_buffer)
	  continue;

	/* Don't auto-save indirect buffers.
	   The base buffer takes care of it.  */
	if (b->base_buffer)
	  continue;

	/* Check for auto save enabled
	   and file changed since last auto save
	   and file changed since last real save.  */
	if (STRINGP (BVAR (b, auto_save_file_name))
	    && BUF_SAVE_MODIFF (b) < BUF_MODIFF (b)
	    && BUF_AUTOSAVE_MODIFF (b) < BUF_MODIFF (b)
	    /* -1 means we've turned off autosaving for a while--see below.  */
	    && XINT (BVAR (b, save_length)) >= 0
	    && (do_handled_files
		|| NILP (Ffind_file_name_handler (BVAR (b, auto_save_file_name),
						  Qwrite_region))))
	  {
	    EMACS_TIME before_time, after_time;

	    EMACS_GET_TIME (before_time);

	    /* If we had a failure, don't try again for 20 minutes.  */
	    if (b->auto_save_failure_time > 0
		&& EMACS_SECS (before_time) - b->auto_save_failure_time < 1200)
	      continue;

	    set_buffer_internal (b);
	    if (NILP (Vauto_save_include_big_deletions)
		&& (XFASTINT (BVAR (b, save_length)) * 10
		    > (BUF_Z (b) - BUF_BEG (b)) * 13)
		/* A short file is likely to change a large fraction;
		   spare the user annoying messages.  */
		&& XFASTINT (BVAR (b, save_length)) > 5000
		/* These messages are frequent and annoying for `*mail*'.  */
		&& !EQ (BVAR (b, filename), Qnil)
		&& NILP (no_message))
	      {
		/* It has shrunk too much; turn off auto-saving here.  */
		minibuffer_auto_raise = orig_minibuffer_auto_raise;
		message_with_string ("Buffer %s has shrunk a lot; auto save disabled in that buffer until next real save",
				     BVAR (b, name), 1);
		minibuffer_auto_raise = 0;
		/* Turn off auto-saving until there's a real save,
		   and prevent any more warnings.  */
		XSETINT (BVAR (b, save_length), -1);
		Fsleep_for (make_number (1), Qnil);
		continue;
	      }
	    if (!auto_saved && NILP (no_message))
	      message1 ("Auto-saving...");
	    internal_condition_case (auto_save_1, Qt, auto_save_error);
	    auto_saved++;
	    BUF_AUTOSAVE_MODIFF (b) = BUF_MODIFF (b);
	    XSETFASTINT (BVAR (current_buffer, save_length), Z - BEG);
	    set_buffer_internal (old);

	    EMACS_GET_TIME (after_time);

	    /* If auto-save took more than 60 seconds,
	       assume it was an NFS failure that got a timeout.  */
	    if (EMACS_SECS (after_time) - EMACS_SECS (before_time) > 60)
	      b->auto_save_failure_time = EMACS_SECS (after_time);
	  }
      }

  /* Prevent another auto save till enough input events come in.  */
  record_auto_save ();

  if (auto_saved && NILP (no_message))
    {
      if (old_message_p)
	{
	  /* If we are going to restore an old message,
	     give time to read ours.  */
	  sit_for (make_number (1), 0, 0);
	  restore_message ();
	}
      else if (!auto_save_error_occurred)
	/* Don't overwrite the error message if an error occurred.
	   If we displayed a message and then restored a state
	   with no message, leave a "done" message on the screen.  */
	message1 ("Auto-saving...done");
    }

  Vquit_flag = oquit;

  /* This restores the message-stack status.  */
  unbind_to (count, Qnil);
  return Qnil;
}

DEFUN ("set-buffer-auto-saved", Fset_buffer_auto_saved,
       Sset_buffer_auto_saved, 0, 0, 0,
       doc: /* Mark current buffer as auto-saved with its current text.
No auto-save file will be written until the buffer changes again.  */)
  (void)
{
  /* FIXME: This should not be called in indirect buffers, since
     they're not autosaved.  */
  BUF_AUTOSAVE_MODIFF (current_buffer) = MODIFF;
  XSETFASTINT (BVAR (current_buffer, save_length), Z - BEG);
  current_buffer->auto_save_failure_time = 0;
  return Qnil;
}

DEFUN ("clear-buffer-auto-save-failure", Fclear_buffer_auto_save_failure,
       Sclear_buffer_auto_save_failure, 0, 0, 0,
       doc: /* Clear any record of a recent auto-save failure in the current buffer.  */)
  (void)
{
  current_buffer->auto_save_failure_time = 0;
  return Qnil;
}

DEFUN ("recent-auto-save-p", Frecent_auto_save_p, Srecent_auto_save_p,
       0, 0, 0,
       doc: /* Return t if current buffer has been auto-saved recently.
More precisely, if it has been auto-saved since last read from or saved
in the visited file.  If the buffer has no visited file,
then any auto-save counts as "recent".  */)
  (void)
{
  /* FIXME: maybe we should return nil for indirect buffers since
     they're never autosaved.  */
  return (SAVE_MODIFF < BUF_AUTOSAVE_MODIFF (current_buffer) ? Qt : Qnil);
}

/* Reading and completing file names */

DEFUN ("next-read-file-uses-dialog-p", Fnext_read_file_uses_dialog_p,
       Snext_read_file_uses_dialog_p, 0, 0, 0,
       doc: /* Return t if a call to `read-file-name' will use a dialog.
The return value is only relevant for a call to `read-file-name' that happens
before any other event (mouse or keypress) is handled.  */)
  (void)
{
#if defined (USE_MOTIF) || defined (HAVE_NTGUI) || defined (USE_GTK)
  if ((NILP (last_nonmenu_event) || CONSP (last_nonmenu_event))
      && use_dialog_box
      && use_file_dialog
      && have_menus_p ())
    return Qt;
#endif
  return Qnil;
}

Lisp_Object
Fread_file_name (Lisp_Object prompt, Lisp_Object dir, Lisp_Object default_filename, Lisp_Object mustmatch, Lisp_Object initial, Lisp_Object predicate)
{
  struct gcpro gcpro1;
  Lisp_Object args[7];

  GCPRO1 (default_filename);
  args[0] = intern ("read-file-name");
  args[1] = prompt;
  args[2] = dir;
  args[3] = default_filename;
  args[4] = mustmatch;
  args[5] = initial;
  args[6] = predicate;
  RETURN_UNGCPRO (Ffuncall (7, args));
}


void
syms_of_fileio (void)
{
  DEFSYM (Qoperations, "operations");
  DEFSYM (Qexpand_file_name, "expand-file-name");
  DEFSYM (Qsubstitute_in_file_name, "substitute-in-file-name");
  DEFSYM (Qdirectory_file_name, "directory-file-name");
  DEFSYM (Qfile_name_directory, "file-name-directory");
  DEFSYM (Qfile_name_nondirectory, "file-name-nondirectory");
  DEFSYM (Qunhandled_file_name_directory, "unhandled-file-name-directory");
  DEFSYM (Qfile_name_as_directory, "file-name-as-directory");
  DEFSYM (Qcopy_file, "copy-file");
  DEFSYM (Qmake_directory_internal, "make-directory-internal");
  DEFSYM (Qmake_directory, "make-directory");
  DEFSYM (Qdelete_directory_internal, "delete-directory-internal");
  DEFSYM (Qdelete_file, "delete-file");
  DEFSYM (Qrename_file, "rename-file");
  DEFSYM (Qadd_name_to_file, "add-name-to-file");
  DEFSYM (Qmake_symbolic_link, "make-symbolic-link");
  DEFSYM (Qfile_exists_p, "file-exists-p");
  DEFSYM (Qfile_executable_p, "file-executable-p");
  DEFSYM (Qfile_readable_p, "file-readable-p");
  DEFSYM (Qfile_writable_p, "file-writable-p");
  DEFSYM (Qfile_symlink_p, "file-symlink-p");
  DEFSYM (Qaccess_file, "access-file");
  DEFSYM (Qfile_directory_p, "file-directory-p");
  DEFSYM (Qfile_regular_p, "file-regular-p");
  DEFSYM (Qfile_accessible_directory_p, "file-accessible-directory-p");
  DEFSYM (Qfile_modes, "file-modes");
  DEFSYM (Qset_file_modes, "set-file-modes");
  DEFSYM (Qset_file_times, "set-file-times");
  DEFSYM (Qfile_selinux_context, "file-selinux-context");
  DEFSYM (Qset_file_selinux_context, "set-file-selinux-context");
  DEFSYM (Qfile_newer_than_file_p, "file-newer-than-file-p");
  DEFSYM (Qinsert_file_contents, "insert-file-contents");
  DEFSYM (Qwrite_region, "write-region");
  DEFSYM (Qverify_visited_file_modtime, "verify-visited-file-modtime");
  DEFSYM (Qset_visited_file_modtime, "set-visited-file-modtime");
  DEFSYM (Qauto_save_coding, "auto-save-coding");

  DEFSYM (Qfile_name_history, "file-name-history");
  Fset (Qfile_name_history, Qnil);

  DEFSYM (Qfile_error, "file-error");
  DEFSYM (Qfile_already_exists, "file-already-exists");
  DEFSYM (Qfile_date_error, "file-date-error");
  DEFSYM (Qexcl, "excl");

  DEFVAR_LISP ("file-name-coding-system", Vfile_name_coding_system,
	       doc: /* *Coding system for encoding file names.
If it is nil, `default-file-name-coding-system' (which see) is used.  */);
  Vfile_name_coding_system = Qnil;

  DEFVAR_LISP ("default-file-name-coding-system",
	       Vdefault_file_name_coding_system,
	       doc: /* Default coding system for encoding file names.
This variable is used only when `file-name-coding-system' is nil.

This variable is set/changed by the command `set-language-environment'.
User should not set this variable manually,
instead use `file-name-coding-system' to get a constant encoding
of file names regardless of the current language environment.  */);
  Vdefault_file_name_coding_system = Qnil;

  DEFSYM (Qformat_decode, "format-decode");
  DEFSYM (Qformat_annotate_function, "format-annotate-function");
  DEFSYM (Qafter_insert_file_set_coding, "after-insert-file-set-coding");
  DEFSYM (Qcar_less_than_car, "car-less-than-car");

  Fput (Qfile_error, Qerror_conditions,
	Fpurecopy (list2 (Qfile_error, Qerror)));
  Fput (Qfile_error, Qerror_message,
	make_pure_c_string ("File error"));

  Fput (Qfile_already_exists, Qerror_conditions,
	Fpurecopy (list3 (Qfile_already_exists, Qfile_error, Qerror)));
  Fput (Qfile_already_exists, Qerror_message,
	make_pure_c_string ("File already exists"));

  Fput (Qfile_date_error, Qerror_conditions,
	Fpurecopy (list3 (Qfile_date_error, Qfile_error, Qerror)));
  Fput (Qfile_date_error, Qerror_message,
	make_pure_c_string ("Cannot set file date"));

  DEFVAR_LISP ("file-name-handler-alist", Vfile_name_handler_alist,
	       doc: /* Alist of elements (REGEXP . HANDLER) for file names handled specially.
If a file name matches REGEXP, all I/O on that file is done by calling
HANDLER.  If a file name matches more than one handler, the handler
whose match starts last in the file name gets precedence.  The
function `find-file-name-handler' checks this list for a handler for
its argument.

HANDLER should be a function.  The first argument given to it is the
name of the I/O primitive to be handled; the remaining arguments are
the arguments that were passed to that primitive.  For example, if you
do (file-exists-p FILENAME) and FILENAME is handled by HANDLER, then
HANDLER is called like this:

  (funcall HANDLER 'file-exists-p FILENAME)

Note that HANDLER must be able to handle all I/O primitives; if it has
nothing special to do for a primitive, it should reinvoke the
primitive to handle the operation \"the usual way\".
See Info node `(elisp)Magic File Names' for more details.  */);
  Vfile_name_handler_alist = Qnil;

  DEFVAR_LISP ("set-auto-coding-function",
	       Vset_auto_coding_function,
	       doc: /* If non-nil, a function to call to decide a coding system of file.
Two arguments are passed to this function: the file name
and the length of a file contents following the point.
This function should return a coding system to decode the file contents.
It should check the file name against `auto-coding-alist'.
If no coding system is decided, it should check a coding system
specified in the heading lines with the format:
	-*- ... coding: CODING-SYSTEM; ... -*-
or local variable spec of the tailing lines with `coding:' tag.  */);
  Vset_auto_coding_function = Qnil;

  DEFVAR_LISP ("after-insert-file-functions", Vafter_insert_file_functions,
	       doc: /* A list of functions to be called at the end of `insert-file-contents'.
Each is passed one argument, the number of characters inserted,
with point at the start of the inserted text.  Each function
should leave point the same, and return the new character count.
If `insert-file-contents' is intercepted by a handler from
`file-name-handler-alist', that handler is responsible for calling the
functions in `after-insert-file-functions' if appropriate.  */);
  Vafter_insert_file_functions = Qnil;

  DEFVAR_LISP ("write-region-annotate-functions", Vwrite_region_annotate_functions,
	       doc: /* A list of functions to be called at the start of `write-region'.
Each is passed two arguments, START and END as for `write-region'.
These are usually two numbers but not always; see the documentation
for `write-region'.  The function should return a list of pairs
of the form (POSITION . STRING), consisting of strings to be effectively
inserted at the specified positions of the file being written (1 means to
insert before the first byte written).  The POSITIONs must be sorted into
increasing order.

If there are several annotation functions, the lists returned by these
functions are merged destructively.  As each annotation function runs,
the variable `write-region-annotations-so-far' contains a list of all
annotations returned by previous annotation functions.

An annotation function can return with a different buffer current.
Doing so removes the annotations returned by previous functions, and
resets START and END to `point-min' and `point-max' of the new buffer.

After `write-region' completes, Emacs calls the function stored in
`write-region-post-annotation-function', once for each buffer that was
current when building the annotations (i.e., at least once), with that
buffer current.  */);
  Vwrite_region_annotate_functions = Qnil;
  DEFSYM (Qwrite_region_annotate_functions, "write-region-annotate-functions");

  DEFVAR_LISP ("write-region-post-annotation-function",
	       Vwrite_region_post_annotation_function,
	       doc: /* Function to call after `write-region' completes.
The function is called with no arguments.  If one or more of the
annotation functions in `write-region-annotate-functions' changed the
current buffer, the function stored in this variable is called for
each of those additional buffers as well, in addition to the original
buffer.  The relevant buffer is current during each function call.  */);
  Vwrite_region_post_annotation_function = Qnil;
  staticpro (&Vwrite_region_annotation_buffers);

  DEFVAR_LISP ("write-region-annotations-so-far",
	       Vwrite_region_annotations_so_far,
	       doc: /* When an annotation function is called, this holds the previous annotations.
These are the annotations made by other annotation functions
that were already called.  See also `write-region-annotate-functions'.  */);
  Vwrite_region_annotations_so_far = Qnil;

  DEFVAR_LISP ("inhibit-file-name-handlers", Vinhibit_file_name_handlers,
	       doc: /* A list of file name handlers that temporarily should not be used.
This applies only to the operation `inhibit-file-name-operation'.  */);
  Vinhibit_file_name_handlers = Qnil;

  DEFVAR_LISP ("inhibit-file-name-operation", Vinhibit_file_name_operation,
	       doc: /* The operation for which `inhibit-file-name-handlers' is applicable.  */);
  Vinhibit_file_name_operation = Qnil;

  DEFVAR_LISP ("auto-save-list-file-name", Vauto_save_list_file_name,
	       doc: /* File name in which we write a list of all auto save file names.
This variable is initialized automatically from `auto-save-list-file-prefix'
shortly after Emacs reads your `.emacs' file, if you have not yet given it
a non-nil value.  */);
  Vauto_save_list_file_name = Qnil;

  DEFVAR_LISP ("auto-save-visited-file-name", Vauto_save_visited_file_name,
	       doc: /* Non-nil says auto-save a buffer in the file it is visiting, when practical.
Normally auto-save files are written under other names.  */);
  Vauto_save_visited_file_name = Qnil;

  DEFVAR_LISP ("auto-save-include-big-deletions", Vauto_save_include_big_deletions,
	       doc: /* If non-nil, auto-save even if a large part of the text is deleted.
If nil, deleting a substantial portion of the text disables auto-save
in the buffer; this is the default behavior, because the auto-save
file is usually more useful if it contains the deleted text.  */);
  Vauto_save_include_big_deletions = Qnil;

#ifdef HAVE_FSYNC
  DEFVAR_BOOL ("write-region-inhibit-fsync", write_region_inhibit_fsync,
	       doc: /* *Non-nil means don't call fsync in `write-region'.
This variable affects calls to `write-region' as well as save commands.
A non-nil value may result in data loss!  */);
  write_region_inhibit_fsync = 0;
#endif

  DEFVAR_BOOL ("delete-by-moving-to-trash", delete_by_moving_to_trash,
               doc: /* Specifies whether to use the system's trash can.
When non-nil, certain file deletion commands use the function
`move-file-to-trash' instead of deleting files outright.
This includes interactive calls to `delete-file' and
`delete-directory' and the Dired deletion commands.  */);
  delete_by_moving_to_trash = 0;
  Qdelete_by_moving_to_trash = intern_c_string ("delete-by-moving-to-trash");

  DEFSYM (Qmove_file_to_trash, "move-file-to-trash");
  DEFSYM (Qcopy_directory, "copy-directory");
  DEFSYM (Qdelete_directory, "delete-directory");

  defsubr (&Sfind_file_name_handler);
  defsubr (&Sfile_name_directory);
  defsubr (&Sfile_name_nondirectory);
  defsubr (&Sunhandled_file_name_directory);
  defsubr (&Sfile_name_as_directory);
  defsubr (&Sdirectory_file_name);
  defsubr (&Smake_temp_name);
  defsubr (&Sexpand_file_name);
  defsubr (&Ssubstitute_in_file_name);
  defsubr (&Scopy_file);
  defsubr (&Smake_directory_internal);
  defsubr (&Sdelete_directory_internal);
  defsubr (&Sdelete_file);
  defsubr (&Srename_file);
  defsubr (&Sadd_name_to_file);
  defsubr (&Smake_symbolic_link);
  defsubr (&Sfile_name_absolute_p);
  defsubr (&Sfile_exists_p);
  defsubr (&Sfile_executable_p);
  defsubr (&Sfile_readable_p);
  defsubr (&Sfile_writable_p);
  defsubr (&Saccess_file);
  defsubr (&Sfile_symlink_p);
  defsubr (&Sfile_directory_p);
  defsubr (&Sfile_accessible_directory_p);
  defsubr (&Sfile_regular_p);
  defsubr (&Sfile_modes);
  defsubr (&Sset_file_modes);
  defsubr (&Sset_file_times);
  defsubr (&Sfile_selinux_context);
  defsubr (&Sset_file_selinux_context);
  defsubr (&Sset_default_file_modes);
  defsubr (&Sdefault_file_modes);
  defsubr (&Sfile_newer_than_file_p);
  defsubr (&Sinsert_file_contents);
  defsubr (&Swrite_region);
  defsubr (&Scar_less_than_car);
  defsubr (&Sverify_visited_file_modtime);
  defsubr (&Sclear_visited_file_modtime);
  defsubr (&Svisited_file_modtime);
  defsubr (&Sset_visited_file_modtime);
  defsubr (&Sdo_auto_save);
  defsubr (&Sset_buffer_auto_saved);
  defsubr (&Sclear_buffer_auto_save_failure);
  defsubr (&Srecent_auto_save_p);

  defsubr (&Snext_read_file_uses_dialog_p);

#ifdef HAVE_SYNC
  defsubr (&Sunix_sync);
#endif
}
